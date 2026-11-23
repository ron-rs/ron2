//! Schema resolution for the RON Language Server.
//!
//! Schemas are cached by type path (and by file for `#![schema]`), including
//! misses. An entry is revalidated at most once per revalidation interval: a
//! found schema is reloaded when its file's mtime changes (e.g. `cargo build`
//! regenerated it), and a miss is probed again. Between revalidations a lookup
//! is a hash-map hit, so validating on every keystroke does no disk I/O, and
//! the lock is never held during I/O.
//!
//! Lookups are synchronous (the schema validator resolves type references
//! through the synchronous [`SchemaResolverTrait`]); the server runs every
//! lookup on Tokio's blocking pool, so a revalidation's file system calls
//! never block the async executor.
//!
//! Misses are bounded: once the cache holds `MAX_ENTRIES` entries, misses
//! and invalid entries are dropped (typing a `#![type]` creates one miss per
//! keystroke).

use std::{
    fmt, fs,
    path::PathBuf,
    sync::{Arc, Mutex, MutexGuard, PoisonError, RwLock},
    time::{Duration, Instant, SystemTime},
};

use ahash::HashMap;
use ron2::schema::{Schema, SchemaResolver as SchemaResolverTrait, find_schema_file, read_schema};

use crate::document::Document;

/// Default time an entry is trusted before checking the disk again.
const DEFAULT_REVALIDATE_AFTER: Duration = Duration::from_secs(1);

/// Cache size at which misses are evicted.
const MAX_ENTRIES: usize = 512;

/// Resolves and caches schemas for RON documents.
pub struct SchemaResolver {
    cache: Mutex<Cache>,
    /// Configured schema directories to search (from workspace config).
    schema_dirs: RwLock<Vec<PathBuf>>,
    revalidate_after: Duration,
}

#[derive(Default)]
struct Cache {
    /// Bumped whenever the cache is cleared, so a lookup that started before
    /// (e.g. with the old schema dirs) does not re-insert a stale entry.
    generation: u64,
    entries: HashMap<Key, Entry>,
}

/// Why a document's schema could not be loaded.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SchemaLookupError {
    /// No schema file was found.
    NotFound,
    /// The schema file exists but could not be read or parsed.
    Invalid {
        /// The schema file.
        path: PathBuf,
        /// The read or parse error.
        message: String,
    },
}

impl fmt::Display for SchemaLookupError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SchemaLookupError::NotFound => f.write_str("schema not found"),
            SchemaLookupError::Invalid { path, message } => {
                write!(f, "invalid schema file {}: {message}", path.display())
            }
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
enum Key {
    Type(String),
    File(PathBuf),
}

struct Entry {
    checked: Instant,
    state: State,
}

#[derive(Clone)]
enum State {
    Found {
        path: PathBuf,
        modified: Option<SystemTime>,
        schema: Arc<Schema>,
    },
    Invalid {
        path: PathBuf,
        modified: Option<SystemTime>,
        message: String,
    },
    Missing,
}

impl State {
    fn result(&self) -> Result<Arc<Schema>, SchemaLookupError> {
        match self {
            State::Found { schema, .. } => Ok(Arc::clone(schema)),
            State::Invalid { path, message, .. } => Err(SchemaLookupError::Invalid {
                path: path.clone(),
                message: message.clone(),
            }),
            State::Missing => Err(SchemaLookupError::NotFound),
        }
    }

    /// The file this state was loaded from, and its mtime then.
    fn file(&self) -> Option<(&PathBuf, Option<SystemTime>)> {
        match self {
            State::Found { path, modified, .. } | State::Invalid { path, modified, .. } => {
                Some((path, *modified))
            }
            State::Missing => None,
        }
    }
}

impl SchemaResolver {
    /// Create a new schema resolver.
    pub fn new() -> Self {
        Self::with_revalidate_after(DEFAULT_REVALIDATE_AFTER)
    }

    /// Create a resolver that trusts cached entries for `interval` before
    /// checking the disk again.
    pub fn with_revalidate_after(interval: Duration) -> Self {
        Self {
            cache: Mutex::new(Cache::default()),
            schema_dirs: RwLock::new(Vec::new()),
            revalidate_after: interval,
        }
    }

    /// Update the schema directories from workspace configuration.
    pub fn set_schema_dirs(&self, dirs: Vec<PathBuf>) {
        // Hold the cache lock across the update so no lookup can observe the
        // new generation with the old directories.
        let mut cache = self.cache();
        *self
            .schema_dirs
            .write()
            .unwrap_or_else(PoisonError::into_inner) = dirs;
        cache.clear();
    }

    /// Get the current schema directories.
    pub fn schema_dirs(&self) -> Vec<PathBuf> {
        self.schema_dirs
            .read()
            .unwrap_or_else(PoisonError::into_inner)
            .clone()
    }

    /// Resolve the schema for a document.
    ///
    /// Resolution order:
    /// 1. `#![schema = "path"]` - explicit path relative to the RON file
    /// 2. `#![type = "crate::Type"]` - see [`Self::load_schema_by_type`]
    pub fn resolve_schema(&self, doc: &Document) -> Option<Arc<Schema>> {
        self.lookup_schema(doc).ok()
    }

    /// Like [`Self::resolve_schema`], but says why no schema is available.
    pub fn lookup_schema(&self, doc: &Document) -> Result<Arc<Schema>, SchemaLookupError> {
        if let Some(schema_path) = &doc.schema_attr {
            let doc_dir = doc.file_path().and_then(|p| p.parent().map(PathBuf::from));
            let Some(doc_dir) = doc_dir else {
                return Err(SchemaLookupError::NotFound);
            };
            return self.lookup(Key::File(doc_dir.join(schema_path)));
        }
        match doc.type_attr.as_deref() {
            Some(type_path) => self.lookup(Key::Type(type_path.to_owned())),
            None => Err(SchemaLookupError::NotFound),
        }
    }

    /// Load a schema by type path.
    ///
    /// Search order: configured schema directories, `RON_SCHEMA_DIR`, then
    /// the XDG data directory.
    pub fn load_schema_by_type(&self, type_path: &str) -> Option<Arc<Schema>> {
        self.lookup(Key::Type(type_path.to_owned())).ok()
    }

    /// Clear the schema cache.
    pub fn clear_cache(&self) {
        self.cache().clear();
    }

    fn cache(&self) -> MutexGuard<'_, Cache> {
        self.cache.lock().unwrap_or_else(PoisonError::into_inner)
    }

    fn lookup(&self, key: Key) -> Result<Arc<Schema>, SchemaLookupError> {
        let now = Instant::now();
        let (generation, previous) = {
            let cache = self.cache();
            match cache.entries.get(&key) {
                Some(entry) if now.duration_since(entry.checked) < self.revalidate_after => {
                    return entry.state.result();
                }
                entry => (cache.generation, entry.map(|e| e.state.clone())),
            }
        };

        let state = self.load(&key, previous);
        let result = state.result();
        self.cache().insert(
            generation,
            key,
            Entry {
                checked: now,
                state,
            },
        );
        result
    }

    /// Locate and (re)load the schema for `key`, reusing `previous` if its
    /// file is unchanged.
    fn load(&self, key: &Key, previous: Option<State>) -> State {
        let path = match key {
            Key::Type(type_path) => match find_schema_file(type_path, &self.schema_dirs()) {
                Ok(path) => path,
                Err(_) => return State::Missing,
            },
            Key::File(path) => path.clone(),
        };
        let Ok(metadata) = fs::metadata(&path) else {
            return State::Missing;
        };
        let modified = metadata.modified().ok();

        if let Some(previous) = previous
            && modified.is_some()
            && previous.file() == Some((&path, modified))
        {
            return previous;
        }

        match read_schema(&path) {
            Ok(schema) => State::Found {
                path,
                modified,
                schema: Arc::new(schema),
            },
            Err(e) => State::Invalid {
                path,
                modified,
                message: e.to_string(),
            },
        }
    }
}

impl Cache {
    fn clear(&mut self) {
        self.generation += 1;
        self.entries.clear();
    }

    /// Insert an entry loaded during `generation`, unless the cache was
    /// cleared since.
    fn insert(&mut self, generation: u64, key: Key, entry: Entry) {
        if generation != self.generation {
            return;
        }
        if self.entries.len() >= MAX_ENTRIES && !self.entries.contains_key(&key) {
            self.entries
                .retain(|_, e| matches!(e.state, State::Found { .. }));
        }
        self.entries.insert(key, entry);
    }
}

impl Default for SchemaResolver {
    fn default() -> Self {
        Self::new()
    }
}

impl SchemaResolverTrait for SchemaResolver {
    fn resolve(&self, type_path: &str) -> Option<Arc<Schema>> {
        self.load_schema_by_type(type_path)
    }
}

#[cfg(test)]
mod tests {
    use std::{
        path::Path,
        sync::atomic::{AtomicUsize, Ordering},
    };

    use ron2::schema::{TypeKind, write_schema};

    use super::*;

    /// A fresh directory, removed on drop.
    struct TempDir(PathBuf);

    impl TempDir {
        fn path(&self) -> &Path {
            &self.0
        }
    }

    impl Drop for TempDir {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.0);
        }
    }

    fn tempdir() -> TempDir {
        static COUNTER: AtomicUsize = AtomicUsize::new(0);
        let dir = std::env::temp_dir().join(format!(
            "ron2-lsp-resolver-{}-{}",
            std::process::id(),
            COUNTER.fetch_add(1, Ordering::Relaxed)
        ));
        fs::create_dir_all(&dir).unwrap();
        TempDir(dir)
    }

    fn write(dir: &Path, type_path: &str, kind: TypeKind) -> PathBuf {
        write_schema(type_path, &Schema::new(kind), Some(dir)).unwrap()
    }

    #[test]
    fn reloads_schema_when_file_changes() {
        let dir = tempdir();
        let resolver = SchemaResolver::with_revalidate_after(Duration::ZERO);
        resolver.set_schema_dirs(vec![dir.path().to_path_buf()]);

        let path = write(dir.path(), "app::Config", TypeKind::Bool);
        assert_eq!(
            resolver.load_schema_by_type("app::Config").unwrap().kind,
            TypeKind::Bool
        );

        write(dir.path(), "app::Config", TypeKind::String);
        // Ensure a different mtime even on coarse-grained file systems.
        let later = SystemTime::now() + Duration::from_secs(5);
        fs::File::options()
            .write(true)
            .open(&path)
            .unwrap()
            .set_modified(later)
            .unwrap();

        assert_eq!(
            resolver.load_schema_by_type("app::Config").unwrap().kind,
            TypeKind::String
        );
    }

    #[test]
    fn caches_misses_until_revalidation() {
        let dir = tempdir();
        let resolver = SchemaResolver::with_revalidate_after(Duration::from_secs(3600));
        resolver.set_schema_dirs(vec![dir.path().to_path_buf()]);

        assert!(resolver.load_schema_by_type("app::Late").is_none());
        write(dir.path(), "app::Late", TypeKind::Bool);
        // Still within the revalidation interval: the miss is cached.
        assert!(resolver.load_schema_by_type("app::Late").is_none());

        let resolver = SchemaResolver::with_revalidate_after(Duration::ZERO);
        resolver.set_schema_dirs(vec![dir.path().to_path_buf()]);
        assert_eq!(
            resolver.load_schema_by_type("app::Late").unwrap().kind,
            TypeKind::Bool
        );
    }

    #[test]
    fn misses_in_first_dir_fall_through_to_second() {
        let first = tempdir();
        let second = tempdir();
        write(second.path(), "app::Only", TypeKind::Bool);

        let resolver = SchemaResolver::new();
        resolver.set_schema_dirs(vec![
            first.path().to_path_buf(),
            second.path().to_path_buf(),
        ]);
        assert_eq!(
            resolver.load_schema_by_type("app::Only").unwrap().kind,
            TypeKind::Bool
        );
    }

    #[test]
    fn rejects_type_paths_escaping_schema_dir() {
        let root = tempdir();
        let schemas = root.path().join("schemas");
        fs::create_dir_all(&schemas).unwrap();
        write(root.path(), "secret", TypeKind::Bool);

        let resolver = SchemaResolver::new();
        resolver.set_schema_dirs(vec![schemas]);
        assert!(resolver.load_schema_by_type("..::secret").is_none());
    }

    #[test]
    fn malformed_schema_is_reported_as_invalid() {
        let dir = tempdir();
        let resolver = SchemaResolver::new();
        resolver.set_schema_dirs(vec![dir.path().to_path_buf()]);
        let path = dir.path().join("app/Broken.schema.ron");
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(&path, "(kind: Struct(fields: [").unwrap();

        let doc = Document::new(
            tower_lsp::lsp_types::Url::parse("file:///x.ron").unwrap(),
            "#![type = \"app::Broken\"]\n()".to_string(),
            1,
        );
        assert!(matches!(
            resolver.lookup_schema(&doc),
            Err(SchemaLookupError::Invalid { path: p, .. }) if p == path
        ));
        assert!(matches!(
            resolver.lookup(Key::Type("app::Missing".into())),
            Err(SchemaLookupError::NotFound)
        ));
    }

    #[test]
    fn generic_type_attribute_resolves() {
        let dir = tempdir();
        write(dir.path(), "a::W<x::Y<u8>, i32>", TypeKind::Bool);
        let resolver = SchemaResolver::new();
        resolver.set_schema_dirs(vec![dir.path().to_path_buf()]);

        let doc = Document::new(
            tower_lsp::lsp_types::Url::parse("file:///x.ron").unwrap(),
            "#![type = \"a::W<x::Y<u8>, i32>\"]\ntrue".to_string(),
            1,
        );
        assert_eq!(resolver.resolve_schema(&doc).unwrap().kind, TypeKind::Bool);
        assert!(resolver.load_schema_by_type("a::W<x::Y<u8>,i32>").is_none());
    }

    #[test]
    fn entries_loaded_before_a_clear_are_dropped() {
        let mut cache = Cache::default();
        let stale = cache.generation;
        cache.clear();
        let entry = || Entry {
            checked: Instant::now(),
            state: State::Missing,
        };
        cache.insert(stale, Key::Type("a::A".into()), entry());
        assert!(cache.entries.is_empty());
        cache.insert(cache.generation, Key::Type("a::A".into()), entry());
        assert_eq!(cache.entries.len(), 1);
    }

    #[test]
    fn misses_are_bounded() {
        let resolver = SchemaResolver::new();
        resolver.set_schema_dirs(vec![tempdir().path().to_path_buf()]);
        // Typing `#![type = "app::Config"]` one character at a time.
        for i in 0..MAX_ENTRIES * 3 {
            assert!(
                resolver
                    .load_schema_by_type(&format!("app::T{i}"))
                    .is_none()
            );
        }
        assert!(resolver.cache().entries.len() <= MAX_ENTRIES);
    }
}
