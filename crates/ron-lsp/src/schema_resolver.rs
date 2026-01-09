//! Schema resolution for the RON Language Server.
//!
//! This module handles loading and caching schemas based on
//! document attributes.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::RwLock;

use ron_schema::{
    find_schema, find_schema_in, read_schema, Schema, SchemaResolver as SchemaResolverTrait,
};

use crate::document::Document;

/// Resolves and caches schemas for RON documents.
pub struct SchemaResolver {
    /// Cache of loaded schemas by path.
    cache: RwLock<HashMap<PathBuf, Schema>>,
    /// Configured schema directories to search (from workspace config).
    schema_dirs: RwLock<Vec<PathBuf>>,
}

impl SchemaResolver {
    /// Create a new schema resolver.
    pub fn new() -> Self {
        Self {
            cache: RwLock::new(HashMap::new()),
            schema_dirs: RwLock::new(Vec::new()),
        }
    }

    /// Update the schema directories from workspace configuration.
    pub fn set_schema_dirs(&self, dirs: Vec<PathBuf>) {
        if let Ok(mut schema_dirs) = self.schema_dirs.write() {
            *schema_dirs = dirs;
        }
        // Clear cache when config changes
        self.clear_cache();
    }

    /// Get the current schema directories.
    pub fn schema_dirs(&self) -> Vec<PathBuf> {
        self.schema_dirs
            .read()
            .map(|dirs| dirs.clone())
            .unwrap_or_default()
    }

    /// Resolve the schema for a document.
    ///
    /// Resolution order:
    /// 1. `#![schema = "path"]` - explicit path relative to the RON file
    /// 2. `#![type = "crate::Type"]` - search in configured dirs, RON_SCHEMA_DIR, or XDG default
    pub fn resolve_schema(&self, doc: &Document) -> Option<Schema> {
        // Try explicit schema path first
        if let Some(ref schema_path) = doc.schema_attr {
            return self.load_schema_by_path(doc, schema_path);
        }

        // Try type-based resolution
        if let Some(ref type_path) = doc.type_attr {
            return self.load_schema_by_type(type_path);
        }

        None
    }

    /// Load a schema from an explicit path (relative to the document).
    fn load_schema_by_path(&self, doc: &Document, schema_path: &str) -> Option<Schema> {
        let doc_path = doc.file_path()?;
        let doc_dir = doc_path.parent()?;
        let full_path = doc_dir.join(schema_path).canonicalize().ok()?;

        self.load_schema_file(&full_path)
    }

    /// Load a schema by type path.
    ///
    /// Search order:
    /// 1. Configured schema directories (from workspace config)
    /// 2. RON_SCHEMA_DIR environment variable
    /// 3. XDG default location
    pub fn load_schema_by_type(&self, type_path: &str) -> Option<Schema> {
        // Check cache first
        let cache_key = PathBuf::from(format!("type:{}", type_path));
        if let Some(schema) = self.get_cached(&cache_key) {
            return Some(schema);
        }

        // Try configured schema directories first
        let dirs = self.schema_dirs();
        for dir in &dirs {
            if let Ok(schema) = find_schema_in(type_path, dir) {
                self.cache_schema(cache_key, schema.clone());
                return Some(schema);
            }
        }

        // Fall back to ron_schema's find_schema (checks RON_SCHEMA_DIR and XDG)
        match find_schema(type_path) {
            Ok(schema) => {
                self.cache_schema(cache_key, schema.clone());
                Some(schema)
            }
            Err(_) => None,
        }
    }

    /// Load a schema from a file path.
    fn load_schema_file(&self, path: &Path) -> Option<Schema> {
        // Check cache first
        if let Some(schema) = self.get_cached(path) {
            return Some(schema);
        }

        // Load from disk
        match read_schema(path) {
            Ok(schema) => {
                self.cache_schema(path.to_path_buf(), schema.clone());
                Some(schema)
            }
            Err(_) => None,
        }
    }

    /// Get a cached schema.
    fn get_cached(&self, path: &Path) -> Option<Schema> {
        self.cache.read().ok()?.get(path).cloned()
    }

    /// Cache a schema.
    fn cache_schema(&self, path: PathBuf, schema: Schema) {
        if let Ok(mut cache) = self.cache.write() {
            cache.insert(path, schema);
        }
    }

    /// Clear the schema cache.
    pub fn clear_cache(&self) {
        if let Ok(mut cache) = self.cache.write() {
            cache.clear();
        }
    }
}

impl Default for SchemaResolver {
    fn default() -> Self {
        Self::new()
    }
}

impl SchemaResolverTrait for SchemaResolver {
    fn resolve(&self, type_path: &str) -> Option<Schema> {
        self.load_schema_by_type(type_path)
    }
}

/// Wrapper that implements ron_schema::SchemaResolver for &SchemaResolver.
///
/// This is needed because validate_with_resolver takes the resolver by reference,
/// and we need to pass our SchemaResolver which is behind an Arc.
impl SchemaResolverTrait for &SchemaResolver {
    fn resolve(&self, type_path: &str) -> Option<Schema> {
        self.load_schema_by_type(type_path)
    }
}
