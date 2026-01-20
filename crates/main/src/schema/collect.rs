use alloc::collections::{BTreeMap, VecDeque};
use std::collections::HashSet;
#[cfg(feature = "derive")]
use std::path::{Path, PathBuf};

#[cfg(feature = "derive")]
use crate::schema::write_schema;
use crate::schema::{RonSchemaType, Schema, SchemaError, StorageError};

/// A schema entry for recursive collection.
#[derive(Clone, Copy, Debug)]
pub struct SchemaEntry {
    /// Fully-qualified type path for this schema.
    pub type_path: &'static str,
    /// Build the schema for this type.
    pub schema: fn() -> Schema,
    /// Direct child schemas referenced by this type.
    pub children: fn() -> &'static [&'static SchemaEntry],
}

/// Collected schemas keyed by type path.
#[derive(Clone, Debug, Default)]
pub struct SchemaCatalog {
    /// Schemas keyed by fully-qualified type path.
    pub schemas: BTreeMap<String, Schema>,
}

impl SchemaCatalog {
    /// Write all schemas in this catalog to disk.
    #[cfg(feature = "derive")]
    pub fn write_all(&self, output_dir: Option<&Path>) -> Result<Vec<PathBuf>, SchemaError> {
        let mut written = Vec::with_capacity(self.schemas.len());
        for (type_path, schema) in &self.schemas {
            written.push(write_schema(type_path, schema, output_dir)?);
        }
        Ok(written)
    }
}

/// Collect schemas recursively starting from `T`.
pub fn collect_schemas<T: RonSchemaType>() -> Result<SchemaCatalog, SchemaError> {
    let type_path = T::type_path().ok_or_else(|| {
        SchemaError::Storage(StorageError::Io(
            "type does not support schema storage".to_string(),
        ))
    })?;

    let root = SchemaEntry {
        type_path,
        schema: T::schema,
        children: T::child_schemas,
    };

    Ok(collect_from_root(&root))
}

/// Collect schemas recursively starting from `T` and write them to `output_dir`.
#[cfg(feature = "derive")]
pub fn write_schemas<T: RonSchemaType>(
    output_dir: Option<&str>,
) -> Result<Vec<PathBuf>, SchemaError> {
    let catalog = collect_schemas::<T>()?;
    let output_path = output_dir.map(Path::new);
    catalog.write_all(output_path)
}

fn collect_from_root(root: &SchemaEntry) -> SchemaCatalog {
    let mut catalog = SchemaCatalog::default();
    let mut queue = VecDeque::new();
    let mut seen = HashSet::new();

    queue.push_back(root);

    while let Some(entry) = queue.pop_front() {
        if !seen.insert(entry.type_path) {
            continue;
        }

        let schema = (entry.schema)();
        catalog.schemas.insert(entry.type_path.to_string(), schema);

        for child in (entry.children)() {
            queue.push_back(child);
        }
    }

    catalog
}
