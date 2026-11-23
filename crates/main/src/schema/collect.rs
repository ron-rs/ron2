//! Collect the schemas of a type and every type it references.

use alloc::{
    collections::{BTreeMap, VecDeque},
    sync::Arc,
};
#[cfg(feature = "derive")]
use std::path::{Path, PathBuf};

#[cfg(feature = "derive")]
use crate::schema::write_schema;
use crate::schema::{RonSchema, Schema, SchemaError, SchemaResolver, traits::require_type_path};

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
///
/// A catalog is a [`SchemaResolver`], so a type's schemas can be validated
/// against without writing them to disk.
#[derive(Clone, Debug, Default)]
pub struct SchemaCatalog {
    /// Schemas keyed by fully-qualified type path.
    pub schemas: BTreeMap<String, Arc<Schema>>,
}

impl SchemaCatalog {
    /// Write all schemas in this catalog to disk.
    #[cfg(feature = "derive")]
    pub fn write_all(&self, output_dir: Option<&Path>) -> Result<Vec<PathBuf>, SchemaError> {
        self.schemas
            .iter()
            .map(|(type_path, schema)| write_schema(type_path, schema, output_dir))
            .collect()
    }
}

impl SchemaResolver for SchemaCatalog {
    fn resolve(&self, type_path: &str) -> Option<Arc<Schema>> {
        self.schemas.get(type_path).cloned()
    }
}

/// Collect schemas recursively starting from `T`.
pub fn collect_schemas<T: RonSchema>() -> Result<SchemaCatalog, SchemaError> {
    let root = SchemaEntry {
        type_path: require_type_path::<T>()?,
        schema: T::schema,
        children: T::child_schemas,
    };

    let mut catalog = SchemaCatalog::default();
    let mut queue = VecDeque::from([&root]);
    while let Some(entry) = queue.pop_front() {
        if catalog.schemas.contains_key(entry.type_path) {
            continue;
        }
        catalog
            .schemas
            .insert(entry.type_path.to_string(), Arc::new((entry.schema)()));
        queue.extend((entry.children)());
    }
    Ok(catalog)
}

/// Collect schemas recursively starting from `T` and write them to `output_dir`.
#[cfg(feature = "derive")]
pub fn write_schemas<T: RonSchema>(output_dir: Option<&str>) -> Result<Vec<PathBuf>, SchemaError> {
    collect_schemas::<T>()?.write_all(output_dir.map(Path::new))
}
