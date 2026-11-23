//! Collect the schemas of a type and every type it references.

use alloc::{collections::BTreeMap, sync::Arc};
#[cfg(feature = "derive")]
use std::path::{Path, PathBuf};

#[cfg(feature = "derive")]
use crate::schema::write_schema;
use crate::schema::{RonSchema, Schema, SchemaError, SchemaResolver, traits::require_type_path};

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
    /// Add `T`'s schema under its type path.
    ///
    /// Returns `false` without doing anything if `T` has no type path or is
    /// already in the catalog, so that registering recursive types terminates.
    pub fn insert<T: RonSchema + ?Sized>(&mut self) -> bool {
        let Some(type_path) = T::type_path() else {
            return false;
        };
        if self.schemas.contains_key(type_path) {
            return false;
        }
        self.schemas
            .insert(type_path.to_string(), Arc::new(T::schema()));
        true
    }

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

/// Collect the schemas of `T` and every type it references.
///
/// Fails if `T` has no type path of its own (primitives and standard library
/// types).
pub fn collect_schemas<T: RonSchema + ?Sized>() -> Result<SchemaCatalog, SchemaError> {
    require_type_path::<T>()?;
    let mut catalog = SchemaCatalog::default();
    T::register_schemas(&mut catalog);
    Ok(catalog)
}

/// Collect schemas recursively starting from `T` and write them to `output_dir`.
#[cfg(feature = "derive")]
pub fn write_schemas<T: RonSchema + ?Sized>(
    output_dir: Option<&str>,
) -> Result<Vec<PathBuf>, SchemaError> {
    collect_schemas::<T>()?.write_all(output_dir.map(Path::new))
}
