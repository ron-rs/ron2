//! Schema file discovery and loading.

use std::path::{Path, PathBuf};

use anyhow::{Context, Result};
use ron2::schema::{storage::read_schema, Schema};
use walkdir::WalkDir;

/// A discovered schema with its metadata.
#[derive(Debug)]
pub struct DiscoveredSchema {
    /// Path to the schema file
    pub path: PathBuf,
    /// Derived type path (e.g., "my_crate::Config")
    pub type_path: String,
    /// Parsed schema
    pub schema: Schema,
}

/// Discover all schema files in a directory or load a single file.
pub fn discover_schemas(input: &Path) -> Result<Vec<DiscoveredSchema>> {
    if input.is_file() {
        // Single file mode
        let schema = read_schema(input)
            .with_context(|| format!("failed to read schema from {}", input.display()))?;
        let type_path = file_path_to_type_path(input, input.parent().unwrap_or(input));
        return Ok(vec![DiscoveredSchema {
            path: input.to_path_buf(),
            type_path,
            schema,
        }]);
    }

    // Directory mode - walk and find all .schema.ron files
    let mut schemas = Vec::new();

    for entry in WalkDir::new(input)
        .follow_links(true)
        .into_iter()
        .filter_map(|e| e.ok())
    {
        let path = entry.path();
        if path.is_file() && path.to_string_lossy().ends_with(".schema.ron") {
            let schema = read_schema(path)
                .with_context(|| format!("failed to read schema from {}", path.display()))?;
            let type_path = file_path_to_type_path(path, input);
            schemas.push(DiscoveredSchema {
                path: path.to_path_buf(),
                type_path,
                schema,
            });
        }
    }

    // Sort by type path for consistent output
    schemas.sort_by(|a, b| a.type_path.cmp(&b.type_path));

    Ok(schemas)
}

/// Convert a file path to a type path.
///
/// Example: `base/my_crate/Config.schema.ron` -> `my_crate::Config`
fn file_path_to_type_path(path: &Path, base: &Path) -> String {
    // Get relative path from base
    let relative = path
        .strip_prefix(base)
        .unwrap_or(path)
        .with_extension("") // Remove .ron
        .with_extension(""); // Remove .schema

    // Convert path separators to ::
    relative
        .components()
        .map(|c| c.as_os_str().to_string_lossy().to_string())
        .collect::<Vec<_>>()
        .join("::")
}

#[cfg(test)]
mod tests {
    use ron2::schema::{write_schema, Field, TypeKind};

    use super::*;

    #[test]
    fn test_file_path_to_type_path() {
        let base = Path::new("/schemas");
        let path = Path::new("/schemas/my_crate/config/AppConfig.schema.ron");
        assert_eq!(
            file_path_to_type_path(path, base),
            "my_crate::config::AppConfig"
        );
    }

    #[test]
    fn test_file_path_to_type_path_single_component() {
        let base = Path::new("/schemas");
        let path = Path::new("/schemas/Config.schema.ron");
        assert_eq!(file_path_to_type_path(path, base), "Config");
    }

    #[test]
    fn test_discover_schemas_preserves_generic_typeref() {
        let temp_dir = tempfile::tempdir().expect("temp dir");
        let schema = Schema::new(TypeKind::Struct {
            fields: vec![Field::new(
                "value",
                TypeKind::TypeRef("my_crate::Wrapper<T>".to_string()),
            )],
        });

        write_schema("my_crate::Generic", &schema, Some(temp_dir.path())).expect("write schema");

        let discovered = discover_schemas(temp_dir.path()).expect("discover schemas");
        assert_eq!(discovered.len(), 1);
        assert_eq!(discovered[0].type_path, "my_crate::Generic");
        assert_eq!(discovered[0].schema, schema);
    }
}
