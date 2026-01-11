use std::path::{Path, PathBuf};
use std::{env, fs};

use crate::error::{Result, SchemaError};
use crate::Schema;
use ron2::{FromRon, ToRon};

/// Environment variable for schema directory override.
pub const SCHEMA_DIR_ENV: &str = "RON_SCHEMA_DIR";

/// Default schema directory name under XDG data dir.
const DEFAULT_SCHEMA_DIR: &str = "ron-schemas";

/// Schema file extension.
const SCHEMA_EXTENSION: &str = "schema.ron";

/// Resolve the schema directory based on precedence:
/// 1. Environment variable `RON_SCHEMA_DIR`
/// 2. XDG data directory (`~/.local/share/ron-schemas/`)
pub fn resolve_schema_dir() -> Result<PathBuf> {
    // Check environment variable first
    if let Ok(dir) = env::var(SCHEMA_DIR_ENV) {
        return Ok(PathBuf::from(dir));
    }

    // Fall back to XDG data directory
    dirs::data_dir()
        .map(|d| d.join(DEFAULT_SCHEMA_DIR))
        .ok_or_else(SchemaError::no_schema_dir)
}

/// Convert a Rust type path to a schema file path.
///
/// Example: `my_crate::config::AppConfig` -> `my_crate/config/AppConfig.schema.ron`
pub fn type_path_to_file_path(type_path: &str) -> PathBuf {
    let parts: Vec<&str> = type_path.split("::").collect();
    let mut path = PathBuf::new();

    for (i, part) in parts.iter().enumerate() {
        if i == parts.len() - 1 {
            // Last part is the type name
            path.push(format!("{}.{}", part, SCHEMA_EXTENSION));
        } else {
            path.push(part);
        }
    }

    path
}

/// Write a schema to the configured schema directory.
///
/// If `output_dir` is provided, it takes precedence over environment variable and XDG default.
pub fn write_schema(
    type_path: &str,
    schema: &Schema,
    output_dir: Option<&Path>,
) -> Result<PathBuf> {
    let base_dir = match output_dir {
        Some(dir) => dir.to_path_buf(),
        None => resolve_schema_dir()?,
    };

    let file_path = base_dir.join(type_path_to_file_path(type_path));

    // Ensure parent directory exists
    if let Some(parent) = file_path.parent() {
        fs::create_dir_all(parent)?;
    }

    // Serialize with pretty printing
    let value = schema.to_ron_value()?;
    let config = ron2::ser::PrettyConfig::default();
    let contents = ron2::ser::to_string_pretty(&value, config)?;

    fs::write(&file_path, contents)?;

    Ok(file_path)
}

/// Read a schema from a file path.
pub fn read_schema(path: &Path) -> Result<Schema> {
    let contents = fs::read_to_string(path)?;
    let value = ron2::from_str(&contents)?;
    let schema = Schema::from_ron_value(value)?;
    Ok(schema)
}

/// Find and read a schema by type path.
///
/// Searches in the schema directory resolved by `resolve_schema_dir()`.
pub fn find_schema(type_path: &str) -> Result<Schema> {
    let base_dir = resolve_schema_dir()?;
    let file_path = base_dir.join(type_path_to_file_path(type_path));

    if !file_path.exists() {
        return Err(SchemaError::schema_not_found(type_path));
    }

    read_schema(&file_path)
}

/// Find a schema by type path, searching in a specific directory first.
pub fn find_schema_in(type_path: &str, search_dir: &Path) -> Result<Schema> {
    let file_path = search_dir.join(type_path_to_file_path(type_path));

    if file_path.exists() {
        return read_schema(&file_path);
    }

    // Fall back to default location
    find_schema(type_path)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_path_to_file_path() {
        assert_eq!(
            type_path_to_file_path("my_crate::config::AppConfig"),
            PathBuf::from("my_crate/config/AppConfig.schema.ron")
        );

        assert_eq!(
            type_path_to_file_path("MyType"),
            PathBuf::from("MyType.schema.ron")
        );
    }
}
