//! Unified error type for schema operations.
//!
//! This module provides error types for schema operations:
//! - [`ValidationError`], [`ValidationErrorKind`], [`PathSegment`] - Re-exported from `ron-error`
//! - [`StorageError`] - Errors related to schema file I/O
//! - [`SchemaError`] - Combined error type for all schema operations

use std::fmt;
use std::io;

// Re-export validation error types from ron-error
pub use ron_error::{PathSegment, Position, Span, ValidationError, ValidationErrorKind};

/// Errors related to schema storage operations.
#[derive(Debug, Clone)]
pub enum StorageError {
    /// IO error during file operations.
    Io(String),
    /// RON parsing error.
    Parse(String),
    /// Could not determine schema directory.
    NoSchemaDir,
    /// Schema file not found for the given type.
    SchemaNotFound(String),
}

impl fmt::Display for StorageError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StorageError::Io(msg) => write!(f, "IO error: {}", msg),
            StorageError::Parse(msg) => write!(f, "parse error: {}", msg),
            StorageError::NoSchemaDir => write!(f, "could not determine schema directory"),
            StorageError::SchemaNotFound(ty) => write!(f, "schema not found for type: {}", ty),
        }
    }
}

impl std::error::Error for StorageError {}

/// Errors that can occur during schema operations.
///
/// This combines storage errors (file I/O) and validation errors
/// into a single error type for convenience.
#[derive(Debug, Clone)]
pub enum SchemaError {
    /// Storage-related error (file I/O, parsing schema files).
    Storage(StorageError),
    /// Validation error (type mismatches, missing fields, etc.).
    Validation(ValidationError),
}

impl SchemaError {
    /// Create a new storage error.
    pub fn storage(err: StorageError) -> Self {
        SchemaError::Storage(err)
    }

    /// Create a new validation error.
    pub fn validation(err: ValidationError) -> Self {
        SchemaError::Validation(err)
    }

    /// Check if this is a storage-related error.
    pub fn is_storage_error(&self) -> bool {
        matches!(self, SchemaError::Storage(_))
    }

    /// Check if this is a validation error.
    pub fn is_validation_error(&self) -> bool {
        matches!(self, SchemaError::Validation(_))
    }

    // =========================================================================
    // Convenience constructors for storage errors
    // =========================================================================

    /// Create a schema not found error.
    pub fn schema_not_found(type_path: impl Into<String>) -> Self {
        SchemaError::Storage(StorageError::SchemaNotFound(type_path.into()))
    }

    /// Create a no schema dir error.
    pub fn no_schema_dir() -> Self {
        SchemaError::Storage(StorageError::NoSchemaDir)
    }

    // =========================================================================
    // Convenience constructors delegating to ValidationError
    // =========================================================================

    /// Create a type mismatch error.
    pub fn type_mismatch(expected: impl Into<String>, actual: impl Into<String>) -> Self {
        SchemaError::Validation(ValidationError::type_mismatch(expected, actual))
    }

    /// Create a missing field error.
    pub fn missing_field(field: impl Into<String>) -> Self {
        SchemaError::Validation(ValidationError::missing_field(field.into()))
    }

    /// Create an unknown field error.
    pub fn unknown_field(field: impl Into<String>) -> Self {
        SchemaError::Validation(ValidationError::unknown_field(field.into(), &[]))
    }

    /// Create an unknown variant error.
    pub fn unknown_variant(variant: impl Into<String>) -> Self {
        SchemaError::Validation(ValidationError::unknown_variant(variant.into(), &[]))
    }

    /// Create a tuple length mismatch error.
    pub fn tuple_length_mismatch(expected: usize, actual: usize) -> Self {
        SchemaError::Validation(ValidationError::length_mismatch(expected, actual))
    }

    // =========================================================================
    // Context builders - delegate to inner ValidationError
    // =========================================================================

    /// Add a field context to this error's path.
    #[must_use]
    pub fn in_field(self, name: impl Into<String>) -> Self {
        match self {
            SchemaError::Validation(e) => SchemaError::Validation(e.in_field(name)),
            other => other,
        }
    }

    /// Add an element context to this error's path.
    #[must_use]
    pub fn in_element(self, index: usize) -> Self {
        match self {
            SchemaError::Validation(e) => SchemaError::Validation(e.in_element(index)),
            other => other,
        }
    }

    /// Add a map key context to this error's path.
    #[must_use]
    pub fn in_map_key(self) -> Self {
        match self {
            SchemaError::Validation(e) => SchemaError::Validation(e.in_map_key()),
            other => other,
        }
    }

    /// Add a map value context to this error's path.
    #[must_use]
    pub fn in_map_value(self, key: impl Into<String>) -> Self {
        match self {
            SchemaError::Validation(e) => SchemaError::Validation(e.in_map_value(key)),
            other => other,
        }
    }

    /// Add a variant context to this error's path.
    #[must_use]
    pub fn in_variant(self, name: impl Into<String>) -> Self {
        match self {
            SchemaError::Validation(e) => SchemaError::Validation(e.in_variant(name)),
            other => other,
        }
    }

    /// Add a type ref context to this error's path.
    #[must_use]
    pub fn in_type_ref(self, type_path: impl Into<String>) -> Self {
        match self {
            SchemaError::Validation(e) => SchemaError::Validation(e.in_type_ref(type_path)),
            other => other,
        }
    }
}

impl fmt::Display for SchemaError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SchemaError::Storage(e) => write!(f, "{}", e),
            SchemaError::Validation(e) => write!(f, "{}", e),
        }
    }
}

impl std::error::Error for SchemaError {}

impl From<io::Error> for SchemaError {
    fn from(e: io::Error) -> Self {
        SchemaError::Storage(StorageError::Io(e.to_string()))
    }
}

impl From<ron2::SpannedError> for SchemaError {
    fn from(e: ron2::SpannedError) -> Self {
        SchemaError::Storage(StorageError::Parse(e.to_string()))
    }
}

impl From<ron2::Error> for SchemaError {
    fn from(e: ron2::Error) -> Self {
        SchemaError::Storage(StorageError::Parse(e.to_string()))
    }
}

impl From<ValidationError> for SchemaError {
    fn from(e: ValidationError) -> Self {
        SchemaError::Validation(e)
    }
}

impl From<StorageError> for SchemaError {
    fn from(e: StorageError) -> Self {
        SchemaError::Storage(e)
    }
}

/// Result type for schema operations.
pub type Result<T> = std::result::Result<T, SchemaError>;

/// Result type for validation-only operations.
pub type ValidationResult<T> = std::result::Result<T, ValidationError>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_display_no_path() {
        let err = SchemaError::type_mismatch("String", "Bool");
        assert_eq!(err.to_string(), "expected String but found Bool");
    }

    #[test]
    fn test_error_display_with_path() {
        let err = SchemaError::type_mismatch("i32", "String")
            .in_element(0)
            .in_field("items");
        assert_eq!(
            err.to_string(),
            "in field 'items' -> element 0: expected i32 but found String"
        );
    }

    #[test]
    fn test_error_display_nested_path() {
        let err = SchemaError::missing_field("name")
            .in_variant("Some")
            .in_field("data")
            .in_type_ref("my::Type");
        assert_eq!(
            err.to_string(),
            "in type 'my::Type' -> field 'data' -> variant 'Some': missing required field `name`"
        );
    }

    #[test]
    fn test_context_methods() {
        let err = SchemaError::unknown_field("bad_field")
            .in_map_value("key1")
            .in_field("config");

        // Verify it's a validation error
        assert!(err.is_validation_error());
        assert!(!err.is_storage_error());
    }

    #[test]
    fn test_error_categories() {
        assert!(SchemaError::no_schema_dir().is_storage_error());
        assert!(SchemaError::schema_not_found("Foo").is_storage_error());
        assert!(SchemaError::type_mismatch("a", "b").is_validation_error());
        assert!(SchemaError::missing_field("x").is_validation_error());
    }
}
