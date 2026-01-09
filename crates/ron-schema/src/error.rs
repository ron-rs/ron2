//! Unified error type for schema operations.
//!
//! Uses a context path approach instead of nested boxing for efficient
//! error tracking and readable error messages.

use std::fmt;
use std::io;

/// A segment in the error context path.
///
/// These segments describe the location within a data structure
/// where an error occurred.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PathSegment {
    /// Error occurred in a struct field.
    Field(String),
    /// Error occurred at a sequence/tuple element index.
    Element(usize),
    /// Error occurred in a map key.
    MapKey,
    /// Error occurred in a map value (includes key for context).
    MapValue(String),
    /// Error occurred in an enum variant.
    Variant(String),
    /// Error occurred in a TypeRef resolution.
    TypeRef(String),
}

impl fmt::Display for PathSegment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PathSegment::Field(name) => write!(f, "field '{}'", name),
            PathSegment::Element(idx) => write!(f, "element {}", idx),
            PathSegment::MapKey => write!(f, "map key"),
            PathSegment::MapValue(key) => write!(f, "map value for '{}'", key),
            PathSegment::Variant(name) => write!(f, "variant '{}'", name),
            PathSegment::TypeRef(path) => write!(f, "type '{}'", path),
        }
    }
}

/// The specific kind of schema error that occurred.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SchemaErrorKind {
    // Storage errors
    /// IO error during file operations.
    Io(String),
    /// RON parsing error.
    Parse(String),
    /// Could not determine schema directory.
    NoSchemaDir,
    /// Schema file not found for the given type.
    SchemaNotFound(String),

    // Validation errors
    /// Value type doesn't match expected schema type.
    TypeMismatch { expected: String, actual: String },
    /// Required field is missing from struct.
    MissingField(String),
    /// Field is not defined in schema.
    UnknownField(String),
    /// Enum variant is not defined in schema.
    UnknownVariant(String),
    /// Tuple/sequence has wrong number of elements.
    TupleLengthMismatch { expected: usize, actual: usize },
}

impl fmt::Display for SchemaErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SchemaErrorKind::Io(msg) => write!(f, "IO error: {}", msg),
            SchemaErrorKind::Parse(msg) => write!(f, "parse error: {}", msg),
            SchemaErrorKind::NoSchemaDir => write!(f, "could not determine schema directory"),
            SchemaErrorKind::SchemaNotFound(ty) => write!(f, "schema not found for type: {}", ty),
            SchemaErrorKind::TypeMismatch { expected, actual } => {
                write!(f, "type mismatch: expected {}, got {}", expected, actual)
            }
            SchemaErrorKind::MissingField(field) => write!(f, "missing required field: {}", field),
            SchemaErrorKind::UnknownField(field) => write!(f, "unknown field: {}", field),
            SchemaErrorKind::UnknownVariant(variant) => {
                write!(f, "unknown enum variant: {}", variant)
            }
            SchemaErrorKind::TupleLengthMismatch { expected, actual } => {
                write!(
                    f,
                    "tuple length mismatch: expected {} elements, got {}",
                    expected, actual
                )
            }
        }
    }
}

/// Errors that can occur during schema operations (storage, validation, etc.).
///
/// Uses a context path to track error location within nested structures,
/// avoiding deep allocation chains from nested boxing.
#[derive(Debug, Clone)]
pub struct SchemaError {
    /// The specific error that occurred.
    pub kind: SchemaErrorKind,
    /// Path segments describing where in the structure the error occurred.
    /// Empty for top-level errors.
    pub path: Vec<PathSegment>,
}

impl SchemaError {
    /// Create a new error with no context path.
    pub fn new(kind: SchemaErrorKind) -> Self {
        Self {
            kind,
            path: Vec::new(),
        }
    }

    /// Add a context segment to this error's path.
    ///
    /// Segments are prepended so the path reads from outermost to innermost.
    pub fn in_context(mut self, segment: PathSegment) -> Self {
        self.path.insert(0, segment);
        self
    }

    /// Add a field context to this error.
    pub fn in_field(self, name: impl Into<String>) -> Self {
        self.in_context(PathSegment::Field(name.into()))
    }

    /// Add an element context to this error.
    pub fn in_element(self, index: usize) -> Self {
        self.in_context(PathSegment::Element(index))
    }

    /// Add a map key context to this error.
    pub fn in_map_key(self) -> Self {
        self.in_context(PathSegment::MapKey)
    }

    /// Add a map value context to this error.
    pub fn in_map_value(self, key: impl Into<String>) -> Self {
        self.in_context(PathSegment::MapValue(key.into()))
    }

    /// Add a variant context to this error.
    pub fn in_variant(self, name: impl Into<String>) -> Self {
        self.in_context(PathSegment::Variant(name.into()))
    }

    /// Add a type ref context to this error.
    pub fn in_type_ref(self, type_path: impl Into<String>) -> Self {
        self.in_context(PathSegment::TypeRef(type_path.into()))
    }

    /// Get the innermost path segment, if any.
    pub fn innermost_segment(&self) -> Option<&PathSegment> {
        self.path.last()
    }

    /// Get the outermost path segment, if any.
    pub fn outermost_segment(&self) -> Option<&PathSegment> {
        self.path.first()
    }

    /// Check if this is a storage-related error.
    pub fn is_storage_error(&self) -> bool {
        matches!(
            self.kind,
            SchemaErrorKind::Io(_)
                | SchemaErrorKind::Parse(_)
                | SchemaErrorKind::NoSchemaDir
                | SchemaErrorKind::SchemaNotFound(_)
        )
    }

    /// Check if this is a validation error.
    pub fn is_validation_error(&self) -> bool {
        !self.is_storage_error()
    }

    // Convenience constructors for common errors

    /// Create a type mismatch error.
    pub fn type_mismatch(expected: impl Into<String>, actual: impl Into<String>) -> Self {
        Self::new(SchemaErrorKind::TypeMismatch {
            expected: expected.into(),
            actual: actual.into(),
        })
    }

    /// Create a missing field error.
    pub fn missing_field(field: impl Into<String>) -> Self {
        Self::new(SchemaErrorKind::MissingField(field.into()))
    }

    /// Create an unknown field error.
    pub fn unknown_field(field: impl Into<String>) -> Self {
        Self::new(SchemaErrorKind::UnknownField(field.into()))
    }

    /// Create an unknown variant error.
    pub fn unknown_variant(variant: impl Into<String>) -> Self {
        Self::new(SchemaErrorKind::UnknownVariant(variant.into()))
    }

    /// Create a tuple length mismatch error.
    pub fn tuple_length_mismatch(expected: usize, actual: usize) -> Self {
        Self::new(SchemaErrorKind::TupleLengthMismatch { expected, actual })
    }

    /// Create a schema not found error.
    pub fn schema_not_found(type_path: impl Into<String>) -> Self {
        Self::new(SchemaErrorKind::SchemaNotFound(type_path.into()))
    }
}

impl fmt::Display for SchemaError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.path.is_empty() {
            write!(f, "{}", self.kind)
        } else {
            // Format as: "in field 'x' -> element 0: type mismatch..."
            for (i, segment) in self.path.iter().enumerate() {
                if i == 0 {
                    write!(f, "in {}", segment)?;
                } else {
                    write!(f, " -> {}", segment)?;
                }
            }
            write!(f, ": {}", self.kind)
        }
    }
}

impl std::error::Error for SchemaError {}

impl From<io::Error> for SchemaError {
    fn from(e: io::Error) -> Self {
        Self::new(SchemaErrorKind::Io(e.to_string()))
    }
}

impl From<ron2::SpannedError> for SchemaError {
    fn from(e: ron2::SpannedError) -> Self {
        Self::new(SchemaErrorKind::Parse(e.to_string()))
    }
}

impl From<ron2::Error> for SchemaError {
    fn from(e: ron2::Error) -> Self {
        Self::new(SchemaErrorKind::Parse(e.to_string()))
    }
}

/// Result type for schema operations.
pub type Result<T> = std::result::Result<T, SchemaError>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_display_no_path() {
        let err = SchemaError::type_mismatch("String", "Bool");
        assert_eq!(err.to_string(), "type mismatch: expected String, got Bool");
    }

    #[test]
    fn test_error_display_with_path() {
        let err = SchemaError::type_mismatch("i32", "String")
            .in_element(0)
            .in_field("items");
        assert_eq!(
            err.to_string(),
            "in field 'items' -> element 0: type mismatch: expected i32, got String"
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
            "in type 'my::Type' -> field 'data' -> variant 'Some': missing required field: name"
        );
    }

    #[test]
    fn test_context_methods() {
        let err = SchemaError::unknown_field("bad_field")
            .in_map_value("key1")
            .in_field("config");

        assert_eq!(
            err.outermost_segment(),
            Some(&PathSegment::Field("config".to_string()))
        );
        assert_eq!(
            err.innermost_segment(),
            Some(&PathSegment::MapValue("key1".to_string()))
        );
    }

    #[test]
    fn test_error_categories() {
        assert!(SchemaError::new(SchemaErrorKind::NoSchemaDir).is_storage_error());
        assert!(SchemaError::schema_not_found("Foo").is_storage_error());
        assert!(SchemaError::type_mismatch("a", "b").is_validation_error());
        assert!(SchemaError::missing_field("x").is_validation_error());
    }
}
