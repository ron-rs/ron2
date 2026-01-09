//! Unified error type for schema operations.

use std::io;

/// Errors that can occur during schema operations (storage, validation, etc.).
#[derive(Debug, thiserror::Error)]
pub enum SchemaError {
    // Storage errors
    #[error("IO error: {0}")]
    Io(#[from] io::Error),
    #[error("RON parse error: {0}")]
    Parse(#[from] ron2::SpannedError),
    #[error("RON error: {0}")]
    Ron(#[from] ron2::Error),
    #[error("Could not determine schema directory")]
    NoSchemaDir,
    #[error("Schema not found for type: {0}")]
    SchemaNotFound(String),

    // Validation errors
    #[error("Type mismatch: expected {expected}, got {actual}")]
    TypeMismatch { expected: String, actual: String },
    #[error("Missing required field: {0}")]
    MissingField(String),
    #[error("Unknown field: {0}")]
    UnknownField(String),
    #[error("Unknown enum variant: {0}")]
    UnknownVariant(String),
    #[error("Invalid tuple length: expected {expected}, got {actual}")]
    TupleLengthMismatch { expected: usize, actual: usize },

    // Nested context wrappers
    #[error("in field '{field}': {source}")]
    FieldError {
        field: String,
        #[source]
        source: Box<SchemaError>,
    },
    #[error("in element {index}: {source}")]
    ElementError {
        index: usize,
        #[source]
        source: Box<SchemaError>,
    },
    #[error("in map key: {source}")]
    MapKeyError {
        #[source]
        source: Box<SchemaError>,
    },
    #[error("in map value for key '{key}': {source}")]
    MapValueError {
        key: String,
        #[source]
        source: Box<SchemaError>,
    },
    #[error("in variant '{variant}': {source}")]
    VariantError {
        variant: String,
        #[source]
        source: Box<SchemaError>,
    },
    #[error("in TypeRef '{type_path}': {source}")]
    TypeRefError {
        type_path: String,
        #[source]
        source: Box<SchemaError>,
    },
}

/// Result type for schema operations.
pub type Result<T> = std::result::Result<T, SchemaError>;
