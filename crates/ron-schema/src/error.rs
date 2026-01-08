//! Error types for SerRon and DeRon operations.

use std::fmt;

/// Errors that can occur during RON serialization or deserialization.
#[derive(Debug)]
pub enum RonError {
    /// RON parsing error.
    Parse(ron::error::SpannedError),
    /// Type mismatch during deserialization.
    TypeMismatch {
        expected: &'static str,
        found: String,
    },
    /// Missing required field in struct.
    MissingField(String),
    /// Unknown field encountered (when deny_unknown_fields is set).
    UnknownField(String),
    /// Unknown enum variant.
    UnknownVariant(String),
    /// Integer value out of range for target type.
    IntegerOutOfRange {
        ty: &'static str,
        value: String,
    },
    /// Invalid value for the target type.
    InvalidValue(String),
    /// IO error.
    Io(std::io::Error),
    /// Formatting error during serialization.
    Fmt(fmt::Error),
    /// RON serialization error.
    Serialize(ron::Error),
    /// Custom error message.
    Custom(String),
}

impl std::error::Error for RonError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            RonError::Parse(e) => Some(e),
            RonError::Io(e) => Some(e),
            RonError::Fmt(e) => Some(e),
            RonError::Serialize(e) => Some(e),
            _ => None,
        }
    }
}

impl fmt::Display for RonError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RonError::Parse(e) => write!(f, "RON parse error: {}", e),
            RonError::TypeMismatch { expected, found } => {
                write!(f, "type mismatch: expected {}, found {}", expected, found)
            }
            RonError::MissingField(field) => write!(f, "missing required field: {}", field),
            RonError::UnknownField(field) => write!(f, "unknown field: {}", field),
            RonError::UnknownVariant(variant) => write!(f, "unknown variant: {}", variant),
            RonError::IntegerOutOfRange { ty, value } => {
                write!(f, "integer {} out of range for type {}", value, ty)
            }
            RonError::InvalidValue(msg) => write!(f, "invalid value: {}", msg),
            RonError::Io(e) => write!(f, "IO error: {}", e),
            RonError::Fmt(e) => write!(f, "format error: {}", e),
            RonError::Serialize(e) => write!(f, "serialization error: {}", e),
            RonError::Custom(msg) => write!(f, "{}", msg),
        }
    }
}

impl From<ron::error::SpannedError> for RonError {
    fn from(e: ron::error::SpannedError) -> Self {
        RonError::Parse(e)
    }
}

impl From<std::io::Error> for RonError {
    fn from(e: std::io::Error) -> Self {
        RonError::Io(e)
    }
}

impl From<fmt::Error> for RonError {
    fn from(e: fmt::Error) -> Self {
        RonError::Fmt(e)
    }
}

impl From<ron::Error> for RonError {
    fn from(e: ron::Error) -> Self {
        RonError::Serialize(e)
    }
}

impl RonError {
    /// Create a custom error with the given message.
    pub fn custom(msg: impl Into<String>) -> Self {
        RonError::Custom(msg.into())
    }

    /// Create a type mismatch error.
    pub fn type_mismatch(expected: &'static str, found: impl fmt::Debug) -> Self {
        RonError::TypeMismatch {
            expected,
            found: format!("{:?}", found),
        }
    }
}
