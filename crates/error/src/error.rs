//! Validation error types.

use alloc::{borrow::Cow, string::String, vec::Vec};
use core::fmt;

use crate::{PathSegment, Span};

/// The specific kind of validation error that occurred.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValidationErrorKind {
    /// Expected one type, found another.
    TypeMismatch {
        /// The expected type description.
        expected: String,
        /// The found type description.
        found: String,
    },

    /// Missing required struct field.
    MissingField {
        /// The name of the missing field.
        field: Cow<'static, str>,
        /// The containing struct/enum name, if known.
        outer: Option<Cow<'static, str>>,
    },

    /// Unknown struct field.
    UnknownField {
        /// The name of the unknown field.
        field: Cow<'static, str>,
        /// The list of expected field names.
        expected: &'static [&'static str],
        /// The containing struct/enum name, if known.
        outer: Option<Cow<'static, str>>,
    },

    /// Unknown enum variant.
    UnknownVariant {
        /// The name of the unknown variant.
        variant: Cow<'static, str>,
        /// The list of expected variant names.
        expected: &'static [&'static str],
        /// The enum name, if known.
        outer: Option<Cow<'static, str>>,
    },

    /// Duplicate struct field.
    DuplicateField {
        /// The name of the duplicated field.
        field: Cow<'static, str>,
        /// The containing struct/enum name, if known.
        outer: Option<Cow<'static, str>>,
    },

    /// Wrong number of elements (tuple, array, etc.).
    LengthMismatch {
        /// The expected number of elements.
        expected: usize,
        /// The found number of elements.
        found: usize,
        /// Optional context (e.g., "tuple", "array").
        context: Option<&'static str>,
    },

    /// Integer out of bounds for target type.
    IntegerOutOfBounds {
        /// The string representation of the out-of-bounds value.
        value: Cow<'static, str>,
        /// The target type that couldn't hold the value.
        target_type: &'static str,
    },
}

impl fmt::Display for ValidationErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ValidationErrorKind::TypeMismatch { expected, found } => {
                write!(f, "expected {expected} but found {found}")
            }
            ValidationErrorKind::MissingField { field, outer } => {
                write!(f, "missing required field `{field}`")?;
                if let Some(outer) = outer {
                    write!(f, " in `{outer}`")?;
                }
                Ok(())
            }
            ValidationErrorKind::UnknownField {
                field,
                expected,
                outer,
            } => {
                write!(f, "unknown field `{field}`")?;
                if let Some(outer) = outer {
                    write!(f, " in `{outer}`")?;
                }
                write!(f, ", ")?;
                format_expected_list(f, expected, "fields")
            }
            ValidationErrorKind::UnknownVariant {
                variant,
                expected,
                outer,
            } => {
                write!(f, "unknown variant `{variant}`")?;
                if let Some(outer) = outer {
                    write!(f, " in enum `{outer}`")?;
                }
                write!(f, ", ")?;
                format_expected_list(f, expected, "variants")
            }
            ValidationErrorKind::DuplicateField { field, outer } => {
                write!(f, "duplicate field `{field}`")?;
                if let Some(outer) = outer {
                    write!(f, " in `{outer}`")?;
                }
                Ok(())
            }
            ValidationErrorKind::LengthMismatch {
                expected,
                found,
                context,
            } => {
                if let Some(ctx) = context {
                    write!(f, "{ctx} ")?;
                }
                write!(f, "expected {expected} elements but found {found}")
            }
            ValidationErrorKind::IntegerOutOfBounds { value, target_type } => {
                write!(f, "integer {value} is out of bounds for {target_type}")
            }
        }
    }
}

/// Format a list of expected values for error messages.
fn format_expected_list(
    f: &mut fmt::Formatter<'_>,
    expected: &[&str],
    none_name: &str,
) -> fmt::Result {
    match expected {
        [] => write!(f, "there are no {none_name}"),
        [a1] => write!(f, "expected `{a1}` instead"),
        [a1, a2] => write!(f, "expected either `{a1}` or `{a2}` instead"),
        [a1, rest @ .., an] => {
            write!(f, "expected one of `{a1}`")?;
            for alt in rest {
                write!(f, ", `{alt}`")?;
            }
            write!(f, ", or `{an}` instead")
        }
    }
}

/// A validation error with optional span and path context.
///
/// This error type is used for all validation errors across the ron-extras
/// workspace. It includes:
/// - The specific error kind
/// - Optional source span for error location
/// - Path context showing where in the structure the error occurred
///
/// # Path Building
///
/// Path segments are appended using `push` (O(1)) and reversed on display
/// to show the path from outermost to innermost context.
///
/// # Example
///
/// ```
/// use ron2_error::{ValidationError, ValidationErrorKind, Span, Position};
///
/// let error = ValidationError::type_mismatch("i32", "String")
///     .in_element(0)
///     .in_field("items");
///
/// assert_eq!(
///     error.to_string(),
///     "in field 'items' -> element 0: expected i32 but found String"
/// );
/// ```
#[derive(Debug, Clone)]
pub struct ValidationError {
    /// The specific error that occurred.
    pub kind: ValidationErrorKind,
    /// Optional source span for the error location.
    pub span: Option<Span>,
    /// Path segments from innermost to outermost (reversed on display).
    pub path: Vec<PathSegment>,
}

impl ValidationError {
    /// Create a new validation error with no span or path context.
    #[must_use]
    pub fn new(kind: ValidationErrorKind) -> Self {
        Self {
            kind,
            span: None,
            path: Vec::new(),
        }
    }

    /// Create a validation error with a span.
    #[must_use]
    pub fn with_span(kind: ValidationErrorKind, span: Span) -> Self {
        Self {
            kind,
            span: Some(span),
            path: Vec::new(),
        }
    }

    /// Add a field context to this error's path.
    #[must_use]
    pub fn in_field(mut self, name: impl Into<String>) -> Self {
        self.path.push(PathSegment::Field(name.into()));
        self
    }

    /// Add an element context to this error's path.
    #[must_use]
    pub fn in_element(mut self, index: usize) -> Self {
        self.path.push(PathSegment::Element(index));
        self
    }

    /// Add a variant context to this error's path.
    #[must_use]
    pub fn in_variant(mut self, name: impl Into<String>) -> Self {
        self.path.push(PathSegment::Variant(name.into()));
        self
    }

    /// Add a type ref context to this error's path.
    #[must_use]
    pub fn in_type_ref(mut self, path: impl Into<String>) -> Self {
        self.path.push(PathSegment::TypeRef(path.into()));
        self
    }

    /// Add a map key context to this error's path.
    #[must_use]
    pub fn in_map_key(mut self) -> Self {
        self.path.push(PathSegment::MapKey);
        self
    }

    /// Add a map value context to this error's path.
    #[must_use]
    pub fn in_map_value(mut self, key: impl Into<String>) -> Self {
        self.path.push(PathSegment::MapValue(key.into()));
        self
    }

    // =========================================================================
    // Convenience constructors
    // =========================================================================

    /// Create a type mismatch error.
    #[must_use]
    pub fn type_mismatch(expected: impl Into<String>, found: impl Into<String>) -> Self {
        Self::new(ValidationErrorKind::TypeMismatch {
            expected: expected.into(),
            found: found.into(),
        })
    }

    /// Create a missing field error.
    #[must_use]
    pub fn missing_field(field: impl Into<Cow<'static, str>>) -> Self {
        Self::new(ValidationErrorKind::MissingField {
            field: field.into(),
            outer: None,
        })
    }

    /// Create a missing field error with outer context.
    #[must_use]
    pub fn missing_field_in(
        field: impl Into<Cow<'static, str>>,
        outer: impl Into<Cow<'static, str>>,
    ) -> Self {
        Self::new(ValidationErrorKind::MissingField {
            field: field.into(),
            outer: Some(outer.into()),
        })
    }

    /// Create an unknown field error.
    #[must_use]
    pub fn unknown_field(
        field: impl Into<Cow<'static, str>>,
        expected: &'static [&'static str],
    ) -> Self {
        Self::new(ValidationErrorKind::UnknownField {
            field: field.into(),
            expected,
            outer: None,
        })
    }

    /// Create an unknown field error with outer context.
    #[must_use]
    pub fn unknown_field_in(
        field: impl Into<Cow<'static, str>>,
        expected: &'static [&'static str],
        outer: impl Into<Cow<'static, str>>,
    ) -> Self {
        Self::new(ValidationErrorKind::UnknownField {
            field: field.into(),
            expected,
            outer: Some(outer.into()),
        })
    }

    /// Create an unknown variant error.
    #[must_use]
    pub fn unknown_variant(
        variant: impl Into<Cow<'static, str>>,
        expected: &'static [&'static str],
    ) -> Self {
        Self::new(ValidationErrorKind::UnknownVariant {
            variant: variant.into(),
            expected,
            outer: None,
        })
    }

    /// Create an unknown variant error with outer context.
    #[must_use]
    pub fn unknown_variant_in(
        variant: impl Into<Cow<'static, str>>,
        expected: &'static [&'static str],
        outer: impl Into<Cow<'static, str>>,
    ) -> Self {
        Self::new(ValidationErrorKind::UnknownVariant {
            variant: variant.into(),
            expected,
            outer: Some(outer.into()),
        })
    }

    /// Create a duplicate field error.
    #[must_use]
    pub fn duplicate_field(field: impl Into<Cow<'static, str>>) -> Self {
        Self::new(ValidationErrorKind::DuplicateField {
            field: field.into(),
            outer: None,
        })
    }

    /// Create a duplicate field error with outer context.
    #[must_use]
    pub fn duplicate_field_in(
        field: impl Into<Cow<'static, str>>,
        outer: impl Into<Cow<'static, str>>,
    ) -> Self {
        Self::new(ValidationErrorKind::DuplicateField {
            field: field.into(),
            outer: Some(outer.into()),
        })
    }

    /// Create a length mismatch error.
    #[must_use]
    pub fn length_mismatch(expected: usize, found: usize) -> Self {
        Self::new(ValidationErrorKind::LengthMismatch {
            expected,
            found,
            context: None,
        })
    }

    /// Create a length mismatch error with context.
    #[must_use]
    pub fn length_mismatch_with_context(
        expected: usize,
        found: usize,
        context: &'static str,
    ) -> Self {
        Self::new(ValidationErrorKind::LengthMismatch {
            expected,
            found,
            context: Some(context),
        })
    }

    /// Create an integer out of bounds error.
    #[must_use]
    pub fn integer_out_of_bounds(
        value: impl Into<Cow<'static, str>>,
        target_type: &'static str,
    ) -> Self {
        Self::new(ValidationErrorKind::IntegerOutOfBounds {
            value: value.into(),
            target_type,
        })
    }
}

impl fmt::Display for ValidationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Span prefix (if present and not synthetic)
        if let Some(ref span) = self.span {
            if !span.is_synthetic() {
                write!(f, "{span}: ")?;
            }
        }

        // Path context (reversed - outermost first)
        if !self.path.is_empty() {
            write!(f, "in ")?;
            for (i, seg) in self.path.iter().rev().enumerate() {
                if i > 0 {
                    write!(f, " -> ")?;
                }
                write!(f, "{seg}")?;
            }
            write!(f, ": ")?;
        }

        write!(f, "{}", self.kind)
    }
}

impl PartialEq for ValidationError {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind && self.span == other.span && self.path == other.path
    }
}

impl Eq for ValidationError {}

impl std::error::Error for ValidationError {}

#[cfg(test)]
mod tests {
    use alloc::string::ToString;

    use super::*;
    use crate::Position;

    #[test]
    fn test_type_mismatch() {
        let err = ValidationError::type_mismatch("i32", "String");
        assert_eq!(err.to_string(), "expected i32 but found String");
    }

    #[test]
    fn test_missing_field() {
        let err = ValidationError::missing_field("name");
        assert_eq!(err.to_string(), "missing required field `name`");

        let err = ValidationError::missing_field_in("name", "Config");
        assert_eq!(err.to_string(), "missing required field `name` in `Config`");
    }

    #[test]
    fn test_unknown_field() {
        let err = ValidationError::unknown_field("bad_field", &["good", "fields"]);
        assert_eq!(
            err.to_string(),
            "unknown field `bad_field`, expected either `good` or `fields` instead"
        );

        let err = ValidationError::unknown_field("bad_field", &[]);
        assert_eq!(
            err.to_string(),
            "unknown field `bad_field`, there are no fields"
        );
    }

    #[test]
    fn test_unknown_variant() {
        let err = ValidationError::unknown_variant_in("Bad", &["Good", "Better", "Best"], "MyEnum");
        assert_eq!(
            err.to_string(),
            "unknown variant `Bad` in enum `MyEnum`, expected one of `Good`, `Better`, or `Best` instead"
        );
    }

    #[test]
    fn test_duplicate_field() {
        let err = ValidationError::duplicate_field("name");
        assert_eq!(err.to_string(), "duplicate field `name`");

        let err = ValidationError::duplicate_field_in("name", "Config");
        assert_eq!(err.to_string(), "duplicate field `name` in `Config`");
    }

    #[test]
    fn test_length_mismatch() {
        let err = ValidationError::length_mismatch(3, 5);
        assert_eq!(err.to_string(), "expected 3 elements but found 5");

        let err = ValidationError::length_mismatch_with_context(2, 4, "tuple");
        assert_eq!(err.to_string(), "tuple expected 2 elements but found 4");
    }

    #[test]
    fn test_integer_out_of_bounds() {
        let err = ValidationError::integer_out_of_bounds("256", "u8");
        assert_eq!(err.to_string(), "integer 256 is out of bounds for u8");
    }

    #[test]
    fn test_with_path_context() {
        let err = ValidationError::type_mismatch("i32", "String")
            .in_element(0)
            .in_field("items");
        assert_eq!(
            err.to_string(),
            "in field 'items' -> element 0: expected i32 but found String"
        );
    }

    #[test]
    fn test_with_span() {
        let span = Span {
            start: Position { line: 3, col: 15 },
            end: Position { line: 3, col: 20 },
            start_offset: 50,
            end_offset: 55,
        };
        let err = ValidationError::with_span(
            ValidationErrorKind::TypeMismatch {
                expected: "i32".into(),
                found: "String".into(),
            },
            span,
        );
        assert_eq!(err.to_string(), "3:15-3:20: expected i32 but found String");
    }

    #[test]
    fn test_with_span_and_path() {
        let span = Span {
            start: Position { line: 3, col: 15 },
            end: Position { line: 3, col: 20 },
            start_offset: 50,
            end_offset: 55,
        };
        let err = ValidationError::with_span(
            ValidationErrorKind::TypeMismatch {
                expected: "i32".into(),
                found: "String".into(),
            },
            span,
        )
        .in_element(0)
        .in_field("items");

        assert_eq!(
            err.to_string(),
            "3:15-3:20: in field 'items' -> element 0: expected i32 but found String"
        );
    }

    #[test]
    fn test_synthetic_span_hidden() {
        let err = ValidationError::with_span(
            ValidationError::type_mismatch("i32", "String").kind,
            Span::synthetic(),
        );
        assert_eq!(err.to_string(), "expected i32 but found String");
    }
}
