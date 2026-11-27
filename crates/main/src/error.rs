//! Error types for RON parsing and deserialization.
//!
//! This module provides a unified error type for all ron2 operations:
//! - [`Error`] - The main error type (internally boxed for efficiency)
//! - [`ErrorKind`] - All possible error variants
//! - [`Position`], [`Span`] - Source position types
//! - [`PathSegment`] - Path context for nested errors

use alloc::{
    borrow::Cow,
    boxed::Box,
    string::{String, ToString},
    sync::Arc,
    vec::Vec,
};
use core::{fmt, str::Utf8Error};
use std::io;

use unicode_ident::is_xid_continue;

use crate::chars::{is_ident_first_char, is_ident_raw_char};

mod path;
mod span;

pub use path::PathSegment;
pub use span::{LineIndex, LineIndexCursor, Position, Span};

// Compatibility aliases for schema validation
#[doc(hidden)]
pub type ValidationError = Error;
#[doc(hidden)]
pub type ValidationErrorKind = ErrorKind;

// =============================================================================
// Result type alias
// =============================================================================

/// Result type for ron2 operations.
pub type Result<T, E = Error> = core::result::Result<T, E>;

// =============================================================================
// Error struct (internally boxed)
// =============================================================================

/// Unified error type for all ron2 operations.
///
/// This type is internally boxed to keep `Result<T, Error>` small (8 bytes),
/// which improves performance in the common success case.
///
/// # Example
///
/// ```
/// use ron2::{Error, ErrorKind, Span};
///
/// // Create an error with a span
/// let err = Error::expected("closing `]`", Span::synthetic());
///
/// // Add path context
/// let err = Error::type_mismatch("i32", "String")
///     .in_element(0)
///     .in_field("items");
///
/// println!("{}", err);
/// ```
#[derive(Debug, Clone)]
pub struct Error(Box<ErrorInner>);

#[derive(Debug, Clone)]
struct ErrorInner {
    kind: ErrorKind,
    span: Span,
    path: Vec<PathSegment>,
}

impl Error {
    // =========================================================================
    // Accessors
    // =========================================================================

    /// Returns the error kind.
    #[must_use]
    pub fn kind(&self) -> &ErrorKind {
        &self.0.kind
    }

    /// Returns the source span.
    #[must_use]
    pub fn span(&self) -> &Span {
        &self.0.span
    }

    /// Returns the path context (innermost to outermost).
    #[must_use]
    pub fn path(&self) -> &[PathSegment] {
        &self.0.path
    }

    // =========================================================================
    // Constructors
    // =========================================================================

    /// Create an error with a synthetic span (for errors without source location).
    ///
    /// Prefer `with_span` when a span is available for better error messages.
    #[must_use]
    pub fn new(kind: ErrorKind) -> Self {
        Self(Box::new(ErrorInner {
            kind,
            span: Span::synthetic(),
            path: Vec::new(),
        }))
    }

    /// Create an error with a specific span.
    #[must_use]
    pub fn with_span(kind: ErrorKind, span: Span) -> Self {
        Self(Box::new(ErrorInner {
            kind,
            span,
            path: Vec::new(),
        }))
    }

    /// Create an error at position (1,1) with zero-length span.
    #[must_use]
    pub fn at_start(kind: ErrorKind) -> Self {
        Self(Box::new(ErrorInner {
            kind,
            span: Span {
                start: Position { line: 1, col: 1 },
                end: Position { line: 1, col: 1 },
                start_offset: 0,
                end_offset: 0,
            },
            path: Vec::new(),
        }))
    }

    /// Attach `span` if this error does not carry a source location yet.
    ///
    /// Errors raised deeper in a conversion already point at the most precise
    /// location, so an existing span is kept.
    #[must_use]
    pub(crate) fn or_span(mut self, span: Span) -> Self {
        if self.0.span.is_synthetic() {
            self.0.span = span;
        }
        self
    }

    // =========================================================================
    // Path builders (fluent API)
    // =========================================================================

    /// Add a field context to this error's path.
    #[must_use]
    pub fn in_field(mut self, name: impl Into<String>) -> Self {
        self.0.path.push(PathSegment::Field(name.into()));
        self
    }

    /// Add an element context to this error's path.
    #[must_use]
    pub fn in_element(mut self, index: usize) -> Self {
        self.0.path.push(PathSegment::Element(index));
        self
    }

    /// Add a variant context to this error's path.
    #[must_use]
    pub fn in_variant(mut self, name: impl Into<String>) -> Self {
        self.0.path.push(PathSegment::Variant(name.into()));
        self
    }

    /// Add a type ref context to this error's path.
    #[must_use]
    pub fn in_type_ref(mut self, path: impl Into<String>) -> Self {
        self.0.path.push(PathSegment::TypeRef(path.into()));
        self
    }

    /// Add a map key context to this error's path.
    #[must_use]
    pub fn in_map_key(mut self) -> Self {
        self.0.path.push(PathSegment::MapKey);
        self
    }

    /// Add a map value context to this error's path.
    #[must_use]
    pub fn in_map_value(mut self, key: impl Into<String>) -> Self {
        self.0.path.push(PathSegment::MapValue(key.into()));
        self
    }

    // =========================================================================
    // Convenience constructors
    // =========================================================================

    /// Create an "expected X" error.
    #[must_use]
    pub fn expected(what: impl Into<Cow<'static, str>>, span: Span) -> Self {
        Self::with_span(
            ErrorKind::Expected {
                expected: what.into(),
                context: None,
            },
            span,
        )
    }

    /// Create a type mismatch error.
    #[must_use]
    pub fn type_mismatch(expected: impl Into<String>, found: impl Into<String>) -> Self {
        Self::new(ErrorKind::TypeMismatch {
            expected: expected.into(),
            found: found.into(),
        })
    }

    /// Create a missing field error.
    #[must_use]
    pub fn missing_field(field: impl Into<Cow<'static, str>>) -> Self {
        Self::new(ErrorKind::MissingField {
            field: field.into(),
            outer: None,
        })
    }

    /// Create an unknown field error.
    #[must_use]
    pub fn unknown_field(
        field: impl Into<Cow<'static, str>>,
        expected: &'static [&'static str],
    ) -> Self {
        Self::new(ErrorKind::UnknownField {
            field: field.into(),
            expected: Cow::Borrowed(expected),
            outer: None,
        })
    }

    /// Create an unknown variant error.
    #[must_use]
    pub fn unknown_variant(
        variant: impl Into<Cow<'static, str>>,
        expected: &'static [&'static str],
    ) -> Self {
        Self::new(ErrorKind::UnknownVariant {
            variant: variant.into(),
            expected,
            outer: None,
        })
    }

    /// Create a length mismatch error.
    #[must_use]
    pub fn length_mismatch(expected: impl Into<String>, found: usize) -> Self {
        Self::new(ErrorKind::LengthMismatch {
            expected: expected.into(),
            found,
            context: None,
        })
    }

    /// Create an integer out of bounds error.
    #[must_use]
    pub fn integer_out_of_bounds(
        value: impl Into<Cow<'static, str>>,
        target_type: &'static str,
    ) -> Self {
        Self::new(ErrorKind::IntegerOutOfBounds {
            value: value.into(),
            target_type,
        })
    }

    // =========================================================================
    // Suggestions
    // =========================================================================

    /// Get a suggestion for fixing this error, if applicable.
    #[must_use]
    pub fn suggestion(&self) -> Option<String> {
        self.0.kind.suggestion()
    }
}

// =============================================================================
// ErrorKind enum
// =============================================================================

/// The underlying cause of an [`ErrorKind::Io`] or [`ErrorKind::Utf8`] error.
///
/// Shared via [`Arc`] so errors stay cheap to clone. Two sources always
/// compare equal: equality of errors is defined by their message, span and path.
#[derive(Debug)]
pub struct ErrorSource<T>(pub Arc<T>);

impl<T> Clone for ErrorSource<T> {
    fn clone(&self) -> Self {
        Self(Arc::clone(&self.0))
    }
}

impl<T> PartialEq for ErrorSource<T> {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl<T> Eq for ErrorSource<T> {}

impl<T> core::ops::Deref for ErrorSource<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.0
    }
}

/// The specific kind of error that occurred.
#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub enum ErrorKind {
    // =========================================================================
    // General / Infrastructure
    // =========================================================================
    /// Generic message (escape hatch for custom errors).
    Message(String),

    /// Formatting error (from `fmt::Error`).
    Fmt,

    /// IO error with preserved source for error chaining.
    Io {
        /// Human-readable error message.
        message: String,
        /// Original IO error.
        source: Option<ErrorSource<io::Error>>,
    },

    /// UTF-8 decoding error with preserved source.
    Utf8 {
        /// Human-readable error message.
        message: String,
        /// Original UTF-8 error.
        source: Option<ErrorSource<Utf8Error>>,
    },

    // =========================================================================
    // Lexical / Token Errors
    // =========================================================================
    /// Unexpected end of input.
    Eof,

    /// Unexpected character encountered.
    UnexpectedChar(char),

    /// Unclosed block comment (`/* ... */`).
    UnclosedBlockComment,

    /// Invalid escape sequence in string.
    InvalidEscape(Cow<'static, str>),

    /// Invalid digit for the number's base.
    InvalidIntegerDigit {
        /// The invalid digit.
        digit: char,
        /// The number base (2, 8, 10, or 16).
        base: u8,
    },

    /// Unexpected underscore in float.
    FloatUnderscore,

    // =========================================================================
    // Structural / Syntax Errors
    // =========================================================================
    /// Expected a specific token or construct.
    Expected {
        /// What was expected (e.g., "closing `]`", "comma").
        expected: Cow<'static, str>,
        /// Optional context (e.g., "array", "struct field").
        context: Option<&'static str>,
    },

    /// A collection is missing its closing delimiter.
    Unclosed {
        /// The opening text: `(`, `[`, `{`, or a struct name with its `(`, such as
        /// `Point(` or `Some(`.
        open: String,
        /// The missing closing delimiter.
        close: char,
        /// Where the opening delimiter is.
        opened_at: Position,
    },

    /// Non-whitespace trailing characters after main value.
    TrailingCharacters,

    /// Expected end of string literal.
    ExpectedStringEnd,

    /// Invalid identifier.
    InvalidIdentifier(String),

    // =========================================================================
    // Type Mismatch Errors
    // =========================================================================
    /// Expected one type, found another.
    TypeMismatch {
        /// The expected type.
        expected: String,
        /// The found type.
        found: String,
    },

    /// Wrong number of elements.
    LengthMismatch {
        /// Expected count description (e.g., "3 elements").
        expected: String,
        /// Actual count found.
        found: usize,
        /// Optional context (e.g., "tuple", "array").
        context: Option<&'static str>,
    },

    // =========================================================================
    // Struct/Enum Field Errors
    // =========================================================================
    /// Missing required struct field.
    MissingField {
        /// The name of the missing field.
        field: Cow<'static, str>,
        /// The containing struct name, if known.
        outer: Option<Cow<'static, str>>,
    },

    /// Unknown struct field.
    UnknownField {
        /// The name of the unknown field.
        field: Cow<'static, str>,
        /// The list of expected field names.
        expected: Cow<'static, [&'static str]>,
        /// The containing struct name, if known.
        outer: Option<Cow<'static, str>>,
    },

    /// Duplicate struct field.
    DuplicateField {
        /// The name of the duplicated field.
        field: Cow<'static, str>,
        /// The containing struct name, if known.
        outer: Option<Cow<'static, str>>,
    },

    /// Duplicate map key.
    DuplicateKey {
        /// The duplicated key, rendered as RON.
        key: String,
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

    // =========================================================================
    // Numeric Errors
    // =========================================================================
    /// Integer out of bounds for target type.
    IntegerOutOfBounds {
        /// The string representation of the out-of-bounds value.
        value: Cow<'static, str>,
        /// The target type that couldn't hold the value.
        target_type: &'static str,
    },

    /// Finite float literal too large for the target type.
    FloatOutOfBounds {
        /// The source text of the literal.
        value: Cow<'static, str>,
        /// The target type that couldn't hold the value.
        target_type: &'static str,
    },

    // =========================================================================
    // Limits
    // =========================================================================
    /// Values nested deeper than
    /// [`MAX_CONVERSION_DEPTH`](crate::ast::MAX_CONVERSION_DEPTH) levels, the
    /// limit of parsing and of the AST↔`Value` conversion.
    ExceededRecursionLimit,

    // =========================================================================
    // Schema Errors
    // =========================================================================
    /// A schema `TypeRef` could not be resolved, so the value was not checked.
    UnresolvedTypeRef(String),
}

impl ErrorKind {
    /// Get a suggestion for fixing this error, if applicable.
    #[must_use]
    pub fn suggestion(&self) -> Option<String> {
        match self {
            ErrorKind::UnknownField {
                field, expected, ..
            } => find_similar(field, expected),
            ErrorKind::UnknownVariant {
                variant, expected, ..
            } => find_similar(variant, expected),
            _ => None,
        }
    }
}

/// Find a similar string from a list of expected values.
fn find_similar(needle: &str, haystack: &[&str]) -> Option<String> {
    let needle_lower = needle.to_lowercase();

    // Find best match using edit distance
    haystack
        .iter()
        .filter_map(|s| {
            let s_lower = s.to_lowercase();
            let dist = edit_distance(&needle_lower, &s_lower);
            // Only consider matches with edit distance <= 2 (or <= half the length for short strings)
            let max_dist = (needle.len().max(s.len()) / 2).max(2);
            if dist <= max_dist {
                Some((*s, dist))
            } else {
                None
            }
        })
        .min_by_key(|(_, dist)| *dist)
        .map(|(s, _)| s.to_string())
}

/// Calculate edit distance between two strings.
fn edit_distance(a: &str, b: &str) -> usize {
    let a_chars: Vec<char> = a.chars().collect();
    let b_chars: Vec<char> = b.chars().collect();
    let m = a_chars.len();
    let n = b_chars.len();

    // Quick checks
    if m == 0 {
        return n;
    }
    if n == 0 {
        return m;
    }

    // Use two-row optimization for space efficiency
    let mut prev = (0..=n).collect::<Vec<_>>();
    let mut curr = vec![0; n + 1];

    for i in 1..=m {
        curr[0] = i;
        for j in 1..=n {
            let cost = usize::from(a_chars[i - 1] != b_chars[j - 1]);
            curr[j] = (prev[j] + 1).min(curr[j - 1] + 1).min(prev[j - 1] + cost);
        }
        core::mem::swap(&mut prev, &mut curr);
    }

    prev[n]
}

// =============================================================================
// Display implementations
// =============================================================================

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Span prefix (if not synthetic)
        if !self.0.span.is_synthetic() {
            write!(f, "{}: ", self.0.span)?;
        }

        write!(f, "{}", self.0.kind)?;

        // Path context last (reversed - outermost first)
        if !self.0.path.is_empty() {
            write!(f, " (in ")?;
            for (i, seg) in self.0.path.iter().rev().enumerate() {
                if i > 0 {
                    write!(f, " -> ")?;
                }
                write!(f, "{seg}")?;
            }
            write!(f, ")")?;
        }
        Ok(())
    }
}

impl fmt::Display for ErrorKind {
    #[allow(clippy::too_many_lines)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // General
            ErrorKind::Message(s) => f.write_str(s),
            ErrorKind::Fmt => f.write_str("Formatting RON failed"),
            ErrorKind::Io { message, .. } | ErrorKind::Utf8 { message, .. } => f.write_str(message),

            // Lexical
            ErrorKind::Eof => f.write_str("Unexpected end of RON"),
            ErrorKind::UnexpectedChar(c) => write!(f, "Unexpected char {c:?}"),
            ErrorKind::UnclosedBlockComment => f.write_str("Unclosed block comment"),
            ErrorKind::InvalidEscape(s) => f.write_str(s),
            ErrorKind::InvalidIntegerDigit { digit, base } => {
                write!(f, "Invalid digit {digit:?} for base {base} integers")
            }
            ErrorKind::FloatUnderscore => f.write_str("Unexpected underscore in float"),

            // Structural
            ErrorKind::Expected {
                expected,
                context: Some(ctx),
            } => write!(f, "Expected {expected} in {ctx}"),
            ErrorKind::Expected {
                expected,
                context: None,
            } => write!(f, "Expected {expected}"),
            ErrorKind::Unclosed {
                open,
                close,
                opened_at,
            } => write!(
                f,
                "Expected closing `{close}` for `{open}` opened at {opened_at}"
            ),
            ErrorKind::TrailingCharacters => f.write_str("Non-whitespace trailing characters"),
            ErrorKind::ExpectedStringEnd => f.write_str("Expected end of string"),
            ErrorKind::InvalidIdentifier(s) => write!(f, "Invalid identifier {s:?}"),

            // Type mismatch
            ErrorKind::TypeMismatch { expected, found } => {
                write!(f, "expected {expected} but found {found}")
            }
            ErrorKind::LengthMismatch {
                expected,
                found,
                context,
            } => {
                if let Some(ctx) = context {
                    write!(f, "{ctx} ")?;
                }
                write!(f, "expected {expected} but found ")?;
                match found {
                    0 => f.write_str("zero elements"),
                    1 => f.write_str("one element"),
                    n => write!(f, "{n} elements"),
                }
            }

            // Fields/variants
            ErrorKind::MissingField { field, outer } => {
                write!(f, "missing required field {}", Identifier(field))?;
                if let Some(outer) = outer {
                    write!(f, " in {}", Identifier(outer))?;
                }
                Ok(())
            }
            ErrorKind::UnknownField {
                field,
                expected,
                outer,
            } => {
                write!(f, "Unknown field {}", Identifier(field))?;
                if let Some(outer) = outer {
                    write!(f, " in {}", Identifier(outer))?;
                }
                write!(f, "{}", OneOf(expected))
            }
            ErrorKind::DuplicateField { field, outer } => {
                write!(f, "Duplicate field {}", Identifier(field))?;
                if let Some(outer) = outer {
                    write!(f, " in {}", Identifier(outer))?;
                }
                Ok(())
            }
            ErrorKind::DuplicateKey { key } => write!(f, "Duplicate map key {key}"),
            ErrorKind::UnknownVariant {
                variant,
                expected,
                outer,
            } => {
                f.write_str("Unknown ")?;
                if outer.is_none() {
                    f.write_str("enum ")?;
                }
                write!(f, "variant {}", Identifier(variant))?;
                if let Some(outer) = outer {
                    write!(f, " in enum {}", Identifier(outer))?;
                }
                write!(f, "{}", OneOf(expected))
            }
            // Numeric
            ErrorKind::IntegerOutOfBounds { value, target_type } => {
                write!(f, "Integer {value} is out of bounds for {target_type}")
            }
            ErrorKind::FloatOutOfBounds { value, target_type } => {
                write!(f, "Float {value} is out of range for {target_type}")
            }

            // Limits
            ErrorKind::ExceededRecursionLimit => write!(
                f,
                "Nesting deeper than {} levels",
                crate::ast::MAX_CONVERSION_DEPTH
            ),

            // Schema
            ErrorKind::UnresolvedTypeRef(path) => {
                write!(f, "No schema found for type `{path}`; value not checked")
            }
        }
    }
}

// =============================================================================
// PartialEq / Eq
// =============================================================================

impl PartialEq for Error {
    fn eq(&self, other: &Self) -> bool {
        self.0.kind == other.0.kind && self.0.span == other.0.span && self.0.path == other.0.path
    }
}

impl Eq for Error {}

// =============================================================================
// std::error::Error implementation
// =============================================================================

impl core::error::Error for Error {
    fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
        match &self.0.kind {
            ErrorKind::Io {
                source: Some(e), ..
            } => Some(&**e),
            ErrorKind::Utf8 {
                source: Some(e), ..
            } => Some(&**e),
            _ => None,
        }
    }
}

// =============================================================================
// From implementations
// =============================================================================

impl From<Utf8Error> for Error {
    fn from(e: Utf8Error) -> Self {
        Error::new(ErrorKind::Utf8 {
            message: e.to_string(),
            source: Some(ErrorSource(Arc::new(e))),
        })
    }
}

impl From<fmt::Error> for Error {
    fn from(_: fmt::Error) -> Self {
        Error::new(ErrorKind::Fmt)
    }
}

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Self {
        Error::new(ErrorKind::Io {
            message: e.to_string(),
            source: Some(ErrorSource(Arc::new(e))),
        })
    }
}

// =============================================================================
// Helper display types
// =============================================================================

/// `, expected ...` suffix listing the alternatives. Empty when the
/// alternatives are unknown (e.g. errors built from a runtime schema).
struct OneOf<'a>(&'a [&'static str]);

impl fmt::Display for OneOf<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            [] => Ok(()),
            [a1] => write!(f, ", expected {} instead", Identifier(a1)),
            [a1, a2] => write!(
                f,
                ", expected either {} or {} instead",
                Identifier(a1),
                Identifier(a2)
            ),
            [a1, alts @ .., an] => {
                write!(f, ", expected one of {}", Identifier(a1))?;
                for alt in alts {
                    write!(f, ", {}", Identifier(alt))?;
                }
                write!(f, ", or {} instead", Identifier(an))
            }
        }
    }
}

struct Identifier<'a>(&'a str);

impl fmt::Display for Identifier<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (raw_prefix, ident) = match self.0.strip_prefix("r#") {
            Some(rest) => (true, rest),
            None => (false, self.0),
        };

        if ident.is_empty() || !ident.chars().all(is_ident_raw_char) {
            return write!(f, "{:?}_[invalid identifier]", self.0);
        }

        if raw_prefix {
            return write!(f, "`r#{ident}`");
        }

        let mut chars = ident.chars();

        if !chars.next().is_some_and(is_ident_first_char) || !chars.all(is_xid_continue) {
            write!(f, "`r#{ident}`")
        } else {
            write!(f, "`{ident}`")
        }
    }
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn error_is_small() {
        // Error should be pointer-sized (8 bytes on 64-bit)
        assert_eq!(
            core::mem::size_of::<Error>(),
            core::mem::size_of::<*const ()>()
        );
    }

    #[test]
    fn error_messages() {
        check_message(&Error::from(fmt::Error), "Formatting RON failed");
        check_message(&Error::new(ErrorKind::Message("custom".into())), "custom");
        check_message(&Error::new(ErrorKind::Eof), "Unexpected end of RON");
        check_message(
            &Error::expected("opening `[`", Span::synthetic()),
            "Expected opening `[`",
        );
        check_message(
            &Error::new(ErrorKind::Expected {
                expected: "comma".into(),
                context: Some("array"),
            }),
            "Expected comma in array",
        );
        check_message(
            &Error::type_mismatch("i32", "String"),
            "expected i32 but found String",
        );
        check_message(
            &Error::missing_field("name"),
            "missing required field `name`",
        );
        check_message(
            &Error::new(ErrorKind::MissingField {
                field: "name".into(),
                outer: Some("Config".into()),
            }),
            "missing required field `name` in `Config`",
        );
        check_message(
            &Error::missing_field("r#type"),
            "missing required field `r#type`",
        );
        check_message(
            &Error::integer_out_of_bounds("256", "u8"),
            "Integer 256 is out of bounds for u8",
        );
        check_message(
            &Error::new(ErrorKind::DuplicateKey {
                key: "\"a\"".into(),
            }),
            "Duplicate map key \"a\"",
        );
        check_message(
            &Error::new(ErrorKind::FloatOutOfBounds {
                value: "1e40".into(),
                target_type: "f32",
            }),
            "Float 1e40 is out of range for f32",
        );
    }

    fn check_message(err: &Error, expected: &str) {
        assert_eq!(alloc::format!("{err}"), expected);
    }

    #[test]
    fn path_context() {
        let err = Error::type_mismatch("i32", "String")
            .in_element(0)
            .in_field("items");
        assert_eq!(
            err.to_string(),
            "expected i32 but found String (in field 'items' -> element 0)"
        );
    }

    #[test]
    fn span_display() {
        let span = Span {
            start: Position { line: 3, col: 15 },
            end: Position { line: 3, col: 20 },
            start_offset: 50,
            end_offset: 55,
        };
        let err = Error::with_span(ErrorKind::Eof, span);
        assert_eq!(err.to_string(), "3:15-3:20: Unexpected end of RON");
    }

    #[test]
    fn suggestions() {
        let err = Error::unknown_field("nme", &["name", "age", "email"]);
        assert_eq!(err.suggestion(), Some("name".to_string()));

        let err = Error::unknown_variant("Tru", &["True", "False"]);
        assert_eq!(err.suggestion(), Some("True".to_string()));
    }

    #[test]
    fn error_source_chain() {
        let io_err = io::Error::new(io::ErrorKind::NotFound, "file not found");
        let err = Error::from(io_err);

        assert_eq!(err.to_string(), "file not found");
        // The original io::Error is the source
        let source = core::error::Error::source(&err).unwrap();
        let io_source = source.downcast_ref::<io::Error>().unwrap();
        assert_eq!(io_source.kind(), io::ErrorKind::NotFound);
        assert_eq!(io_source.to_string(), "file not found");
    }

    #[test]
    fn error_equality() {
        let err1 = Error::type_mismatch("i32", "String");
        let err2 = Error::type_mismatch("i32", "String");
        assert_eq!(err1, err2);

        let err3 = Error::type_mismatch("i32", "bool");
        assert_ne!(err1, err3);

        assert_ne!(err1, err1.clone().in_field("x"));
    }

    #[test]
    fn io_error_equality_ignores_source() {
        let a = Error::from(io::Error::new(io::ErrorKind::NotFound, "gone"));
        let b = Error::from(io::Error::other("gone"));
        assert_eq!(a, b);

        let c = Error::from(io::Error::other("different"));
        assert_ne!(a, c);
    }

    #[test]
    fn or_span_keeps_existing_span() {
        let span = Span {
            start: Position { line: 2, col: 3 },
            end: Position { line: 2, col: 4 },
            start_offset: 5,
            end_offset: 6,
        };
        let other = Span {
            start: Position { line: 9, col: 9 },
            end: Position { line: 9, col: 10 },
            start_offset: 90,
            end_offset: 91,
        };

        let err = Error::type_mismatch("i32", "String").in_field("x");
        let err = err.or_span(span);
        assert_eq!(*err.span(), span);
        assert_eq!(err.path(), &[PathSegment::Field("x".into())]);

        assert_eq!(*err.or_span(other).span(), span);
    }
}
