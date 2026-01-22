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

    /// Create an error spanning from (1,1) to the end of source.
    #[must_use]
    pub fn wrap(kind: ErrorKind, source: &str) -> Self {
        Self(Box::new(ErrorInner {
            kind,
            span: Span {
                start: Position { line: 1, col: 1 },
                end: Position::from_src_end(source),
                start_offset: 0,
                end_offset: source.len(),
            },
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

    /// Create an "unexpected end of input" error.
    #[must_use]
    pub fn eof() -> Self {
        Self::new(ErrorKind::Eof)
    }

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

    /// Create an "expected X in Y" error.
    #[must_use]
    pub fn expected_in(
        what: impl Into<Cow<'static, str>>,
        context: &'static str,
        span: Span,
    ) -> Self {
        Self::with_span(
            ErrorKind::Expected {
                expected: what.into(),
                context: Some(context),
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

    /// Create a missing field error with outer context.
    #[must_use]
    pub fn missing_field_in(
        field: impl Into<Cow<'static, str>>,
        outer: impl Into<Cow<'static, str>>,
    ) -> Self {
        Self::new(ErrorKind::MissingField {
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
        Self::new(ErrorKind::UnknownField {
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
        Self::new(ErrorKind::UnknownField {
            field: field.into(),
            expected,
            outer: Some(outer.into()),
        })
    }

    /// Create a duplicate field error.
    #[must_use]
    pub fn duplicate_field(field: impl Into<Cow<'static, str>>) -> Self {
        Self::new(ErrorKind::DuplicateField {
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
        Self::new(ErrorKind::DuplicateField {
            field: field.into(),
            outer: Some(outer.into()),
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

    /// Create an unknown variant error with outer context.
    #[must_use]
    pub fn unknown_variant_in(
        variant: impl Into<Cow<'static, str>>,
        expected: &'static [&'static str],
        outer: impl Into<Cow<'static, str>>,
    ) -> Self {
        Self::new(ErrorKind::UnknownVariant {
            variant: variant.into(),
            expected,
            outer: Some(outer.into()),
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

    /// Create a length mismatch error with context.
    #[must_use]
    pub fn length_mismatch_in(
        expected: impl Into<String>,
        found: usize,
        context: &'static str,
    ) -> Self {
        Self::new(ErrorKind::LengthMismatch {
            expected: expected.into(),
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

/// The specific kind of error that occurred.
#[derive(Debug, Clone)]
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
        /// Original IO error (wrapped in Arc for Clone).
        #[allow(clippy::redundant_allocation)]
        source: Option<Arc<io::Error>>,
    },

    /// UTF-8 decoding error with preserved source.
    Utf8 {
        /// Human-readable error message.
        message: String,
        /// Original UTF-8 error (wrapped in Arc for Clone).
        source: Option<Arc<Utf8Error>>,
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

    /// Unclosed line comment (for `RawValue`).
    UnclosedLineComment,

    /// Invalid escape sequence in string.
    InvalidEscape(Cow<'static, str>),

    /// Invalid digit for the number's base.
    InvalidIntegerDigit {
        /// The invalid digit.
        digit: char,
        /// The number base (2, 8, 10, or 16).
        base: u8,
    },

    /// Unexpected leading underscore in number.
    UnderscoreAtBeginning,

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

    /// Non-whitespace trailing characters after main value.
    TrailingCharacters,

    /// Expected end of string literal.
    ExpectedStringEnd,

    /// Invalid identifier.
    InvalidIdentifier(String),

    /// Suggest using raw identifier syntax.
    SuggestRawIdentifier(String),

    /// Expected a `ron::value::RawValue`.
    ExpectedRawValue,

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
        expected: &'static [&'static str],
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

    /// Unknown enum variant.
    UnknownVariant {
        /// The name of the unknown variant.
        variant: Cow<'static, str>,
        /// The list of expected variant names.
        expected: &'static [&'static str],
        /// The enum name, if known.
        outer: Option<Cow<'static, str>>,
    },

    /// Expected a different struct name.
    ExpectedStructName {
        /// The expected struct name.
        expected: Cow<'static, str>,
        /// The found struct name (if any).
        found: Option<String>,
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

    // =========================================================================
    // Extension / Config Errors
    // =========================================================================
    /// Unknown RON extension.
    NoSuchExtension(String),

    /// Exceeded recursion limit.
    ExceededRecursionLimit,

    /// Too many struct fields for deserialization (max 64).
    TooManyFields {
        /// Number of fields found.
        count: usize,
        /// Maximum supported.
        limit: usize,
    },
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
            ErrorKind::SuggestRawIdentifier(ident) => Some(alloc::format!("r#{ident}")),
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

        // Path context (reversed - outermost first)
        if !self.0.path.is_empty() {
            write!(f, "in ")?;
            for (i, seg) in self.0.path.iter().rev().enumerate() {
                if i > 0 {
                    write!(f, " -> ")?;
                }
                write!(f, "{seg}")?;
            }
            write!(f, ": ")?;
        }

        // Error kind message
        write!(f, "{}", self.0.kind)
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
            ErrorKind::UnclosedLineComment => f.write_str(
                "`ron::value::RawValue` cannot end in unclosed line comment, \
                try using a block comment or adding a newline",
            ),
            ErrorKind::InvalidEscape(s) => f.write_str(s),
            ErrorKind::InvalidIntegerDigit { digit, base } => {
                write!(f, "Invalid digit {digit:?} for base {base} integers")
            }
            ErrorKind::UnderscoreAtBeginning => {
                f.write_str("Unexpected leading underscore in a number")
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
            ErrorKind::TrailingCharacters => f.write_str("Non-whitespace trailing characters"),
            ErrorKind::ExpectedStringEnd => f.write_str("Expected end of string"),
            ErrorKind::InvalidIdentifier(s) => write!(f, "Invalid identifier {s:?}"),
            ErrorKind::SuggestRawIdentifier(s) => write!(
                f,
                "Found invalid std identifier {s:?}, try the raw identifier `r#{s}` instead"
            ),
            ErrorKind::ExpectedRawValue => f.write_str("Expected a `ron::value::RawValue`"),

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
                write!(
                    f,
                    ", {}",
                    OneOf {
                        alts: expected,
                        none: "fields"
                    }
                )
            }
            ErrorKind::DuplicateField { field, outer } => {
                write!(f, "Duplicate field {}", Identifier(field))?;
                if let Some(outer) = outer {
                    write!(f, " in {}", Identifier(outer))?;
                }
                Ok(())
            }
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
                write!(
                    f,
                    ", {}",
                    OneOf {
                        alts: expected,
                        none: "variants"
                    }
                )
            }
            ErrorKind::ExpectedStructName { expected, found } => {
                if let Some(found) = found {
                    write!(
                        f,
                        "Expected struct {} but found {}",
                        Identifier(expected),
                        Identifier(found)
                    )
                } else {
                    write!(
                        f,
                        "Expected the explicit struct name {}, but none was found",
                        Identifier(expected)
                    )
                }
            }

            // Numeric
            ErrorKind::IntegerOutOfBounds { value, target_type } => {
                write!(f, "Integer {value} is out of bounds for {target_type}")
            }

            // Config
            ErrorKind::NoSuchExtension(name) => {
                write!(f, "No RON extension named {}", Identifier(name))
            }
            ErrorKind::ExceededRecursionLimit => f.write_str("Exceeded recursion limit"),
            ErrorKind::TooManyFields { count, limit } => {
                write!(f, "Struct has {count} fields but maximum is {limit}")
            }
        }
    }
}

// =============================================================================
// PartialEq / Eq (manual implementation)
// =============================================================================

impl PartialEq for Error {
    fn eq(&self, other: &Self) -> bool {
        self.0.kind == other.0.kind && self.0.span == other.0.span && self.0.path == other.0.path
    }
}

impl Eq for Error {}

impl PartialEq for ErrorKind {
    #[allow(clippy::too_many_lines)]
    fn eq(&self, other: &Self) -> bool {
        // Compare variants, skipping source fields (not meaningful for equality)
        match (self, other) {
            (ErrorKind::Message(a), ErrorKind::Message(b))
            | (ErrorKind::InvalidIdentifier(a), ErrorKind::InvalidIdentifier(b))
            | (ErrorKind::SuggestRawIdentifier(a), ErrorKind::SuggestRawIdentifier(b))
            | (ErrorKind::NoSuchExtension(a), ErrorKind::NoSuchExtension(b))
            | (ErrorKind::Io { message: a, .. }, ErrorKind::Io { message: b, .. })
            | (ErrorKind::Utf8 { message: a, .. }, ErrorKind::Utf8 { message: b, .. }) => a == b,
            (ErrorKind::Fmt, ErrorKind::Fmt)
            | (ErrorKind::Eof, ErrorKind::Eof)
            | (ErrorKind::UnclosedBlockComment, ErrorKind::UnclosedBlockComment)
            | (ErrorKind::UnclosedLineComment, ErrorKind::UnclosedLineComment)
            | (ErrorKind::UnderscoreAtBeginning, ErrorKind::UnderscoreAtBeginning)
            | (ErrorKind::FloatUnderscore, ErrorKind::FloatUnderscore)
            | (ErrorKind::TrailingCharacters, ErrorKind::TrailingCharacters)
            | (ErrorKind::ExpectedStringEnd, ErrorKind::ExpectedStringEnd)
            | (ErrorKind::ExpectedRawValue, ErrorKind::ExpectedRawValue)
            | (ErrorKind::ExceededRecursionLimit, ErrorKind::ExceededRecursionLimit) => true,
            (ErrorKind::UnexpectedChar(a), ErrorKind::UnexpectedChar(b)) => a == b,
            (ErrorKind::InvalidEscape(a), ErrorKind::InvalidEscape(b)) => a == b,
            (
                ErrorKind::InvalidIntegerDigit {
                    digit: d1,
                    base: b1,
                },
                ErrorKind::InvalidIntegerDigit {
                    digit: d2,
                    base: b2,
                },
            ) => d1 == d2 && b1 == b2,
            (
                ErrorKind::Expected {
                    expected: e1,
                    context: c1,
                },
                ErrorKind::Expected {
                    expected: e2,
                    context: c2,
                },
            ) => e1 == e2 && c1 == c2,
            (
                ErrorKind::TypeMismatch {
                    expected: e1,
                    found: f1,
                },
                ErrorKind::TypeMismatch {
                    expected: e2,
                    found: f2,
                },
            ) => e1 == e2 && f1 == f2,
            (
                ErrorKind::LengthMismatch {
                    expected: e1,
                    found: f1,
                    context: c1,
                },
                ErrorKind::LengthMismatch {
                    expected: e2,
                    found: f2,
                    context: c2,
                },
            ) => e1 == e2 && f1 == f2 && c1 == c2,
            (
                ErrorKind::MissingField {
                    field: f1,
                    outer: o1,
                },
                ErrorKind::MissingField {
                    field: f2,
                    outer: o2,
                },
            )
            | (
                ErrorKind::DuplicateField {
                    field: f1,
                    outer: o1,
                },
                ErrorKind::DuplicateField {
                    field: f2,
                    outer: o2,
                },
            ) => f1 == f2 && o1 == o2,
            (
                ErrorKind::UnknownField {
                    field: f1,
                    expected: e1,
                    outer: o1,
                },
                ErrorKind::UnknownField {
                    field: f2,
                    expected: e2,
                    outer: o2,
                },
            ) => f1 == f2 && e1 == e2 && o1 == o2,
            (
                ErrorKind::UnknownVariant {
                    variant: v1,
                    expected: e1,
                    outer: o1,
                },
                ErrorKind::UnknownVariant {
                    variant: v2,
                    expected: e2,
                    outer: o2,
                },
            ) => v1 == v2 && e1 == e2 && o1 == o2,
            (
                ErrorKind::ExpectedStructName {
                    expected: e1,
                    found: f1,
                },
                ErrorKind::ExpectedStructName {
                    expected: e2,
                    found: f2,
                },
            ) => e1 == e2 && f1 == f2,
            (
                ErrorKind::IntegerOutOfBounds {
                    value: v1,
                    target_type: t1,
                },
                ErrorKind::IntegerOutOfBounds {
                    value: v2,
                    target_type: t2,
                },
            ) => v1 == v2 && t1 == t2,
            (
                ErrorKind::TooManyFields {
                    count: c1,
                    limit: l1,
                },
                ErrorKind::TooManyFields {
                    count: c2,
                    limit: l2,
                },
            ) => c1 == c2 && l1 == l2,
            _ => false,
        }
    }
}

impl Eq for ErrorKind {}

// =============================================================================
// std::error::Error implementation
// =============================================================================

impl core::error::Error for Error {
    fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
        match &self.0.kind {
            ErrorKind::Io {
                source: Some(e), ..
            } => Some(e.as_ref()),
            ErrorKind::Utf8 {
                source: Some(e), ..
            } => Some(e.as_ref()),
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
            source: Some(Arc::new(e)),
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
            source: Some(Arc::new(e)),
        })
    }
}

// =============================================================================
// Helper display types
// =============================================================================

struct OneOf {
    alts: &'static [&'static str],
    none: &'static str,
}

impl fmt::Display for OneOf {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.alts {
            [] => write!(f, "there are no {}", self.none),
            [a1] => write!(f, "expected {} instead", Identifier(a1)),
            [a1, a2] => write!(
                f,
                "expected either {} or {} instead",
                Identifier(a1),
                Identifier(a2)
            ),
            [a1, alts @ .., an] => {
                write!(f, "expected one of {}", Identifier(a1))?;
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
        check_message(&Error::eof(), "Unexpected end of RON");
        check_message(
            &Error::expected("opening `[`", Span::synthetic()),
            "Expected opening `[`",
        );
        check_message(
            &Error::expected_in("comma", "array", Span::synthetic()),
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
            &Error::missing_field_in("name", "Config"),
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
            "in field 'items' -> element 0: expected i32 but found String"
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
    fn synthetic_span_hidden() {
        let err = Error::type_mismatch("i32", "String");
        // Should NOT show span prefix
        assert!(!err.to_string().contains(':'));
        assert!(err.to_string().starts_with("expected"));
    }

    #[test]
    fn suggestions() {
        let err = Error::unknown_field("nme", &["name", "age", "email"]);
        assert_eq!(err.suggestion(), Some("name".to_string()));

        let err = Error::unknown_variant("Tru", &["True", "False"]);
        assert_eq!(err.suggestion(), Some("True".to_string()));

        let err = Error::new(ErrorKind::SuggestRawIdentifier("type".into()));
        assert_eq!(err.suggestion(), Some("r#type".to_string()));
    }

    #[test]
    fn error_source_chain() {
        let io_err = io::Error::new(io::ErrorKind::NotFound, "file not found");
        let err = Error::from(io_err);

        // Should have source
        assert!(core::error::Error::source(&err).is_some());
    }

    #[test]
    fn error_equality() {
        let err1 = Error::type_mismatch("i32", "String");
        let err2 = Error::type_mismatch("i32", "String");
        assert_eq!(err1, err2);

        let err3 = Error::type_mismatch("i32", "bool");
        assert_ne!(err1, err3);
    }
}
