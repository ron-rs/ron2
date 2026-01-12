//! Error types for RON parsing and deserialization.
//!
//! This module provides error types for parsing and validation:
//! - [`Error`] - All ron2 errors (parsing + validation)
//! - [`SpannedError`] - Error with source location
//! - [`Position`], [`Span`] - Source position types
//! - [`ValidationError`], [`ValidationErrorKind`] - Validation errors
//! - [`PathSegment`] - Path context for errors

use alloc::{
    borrow::Cow,
    string::{String, ToString},
};
use core::{fmt, str::Utf8Error};

use crate::chars::{is_ident_first_char, is_ident_raw_char};
use unicode_ident::is_xid_continue;

use std::io;

mod path;
mod span;
mod validation;

pub use path::PathSegment;
pub use span::{Position, Span};
pub use validation::{ValidationError, ValidationErrorKind};

/// This type represents all possible errors that can occur when
/// serializing or deserializing RON data.
#[allow(clippy::module_name_repetitions)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SpannedError {
    pub code: Error,
    pub span: Span,
}

pub type Result<T, E = Error> = core::result::Result<T, E>;
pub type SpannedResult<T> = core::result::Result<T, SpannedError>;

#[derive(Clone, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub enum Error {
    Fmt,
    Io(String),
    Message(String),
    Eof,
    ExpectedArray,
    ExpectedArrayEnd,
    ExpectedAttribute,
    ExpectedAttributeEnd,
    ExpectedBoolean,
    /// Expected a comma separator.
    ///
    /// The optional context indicates where the comma was expected
    /// (e.g., "array", "map", "tuple", "struct").
    ExpectedComma {
        /// Optional context about where the comma was expected.
        context: Option<&'static str>,
    },
    ExpectedChar,
    ExpectedByteLiteral,
    ExpectedFloat,
    FloatUnderscore,
    ExpectedInteger,
    ExpectedOption,
    ExpectedOptionEnd,
    ExpectedMap,
    /// Expected a colon separator.
    ///
    /// The optional context indicates where the colon was expected
    /// (e.g., "map entry", "struct field").
    ExpectedMapColon {
        /// Optional context about where the colon was expected.
        context: Option<&'static str>,
    },
    ExpectedMapEnd,
    ExpectedDifferentStructName {
        expected: &'static str,
        found: String,
    },
    ExpectedStructLike,
    ExpectedNamedStructLike(&'static str),
    ExpectedStructLikeEnd,
    ExpectedUnit,
    ExpectedString,
    ExpectedByteString,
    ExpectedStringEnd,
    ExpectedIdentifier,

    InvalidEscape(&'static str),

    /// Integer out of bounds with context about the value and target type.
    ///
    /// Note: Consider using [`ValidationErrorKind::IntegerOutOfBounds`]
    /// and converting via [`Error::from_validation_kind`] for consistency.
    #[deprecated(
        since = "0.2.0",
        note = "use ValidationErrorKind::IntegerOutOfBounds and Error::from_validation_kind() instead"
    )]
    IntegerOutOfBounds {
        /// The string representation of the integer that was out of bounds.
        value: Cow<'static, str>,
        /// The target type that couldn't hold the value.
        target_type: &'static str,
    },
    InvalidIntegerDigit {
        digit: char,
        base: u8,
    },

    NoSuchExtension(String),

    UnclosedBlockComment,
    UnclosedLineComment,
    UnderscoreAtBeginning,
    UnexpectedChar(char),

    Utf8Error(Utf8Error),
    TrailingCharacters,

    /// Type mismatch error.
    ///
    /// Note: Consider using [`ValidationErrorKind::TypeMismatch`]
    /// and converting via [`Error::from_validation_kind`] for consistency.
    #[deprecated(
        since = "0.2.0",
        note = "use ValidationErrorKind::TypeMismatch and Error::from_validation_kind() instead"
    )]
    InvalidValueForType {
        expected: String,
        found: String,
    },
    ExpectedDifferentLength {
        expected: String,
        found: usize,
    },
    /// Unknown enum variant error.
    ///
    /// Note: Consider using [`ValidationErrorKind::UnknownVariant`]
    /// and converting via [`Error::from_validation_kind`] for consistency.
    #[deprecated(
        since = "0.2.0",
        note = "use ValidationErrorKind::UnknownVariant and Error::from_validation_kind() instead"
    )]
    NoSuchEnumVariant {
        expected: &'static [&'static str],
        found: Cow<'static, str>,
        outer: Option<Cow<'static, str>>,
    },
    /// Unknown struct field error.
    ///
    /// Note: Consider using [`ValidationErrorKind::UnknownField`]
    /// and converting via [`Error::from_validation_kind`] for consistency.
    #[deprecated(
        since = "0.2.0",
        note = "use ValidationErrorKind::UnknownField and Error::from_validation_kind() instead"
    )]
    NoSuchStructField {
        expected: &'static [&'static str],
        found: Cow<'static, str>,
        outer: Option<Cow<'static, str>>,
    },
    /// Missing struct field error.
    ///
    /// Note: Consider using [`ValidationErrorKind::MissingField`]
    /// and converting via [`Error::from_validation_kind`] for consistency.
    #[deprecated(
        since = "0.2.0",
        note = "use ValidationErrorKind::MissingField and Error::from_validation_kind() instead"
    )]
    MissingStructField {
        field: Cow<'static, str>,
        outer: Option<Cow<'static, str>>,
    },
    /// Duplicate struct field error.
    ///
    /// Note: Consider using [`ValidationErrorKind::DuplicateField`]
    /// and converting via [`Error::from_validation_kind`] for consistency.
    #[deprecated(
        since = "0.2.0",
        note = "use ValidationErrorKind::DuplicateField and Error::from_validation_kind() instead"
    )]
    DuplicateStructField {
        field: Cow<'static, str>,
        outer: Option<Cow<'static, str>>,
    },
    InvalidIdentifier(String),
    SuggestRawIdentifier(String),
    ExpectedRawValue,
    ExceededRecursionLimit,
    /// Too many fields in struct for deserialization.
    ///
    /// The AST-based deserializer uses a 64-bit bitmask for field tracking,
    /// limiting structs to 64 fields maximum.
    TooManyFields {
        /// Number of fields found
        count: usize,
        /// Maximum supported
        limit: usize,
    },
    ExpectedStructName(String),
}

impl SpannedError {
    /// Creates a `SpannedError` that wraps the given error code with a span
    /// covering from position (1,1) to the end of the source.
    #[must_use]
    pub fn wrap(code: Error, source: &str) -> Self {
        Self {
            code,
            span: Span {
                start: Position { line: 1, col: 1 },
                end: Position::from_src_end(source),
                start_offset: 0,
                end_offset: source.len(),
            },
        }
    }

    /// Creates a `SpannedError` at position (1,1) with zero-length span.
    #[must_use]
    pub fn at_start(code: Error) -> Self {
        Self {
            code,
            span: Span {
                start: Position { line: 1, col: 1 },
                end: Position { line: 1, col: 1 },
                start_offset: 0,
                end_offset: 0,
            },
        }
    }
}

impl fmt::Display for SpannedError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.span, self.code)
    }
}

impl fmt::Display for Error {
    #[allow(clippy::too_many_lines, deprecated)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Error::Fmt => f.write_str("Formatting RON failed"),
            Error::Io(ref s) | Error::Message(ref s) => f.write_str(s),
            Error::Eof => f.write_str("Unexpected end of RON"),
            Error::ExpectedArray => f.write_str("Expected opening `[`"),
            Error::ExpectedArrayEnd => f.write_str("Expected closing `]`"),
            Error::ExpectedAttribute => f.write_str("Expected an `#![enable(...)]` attribute"),
            Error::ExpectedAttributeEnd => {
                f.write_str("Expected closing `)]` after the enable attribute")
            }
            Error::ExpectedBoolean => f.write_str("Expected boolean"),
            Error::ExpectedComma { context: Some(ctx) } => write!(f, "Expected comma in {ctx}"),
            Error::ExpectedComma { context: None } => f.write_str("Expected comma"),
            Error::ExpectedChar => f.write_str("Expected char"),
            Error::ExpectedByteLiteral => f.write_str("Expected byte literal"),
            Error::ExpectedFloat => f.write_str("Expected float"),
            Error::FloatUnderscore => f.write_str("Unexpected underscore in float"),
            Error::ExpectedInteger => f.write_str("Expected integer"),
            Error::ExpectedOption => f.write_str("Expected option"),
            Error::ExpectedOptionEnd | Error::ExpectedStructLikeEnd => {
                f.write_str("Expected closing `)`")
            }
            Error::ExpectedMap => f.write_str("Expected opening `{`"),
            Error::ExpectedMapColon { context: Some(ctx) } => write!(f, "Expected colon in {ctx}"),
            Error::ExpectedMapColon { context: None } => f.write_str("Expected colon"),
            Error::ExpectedMapEnd => f.write_str("Expected closing `}`"),
            Error::ExpectedDifferentStructName {
                expected,
                ref found,
            } => write!(
                f,
                "Expected struct {} but found {}",
                Identifier(expected),
                Identifier(found)
            ),
            Error::ExpectedStructLike => f.write_str("Expected opening `(`"),
            Error::ExpectedNamedStructLike(name) => {
                if name.is_empty() {
                    f.write_str("Expected only opening `(`, no name, for un-nameable struct")
                } else {
                    write!(f, "Expected opening `(` for struct {}", Identifier(name))
                }
            }
            Error::ExpectedUnit => f.write_str("Expected unit"),
            Error::ExpectedString => f.write_str("Expected string"),
            Error::ExpectedByteString => f.write_str("Expected byte string"),
            Error::ExpectedStringEnd => f.write_str("Expected end of string"),
            Error::ExpectedIdentifier => f.write_str("Expected identifier"),
            Error::InvalidEscape(s) => f.write_str(s),
            Error::IntegerOutOfBounds {
                ref value,
                target_type,
            } => {
                write!(f, "Integer {value} is out of bounds for {target_type}")
            }
            Error::InvalidIntegerDigit { digit, base } => {
                write!(f, "Invalid digit {digit:?} for base {base} integers")
            }
            Error::NoSuchExtension(ref name) => {
                write!(f, "No RON extension named {}", Identifier(name))
            }
            Error::Utf8Error(ref e) => fmt::Display::fmt(e, f),
            Error::UnclosedBlockComment => f.write_str("Unclosed block comment"),
            Error::UnclosedLineComment => f.write_str(
                "`ron::value::RawValue` cannot end in unclosed line comment, \
                try using a block comment or adding a newline",
            ),
            Error::UnderscoreAtBeginning => {
                f.write_str("Unexpected leading underscore in a number")
            }
            Error::UnexpectedChar(c) => write!(f, "Unexpected char {c:?}"),
            Error::TrailingCharacters => f.write_str("Non-whitespace trailing characters"),
            Error::InvalidValueForType {
                ref expected,
                ref found,
            } => {
                write!(f, "Expected {expected} but found {found} instead")
            }
            Error::ExpectedDifferentLength {
                ref expected,
                found,
            } => {
                write!(f, "Expected {expected} but found ")?;

                match found {
                    0 => f.write_str("zero elements")?,
                    1 => f.write_str("one element")?,
                    n => write!(f, "{n} elements")?,
                }

                f.write_str(" instead")
            }
            Error::NoSuchEnumVariant {
                expected,
                ref found,
                ref outer,
            } => {
                f.write_str("Unknown ")?;

                if outer.is_none() {
                    f.write_str("enum ")?;
                }

                write!(f, "variant {}", Identifier(found.as_ref()))?;

                if let Some(outer) = outer {
                    write!(f, " in enum {}", Identifier(outer.as_ref()))?;
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
            Error::NoSuchStructField {
                expected,
                ref found,
                ref outer,
            } => {
                write!(f, "Unknown field {}", Identifier(found.as_ref()))?;

                if let Some(outer) = outer {
                    write!(f, " in {}", Identifier(outer.as_ref()))?;
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
            Error::MissingStructField {
                ref field,
                ref outer,
            } => {
                write!(f, "Missing required field {}", Identifier(field.as_ref()))?;

                match outer {
                    Some(outer) => write!(f, " in {}", Identifier(outer.as_ref())),
                    None => Ok(()),
                }
            }
            Error::DuplicateStructField {
                ref field,
                ref outer,
            } => {
                write!(f, "Duplicate field {}", Identifier(field.as_ref()))?;

                match outer {
                    Some(outer) => write!(f, " in {}", Identifier(outer.as_ref())),
                    None => Ok(()),
                }
            }
            Error::InvalidIdentifier(ref invalid) => write!(f, "Invalid identifier {invalid:?}"),
            Error::SuggestRawIdentifier(ref identifier) => write!(
                f,
                "Found invalid std identifier {identifier:?}, try the raw identifier `r#{identifier}` instead"
            ),
            Error::ExpectedRawValue => f.write_str("Expected a `ron::value::RawValue`"),
            Error::ExceededRecursionLimit => f.write_str(
                "Exceeded recursion limit, try increasing `ron::Options::recursion_limit`",
            ),
            Error::TooManyFields { count, limit } => {
                write!(f, "Struct has {count} fields but maximum is {limit}")
            }
            Error::ExpectedStructName(ref name) => write!(
                f,
                "Expected the explicit struct name {}, but none was found",
                Identifier(name)
            ),
        }
    }
}

impl core::error::Error for SpannedError {}

impl core::error::Error for Error {}

impl From<Utf8Error> for Error {
    fn from(e: Utf8Error) -> Self {
        Error::Utf8Error(e)
    }
}

impl From<fmt::Error> for Error {
    fn from(_: fmt::Error) -> Self {
        Error::Fmt
    }
}

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Self {
        Error::Io(e.to_string())
    }
}

impl From<SpannedError> for Error {
    fn from(e: SpannedError) -> Self {
        e.code
    }
}

impl Error {
    /// Convert a [`ValidationErrorKind`] to an [`Error`].
    ///
    /// This is the canonical way to create validation-related errors.
    /// The deprecated error variants (e.g., `IntegerOutOfBounds`, `InvalidValueForType`)
    /// are mapped from their corresponding `ValidationErrorKind` variants.
    ///
    /// # Example
    ///
    /// ```
    /// use ron2::error::Error;
    /// use ron2::ValidationErrorKind;
    ///
    /// let kind = ValidationErrorKind::TypeMismatch {
    ///     expected: "i32".into(),
    ///     found: "String".into(),
    /// };
    /// let error = Error::from_validation_kind(kind);
    /// ```
    #[must_use]
    #[allow(deprecated)]
    pub fn from_validation_kind(kind: ValidationErrorKind) -> Self {
        match kind {
            ValidationErrorKind::TypeMismatch { expected, found } => {
                Error::InvalidValueForType { expected, found }
            }
            ValidationErrorKind::MissingField { field, outer } => {
                Error::MissingStructField { field, outer }
            }
            ValidationErrorKind::UnknownField {
                field,
                expected,
                outer,
            } => Error::NoSuchStructField {
                expected,
                found: field,
                outer,
            },
            ValidationErrorKind::UnknownVariant {
                variant,
                expected,
                outer,
            } => Error::NoSuchEnumVariant {
                expected,
                found: variant,
                outer,
            },
            ValidationErrorKind::DuplicateField { field, outer } => {
                Error::DuplicateStructField { field, outer }
            }
            ValidationErrorKind::LengthMismatch {
                expected,
                found,
                context,
            } => Error::ExpectedDifferentLength {
                expected: if let Some(ctx) = context {
                    alloc::format!("{ctx} with {expected} elements")
                } else {
                    alloc::format!("{expected} elements")
                },
                found,
            },
            ValidationErrorKind::IntegerOutOfBounds { value, target_type } => {
                Error::IntegerOutOfBounds { value, target_type }
            }
        }
    }
}

/// Convert a [`ValidationError`] to a [`SpannedError`].
///
/// The span is taken from the [`ValidationError`] if present, otherwise a synthetic span is used.
impl From<ValidationError> for SpannedError {
    fn from(e: ValidationError) -> Self {
        let span = e.span.clone().unwrap_or_else(Span::synthetic);
        let code = Error::from_validation_kind(e.kind);
        Self { code, span }
    }
}

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
        if self.0.is_empty() || !self.0.chars().all(is_ident_raw_char) {
            return write!(f, "{:?}_[invalid identifier]", self.0);
        }

        let mut chars = self.0.chars();

        if !chars.next().is_some_and(is_ident_first_char) || !chars.all(is_xid_continue) {
            write!(f, "`r#{}`", self.0)
        } else {
            write!(f, "`{}`", self.0)
        }
    }
}

#[cfg(test)]
#[allow(deprecated)]
mod tests {
    use alloc::string::String;

    use super::{
        Error, PathSegment, Position, Span, SpannedError, ValidationError, ValidationErrorKind,
    };

    #[test]
    fn error_messages() {
        check_error_message(&Error::from(core::fmt::Error), "Formatting RON failed");
        check_error_message(&Error::Message(String::from("custom")), "custom");
        check_error_message(&Error::Eof, "Unexpected end of RON");
        check_error_message(&Error::ExpectedArray, "Expected opening `[`");
        check_error_message(&Error::ExpectedArrayEnd, "Expected closing `]`");
        check_error_message(
            &Error::ExpectedAttribute,
            "Expected an `#![enable(...)]` attribute",
        );
        check_error_message(
            &Error::ExpectedAttributeEnd,
            "Expected closing `)]` after the enable attribute",
        );
        check_error_message(&Error::ExpectedBoolean, "Expected boolean");
        check_error_message(&Error::ExpectedComma { context: None }, "Expected comma");
        check_error_message(
            &Error::ExpectedComma {
                context: Some("array"),
            },
            "Expected comma in array",
        );
        check_error_message(&Error::ExpectedChar, "Expected char");
        check_error_message(&Error::ExpectedByteLiteral, "Expected byte literal");
        check_error_message(&Error::ExpectedFloat, "Expected float");
        check_error_message(&Error::FloatUnderscore, "Unexpected underscore in float");
        check_error_message(&Error::ExpectedInteger, "Expected integer");
        check_error_message(&Error::ExpectedOption, "Expected option");
        check_error_message(&Error::ExpectedOptionEnd, "Expected closing `)`");
        check_error_message(&Error::ExpectedStructLikeEnd, "Expected closing `)`");
        check_error_message(&Error::ExpectedMap, "Expected opening `{`");
        check_error_message(&Error::ExpectedMapColon { context: None }, "Expected colon");
        check_error_message(
            &Error::ExpectedMapColon {
                context: Some("struct field"),
            },
            "Expected colon in struct field",
        );
        check_error_message(&Error::ExpectedMapEnd, "Expected closing `}`");
        check_error_message(&Error::ExpectedStructLike, "Expected opening `(`");
        check_error_message(&Error::ExpectedUnit, "Expected unit");
        check_error_message(&Error::ExpectedString, "Expected string");
        check_error_message(&Error::ExpectedByteString, "Expected byte string");
        check_error_message(&Error::ExpectedStringEnd, "Expected end of string");
        check_error_message(&Error::ExpectedIdentifier, "Expected identifier");
        check_error_message(&Error::InvalidEscape("Invalid escape"), "Invalid escape");
        check_error_message(
            &Error::IntegerOutOfBounds {
                value: "256".into(),
                target_type: "u8",
            },
            "Integer 256 is out of bounds for u8",
        );
        check_error_message(&Error::UnclosedBlockComment, "Unclosed block comment");
        check_error_message(
            &Error::TrailingCharacters,
            "Non-whitespace trailing characters",
        );
    }

    fn check_error_message<T: core::fmt::Display>(err: &T, msg: &str) {
        assert_eq!(alloc::format!("{err}"), msg);
    }

    #[test]
    fn spanned_error_into_code() {
        assert_eq!(
            Error::from(SpannedError {
                code: Error::Eof,
                span: Span {
                    start: Position { line: 1, col: 1 },
                    end: Position { line: 1, col: 5 },
                    start_offset: 0,
                    end_offset: 4,
                }
            }),
            Error::Eof
        );
    }

    #[test]
    fn test_ron_error_reexports() {
        // Verify helper error types are accessible
        let pos = Position { line: 1, col: 1 };
        assert_eq!(pos.line, 1);
        let span = Span::synthetic();
        assert!(span.is_synthetic());

        let seg = PathSegment::Field("test".into());
        assert!(matches!(seg, PathSegment::Field(_)));

        let kind = ValidationErrorKind::TypeMismatch {
            expected: "String".into(),
            found: "i32".into(),
        };
        let _err = ValidationError::new(kind);
    }

    #[test]
    fn test_validation_error_to_spanned_error_conversion() {
        let span = Span {
            start: Position { line: 3, col: 15 },
            end: Position { line: 3, col: 20 },
            start_offset: 50,
            end_offset: 55,
        };

        let validation_err = ValidationError::with_span(
            ValidationErrorKind::MissingField {
                field: "name".into(),
                outer: Some("Config".into()),
            },
            span.clone(),
        );

        let spanned: SpannedError = validation_err.into();

        // Verify span is preserved
        assert_eq!(spanned.span, span);

        // Verify error kind is converted correctly
        match spanned.code {
            Error::MissingStructField { field, outer } => {
                assert_eq!(field.as_ref(), "name");
                assert_eq!(outer.as_deref(), Some("Config"));
            }
            _ => panic!("Expected MissingStructField error"),
        }
    }

    #[test]
    fn test_validation_error_without_span_uses_synthetic() {
        let validation_err = ValidationError::type_mismatch("bool", "string");

        let spanned: SpannedError = validation_err.into();

        // Verify synthetic span is used
        assert!(spanned.span.is_synthetic());
    }
}
