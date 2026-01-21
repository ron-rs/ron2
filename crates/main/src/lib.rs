//! RON2 - Rusty Object Notation parser with full AST access
//!
//! This crate provides a standalone RON parser with three APIs:
//! - **AST API**: Full fidelity parsing with perfect round-trip support
//! - **Value API**: Simplified access to semantic content only
//! - **Typed Conversions**: [`FromRon`] and [`ToRon`] traits for Rust types
//!
//! No serde dependency required.
//!
//! # AST Example (full fidelity)
//!
//! ```
//! use ron2::ast::{parse_document, serialize_document};
//!
//! let source = "// config\nPoint(x: 1, y: 2)";
//! let doc = parse_document(source).unwrap();
//! let output = serialize_document(&doc).unwrap();
//! assert_eq!(source, output); // Perfect round-trip
//! ```
//!
//! # Value Example (semantic only)
//!
//! ```
//! use ron2::Value;
//!
//! let value: Value = "[1, 2, 3]".parse().unwrap();
//! assert!(matches!(value, Value::Seq(_)));
//! ```
//!
//! # Typed Conversions
//!
//! ```
//! use ron2::{FromRon, ToRon};
//!
//! let numbers: Vec<i32> = Vec::from_ron("[1, 2, 3]").unwrap();
//! assert_eq!(numbers, vec![1, 2, 3]);
//!
//! let ron_string = numbers.to_ron().unwrap();
//! ```

#![deny(clippy::correctness)]
#![deny(clippy::suspicious)]
#![deny(clippy::complexity)]
#![deny(clippy::perf)]
#![deny(clippy::style)]
#![warn(clippy::pedantic)]
#![cfg_attr(not(test), deny(clippy::unwrap_used))]
#![cfg_attr(not(test), deny(clippy::expect_used))]
#![cfg_attr(not(test), deny(clippy::panic))]
#![warn(clippy::todo)]
#![deny(clippy::unimplemented)]
#![deny(clippy::unreachable)]
#![deny(unsafe_code)]
#![allow(clippy::missing_errors_doc)]
#![warn(clippy::alloc_instead_of_core)]
#![warn(clippy::std_instead_of_alloc)]
#![warn(clippy::std_instead_of_core)]

extern crate std;

extern crate alloc;
extern crate self as ron2;

pub mod ast;
pub(crate) mod chars;
pub mod convert;
pub mod error;
pub(crate) mod lexer;
pub mod schema;
pub(crate) mod token;
mod util;
pub mod value;

// Re-export formatting config types from ast::fmt
// Re-export derive macros when the derive feature is enabled
#[cfg(feature = "derive")]
pub use ron2_derive::{FromRon, Ron, RonSchema, ToRon};

pub use crate::{
    ast::{CommentMode, CompactTypes, Compaction, FormatConfig, Spacing},
    convert::{
        AstMapAccess, FromRon, FromRonFields, SerializeConfig, Spanned, ToRon, ToRonDocument,
    },
    error::{Error, ErrorKind, PathSegment, Position, Result, Span},
    value::{Map, NamedContent, Number, StructFields, Value},
};

/// Internal module for benchmarks. Not part of the public API.
#[doc(hidden)]
pub mod __internal {
    pub use crate::lexer::Lexer;
}

/// Format RON source with the given configuration.
///
/// # Example
/// ```
/// use ron2::FormatConfig;
///
/// let source = "(x:1,y:2)";
/// let formatted = ron2::format(source, &FormatConfig::default())?;
/// # Ok::<(), ron2::Error>(())
/// ```
pub fn format(source: &str, config: &FormatConfig) -> Result<alloc::string::String> {
    let doc = ast::parse_document(source)?;
    Ok(ast::format_document(&doc, config))
}

/// Parse a RON string into a type that implements `FromRon`.
///
/// # Example
/// ```
/// use ron2::from_str;
///
/// let value: i32 = ron2::from_str("42")?;
/// assert_eq!(value, 42);
/// # Ok::<(), ron2::Error>(())
/// ```
pub fn from_str<T: FromRon>(s: &str) -> Result<T> {
    T::from_ron(s)
}

/// Serialize a value to a RON string with default formatting.
///
/// # Example
/// ```
/// use ron2::{to_string, Value};
///
/// let value = Value::Bool(true);
/// let ron = ron2::to_string(&value)?;
/// assert_eq!(ron, "true");
/// # Ok::<(), ron2::Error>(())
/// ```
pub fn to_string<T: ToRon>(value: &T) -> Result<alloc::string::String> {
    value.to_ron()
}

/// Serialize a value to a RON string with custom formatting.
///
/// # Example
/// ```
/// use ron2::{to_string_with, FormatConfig, Value};
///
/// let value = Value::Bool(true);
/// let ron = ron2::to_string_with(&value, &FormatConfig::minimal())?;
/// assert_eq!(ron, "true");
/// # Ok::<(), ron2::Error>(())
/// ```
pub fn to_string_with<T: ToRon>(value: &T, config: &FormatConfig) -> Result<alloc::string::String> {
    value.to_ron_with(config)
}
