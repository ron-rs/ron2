//! RON2 - Rusty Object Notation (Value-only variant)
//!
//! This crate provides serialization and deserialization for RON,
//! but only to/from the [`Value`] type. It does not use serde.
//!
//! # Example
//!
//! ```
//! use ron2::{from_str, to_string, Value};
//!
//! let value = from_str("Point { x: 1, y: 2 }").unwrap();
//! let output = to_string(&value).unwrap();
//! ```

#![deny(clippy::correctness)]
#![deny(clippy::suspicious)]
#![deny(clippy::complexity)]
#![deny(clippy::perf)]
#![deny(clippy::style)]
#![warn(clippy::pedantic)]
#![cfg_attr(not(test), deny(clippy::unwrap_used))]
#![cfg_attr(not(test), deny(clippy::expect_used))]
#![deny(clippy::panic)]
#![warn(clippy::todo)]
#![deny(clippy::unimplemented)]
#![deny(clippy::unreachable)]
#![deny(unsafe_code)]
#![allow(clippy::missing_errors_doc)]
#![warn(clippy::alloc_instead_of_core)]
#![warn(clippy::std_instead_of_alloc)]
#![warn(clippy::std_instead_of_core)]
#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(feature = "std")]
extern crate std;

extern crate alloc;

pub mod ast;
pub mod chars;
pub mod error;
pub mod extensions;
pub mod lexer;
pub mod options;
pub mod ser;
pub mod token;
mod util;
pub mod value;

pub use crate::error::{Error, SpannedError, SpannedResult};
pub use crate::extensions::Extensions;
pub use crate::options::Options;
pub use crate::ser::{PrettyConfig, Serializer};
pub use crate::value::{Map, NamedContent, Number, StructFields, Value};

/// Deserialize a Value from a string.
///
/// # Example
///
/// ```
/// use ron2::{from_str, Value, Number};
///
/// let value = from_str("42").unwrap();
/// assert_eq!(value, Value::Number(Number::U8(42)));
/// ```
pub fn from_str(s: &str) -> SpannedResult<Value> {
    Options::default().from_str(s)
}

/// Deserialize a Value from bytes.
///
/// # Example
///
/// ```
/// use ron2::{from_bytes, Value, Number};
///
/// let value = from_bytes(b"42").unwrap();
/// assert_eq!(value, Value::Number(Number::U8(42)));
/// ```
pub fn from_bytes(s: &[u8]) -> SpannedResult<Value> {
    Options::default().from_bytes(s)
}

/// Deserialize a Value from a reader.
#[cfg(feature = "std")]
pub fn from_reader<R: std::io::Read>(rdr: R) -> SpannedResult<Value> {
    Options::default().from_reader(rdr)
}

/// Serialize a Value to a String.
///
/// # Example
///
/// ```
/// use ron2::{to_string, Value, Number};
///
/// let value = Value::Number(Number::U8(42));
/// let output = to_string(&value).unwrap();
/// assert_eq!(output, "42");
/// ```
pub fn to_string(value: &Value) -> error::Result<alloc::string::String> {
    Options::default().to_string(value)
}

/// Serialize a Value to a String with pretty printing.
///
/// # Example
///
/// ```
/// use ron2::{to_string_pretty, PrettyConfig, Value};
///
/// let value = Value::Seq(vec![Value::from(1_u8), Value::from(2_u8)]);
/// let output = to_string_pretty(&value, PrettyConfig::new()).unwrap();
/// ```
pub fn to_string_pretty(
    value: &Value,
    config: PrettyConfig,
) -> error::Result<alloc::string::String> {
    Options::default().to_string_pretty(value, config)
}

/// Serialize a Value to a writer.
pub fn to_writer<W: core::fmt::Write>(writer: W, value: &Value) -> error::Result<()> {
    Options::default().to_writer(writer, value)
}

/// Serialize a Value to a writer with pretty printing.
pub fn to_writer_pretty<W: core::fmt::Write>(
    writer: W,
    value: &Value,
    config: PrettyConfig,
) -> error::Result<()> {
    Options::default().to_writer_pretty(writer, value, config)
}

/// Serialize a Value to an `io::Write` writer.
#[cfg(feature = "std")]
pub fn to_io_writer<W: std::io::Write>(writer: W, value: &Value) -> error::Result<()> {
    Options::default().to_io_writer(writer, value)
}

/// Serialize a Value to an `io::Write` writer with pretty printing.
#[cfg(feature = "std")]
pub fn to_io_writer_pretty<W: std::io::Write>(
    writer: W,
    value: &Value,
    config: PrettyConfig,
) -> error::Result<()> {
    Options::default().to_io_writer_pretty(writer, value, config)
}
