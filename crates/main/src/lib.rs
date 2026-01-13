//! RON2 - Rusty Object Notation parser with full AST access
//!
//! This crate provides a standalone RON parser with two APIs:
//! - **AST API**: Full fidelity parsing with perfect round-trip support
//! - **Value API**: Simplified access to semantic content only
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
//! use ron2::{Options, Value, ToRon, FormatConfig};
//!
//! let value: Value = Options::default().from_str("Point(x: 1, y: 2)").unwrap();
//! // Pretty output (default)
//! let pretty = value.to_ron().unwrap();
//! // Compact output (no whitespace)
//! let compact = value.to_ron_with(&FormatConfig::minimal()).unwrap();
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
pub mod chars;
pub mod convert;
pub mod error;
pub mod extensions;
pub mod lexer;
pub mod options;
pub mod schema;
pub mod token;
mod util;
pub mod value;

// Re-export formatting config types from ast::fmt
pub use crate::{
    ast::{CommentMode, CompactTypes, Compaction, FormatConfig, Spacing},
    convert::{AstMapAccess, FromRon, FromRonFields, ParsedInt, ToRon, parse_int_raw},
    error::{Error, ErrorKind, PathSegment, Position, Result, Span},
    extensions::Extensions,
    options::Options,
    value::{Map, NamedContent, Number, StructFields, Value},
};
