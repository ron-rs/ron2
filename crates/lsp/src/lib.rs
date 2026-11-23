//! RON Language Server Library
//!
//! A Language Server Protocol implementation for RON (Rusty Object Notation) files.
//! Provides auto-completion, diagnostics, and hover documentation based on schemas.

mod completion;
mod cursor;
pub mod diagnostics;
mod document;
pub mod hover;
mod schema_nav;
mod schema_resolver;
mod schema_utils;

pub use completion::provide_completions;
pub use cursor::{CursorContext, Ident, PathStep};
pub use document::Document;
pub use schema_resolver::{SchemaLookupError, SchemaResolver};
