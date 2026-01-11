//! RON Language Server Library
//!
//! A Language Server Protocol implementation for RON (Rusty Object Notation) files.
//! Provides auto-completion, diagnostics, and hover documentation based on schemas.

mod completion;
pub mod diagnostics;
mod document;
pub mod hover;
mod lsp_utils;
mod schema_resolver;
mod schema_utils;

// Re-export public API
pub use completion::provide_completions;
pub use document::{CompletionContext, Document};
pub use schema_resolver::SchemaResolver;
