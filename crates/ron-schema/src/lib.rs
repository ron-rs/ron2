//! RON Schema - Schema types, validation, and storage for RON files.
//!
//! This crate provides:
//! - Schema type definitions for representing Rust types
//! - Storage utilities for reading/writing schema files
//! - Validation of RON values against schemas
//!
//! # Example
//!
//! ```rust
//! use ron_schema::{Schema, TypeKind, Field};
//!
//! // Define a schema for a config struct
//! let schema = Schema::with_doc(
//!     "Application configuration",
//!     TypeKind::Struct {
//!         fields: vec![
//!             Field::new("port", TypeKind::U16).with_doc("Server port"),
//!             Field::optional("host", TypeKind::String).with_doc("Hostname"),
//!         ],
//!     },
//! );
//!
//! // Serialize to RON
//! let ron_str = ron::ser::to_string_pretty(&schema, Default::default()).unwrap();
//! println!("{}", ron_str);
//! ```

pub mod storage;
pub mod types;
pub mod validation;

pub use storage::{
    find_schema, find_schema_in, read_schema, resolve_schema_dir, write_schema, StorageError,
    SCHEMA_DIR_ENV,
};
pub use types::{Field, Schema, TypeKind, Variant, VariantKind};
pub use validation::{validate, validate_type, ValidationError};

use std::path::PathBuf;

/// Trait implemented by types that have a RON schema.
///
/// This trait is automatically implemented by the `#[derive(RonSchema)]` macro
/// from the `ron-derive` crate.
pub trait RonSchema {
    /// Returns the schema for this type.
    fn schema() -> Schema;

    /// Writes the schema to the configured output directory.
    ///
    /// Returns the path to the written schema file.
    fn write_schema() -> Result<PathBuf, StorageError>;

    /// Returns the fully-qualified type path for this type.
    fn type_path() -> &'static str;
}
