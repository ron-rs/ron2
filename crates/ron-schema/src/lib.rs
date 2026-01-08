//! RON Schema - Schema types, validation, and storage for RON files.
//!
//! This crate provides:
//! - Schema type definitions for representing Rust types
//! - Trait-based schema system for custom types
//! - Storage utilities for reading/writing schema files
//! - Validation of RON values against schemas
//! - Serde-independent serialization ([`SerRon`]) and deserialization ([`DeRon`]) traits
//!
//! # Serialization and Deserialization
//!
//! The [`SerRon`] and [`DeRon`] traits provide serde-independent serialization
//! and deserialization for RON format:
//!
//! ```rust
//! use ron_schema::{SerRon, DeRon};
//!
//! // Serialize (ron2 uses compact format without spaces)
//! let values = vec![1, 2, 3];
//! let ron_string = values.to_ron().unwrap();
//! assert_eq!(ron_string, "[1,2,3]");
//!
//! // Deserialize
//! let parsed: Vec<i32> = Vec::from_ron("[1, 2, 3]").unwrap();
//! assert_eq!(parsed, vec![1, 2, 3]);
//! ```
//!
//! # Trait-Based Schema System
//!
//! The schema system is built on traits that allow custom types to participate:
//!
//! - [`RonSchemaType`] - Core trait for types representable in schemas
//! - [`RonList`] - Marker trait for list/sequence-like types
//! - [`RonMap`] - Marker trait for map/dictionary-like types
//! - [`RonOptional`] - Marker trait for optional/nullable types
//! - [`RonSchema`] - High-level trait for types with full schema support
//!
//! ## Implementing Custom Types
//!
//! ```rust
//! use ron_schema::{RonSchemaType, RonList, TypeKind};
//!
//! // A custom list type
//! struct MyVec<T>(Vec<T>);
//!
//! impl<T: RonSchemaType> RonSchemaType for MyVec<T> {
//!     fn type_kind() -> TypeKind {
//!         TypeKind::List(Box::new(T::type_kind()))
//!     }
//! }
//!
//! // Mark it as a list type for additional type information
//! impl<T: RonSchemaType> RonList for MyVec<T> {
//!     type Element = T;
//! }
//! ```
//!
//! # Creating Schemas Manually
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
//! // Serialize to RON using the SerRon trait
//! use ron_schema::SerRon;
//! let ron_str = schema.to_ron().unwrap();
//! println!("{}", ron_str);
//! ```

pub mod de;
pub mod error;
pub mod ser;
pub mod storage;
pub mod traits;
pub mod types;
pub mod validation;

pub use de::{parse_ron_value, DeRon, MapAccess};
pub use error::RonError;
pub use ser::{value_to_string, value_to_string_pretty, PrettyConfig, SerRon};
pub use storage::{
    find_schema, find_schema_in, read_schema, resolve_schema_dir, write_schema, StorageError,
    SCHEMA_DIR_ENV,
};
pub use traits::{RonList, RonMap, RonOptional, RonSchemaType};
pub use types::{Field, Schema, TypeKind, Variant, VariantKind};
pub use validation::{validate, validate_type, ValidationError};

use std::path::PathBuf;

/// Trait implemented by types that have a full RON schema.
///
/// This trait is automatically implemented by the `#[derive(RonSchema)]` macro
/// from the `ron-derive` crate. It combines [`RonSchemaType`] with additional
/// capabilities for schema storage and type path resolution.
///
/// # Difference from `RonSchemaType`
///
/// - [`RonSchemaType`] is the low-level trait for getting type information
/// - [`RonSchema`] is the high-level trait that adds schema storage and type paths
///
/// Most users will use `#[derive(RonSchema)]` which implements both traits.
pub trait RonSchema: RonSchemaType {
    /// Returns the complete schema for this type, including documentation.
    fn schema() -> Schema;

    /// Writes the schema to the configured output directory.
    ///
    /// Returns the path to the written schema file.
    fn write_schema() -> Result<PathBuf, StorageError>;

    /// Returns the fully-qualified type path for this type.
    ///
    /// This is used to locate schema files and for `TypeRef` references.
    fn type_path() -> &'static str;
}
