//! RON Schema - Schema types, validation, and storage for RON files.
//!
//! This crate provides:
//! - Schema type definitions for representing Rust types
//! - Trait-based schema system for custom types
//! - Storage utilities for reading/writing schema files
//! - Validation of RON values against schemas
//!
//! # Serialization and Deserialization
//!
//! The [`ToRon`] and [`FromRon`] traits (re-exported from `ron2`) provide
//! serde-independent serialization and deserialization for RON format:
//!
//! ```rust
//! use ron_schema::{ToRon, FromRon};
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
//! The schema system is built on the [`RonSchemaType`] trait:
//!
//! - [`RonSchemaType`] - Core trait for types representable in schemas
//! - [`RonList`] - Marker trait for list/sequence-like types
//! - [`RonMap`] - Marker trait for map/dictionary-like types
//! - [`RonOptional`] - Marker trait for optional/nullable types
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
//! // Serialize to RON using the ToRon trait
//! use ron_schema::ToRon;
//! let ron_str = schema.to_ron().unwrap();
//! println!("{}", ron_str);
//! ```

pub mod error;
pub mod storage;
pub mod traits;
pub mod types;
pub mod validation;

// Re-export conversion traits from ron2
pub use ron2::{AstMapAccess, FromRon, PrettyConfig, ToRon};

pub use error::{Result, SchemaError};
pub use storage::{
    find_schema, find_schema_in, read_schema, resolve_schema_dir, type_path_to_file_path,
    write_schema, SCHEMA_DIR_ENV,
};
pub use traits::{RonList, RonMap, RonOptional, RonSchemaType};
pub use types::{Field, Schema, TypeKind, Variant, VariantKind};
pub use validation::{
    validate, validate_type, validate_type_with_resolver, validate_with_resolver,
    AcceptAllResolver, SchemaResolver, StorageResolver,
};
