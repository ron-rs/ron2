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
//! The [`ToRon`] and [`FromRon`] traits (re-exported from `crate`) provide
//! serde-independent serialization and deserialization for RON format:
//!
//! ```rust
//! use crate::schema::{ToRon, FromRon};
//!
//! // Serialize (crate uses compact format without spaces)
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
//! The schema system is built on the [`RonSchema`] trait:
//!
//! - [`RonSchema`] - Core trait for types representable in schemas
//! - [`RonList`] - Marker trait for list/sequence-like types
//! - [`RonMap`] - Marker trait for map/dictionary-like types
//! - [`RonOptional`] - Marker trait for optional/nullable types
//!
//! ## `TypeRef` Canonicalization
//!
//! `RonSchema` derives emit `TypeRef` paths in a canonical form for consistent
//! schema discovery and linking:
//! - Generic parameters remain unqualified (e.g., `T`).
//! - Standard collection types are normalized to short names (e.g., `Vec`, `HashMap`).
//!
//! ## Implementing Custom Types
//!
//! ```rust
//! use crate::schema::{RonSchema, RonList, TypeKind};
//!
//! // A custom list type
//! struct MyVec<T>(Vec<T>);
//!
//! impl<T: RonSchema> RonSchema for MyVec<T> {
//!     fn type_kind() -> TypeKind {
//!         TypeKind::List(Box::new(T::type_kind()))
//!     }
//! }
//!
//! // Mark it as a list type for additional type information
//! impl<T: RonSchema> RonList for MyVec<T> {
//!     type Element = T;
//! }
//! ```
//!
//! # Creating Schemas Manually
//!
//! ```rust
//! use crate::schema::{Schema, TypeKind, Field};
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
//! use crate::schema::ToRon;
//! let ron_str = schema.to_ron().unwrap();
//! println!("{}", ron_str);
//! ```

pub mod collect;
pub mod error;
pub mod storage;
pub mod traits;
pub mod types;
pub mod validation;

// Re-export types (always available)
// Re-export functions that require the derive feature
#[cfg(feature = "derive")]
pub use collect::write_schemas;
pub use collect::{SchemaCatalog, SchemaEntry, collect_schemas};
pub use error::{
    PathSegment, Position, Result, SchemaError, Span, StorageError, ValidationError,
    ValidationErrorKind, ValidationResult,
};
pub use storage::{SCHEMA_DIR_ENV, resolve_schema_dir, type_path_to_file_path};
#[cfg(feature = "derive")]
pub use storage::{find_schema, find_schema_in, read_schema, write_schema};
pub use traits::{RonList, RonMap, RonOptional, RonSchema};
pub use types::{Field, Schema, TypeKind, Variant, VariantKind};
#[cfg(feature = "derive")]
pub use validation::StorageResolver;
pub use validation::{
    AcceptAllResolver, SchemaResolver, validate, validate_expr, validate_expr_collect_all,
    validate_expr_type, validate_expr_type_with_resolver, validate_expr_with_resolver,
    validate_type, validate_type_with_resolver, validate_with_resolver,
};

// Re-export conversion traits from crate
pub use crate::{AstMapAccess, FormatConfig, FromRon, ToRon};
