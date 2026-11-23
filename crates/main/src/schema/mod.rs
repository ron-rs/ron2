//! RON Schema - Schema types, validation, and storage for RON files.
//!
//! This module provides:
//! - [`Schema`] and [`TypeKind`], describing Rust types
//! - the [`RonSchema`] trait, implemented by `#[derive(RonSchema)]`
//! - [`storage`]: reading, writing and locating `.schema.ron` files
//! - [`validation`]: checking parsed RON against a schema
//!
//! # Implementing `RonSchema` manually
//!
//! ```rust
//! use ron2::schema::{RonSchema, TypeKind};
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
//! assert_eq!(MyVec::<u8>::type_kind(), Vec::<u8>::type_kind());
//! ```
//!
//! # Creating and validating schemas manually
//!
//! ```rust
//! use ron2::{
//!     ToRon,
//!     ast::parse_document,
//!     schema::{Field, NoResolver, Schema, TypeKind, validate},
//! };
//!
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
//! let ron = schema.to_ron().unwrap();
//! assert!(ron.contains("Server port"));
//!
//! let doc = parse_document(r#"(port: 8080, host: "localhost")"#).unwrap();
//! assert!(validate(doc.value.as_ref().unwrap(), &schema, &NoResolver).is_ok());
//! ```

pub mod collect;
pub mod error;
pub mod storage;
pub mod traits;
pub mod types;
pub mod validation;

#[cfg(feature = "derive")]
pub use collect::write_schemas;
pub use error::{SchemaError, ValidationError, ValidationErrorKind};
pub use storage::{SCHEMA_DIR_ENV, find_schema_file};
#[cfg(feature = "derive")]
pub use storage::{find_schema, read_schema, write_schema};
pub use traits::RonSchema;
pub use types::{Field, Schema, TypeKind, Variant, VariantKind};
pub use validation::{
    NoResolver, SchemaResolver, Severity, ValidationIssue, validate, validate_all,
};
