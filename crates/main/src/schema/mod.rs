//! RON Schema - Schema types, validation, and storage for RON files.
//!
//! This module provides:
//! - Schema type definitions for representing Rust types
//! - Trait-based schema system for custom types
//! - Storage utilities for reading/writing schema files
//! - Validation of RON values against schemas
//!
//! # Quick Start
//!
//! Most users only need these imports:
//!
//! ```rust
//! use ron2::schema::{Schema, TypeKind, Field, RonSchema};
//! ```
//!
//! # API Tiers
//!
//! This module provides a **minimal public API** at the root level. Advanced
//! functionality is available through submodules:
//!
//! - **Root level**: Core types, traits, and common operations
//! - **Submodules**: Advanced validation functions, storage utilities, error details
//!
//! ## Submodule Access
//!
//! For advanced use cases, access the submodules directly:
//!
//! ```rust,ignore
//! // Advanced validation functions
//! use ron2::schema::validation::{validate_type, validate_with_resolver};
//!
//! // Storage internals
//! use ron2::schema::storage::{type_path_to_file_path, resolve_schema_dir};
//!
//! // Schema collection internals
//! use ron2::schema::collect::{SchemaCatalog, SchemaEntry, collect_schemas};
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
//! ## Implementing Custom Types
//!
//! ```rust,ignore
//! use ron2::schema::{RonSchema, RonList, TypeKind};
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
//! ```rust,ignore
//! use ron2::{ToRon, schema::{Schema, TypeKind, Field}};
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
//! let ron_str = schema.to_ron().unwrap();
//! println!("{}", ron_str);
//! ```

// Submodules - public for advanced access
pub mod collect;
pub mod error;
pub mod storage;
pub mod traits;
pub mod types;
pub mod validation;

// =============================================================================
// Core Types
// =============================================================================

// =============================================================================
// Storage (derive feature only)
// =============================================================================
#[cfg(feature = "derive")]
pub use collect::write_schemas;
// =============================================================================
// Errors
// =============================================================================
pub use error::{SchemaError, ValidationError, ValidationErrorKind};
#[cfg(feature = "derive")]
pub use storage::{SCHEMA_DIR_ENV, find_schema, find_schema_in, read_schema, write_schema};
// =============================================================================
// Traits
// =============================================================================
pub use traits::{RonList, RonMap, RonOptional, RonSchema};
pub use types::{Field, Schema, TypeKind, Variant, VariantKind};
// =============================================================================
// Validation
// =============================================================================
pub use validation::{SchemaResolver, validate, validate_expr_collect_all, validate_with_resolver};
