//! RON derive macros for serialization, deserialization, and schema generation.
//!
//! This crate provides derive macros:
//!
//! - `#[derive(Ron)]` - Implements ToRon, FromRon, and RonSchema (all three)
//! - `#[derive(ToRon)]` - Serialize to RON without serde
//! - `#[derive(FromRon)]` - Deserialize from RON without serde
//! - `#[derive(RonSchema)]` - Generate RON schema files at compile time
//!
//! All derives share the `#[ron(...)]` attribute namespace.
//!
//! # Example
//!
//! ```ignore
//! use ron2_derive::Ron;
//!
//! #[derive(Debug, PartialEq, Ron)]
//! /// Application configuration
//! struct AppConfig {
//!     /// Server port
//!     port: u16,
//!     /// Optional hostname
//!     #[ron(default)]
//!     host: Option<String>,
//! }
//! ```
//!
//! # Container Attributes
//!
//! - `#[ron(rename = "Name")]` - Rename the type in RON output
//! - `#[ron(rename_all = "camelCase")]` - Rename all fields (camelCase, snake_case, PascalCase, etc.)
//! - `#[ron(deny_unknown_fields)]` - Error on unknown fields during deserialization
//! - `#[ron(transparent)]` - Serialize/deserialize as the single inner field
//!
//! # Field Attributes
//!
//! - `#[ron(rename = "name")]` - Rename this field
//! - `#[ron(skip)]` - Skip this field entirely
//! - `#[ron(skip_serializing)]` - Skip during serialization only
//! - `#[ron(skip_deserializing)]` - Skip during deserialization (use default)
//! - `#[ron(default)]` - Use `Default::default()` if missing
//! - `#[ron(default = "path::to::fn")]` - Use custom default function
//! - `#[ron(flatten)]` - Flatten nested struct fields into parent
//! - `#[ron(skip_serializing_if = "path::to::fn")]` - Skip if predicate returns true
//! - `#[ron(explicit)]` - Require explicit `Some(...)` or `None` for Option fields
//!
//! # Variant Attributes
//!
//! - `#[ron(rename = "Name")]` - Rename this variant
//! - `#[ron(skip)]` - Skip this variant
//!
//! # Extension Behavior
//!
//! ## Implicit Some (Default)
//!
//! `Option<T>` fields accept bare values without `Some(...)`:
//!
//! ```ignore
//! #[derive(FromRon)]
//! struct Config {
//!     name: Option<String>,
//! }
//! // Accepts: (name: "Alice") or (name: Some("Alice")) or (name: None)
//! ```
//!
//! ## Explicit Option (`#[ron(explicit)]`)
//!
//! Require `Some(...)` or `None` syntax for disambiguation:
//!
//! ```ignore
//! #[derive(FromRon)]
//! struct Config {
//!     #[ron(explicit)]
//!     value: Option<Option<bool>>,
//! }
//! // Requires: (value: Some(Some(true))) or (value: Some(None)) or (value: None)
//! ```
//!
//! ## Transparent Newtypes (`#[ron(transparent)]`)
//!
//! Single-field structs serialize as their inner type:
//!
//! ```ignore
//! #[derive(FromRon, ToRon)]
//! #[ron(transparent)]
//! struct UserId(u64);
//!
//! // Serializes as: 42
//! // Not as: UserId(42)
//! ```
//!
//! # Legacy Support
//!
//! The `#[ron_schema(...)]` attribute is still supported for backwards compatibility
//! but is deprecated in favor of `#[ron(...)]`.

mod attr;
mod de;
mod schema_codegen;
mod ser;
mod type_mapper;
mod util;

use attr::ContainerAttrs;
use proc_macro::TokenStream;
use quote::quote;
use syn::parse_macro_input;

/// Derive macro for generating RON schemas at compile time.
///
/// # Attributes
///
/// ## Container attributes
///
/// - `#[ron(rename = "Name")]` - Rename the type
/// - `#[ron(rename_all = "camelCase")]` - Rename all fields
///
/// ## Field attributes
///
/// - `#[ron(default)]` - Mark field as optional (has a default value)
/// - `#[ron(flatten)]` - Flatten nested struct fields into the parent
/// - `#[ron(skip)]` - Skip this field in the schema
/// - `#[ron(rename = "name")]` - Rename this field
///
/// # Schema Output
///
/// Schemas are written at compile time when either:
/// - `RON_SCHEMA_DIR` environment variable is set (schemas written to that directory)
/// - `RON_SCHEMA_GLOBAL=1` environment variable is set (schemas written to XDG data dir)
#[proc_macro_derive(RonSchema, attributes(ron, ron_schema))]
pub fn derive_ron_schema(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as syn::DeriveInput);

    // Extract container attributes
    let container_attrs = match ContainerAttrs::from_ast(&input.attrs) {
        Ok(attrs) => attrs,
        Err(err) => return err.to_compile_error().into(),
    };

    // // Compile-time schema generation
    // let schema_dir = std::env::var("RON_SCHEMA_DIR").ok();
    // let schema_global = std::env::var("RON_SCHEMA_GLOBAL")
    //     .map(|v| v == "1")
    //     .unwrap_or(false);

    // if schema_dir.is_some() || schema_global {
    //     if let Some(schema) = schema_build::build_schema(&input, &container_attrs) {
    //         // Ignore errors during compile-time schema writing - don't fail the build
    //         let _ = schema_build::write_schema_at_compile_time(
    //             &input.ident,
    //             &schema,
    //             schema_dir.as_deref(),
    //         );
    //     }
    // }

    // Code generation
    match schema_codegen::impl_ron_schema(&input, &container_attrs) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

/// Derive macro for serializing to RON without serde.
///
/// # Example
///
/// ```ignore
/// use ron2_derive::ToRon;
/// use ron2::ToRon;
///
/// #[derive(ToRon)]
/// struct Point {
///     x: f32,
///     y: f32,
/// }
///
/// let point = Point { x: 1.0, y: 2.0 };
/// let ron = point.to_ron().unwrap();
/// ```
#[proc_macro_derive(ToRon, attributes(ron, ron_schema))]
pub fn derive_to_ron(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as syn::DeriveInput);

    match ser::derive_to_ron(&input) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

/// Derive macro for deserializing from RON without serde.
///
/// # Example
///
/// ```ignore
/// use ron2_derive::FromRon;
/// use ron2::FromRon;
///
/// #[derive(FromRon)]
/// struct Point {
///     x: f32,
///     y: f32,
/// }
///
/// let point: Point = Point::from_ron("(x: 1.0, y: 2.0)").unwrap();
/// ```
#[proc_macro_derive(FromRon, attributes(ron, ron_schema))]
pub fn derive_from_ron(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as syn::DeriveInput);

    match de::derive_from_ron(&input) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

/// Derive macro that implements ToRon, FromRon, and RonSchema.
///
/// This is a convenience macro that combines all three derives.
///
/// # Example
///
/// ```ignore
/// use ron2_derive::Ron;
///
/// #[derive(Debug, PartialEq, Ron)]
/// struct Point {
///     x: f32,
///     y: f32,
/// }
///
/// let point = Point { x: 1.0, y: 2.0 };
/// let ron = point.to_ron().unwrap();
/// let parsed: Point = Point::from_ron(&ron).unwrap();
/// let schema = Point::schema();
/// ```
#[proc_macro_derive(Ron, attributes(ron, ron_schema))]
pub fn derive_ron(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as syn::DeriveInput);

    // Extract container attributes
    let container_attrs = match ContainerAttrs::from_ast(&input.attrs) {
        Ok(attrs) => attrs,
        Err(err) => return err.to_compile_error().into(),
    };

    // // Compile-time schema generation
    // let schema_dir = std::env::var("RON_SCHEMA_DIR").ok();
    // let schema_global = std::env::var("RON_SCHEMA_GLOBAL")
    //     .map(|v| v == "1")
    //     .unwrap_or(false);

    // if schema_dir.is_some() || schema_global {
    //     if let Some(schema) = schema_build::build_schema(&input, &container_attrs) {
    //         let _ = schema_build::write_schema_at_compile_time(
    //             &input.ident,
    //             &schema,
    //             schema_dir.as_deref(),
    //         );
    //     }
    // }

    // Combine all three derives
    let schema_result = schema_codegen::impl_ron_schema(&input, &container_attrs);
    let to_ron_result = ser::derive_to_ron(&input);
    let from_ron_result = de::derive_from_ron(&input);

    match (schema_result, to_ron_result, from_ron_result) {
        (Ok(schema), Ok(to_ron), Ok(from_ron)) => {
            let combined = quote! {
                #schema
                #to_ron
                #from_ron
            };
            combined.into()
        }
        (Err(e), _, _) => e.to_compile_error().into(),
        (_, Err(e), _) => e.to_compile_error().into(),
        (_, _, Err(e)) => e.to_compile_error().into(),
    }
}
