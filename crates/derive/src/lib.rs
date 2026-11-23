//! RON derive macros for serialization, deserialization, and schema generation.
//!
//! - `#[derive(Ron)]` - Implements `ToRon`, `FromRon`, and `RonSchema`
//! - `#[derive(ToRon)]` - Serialize to RON
//! - `#[derive(FromRon)]` - Deserialize from RON
//! - `#[derive(RonSchema)]` - Describe the type for validation and the LSP
//!
//! All derives share the `#[ron(...)]` attribute namespace. Unknown or
//! conflicting attributes are compile errors.
//!
//! # Example
//!
//! ```
//! use ron2::{FromRon, Ron};
//!
//! /// Application configuration
//! #[derive(Debug, PartialEq, Ron)]
//! struct AppConfig {
//!     /// Server port
//!     port: u16,
//!     /// Optional hostname
//!     #[ron(default)]
//!     host: Option<String>,
//! }
//!
//! let config = AppConfig::from_ron("(port: 8080)")?;
//! assert_eq!(config, AppConfig { port: 8080, host: None });
//! # Ok::<(), ron2::Error>(())
//! ```
//!
//! # Container Attributes
//!
//! - `#[ron(rename = "Name")]` - Struct name in RON (structs only)
//! - `#[ron(rename_all = "camelCase")]` - Rename the fields of a struct, or
//!   the variants of an enum (camelCase, snake_case, PascalCase,
//!   SCREAMING_SNAKE_CASE, lowercase, UPPERCASE)
//! - `#[ron(deny_unknown_fields)]` - Error on unknown fields during deserialization
//! - `#[ron(transparent)]` - Serialize/deserialize as the single non-skipped field
//! - `#[ron(bound = "T: Trait, ...")]` - Replace the inferred where-clause
//!   predicates (by default every type parameter used by a field gets the
//!   derived trait as bound)
//!
//! # Variant Attributes
//!
//! - `#[ron(rename = "Name")]` - Rename this variant
//! - `#[ron(rename_all = "camelCase")]` - Rename the fields of this struct variant
//! - `#[ron(skip)]` - Never deserialize this variant; serializing it is an error
//!
//! # Field Attributes (named fields only)
//!
//! - `#[ron(rename = "name")]` - Rename this field
//! - `#[ron(skip)]` - Skip this field entirely (deserializes to its default)
//! - `#[ron(skip_serializing)]` - Skip during serialization only
//! - `#[ron(skip_deserializing)]` - Skip during deserialization (use default)
//! - `#[ron(default)]` - Use `Default::default()` if missing
//! - `#[ron(default = "path::to::fn")]` - Use a custom function if missing (or skipped)
//! - `#[ron(flatten)]` - Merge the fields of a nested struct into this one
//! - `#[ron(skip_serializing_if = "path::to::fn")]` - Skip if the predicate returns true
//! - `#[ron(explicit)]` - Require explicit `Some(...)` or `None` for an `Option` field
//! - `#[ron(opt)]` - `default`, and skip serialization when equal to the default
//!
//! # Extension Behavior
//!
//! `Option<T>` fields accept bare values (`name: "Alice"` means
//! `Some("Alice")`) unless marked `#[ron(explicit)]`. Transparent newtypes
//! (`#[ron(transparent)] struct UserId(u64);`) serialize as their inner value.

mod ast;
mod attr;
mod bound;
mod de;
mod schema;
mod ser;

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use syn::parse_macro_input;

use crate::ast::Container;

fn expand(
    input: TokenStream,
    generate: impl FnOnce(&Container<'_>) -> TokenStream2,
) -> TokenStream {
    let input = parse_macro_input!(input as syn::DeriveInput);
    match Container::from_ast(&input) {
        Ok(cont) => generate(&cont).into(),
        Err(err) => err.to_compile_error().into(),
    }
}

/// Derive macro for describing a type in the RON schema system.
///
/// Schemas are written at runtime with `write_schemas`:
///
/// ```no_run
/// use ron2::{RonSchema, schema::RonSchema as _};
///
/// #[derive(RonSchema)]
/// struct Config {
///     port: u16,
/// }
///
/// // Write to a specific directory
/// Config::write_schemas(Some("./schemas"))?;
///
/// // Or to $RON_SCHEMA_DIR, falling back to the XDG data directory
/// // (~/.local/share/ron-schemas/, requires the `xdg` feature of ron2)
/// Config::write_schemas(None)?;
/// # Ok::<(), ron2::schema::SchemaError>(())
/// ```
///
/// Schema files are named by their fully-qualified type path:
/// `my_crate::config::Config` → `my_crate/config/Config.schema.ron`.
/// Each instantiation of a generic type has its own schema, named like
/// `core::any::type_name` (`my_crate::Wrapper<u8>`).
#[proc_macro_derive(RonSchema, attributes(ron))]
pub fn derive_ron_schema(input: TokenStream) -> TokenStream {
    expand(input, schema::expand)
}

/// Derive macro for serializing to RON.
///
/// ```
/// use ron2::ToRon;
///
/// #[derive(ToRon)]
/// struct Point {
///     x: f32,
///     y: f32,
/// }
///
/// let ron = Point { x: 1.0, y: 2.0 }.to_ron_with(&ron2::fmt::FormatConfig::minimal())?;
/// assert_eq!(ron, "Point(x:1.0,y:2.0)");
/// # Ok::<(), ron2::Error>(())
/// ```
#[proc_macro_derive(ToRon, attributes(ron))]
pub fn derive_to_ron(input: TokenStream) -> TokenStream {
    expand(input, ser::expand)
}

/// Derive macro for deserializing from RON.
///
/// ```
/// use ron2::FromRon;
///
/// #[derive(FromRon)]
/// struct Point {
///     x: f32,
///     y: f32,
/// }
///
/// let point = Point::from_ron("(x: 1.0, y: 2.0)")?;
/// assert_eq!(point.y, 2.0);
/// # Ok::<(), ron2::Error>(())
/// ```
#[proc_macro_derive(FromRon, attributes(ron))]
pub fn derive_from_ron(input: TokenStream) -> TokenStream {
    expand(input, de::expand)
}

/// Derive macro that implements `ToRon`, `FromRon`, and `RonSchema`.
#[proc_macro_derive(Ron, attributes(ron))]
pub fn derive_ron(input: TokenStream) -> TokenStream {
    expand(input, |cont| {
        let mut tokens = schema::expand(cont);
        tokens.extend(ser::expand(cont));
        tokens.extend(de::expand(cont));
        tokens
    })
}
