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
//! use ron_derive::Ron;
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
//!
//! # Variant Attributes
//!
//! - `#[ron(rename = "Name")]` - Rename this variant
//! - `#[ron(skip)]` - Skip this variant
//!
//! # Legacy Support
//!
//! The `#[ron_schema(...)]` attribute is still supported for backwards compatibility
//! but is deprecated in favor of `#[ron(...)]`.

mod attr;
mod de;
mod ser;

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{
    parse_macro_input, Data, DeriveInput, Fields, GenericArgument, Ident, PathArguments, Type,
};

use attr::{extract_doc_comment, ContainerAttrs, FieldAttrs};

// =============================================================================
// Type mapping abstraction
// =============================================================================
//
// These types allow sharing the logic for converting Rust types to TypeKind
// between code generation (TokenStream2) and compile-time building (TypeKind).

/// Primitive types that map directly to TypeKind variants.
#[derive(Debug, Clone, Copy)]
enum PrimitiveKind {
    Bool,
    I8,
    I16,
    I32,
    I64,
    I128,
    U8,
    U16,
    U32,
    U64,
    U128,
    F32,
    F64,
    Char,
    String,
}

impl PrimitiveKind {
    /// Try to parse a primitive kind from a type identifier.
    fn from_ident(ident: &str) -> Option<Self> {
        match ident {
            "bool" => Some(Self::Bool),
            "i8" => Some(Self::I8),
            "i16" => Some(Self::I16),
            "i32" => Some(Self::I32),
            "i64" => Some(Self::I64),
            "i128" => Some(Self::I128),
            "u8" => Some(Self::U8),
            "u16" => Some(Self::U16),
            "u32" => Some(Self::U32),
            "u64" => Some(Self::U64),
            "u128" => Some(Self::U128),
            "f32" => Some(Self::F32),
            "f64" => Some(Self::F64),
            "char" => Some(Self::Char),
            "String" | "str" => Some(Self::String),
            _ => None,
        }
    }
}

/// Trait for mapping Rust types to TypeKind representations.
///
/// This abstraction allows sharing the type analysis logic between:
/// - Code generation (returns TokenStream2 that constructs TypeKind at runtime)
/// - Compile-time building (returns TypeKind values directly)
trait TypeKindMapper {
    type Output;

    fn unit(&self) -> Self::Output;
    fn primitive(&self, kind: PrimitiveKind) -> Self::Output;
    fn option(&self, inner: Self::Output) -> Self::Output;
    fn list(&self, inner: Self::Output) -> Self::Output;
    fn map(&self, key: Self::Output, value: Self::Output) -> Self::Output;
    fn tuple(&self, elements: Vec<Self::Output>) -> Self::Output;
    fn type_ref(&self, path: String) -> Self::Output;
}

/// Mapper that generates TokenStream2 for runtime TypeKind construction.
struct TokenMapper;

impl TypeKindMapper for TokenMapper {
    type Output = TokenStream2;

    fn unit(&self) -> Self::Output {
        quote! { ::ron_schema::TypeKind::Unit }
    }

    fn primitive(&self, kind: PrimitiveKind) -> Self::Output {
        match kind {
            PrimitiveKind::Bool => quote! { ::ron_schema::TypeKind::Bool },
            PrimitiveKind::I8 => quote! { ::ron_schema::TypeKind::I8 },
            PrimitiveKind::I16 => quote! { ::ron_schema::TypeKind::I16 },
            PrimitiveKind::I32 => quote! { ::ron_schema::TypeKind::I32 },
            PrimitiveKind::I64 => quote! { ::ron_schema::TypeKind::I64 },
            PrimitiveKind::I128 => quote! { ::ron_schema::TypeKind::I128 },
            PrimitiveKind::U8 => quote! { ::ron_schema::TypeKind::U8 },
            PrimitiveKind::U16 => quote! { ::ron_schema::TypeKind::U16 },
            PrimitiveKind::U32 => quote! { ::ron_schema::TypeKind::U32 },
            PrimitiveKind::U64 => quote! { ::ron_schema::TypeKind::U64 },
            PrimitiveKind::U128 => quote! { ::ron_schema::TypeKind::U128 },
            PrimitiveKind::F32 => quote! { ::ron_schema::TypeKind::F32 },
            PrimitiveKind::F64 => quote! { ::ron_schema::TypeKind::F64 },
            PrimitiveKind::Char => quote! { ::ron_schema::TypeKind::Char },
            PrimitiveKind::String => quote! { ::ron_schema::TypeKind::String },
        }
    }

    fn option(&self, inner: Self::Output) -> Self::Output {
        quote! { ::ron_schema::TypeKind::Option(Box::new(#inner)) }
    }

    fn list(&self, inner: Self::Output) -> Self::Output {
        quote! { ::ron_schema::TypeKind::List(Box::new(#inner)) }
    }

    fn map(&self, key: Self::Output, value: Self::Output) -> Self::Output {
        quote! {
            ::ron_schema::TypeKind::Map {
                key: Box::new(#key),
                value: Box::new(#value),
            }
        }
    }

    fn tuple(&self, elements: Vec<Self::Output>) -> Self::Output {
        quote! { ::ron_schema::TypeKind::Tuple(vec![#(#elements),*]) }
    }

    fn type_ref(&self, path: String) -> Self::Output {
        quote! { ::ron_schema::TypeKind::TypeRef(#path.to_string()) }
    }
}

/// Mapper that builds TypeKind values directly at compile time.
struct ValueMapper;

impl TypeKindMapper for ValueMapper {
    type Output = ron_schema::TypeKind;

    fn unit(&self) -> Self::Output {
        ron_schema::TypeKind::Unit
    }

    fn primitive(&self, kind: PrimitiveKind) -> Self::Output {
        match kind {
            PrimitiveKind::Bool => ron_schema::TypeKind::Bool,
            PrimitiveKind::I8 => ron_schema::TypeKind::I8,
            PrimitiveKind::I16 => ron_schema::TypeKind::I16,
            PrimitiveKind::I32 => ron_schema::TypeKind::I32,
            PrimitiveKind::I64 => ron_schema::TypeKind::I64,
            PrimitiveKind::I128 => ron_schema::TypeKind::I128,
            PrimitiveKind::U8 => ron_schema::TypeKind::U8,
            PrimitiveKind::U16 => ron_schema::TypeKind::U16,
            PrimitiveKind::U32 => ron_schema::TypeKind::U32,
            PrimitiveKind::U64 => ron_schema::TypeKind::U64,
            PrimitiveKind::U128 => ron_schema::TypeKind::U128,
            PrimitiveKind::F32 => ron_schema::TypeKind::F32,
            PrimitiveKind::F64 => ron_schema::TypeKind::F64,
            PrimitiveKind::Char => ron_schema::TypeKind::Char,
            PrimitiveKind::String => ron_schema::TypeKind::String,
        }
    }

    fn option(&self, inner: Self::Output) -> Self::Output {
        ron_schema::TypeKind::Option(Box::new(inner))
    }

    fn list(&self, inner: Self::Output) -> Self::Output {
        ron_schema::TypeKind::List(Box::new(inner))
    }

    fn map(&self, key: Self::Output, value: Self::Output) -> Self::Output {
        ron_schema::TypeKind::Map {
            key: Box::new(key),
            value: Box::new(value),
        }
    }

    fn tuple(&self, elements: Vec<Self::Output>) -> Self::Output {
        ron_schema::TypeKind::Tuple(elements)
    }

    fn type_ref(&self, path: String) -> Self::Output {
        ron_schema::TypeKind::TypeRef(path)
    }
}

/// Map a Rust type to its TypeKind representation using the given mapper.
fn map_type<M: TypeKindMapper>(ty: &Type, mapper: &M) -> M::Output {
    match ty {
        Type::Path(type_path) => {
            let path = &type_path.path;

            if let Some(segment) = path.segments.last() {
                let ident_str = segment.ident.to_string();

                // Check for primitives
                if let Some(prim) = PrimitiveKind::from_ident(&ident_str) {
                    return mapper.primitive(prim);
                }

                // Check for generic types (Option, Vec, HashMap, etc.)
                if let PathArguments::AngleBracketed(args) = &segment.arguments {
                    let generic_args: Vec<_> = args
                        .args
                        .iter()
                        .filter_map(|arg| {
                            if let GenericArgument::Type(t) = arg {
                                Some(t)
                            } else {
                                None
                            }
                        })
                        .collect();

                    match ident_str.as_str() {
                        "Option" if generic_args.len() == 1 => {
                            let inner = map_type(generic_args[0], mapper);
                            return mapper.option(inner);
                        }
                        "Vec" | "VecDeque" | "HashSet" | "BTreeSet" | "LinkedList"
                            if generic_args.len() == 1 =>
                        {
                            let inner = map_type(generic_args[0], mapper);
                            return mapper.list(inner);
                        }
                        "HashMap" | "BTreeMap" if generic_args.len() == 2 => {
                            let key = map_type(generic_args[0], mapper);
                            let value = map_type(generic_args[1], mapper);
                            return mapper.map(key, value);
                        }
                        "Box" if generic_args.len() == 1 => {
                            // Box<T> is treated as just T for schema purposes
                            return map_type(generic_args[0], mapper);
                        }
                        _ => {}
                    }
                }

                // For any other type, generate a TypeRef with the full path
                let type_path_str = path_to_string(path);
                return mapper.type_ref(type_path_str);
            }

            // Fallback for invalid paths (shouldn't happen in practice)
            mapper.type_ref(quote!(#ty).to_string())
        }
        Type::Tuple(tuple) => {
            if tuple.elems.is_empty() {
                return mapper.unit();
            }

            let elements: Vec<_> = tuple.elems.iter().map(|t| map_type(t, mapper)).collect();
            mapper.tuple(elements)
        }
        Type::Reference(reference) => {
            // For references, use the underlying type
            map_type(&reference.elem, mapper)
        }
        Type::Array(array) => {
            // Treat arrays as List for schema purposes
            let inner = map_type(&array.elem, mapper);
            mapper.list(inner)
        }
        Type::Slice(slice) => {
            // Treat slices as List for schema purposes
            let inner = map_type(&slice.elem, mapper);
            mapper.list(inner)
        }
        _ => {
            // For other types, generate a TypeRef with the type as-is
            let type_str = quote!(#ty).to_string();
            mapper.type_ref(type_str)
        }
    }
}

/// Convert a Rust type to TypeKind tokens (for code generation).
fn type_to_type_kind(ty: &Type) -> TokenStream2 {
    map_type(ty, &TokenMapper)
}

/// Convert a Rust type to a TypeKind value (for compile-time building).
fn rust_type_to_type_kind(ty: &Type) -> ron_schema::TypeKind {
    map_type(ty, &ValueMapper)
}

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
    let input = parse_macro_input!(input as DeriveInput);

    match impl_ron_schema(&input) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

/// Derive macro for serializing to RON without serde.
///
/// # Example
///
/// ```ignore
/// use ron_derive::ToRon;
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
    let input = parse_macro_input!(input as DeriveInput);

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
/// use ron_derive::FromRon;
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
    let input = parse_macro_input!(input as DeriveInput);

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
/// use ron_derive::Ron;
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
    let input = parse_macro_input!(input as DeriveInput);

    // Combine all three derives
    let schema_result = impl_ron_schema(&input);
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

fn impl_ron_schema(input: &DeriveInput) -> syn::Result<TokenStream2> {
    let name = &input.ident;
    let type_name = name.to_string();

    // Extract doc comment
    let doc = extract_doc_comment(&input.attrs);

    // Extract container attributes
    let container_attrs = ContainerAttrs::from_ast(&input.attrs)?;

    // ==========================================================================
    // Compile-time schema generation
    // ==========================================================================
    // Check if compile-time schema generation is enabled via environment variables
    let schema_dir = std::env::var("RON_SCHEMA_DIR").ok();
    let schema_global = std::env::var("RON_SCHEMA_GLOBAL")
        .map(|v| v == "1")
        .unwrap_or(false);

    if schema_dir.is_some() || schema_global {
        if let Some(schema) = build_schema(input, &container_attrs) {
            // Ignore errors during compile-time schema writing - don't fail the build
            let _ = write_schema_at_compile_time(name, &schema, schema_dir.as_deref());
        }
    }

    // ==========================================================================
    // Code generation (as before)
    // ==========================================================================

    // Generate the TypeKind based on the data type
    let type_kind_tokens = match &input.data {
        Data::Struct(data_struct) => generate_struct_kind(&data_struct.fields, &container_attrs)?,
        Data::Enum(data_enum) => generate_enum_kind(data_enum, &container_attrs)?,
        Data::Union(_) => {
            return Err(syn::Error::new_spanned(
                input,
                "RonSchema cannot be derived for unions",
            ));
        }
    };

    // Generate the schema construction code
    let schema_tokens = if let Some(doc_str) = doc {
        quote! {
            ::ron_schema::Schema::with_doc(#doc_str, #type_kind_tokens)
        }
    } else {
        quote! {
            ::ron_schema::Schema::new(#type_kind_tokens)
        }
    };

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let expanded = quote! {
        impl #impl_generics ::ron_schema::RonSchemaType for #name #ty_generics #where_clause {
            fn type_kind() -> ::ron_schema::TypeKind {
                #type_kind_tokens
            }

            fn schema() -> ::ron_schema::Schema {
                #schema_tokens
            }

            fn type_path() -> Option<&'static str> {
                Some(concat!(module_path!(), "::", #type_name))
            }
        }
    };

    Ok(expanded)
}

/// Generate TypeKind tokens for a struct's fields.
fn generate_struct_kind(
    fields: &Fields,
    container_attrs: &ContainerAttrs,
) -> syn::Result<TokenStream2> {
    match fields {
        Fields::Named(named) => {
            let mut field_tokens: Vec<TokenStream2> = Vec::new();

            for f in named.named.iter() {
                let attrs = FieldAttrs::from_ast(&f.attrs)?;

                // Skip fields marked with #[ron(skip)]
                if attrs.skip {
                    continue;
                }

                field_tokens.push(generate_field(f, &attrs, container_attrs)?);
            }

            Ok(quote! {
                ::ron_schema::TypeKind::Struct {
                    fields: vec![#(#field_tokens),*],
                }
            })
        }
        Fields::Unnamed(unnamed) => {
            // Tuple struct -> TypeKind::Tuple
            let type_tokens: Vec<TokenStream2> = unnamed
                .unnamed
                .iter()
                .map(|f| type_to_type_kind(&f.ty))
                .collect();

            Ok(quote! {
                ::ron_schema::TypeKind::Tuple(vec![#(#type_tokens),*])
            })
        }
        Fields::Unit => {
            // Unit struct -> empty struct
            Ok(quote! {
                ::ron_schema::TypeKind::Unit
            })
        }
    }
}

/// Generate TypeKind tokens for an enum.
fn generate_enum_kind(
    data_enum: &syn::DataEnum,
    container_attrs: &ContainerAttrs,
) -> syn::Result<TokenStream2> {
    let variant_tokens: Vec<TokenStream2> = data_enum
        .variants
        .iter()
        .map(|v| generate_variant(v, container_attrs))
        .collect::<syn::Result<Vec<_>>>()?;

    Ok(quote! {
        ::ron_schema::TypeKind::Enum {
            variants: vec![#(#variant_tokens),*],
        }
    })
}

/// Generate Field tokens for a struct field.
fn generate_field(
    field: &syn::Field,
    attrs: &FieldAttrs,
    container_attrs: &ContainerAttrs,
) -> syn::Result<TokenStream2> {
    let original_name = field
        .ident
        .as_ref()
        .ok_or_else(|| syn::Error::new_spanned(field, "Expected named field"))?
        .to_string();

    let name = attrs.effective_name(&original_name, container_attrs);
    let type_kind = type_to_type_kind(&field.ty);
    let doc = extract_doc_comment(&field.attrs);
    let optional = attrs.has_default();
    let flattened = attrs.flatten;

    let doc_tokens = match doc {
        Some(d) => quote! { Some(#d.to_string()) },
        None => quote! { None },
    };

    Ok(quote! {
        ::ron_schema::Field {
            name: #name.to_string(),
            ty: #type_kind,
            doc: #doc_tokens,
            optional: #optional,
            flattened: #flattened,
        }
    })
}

/// Generate Variant tokens for an enum variant.
fn generate_variant(
    variant: &syn::Variant,
    container_attrs: &ContainerAttrs,
) -> syn::Result<TokenStream2> {
    let variant_attrs = attr::VariantAttrs::from_ast(&variant.attrs)?;
    let original_name = variant.ident.to_string();
    let name = variant_attrs.effective_name(&original_name, container_attrs);
    let doc = extract_doc_comment(&variant.attrs);

    let doc_tokens = match doc {
        Some(d) => quote! { Some(#d.to_string()) },
        None => quote! { None },
    };

    let kind_tokens = match &variant.fields {
        Fields::Unit => quote! { ::ron_schema::VariantKind::Unit },
        Fields::Unnamed(unnamed) => {
            let type_tokens: Vec<TokenStream2> = unnamed
                .unnamed
                .iter()
                .map(|f| type_to_type_kind(&f.ty))
                .collect();

            quote! {
                ::ron_schema::VariantKind::Tuple(vec![#(#type_tokens),*])
            }
        }
        Fields::Named(named) => {
            let mut field_tokens: Vec<TokenStream2> = Vec::new();

            for f in named.named.iter() {
                let attrs = FieldAttrs::from_ast(&f.attrs)?;

                // Skip fields marked with #[ron(skip)]
                if attrs.skip {
                    continue;
                }

                field_tokens.push(generate_field(f, &attrs, container_attrs)?);
            }

            quote! {
                ::ron_schema::VariantKind::Struct(vec![#(#field_tokens),*])
            }
        }
    };

    Ok(quote! {
        ::ron_schema::Variant {
            name: #name.to_string(),
            doc: #doc_tokens,
            kind: #kind_tokens,
        }
    })
}

/// Convert a syn::Path to a string representation.
///
/// Single-segment paths (local types) are prefixed with the crate name to produce
/// fully qualified paths like `my_crate::MyType`.
fn path_to_string(path: &syn::Path) -> String {
    let raw_path = path
        .segments
        .iter()
        .map(|seg| {
            let ident = seg.ident.to_string();
            match &seg.arguments {
                PathArguments::None => ident,
                PathArguments::AngleBracketed(args) => {
                    let args_str: Vec<String> = args
                        .args
                        .iter()
                        .filter_map(|arg| {
                            if let GenericArgument::Type(t) = arg {
                                Some(quote!(#t).to_string())
                            } else {
                                None
                            }
                        })
                        .collect();
                    if args_str.is_empty() {
                        ident
                    } else {
                        format!("{}<{}>", ident, args_str.join(", "))
                    }
                }
                PathArguments::Parenthesized(_) => ident,
            }
        })
        .collect::<Vec<_>>()
        .join("::");

    // Qualify single-segment paths (local types) with crate name
    if path.segments.len() == 1 {
        let crate_name = get_crate_name();
        format!("{}::{}", crate_name, raw_path)
    } else {
        raw_path
    }
}


/// Get the crate name, normalized for Rust path syntax (hyphens replaced with underscores).
fn get_crate_name() -> String {
    std::env::var("CARGO_CRATE_NAME")
        .or_else(|_| std::env::var("CARGO_PKG_NAME").map(|s| s.replace('-', "_")))
        .unwrap_or_else(|_| "unknown_crate".to_string())
}

// =============================================================================
// Compile-time schema building functions
// =============================================================================
//
// These functions build actual `ron_schema` values (not tokens) for use during
// proc macro execution. This allows writing schema files at compile time.

use ron_schema::{Field, Schema, TypeKind, Variant, VariantKind};

/// Build a Schema value at compile time.
fn build_schema(input: &DeriveInput, container_attrs: &ContainerAttrs) -> Option<Schema> {
    let doc = extract_doc_comment(&input.attrs);

    let type_kind = match &input.data {
        Data::Struct(data_struct) => {
            build_struct_kind(&data_struct.fields, container_attrs).ok()?
        }
        Data::Enum(data_enum) => build_enum_kind(data_enum, container_attrs).ok()?,
        Data::Union(_) => return None,
    };

    Some(if let Some(doc_str) = doc {
        Schema::with_doc(doc_str, type_kind)
    } else {
        Schema::new(type_kind)
    })
}

/// Build TypeKind for a struct's fields.
fn build_struct_kind(fields: &Fields, container_attrs: &ContainerAttrs) -> syn::Result<TypeKind> {
    match fields {
        Fields::Named(named) => {
            let mut field_values: Vec<Field> = Vec::new();

            for f in named.named.iter() {
                let attrs = FieldAttrs::from_ast(&f.attrs)?;
                if attrs.skip {
                    continue;
                }
                field_values.push(build_field(f, &attrs, container_attrs)?);
            }

            Ok(TypeKind::Struct {
                fields: field_values,
            })
        }
        Fields::Unnamed(unnamed) => {
            let type_kinds: Vec<TypeKind> = unnamed
                .unnamed
                .iter()
                .map(|f| rust_type_to_type_kind(&f.ty))
                .collect();

            Ok(TypeKind::Tuple(type_kinds))
        }
        Fields::Unit => Ok(TypeKind::Unit),
    }
}

/// Build TypeKind for an enum.
fn build_enum_kind(
    data_enum: &syn::DataEnum,
    container_attrs: &ContainerAttrs,
) -> syn::Result<TypeKind> {
    let variants: Vec<Variant> = data_enum
        .variants
        .iter()
        .map(|v| build_variant(v, container_attrs))
        .collect::<syn::Result<Vec<_>>>()?;

    Ok(TypeKind::Enum { variants })
}

/// Build a Field value for a struct field.
fn build_field(
    field: &syn::Field,
    attrs: &FieldAttrs,
    container_attrs: &ContainerAttrs,
) -> syn::Result<Field> {
    let original_name = field
        .ident
        .as_ref()
        .ok_or_else(|| syn::Error::new_spanned(field, "Expected named field"))?
        .to_string();

    let name = attrs.effective_name(&original_name, container_attrs);
    let ty = rust_type_to_type_kind(&field.ty);
    let doc = extract_doc_comment(&field.attrs);
    let optional = attrs.has_default();
    let flattened = attrs.flatten;

    Ok(Field {
        name,
        ty,
        doc,
        optional,
        flattened,
    })
}

/// Build a Variant value for an enum variant.
fn build_variant(variant: &syn::Variant, container_attrs: &ContainerAttrs) -> syn::Result<Variant> {
    let variant_attrs = attr::VariantAttrs::from_ast(&variant.attrs)?;
    let original_name = variant.ident.to_string();
    let name = variant_attrs.effective_name(&original_name, container_attrs);
    let doc = extract_doc_comment(&variant.attrs);

    let kind = match &variant.fields {
        Fields::Unit => VariantKind::Unit,
        Fields::Unnamed(unnamed) => {
            let type_kinds: Vec<TypeKind> = unnamed
                .unnamed
                .iter()
                .map(|f| rust_type_to_type_kind(&f.ty))
                .collect();
            VariantKind::Tuple(type_kinds)
        }
        Fields::Named(named) => {
            let mut field_values: Vec<Field> = Vec::new();
            for f in named.named.iter() {
                let attrs = FieldAttrs::from_ast(&f.attrs)?;
                if attrs.skip {
                    continue;
                }
                field_values.push(build_field(f, &attrs, container_attrs)?);
            }
            VariantKind::Struct(field_values)
        }
    };

    Ok(Variant { name, doc, kind })
}

/// Write a schema file at compile time (during proc macro execution).
fn write_schema_at_compile_time(
    type_name: &Ident,
    schema: &Schema,
    env_schema_dir: Option<&str>,
) -> Result<std::path::PathBuf, Box<dyn std::error::Error>> {
    use ron2::ser::PrettyConfig;
    use ron_schema::ToRon;
    use std::path::PathBuf;

    // Resolve output directory (env var or XDG default)
    let output_dir: PathBuf = if let Some(dir) = env_schema_dir {
        PathBuf::from(dir)
    } else {
        // RON_SCHEMA_GLOBAL=1 but no dir specified - use XDG default
        dirs::data_dir()
            .ok_or("No data directory found")?
            .join("ron-schemas")
    };

    // Build type path: crate_name::TypeName (normalized, hyphens -> underscores)
    let crate_name = get_crate_name();
    let type_path = format!("{}::{}", crate_name, type_name);

    // Convert to file path
    let file_path = output_dir.join(ron_schema::type_path_to_file_path(&type_path));

    // Serialize
    let value = schema.to_ron_value()?;
    let config = PrettyConfig::default();
    let contents = ron2::ser::to_string_pretty(&value, config)?;

    // Write
    if let Some(parent) = file_path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    std::fs::write(&file_path, contents)?;

    Ok(file_path)
}
