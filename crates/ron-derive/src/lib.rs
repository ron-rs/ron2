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
//! #[ron(output = "schemas/")]  // Optional: relative to crate root
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
//! - `#[ron(output = "path/")]` - Set schema output directory (RonSchema only)
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

/// Derive macro for generating RON schemas at compile time.
///
/// # Attributes
///
/// ## Container attributes
///
/// - `#[ron(output = "path/")]` - Set custom output directory relative to crate root
/// - `#[ron(rename = "Name")]` - Rename the type
/// - `#[ron(rename_all = "camelCase")]` - Rename all fields
///
/// ## Field attributes
///
/// - `#[ron(default)]` - Mark field as optional (has a default value)
/// - `#[ron(flatten)]` - Flatten nested struct fields into the parent
/// - `#[ron(skip)]` - Skip this field in the schema
/// - `#[ron(rename = "name")]` - Rename this field
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

    // Generate output directory resolution
    let output_dir_tokens = if let Some(ref path) = container_attrs.output {
        quote! {
            // Relative to CARGO_MANIFEST_DIR (crate root)
            let manifest_dir = ::std::env::var("CARGO_MANIFEST_DIR")
                .expect("CARGO_MANIFEST_DIR not set");
            Some(::std::path::PathBuf::from(manifest_dir).join(#path))
        }
    } else {
        quote! {
            // Check RON_SCHEMA_DIR env var, otherwise use None for XDG default
            ::std::env::var("RON_SCHEMA_DIR").ok().map(::std::path::PathBuf::from)
        }
    };

    // Generate a unique function name for this type (using snake_case)
    let snake_name = to_snake_case(&type_name);
    let schema_fn_name = Ident::new(&format!("__ron_schema_{}", snake_name), name.span());
    let write_fn_name = Ident::new(&format!("__ron_schema_write_{}", snake_name), name.span());

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let expanded = quote! {
        impl #impl_generics #name #ty_generics #where_clause {
            /// Returns the RON schema for this type.
            #[doc(hidden)]
            pub fn #schema_fn_name() -> ::ron_schema::Schema {
                #schema_tokens
            }

            /// Writes the RON schema for this type to the configured output directory.
            ///
            /// Returns the path to the written schema file.
            #[doc(hidden)]
            pub fn #write_fn_name() -> ::std::result::Result<::std::path::PathBuf, ::ron_schema::StorageError> {
                let schema = Self::#schema_fn_name();

                // Get the full type path
                let type_path = concat!(module_path!(), "::", #type_name);

                // Resolve output directory
                let output_dir: Option<::std::path::PathBuf> = {
                    #output_dir_tokens
                };

                // Write the schema file
                ::ron_schema::write_schema(
                    type_path,
                    &schema,
                    output_dir.as_deref(),
                )
            }
        }

        // Implement RonSchemaType trait
        impl #impl_generics ::ron_schema::RonSchemaType for #name #ty_generics #where_clause {
            fn type_kind() -> ::ron_schema::TypeKind {
                #type_kind_tokens
            }
        }

        // Implement RonSchema trait
        impl #impl_generics ::ron_schema::RonSchema for #name #ty_generics #where_clause {
            fn schema() -> ::ron_schema::Schema {
                Self::#schema_fn_name()
            }

            fn write_schema() -> ::std::result::Result<::std::path::PathBuf, ::ron_schema::StorageError> {
                Self::#write_fn_name()
            }

            fn type_path() -> &'static str {
                concat!(module_path!(), "::", #type_name)
            }
        }
    };

    Ok(expanded)
}

/// Generate TypeKind tokens for a struct's fields.
fn generate_struct_kind(fields: &Fields, container_attrs: &ContainerAttrs) -> syn::Result<TokenStream2> {
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
                .collect::<syn::Result<Vec<_>>>()?;

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
fn generate_enum_kind(data_enum: &syn::DataEnum, container_attrs: &ContainerAttrs) -> syn::Result<TokenStream2> {
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
    let type_kind = type_to_type_kind(&field.ty)?;
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
fn generate_variant(variant: &syn::Variant, container_attrs: &ContainerAttrs) -> syn::Result<TokenStream2> {
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
                .collect::<syn::Result<Vec<_>>>()?;

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

/// Convert a Rust type to TypeKind tokens.
fn type_to_type_kind(ty: &Type) -> syn::Result<TokenStream2> {
    match ty {
        Type::Path(type_path) => {
            let path = &type_path.path;

            // Get the last segment (the actual type name)
            if let Some(segment) = path.segments.last() {
                let ident = &segment.ident;
                let ident_str = ident.to_string();

                // Check for primitives
                match ident_str.as_str() {
                    "bool" => return Ok(quote! { ::ron_schema::TypeKind::Bool }),
                    "i8" => return Ok(quote! { ::ron_schema::TypeKind::I8 }),
                    "i16" => return Ok(quote! { ::ron_schema::TypeKind::I16 }),
                    "i32" => return Ok(quote! { ::ron_schema::TypeKind::I32 }),
                    "i64" => return Ok(quote! { ::ron_schema::TypeKind::I64 }),
                    "i128" => return Ok(quote! { ::ron_schema::TypeKind::I128 }),
                    "u8" => return Ok(quote! { ::ron_schema::TypeKind::U8 }),
                    "u16" => return Ok(quote! { ::ron_schema::TypeKind::U16 }),
                    "u32" => return Ok(quote! { ::ron_schema::TypeKind::U32 }),
                    "u64" => return Ok(quote! { ::ron_schema::TypeKind::U64 }),
                    "u128" => return Ok(quote! { ::ron_schema::TypeKind::U128 }),
                    "f32" => return Ok(quote! { ::ron_schema::TypeKind::F32 }),
                    "f64" => return Ok(quote! { ::ron_schema::TypeKind::F64 }),
                    "char" => return Ok(quote! { ::ron_schema::TypeKind::Char }),
                    "String" | "str" => return Ok(quote! { ::ron_schema::TypeKind::String }),
                    _ => {}
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
                            let inner = type_to_type_kind(generic_args[0])?;
                            return Ok(quote! {
                                ::ron_schema::TypeKind::Option(Box::new(#inner))
                            });
                        }
                        // List-like types: Vec, VecDeque, HashSet, BTreeSet, LinkedList
                        "Vec" | "VecDeque" | "HashSet" | "BTreeSet" | "LinkedList"
                            if generic_args.len() == 1 =>
                        {
                            let inner = type_to_type_kind(generic_args[0])?;
                            return Ok(quote! {
                                ::ron_schema::TypeKind::List(Box::new(#inner))
                            });
                        }
                        "HashMap" | "BTreeMap" if generic_args.len() == 2 => {
                            let key = type_to_type_kind(generic_args[0])?;
                            let value = type_to_type_kind(generic_args[1])?;
                            return Ok(quote! {
                                ::ron_schema::TypeKind::Map {
                                    key: Box::new(#key),
                                    value: Box::new(#value),
                                }
                            });
                        }
                        "Box" if generic_args.len() == 1 => {
                            // Box<T> is treated as just T for schema purposes
                            return type_to_type_kind(generic_args[0]);
                        }
                        _ => {}
                    }
                }

                // For any other type, generate a TypeRef with the full path
                let type_path_str = path_to_string(path);
                return Ok(quote! {
                    ::ron_schema::TypeKind::TypeRef(#type_path_str.to_string())
                });
            }

            Err(syn::Error::new_spanned(ty, "Unsupported type"))
        }
        Type::Tuple(tuple) => {
            if tuple.elems.is_empty() {
                // Unit type ()
                return Ok(quote! { ::ron_schema::TypeKind::Unit });
            }

            let elem_tokens: Vec<TokenStream2> = tuple
                .elems
                .iter()
                .map(|t| type_to_type_kind(t))
                .collect::<syn::Result<Vec<_>>>()?;

            Ok(quote! {
                ::ron_schema::TypeKind::Tuple(vec![#(#elem_tokens),*])
            })
        }
        Type::Reference(reference) => {
            // For references, use the underlying type
            type_to_type_kind(&reference.elem)
        }
        Type::Array(array) => {
            // Treat arrays as List for schema purposes
            let inner = type_to_type_kind(&array.elem)?;
            Ok(quote! {
                ::ron_schema::TypeKind::List(Box::new(#inner))
            })
        }
        Type::Slice(slice) => {
            // Treat slices as List for schema purposes
            let inner = type_to_type_kind(&slice.elem)?;
            Ok(quote! {
                ::ron_schema::TypeKind::List(Box::new(#inner))
            })
        }
        _ => {
            // For other types, generate a TypeRef with the type as-is
            let type_str = quote!(#ty).to_string();
            Ok(quote! {
                ::ron_schema::TypeKind::TypeRef(#type_str.to_string())
            })
        }
    }
}

/// Convert a syn::Path to a string representation.
fn path_to_string(path: &syn::Path) -> String {
    path.segments
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
        .join("::")
}

/// Convert a PascalCase string to snake_case.
fn to_snake_case(s: &str) -> String {
    let mut result = String::new();
    for (i, c) in s.chars().enumerate() {
        if c.is_uppercase() {
            if i > 0 {
                result.push('_');
            }
            result.push(c.to_lowercase().next().unwrap());
        } else {
            result.push(c);
        }
    }
    result
}
