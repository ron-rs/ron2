//! RON Schema derive macro for generating schemas at compile time.
//!
//! This crate provides the `#[derive(RonSchema)]` macro which generates RON schema
//! files at compile time for Rust structs and enums.
//!
//! # Example
//!
//! ```ignore
//! use ron_derive::RonSchema;
//!
//! #[derive(RonSchema)]
//! #[ron_schema(output = "schemas/")]  // Optional: relative to crate root
//! /// Application configuration
//! struct AppConfig {
//!     /// Server port
//!     port: u16,
//!     /// Optional hostname
//!     #[serde(default)]
//!     host: Option<String>,
//! }
//! ```
//!
//! This will generate `schemas/crate_name/AppConfig.schema.ron` at compile time.

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{
    parse_macro_input, Attribute, Data, DeriveInput, Field, Fields, GenericArgument, Ident,
    PathArguments, Type,
};

/// Derive macro for generating RON schemas at compile time.
///
/// # Attributes
///
/// - `#[ron_schema(output = "path/")]` - Set custom output directory relative to crate root
///
/// # Example
///
/// ```ignore
/// #[derive(RonSchema)]
/// /// My struct documentation
/// struct MyStruct {
///     /// Field documentation
///     field: u32,
///     #[serde(default)]
///     optional_field: Option<String>,
/// }
/// ```
#[proc_macro_derive(RonSchema, attributes(ron_schema, serde))]
pub fn derive_ron_schema(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    match impl_ron_schema(&input) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

fn impl_ron_schema(input: &DeriveInput) -> syn::Result<TokenStream2> {
    let name = &input.ident;
    let type_name = name.to_string();

    // Extract doc comment
    let doc = extract_doc_comment(&input.attrs);

    // Extract output path from #[ron_schema(output = "...")] attribute
    let output_path = extract_output_path(&input.attrs)?;

    // Generate the TypeKind based on the data type
    let type_kind_tokens = match &input.data {
        Data::Struct(data_struct) => generate_struct_kind(&data_struct.fields)?,
        Data::Enum(data_enum) => generate_enum_kind(data_enum)?,
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
    let output_dir_tokens = if let Some(path) = output_path {
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

    let expanded = quote! {
        impl #name {
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

        // Implement RonSchema trait
        impl ::ron_schema::RonSchema for #name {
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

/// Extract the doc comment from attributes.
fn extract_doc_comment(attrs: &[Attribute]) -> Option<String> {
    let mut docs = Vec::new();

    for attr in attrs {
        if attr.path().is_ident("doc") {
            if let syn::Meta::NameValue(meta) = &attr.meta {
                if let syn::Expr::Lit(syn::ExprLit {
                    lit: syn::Lit::Str(lit_str),
                    ..
                }) = &meta.value
                {
                    let value = lit_str.value();
                    // Trim leading space that's typically added by rustdoc
                    let trimmed = value.strip_prefix(' ').unwrap_or(&value);
                    docs.push(trimmed.to_string());
                }
            }
        }
    }

    if docs.is_empty() {
        None
    } else {
        Some(docs.join("\n"))
    }
}

/// Extract the output path from #[ron_schema(output = "...")] attribute.
fn extract_output_path(attrs: &[Attribute]) -> syn::Result<Option<String>> {
    for attr in attrs {
        if attr.path().is_ident("ron_schema") {
            let nested = attr.parse_args_with(
                syn::punctuated::Punctuated::<syn::Meta, syn::Token![,]>::parse_terminated,
            )?;

            for meta in nested {
                if let syn::Meta::NameValue(nv) = meta {
                    if nv.path.is_ident("output") {
                        if let syn::Expr::Lit(syn::ExprLit {
                            lit: syn::Lit::Str(lit_str),
                            ..
                        }) = &nv.value
                        {
                            return Ok(Some(lit_str.value()));
                        }
                    }
                }
            }
        }
    }

    Ok(None)
}

/// Check if a field has #[serde(default)] or #[serde(default = "...")] attribute.
fn has_serde_default(attrs: &[Attribute]) -> bool {
    for attr in attrs {
        if attr.path().is_ident("serde") {
            if let Ok(nested) = attr.parse_args_with(
                syn::punctuated::Punctuated::<syn::Meta, syn::Token![,]>::parse_terminated,
            ) {
                for meta in nested {
                    match &meta {
                        syn::Meta::Path(path) if path.is_ident("default") => return true,
                        syn::Meta::NameValue(nv) if nv.path.is_ident("default") => return true,
                        _ => {}
                    }
                }
            }
        }
    }
    false
}

/// Generate TypeKind tokens for a struct's fields.
fn generate_struct_kind(fields: &Fields) -> syn::Result<TokenStream2> {
    match fields {
        Fields::Named(named) => {
            let field_tokens: Vec<TokenStream2> = named
                .named
                .iter()
                .map(|f| generate_field(f))
                .collect::<syn::Result<Vec<_>>>()?;

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
fn generate_enum_kind(data_enum: &syn::DataEnum) -> syn::Result<TokenStream2> {
    let variant_tokens: Vec<TokenStream2> = data_enum
        .variants
        .iter()
        .map(|v| generate_variant(v))
        .collect::<syn::Result<Vec<_>>>()?;

    Ok(quote! {
        ::ron_schema::TypeKind::Enum {
            variants: vec![#(#variant_tokens),*],
        }
    })
}

/// Generate Field tokens for a struct field.
fn generate_field(field: &Field) -> syn::Result<TokenStream2> {
    let name = field
        .ident
        .as_ref()
        .ok_or_else(|| syn::Error::new_spanned(field, "Expected named field"))?
        .to_string();

    let type_kind = type_to_type_kind(&field.ty)?;
    let doc = extract_doc_comment(&field.attrs);
    let optional = has_serde_default(&field.attrs);

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
        }
    })
}

/// Generate Variant tokens for an enum variant.
fn generate_variant(variant: &syn::Variant) -> syn::Result<TokenStream2> {
    let name = variant.ident.to_string();
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
            let field_tokens: Vec<TokenStream2> = named
                .named
                .iter()
                .map(|f| generate_field(f))
                .collect::<syn::Result<Vec<_>>>()?;

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
                        "Vec" if generic_args.len() == 1 => {
                            let inner = type_to_type_kind(generic_args[0])?;
                            return Ok(quote! {
                                ::ron_schema::TypeKind::Vec(Box::new(#inner))
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
            // Treat arrays like Vec for schema purposes
            let inner = type_to_type_kind(&array.elem)?;
            Ok(quote! {
                ::ron_schema::TypeKind::Vec(Box::new(#inner))
            })
        }
        Type::Slice(slice) => {
            // Treat slices like Vec for schema purposes
            let inner = type_to_type_kind(&slice.elem)?;
            Ok(quote! {
                ::ron_schema::TypeKind::Vec(Box::new(#inner))
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
