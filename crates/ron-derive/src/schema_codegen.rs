//! Schema code generation - generates TokenStream2 for runtime TypeKind construction.

use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{DeriveInput, Fields};

use crate::attr::{extract_doc_comment, ContainerAttrs, FieldAttrs, VariantAttrs};
use crate::type_mapper::type_to_type_kind;

/// Generate the RonSchemaType implementation for a type.
pub fn impl_ron_schema(input: &DeriveInput, container_attrs: &ContainerAttrs) -> syn::Result<TokenStream2> {
    let name = &input.ident;
    let type_name = name.to_string();

    // Extract doc comment
    let doc = extract_doc_comment(&input.attrs);

    // Generate the TypeKind based on the data type
    let type_kind_tokens = match &input.data {
        syn::Data::Struct(data_struct) => generate_struct_kind(&data_struct.fields, container_attrs)?,
        syn::Data::Enum(data_enum) => generate_enum_kind(data_enum, container_attrs)?,
        syn::Data::Union(_) => {
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
pub fn generate_struct_kind(
    fields: &Fields,
    container_attrs: &ContainerAttrs,
) -> syn::Result<TokenStream2> {
    // Handle transparent structs - return the inner type's TypeKind
    if container_attrs.transparent {
        return generate_transparent_struct_kind(fields);
    }

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

/// Generate TypeKind tokens for a transparent struct.
///
/// Returns the inner type's TypeKind directly.
fn generate_transparent_struct_kind(fields: &Fields) -> syn::Result<TokenStream2> {
    match fields {
        Fields::Named(named) => {
            // Find the single non-skipped field
            let mut active_fields = Vec::new();
            for f in &named.named {
                let attrs = FieldAttrs::from_ast(&f.attrs)?;
                if !attrs.skip {
                    active_fields.push(f);
                }
            }

            if active_fields.len() != 1 {
                return Err(syn::Error::new_spanned(
                    &named.named,
                    "#[ron(transparent)] requires exactly one non-skipped field",
                ));
            }

            Ok(type_to_type_kind(&active_fields[0].ty))
        }
        Fields::Unnamed(unnamed) => {
            if unnamed.unnamed.len() != 1 {
                return Err(syn::Error::new_spanned(
                    unnamed,
                    "#[ron(transparent)] requires exactly one field for tuple structs",
                ));
            }

            Ok(type_to_type_kind(&unnamed.unnamed[0].ty))
        }
        Fields::Unit => Err(syn::Error::new(
            proc_macro2::Span::call_site(),
            "#[ron(transparent)] cannot be used on unit structs",
        )),
    }
}

/// Generate TypeKind tokens for an enum.
pub fn generate_enum_kind(
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
    let variant_attrs = VariantAttrs::from_ast(&variant.attrs)?;
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
