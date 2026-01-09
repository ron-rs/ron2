//! FromRon derive macro implementation.
//!
//! This module generates `FromRon` implementations that work directly with
//! AST expressions, preserving span information for precise error messages.

use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn::{Data, DeriveInput, Fields, Ident};

use crate::attr::{ContainerAttrs, FieldAttrs, FieldDefault, VariantAttrs};

/// Generate FromRon implementation for a type.
pub fn derive_from_ron(input: &DeriveInput) -> syn::Result<TokenStream2> {
    let name = &input.ident;
    let container_attrs = ContainerAttrs::from_ast(&input.attrs)?;

    let from_ast_body = match &input.data {
        Data::Struct(data) => derive_struct_de(name, &data.fields, &container_attrs)?,
        Data::Enum(data) => derive_enum_de(name, data, &container_attrs)?,
        Data::Union(_) => {
            return Err(syn::Error::new_spanned(
                input,
                "FromRon cannot be derived for unions",
            ));
        }
    };

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    // Generate only from_ast - from_ron_value uses the default trait implementation
    // which converts Value to Expr and calls from_ast
    Ok(quote! {
        impl #impl_generics ::ron2::FromRon for #name #ty_generics #where_clause {
            fn from_ast(expr: &::ron2::ast::Expr<'_>) -> ::ron2::error::SpannedResult<Self> {
                #from_ast_body
            }
        }
    })
}

/// Generate deserialization for a struct.
fn derive_struct_de(
    name: &Ident,
    fields: &Fields,
    container_attrs: &ContainerAttrs,
) -> syn::Result<TokenStream2> {
    match fields {
        Fields::Named(named) => derive_named_struct_de(name, named, container_attrs),
        Fields::Unnamed(unnamed) => derive_tuple_struct_de(name, unnamed),
        Fields::Unit => derive_unit_struct_de(name),
    }
}

/// Generate deserialization for a named struct (with named fields).
fn derive_named_struct_de(
    name: &Ident,
    named: &syn::FieldsNamed,
    container_attrs: &ContainerAttrs,
) -> syn::Result<TokenStream2> {
    let struct_name = name.to_string();
    let mut field_extractions = Vec::new();
    let mut field_names = Vec::new();
    let mut known_fields = Vec::new();

    for field in &named.named {
        let field_attrs = FieldAttrs::from_ast(&field.attrs)?;
        let field_ident = field.ident.as_ref().unwrap();
        let field_ty = &field.ty;

        field_names.push(field_ident.clone());

        if field_attrs.should_skip_deserializing() {
            // Use default for skipped fields
            field_extractions.push(quote! {
                let #field_ident: #field_ty = ::std::default::Default::default();
            });
            continue;
        }

        let ron_name = field_attrs.effective_name(&field_ident.to_string(), container_attrs);
        known_fields.push(ron_name.clone());

        // Generate field extraction using AstMapAccess methods
        let extraction = match &field_attrs.default {
            FieldDefault::None => {
                quote! {
                    let #field_ident: #field_ty = access.required(#ron_name)?;
                }
            }
            FieldDefault::Default => {
                quote! {
                    let #field_ident: #field_ty = access.with_default(#ron_name)?;
                }
            }
            FieldDefault::Path(path) => {
                quote! {
                    let #field_ident: #field_ty = access.with_default_fn(#ron_name, #path)?;
                }
            }
        };

        field_extractions.push(extraction);
    }

    let deny_unknown = if container_attrs.deny_unknown_fields {
        let known_fields_slice: Vec<_> = known_fields.iter().map(|s| quote! { #s }).collect();
        quote! {
            access.deny_unknown_fields(&[#(#known_fields_slice),*])?;
        }
    } else {
        quote! {}
    };

    Ok(quote! {
        match expr {
            // Anonymous struct: (field: val, ...)
            ::ron2::ast::Expr::AnonStruct(s) => {
                let mut access = ::ron2::AstMapAccess::from_anon(s);
                #(#field_extractions)*
                #deny_unknown
                Ok(#name { #(#field_names),* })
            }
            // Named struct: StructName(field: val) or StructName { field: val }
            ::ron2::ast::Expr::Struct(s) => {
                match &s.body {
                    Some(::ron2::ast::StructBody::Fields(fields)) => {
                        let mut access = ::ron2::AstMapAccess::from_fields(
                            fields,
                            Some(s.name.name.as_ref()),
                            s.span.clone(),
                        );
                        #(#field_extractions)*
                        #deny_unknown
                        Ok(#name { #(#field_names),* })
                    }
                    _ => Err(::ron2::error::SpannedError {
                        code: ::ron2::error::Error::InvalidValueForType {
                            expected: concat!("struct ", #struct_name).to_string(),
                            found: "non-struct body".to_string(),
                        },
                        span: s.span.clone(),
                    }),
                }
            }
            _ => Err(::ron2::error::SpannedError {
                code: ::ron2::error::Error::InvalidValueForType {
                    expected: concat!("struct ", #struct_name).to_string(),
                    found: ::ron2::convert::expr_type_name(expr).to_string(),
                },
                span: expr.span().clone(),
            }),
        }
    })
}

/// Generate deserialization for a tuple struct.
fn derive_tuple_struct_de(name: &Ident, unnamed: &syn::FieldsUnnamed) -> syn::Result<TokenStream2> {
    let struct_name = name.to_string();
    let field_count = unnamed.unnamed.len();

    // Generate field extraction by index
    let field_extractions: Vec<_> = unnamed
        .unnamed
        .iter()
        .enumerate()
        .map(|(i, field)| {
            let field_ty = &field.ty;
            let field_name = format_ident!("field_{}", i);
            quote! {
                let #field_name: #field_ty = <#field_ty as ::ron2::FromRon>::from_ast(elements[#i])?;
            }
        })
        .collect();

    let field_indices: Vec<_> = (0..field_count)
        .map(|i| {
            let field_name = format_ident!("field_{}", i);
            quote! { #field_name }
        })
        .collect();

    Ok(quote! {
        // Helper to extract elements from tuple-like expressions
        let elements: ::std::vec::Vec<&::ron2::ast::Expr<'_>> = match expr {
            // Tuple: (a, b, c)
            ::ron2::ast::Expr::Tuple(t) => {
                t.elements.iter().map(|e| &e.expr).collect()
            }
            // Sequence: [a, b, c]
            ::ron2::ast::Expr::Seq(s) => {
                s.items.iter().map(|i| &i.expr).collect()
            }
            // Named tuple: TupleStruct(a, b, c)
            ::ron2::ast::Expr::Struct(s) => {
                match &s.body {
                    Some(::ron2::ast::StructBody::Tuple(t)) => {
                        t.elements.iter().map(|e| &e.expr).collect()
                    }
                    _ => return Err(::ron2::error::SpannedError {
                        code: ::ron2::error::Error::InvalidValueForType {
                            expected: concat!("tuple struct ", #struct_name).to_string(),
                            found: "non-tuple body".to_string(),
                        },
                        span: s.span.clone(),
                    }),
                }
            }
            _ => return Err(::ron2::error::SpannedError {
                code: ::ron2::error::Error::InvalidValueForType {
                    expected: concat!("tuple struct ", #struct_name).to_string(),
                    found: ::ron2::convert::expr_type_name(expr).to_string(),
                },
                span: expr.span().clone(),
            }),
        };

        if elements.len() != #field_count {
            return Err(::ron2::error::SpannedError {
                code: ::ron2::error::Error::InvalidValueForType {
                    expected: format!("tuple with {} elements", #field_count),
                    found: format!("tuple with {} elements", elements.len()),
                },
                span: expr.span().clone(),
            });
        }

        #(#field_extractions)*
        Ok(#name(#(#field_indices),*))
    })
}

/// Generate deserialization for a unit struct.
fn derive_unit_struct_de(name: &Ident) -> syn::Result<TokenStream2> {
    let struct_name = name.to_string();

    Ok(quote! {
        match expr {
            // Unit: ()
            ::ron2::ast::Expr::Unit(_) => Ok(#name),
            // Named unit: UnitStruct
            ::ron2::ast::Expr::Struct(s) if s.body.is_none() => Ok(#name),
            _ => Err(::ron2::error::SpannedError {
                code: ::ron2::error::Error::InvalidValueForType {
                    expected: concat!("unit struct ", #struct_name).to_string(),
                    found: ::ron2::convert::expr_type_name(expr).to_string(),
                },
                span: expr.span().clone(),
            }),
        }
    })
}

/// Generate deserialization for an enum.
fn derive_enum_de(
    name: &Ident,
    data: &syn::DataEnum,
    container_attrs: &ContainerAttrs,
) -> syn::Result<TokenStream2> {
    let mut variant_arms = Vec::new();

    for variant in &data.variants {
        let variant_attrs = VariantAttrs::from_ast(&variant.attrs)?;

        if variant_attrs.skip {
            continue;
        }

        let variant_ident = &variant.ident;
        let variant_name =
            variant_attrs.effective_name(&variant_ident.to_string(), container_attrs);

        let arm = match &variant.fields {
            Fields::Unit => {
                // Unit variant: Variant or Variant()
                quote! {
                    #variant_name => {
                        match &s.body {
                            None => Ok(#name::#variant_ident),
                            Some(::ron2::ast::StructBody::Tuple(t)) if t.elements.is_empty() => {
                                Ok(#name::#variant_ident)
                            }
                            _ => Err(::ron2::error::SpannedError {
                                code: ::ron2::error::Error::InvalidValueForType {
                                    expected: concat!("unit variant ", #variant_name).to_string(),
                                    found: "variant with content".to_string(),
                                },
                                span: s.span.clone(),
                            }),
                        }
                    }
                }
            }
            Fields::Unnamed(unnamed) => {
                let field_count = unnamed.unnamed.len();

                // Generate field extraction by index
                let field_extractions: Vec<_> = unnamed
                    .unnamed
                    .iter()
                    .enumerate()
                    .map(|(i, field)| {
                        let field_ty = &field.ty;
                        let field_name = format_ident!("field_{}", i);
                        quote! {
                            let #field_name: #field_ty = <#field_ty as ::ron2::FromRon>::from_ast(&elements[#i])?;
                        }
                    })
                    .collect();

                let field_indices: Vec<_> = (0..field_count)
                    .map(|i| {
                        let field_name = format_ident!("field_{}", i);
                        quote! { #field_name }
                    })
                    .collect();

                quote! {
                    #variant_name => {
                        match &s.body {
                            Some(::ron2::ast::StructBody::Tuple(t)) => {
                                let elements: ::std::vec::Vec<_> = t.elements.iter().map(|e| &e.expr).collect();
                                if elements.len() != #field_count {
                                    return Err(::ron2::error::SpannedError {
                                        code: ::ron2::error::Error::InvalidValueForType {
                                            expected: format!(concat!("tuple variant ", #variant_name, " with {} elements"), #field_count),
                                            found: format!("tuple with {} elements", elements.len()),
                                        },
                                        span: s.span.clone(),
                                    });
                                }
                                #(#field_extractions)*
                                Ok(#name::#variant_ident(#(#field_indices),*))
                            }
                            _ => Err(::ron2::error::SpannedError {
                                code: ::ron2::error::Error::InvalidValueForType {
                                    expected: concat!("tuple variant ", #variant_name).to_string(),
                                    found: "non-tuple variant body".to_string(),
                                },
                                span: s.span.clone(),
                            }),
                        }
                    }
                }
            }
            Fields::Named(named) => {
                let mut field_extractions = Vec::new();
                let mut field_names = Vec::new();
                let mut known_fields = Vec::new();

                for field in &named.named {
                    let field_attrs = FieldAttrs::from_ast(&field.attrs)?;
                    let field_ident = field.ident.as_ref().unwrap();
                    let field_ty = &field.ty;

                    field_names.push(field_ident.clone());

                    if field_attrs.should_skip_deserializing() {
                        field_extractions.push(quote! {
                            let #field_ident: #field_ty = ::std::default::Default::default();
                        });
                        continue;
                    }

                    let ron_name =
                        field_attrs.effective_name(&field_ident.to_string(), container_attrs);
                    known_fields.push(ron_name.clone());

                    let extraction = match &field_attrs.default {
                        FieldDefault::None => {
                            quote! {
                                let #field_ident: #field_ty = access.required(#ron_name)?;
                            }
                        }
                        FieldDefault::Default => {
                            quote! {
                                let #field_ident: #field_ty = access.with_default(#ron_name)?;
                            }
                        }
                        FieldDefault::Path(path) => {
                            quote! {
                                let #field_ident: #field_ty = access.with_default_fn(#ron_name, #path)?;
                            }
                        }
                    };

                    field_extractions.push(extraction);
                }

                let deny_unknown = if container_attrs.deny_unknown_fields {
                    let known_fields_slice: Vec<_> =
                        known_fields.iter().map(|s| quote! { #s }).collect();
                    quote! {
                        access.deny_unknown_fields(&[#(#known_fields_slice),*])?;
                    }
                } else {
                    quote! {}
                };

                quote! {
                    #variant_name => {
                        match &s.body {
                            Some(::ron2::ast::StructBody::Fields(fields)) => {
                                let mut access = ::ron2::AstMapAccess::from_fields(
                                    fields,
                                    Some(#variant_name),
                                    s.span.clone(),
                                );
                                #(#field_extractions)*
                                #deny_unknown
                                Ok(#name::#variant_ident { #(#field_names),* })
                            }
                            _ => Err(::ron2::error::SpannedError {
                                code: ::ron2::error::Error::InvalidValueForType {
                                    expected: concat!("struct variant ", #variant_name).to_string(),
                                    found: "non-struct variant body".to_string(),
                                },
                                span: s.span.clone(),
                            }),
                        }
                    }
                }
            }
        };

        variant_arms.push(arm);
    }

    // Collect variant names for error message
    let variant_names: Vec<_> = data
        .variants
        .iter()
        .filter(|v| {
            !VariantAttrs::from_ast(&v.attrs)
                .map(|a| a.skip)
                .unwrap_or(false)
        })
        .map(|v| {
            let attrs = VariantAttrs::from_ast(&v.attrs).unwrap_or_default();
            attrs.effective_name(&v.ident.to_string(), container_attrs)
        })
        .collect();

    let enum_name = name.to_string();

    Ok(quote! {
        // Enums in RON are parsed as Expr::Struct with the variant name
        match expr {
            ::ron2::ast::Expr::Struct(s) => {
                let variant_name = s.name.name.as_ref();
                match variant_name {
                    #(#variant_arms)*
                    unknown => Err(::ron2::error::SpannedError {
                        code: ::ron2::error::Error::NoSuchEnumVariant {
                            expected: &[#(#variant_names),*],
                            found: ::std::borrow::Cow::Owned(unknown.to_string()),
                            outer: Some(::std::borrow::Cow::Borrowed(#enum_name)),
                        },
                        span: s.name.span.clone(),
                    }),
                }
            }
            _ => Err(::ron2::error::SpannedError {
                code: ::ron2::error::Error::InvalidValueForType {
                    expected: concat!("enum ", #enum_name).to_string(),
                    found: ::ron2::convert::expr_type_name(expr).to_string(),
                },
                span: expr.span().clone(),
            }),
        }
    })
}
