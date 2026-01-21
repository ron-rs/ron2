//! FromRon derive macro implementation.
//!
//! This module generates `FromRon` implementations that work directly with
//! AST expressions, preserving span information for precise error messages.

use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn::{Data, DeriveInput, Fields, Ident};

use crate::{
    attr::{ContainerAttrs, FieldAttrs, FieldDefault, VariantAttrs},
    field_util::{FieldSkipMode, TransparentField, validate_transparent_struct},
};

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

    // Generate FromRonFields impl for named structs (needed for #[ron(flatten)])
    let from_ron_fields_impl = match &input.data {
        Data::Struct(data) => match &data.fields {
            Fields::Named(named) => Some(derive_from_ron_fields(
                name,
                named,
                &container_attrs,
                &impl_generics,
                &ty_generics,
                where_clause,
            )?),
            _ => None,
        },
        _ => None,
    };

    // Generate only from_ast - from_ron_value uses the default trait implementation
    // which converts Value to Expr and calls from_ast
    Ok(quote! {
        impl #impl_generics ::ron2::FromRon for #name #ty_generics #where_clause {
            fn from_ast(expr: &::ron2::ast::Expr<'_>) -> ::ron2::error::Result<Self> {
                #from_ast_body
            }
        }

        #from_ron_fields_impl
    })
}

/// Generate FromRonFields implementation for named structs.
///
/// This allows the struct to be used with `#[ron(flatten)]` in parent structs.
fn derive_from_ron_fields(
    name: &Ident,
    named: &syn::FieldsNamed,
    container_attrs: &ContainerAttrs,
    impl_generics: &syn::ImplGenerics<'_>,
    ty_generics: &syn::TypeGenerics<'_>,
    where_clause: Option<&syn::WhereClause>,
) -> syn::Result<TokenStream2> {
    let mut field_extractions = Vec::new();
    let mut field_names = Vec::new();

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

        let ron_name = field_attrs.effective_name(&field_ident.to_string(), container_attrs);
        let extraction =
            generate_field_extraction(field_ident, field_ty, &ron_name, &field_attrs, field)?;
        field_extractions.push(extraction);
    }

    Ok(quote! {
        impl #impl_generics ::ron2::FromRonFields for #name #ty_generics #where_clause {
            fn from_fields(access: &mut ::ron2::AstMapAccess<'_>) -> ::ron2::error::Result<Self> {
                #(#field_extractions)*
                Ok(#name { #(#field_names),* })
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
    // Handle transparent structs
    if container_attrs.transparent {
        return derive_transparent_struct_de(name, fields);
    }

    match fields {
        Fields::Named(named) => derive_named_struct_de(name, named, container_attrs),
        Fields::Unnamed(unnamed) => derive_tuple_struct_de(name, unnamed, container_attrs),
        Fields::Unit => derive_unit_struct_de(name, container_attrs),
    }
}

/// Generate deserialization for a named struct (with named fields).
fn derive_named_struct_de(
    name: &Ident,
    named: &syn::FieldsNamed,
    container_attrs: &ContainerAttrs,
) -> syn::Result<TokenStream2> {
    let struct_name = container_attrs
        .rename
        .clone()
        .unwrap_or_else(|| name.to_string());
    let mut field_extractions = Vec::new();
    let mut field_names = Vec::new();
    let mut known_fields = Vec::new();
    let mut all_fields_have_defaults = true;

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

        // Flatten fields don't have a field name in the RON - they consume from parent access
        if !field_attrs.flatten {
            known_fields.push(ron_name.clone());
        }

        // Check if field has a default (includes opt which implies default)
        if !field_attrs.has_default() && !field_attrs.flatten {
            all_fields_have_defaults = false;
        }

        // Generate field extraction using the shared helper
        let extraction =
            generate_field_extraction(field_ident, field_ty, &ron_name, &field_attrs, field)?;
        field_extractions.push(extraction);
    }

    // Generate Unit case only if all fields have defaults
    let unit_case = if all_fields_have_defaults {
        // Build unit extractions using defaults
        let mut unit_extractions = Vec::new();
        for field in &named.named {
            let field_attrs = FieldAttrs::from_ast(&field.attrs)?;
            let field_ident = field.ident.as_ref().unwrap();
            let field_ty = &field.ty;

            if field_attrs.should_skip_deserializing() {
                unit_extractions.push(quote! {
                    let #field_ident: #field_ty = ::std::default::Default::default();
                });
                continue;
            }

            // Note: opt implies default (uses Default::default())
            let unit_extraction = match &field_attrs.default {
                FieldDefault::Default => {
                    quote! {
                        let #field_ident: #field_ty = ::std::default::Default::default();
                    }
                }
                FieldDefault::Path(path) => {
                    quote! {
                        let #field_ident: #field_ty = #path();
                    }
                }
                FieldDefault::None if field_attrs.flatten || field_attrs.opt => {
                    quote! {
                        let #field_ident: #field_ty = ::std::default::Default::default();
                    }
                }
                _ => quote! {},
            };
            unit_extractions.push(unit_extraction);
        }

        quote! {
            // Unit: () - accept for structs with all fields having defaults
            ::ron2::ast::Expr::Unit(_) => {
                #(#unit_extractions)*
                Ok(#name { #(#field_names),* })
            }
        }
    } else {
        quote! {}
    };

    let deny_unknown = generate_deny_unknown_check(container_attrs, &known_fields);

    Ok(quote! {
        match expr {
            #unit_case
            // Anonymous struct: (field: val, ...)
            ::ron2::ast::Expr::AnonStruct(s) => {
                let mut access = ::ron2::AstMapAccess::from_anon(s, Some(stringify!(#name)))?;
                #(#field_extractions)*
                #deny_unknown
                Ok(#name { #(#field_names),* })
            }
            // Named struct: StructName(field: val) or StructName { field: val }
            ::ron2::ast::Expr::Struct(s) => {
                // Validate struct name matches
                let expected_name = #struct_name;
                let found_name = s.name.name.as_ref();
                if found_name != expected_name {
                    return Err(::ron2::Error::with_span(
                        ::ron2::error::ErrorKind::TypeMismatch {
                            expected: format!("struct {}", expected_name),
                            found: format!("struct {}", found_name),
                        },
                        s.name.span.clone(),
                    ));
                }
                match &s.body {
                    Some(::ron2::ast::StructBody::Fields(fields)) => {
                        let mut access = ::ron2::AstMapAccess::from_fields(
                            fields,
                            Some(s.name.name.as_ref()),
                            s.span.clone(),
                        )?;
                        #(#field_extractions)*
                        #deny_unknown
                        Ok(#name { #(#field_names),* })
                    }
                    _ => Err(::ron2::Error::with_span(
                        ::ron2::error::ErrorKind::TypeMismatch {
                            expected: concat!("struct ", #struct_name).to_string(),
                            found: "non-struct body".to_string(),
                        },
                        s.span.clone(),
                    )),
                }
            }
            _ => {
                let found_type = ::ron2::convert::expr_type_name(expr);
                // Provide more helpful message when () is passed for a struct
                let expected_msg = if found_type == "unit" {
                    let fields: &[&str] = &[#(#known_fields),*];
                    if fields.is_empty() {
                        concat!("struct ", #struct_name).to_string()
                    } else {
                        format!("struct {} with fields: {}", #struct_name, fields.join(", "))
                    }
                } else {
                    concat!("struct ", #struct_name).to_string()
                };
                Err(::ron2::Error::with_span(
                    ::ron2::error::ErrorKind::TypeMismatch {
                        expected: expected_msg,
                        found: found_type.to_string(),
                    },
                    expr.span().clone(),
                ))
            }
        }
    })
}

/// Generate the deny_unknown_fields check if enabled.
fn generate_deny_unknown_check(
    container_attrs: &ContainerAttrs,
    known_fields: &[String],
) -> TokenStream2 {
    if container_attrs.deny_unknown_fields {
        let known_fields_slice: Vec<_> = known_fields.iter().map(|s| quote! { #s }).collect();
        quote! {
            access.deny_unknown_fields(&[#(#known_fields_slice),*])?;
        }
    } else {
        quote! {}
    }
}

/// Generate deserialization for a tuple struct.
fn derive_tuple_struct_de(
    name: &Ident,
    unnamed: &syn::FieldsUnnamed,
    container_attrs: &ContainerAttrs,
) -> syn::Result<TokenStream2> {
    let struct_name = container_attrs
        .rename
        .clone()
        .unwrap_or_else(|| name.to_string());
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
                // Validate struct name matches
                let expected_name = #struct_name;
                let found_name = s.name.name.as_ref();
                if found_name != expected_name {
                    return Err(::ron2::Error::with_span(
                        ::ron2::error::ErrorKind::TypeMismatch {
                            expected: format!("struct {}", expected_name),
                            found: format!("struct {}", found_name),
                        },
                        s.name.span.clone(),
                    ));
                }
                match &s.body {
                    Some(::ron2::ast::StructBody::Tuple(t)) => {
                        t.elements.iter().map(|e| &e.expr).collect()
                    }
                    _ => return Err(::ron2::Error::with_span(
                        ::ron2::error::ErrorKind::TypeMismatch {
                            expected: concat!("tuple struct ", #struct_name).to_string(),
                            found: "non-tuple body".to_string(),
                        },
                        s.span.clone(),
                    )),
                }
            }
            _ => return Err(::ron2::Error::with_span(
                ::ron2::error::ErrorKind::TypeMismatch {
                    expected: concat!("tuple struct ", #struct_name).to_string(),
                    found: ::ron2::convert::expr_type_name(expr).to_string(),
                },
                expr.span().clone(),
            )),
        };

        if elements.len() != #field_count {
            return Err(::ron2::Error::with_span(
                ::ron2::error::ErrorKind::TypeMismatch {
                    expected: format!("tuple with {} elements", #field_count),
                    found: format!("tuple with {} elements", elements.len()),
                },
                expr.span().clone(),
            ));
        }

        #(#field_extractions)*
        Ok(#name(#(#field_indices),*))
    })
}

/// Generate deserialization for a unit struct.
fn derive_unit_struct_de(
    name: &Ident,
    container_attrs: &ContainerAttrs,
) -> syn::Result<TokenStream2> {
    let struct_name = container_attrs
        .rename
        .clone()
        .unwrap_or_else(|| name.to_string());

    Ok(quote! {
        match expr {
            // Unit: ()
            ::ron2::ast::Expr::Unit(_) => Ok(#name),
            // Named unit: UnitStruct
            ::ron2::ast::Expr::Struct(s) if s.body.is_none() => {
                // Validate struct name matches
                let expected_name = #struct_name;
                let found_name = s.name.name.as_ref();
                if found_name != expected_name {
                    return Err(::ron2::Error::with_span(
                        ::ron2::error::ErrorKind::TypeMismatch {
                            expected: format!("struct {}", expected_name),
                            found: format!("struct {}", found_name),
                        },
                        s.name.span.clone(),
                    ));
                }
                Ok(#name)
            }
            _ => Err(::ron2::Error::with_span(
                ::ron2::error::ErrorKind::TypeMismatch {
                    expected: concat!("unit struct ", #struct_name).to_string(),
                    found: ::ron2::convert::expr_type_name(expr).to_string(),
                },
                expr.span().clone(),
            )),
        }
    })
}

/// Generate deserialization for a transparent struct.
///
/// Transparent structs deserialize as their single inner field directly.
fn derive_transparent_struct_de(name: &Ident, fields: &Fields) -> syn::Result<TokenStream2> {
    let transparent = validate_transparent_struct(name, fields, FieldSkipMode::Deserializing)?;

    match transparent {
        TransparentField::Named {
            ident,
            ty,
            skipped_fields,
            ..
        } => {
            // Generate default values for skipped fields
            let skipped_inits = skipped_fields.iter().map(|(name, ty)| {
                quote! {
                    #name: <#ty as ::std::default::Default>::default()
                }
            });

            Ok(quote! {
                let inner = <#ty as ::ron2::FromRon>::from_ast(expr)?;
                Ok(#name {
                    #ident: inner,
                    #(#skipped_inits,)*
                })
            })
        }
        TransparentField::Unnamed { ty } => Ok(quote! {
            let inner = <#ty as ::ron2::FromRon>::from_ast(expr)?;
            Ok(#name(inner))
        }),
    }
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
                            _ => Err(::ron2::Error::with_span(
                                ::ron2::error::ErrorKind::TypeMismatch {
                                    expected: concat!("unit variant ", #variant_name).to_string(),
                                    found: "variant with content".to_string(),
                                },
                                s.span.clone(),
                            )),
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
                                    return Err(::ron2::Error::with_span(
                                        ::ron2::error::ErrorKind::TypeMismatch {
                                            expected: format!(concat!("tuple variant ", #variant_name, " with {} elements"), #field_count),
                                            found: format!("tuple with {} elements", elements.len()),
                                        },
                                        s.span.clone(),
                                    ));
                                }
                                #(#field_extractions)*
                                Ok(#name::#variant_ident(#(#field_indices),*))
                            }
                            _ => Err(::ron2::Error::with_span(
                                ::ron2::error::ErrorKind::TypeMismatch {
                                    expected: concat!("tuple variant ", #variant_name).to_string(),
                                    found: "non-tuple variant body".to_string(),
                                },
                                s.span.clone(),
                            )),
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

                    // Generate field extraction using the shared helper
                    let extraction = generate_field_extraction(
                        field_ident,
                        field_ty,
                        &ron_name,
                        &field_attrs,
                        field,
                    )?;
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
                                )?;
                                #(#field_extractions)*
                                #deny_unknown
                                Ok(#name::#variant_ident { #(#field_names),* })
                            }
                            _ => Err(::ron2::Error::with_span(
                                ::ron2::error::ErrorKind::TypeMismatch {
                                    expected: concat!("struct variant ", #variant_name).to_string(),
                                    found: "non-struct variant body".to_string(),
                                },
                                s.span.clone(),
                            )),
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
                    unknown => Err(::ron2::Error::with_span(
                        ::ron2::error::ErrorKind::UnknownVariant {
                            variant: ::std::borrow::Cow::Owned(unknown.to_string()),
                            expected: &[#(#variant_names),*],
                            outer: Some(::std::borrow::Cow::Borrowed(#enum_name)),
                        },
                        s.name.span.clone(),
                    )),
                }
            }
            _ => Err(::ron2::Error::with_span(
                ::ron2::error::ErrorKind::TypeMismatch {
                    expected: concat!("enum ", #enum_name).to_string(),
                    found: ::ron2::convert::expr_type_name(expr).to_string(),
                },
                expr.span().clone(),
            )),
        }
    })
}

/// Check if a type is `Option<T>` and return the inner type.
///
/// Recognizes `Option<T>`, `std::option::Option<T>`, and `core::option::Option<T>`.
/// Returns `Some(&inner_type)` if the type is `Option<T>`, `None` otherwise.
fn extract_option_inner(ty: &syn::Type) -> Option<&syn::Type> {
    if let syn::Type::Path(type_path) = ty {
        let path = &type_path.path;

        // Check if the path ends with Option
        let is_option = match path.segments.len() {
            1 => path.segments[0].ident == "Option",
            3 => {
                // std::option::Option or core::option::Option
                let first = &path.segments[0].ident;
                let second = &path.segments[1].ident;
                let third = &path.segments[2].ident;
                (first == "std" || first == "core") && second == "option" && third == "Option"
            }
            _ => false,
        };

        if is_option {
            let seg = path.segments.last()?;
            if let syn::PathArguments::AngleBracketed(args) = &seg.arguments
                && let Some(syn::GenericArgument::Type(inner)) = args.args.first()
            {
                return Some(inner);
            }
        }
    }
    None
}

/// Generate the field extraction code for a named struct field.
///
/// This is shared between struct deserialization and enum variant field deserialization.
fn generate_field_extraction(
    field_ident: &Ident,
    field_ty: &syn::Type,
    ron_name: &str,
    field_attrs: &FieldAttrs,
    field: &syn::Field,
) -> syn::Result<TokenStream2> {
    // Handle flatten fields - consume from the same access
    if field_attrs.flatten {
        return Ok(quote! {
            let #field_ident: #field_ty = access.flatten()?;
        });
    }

    if field_attrs.explicit {
        // Explicit mode: require Some(...) or None syntax for Option fields
        let inner_ty = extract_option_inner(field_ty).ok_or_else(|| {
            syn::Error::new_spanned(
                field,
                "#[ron(explicit)] can only be used on Option<T> fields",
            )
        })?;

        Ok(match &field_attrs.default {
            FieldDefault::None => {
                quote! {
                    let #field_ident: #field_ty = access.required_explicit::<#inner_ty>(#ron_name)?;
                }
            }
            FieldDefault::Default | FieldDefault::Path(_) => {
                // For explicit Option fields with default, missing = None
                quote! {
                    let #field_ident: #field_ty = access.with_default_explicit::<#inner_ty>(#ron_name)?;
                }
            }
        })
    } else {
        // Standard mode: uses implicit Some via Option<T>::from_ast
        // Note: opt implies default (uses Default::default() when missing)
        Ok(match &field_attrs.default {
            FieldDefault::None if field_attrs.opt => {
                quote! {
                    let #field_ident: #field_ty = access.with_default(#ron_name)?;
                }
            }
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
        })
    }
}
