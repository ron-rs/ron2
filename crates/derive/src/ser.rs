//! ToRon derive macro implementation.

use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{Data, DeriveInput, Fields, Ident};

use crate::{
    attr::{ContainerAttrs, FieldAttrs, VariantAttrs},
    field_util::{
        FieldSkipMode, TransparentField, emit_flatten_ast_helper, generate_flatten_ast_merge,
        validate_transparent_struct,
    },
};

/// Generate ToRon implementation for a type.
pub fn derive_to_ron(input: &DeriveInput) -> syn::Result<TokenStream2> {
    let name = &input.ident;
    let container_attrs = ContainerAttrs::from_ast(&input.attrs)?;

    let body = match &input.data {
        Data::Struct(data) => derive_struct_ser(name, &data.fields, &container_attrs)?,
        Data::Enum(data) => derive_enum_ser(name, data, &container_attrs)?,
        Data::Union(_) => {
            return Err(syn::Error::new_spanned(
                input,
                "ToRon cannot be derived for unions",
            ));
        }
    };

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    Ok(quote! {
        impl #impl_generics ::ron2::ToRon for #name #ty_generics #where_clause {
            fn to_ast(&self) -> ::ron2::error::Result<::ron2::ast::Expr<'static>> {
                #body
            }
        }
    })
}

/// Generate serialization for a struct.
fn derive_struct_ser(
    name: &Ident,
    fields: &Fields,
    container_attrs: &ContainerAttrs,
) -> syn::Result<TokenStream2> {
    // Handle transparent structs
    if container_attrs.transparent {
        return derive_transparent_struct_ser(name, fields);
    }

    let struct_name = container_attrs
        .rename
        .clone()
        .unwrap_or_else(|| name.to_string());

    match fields {
        Fields::Named(named) => {
            // Check if we need the flatten helper
            let has_flatten = named.named.iter().any(|f| {
                FieldAttrs::from_ast(&f.attrs)
                    .map(|attrs| attrs.flatten)
                    .unwrap_or(false)
            });

            let field_count = named.named.len();
            let mut field_serializations = Vec::new();

            for field in &named.named {
                let field_attrs = FieldAttrs::from_ast(&field.attrs)?;

                if field_attrs.should_skip_serializing() {
                    continue;
                }

                let field_ident = field.ident.as_ref().unwrap();
                let field_name =
                    field_attrs.effective_name(&field_ident.to_string(), container_attrs);

                // Handle flatten: merge fields from nested struct
                if field_attrs.flatten {
                    let serialize_expr = generate_flatten_ast_merge(field_ident);
                    field_serializations.push(serialize_expr);
                    continue;
                }

                let field_ty = &field.ty;
                let serialize_expr = quote! {
                    ::ron2::ToRon::to_ast(&self.#field_ident)?
                };

                // Determine skip condition: opt takes precedence over skip_serializing_if
                let skip_condition = if field_attrs.opt {
                    Some(
                        quote! { self.#field_ident == <#field_ty as ::std::default::Default>::default() },
                    )
                } else {
                    field_attrs
                        .skip_serializing_if
                        .as_ref()
                        .map(|predicate| quote! { #predicate(&self.#field_ident) })
                };

                if let Some(condition) = skip_condition {
                    field_serializations.push(quote! {
                        if !(#condition) {
                            __fields.push((::std::borrow::Cow::Borrowed(#field_name), #serialize_expr));
                        }
                    });
                } else {
                    field_serializations.push(quote! {
                        __fields.push((::std::borrow::Cow::Borrowed(#field_name), #serialize_expr));
                    });
                }
            }

            // Produce Named struct using synthetic_struct
            let flatten_helper = if has_flatten {
                emit_flatten_ast_helper()
            } else {
                quote! {}
            };

            Ok(quote! {
                #flatten_helper

                let mut __fields: Vec<(::std::borrow::Cow<'static, str>, ::ron2::ast::Expr<'static>)> =
                    Vec::with_capacity(#field_count);
                #(#field_serializations)*
                Ok(::ron2::ast::synthetic_struct(#struct_name, __fields))
            })
        }
        Fields::Unnamed(unnamed) => {
            // Tuple struct -> serialize as Named tuple: TupleStruct(a, b, c)
            let field_serializations: Vec<_> = unnamed
                .unnamed
                .iter()
                .enumerate()
                .map(|(i, _)| {
                    let index = syn::Index::from(i);
                    quote! {
                        ::ron2::ToRon::to_ast(&self.#index)?
                    }
                })
                .collect();

            Ok(quote! {
                Ok(::ron2::ast::synthetic_named_tuple(#struct_name, vec![#(#field_serializations),*]))
            })
        }
        Fields::Unit => {
            // Unit struct -> Named unit: UnitStruct
            Ok(quote! {
                Ok(::ron2::ast::synthetic_named_unit(#struct_name))
            })
        }
    }
}

/// Generate serialization for an enum.
fn derive_enum_ser(
    name: &Ident,
    data: &syn::DataEnum,
    container_attrs: &ContainerAttrs,
) -> syn::Result<TokenStream2> {
    let mut arms = Vec::new();

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
                // Unit variant: Variant -> Variant
                quote! {
                    #name::#variant_ident => {
                        Ok(::ron2::ast::synthetic_named_unit(#variant_name))
                    }
                }
            }
            Fields::Unnamed(unnamed) => {
                let field_names: Vec<_> = (0..unnamed.unnamed.len())
                    .map(|i| Ident::new(&format!("field{}", i), variant_ident.span()))
                    .collect();

                let field_values: Vec<_> = field_names
                    .iter()
                    .map(|f| quote! { ::ron2::ToRon::to_ast(#f)? })
                    .collect();

                // Tuple variant: Variant(a, b) -> Variant(a, b)
                quote! {
                    #name::#variant_ident(#(#field_names),*) => {
                        Ok(::ron2::ast::synthetic_named_tuple(#variant_name, vec![#(#field_values),*]))
                    }
                }
            }
            Fields::Named(named) => {
                // Struct variant: Variant { field: val } -> Variant(field: val)
                let field_names: Vec<_> = named
                    .named
                    .iter()
                    .map(|f| f.ident.as_ref().unwrap())
                    .collect();

                let field_count = named.named.len();
                let mut field_serializations = Vec::new();
                for field in &named.named {
                    let field_attrs = FieldAttrs::from_ast(&field.attrs)?;
                    if field_attrs.should_skip_serializing() {
                        continue;
                    }

                    let field_ident = field.ident.as_ref().unwrap();
                    let field_ty = &field.ty;
                    let ron_name =
                        field_attrs.effective_name(&field_ident.to_string(), container_attrs);

                    let serialize_expr = quote! {
                        ::ron2::ToRon::to_ast(#field_ident)?
                    };

                    // Determine skip condition: opt takes precedence over skip_serializing_if
                    // Note: field_ident is bound by reference in pattern, so we dereference for comparison
                    let skip_condition = if field_attrs.opt {
                        Some(
                            quote! { *#field_ident == <#field_ty as ::std::default::Default>::default() },
                        )
                    } else {
                        field_attrs
                            .skip_serializing_if
                            .as_ref()
                            .map(|predicate| quote! { #predicate(#field_ident) })
                    };

                    if let Some(condition) = skip_condition {
                        field_serializations.push(quote! {
                            if !(#condition) {
                                __fields.push((::std::borrow::Cow::Borrowed(#ron_name), #serialize_expr));
                            }
                        });
                    } else {
                        field_serializations.push(quote! {
                            __fields.push((::std::borrow::Cow::Borrowed(#ron_name), #serialize_expr));
                        });
                    }
                }

                quote! {
                    #name::#variant_ident { #(#field_names),* } => {
                        let mut __fields: Vec<(::std::borrow::Cow<'static, str>, ::ron2::ast::Expr<'static>)> =
                            Vec::with_capacity(#field_count);
                        #(#field_serializations)*
                        Ok(::ron2::ast::synthetic_struct(#variant_name, __fields))
                    }
                }
            }
        };

        arms.push(arm);
    }

    Ok(quote! {
        match self {
            #(#arms)*
        }
    })
}

/// Generate serialization for a transparent struct.
///
/// Transparent structs serialize as their single inner field directly.
fn derive_transparent_struct_ser(name: &Ident, fields: &Fields) -> syn::Result<TokenStream2> {
    let transparent = validate_transparent_struct(name, fields, FieldSkipMode::Serializing)?;

    match transparent {
        TransparentField::Named { ident, .. } => Ok(quote! {
            ::ron2::ToRon::to_ast(&self.#ident)
        }),
        TransparentField::Unnamed { .. } => Ok(quote! {
            ::ron2::ToRon::to_ast(&self.0)
        }),
    }
}
