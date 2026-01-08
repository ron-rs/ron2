//! DeRon derive macro implementation.

use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{Data, DeriveInput, Fields, Ident};

use crate::attr::{ContainerAttrs, FieldAttrs, FieldDefault, VariantAttrs};

/// Generate DeRon implementation for a type.
pub fn derive_de_ron(input: &DeriveInput) -> syn::Result<TokenStream2> {
    let name = &input.ident;
    let container_attrs = ContainerAttrs::from_ast(&input.attrs)?;

    let body = match &input.data {
        Data::Struct(data) => derive_struct_de(name, &data.fields, &container_attrs)?,
        Data::Enum(data) => derive_enum_de(name, data, &container_attrs)?,
        Data::Union(_) => {
            return Err(syn::Error::new_spanned(
                input,
                "DeRon cannot be derived for unions",
            ));
        }
    };

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    Ok(quote! {
        impl #impl_generics ::ron_schema::DeRon for #name #ty_generics #where_clause {
            fn from_ron_value(value: ::ron2::Value) -> ::std::result::Result<Self, ::ron_schema::RonError> {
                #body
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
                    // Use default for skipped fields
                    field_extractions.push(quote! {
                        let #field_ident: #field_ty = ::std::default::Default::default();
                    });
                    continue;
                }

                let ron_name = field_attrs.effective_name(&field_ident.to_string(), container_attrs);
                known_fields.push(ron_name.clone());

                let deserialize_expr = quote! {
                    ::ron_schema::DeRon::from_ron_value(field_value)?
                };

                let default_expr = match &field_attrs.default {
                    FieldDefault::None => {
                        quote! {
                            return Err(::ron_schema::RonError::MissingField(#ron_name.to_string()))
                        }
                    }
                    FieldDefault::Default => {
                        quote! {
                            ::std::default::Default::default()
                        }
                    }
                    FieldDefault::Path(path) => {
                        quote! {
                            #path()
                        }
                    }
                };

                field_extractions.push(quote! {
                    let #field_ident: #field_ty = match map.remove(#ron_name) {
                        Some(field_value) => #deserialize_expr,
                        None => #default_expr,
                    };
                });
            }

            let deny_unknown = if container_attrs.deny_unknown_fields {
                quote! {
                    for key in map.keys() {
                        return Err(::ron_schema::RonError::UnknownField(key.clone()));
                    }
                }
            } else {
                quote! {}
            };

            Ok(quote! {
                let mut map = match value {
                    // ron2 parses (name: val) as Value::Struct
                    ::ron2::Value::Struct(fields) => {
                        let mut result = ::std::collections::HashMap::new();
                        for (name, v) in fields {
                            result.insert(name, v);
                        }
                        result
                    }
                    // { "name": val } is parsed as Value::Map
                    ::ron2::Value::Map(m) => {
                        let mut result = ::std::collections::HashMap::new();
                        for (k, v) in m {
                            if let ::ron2::Value::String(key) = k {
                                result.insert(key, v);
                            }
                        }
                        result
                    }
                    other => return Err(::ron_schema::RonError::type_mismatch("struct", other)),
                };

                #(#field_extractions)*
                #deny_unknown

                Ok(#name {
                    #(#field_names),*
                })
            })
        }
        Fields::Unnamed(unnamed) => {
            // Tuple struct -> deserialize from sequence
            let field_count = unnamed.unnamed.len();
            let field_extractions: Vec<_> = unnamed
                .unnamed
                .iter()
                .enumerate()
                .map(|(_i, field)| {
                    let field_ty = &field.ty;
                    quote! {
                        {
                            let v = seq.pop().ok_or_else(|| {
                                ::ron_schema::RonError::InvalidValue(
                                    format!("expected {} elements, got fewer", #field_count)
                                )
                            })?;
                            <#field_ty as ::ron_schema::DeRon>::from_ron_value(v)?
                        }
                    }
                })
                .collect();

            Ok(quote! {
                match value {
                    ::ron2::Value::Seq(mut seq) => {
                        if seq.len() != #field_count {
                            return Err(::ron_schema::RonError::InvalidValue(
                                format!("expected {} elements, got {}", #field_count, seq.len())
                            ));
                        }
                        seq.reverse();
                        Ok(#name(#(#field_extractions),*))
                    }
                    other => Err(::ron_schema::RonError::type_mismatch("tuple struct (Seq)", other)),
                }
            })
        }
        Fields::Unit => {
            Ok(quote! {
                match value {
                    ::ron2::Value::Unit => Ok(#name),
                    // Also accept empty map for compatibility
                    ::ron2::Value::Map(m) if m.is_empty() => Ok(#name),
                    other => Err(::ron_schema::RonError::type_mismatch("unit struct", other)),
                }
            })
        }
    }
}

/// Generate deserialization for an enum.
fn derive_enum_de(
    name: &Ident,
    data: &syn::DataEnum,
    container_attrs: &ContainerAttrs,
) -> syn::Result<TokenStream2> {
    let mut map_variant_arms = Vec::new();
    let mut string_variant_arms = Vec::new();

    for variant in &data.variants {
        let variant_attrs = VariantAttrs::from_ast(&variant.attrs)?;

        if variant_attrs.skip {
            continue;
        }

        let variant_ident = &variant.ident;
        let variant_name = variant_attrs.effective_name(&variant_ident.to_string(), container_attrs);

        let arm = match &variant.fields {
            Fields::Unit => {
                // Unit variants can be parsed from both Map and String
                string_variant_arms.push(quote! {
                    #variant_name => Ok(#name::#variant_ident),
                });
                quote! {
                    #variant_name => {
                        match inner {
                            ::ron2::Value::Unit => Ok(#name::#variant_ident),
                            other => Err(::ron_schema::RonError::type_mismatch("unit variant", other)),
                        }
                    }
                }
            }
            Fields::Unnamed(unnamed) => {
                let field_count = unnamed.unnamed.len();

                if field_count == 1 {
                    // Newtype variant
                    let field_ty = &unnamed.unnamed[0].ty;
                    quote! {
                        #variant_name => {
                            let inner_val = <#field_ty as ::ron_schema::DeRon>::from_ron_value(inner)?;
                            Ok(#name::#variant_ident(inner_val))
                        }
                    }
                } else {
                    // Tuple variant
                    let field_types: Vec<_> = unnamed.unnamed.iter().map(|f| &f.ty).collect();
                    let field_extractions: Vec<_> = field_types
                        .iter()
                        .map(|ty| {
                            quote! {
                                {
                                    let v = seq.pop().ok_or_else(|| {
                                        ::ron_schema::RonError::InvalidValue(
                                            format!("expected {} elements", #field_count)
                                        )
                                    })?;
                                    <#ty as ::ron_schema::DeRon>::from_ron_value(v)?
                                }
                            }
                        })
                        .collect();

                    quote! {
                        #variant_name => {
                            match inner {
                                ::ron2::Value::Seq(mut seq) => {
                                    if seq.len() != #field_count {
                                        return Err(::ron_schema::RonError::InvalidValue(
                                            format!("expected {} elements, got {}", #field_count, seq.len())
                                        ));
                                    }
                                    seq.reverse();
                                    Ok(#name::#variant_ident(#(#field_extractions),*))
                                }
                                other => Err(::ron_schema::RonError::type_mismatch("tuple variant (Seq)", other)),
                            }
                        }
                    }
                }
            }
            Fields::Named(named) => {
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

                    let default_expr = match &field_attrs.default {
                        FieldDefault::None => {
                            quote! {
                                return Err(::ron_schema::RonError::MissingField(#ron_name.to_string()))
                            }
                        }
                        FieldDefault::Default => {
                            quote! {
                                ::std::default::Default::default()
                            }
                        }
                        FieldDefault::Path(path) => {
                            quote! {
                                #path()
                            }
                        }
                    };

                    field_extractions.push(quote! {
                        let #field_ident: #field_ty = match map.remove(#ron_name) {
                            Some(field_value) => ::ron_schema::DeRon::from_ron_value(field_value)?,
                            None => #default_expr,
                        };
                    });
                }

                quote! {
                    #variant_name => {
                        let map: ::std::collections::HashMap<String, ::ron2::Value> = match inner {
                            // ron2 parses (name: val) as Value::Struct
                            ::ron2::Value::Struct(fields) => {
                                let mut result = ::std::collections::HashMap::new();
                                for (name, v) in fields {
                                    result.insert(name, v);
                                }
                                result
                            }
                            // { "name": val } is parsed as Value::Map
                            ::ron2::Value::Map(m) => {
                                let mut result = ::std::collections::HashMap::new();
                                for (k, v) in m {
                                    if let ::ron2::Value::String(key) = k {
                                        result.insert(key, v);
                                    }
                                }
                                result
                            }
                            other => return Err(::ron_schema::RonError::type_mismatch("struct variant", other)),
                        };

                        let mut map = map;
                        #(#field_extractions)*

                        Ok(#name::#variant_ident { #(#field_names),* })
                    }
                }
            }
        };

        map_variant_arms.push(arm);
    }

    // Collect variant names for error message
    let variant_names: Vec<_> = data
        .variants
        .iter()
        .filter(|v| !VariantAttrs::from_ast(&v.attrs).map(|a| a.skip).unwrap_or(false))
        .map(|v| {
            let attrs = VariantAttrs::from_ast(&v.attrs).unwrap_or_default();
            attrs.effective_name(&v.ident.to_string(), container_attrs)
        })
        .collect();

    Ok(quote! {
        // Enum in RON is typically { "Variant": value } or just "Variant" for unit
        match value {
            ::ron2::Value::Map(m) => {
                if m.len() != 1 {
                    return Err(::ron_schema::RonError::InvalidValue(
                        format!("expected map with single variant, got {} entries", m.len())
                    ));
                }
                let (variant_key, inner) = m.into_iter().next().unwrap();
                let variant_name = match variant_key {
                    ::ron2::Value::String(s) => s,
                    other => return Err(::ron_schema::RonError::type_mismatch("string (variant name)", other)),
                };

                match variant_name.as_str() {
                    #(#map_variant_arms)*
                    unknown => Err(::ron_schema::RonError::UnknownVariant(unknown.to_string())),
                }
            }
            // Also try to parse as string for unit variants only
            ::ron2::Value::String(s) => {
                match s.as_str() {
                    #(#string_variant_arms)*
                    unknown => Err(::ron_schema::RonError::UnknownVariant(unknown.to_string())),
                }
            }
            other => Err(::ron_schema::RonError::type_mismatch(
                concat!("enum (Map or String with variants: ", #(#variant_names, " "),* , ")"),
                other
            )),
        }
    })
}
