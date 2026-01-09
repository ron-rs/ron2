//! FromRon derive macro implementation.

use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{Data, DeriveInput, Fields, Ident};

use crate::attr::{ContainerAttrs, FieldAttrs, FieldDefault, VariantAttrs};

/// Generate FromRon implementation for a type.
pub fn derive_from_ron(input: &DeriveInput) -> syn::Result<TokenStream2> {
    let name = &input.ident;
    let container_attrs = ContainerAttrs::from_ast(&input.attrs)?;

    let body = match &input.data {
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

    Ok(quote! {
        impl #impl_generics ::ron2::FromRon for #name #ty_generics #where_clause {
            fn from_ron_value(value: ::ron2::Value) -> ::ron2::error::Result<Self> {
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

                let ron_name =
                    field_attrs.effective_name(&field_ident.to_string(), container_attrs);
                known_fields.push(ron_name.clone());

                let deserialize_expr = quote! {
                    ::ron2::FromRon::from_ron_value(field_value)?
                };

                let default_expr = match &field_attrs.default {
                    FieldDefault::None => {
                        quote! {
                            return Err(::ron2::error::Error::missing_field(#ron_name))
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
                        return Err(::ron2::error::Error::invalid_value(format!("unknown field: {:?}", key)));
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
                    // Named struct: StructName(field: val) from original ron crate
                    ::ron2::Value::Named { name: _, content: ::ron2::NamedContent::Struct(fields) } => {
                        let mut result = ::std::collections::HashMap::new();
                        for (name, v) in fields {
                            result.insert(name, v);
                        }
                        result
                    }
                    other => return Err(::ron2::error::Error::type_mismatch(
                        concat!("named struct like ", #struct_name, "(field: val)"),
                        &other
                    )),
                };

                #(#field_extractions)*
                #deny_unknown

                Ok(#name {
                    #(#field_names),*
                })
            })
        }
        Fields::Unnamed(unnamed) => {
            // Tuple struct -> deserialize from sequence or Named tuple
            let struct_name = name.to_string();
            let field_count = unnamed.unnamed.len();
            let field_extractions: Vec<_> = unnamed
                .unnamed
                .iter()
                .map(|field| {
                    let field_ty = &field.ty;
                    quote! {
                        {
                            let v = seq.pop().ok_or_else(|| {
                                ::ron2::error::Error::invalid_value(
                                    format!("expected {} elements, got fewer", #field_count)
                                )
                            })?;
                            <#field_ty as ::ron2::FromRon>::from_ron_value(v)?
                        }
                    }
                })
                .collect();

            Ok(quote! {
                let mut seq = match value {
                    // Sequence: [a, b, c]
                    ::ron2::Value::Seq(seq) => seq,
                    // Tuple: (a, b, c)
                    ::ron2::Value::Tuple(seq) => seq,
                    // Named tuple: TupleStruct(a, b, c) from original ron crate
                    ::ron2::Value::Named { name: _, content: ::ron2::NamedContent::Tuple(seq) } => seq,
                    other => return Err(::ron2::error::Error::type_mismatch(
                        concat!("tuple struct like ", #struct_name, "(...)"),
                        &other
                    )),
                };
                if seq.len() != #field_count {
                    return Err(::ron2::error::Error::invalid_value(
                        format!("expected {} elements, got {}", #field_count, seq.len())
                    ));
                }
                seq.reverse();
                Ok(#name(#(#field_extractions),*))
            })
        }
        Fields::Unit => {
            let struct_name = name.to_string();
            Ok(quote! {
                match value {
                    ::ron2::Value::Unit => Ok(#name),
                    // Also accept empty map for compatibility
                    ::ron2::Value::Map(m) if m.is_empty() => Ok(#name),
                    // Named unit: UnitStruct from original ron crate
                    ::ron2::Value::Named { name: _, content: ::ron2::NamedContent::Unit } => Ok(#name),
                    other => Err(::ron2::error::Error::type_mismatch(
                        concat!("unit struct like ", #struct_name),
                        &other
                    )),
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
    let mut named_variant_arms = Vec::new();
    let mut string_variant_arms = Vec::new();

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
                // Unit variants can be parsed from Named(Unit) or bare String
                string_variant_arms.push(quote! {
                    #variant_name => Ok(#name::#variant_ident),
                });
                quote! {
                    #variant_name => {
                        match content {
                            ::ron2::NamedContent::Unit => Ok(#name::#variant_ident),
                            other => Err(::ron2::error::Error::invalid_value(
                                format!("expected unit variant, got {:?}", other)
                            )),
                        }
                    }
                }
            }
            Fields::Unnamed(unnamed) => {
                let field_count = unnamed.unnamed.len();
                let field_types: Vec<_> = unnamed.unnamed.iter().map(|f| &f.ty).collect();
                let field_extractions: Vec<_> = field_types
                    .iter()
                    .map(|ty| {
                        quote! {
                            {
                                let v = elements.pop().ok_or_else(|| {
                                    ::ron2::error::Error::invalid_value(
                                        format!("expected {} elements", #field_count)
                                    )
                                })?;
                                <#ty as ::ron2::FromRon>::from_ron_value(v)?
                            }
                        }
                    })
                    .collect();

                quote! {
                    #variant_name => {
                        match content {
                            ::ron2::NamedContent::Tuple(mut elements) => {
                                if elements.len() != #field_count {
                                    return Err(::ron2::error::Error::invalid_value(
                                        format!("expected {} elements, got {}", #field_count, elements.len())
                                    ));
                                }
                                elements.reverse();
                                Ok(#name::#variant_ident(#(#field_extractions),*))
                            }
                            other => Err(::ron2::error::Error::invalid_value(
                                format!("expected tuple variant, got {:?}", other)
                            )),
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

                    let ron_name =
                        field_attrs.effective_name(&field_ident.to_string(), container_attrs);

                    let default_expr = match &field_attrs.default {
                        FieldDefault::None => {
                            quote! {
                                return Err(::ron2::error::Error::missing_field(#ron_name))
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
                            Some(field_value) => ::ron2::FromRon::from_ron_value(field_value)?,
                            None => #default_expr,
                        };
                    });
                }

                quote! {
                    #variant_name => {
                        match content {
                            ::ron2::NamedContent::Struct(fields) => {
                                let mut map: ::std::collections::HashMap<String, ::ron2::Value> =
                                    fields.into_iter().collect();
                                #(#field_extractions)*
                                Ok(#name::#variant_ident { #(#field_names),* })
                            }
                            other => Err(::ron2::error::Error::invalid_value(
                                format!("expected struct variant, got {:?}", other)
                            )),
                        }
                    }
                }
            }
        };

        named_variant_arms.push(arm);
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

    Ok(quote! {
        // Enum in RON is Value::Named { name, content } or bare identifier for unit variants
        match value {
            ::ron2::Value::Named { name: variant_name, content } => {
                match variant_name.as_str() {
                    #(#named_variant_arms)*
                    unknown => Err(::ron2::error::Error::invalid_value(format!("unknown variant: {}", unknown))),
                }
            }
            // Also try to parse as string for unit variants only (bare identifier)
            ::ron2::Value::String(s) => {
                match s.as_str() {
                    #(#string_variant_arms)*
                    unknown => Err(::ron2::error::Error::invalid_value(format!("unknown variant: {}", unknown))),
                }
            }
            other => Err(::ron2::error::Error::type_mismatch(
                concat!("enum (Named with variants: ", #(#variant_names, " "),* , ")"),
                &other
            )),
        }
    })
}
