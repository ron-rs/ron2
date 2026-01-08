//! SerRon derive macro implementation.

use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{Data, DeriveInput, Fields, Ident};

use crate::attr::{ContainerAttrs, FieldAttrs, VariantAttrs};

/// Generate SerRon implementation for a type.
pub fn derive_ser_ron(input: &DeriveInput) -> syn::Result<TokenStream2> {
    let name = &input.ident;
    let container_attrs = ContainerAttrs::from_ast(&input.attrs)?;

    let body = match &input.data {
        Data::Struct(data) => derive_struct_ser(name, &data.fields, &container_attrs)?,
        Data::Enum(data) => derive_enum_ser(name, data, &container_attrs)?,
        Data::Union(_) => {
            return Err(syn::Error::new_spanned(
                input,
                "SerRon cannot be derived for unions",
            ));
        }
    };

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    Ok(quote! {
        impl #impl_generics ::ron_schema::SerRon for #name #ty_generics #where_clause {
            fn to_ron_value(&self) -> ::std::result::Result<::ron::Value, ::ron_schema::RonError> {
                #body
            }
        }
    })
}

/// Generate serialization for a struct.
fn derive_struct_ser(
    _name: &Ident,
    fields: &Fields,
    container_attrs: &ContainerAttrs,
) -> syn::Result<TokenStream2> {
    match fields {
        Fields::Named(named) => {
            let mut field_serializations = Vec::new();

            for field in &named.named {
                let field_attrs = FieldAttrs::from_ast(&field.attrs)?;

                if field_attrs.should_skip_serializing() {
                    continue;
                }

                let field_ident = field.ident.as_ref().unwrap();
                let field_name = field_attrs.effective_name(&field_ident.to_string(), container_attrs);

                let serialize_expr = quote! {
                    ::ron_schema::SerRon::to_ron_value(&self.#field_ident)?
                };

                if let Some(ref predicate) = field_attrs.skip_serializing_if {
                    field_serializations.push(quote! {
                        if !#predicate(&self.#field_ident) {
                            map.insert(
                                ::ron::Value::String(#field_name.to_string()),
                                #serialize_expr
                            );
                        }
                    });
                } else {
                    field_serializations.push(quote! {
                        map.insert(
                            ::ron::Value::String(#field_name.to_string()),
                            #serialize_expr
                        );
                    });
                }
            }

            Ok(quote! {
                let mut map = ::ron::value::Map::new();
                #(#field_serializations)*
                Ok(::ron::Value::Map(map))
            })
        }
        Fields::Unnamed(unnamed) => {
            // Tuple struct -> serialize as sequence
            let field_serializations: Vec<_> = unnamed
                .unnamed
                .iter()
                .enumerate()
                .map(|(i, _)| {
                    let index = syn::Index::from(i);
                    quote! {
                        ::ron_schema::SerRon::to_ron_value(&self.#index)?
                    }
                })
                .collect();

            Ok(quote! {
                Ok(::ron::Value::Seq(vec![#(#field_serializations),*]))
            })
        }
        Fields::Unit => {
            Ok(quote! {
                Ok(::ron::Value::Unit)
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
        let variant_name = variant_attrs.effective_name(&variant_ident.to_string(), container_attrs);

        let arm = match &variant.fields {
            Fields::Unit => {
                // Unit variant: Variant -> "Variant"
                quote! {
                    #name::#variant_ident => {
                        // For unit variants, use a map with the variant name as key
                        let mut map = ::ron::value::Map::new();
                        map.insert(
                            ::ron::Value::String(#variant_name.to_string()),
                            ::ron::Value::Unit
                        );
                        Ok(::ron::Value::Map(map))
                    }
                }
            }
            Fields::Unnamed(unnamed) => {
                let field_names: Vec<_> = (0..unnamed.unnamed.len())
                    .map(|i| Ident::new(&format!("field{}", i), variant_ident.span()))
                    .collect();

                let field_values: Vec<_> = field_names
                    .iter()
                    .map(|f| quote! { ::ron_schema::SerRon::to_ron_value(#f)? })
                    .collect();

                if field_names.len() == 1 {
                    // Newtype variant: Variant(val) -> { "Variant": val }
                    let field = &field_names[0];
                    quote! {
                        #name::#variant_ident(#(#field_names),*) => {
                            let mut map = ::ron::value::Map::new();
                            map.insert(
                                ::ron::Value::String(#variant_name.to_string()),
                                ::ron_schema::SerRon::to_ron_value(#field)?
                            );
                            Ok(::ron::Value::Map(map))
                        }
                    }
                } else {
                    // Tuple variant: Variant(a, b) -> { "Variant": [a, b] }
                    quote! {
                        #name::#variant_ident(#(#field_names),*) => {
                            let mut map = ::ron::value::Map::new();
                            map.insert(
                                ::ron::Value::String(#variant_name.to_string()),
                                ::ron::Value::Seq(vec![#(#field_values),*])
                            );
                            Ok(::ron::Value::Map(map))
                        }
                    }
                }
            }
            Fields::Named(named) => {
                // Struct variant: Variant { field: val } -> { "Variant": { "field": val } }
                let field_names: Vec<_> = named
                    .named
                    .iter()
                    .map(|f| f.ident.as_ref().unwrap())
                    .collect();

                let mut field_serializations = Vec::new();
                for field in &named.named {
                    let field_attrs = FieldAttrs::from_ast(&field.attrs)?;
                    if field_attrs.should_skip_serializing() {
                        continue;
                    }

                    let field_ident = field.ident.as_ref().unwrap();
                    let ron_name = field_attrs.effective_name(&field_ident.to_string(), container_attrs);

                    field_serializations.push(quote! {
                        inner_map.insert(
                            ::ron::Value::String(#ron_name.to_string()),
                            ::ron_schema::SerRon::to_ron_value(#field_ident)?
                        );
                    });
                }

                quote! {
                    #name::#variant_ident { #(#field_names),* } => {
                        let mut inner_map = ::ron::value::Map::new();
                        #(#field_serializations)*
                        let mut map = ::ron::value::Map::new();
                        map.insert(
                            ::ron::Value::String(#variant_name.to_string()),
                            ::ron::Value::Map(inner_map)
                        );
                        Ok(::ron::Value::Map(map))
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
