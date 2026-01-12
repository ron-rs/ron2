//! ToRon derive macro implementation.

use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{Data, DeriveInput, Fields, Ident};

use crate::attr::{ContainerAttrs, FieldAttrs, VariantAttrs};

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
                let __value_result: ::ron2::error::Result<::ron2::Value> = (|| { #body })();
                __value_result.map(::ron2::ast::value_to_expr)
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
                    let serialize_expr = quote! {
                        match ::ron2::ToRon::to_ron_value(&self.#field_ident)? {
                            ::ron2::Value::Option(None) => {}
                            ::ron2::Value::Option(Some(inner)) => {
                                match *inner {
                                    ::ron2::Value::Named { content: ::ron2::NamedContent::Struct(nested_fields), .. } => {
                                        fields.extend(nested_fields);
                                    }
                                    ::ron2::Value::Struct(nested_fields) => {
                                        fields.extend(nested_fields);
                                    }
                                    _ => {
                                        return Err(::ron2::error::Error::Message(
                                            format!("flatten field {} must serialize to a struct", stringify!(#field_ident))
                                        ));
                                    }
                                }
                            }
                            ::ron2::Value::Named { content: ::ron2::NamedContent::Struct(nested_fields), .. } => {
                                fields.extend(nested_fields);
                            }
                            ::ron2::Value::Struct(nested_fields) => {
                                fields.extend(nested_fields);
                            }
                            _ => {
                                return Err(::ron2::error::Error::Message(
                                    format!("flatten field {} must serialize to a struct", stringify!(#field_ident))
                                ));
                            }
                        }
                    };
                    field_serializations.push(serialize_expr);
                    continue;
                }

                let serialize_expr = quote! {
                    ::ron2::ToRon::to_ron_value(&self.#field_ident)?
                };

                if let Some(ref predicate) = field_attrs.skip_serializing_if {
                    field_serializations.push(quote! {
                        if !#predicate(&self.#field_ident) {
                            fields.push((#field_name.to_string(), #serialize_expr));
                        }
                    });
                } else {
                    field_serializations.push(quote! {
                        fields.push((#field_name.to_string(), #serialize_expr));
                    });
                }
            }

            // Produce Named struct: StructName(field: val, ...)
            Ok(quote! {
                let mut fields: ::ron2::StructFields = ::std::vec::Vec::new();
                #(#field_serializations)*
                Ok(::ron2::Value::Named {
                    name: #struct_name.to_string(),
                    content: ::ron2::NamedContent::Struct(fields),
                })
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
                        ::ron2::ToRon::to_ron_value(&self.#index)?
                    }
                })
                .collect();

            Ok(quote! {
                Ok(::ron2::Value::Named {
                    name: #struct_name.to_string(),
                    content: ::ron2::NamedContent::Tuple(vec![#(#field_serializations),*]),
                })
            })
        }
        Fields::Unit => {
            // Unit struct -> Named unit: UnitStruct
            Ok(quote! {
                Ok(::ron2::Value::Named {
                    name: #struct_name.to_string(),
                    content: ::ron2::NamedContent::Unit,
                })
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
                        Ok(::ron2::Value::Named {
                            name: #variant_name.to_string(),
                            content: ::ron2::NamedContent::Unit,
                        })
                    }
                }
            }
            Fields::Unnamed(unnamed) => {
                let field_names: Vec<_> = (0..unnamed.unnamed.len())
                    .map(|i| Ident::new(&format!("field{}", i), variant_ident.span()))
                    .collect();

                let field_values: Vec<_> = field_names
                    .iter()
                    .map(|f| quote! { ::ron2::ToRon::to_ron_value(#f)? })
                    .collect();

                // Tuple variant: Variant(a, b) -> Variant(a, b)
                quote! {
                    #name::#variant_ident(#(#field_names),*) => {
                        Ok(::ron2::Value::Named {
                            name: #variant_name.to_string(),
                            content: ::ron2::NamedContent::Tuple(vec![#(#field_values),*]),
                        })
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

                let mut field_serializations = Vec::new();
                for field in &named.named {
                    let field_attrs = FieldAttrs::from_ast(&field.attrs)?;
                    if field_attrs.should_skip_serializing() {
                        continue;
                    }

                    let field_ident = field.ident.as_ref().unwrap();
                    let ron_name =
                        field_attrs.effective_name(&field_ident.to_string(), container_attrs);

                    field_serializations.push(quote! {
                        (#ron_name.to_string(), ::ron2::ToRon::to_ron_value(#field_ident)?)
                    });
                }

                quote! {
                    #name::#variant_ident { #(#field_names),* } => {
                        let fields: ::ron2::StructFields = vec![#(#field_serializations),*];
                        Ok(::ron2::Value::Named {
                            name: #variant_name.to_string(),
                            content: ::ron2::NamedContent::Struct(fields),
                        })
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
    match fields {
        Fields::Named(named) => {
            // Find the single non-skipped field
            let mut active_fields = Vec::new();
            for field in &named.named {
                let field_attrs = FieldAttrs::from_ast(&field.attrs)?;
                if !field_attrs.should_skip_serializing() {
                    active_fields.push(field);
                }
            }

            if active_fields.len() != 1 {
                return Err(syn::Error::new_spanned(
                    name,
                    "#[ron(transparent)] requires exactly one non-skipped field",
                ));
            }

            let field_ident = active_fields[0].ident.as_ref().unwrap();
            Ok(quote! {
                ::ron2::ToRon::to_ron_value(&self.#field_ident)
            })
        }
        Fields::Unnamed(unnamed) => {
            // For tuple structs, must have exactly one field
            if unnamed.unnamed.len() != 1 {
                return Err(syn::Error::new_spanned(
                    name,
                    "#[ron(transparent)] requires exactly one field for tuple structs",
                ));
            }

            Ok(quote! {
                ::ron2::ToRon::to_ron_value(&self.0)
            })
        }
        Fields::Unit => Err(syn::Error::new_spanned(
            name,
            "#[ron(transparent)] cannot be used on unit structs",
        )),
    }
}
