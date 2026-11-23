//! `ToRon` code generation.

use proc_macro2::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use syn::spanned::Spanned;

use crate::{
    ast::{Container, Data, Field, Style, Variant, unraw},
    bound::{self, Derive},
    names::{self, Direction},
};

pub fn expand(cont: &Container<'_>) -> TokenStream {
    let ident = cont.ident;
    let (impl_generics, ty_generics, _) = cont.generics.split_for_impl();
    let where_clause = bound::where_clause(cont, Derive::ToRon);
    let name = &cont.name;
    let mut checks = Vec::new();

    let mut fields_impl = None;
    let body = match &cont.data {
        Data::Struct(..) if cont.attrs.transparent.is_some() => {
            let member = cont.transparent_field().map(|f| &f.member);
            quote!(::ron2::ToRon::to_ast(&self.#member))
        }
        Data::Struct(Style::Named, fields) => {
            let active: Vec<_> = fields.iter().filter(|f| !f.skip_serializing()).collect();
            let pushes = push_fields(active.iter().map(|f| {
                let member = &f.member;
                (*f, quote!(&self.#member))
            }));
            let tree = names::tree(fields, Direction::Write);
            let check = names::unique_check(cont, fields, Direction::Write, &unraw(ident));
            checks.extend(check.item);
            let check = check.stmt;
            let catch_all = active.iter().filter(|f| f.is_flatten()).map(|f| {
                let ty = f.ty;
                quote!(<#ty as ::ron2::convert::ToRonFields>::CATCH_ALL)
            });
            fields_impl = Some(quote! {
                impl #impl_generics ::ron2::convert::ToRonFields for #ident #ty_generics #where_clause {
                    const CATCH_ALL: bool = false #(|| #catch_all)*;
                    const FIELDS: &'static ::ron2::convert::FieldNames = #tree;

                    fn to_fields(
                        &self,
                        __fields: &mut ::ron2::convert::StructFieldList,
                    ) -> ::ron2::error::Result<()> {
                        #check
                        #pushes
                        ::core::result::Result::Ok(())
                    }
                }
            });
            let capacity = active.len();
            quote! {
                let mut __fields = ::std::vec::Vec::with_capacity(#capacity);
                <Self as ::ron2::convert::ToRonFields>::to_fields(self, &mut __fields)?;
                ::core::result::Result::Ok(::ron2::ast::synthetic_struct(#name, __fields))
            }
        }
        Data::Struct(Style::Tuple, fields) => {
            let values = fields.iter().filter(|f| f.attrs.skip.is_none()).map(|f| {
                let member = &f.member;
                quote!(::ron2::ToRon::to_ast(&self.#member)?)
            });
            quote! {
                ::core::result::Result::Ok(::ron2::ast::synthetic_named_tuple(
                    #name,
                    ::std::vec![#(#values),*],
                ))
            }
        }
        Data::Struct(Style::Unit, _) => {
            quote!(::core::result::Result::Ok(::ron2::ast::synthetic_named_unit(#name)))
        }
        Data::Enum(variants) if variants.is_empty() => quote!(match *self {}),
        Data::Enum(variants) => {
            let enum_name = unraw(ident);
            let arms = variants
                .iter()
                .map(|v| variant_arm(cont, &enum_name, v, &mut checks));
            quote!(match self { #(#arms)* })
        }
    };

    quote! {
        impl #impl_generics ::ron2::ToRon for #ident #ty_generics #where_clause {
            fn to_ast(&self) -> ::ron2::error::Result<::ron2::ast::Expr<'static>> {
                #body
            }
        }

        #fields_impl
        #(#checks)*
    }
}

/// The match arm writing `variant`; free items (the field-name check) go to
/// `checks`.
fn variant_arm(
    cont: &Container<'_>,
    enum_name: &str,
    variant: &Variant<'_>,
    checks: &mut Vec<TokenStream>,
) -> TokenStream {
    let ident = variant.ident;
    let name = &variant.name;
    if variant.attrs.skip.is_some() {
        let rust_name = unraw(ident);
        return quote! {
            Self::#ident { .. } => ::core::result::Result::Err(
                ::ron2::__private::skipped_variant(#enum_name, #rust_name),
            ),
        };
    }
    match variant.style {
        Style::Unit => quote! {
            Self::#ident => ::core::result::Result::Ok(::ron2::ast::synthetic_named_unit(#name)),
        },
        Style::Tuple => {
            let patterns = variant.fields.iter().enumerate().map(|(i, f)| {
                if f.attrs.skip.is_some() {
                    quote!(_)
                } else {
                    let binding = format_ident!("__field{}", i);
                    quote!(#binding)
                }
            });
            let values = variant
                .fields
                .iter()
                .enumerate()
                .filter(|(_, f)| f.attrs.skip.is_none())
                .map(|(i, _)| {
                    let binding = format_ident!("__field{}", i);
                    quote!(::ron2::ToRon::to_ast(#binding)?)
                });
            quote! {
                Self::#ident(#(#patterns),*) => ::core::result::Result::Ok(
                    ::ron2::ast::synthetic_named_tuple(#name, ::std::vec![#(#values),*]),
                ),
            }
        }
        Style::Named => {
            let active: Vec<_> = variant
                .fields
                .iter()
                .filter(|f| !f.skip_serializing())
                .collect();
            let bindings: Vec<_> = (0..active.len())
                .map(|i| format_ident!("__field{}", i))
                .collect();
            let members = active.iter().map(|f| &f.member);
            let pushes = push_fields(
                active
                    .iter()
                    .copied()
                    .zip(bindings.iter().map(|b| quote!(#b))),
            );
            let capacity = active.len();
            let label = format!("{enum_name}::{}", unraw(ident));
            let check = names::unique_check(cont, &variant.fields, Direction::Write, &label);
            checks.extend(check.item);
            let check = check.stmt;
            quote! {
                Self::#ident { #(#members: #bindings,)* .. } => {
                    #check
                    let mut __vec = ::std::vec::Vec::with_capacity(#capacity);
                    {
                        let __fields = &mut __vec;
                        #pushes
                    }
                    ::core::result::Result::Ok(::ron2::ast::synthetic_struct(#name, __vec))
                }
            }
        }
    }
}

/// Statements appending fields to `__fields`, each given with an expression
/// of type `&FieldType`. Shared by structs (via `ToRonFields`) and struct
/// variants.
///
/// Fields keep declaration order, except that flattened catch-all fields
/// (maps) come last, matching the order in which `FromRon` claims keys.
fn push_fields<'f, 'a: 'f>(
    fields: impl Iterator<Item = (&'f Field<'a>, TokenStream)>,
) -> TokenStream {
    let mut pushes = Vec::new();
    let mut catch_all = Vec::new();
    for (field, value) in fields {
        let ty = field.ty;
        let span = ty.span();
        if field.is_flatten() {
            let write = quote_spanned! {span=>
                ::ron2::convert::ToRonFields::to_fields(#value, __fields)?;
            };
            let is_catch_all = quote!(<#ty as ::ron2::convert::ToRonFields>::CATCH_ALL);
            pushes.push(quote!(if !#is_catch_all { #write }));
            catch_all.push(quote!(if #is_catch_all { #write }));
            continue;
        }
        let name = &field.name;
        let push = quote_spanned! {span=>
            __fields.push((
                ::std::borrow::Cow::Borrowed(#name),
                ::ron2::ToRon::to_ast(#value)?,
            ));
        };
        pushes.push(if field.attrs.opt.is_some() {
            quote_spanned! {span=>
                if !::core::cmp::PartialEq::eq(
                    #value,
                    &<#ty as ::core::default::Default>::default(),
                ) {
                    #push
                }
            }
        } else if let Some(predicate) = &field.attrs.skip_serializing_if {
            let predicate = &predicate.value;
            quote!(if !#predicate(#value) { #push })
        } else {
            push
        });
    }
    quote!(#(#pushes)* #(#catch_all)*)
}
