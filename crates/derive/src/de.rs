//! `FromRon` code generation.
//!
//! Generated code reads straight from the AST, so errors keep source spans.
//! Expression-shape checks live in `ron2::__private`; the expansion only
//! wires fields to them.

use proc_macro2::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use syn::spanned::Spanned;

use crate::{
    ast::{Container, Data, Field, Style, Variant, unraw},
    bound::{self, Derive},
};

pub fn expand(cont: &Container<'_>) -> TokenStream {
    let ident = cont.ident;
    let (impl_generics, ty_generics, _) = cont.generics.split_for_impl();
    let where_clause = bound::where_clause(cont, Derive::FromRon);

    let (body, fields_impl) = match &cont.data {
        Data::Struct(..) if cont.attrs.transparent.is_some() => (transparent(cont), None),
        Data::Struct(Style::Named, fields) => {
            let (body, fields_impl) = named_struct(cont, fields);
            (body, Some(fields_impl))
        }
        Data::Struct(Style::Tuple, fields) => (tuple_struct(cont, fields), None),
        Data::Struct(Style::Unit, _) => {
            let name = &cont.name;
            let body = quote! {
                ::ron2::__private::unit_struct(__expr, #name)?;
                ::core::result::Result::Ok(Self)
            };
            (body, None)
        }
        Data::Enum(variants) => (enum_body(cont, variants), None),
    };

    let fields_impl = fields_impl.map(|fields_impl| {
        quote! {
            impl #impl_generics ::ron2::convert::FromRonFields for #ident #ty_generics #where_clause {
                #fields_impl
            }
        }
    });

    quote! {
        impl #impl_generics ::ron2::FromRon for #ident #ty_generics #where_clause {
            fn from_ast(__expr: &::ron2::ast::Expr<'_>) -> ::ron2::error::Result<Self> {
                #body
            }
        }

        #fields_impl
    }
}

/// `FromRon` reads a named struct through its `FromRonFields` impl, which
/// `#[ron(flatten)]` in other structs uses too.
fn named_struct(cont: &Container<'_>, fields: &[Field<'_>]) -> (TokenStream, TokenStream) {
    let name = &cont.name;
    let own_names = own_field_names(fields);
    let accepts_empty = accepts_empty(fields);
    let deny = cont.attrs.deny_unknown_fields.map(|_| {
        quote! {
            ::ron2::__private::deny_unknown_fields(
                &__access,
                <Self as ::ron2::convert::FromRonFields>::field_names,
            )?;
        }
    });
    let from_ast = quote! {
        let mut __access =
            ::ron2::__private::struct_access(__expr, #name, &[#(#own_names),*], #accepts_empty)?;
        let __value = <Self as ::ron2::convert::FromRonFields>::from_fields(&mut __access)?;
        #deny
        ::core::result::Result::Ok(__value)
    };

    let construct = construct_named(&quote!(Self), fields);
    let push_names = push_field_names(fields);
    let catch_all = fields
        .iter()
        .filter(|f| f.is_flatten() && !f.skip_deserializing())
        .map(|f| {
            let ty = f.ty;
            quote!(<#ty as ::ron2::convert::FromRonFields>::CATCH_ALL)
        });
    let fields_impl = quote! {
        const CATCH_ALL: bool = false #(|| #catch_all)*;

        fn from_fields(
            __access: &mut ::ron2::convert::AstMapAccess<'_>,
        ) -> ::ron2::error::Result<Self> {
            #construct
            ::core::result::Result::Ok(__value)
        }

        fn field_names(__names: &mut ::std::vec::Vec<&'static str>) {
            #push_names
        }
    };
    (from_ast, fields_impl)
}

fn tuple_struct(cont: &Container<'_>, fields: &[Field<'_>]) -> TokenStream {
    let name = &cont.name;
    let (len, values) = tuple_values(fields, |i| quote!(__elements.get(#i)));
    quote! {
        let __elements = ::ron2::__private::tuple_struct_elements(__expr, #name, #len)?;
        ::core::result::Result::Ok(Self(#(#values),*))
    }
}

/// The values of tuple fields, reading the non-skipped ones from consecutive
/// elements (`element(i)` gives the `i`th element as `&Expr`), and the
/// number of elements read.
fn tuple_values(
    fields: &[Field<'_>],
    element: impl Fn(usize) -> TokenStream,
) -> (usize, Vec<TokenStream>) {
    let mut len = 0;
    let values = fields
        .iter()
        .map(|field| {
            if field.attrs.skip.is_some() {
                return field.default_value();
            }
            let ty = field.ty;
            let expr = element(len);
            len += 1;
            quote_spanned!(ty.span()=> <#ty as ::ron2::FromRon>::from_ast(#expr)?)
        })
        .collect();
    (len, values)
}

fn transparent(cont: &Container<'_>) -> TokenStream {
    let Data::Struct(_, fields) = &cont.data else {
        return quote!();
    };
    let inits = fields.iter().map(|field| {
        let member = &field.member;
        let value = if field.attrs.skip.is_some() {
            field.default_value()
        } else {
            let ty = field.ty;
            quote_spanned!(ty.span()=> <#ty as ::ron2::FromRon>::from_ast(__expr)?)
        };
        quote!(#member: #value)
    });
    quote!(::core::result::Result::Ok(Self { #(#inits),* }))
}

fn enum_body(cont: &Container<'_>, variants: &[Variant<'_>]) -> TokenStream {
    let enum_name = unraw(cont.ident);
    let active: Vec<_> = variants.iter().filter(|v| v.attrs.skip.is_none()).collect();
    let names: Vec<_> = active.iter().map(|v| &v.name).collect();
    let arms = active.iter().map(|variant| {
        let name = &variant.name;
        let body = variant_body(cont, variant);
        quote!(#name => { #body })
    });

    // `None` and `Some(x)` parse as options, not as named variants.
    let none = active
        .iter()
        .find(|v| v.name == "None" && v.style == Style::Unit);
    let some = active.iter().find(|v| {
        v.name == "Some"
            && v.style == Style::Tuple
            && v.fields.len() == 1
            && v.fields[0].attrs.skip.is_none()
    });
    let option_arm = (none.is_some() || some.is_some()).then(|| {
        let none = match none {
            Some(v) => {
                let ident = v.ident;
                quote!(::core::result::Result::Ok(Self::#ident))
            }
            None => quote!(::core::result::Result::Err(
                ::ron2::__private::unexpected_option(#enum_name, __expr)
            )),
        };
        let some = match some {
            Some(v) => {
                let ident = v.ident;
                let ty = v.fields[0].ty;
                quote!(::core::result::Result::Ok(Self::#ident(
                    <#ty as ::ron2::FromRon>::from_ast(&__inner.expr)?
                )))
            }
            None => quote!(::core::result::Result::Err(
                ::ron2::__private::unexpected_option(#enum_name, __expr)
            )),
        };
        quote! {
            ::ron2::ast::Expr::Option(__option) => match &__option.value {
                ::core::option::Option::None => #none,
                ::core::option::Option::Some(__inner) => #some,
            },
        }
    });

    quote! {
        match __expr {
            ::ron2::ast::Expr::Struct(__s) => match ::core::convert::AsRef::<str>::as_ref(&__s.name.name) {
                #(#arms)*
                _ => ::core::result::Result::Err(
                    ::ron2::__private::unknown_variant(__s, #enum_name, &[#(#names),*]),
                ),
            },
            #option_arm
            _ => ::core::result::Result::Err(::ron2::__private::expected_enum(#enum_name, __expr)),
        }
    }
}

fn variant_body(cont: &Container<'_>, variant: &Variant<'_>) -> TokenStream {
    let ident = variant.ident;
    let name = &variant.name;
    match variant.style {
        Style::Unit => quote! {
            ::ron2::__private::unit_variant(__s, #name)?;
            ::core::result::Result::Ok(Self::#ident)
        },
        Style::Tuple => {
            let (len, values) = tuple_values(&variant.fields, |i| quote!(&__elements[#i].expr));
            quote! {
                let __elements = ::ron2::__private::tuple_variant_elements(__s, #name, #len)?;
                ::core::result::Result::Ok(Self::#ident(#(#values),*))
            }
        }
        Style::Named => {
            let accepts_empty = accepts_empty(&variant.fields);
            let construct = construct_named(&quote!(Self::#ident), &variant.fields);
            let deny = cont.attrs.deny_unknown_fields.map(|_| {
                let push_names = push_field_names(&variant.fields);
                quote! {
                    ::ron2::__private::deny_unknown_fields(__access, |__names| { #push_names })?;
                }
            });
            quote! {
                let __access = &mut ::ron2::__private::variant_access(__s, #name, #accepts_empty)?;
                #construct
                #deny
                ::core::result::Result::Ok(__value)
            }
        }
    }
}

/// Statements reading `__access` into `let __value = Path { .. };`, shared
/// by structs (via `FromRonFields`) and struct variants.
///
/// Fields are read in declaration order, except that flattened catch-all
/// fields (maps) are read last, so named fields claim their keys first.
fn construct_named(path: &TokenStream, fields: &[Field<'_>]) -> TokenStream {
    let locals: Vec<_> = (0..fields.len())
        .map(|i| format_ident!("__field{}", i))
        .collect();
    let mut first_pass = Vec::new();
    let mut flatten_pass = Vec::new();
    for (field, local) in fields.iter().zip(&locals) {
        let value = read_field(field);
        if field.is_flatten() && !field.skip_deserializing() {
            let ty = field.ty;
            first_pass.push(quote! {
                let mut #local = ::core::option::Option::None;
                if !<#ty as ::ron2::convert::FromRonFields>::CATCH_ALL {
                    #local = ::core::option::Option::Some(#value);
                }
            });
            flatten_pass.push(quote! {
                let #local = match #local {
                    ::core::option::Option::Some(__v) => __v,
                    ::core::option::Option::None => #value,
                };
            });
        } else {
            first_pass.push(quote!(let #local = #value;));
        }
    }
    let members = fields.iter().map(|f| &f.member);
    quote! {
        #(#first_pass)*
        #(#flatten_pass)*
        let __value = #path { #(#members: #locals),* };
    }
}

fn read_field(field: &Field<'_>) -> TokenStream {
    if field.skip_deserializing() {
        return field.default_value();
    }
    let ty = field.ty;
    let name = &field.name;
    let default = field.default_fn();
    let span = ty.span();
    if field.is_flatten() {
        return match default {
            Some(default) => quote_spanned! {span=>
                __access.flatten::<::core::option::Option<#ty>>()?.unwrap_or_else(#default)
            },
            None => quote_spanned!(span=> __access.flatten::<#ty>()?),
        };
    }
    let explicit = field.attrs.explicit.is_some();
    match (explicit, default) {
        (false, None) => quote_spanned!(span=> __access.required::<#ty>(#name)?),
        (false, Some(default)) => {
            quote_spanned!(span=> __access.with_default_fn::<#ty, _>(#name, #default)?)
        }
        (true, None) => quote_spanned! {span=>
            ::ron2::__private::read_explicit::<#ty>(__access, #name)?
        },
        (true, Some(default)) => quote_spanned! {span=>
            ::ron2::__private::read_explicit_or::<#ty>(__access, #name, #default)?
        },
    }
}

/// RON names of the fields read directly (not through `flatten`).
fn own_field_names<'f>(fields: &'f [Field<'_>]) -> Vec<&'f str> {
    fields
        .iter()
        .filter(|f| !f.skip_deserializing() && !f.is_flatten())
        .map(|f| f.name.as_str())
        .collect()
}

/// Body of a `FromRonFields::field_names`-like function filling `__names`.
fn push_field_names(fields: &[Field<'_>]) -> TokenStream {
    let own = own_field_names(fields);
    let flattened = fields
        .iter()
        .filter(|f| !f.skip_deserializing() && f.is_flatten())
        .map(|f| {
            let ty = f.ty;
            quote!(<#ty as ::ron2::convert::FromRonFields>::field_names(__names);)
        });
    quote! {
        __names.extend_from_slice(&[#(#own),*]);
        #(#flattened)*
    }
}

/// Whether `()` / `Name()` is read as a struct without fields: when every
/// field may have a value without input. Flattened fields decide that at
/// runtime (an empty map or a missing `Option` is fine, a required inner
/// field is a `MissingField` error); otherwise `()` gets a type error that
/// lists the expected fields.
fn accepts_empty(fields: &[Field<'_>]) -> bool {
    fields
        .iter()
        .all(|f| f.skip_deserializing() || f.is_flatten() || f.default_fn().is_some())
}
