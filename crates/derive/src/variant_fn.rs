//! Derived enum code is split into one function per variant.
//!
//! A single `to_ast` or `from_ast` holding every variant's code in one
//! `match` reserves stack for all variants at once in debug builds: hundreds
//! of KB for an enum with many struct variants, so recursive enums overflow a
//! 2 MiB thread after a few levels. The trait method only dispatches to
//! per-variant functions marked `#[inline(never)]`.

use proc_macro2::{Ident, TokenStream};
use quote::quote;

use crate::ast::Container;

/// A hidden inherent impl of the container holding the per-variant `fns`,
/// with the derived impl's where-clause.
pub fn variant_fns(
    cont: &Container<'_>,
    where_clause: &TokenStream,
    fns: &[TokenStream],
) -> TokenStream {
    if fns.is_empty() {
        return quote!();
    }
    let ident = cont.ident;
    let (impl_generics, ty_generics, _) = cont.generics.split_for_impl();
    quote! {
        #[automatically_derived]
        impl #impl_generics #ident #ty_generics #where_clause {
            #(#fns)*
        }
    }
}

/// A hidden per-variant function, named by the variant's index so that raw
/// or non-snake-case variant names need no care. It takes one parameter,
/// `pattern: ty`: a single tuple keeps clippy's argument-count and
/// pass-by-reference lints quiet for any number and type of fields.
pub fn variant_fn(
    name: &Ident,
    (pattern, ty): (TokenStream, TokenStream),
    ret: &TokenStream,
    body: &TokenStream,
) -> TokenStream {
    quote! {
        #[doc(hidden)]
        #[inline(never)]
        fn #name(#pattern: #ty) -> ::ron2::error::Result<#ret> {
            #body
        }
    }
}
