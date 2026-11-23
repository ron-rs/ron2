//! Where-clause inference for generic types.
//!
//! Like serde, every type parameter used by a field that takes part in the
//! derived conversion is bounded by the derived trait (`T: FromRon`), and an
//! associated type used by a field is bounded itself (`T::Item: FromRon`).
//! Bounding type parameters rather than whole field types avoids the cycles
//! that `Vec<Self>: FromRon` would cause. Where the generated code calls a
//! trait on a field type directly (`Default`, `PartialEq`, and the
//! `*Fields` traits of flattened fields), that field type is bounded instead.
//! `#[ron(bound = "...")]` (or `bound(ser = .., de = .., schema = ..)`)
//! replaces the inferred predicates.

use std::collections::HashSet;

use proc_macro2::TokenStream;
use quote::{ToTokens, quote, quote_spanned};
use syn::{
    Ident, Type,
    spanned::Spanned,
    visit::{self, Visit},
};

use crate::{
    ast::{Container, Field},
    attr::FieldDefault,
};

#[derive(Clone, Copy)]
pub enum Derive {
    FromRon,
    ToRon,
    RonSchema,
}

/// The `where` clause for the impl of `derive` on `cont`.
pub fn where_clause(cont: &Container<'_>, derive: Derive) -> TokenStream {
    let mut predicates: Vec<TokenStream> = cont
        .generics
        .where_clause
        .iter()
        .flat_map(|w| w.predicates.iter())
        .map(ToTokens::to_token_stream)
        .collect();

    let explicit = match derive {
        Derive::FromRon => &cont.attrs.bound.de,
        Derive::ToRon => &cont.attrs.bound.ser,
        Derive::RonSchema => &cont.attrs.bound.schema,
    };
    if let Some(bound) = explicit {
        predicates.extend(bound.iter().map(ToTokens::to_token_stream));
    } else {
        let params: HashSet<Ident> = cont
            .generics
            .type_params()
            .map(|p| p.ident.clone())
            .collect();
        if !params.is_empty() {
            let mut inferred = Inferred::default();
            for field in cont.all_fields() {
                inferred.field(field, derive, &params);
            }
            predicates.extend(inferred.predicates);
        }
    }

    if predicates.is_empty() {
        quote!()
    } else {
        quote!(where #(#predicates,)*)
    }
}

#[derive(Default)]
struct Inferred {
    predicates: Vec<TokenStream>,
    seen: HashSet<String>,
}

impl Inferred {
    fn push(&mut self, predicate: TokenStream) {
        if self.seen.insert(predicate.to_string()) {
            self.predicates.push(predicate);
        }
    }

    /// `T: bound` for each parameter and associated type `ty` uses.
    fn bound_params(&mut self, ty: &Type, params: &HashSet<Ident>, bound: &TokenStream) {
        for target in generic_parts(ty, params) {
            self.push(quote!(#target: #bound));
        }
    }

    /// `ty: bound` if `ty` depends on a type parameter.
    fn bound_type(&mut self, ty: &Type, params: &HashSet<Ident>, bound: TokenStream) {
        if !generic_parts(ty, params).is_empty() {
            self.push(quote_spanned!(ty.span()=> #ty: #bound));
        }
    }

    fn field(&mut self, field: &Field<'_>, derive: Derive, params: &HashSet<Ident>) {
        let ty = field.ty;
        match derive {
            Derive::FromRon => {
                if uses_default_trait(field) {
                    self.bound_type(ty, params, quote!(::core::default::Default));
                }
                if field.skip_deserializing() {
                    return;
                }
                if field.is_flatten() {
                    self.bound_type(ty, params, quote!(::ron2::convert::FromRonFields));
                } else {
                    self.bound_params(ty, params, &quote!(::ron2::FromRon));
                }
            }
            Derive::ToRon => {
                if field.skip_serializing() {
                    return;
                }
                if field.attrs.opt.is_some() {
                    self.bound_type(
                        ty,
                        params,
                        quote!(::core::cmp::PartialEq + ::core::default::Default),
                    );
                }
                if field.is_flatten() {
                    self.bound_type(ty, params, quote!(::ron2::convert::ToRonFields));
                } else {
                    self.bound_params(ty, params, &quote!(::ron2::ToRon));
                }
            }
            Derive::RonSchema => {
                if field.in_schema() {
                    self.bound_params(ty, params, &quote!(::ron2::schema::RonSchema));
                }
            }
        }
    }
}

/// Whether deserialization calls `Default::default()` for this field.
fn uses_default_trait(field: &Field<'_>) -> bool {
    match field.attrs.default.as_ref().map(|d| &d.value) {
        Some(FieldDefault::Default) => true,
        Some(FieldDefault::Path(_)) => false,
        None => field.attrs.opt.is_some() || field.skip_deserializing(),
    }
}

/// The type parameters (`T`) and associated types (`T::Item`,
/// `<T as Tr>::Item`) that `ty` uses, ignoring uses inside `PhantomData`.
fn generic_parts(ty: &Type, params: &HashSet<Ident>) -> Vec<TokenStream> {
    struct Finder<'p> {
        params: &'p HashSet<Ident>,
        found: Vec<TokenStream>,
    }

    impl Finder<'_> {
        fn add(&mut self, tokens: TokenStream) {
            let key = tokens.to_string();
            if !self.found.iter().any(|t| t.to_string() == key) {
                self.found.push(tokens);
            }
        }

        fn mentions_param(&self, ty: &Type) -> bool {
            !generic_parts(ty, self.params).is_empty()
        }
    }

    impl<'ast> Visit<'ast> for Finder<'_> {
        fn visit_type_path(&mut self, ty: &'ast syn::TypePath) {
            if let Some(qself) = &ty.qself {
                if self.mentions_param(&qself.ty) {
                    self.add(ty.to_token_stream());
                    return;
                }
            } else if ty.path.leading_colon.is_none()
                && let Some(first) = ty.path.segments.first()
                && self.params.contains(&first.ident)
            {
                if ty.path.segments.len() == 1 {
                    self.add(first.ident.to_token_stream());
                } else {
                    self.add(ty.to_token_stream());
                }
                return;
            }
            visit::visit_type_path(self, ty);
        }

        fn visit_path(&mut self, path: &'ast syn::Path) {
            if path
                .segments
                .last()
                .is_some_and(|seg| seg.ident == "PhantomData")
            {
                return;
            }
            visit::visit_path(self, path);
        }

        fn visit_type_macro(&mut self, _: &'ast syn::TypeMacro) {}
    }

    let mut finder = Finder {
        params,
        found: Vec::new(),
    };
    finder.visit_type(ty);
    finder.found
}
