//! `RonSchema` code generation.
//!
//! Field types are described by asking their own `RonSchema` impls
//! (`reference_kind`, `register_schemas`), never by inspecting their syntax,
//! so aliases, imports and custom types resolve exactly like in rustc.
//!
//! Every derived type has a type path, which `TypeRef`s and the
//! `SchemaCatalog` use as its identity, so recursive types stay finite. A
//! non-generic type's path is `module::Name`. A generic type's schema depends
//! on its arguments, so each instantiation is its own schema, identified by
//! `core::any::type_name` (`module::Name<u8>`), the same string `to_typed_ron`
//! writes into `#![type = ".."]`.

use std::collections::HashSet;

use proc_macro2::TokenStream;
use quote::{quote, quote_spanned};
use syn::spanned::Spanned;

use crate::{
    ast::{Container, Data, Field, Style, Variant, unraw},
    bound::{self, Derive},
};

pub fn expand(cont: &Container<'_>) -> TokenStream {
    let ident = cont.ident;
    let (impl_generics, ty_generics, _) = cont.generics.split_for_impl();
    let where_clause = bound::where_clause(cont, Derive::RonSchema);

    let kind = match &cont.data {
        Data::Struct(..) if cont.attrs.transparent.is_some() => {
            let ty = cont.transparent_field().map(|f| f.ty);
            quote!(<#ty as ::ron2::schema::RonSchema>::reference_kind())
        }
        Data::Struct(Style::Named, fields) => {
            let fields = field_schemas(fields);
            quote!(::ron2::schema::TypeKind::Struct { fields: #fields })
        }
        Data::Struct(Style::Tuple, fields) => {
            let kinds = fields.iter().filter(|f| f.in_schema()).map(reference_kind);
            quote!(::ron2::schema::TypeKind::Tuple(::std::vec![#(#kinds),*]))
        }
        Data::Struct(Style::Unit, _) => quote!(::ron2::schema::TypeKind::Unit),
        Data::Enum(variants) => {
            let variants = variants
                .iter()
                .filter(|v| v.attrs.skip.is_none())
                .map(variant_schema);
            quote!(::ron2::schema::TypeKind::Enum {
                variants: ::std::vec![#(#variants),*]
            })
        }
    };

    let schema_fn = cont.doc.as_ref().map(|doc| {
        quote! {
            fn schema() -> ::ron2::schema::Schema {
                ::ron2::schema::Schema::with_doc(#doc, <Self as ::ron2::schema::RonSchema>::type_kind())
            }
        }
    });

    let mut seen = HashSet::new();
    let registrations: Vec<_> = cont
        .all_fields()
        .filter(|f| f.in_schema())
        .map(|f| f.ty)
        .filter(|ty| seen.insert(quote!(#ty).to_string()))
        .map(|ty| quote!(<#ty as ::ron2::schema::RonSchema>::register_schemas(__catalog);))
        .collect();

    let type_path = if cont.is_generic() {
        quote!(::core::any::type_name::<Self>())
    } else {
        let rust_name = unraw(cont.ident);
        quote!(::core::concat!(::core::module_path!(), "::", #rust_name))
    };

    quote! {
        impl #impl_generics ::ron2::schema::RonSchema for #ident #ty_generics #where_clause {
            fn type_kind() -> ::ron2::schema::TypeKind {
                #kind
            }

            #schema_fn

            fn type_path() -> ::core::option::Option<&'static str> {
                ::core::option::Option::Some(#type_path)
            }

            fn register_schemas(__catalog: &mut ::ron2::schema::collect::SchemaCatalog) {
                if __catalog.insert::<Self>() {
                    #(#registrations)*
                }
            }
        }
    }
}

fn reference_kind(field: &Field<'_>) -> TokenStream {
    let ty = field.ty;
    quote_spanned!(ty.span()=> <#ty as ::ron2::schema::RonSchema>::reference_kind())
}

/// `vec![Field, ..]` for the fields that appear in RON input.
fn field_schemas(fields: &[Field<'_>]) -> TokenStream {
    let fields = fields.iter().filter(|f| f.in_schema()).map(|field| {
        let name = &field.name;
        let kind = reference_kind(field);
        let has_default = field.default_fn().is_some();
        let doc = field.doc.as_ref().map(|doc| quote!(.with_doc(#doc)));
        let field = if field.is_flatten() {
            quote!(::ron2::__private::flattened_field(#name, #kind, #has_default))
        } else if has_default {
            quote!(::ron2::schema::Field::optional(#name, #kind))
        } else {
            quote!(::ron2::schema::Field::new(#name, #kind))
        };
        quote!(#field #doc)
    });
    quote!(::std::vec![#(#fields),*])
}

fn variant_schema(variant: &Variant<'_>) -> TokenStream {
    let name = &variant.name;
    let doc = variant.doc.as_ref().map(|doc| quote!(.with_doc(#doc)));
    let variant = match variant.style {
        Style::Unit => quote!(::ron2::schema::Variant::unit(#name)),
        Style::Tuple => {
            let kinds = variant
                .fields
                .iter()
                .filter(|f| f.in_schema())
                .map(reference_kind);
            quote!(::ron2::schema::Variant::tuple(#name, ::std::vec![#(#kinds),*]))
        }
        Style::Named => {
            let fields = field_schemas(&variant.fields);
            quote!(::ron2::schema::Variant::struct_variant(#name, #fields))
        }
    };
    quote!(#variant #doc)
}
