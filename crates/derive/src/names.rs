//! Field-name trees (`FieldNames`) and the compile-time check that a struct
//! and its `#[ron(flatten)]` fields share no RON field name.
//!
//! The names of a flattened type are not known to the macro, so the check is
//! a constant evaluated by the compiler over the `FIELDS` constants of the
//! `*Fields` traits.

use proc_macro2::TokenStream;
use quote::quote;

use crate::ast::{Container, Field};

/// Which side of the format a tree or check covers.
#[derive(Clone, Copy)]
pub enum Direction {
    /// Fields read by `FromRon` (`FromRonFields`).
    Read,
    /// Fields written by `ToRon` (`ToRonFields`).
    Write,
}

impl Direction {
    fn trait_path(self) -> TokenStream {
        match self {
            Self::Read => quote!(::ron2::convert::FromRonFields),
            Self::Write => quote!(::ron2::convert::ToRonFields),
        }
    }

    fn includes(self, field: &Field<'_>) -> bool {
        match self {
            Self::Read => !field.skip_deserializing(),
            Self::Write => !field.skip_serializing(),
        }
    }

    fn verb(self) -> &'static str {
        match self {
            Self::Read => "reads",
            Self::Write => "writes",
        }
    }
}

/// A `&'static ::ron2::convert::FieldNames` expression listing the RON names
/// of `fields`, with the `FIELDS` of each flattened field's type.
pub fn tree(fields: &[Field<'_>], dir: Direction) -> TokenStream {
    let active = fields.iter().filter(|f| dir.includes(f));
    let (flattened, own): (Vec<_>, Vec<_>) = active.partition(|f| f.is_flatten());
    let own = own.iter().map(|f| &f.name);
    let trait_path = dir.trait_path();
    let flattened = flattened.iter().map(|f| {
        let ty = f.ty;
        quote!(<#ty as #trait_path>::FIELDS)
    });
    quote! {
        &::ron2::convert::FieldNames {
            own: &[#(#own),*],
            flattened: &[#(#flattened),*],
        }
    }
}

/// The check that `fields` (of the struct or variant called `label`) hold no
/// RON name twice, when one of them is flattened.
///
/// For a type without type or lifetime parameters this is a free constant,
/// so `cargo check` reports it. Otherwise the names depend on the
/// instantiation and it is a `const` block statement for the (de)serialize
/// function body, reported when that instantiation is compiled.
pub struct UniqueCheck {
    pub stmt: Option<TokenStream>,
    pub item: Option<TokenStream>,
}

pub fn unique_check(
    cont: &Container<'_>,
    fields: &[Field<'_>],
    dir: Direction,
    label: &str,
) -> UniqueCheck {
    let flattens = fields.iter().any(|f| dir.includes(f) && f.is_flatten());
    if !flattens {
        return UniqueCheck {
            stmt: None,
            item: None,
        };
    }
    let tree = tree(fields, dir);
    let verb = dir.verb();
    let check = quote! {
        ::ron2::__private::assert_unique_field_names(#tree, #label, #verb)
    };
    if cont.generics.params.is_empty() {
        UniqueCheck {
            stmt: None,
            item: Some(quote!(const _: () = #check;)),
        }
    } else {
        UniqueCheck {
            stmt: Some(quote!(const { #check };)),
            item: None,
        }
    }
}
