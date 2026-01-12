//! Utility functions for the derive macros.

use std::collections::HashSet;

use quote::quote;
use syn::{GenericArgument, PathArguments, Type};

use crate::type_mapper::PrimitiveKind;

const KNOWN_STD_TYPES: &[&str] = &[
    "Option",
    "Vec",
    "VecDeque",
    "HashSet",
    "BTreeSet",
    "LinkedList",
    "HashMap",
    "BTreeMap",
    "Box",
    "String",
];

const KNOWN_STD_MODULES: &[&str] = &[
    "std",
    "core",
    "alloc",
    "vec",
    "collections",
    "option",
    "string",
    "boxed",
];

/// Convert a syn::Path to a string representation.
///
/// Single-segment paths (local types) are prefixed with the crate name to produce
/// fully qualified paths like `my_crate::MyType`.
pub fn path_to_string_canonical(path: &syn::Path, generics: &HashSet<String>) -> String {
    let last = match path.segments.last() {
        Some(seg) => seg,
        None => return String::new(),
    };
    let last_ident = last.ident.to_string();

    if KNOWN_STD_TYPES.contains(&last_ident.as_str())
        && path
            .segments
            .iter()
            .take(path.segments.len().saturating_sub(1))
            .all(|seg| KNOWN_STD_MODULES.contains(&seg.ident.to_string().as_str()))
    {
        return format_path_segment(last, generics);
    }

    let raw_path = path
        .segments
        .iter()
        .map(|seg| format_path_segment(seg, generics))
        .collect::<Vec<_>>()
        .join("::");

    if path.segments.len() == 1
        && !generics.contains(&last_ident)
        && PrimitiveKind::from_ident(&last_ident).is_none()
        && !KNOWN_STD_TYPES.contains(&last_ident.as_str())
    {
        let crate_name = get_crate_name();
        format!("{}::{}", crate_name, raw_path)
    } else {
        raw_path
    }
}

pub fn type_to_string_canonical(ty: &Type, generics: &HashSet<String>) -> String {
    match ty {
        Type::Path(type_path) => path_to_string_canonical(&type_path.path, generics),
        Type::Reference(reference) => {
            format!("&{}", type_to_string_canonical(&reference.elem, generics))
        }
        Type::Tuple(tuple) => {
            if tuple.elems.is_empty() {
                return "()".to_string();
            }
            let elems: Vec<_> = tuple
                .elems
                .iter()
                .map(|t| type_to_string_canonical(t, generics))
                .collect();
            format!("({})", elems.join(", "))
        }
        Type::Array(array) => {
            let len = &array.len;
            let len = quote!(#len).to_string();
            format!(
                "[{}; {}]",
                type_to_string_canonical(&array.elem, generics),
                len
            )
        }
        Type::Slice(slice) => format!("[{}]", type_to_string_canonical(&slice.elem, generics)),
        Type::Paren(paren) => type_to_string_canonical(&paren.elem, generics),
        Type::Group(group) => type_to_string_canonical(&group.elem, generics),
        _ => quote!(#ty).to_string(),
    }
}

fn format_path_segment(segment: &syn::PathSegment, generics: &HashSet<String>) -> String {
    let ident = segment.ident.to_string();
    match &segment.arguments {
        PathArguments::None => ident,
        PathArguments::AngleBracketed(args) => {
            let args_str: Vec<String> = args
                .args
                .iter()
                .map(|arg| match arg {
                    GenericArgument::Type(t) => type_to_string_canonical(t, generics),
                    _ => quote!(#arg).to_string(),
                })
                .collect();
            if args_str.is_empty() {
                ident
            } else {
                format!("{}<{}>", ident, args_str.join(", "))
            }
        }
        PathArguments::Parenthesized(_) => ident,
    }
}

/// Get the crate name, normalized for Rust path syntax (hyphens replaced with underscores).
pub fn get_crate_name() -> String {
    std::env::var("CARGO_CRATE_NAME")
        .or_else(|_| std::env::var("CARGO_PKG_NAME").map(|s| s.replace('-', "_")))
        .unwrap_or_else(|_| "unknown_crate".to_string())
}
