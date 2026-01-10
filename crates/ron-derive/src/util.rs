//! Utility functions for the derive macros.

use quote::quote;
use syn::{GenericArgument, PathArguments};

/// Convert a syn::Path to a string representation.
///
/// Single-segment paths (local types) are prefixed with the crate name to produce
/// fully qualified paths like `my_crate::MyType`.
pub fn path_to_string(path: &syn::Path) -> String {
    let raw_path = path
        .segments
        .iter()
        .map(|seg| {
            let ident = seg.ident.to_string();
            match &seg.arguments {
                PathArguments::None => ident,
                PathArguments::AngleBracketed(args) => {
                    let args_str: Vec<String> = args
                        .args
                        .iter()
                        .filter_map(|arg| {
                            if let GenericArgument::Type(t) = arg {
                                Some(quote!(#t).to_string())
                            } else {
                                None
                            }
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
        })
        .collect::<Vec<_>>()
        .join("::");

    // Qualify single-segment paths (local types) with crate name
    if path.segments.len() == 1 {
        let crate_name = get_crate_name();
        format!("{}::{}", crate_name, raw_path)
    } else {
        raw_path
    }
}

/// Get the crate name, normalized for Rust path syntax (hyphens replaced with underscores).
pub fn get_crate_name() -> String {
    std::env::var("CARGO_CRATE_NAME")
        .or_else(|_| std::env::var("CARGO_PKG_NAME").map(|s| s.replace('-', "_")))
        .unwrap_or_else(|_| "unknown_crate".to_string())
}
