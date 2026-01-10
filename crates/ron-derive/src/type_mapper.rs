//! Type mapping abstraction for converting Rust types to TypeKind representations.
//!
//! This module provides a trait-based abstraction that allows sharing type analysis logic
//! between code generation (TokenStream2) and compile-time building (TypeKind values).

use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{GenericArgument, PathArguments, Type};

use crate::util::path_to_string;

/// Primitive types that map directly to TypeKind variants.
#[derive(Debug, Clone, Copy)]
pub enum PrimitiveKind {
    Bool,
    I8,
    I16,
    I32,
    I64,
    I128,
    U8,
    U16,
    U32,
    U64,
    U128,
    F32,
    F64,
    Char,
    String,
}

impl PrimitiveKind {
    /// Try to parse a primitive kind from a type identifier.
    pub fn from_ident(ident: &str) -> Option<Self> {
        match ident {
            "bool" => Some(Self::Bool),
            "i8" => Some(Self::I8),
            "i16" => Some(Self::I16),
            "i32" => Some(Self::I32),
            "i64" => Some(Self::I64),
            "i128" => Some(Self::I128),
            "u8" => Some(Self::U8),
            "u16" => Some(Self::U16),
            "u32" => Some(Self::U32),
            "u64" => Some(Self::U64),
            "u128" => Some(Self::U128),
            "f32" => Some(Self::F32),
            "f64" => Some(Self::F64),
            "char" => Some(Self::Char),
            "String" | "str" => Some(Self::String),
            _ => None,
        }
    }

    /// Returns the TypeKind variant name (e.g., "Bool", "I32", "String").
    pub const fn variant_name(self) -> &'static str {
        match self {
            Self::Bool => "Bool",
            Self::I8 => "I8",
            Self::I16 => "I16",
            Self::I32 => "I32",
            Self::I64 => "I64",
            Self::I128 => "I128",
            Self::U8 => "U8",
            Self::U16 => "U16",
            Self::U32 => "U32",
            Self::U64 => "U64",
            Self::U128 => "U128",
            Self::F32 => "F32",
            Self::F64 => "F64",
            Self::Char => "Char",
            Self::String => "String",
        }
    }

    /// Convert to the corresponding `ron_schema::TypeKind` value.
    pub fn to_type_kind(self) -> ron_schema::TypeKind {
        match self {
            Self::Bool => ron_schema::TypeKind::Bool,
            Self::I8 => ron_schema::TypeKind::I8,
            Self::I16 => ron_schema::TypeKind::I16,
            Self::I32 => ron_schema::TypeKind::I32,
            Self::I64 => ron_schema::TypeKind::I64,
            Self::I128 => ron_schema::TypeKind::I128,
            Self::U8 => ron_schema::TypeKind::U8,
            Self::U16 => ron_schema::TypeKind::U16,
            Self::U32 => ron_schema::TypeKind::U32,
            Self::U64 => ron_schema::TypeKind::U64,
            Self::U128 => ron_schema::TypeKind::U128,
            Self::F32 => ron_schema::TypeKind::F32,
            Self::F64 => ron_schema::TypeKind::F64,
            Self::Char => ron_schema::TypeKind::Char,
            Self::String => ron_schema::TypeKind::String,
        }
    }
}

/// Trait for mapping Rust types to TypeKind representations.
///
/// This abstraction allows sharing the type analysis logic between:
/// - Code generation (returns TokenStream2 that constructs TypeKind at runtime)
/// - Compile-time building (returns TypeKind values directly)
pub trait TypeKindMapper {
    type Output;

    fn unit(&self) -> Self::Output;
    fn primitive(&self, kind: PrimitiveKind) -> Self::Output;
    fn option(&self, inner: Self::Output) -> Self::Output;
    fn list(&self, inner: Self::Output) -> Self::Output;
    fn map(&self, key: Self::Output, value: Self::Output) -> Self::Output;
    fn tuple(&self, elements: Vec<Self::Output>) -> Self::Output;
    fn type_ref(&self, path: String) -> Self::Output;
}

/// Mapper that generates TokenStream2 for runtime TypeKind construction.
pub struct TokenMapper;

impl TypeKindMapper for TokenMapper {
    type Output = TokenStream2;

    fn unit(&self) -> Self::Output {
        quote! { ::ron_schema::TypeKind::Unit }
    }

    fn primitive(&self, kind: PrimitiveKind) -> Self::Output {
        let variant = syn::Ident::new(kind.variant_name(), proc_macro2::Span::call_site());
        quote! { ::ron_schema::TypeKind::#variant }
    }

    fn option(&self, inner: Self::Output) -> Self::Output {
        quote! { ::ron_schema::TypeKind::Option(Box::new(#inner)) }
    }

    fn list(&self, inner: Self::Output) -> Self::Output {
        quote! { ::ron_schema::TypeKind::List(Box::new(#inner)) }
    }

    fn map(&self, key: Self::Output, value: Self::Output) -> Self::Output {
        quote! {
            ::ron_schema::TypeKind::Map {
                key: Box::new(#key),
                value: Box::new(#value),
            }
        }
    }

    fn tuple(&self, elements: Vec<Self::Output>) -> Self::Output {
        quote! { ::ron_schema::TypeKind::Tuple(vec![#(#elements),*]) }
    }

    fn type_ref(&self, path: String) -> Self::Output {
        quote! { ::ron_schema::TypeKind::TypeRef(#path.to_string()) }
    }
}

/// Mapper that builds TypeKind values directly at compile time.
pub struct ValueMapper;

impl TypeKindMapper for ValueMapper {
    type Output = ron_schema::TypeKind;

    fn unit(&self) -> Self::Output {
        ron_schema::TypeKind::Unit
    }

    fn primitive(&self, kind: PrimitiveKind) -> Self::Output {
        kind.to_type_kind()
    }

    fn option(&self, inner: Self::Output) -> Self::Output {
        ron_schema::TypeKind::Option(Box::new(inner))
    }

    fn list(&self, inner: Self::Output) -> Self::Output {
        ron_schema::TypeKind::List(Box::new(inner))
    }

    fn map(&self, key: Self::Output, value: Self::Output) -> Self::Output {
        ron_schema::TypeKind::Map {
            key: Box::new(key),
            value: Box::new(value),
        }
    }

    fn tuple(&self, elements: Vec<Self::Output>) -> Self::Output {
        ron_schema::TypeKind::Tuple(elements)
    }

    fn type_ref(&self, path: String) -> Self::Output {
        ron_schema::TypeKind::TypeRef(path)
    }
}

/// Map a Rust type to its TypeKind representation using the given mapper.
pub fn map_type<M: TypeKindMapper>(ty: &Type, mapper: &M) -> M::Output {
    match ty {
        Type::Path(type_path) => {
            let path = &type_path.path;

            if let Some(segment) = path.segments.last() {
                let ident_str = segment.ident.to_string();

                // Check for primitives
                if let Some(prim) = PrimitiveKind::from_ident(&ident_str) {
                    return mapper.primitive(prim);
                }

                // Check for generic types (Option, Vec, HashMap, etc.)
                if let PathArguments::AngleBracketed(args) = &segment.arguments {
                    let generic_args: Vec<_> = args
                        .args
                        .iter()
                        .filter_map(|arg| {
                            if let GenericArgument::Type(t) = arg {
                                Some(t)
                            } else {
                                None
                            }
                        })
                        .collect();

                    match ident_str.as_str() {
                        "Option" if generic_args.len() == 1 => {
                            let inner = map_type(generic_args[0], mapper);
                            return mapper.option(inner);
                        }
                        "Vec" | "VecDeque" | "HashSet" | "BTreeSet" | "LinkedList"
                            if generic_args.len() == 1 =>
                        {
                            let inner = map_type(generic_args[0], mapper);
                            return mapper.list(inner);
                        }
                        "HashMap" | "BTreeMap" if generic_args.len() == 2 => {
                            let key = map_type(generic_args[0], mapper);
                            let value = map_type(generic_args[1], mapper);
                            return mapper.map(key, value);
                        }
                        "Box" if generic_args.len() == 1 => {
                            // Box<T> is treated as just T for schema purposes
                            return map_type(generic_args[0], mapper);
                        }
                        _ => {}
                    }
                }

                // For any other type, generate a TypeRef with the full path
                let type_path_str = path_to_string(path);
                return mapper.type_ref(type_path_str);
            }

            // Fallback for invalid paths (shouldn't happen in practice)
            mapper.type_ref(quote!(#ty).to_string())
        }
        Type::Tuple(tuple) => {
            if tuple.elems.is_empty() {
                return mapper.unit();
            }

            let elements: Vec<_> = tuple.elems.iter().map(|t| map_type(t, mapper)).collect();
            mapper.tuple(elements)
        }
        Type::Reference(reference) => {
            // For references, use the underlying type
            map_type(&reference.elem, mapper)
        }
        Type::Array(array) => {
            // Treat arrays as List for schema purposes
            let inner = map_type(&array.elem, mapper);
            mapper.list(inner)
        }
        Type::Slice(slice) => {
            // Treat slices as List for schema purposes
            let inner = map_type(&slice.elem, mapper);
            mapper.list(inner)
        }
        _ => {
            // For other types, generate a TypeRef with the type as-is
            let type_str = quote!(#ty).to_string();
            mapper.type_ref(type_str)
        }
    }
}

/// Convert a Rust type to TypeKind tokens (for code generation).
pub fn type_to_type_kind(ty: &Type) -> TokenStream2 {
    map_type(ty, &TokenMapper)
}

/// Convert a Rust type to a TypeKind value (for compile-time building).
pub fn rust_type_to_type_kind(ty: &Type) -> ron_schema::TypeKind {
    map_type(ty, &ValueMapper)
}
