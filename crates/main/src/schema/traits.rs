//! Traits for the RON schema system.
//!
//! This module provides traits that types can implement to participate in the
//! schema system. By implementing these traits, custom types can be recognized
//! as lists, maps, or other schema-compatible types.
//!
//! # Example
//!
//! ```rust
//! use crate::schema::{RonSchemaType, RonList, TypeKind};
//!
//! // A custom list type
//! struct MyVec<T>(Vec<T>);
//!
//! impl<T: RonSchemaType> RonSchemaType for MyVec<T> {
//!     fn type_kind() -> TypeKind {
//!         TypeKind::List(Box::new(T::type_kind()))
//!     }
//! }
//!
//! // Mark it as a list type
//! impl<T: RonSchemaType> RonList for MyVec<T> {
//!     type Element = T;
//! }
//! ```

use alloc::{
    borrow::Cow,
    collections::{BTreeMap, BTreeSet, LinkedList, VecDeque},
    rc::Rc,
    sync::Arc,
};
use core::{
    cell::{Cell, RefCell},
    hash::BuildHasher,
    marker::PhantomData,
};
use std::{
    collections::{HashMap, HashSet},
    path::{Path, PathBuf},
};

use crate::schema::{Schema, SchemaEntry, SchemaError, StorageError, TypeKind};

/// Core trait for types that can be represented in the RON schema system.
///
/// This trait is the foundation of the schema system. Types that implement
/// this trait can be used in schemas and will be properly recognized during
/// validation and LSP operations.
///
/// # Implementing for Custom Types
///
/// For most custom structs and enums, use `#[derive(RonSchema)]` which
/// generates a complete implementation including schema storage support.
///
/// For wrapper types or newtypes that should behave like existing types,
/// implement this trait manually:
///
/// ```rust
/// use crate::schema::{RonSchemaType, TypeKind};
///
/// struct UserId(u64);
///
/// impl RonSchemaType for UserId {
///     fn type_kind() -> TypeKind {
///         TypeKind::U64  // UserId serializes as a u64
///     }
/// }
/// ```
pub trait RonSchemaType {
    /// Returns the `TypeKind` that represents this type in the schema system.
    fn type_kind() -> TypeKind;

    /// Returns optional documentation for this type.
    ///
    /// Override this to provide documentation that appears in the schema.
    #[must_use]
    fn type_doc() -> Option<&'static str> {
        None
    }

    /// Returns the complete schema for this type, including documentation.
    ///
    /// The default implementation constructs a schema from `type_kind()` and `type_doc()`.
    /// Types using `#[derive(RonSchema)]` will override this with a more complete
    /// implementation that includes field-level documentation.
    #[must_use]
    fn schema() -> Schema {
        Schema {
            doc: Self::type_doc().map(str::to_string),
            kind: Self::type_kind(),
        }
    }

    /// Returns the fully-qualified type path for this type.
    ///
    /// This is used to locate schema files and for `TypeRef` references.
    /// Returns `None` for primitive types and standard library types.
    #[must_use]
    fn type_path() -> Option<&'static str> {
        None
    }

    /// Returns the direct child schemas referenced by this type.
    ///
    /// This is a non-recursive list; use `collect_schemas` or `write_schemas`
    /// to traverse the full schema graph.
    #[must_use]
    fn child_schemas() -> &'static [&'static SchemaEntry] {
        &[]
    }

    /// Writes the schema to the specified output directory.
    ///
    /// If `output_dir` is `None`, the schema is written to the default location
    /// determined by `RON_SCHEMA_DIR` env var or XDG data directory.
    ///
    /// Returns the path to the written schema file, or `None` if this type
    /// doesn't support schema storage (e.g., primitive types).
    fn write_schema(output_dir: Option<&Path>) -> Result<PathBuf, SchemaError> {
        let type_path = Self::type_path().ok_or_else(|| {
            SchemaError::Storage(StorageError::Io(
                "type does not support schema storage".to_string(),
            ))
        })?;
        let schema = Self::schema();
        super::write_schema(type_path, &schema, output_dir)
    }

    /// Writes this type's schema and all child schemas recursively.
    fn write_schemas(output_dir: Option<&str>) -> Result<Vec<PathBuf>, SchemaError>
    where
        Self: Sized,
    {
        super::write_schemas::<Self>(output_dir)
    }
}

/// Marker trait for types that behave like lists/sequences.
///
/// Implement this trait alongside `RonSchemaType` to mark a type as list-like.
/// This allows the LSP and validation system to treat your custom type as a
/// sequence when providing completions and validating values.
///
/// # Example
///
/// ```rust
/// use crate::schema::{RonSchemaType, RonList, TypeKind};
///
/// struct SortedVec<T>(Vec<T>);
///
/// impl<T: RonSchemaType> RonSchemaType for SortedVec<T> {
///     fn type_kind() -> TypeKind {
///         TypeKind::List(Box::new(T::type_kind()))
///     }
/// }
///
/// impl<T: RonSchemaType> RonList for SortedVec<T> {
///     type Element = T;
/// }
/// ```
pub trait RonList: RonSchemaType {
    /// The element type of this list.
    type Element: RonSchemaType;

    /// Returns the `TypeKind` of the element type.
    #[must_use]
    fn element_type_kind() -> TypeKind {
        Self::Element::type_kind()
    }
}

/// Marker trait for types that behave like maps/dictionaries.
///
/// Implement this trait alongside `RonSchemaType` to mark a type as map-like.
/// This allows the LSP and validation system to treat your custom type as a
/// map when providing completions and validating values.
///
/// # Example
///
/// ```rust
/// use crate::schema::{RonSchemaType, RonMap, TypeKind};
/// use std::collections::BTreeMap;
///
/// struct OrderedMap<K, V>(BTreeMap<K, V>);
///
/// impl<K: RonSchemaType, V: RonSchemaType> RonSchemaType for OrderedMap<K, V> {
///     fn type_kind() -> TypeKind {
///         TypeKind::Map {
///             key: Box::new(K::type_kind()),
///             value: Box::new(V::type_kind()),
///         }
///     }
/// }
///
/// impl<K: RonSchemaType, V: RonSchemaType> RonMap for OrderedMap<K, V> {
///     type Key = K;
///     type Value = V;
/// }
/// ```
pub trait RonMap: RonSchemaType {
    /// The key type of this map.
    type Key: RonSchemaType;
    /// The value type of this map.
    type Value: RonSchemaType;

    /// Returns the `TypeKind` of the key type.
    #[must_use]
    fn key_type_kind() -> TypeKind {
        Self::Key::type_kind()
    }

    /// Returns the `TypeKind` of the value type.
    #[must_use]
    fn value_type_kind() -> TypeKind {
        Self::Value::type_kind()
    }
}

/// Marker trait for types that behave like optional values.
///
/// Implement this trait alongside `RonSchemaType` to mark a type as optional-like.
/// This is primarily used for `Option<T>` but can be implemented for custom
/// nullable/optional wrapper types.
pub trait RonOptional: RonSchemaType {
    /// The inner type when the optional contains a value.
    type Inner: RonSchemaType;

    /// Returns the `TypeKind` of the inner type.
    #[must_use]
    fn inner_type_kind() -> TypeKind {
        Self::Inner::type_kind()
    }
}

// =============================================================================
// Macro Definitions for Repetitive Implementations
// =============================================================================

/// Implements `RonSchemaType` for primitive types with a direct `TypeKind` mapping.
macro_rules! impl_primitive_schema {
    ($($ty:ty => $variant:ident),* $(,)?) => {
        $(
            impl RonSchemaType for $ty {
                fn type_kind() -> TypeKind {
                    TypeKind::$variant
                }
            }
        )*
    };
}

/// Implements `RonSchemaType` for transparent wrapper types that delegate to their inner type.
macro_rules! impl_transparent_schema {
    ($($ty:ident),* $(,)?) => {
        $(
            impl<T: RonSchemaType> RonSchemaType for $ty<T> {
                fn type_kind() -> TypeKind {
                    T::type_kind()
                }
            }
        )*
    };
}

/// Implements `RonSchemaType` and `RonList` for list-like collection types.
macro_rules! impl_list_schema {
    ($($ty:ty),* $(,)?) => {
        $(
            impl<T: RonSchemaType> RonSchemaType for $ty {
                fn type_kind() -> TypeKind {
                    TypeKind::List(Box::new(T::type_kind()))
                }
            }

            impl<T: RonSchemaType> RonList for $ty {
                type Element = T;
            }
        )*
    };
}

/// Implements `RonSchemaType` for tuple types.
macro_rules! impl_tuple_schema {
    ($($T:ident),+) => {
        impl<$($T: RonSchemaType),+> RonSchemaType for ($($T,)+) {
            fn type_kind() -> TypeKind {
                TypeKind::Tuple(alloc::vec![$($T::type_kind()),+])
            }
        }
    };
}

/// Recursively generates tuple implementations from 1-tuple to N-tuple.
macro_rules! impl_tuple_schema_all {
    () => {};
    ($first:ident $(, $rest:ident)*) => {
        impl_tuple_schema!($first $(, $rest)*);
        impl_tuple_schema_all!($($rest),*);
    };
}

// =============================================================================
// Standard Library Implementations
// =============================================================================

// Primitives

impl_primitive_schema! {
    bool => Bool,
    i8 => I8, i16 => I16, i32 => I32, i64 => I64, i128 => I128, isize => I64,
    u8 => U8, u16 => U16, u32 => U32, u64 => U64, u128 => U128, usize => U64,
    f32 => F32, f64 => F64,
    char => Char,
    String => String,
    () => Unit,
    std::path::PathBuf => String,
    std::path::Path => String,
    std::ffi::OsString => String,
    std::ffi::OsStr => String,
}

// &str is a reference type, not handled by the macro
impl RonSchemaType for &str {
    fn type_kind() -> TypeKind {
        TypeKind::String
    }
}

// Option<T>

impl<T: RonSchemaType> RonSchemaType for Option<T> {
    fn type_kind() -> TypeKind {
        TypeKind::Option(Box::new(T::type_kind()))
    }
}

impl<T: RonSchemaType> RonOptional for Option<T> {
    type Inner = T;
}

// List-like collections

impl_list_schema! { Vec<T>, VecDeque<T>, LinkedList<T>, BTreeSet<T> }

// Slices and arrays (special cases with unsized/const generic)

impl<T: RonSchemaType> RonSchemaType for [T] {
    fn type_kind() -> TypeKind {
        TypeKind::List(Box::new(T::type_kind()))
    }
}

impl<T: RonSchemaType, const N: usize> RonSchemaType for [T; N] {
    fn type_kind() -> TypeKind {
        // Arrays are represented as lists with a known length
        // For now, we just use List; we could add a dedicated Array variant
        TypeKind::List(Box::new(T::type_kind()))
    }
}

impl<T: RonSchemaType, const N: usize> RonList for [T; N] {
    type Element = T;
}

// HashMap and BTreeMap

impl<K: RonSchemaType, V: RonSchemaType, S: BuildHasher> RonSchemaType for HashMap<K, V, S> {
    fn type_kind() -> TypeKind {
        TypeKind::Map {
            key: Box::new(K::type_kind()),
            value: Box::new(V::type_kind()),
        }
    }
}

impl<K: RonSchemaType, V: RonSchemaType, S: BuildHasher> RonMap for HashMap<K, V, S> {
    type Key = K;
    type Value = V;
}

impl<K: RonSchemaType, V: RonSchemaType> RonSchemaType for BTreeMap<K, V> {
    fn type_kind() -> TypeKind {
        TypeKind::Map {
            key: Box::new(K::type_kind()),
            value: Box::new(V::type_kind()),
        }
    }
}

impl<K: RonSchemaType, V: RonSchemaType> RonMap for BTreeMap<K, V> {
    type Key = K;
    type Value = V;
}

// HashSet (special case with BuildHasher bound)

impl<T: RonSchemaType, S: BuildHasher> RonSchemaType for HashSet<T, S> {
    fn type_kind() -> TypeKind {
        TypeKind::List(Box::new(T::type_kind()))
    }
}

impl<T: RonSchemaType, S: BuildHasher> RonList for HashSet<T, S> {
    type Element = T;
}

// Transparent wrapper types

impl_transparent_schema! { Box, Rc, Arc, Cell, RefCell }

// Mutex and RwLock (special module paths)

impl<T: RonSchemaType> RonSchemaType for std::sync::Mutex<T> {
    fn type_kind() -> TypeKind {
        T::type_kind()
    }
}

impl<T: RonSchemaType> RonSchemaType for std::sync::RwLock<T> {
    fn type_kind() -> TypeKind {
        T::type_kind()
    }
}

// Cow (special bounds: ToOwned + ?Sized)

impl<T: RonSchemaType + ToOwned + ?Sized> RonSchemaType for Cow<'_, T> {
    fn type_kind() -> TypeKind {
        T::type_kind()
    }
}

// Tuples (up to 12 elements like serde)

impl_tuple_schema_all!(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11);

// PathBuf and Path

// PhantomData (special ?Sized bound)

impl<T: ?Sized> RonSchemaType for PhantomData<T> {
    fn type_kind() -> TypeKind {
        TypeKind::Unit
    }
}

// Spanned<T> - transparent wrapper that captures source spans

impl<T: RonSchemaType> RonSchemaType for crate::convert::Spanned<T> {
    fn type_kind() -> TypeKind {
        // Spanned<T> has the same schema as T (span is deserialization metadata)
        T::type_kind()
    }

    fn type_doc() -> Option<&'static str> {
        // Forward documentation from inner type
        T::type_doc()
    }

    fn schema() -> Schema {
        // Forward complete schema from inner type
        T::schema()
    }

    fn type_path() -> Option<&'static str> {
        // Forward type path from inner type
        T::type_path()
    }
}

// Mark Spanned<T> as optional if T is optional
impl<T: RonOptional> RonOptional for crate::convert::Spanned<T> {
    type Inner = T;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_primitive_type_kinds() {
        assert_eq!(bool::type_kind(), TypeKind::Bool);
        assert_eq!(i32::type_kind(), TypeKind::I32);
        assert_eq!(u64::type_kind(), TypeKind::U64);
        assert_eq!(f64::type_kind(), TypeKind::F64);
        assert_eq!(char::type_kind(), TypeKind::Char);
        assert_eq!(String::type_kind(), TypeKind::String);
        assert_eq!(<()>::type_kind(), TypeKind::Unit);
    }

    #[test]
    fn test_option_type_kind() {
        assert_eq!(
            Option::<i32>::type_kind(),
            TypeKind::Option(Box::new(TypeKind::I32))
        );
    }

    #[test]
    fn test_vec_type_kind() {
        assert_eq!(
            Vec::<String>::type_kind(),
            TypeKind::List(Box::new(TypeKind::String))
        );
    }

    #[test]
    fn test_hashmap_type_kind() {
        assert_eq!(
            std::collections::HashMap::<String, i32>::type_kind(),
            TypeKind::Map {
                key: Box::new(TypeKind::String),
                value: Box::new(TypeKind::I32),
            }
        );
    }

    #[test]
    fn test_tuple_type_kind() {
        assert_eq!(
            <(i32, String)>::type_kind(),
            TypeKind::Tuple(vec![TypeKind::I32, TypeKind::String])
        );
    }

    #[test]
    fn test_box_transparent() {
        assert_eq!(Box::<i32>::type_kind(), TypeKind::I32);
    }

    #[test]
    fn test_custom_list_type() {
        // Simulate a custom list type
        #[allow(dead_code)]
        struct MyVec<T>(Vec<T>);

        impl<T: RonSchemaType> RonSchemaType for MyVec<T> {
            fn type_kind() -> TypeKind {
                TypeKind::List(Box::new(T::type_kind()))
            }
        }

        impl<T: RonSchemaType> RonList for MyVec<T> {
            type Element = T;
        }

        assert_eq!(
            MyVec::<i32>::type_kind(),
            TypeKind::List(Box::new(TypeKind::I32))
        );
        assert_eq!(MyVec::<i32>::element_type_kind(), TypeKind::I32);
    }

    #[test]
    fn test_custom_map_type() {
        // Simulate a custom map type
        #[allow(dead_code)]
        struct MyMap<K, V>(std::collections::HashMap<K, V>);

        impl<K: RonSchemaType, V: RonSchemaType> RonSchemaType for MyMap<K, V> {
            fn type_kind() -> TypeKind {
                TypeKind::Map {
                    key: Box::new(K::type_kind()),
                    value: Box::new(V::type_kind()),
                }
            }
        }

        impl<K: RonSchemaType, V: RonSchemaType> RonMap for MyMap<K, V> {
            type Key = K;
            type Value = V;
        }

        assert_eq!(
            MyMap::<String, i32>::type_kind(),
            TypeKind::Map {
                key: Box::new(TypeKind::String),
                value: Box::new(TypeKind::I32),
            }
        );
        assert_eq!(MyMap::<String, i32>::key_type_kind(), TypeKind::String);
        assert_eq!(MyMap::<String, i32>::value_type_kind(), TypeKind::I32);
    }

    #[test]
    fn test_spanned_schema_is_transparent() {
        use crate::convert::Spanned;

        // Spanned<T> has the same schema as T
        assert_eq!(Spanned::<i32>::type_kind(), i32::type_kind());
        assert_eq!(Spanned::<String>::type_kind(), String::type_kind());
        assert_eq!(
            Spanned::<Option<bool>>::type_kind(),
            Option::<bool>::type_kind()
        );
        assert_eq!(
            Spanned::<Vec<i32>>::type_kind(),
            Vec::<i32>::type_kind()
        );
    }

    #[test]
    fn test_spanned_nested_transparency() {
        use crate::convert::Spanned;

        // Nested Spanned<T> still has the same schema as T
        assert_eq!(
            Spanned::<Spanned<i32>>::type_kind(),
            i32::type_kind()
        );
    }

    #[test]
    fn test_spanned_option_combinations() {
        use crate::convert::Spanned;

        // Both Option<Spanned<T>> and Spanned<Option<T>> have Option<T> schema
        assert_eq!(
            Option::<Spanned<i32>>::type_kind(),
            Option::<i32>::type_kind()
        );
        assert_eq!(
            Spanned::<Option<i32>>::type_kind(),
            Option::<i32>::type_kind()
        );
    }
}
