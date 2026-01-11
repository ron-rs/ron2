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

use crate::schema::StorageError;
use crate::schema::{Schema, SchemaError, TypeKind};
use std::path::PathBuf;

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
    fn type_doc() -> Option<&'static str> {
        None
    }

    /// Returns the complete schema for this type, including documentation.
    ///
    /// The default implementation constructs a schema from `type_kind()` and `type_doc()`.
    /// Types using `#[derive(RonSchema)]` will override this with a more complete
    /// implementation that includes field-level documentation.
    fn schema() -> Schema {
        Schema {
            doc: Self::type_doc().map(|s| s.to_string()),
            kind: Self::type_kind(),
        }
    }

    /// Returns the fully-qualified type path for this type.
    ///
    /// This is used to locate schema files and for `TypeRef` references.
    /// Returns `None` for primitive types and standard library types.
    fn type_path() -> Option<&'static str> {
        None
    }

    /// Writes the schema to the specified output directory.
    ///
    /// If `output_dir` is `None`, the schema is written to the default location
    /// determined by `RON_SCHEMA_DIR` env var or XDG data directory.
    ///
    /// Returns the path to the written schema file, or `None` if this type
    /// doesn't support schema storage (e.g., primitive types).
    fn write_schema(output_dir: Option<&std::path::Path>) -> Result<PathBuf, SchemaError> {
        let type_path = Self::type_path().ok_or_else(|| {
            SchemaError::Storage(StorageError::Io(
                "type does not support schema storage".to_string(),
            ))
        })?;
        let schema = Self::schema();
        super::write_schema(type_path, &schema, output_dir)
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
    fn key_type_kind() -> TypeKind {
        Self::Key::type_kind()
    }

    /// Returns the `TypeKind` of the value type.
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
    fn inner_type_kind() -> TypeKind {
        Self::Inner::type_kind()
    }
}

// =============================================================================
// Standard Library Implementations
// =============================================================================

// Primitives

impl RonSchemaType for bool {
    fn type_kind() -> TypeKind {
        TypeKind::Bool
    }
}

impl RonSchemaType for i8 {
    fn type_kind() -> TypeKind {
        TypeKind::I8
    }
}

impl RonSchemaType for i16 {
    fn type_kind() -> TypeKind {
        TypeKind::I16
    }
}

impl RonSchemaType for i32 {
    fn type_kind() -> TypeKind {
        TypeKind::I32
    }
}

impl RonSchemaType for i64 {
    fn type_kind() -> TypeKind {
        TypeKind::I64
    }
}

impl RonSchemaType for i128 {
    fn type_kind() -> TypeKind {
        TypeKind::I128
    }
}

impl RonSchemaType for isize {
    fn type_kind() -> TypeKind {
        // isize is platform-dependent, but we'll represent it as I64 for schema purposes
        TypeKind::I64
    }
}

impl RonSchemaType for u8 {
    fn type_kind() -> TypeKind {
        TypeKind::U8
    }
}

impl RonSchemaType for u16 {
    fn type_kind() -> TypeKind {
        TypeKind::U16
    }
}

impl RonSchemaType for u32 {
    fn type_kind() -> TypeKind {
        TypeKind::U32
    }
}

impl RonSchemaType for u64 {
    fn type_kind() -> TypeKind {
        TypeKind::U64
    }
}

impl RonSchemaType for u128 {
    fn type_kind() -> TypeKind {
        TypeKind::U128
    }
}

impl RonSchemaType for usize {
    fn type_kind() -> TypeKind {
        // usize is platform-dependent, but we'll represent it as U64 for schema purposes
        TypeKind::U64
    }
}

impl RonSchemaType for f32 {
    fn type_kind() -> TypeKind {
        TypeKind::F32
    }
}

impl RonSchemaType for f64 {
    fn type_kind() -> TypeKind {
        TypeKind::F64
    }
}

impl RonSchemaType for char {
    fn type_kind() -> TypeKind {
        TypeKind::Char
    }
}

impl RonSchemaType for String {
    fn type_kind() -> TypeKind {
        TypeKind::String
    }
}

impl RonSchemaType for &str {
    fn type_kind() -> TypeKind {
        TypeKind::String
    }
}

impl RonSchemaType for () {
    fn type_kind() -> TypeKind {
        TypeKind::Unit
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

// Vec<T>

impl<T: RonSchemaType> RonSchemaType for Vec<T> {
    fn type_kind() -> TypeKind {
        TypeKind::List(Box::new(T::type_kind()))
    }
}

impl<T: RonSchemaType> RonList for Vec<T> {
    type Element = T;
}

// Slices and arrays

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

impl<K: RonSchemaType, V: RonSchemaType> RonSchemaType for std::collections::HashMap<K, V> {
    fn type_kind() -> TypeKind {
        TypeKind::Map {
            key: Box::new(K::type_kind()),
            value: Box::new(V::type_kind()),
        }
    }
}

impl<K: RonSchemaType, V: RonSchemaType> RonMap for std::collections::HashMap<K, V> {
    type Key = K;
    type Value = V;
}

impl<K: RonSchemaType, V: RonSchemaType> RonSchemaType for std::collections::BTreeMap<K, V> {
    fn type_kind() -> TypeKind {
        TypeKind::Map {
            key: Box::new(K::type_kind()),
            value: Box::new(V::type_kind()),
        }
    }
}

impl<K: RonSchemaType, V: RonSchemaType> RonMap for std::collections::BTreeMap<K, V> {
    type Key = K;
    type Value = V;
}

// HashSet and BTreeSet (as lists)

impl<T: RonSchemaType> RonSchemaType for std::collections::HashSet<T> {
    fn type_kind() -> TypeKind {
        TypeKind::List(Box::new(T::type_kind()))
    }
}

impl<T: RonSchemaType> RonList for std::collections::HashSet<T> {
    type Element = T;
}

impl<T: RonSchemaType> RonSchemaType for std::collections::BTreeSet<T> {
    fn type_kind() -> TypeKind {
        TypeKind::List(Box::new(T::type_kind()))
    }
}

impl<T: RonSchemaType> RonList for std::collections::BTreeSet<T> {
    type Element = T;
}

// VecDeque

impl<T: RonSchemaType> RonSchemaType for std::collections::VecDeque<T> {
    fn type_kind() -> TypeKind {
        TypeKind::List(Box::new(T::type_kind()))
    }
}

impl<T: RonSchemaType> RonList for std::collections::VecDeque<T> {
    type Element = T;
}

// LinkedList

impl<T: RonSchemaType> RonSchemaType for std::collections::LinkedList<T> {
    fn type_kind() -> TypeKind {
        TypeKind::List(Box::new(T::type_kind()))
    }
}

impl<T: RonSchemaType> RonList for std::collections::LinkedList<T> {
    type Element = T;
}

// Box<T>

impl<T: RonSchemaType> RonSchemaType for Box<T> {
    fn type_kind() -> TypeKind {
        T::type_kind()
    }
}

// Rc<T> and Arc<T>

impl<T: RonSchemaType> RonSchemaType for std::rc::Rc<T> {
    fn type_kind() -> TypeKind {
        T::type_kind()
    }
}

impl<T: RonSchemaType> RonSchemaType for std::sync::Arc<T> {
    fn type_kind() -> TypeKind {
        T::type_kind()
    }
}

// Cell<T> and RefCell<T>

impl<T: RonSchemaType> RonSchemaType for std::cell::Cell<T> {
    fn type_kind() -> TypeKind {
        T::type_kind()
    }
}

impl<T: RonSchemaType> RonSchemaType for std::cell::RefCell<T> {
    fn type_kind() -> TypeKind {
        T::type_kind()
    }
}

// Mutex<T> and RwLock<T>

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

// Cow<T>

impl<'a, T: RonSchemaType + ToOwned + ?Sized> RonSchemaType for std::borrow::Cow<'a, T> {
    fn type_kind() -> TypeKind {
        T::type_kind()
    }
}

// Tuples (up to 12 elements like serde)

impl<T0: RonSchemaType> RonSchemaType for (T0,) {
    fn type_kind() -> TypeKind {
        TypeKind::Tuple(vec![T0::type_kind()])
    }
}

impl<T0: RonSchemaType, T1: RonSchemaType> RonSchemaType for (T0, T1) {
    fn type_kind() -> TypeKind {
        TypeKind::Tuple(vec![T0::type_kind(), T1::type_kind()])
    }
}

impl<T0: RonSchemaType, T1: RonSchemaType, T2: RonSchemaType> RonSchemaType for (T0, T1, T2) {
    fn type_kind() -> TypeKind {
        TypeKind::Tuple(vec![T0::type_kind(), T1::type_kind(), T2::type_kind()])
    }
}

impl<T0: RonSchemaType, T1: RonSchemaType, T2: RonSchemaType, T3: RonSchemaType> RonSchemaType
    for (T0, T1, T2, T3)
{
    fn type_kind() -> TypeKind {
        TypeKind::Tuple(vec![
            T0::type_kind(),
            T1::type_kind(),
            T2::type_kind(),
            T3::type_kind(),
        ])
    }
}

impl<T0: RonSchemaType, T1: RonSchemaType, T2: RonSchemaType, T3: RonSchemaType, T4: RonSchemaType>
    RonSchemaType for (T0, T1, T2, T3, T4)
{
    fn type_kind() -> TypeKind {
        TypeKind::Tuple(vec![
            T0::type_kind(),
            T1::type_kind(),
            T2::type_kind(),
            T3::type_kind(),
            T4::type_kind(),
        ])
    }
}

impl<
    T0: RonSchemaType,
    T1: RonSchemaType,
    T2: RonSchemaType,
    T3: RonSchemaType,
    T4: RonSchemaType,
    T5: RonSchemaType,
> RonSchemaType for (T0, T1, T2, T3, T4, T5)
{
    fn type_kind() -> TypeKind {
        TypeKind::Tuple(vec![
            T0::type_kind(),
            T1::type_kind(),
            T2::type_kind(),
            T3::type_kind(),
            T4::type_kind(),
            T5::type_kind(),
        ])
    }
}

impl<
    T0: RonSchemaType,
    T1: RonSchemaType,
    T2: RonSchemaType,
    T3: RonSchemaType,
    T4: RonSchemaType,
    T5: RonSchemaType,
    T6: RonSchemaType,
> RonSchemaType for (T0, T1, T2, T3, T4, T5, T6)
{
    fn type_kind() -> TypeKind {
        TypeKind::Tuple(vec![
            T0::type_kind(),
            T1::type_kind(),
            T2::type_kind(),
            T3::type_kind(),
            T4::type_kind(),
            T5::type_kind(),
            T6::type_kind(),
        ])
    }
}

impl<
    T0: RonSchemaType,
    T1: RonSchemaType,
    T2: RonSchemaType,
    T3: RonSchemaType,
    T4: RonSchemaType,
    T5: RonSchemaType,
    T6: RonSchemaType,
    T7: RonSchemaType,
> RonSchemaType for (T0, T1, T2, T3, T4, T5, T6, T7)
{
    fn type_kind() -> TypeKind {
        TypeKind::Tuple(vec![
            T0::type_kind(),
            T1::type_kind(),
            T2::type_kind(),
            T3::type_kind(),
            T4::type_kind(),
            T5::type_kind(),
            T6::type_kind(),
            T7::type_kind(),
        ])
    }
}

impl<
    T0: RonSchemaType,
    T1: RonSchemaType,
    T2: RonSchemaType,
    T3: RonSchemaType,
    T4: RonSchemaType,
    T5: RonSchemaType,
    T6: RonSchemaType,
    T7: RonSchemaType,
    T8: RonSchemaType,
> RonSchemaType for (T0, T1, T2, T3, T4, T5, T6, T7, T8)
{
    fn type_kind() -> TypeKind {
        TypeKind::Tuple(vec![
            T0::type_kind(),
            T1::type_kind(),
            T2::type_kind(),
            T3::type_kind(),
            T4::type_kind(),
            T5::type_kind(),
            T6::type_kind(),
            T7::type_kind(),
            T8::type_kind(),
        ])
    }
}

impl<
    T0: RonSchemaType,
    T1: RonSchemaType,
    T2: RonSchemaType,
    T3: RonSchemaType,
    T4: RonSchemaType,
    T5: RonSchemaType,
    T6: RonSchemaType,
    T7: RonSchemaType,
    T8: RonSchemaType,
    T9: RonSchemaType,
> RonSchemaType for (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9)
{
    fn type_kind() -> TypeKind {
        TypeKind::Tuple(vec![
            T0::type_kind(),
            T1::type_kind(),
            T2::type_kind(),
            T3::type_kind(),
            T4::type_kind(),
            T5::type_kind(),
            T6::type_kind(),
            T7::type_kind(),
            T8::type_kind(),
            T9::type_kind(),
        ])
    }
}

impl<
    T0: RonSchemaType,
    T1: RonSchemaType,
    T2: RonSchemaType,
    T3: RonSchemaType,
    T4: RonSchemaType,
    T5: RonSchemaType,
    T6: RonSchemaType,
    T7: RonSchemaType,
    T8: RonSchemaType,
    T9: RonSchemaType,
    T10: RonSchemaType,
> RonSchemaType for (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)
{
    fn type_kind() -> TypeKind {
        TypeKind::Tuple(vec![
            T0::type_kind(),
            T1::type_kind(),
            T2::type_kind(),
            T3::type_kind(),
            T4::type_kind(),
            T5::type_kind(),
            T6::type_kind(),
            T7::type_kind(),
            T8::type_kind(),
            T9::type_kind(),
            T10::type_kind(),
        ])
    }
}

impl<
    T0: RonSchemaType,
    T1: RonSchemaType,
    T2: RonSchemaType,
    T3: RonSchemaType,
    T4: RonSchemaType,
    T5: RonSchemaType,
    T6: RonSchemaType,
    T7: RonSchemaType,
    T8: RonSchemaType,
    T9: RonSchemaType,
    T10: RonSchemaType,
    T11: RonSchemaType,
> RonSchemaType for (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)
{
    fn type_kind() -> TypeKind {
        TypeKind::Tuple(vec![
            T0::type_kind(),
            T1::type_kind(),
            T2::type_kind(),
            T3::type_kind(),
            T4::type_kind(),
            T5::type_kind(),
            T6::type_kind(),
            T7::type_kind(),
            T8::type_kind(),
            T9::type_kind(),
            T10::type_kind(),
            T11::type_kind(),
        ])
    }
}

// PathBuf and Path

impl RonSchemaType for std::path::PathBuf {
    fn type_kind() -> TypeKind {
        TypeKind::String
    }
}

impl RonSchemaType for std::path::Path {
    fn type_kind() -> TypeKind {
        TypeKind::String
    }
}

// OsString and OsStr

impl RonSchemaType for std::ffi::OsString {
    fn type_kind() -> TypeKind {
        TypeKind::String
    }
}

impl RonSchemaType for std::ffi::OsStr {
    fn type_kind() -> TypeKind {
        TypeKind::String
    }
}

// PhantomData

impl<T: ?Sized> RonSchemaType for std::marker::PhantomData<T> {
    fn type_kind() -> TypeKind {
        TypeKind::Unit
    }
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
}
