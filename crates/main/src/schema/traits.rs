//! The [`RonSchema`] trait and its implementations for standard library types.

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
use std::collections::{HashMap, HashSet};
#[cfg(feature = "derive")]
use std::path::{Path, PathBuf};

#[cfg(feature = "derive")]
use crate::schema::SchemaError;
use crate::schema::{Schema, TypeKind, collect::SchemaEntry, error::StorageError};

/// Core trait for types that can be represented in the RON schema system.
///
/// For most custom structs and enums, use `#[derive(RonSchema)]` which
/// generates a complete implementation including schema storage support.
///
/// For wrapper types or newtypes that should behave like existing types,
/// implement this trait manually:
///
/// ```rust
/// use ron2::schema::{RonSchema, TypeKind};
///
/// struct UserId(u64);
///
/// impl RonSchema for UserId {
///     fn type_kind() -> TypeKind {
///         TypeKind::U64 // UserId serializes as a u64
///     }
/// }
///
/// assert_eq!(UserId::type_kind(), TypeKind::U64);
/// ```
pub trait RonSchema {
    /// Returns the `TypeKind` that represents this type in the schema system.
    fn type_kind() -> TypeKind;

    /// Returns optional documentation for this type.
    #[must_use]
    fn type_doc() -> Option<&'static str> {
        None
    }

    /// Returns the complete schema for this type, including documentation.
    ///
    /// The default implementation constructs a schema from `type_kind()` and
    /// `type_doc()`. `#[derive(RonSchema)]` overrides it with field-level
    /// documentation and type metadata.
    #[must_use]
    fn schema() -> Schema {
        Schema {
            doc: Self::type_doc().map(str::to_string),
            ..Schema::new(Self::type_kind())
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
    /// Returns the path to the written schema file, or
    /// [`StorageError::NoTypePath`] if this type has no schema file of its own
    /// (e.g., primitive types).
    #[cfg(feature = "derive")]
    fn write_schema(output_dir: Option<&Path>) -> Result<PathBuf, SchemaError> {
        super::write_schema(require_type_path::<Self>()?, &Self::schema(), output_dir)
    }

    /// Writes this type's schema and all child schemas recursively.
    #[cfg(feature = "derive")]
    fn write_schemas(output_dir: Option<&str>) -> Result<Vec<PathBuf>, SchemaError>
    where
        Self: Sized,
    {
        super::write_schemas::<Self>(output_dir)
    }
}

/// The type path of `T`, or [`StorageError::NoTypePath`].
pub(crate) fn require_type_path<T: RonSchema + ?Sized>() -> Result<&'static str, StorageError> {
    T::type_path().ok_or(StorageError::NoTypePath)
}

/// Implements `RonSchema` for primitive types with a direct `TypeKind` mapping.
macro_rules! impl_primitive_schema {
    ($($ty:ty => $variant:ident),* $(,)?) => {
        $(
            impl RonSchema for $ty {
                fn type_kind() -> TypeKind {
                    TypeKind::$variant
                }
            }
        )*
    };
}

/// Implements `RonSchema` for wrapper types that have the same schema as `T`.
macro_rules! impl_transparent_schema {
    ($($ty:ty),* $(,)?) => {
        $(
            impl<T: RonSchema> RonSchema for $ty {
                fn type_kind() -> TypeKind {
                    T::type_kind()
                }
            }
        )*
    };
}

/// Implements `RonSchema` for list-like collection types.
macro_rules! impl_list_schema {
    ($($ty:ty),* $(,)?) => {
        $(
            impl<T: RonSchema> RonSchema for $ty {
                fn type_kind() -> TypeKind {
                    TypeKind::List(Box::new(T::type_kind()))
                }
            }
        )*
    };
}

/// Implements `RonSchema` for tuple types.
macro_rules! impl_tuple_schema {
    ($($T:ident),+) => {
        impl<$($T: RonSchema),+> RonSchema for ($($T,)+) {
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

impl RonSchema for &str {
    fn type_kind() -> TypeKind {
        TypeKind::String
    }
}

impl<T: RonSchema> RonSchema for Option<T> {
    fn type_kind() -> TypeKind {
        TypeKind::Option(Box::new(T::type_kind()))
    }
}

impl_list_schema! { Vec<T>, VecDeque<T>, LinkedList<T>, BTreeSet<T>, [T] }

impl<T: RonSchema, const N: usize> RonSchema for [T; N] {
    fn type_kind() -> TypeKind {
        TypeKind::Array {
            element: Box::new(T::type_kind()),
            len: N,
        }
    }
}

impl<T: RonSchema, S: BuildHasher> RonSchema for HashSet<T, S> {
    fn type_kind() -> TypeKind {
        TypeKind::List(Box::new(T::type_kind()))
    }
}

impl<K: RonSchema, V: RonSchema, S: BuildHasher> RonSchema for HashMap<K, V, S> {
    fn type_kind() -> TypeKind {
        TypeKind::Map {
            key: Box::new(K::type_kind()),
            value: Box::new(V::type_kind()),
        }
    }
}

impl<K: RonSchema, V: RonSchema> RonSchema for BTreeMap<K, V> {
    fn type_kind() -> TypeKind {
        TypeKind::Map {
            key: Box::new(K::type_kind()),
            value: Box::new(V::type_kind()),
        }
    }
}

impl_transparent_schema! {
    Box<T>, Rc<T>, Arc<T>, Cell<T>, RefCell<T>, std::sync::Mutex<T>, std::sync::RwLock<T>
}

impl<T: RonSchema + ToOwned + ?Sized> RonSchema for Cow<'_, T> {
    fn type_kind() -> TypeKind {
        T::type_kind()
    }
}

// Tuples (up to 12 elements like serde)
impl_tuple_schema_all!(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11);

impl<T: ?Sized> RonSchema for PhantomData<T> {
    fn type_kind() -> TypeKind {
        TypeKind::Unit
    }
}

/// `Spanned<T>` has exactly the schema of `T`: the span is deserialization
/// metadata, not part of the RON value.
impl<T: RonSchema> RonSchema for crate::convert::Spanned<T> {
    fn type_kind() -> TypeKind {
        T::type_kind()
    }

    fn type_doc() -> Option<&'static str> {
        T::type_doc()
    }

    fn schema() -> Schema {
        T::schema()
    }

    fn type_path() -> Option<&'static str> {
        T::type_path()
    }

    fn child_schemas() -> &'static [&'static SchemaEntry] {
        T::child_schemas()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::convert::Spanned;

    struct Named;

    impl RonSchema for Named {
        fn type_kind() -> TypeKind {
            TypeKind::Struct { fields: vec![] }
        }

        fn type_doc() -> Option<&'static str> {
            Some("docs")
        }

        fn type_path() -> Option<&'static str> {
            Some("my::Named")
        }
    }

    #[test]
    fn spanned_forwards_everything_to_inner_type() {
        assert_eq!(Spanned::<Named>::type_kind(), Named::type_kind());
        assert_eq!(Spanned::<Named>::schema(), Named::schema());
        assert_eq!(Spanned::<Named>::type_path(), Some("my::Named"));
        assert_eq!(Spanned::<Named>::type_doc(), Some("docs"));
        assert_eq!(
            Spanned::<Spanned<Option<Named>>>::type_kind(),
            Option::<Named>::type_kind()
        );
    }

    #[test]
    fn default_schema_uses_type_doc() {
        assert_eq!(Named::schema().doc.as_deref(), Some("docs"));
    }

    #[test]
    fn std_types_have_no_type_path() {
        assert!(matches!(
            require_type_path::<Vec<u8>>(),
            Err(StorageError::NoTypePath)
        ));
        assert_eq!(require_type_path::<Named>().ok(), Some("my::Named"));
    }
}
