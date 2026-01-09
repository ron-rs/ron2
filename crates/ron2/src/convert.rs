//! Type conversion traits for RON.
//!
//! This module provides [`ToRon`] and [`FromRon`] traits for converting
//! between Rust types and RON [`Value`]s, without requiring serde.
//!
//! # Example
//!
//! ```
//! use ron2::{ToRon, FromRon, Value};
//!
//! // Serialize to RON
//! let numbers = vec![1, 2, 3];
//! let ron_string = numbers.to_ron().unwrap();
//! assert_eq!(ron_string, "[1,2,3]");
//!
//! // Deserialize from RON
//! let parsed: Vec<i32> = Vec::from_ron("[1, 2, 3]").unwrap();
//! assert_eq!(parsed, vec![1, 2, 3]);
//! ```

use alloc::{
    borrow::Cow,
    boxed::Box,
    collections::{BTreeMap, BTreeSet, LinkedList, VecDeque},
    rc::Rc,
    string::String,
    sync::Arc,
    vec,
    vec::Vec,
};
#[cfg(feature = "std")]
use core::hash::BuildHasher;
use core::{
    cell::{Cell, RefCell},
    hash::Hash,
};

#[cfg(feature = "std")]
use std::{
    collections::{HashMap, HashSet},
    io::Read,
};

use crate::{
    Value,
    error::{Error, Result},
    ser::PrettyConfig,
    value::{Map, Number},
};

/// Trait for types that can be converted to RON.
///
/// # Example
///
/// ```
/// use ron2::ToRon;
///
/// let value = vec![1, 2, 3];
/// let ron_string = value.to_ron().unwrap();
/// assert_eq!(ron_string, "[1,2,3]");
/// ```
pub trait ToRon {
    /// Convert this value to a RON [`Value`].
    fn to_ron_value(&self) -> Result<Value>;

    /// Convert this value to a compact RON string.
    fn to_ron(&self) -> Result<String> {
        crate::to_string(&self.to_ron_value()?)
    }

    /// Convert this value to a pretty-printed RON string.
    fn to_ron_pretty(&self, config: &PrettyConfig) -> Result<String> {
        crate::to_string_pretty(&self.to_ron_value()?, config.clone())
    }
}

/// Trait for types that can be constructed from RON.
///
/// # Example
///
/// ```
/// use ron2::FromRon;
///
/// let values: Vec<i32> = Vec::from_ron("[1, 2, 3]").unwrap();
/// assert_eq!(values, vec![1, 2, 3]);
/// ```
pub trait FromRon: Sized {
    /// Construct this type from a RON [`Value`].
    fn from_ron_value(value: Value) -> Result<Self>;

    /// Parse a RON string and construct this type.
    fn from_ron(s: &str) -> Result<Self> {
        let value = crate::from_str(s)?;
        Self::from_ron_value(value)
    }

    /// Read RON from a reader and construct this type.
    #[cfg(feature = "std")]
    fn from_ron_reader<R: Read>(mut reader: R) -> Result<Self> {
        let mut buf = String::new();
        reader.read_to_string(&mut buf)?;
        Self::from_ron(&buf)
    }
}

// =============================================================================
// Error helpers
// =============================================================================

impl Error {
    /// Create a type mismatch error.
    #[must_use]
    pub fn type_mismatch(expected: &str, found: &Value) -> Self {
        Error::InvalidValueForType {
            expected: expected.into(),
            found: value_type_name(found).into(),
        }
    }

    /// Create a missing field error.
    #[must_use]
    pub fn missing_field(field: &str) -> Self {
        Error::Message(alloc::format!("missing required field: {field}"))
    }

    /// Create an unknown field error.
    #[must_use]
    pub fn unknown_field(field: &str) -> Self {
        Error::Message(alloc::format!("unknown field: {field}"))
    }

    /// Create an integer out of range error.
    #[must_use]
    pub fn integer_out_of_range(ty: &str, value: &str) -> Self {
        Error::Message(alloc::format!("integer {value} out of range for type {ty}"))
    }

    /// Create an invalid value error.
    #[must_use]
    pub fn invalid_value(msg: impl Into<String>) -> Self {
        Error::Message(msg.into())
    }
}

/// Get a human-readable type name for a Value.
fn value_type_name(value: &Value) -> &'static str {
    match value {
        Value::Bool(_) => "bool",
        Value::Char(_) => "char",
        Value::Number(n) => match n {
            Number::I8(_) => "i8",
            Number::I16(_) => "i16",
            Number::I32(_) => "i32",
            Number::I64(_) => "i64",
            #[cfg(feature = "integer128")]
            Number::I128(_) => "i128",
            Number::U8(_) => "u8",
            Number::U16(_) => "u16",
            Number::U32(_) => "u32",
            Number::U64(_) => "u64",
            #[cfg(feature = "integer128")]
            Number::U128(_) => "u128",
            Number::F32(_) => "f32",
            Number::F64(_) => "f64",
            // Handle non-exhaustive variant
            _ => "number",
        },
        Value::String(_) => "string",
        Value::Bytes(_) => "bytes",
        Value::Option(_) => "option",
        Value::Seq(_) => "sequence",
        Value::Tuple(_) => "tuple",
        Value::Map(_) => "map",
        Value::Struct(_) => "struct",
        Value::Named { .. } => "named",
        Value::Unit => "unit",
    }
}

// =============================================================================
// Primitive implementations - ToRon
// =============================================================================

impl ToRon for bool {
    fn to_ron_value(&self) -> Result<Value> {
        Ok(Value::Bool(*self))
    }
}

impl ToRon for char {
    fn to_ron_value(&self) -> Result<Value> {
        Ok(Value::Char(*self))
    }
}

impl ToRon for String {
    fn to_ron_value(&self) -> Result<Value> {
        Ok(Value::String(self.clone()))
    }
}

impl ToRon for str {
    fn to_ron_value(&self) -> Result<Value> {
        Ok(Value::String(self.into()))
    }
}

impl ToRon for () {
    fn to_ron_value(&self) -> Result<Value> {
        Ok(Value::Unit)
    }
}

// Integer types
macro_rules! impl_to_ron_int {
    ($($ty:ty => $variant:ident),+ $(,)?) => {
        $(
            impl ToRon for $ty {
                fn to_ron_value(&self) -> Result<Value> {
                    Ok(Value::Number(Number::$variant(*self)))
                }
            }
        )+
    };
}

impl_to_ron_int! {
    i8 => I8,
    i16 => I16,
    i32 => I32,
    i64 => I64,
    u8 => U8,
    u16 => U16,
    u32 => U32,
    u64 => U64,
}

#[cfg(feature = "integer128")]
impl_to_ron_int! {
    i128 => I128,
    u128 => U128,
}

// Float types
impl ToRon for f32 {
    fn to_ron_value(&self) -> Result<Value> {
        Ok(Value::Number(Number::F32((*self).into())))
    }
}

impl ToRon for f64 {
    fn to_ron_value(&self) -> Result<Value> {
        Ok(Value::Number(Number::F64((*self).into())))
    }
}

// =============================================================================
// Primitive implementations - FromRon
// =============================================================================

impl FromRon for bool {
    fn from_ron_value(value: Value) -> Result<Self> {
        match value {
            Value::Bool(b) => Ok(b),
            other => Err(Error::type_mismatch("bool", &other)),
        }
    }
}

impl FromRon for char {
    fn from_ron_value(value: Value) -> Result<Self> {
        match value {
            Value::Char(c) => Ok(c),
            other => Err(Error::type_mismatch("char", &other)),
        }
    }
}

impl FromRon for String {
    fn from_ron_value(value: Value) -> Result<Self> {
        match value {
            Value::String(s) => Ok(s),
            other => Err(Error::type_mismatch("String", &other)),
        }
    }
}

impl FromRon for () {
    fn from_ron_value(value: Value) -> Result<Self> {
        match value {
            Value::Unit => Ok(()),
            other => Err(Error::type_mismatch("()", &other)),
        }
    }
}

// Helper trait for extracting numbers with range checking
trait FromNumber: Sized {
    fn from_number(n: &Number) -> Option<Self>;
    fn type_name() -> &'static str;
}

macro_rules! impl_from_number_signed {
    ($($ty:ty),+ $(,)?) => {
        $(
            impl FromNumber for $ty {
                fn from_number(n: &Number) -> Option<Self> {
                    let val = match n {
                        Number::I8(v) => i128::from(*v),
                        Number::I16(v) => i128::from(*v),
                        Number::I32(v) => i128::from(*v),
                        Number::I64(v) => i128::from(*v),
                        #[cfg(feature = "integer128")]
                        Number::I128(v) => *v,
                        Number::U8(v) => i128::from(*v),
                        Number::U16(v) => i128::from(*v),
                        Number::U32(v) => i128::from(*v),
                        Number::U64(v) => i128::from(*v),
                        #[cfg(feature = "integer128")]
                        Number::U128(v) => i128::try_from(*v).ok()?,
                        _ => return None,
                    };
                    val.try_into().ok()
                }
                fn type_name() -> &'static str { stringify!($ty) }
            }
        )+
    };
}

macro_rules! impl_from_number_unsigned {
    ($($ty:ty),+ $(,)?) => {
        $(
            impl FromNumber for $ty {
                fn from_number(n: &Number) -> Option<Self> {
                    let val = match n {
                        Number::I8(v) if *v >= 0 => u128::try_from(*v).ok()?,
                        Number::I16(v) if *v >= 0 => u128::try_from(*v).ok()?,
                        Number::I32(v) if *v >= 0 => u128::try_from(*v).ok()?,
                        Number::I64(v) if *v >= 0 => u128::try_from(*v).ok()?,
                        #[cfg(feature = "integer128")]
                        Number::I128(v) if *v >= 0 => u128::try_from(*v).ok()?,
                        Number::U8(v) => u128::from(*v),
                        Number::U16(v) => u128::from(*v),
                        Number::U32(v) => u128::from(*v),
                        Number::U64(v) => u128::from(*v),
                        #[cfg(feature = "integer128")]
                        Number::U128(v) => *v,
                        _ => return None,
                    };
                    val.try_into().ok()
                }
                fn type_name() -> &'static str { stringify!($ty) }
            }
        )+
    };
}

impl_from_number_signed!(i8, i16, i32, i64);
impl_from_number_unsigned!(u8, u16, u32, u64);

#[cfg(feature = "integer128")]
impl_from_number_signed!(i128);

#[cfg(feature = "integer128")]
impl_from_number_unsigned!(u128);

macro_rules! impl_from_ron_int {
    ($($ty:ty),+ $(,)?) => {
        $(
            impl FromRon for $ty {
                fn from_ron_value(value: Value) -> Result<Self> {
                    match value {
                        Value::Number(ref n) => {
                            <$ty as FromNumber>::from_number(n).ok_or_else(|| {
                                Error::integer_out_of_range(
                                    <$ty as FromNumber>::type_name(),
                                    &alloc::format!("{n:?}"),
                                )
                            })
                        }
                        other => Err(Error::type_mismatch(<$ty as FromNumber>::type_name(), &other)),
                    }
                }
            }
        )+
    };
}

impl_from_ron_int!(i8, i16, i32, i64, u8, u16, u32, u64);

#[cfg(feature = "integer128")]
impl_from_ron_int!(i128, u128);

impl FromRon for f32 {
    #[allow(clippy::cast_possible_truncation, clippy::cast_precision_loss)]
    fn from_ron_value(value: Value) -> Result<Self> {
        match value {
            Value::Number(Number::F32(f)) => Ok(f.get()),
            Value::Number(Number::F64(f)) => Ok(f.get() as f32),
            Value::Number(n) => {
                // Also accept integers as floats
                let val: f64 = match n {
                    Number::I8(v) => f64::from(v),
                    Number::I16(v) => f64::from(v),
                    Number::I32(v) => f64::from(v),
                    Number::I64(v) => v as f64,
                    #[cfg(feature = "integer128")]
                    Number::I128(v) => v as f64,
                    Number::U8(v) => f64::from(v),
                    Number::U16(v) => f64::from(v),
                    Number::U32(v) => f64::from(v),
                    Number::U64(v) => v as f64,
                    #[cfg(feature = "integer128")]
                    Number::U128(v) => v as f64,
                    _ => return Err(Error::type_mismatch("f32", &Value::Number(n))),
                };
                Ok(val as f32)
            }
            other => Err(Error::type_mismatch("f32", &other)),
        }
    }
}

impl FromRon for f64 {
    #[allow(clippy::cast_precision_loss)]
    fn from_ron_value(value: Value) -> Result<Self> {
        match value {
            Value::Number(Number::F64(f)) => Ok(f.get()),
            Value::Number(Number::F32(f)) => Ok(f64::from(f.get())),
            Value::Number(n) => {
                // Also accept integers as floats
                let val: f64 = match n {
                    Number::I8(v) => f64::from(v),
                    Number::I16(v) => f64::from(v),
                    Number::I32(v) => f64::from(v),
                    Number::I64(v) => v as f64,
                    #[cfg(feature = "integer128")]
                    Number::I128(v) => v as f64,
                    Number::U8(v) => f64::from(v),
                    Number::U16(v) => f64::from(v),
                    Number::U32(v) => f64::from(v),
                    Number::U64(v) => v as f64,
                    #[cfg(feature = "integer128")]
                    Number::U128(v) => v as f64,
                    _ => return Err(Error::type_mismatch("f64", &Value::Number(n))),
                };
                Ok(val)
            }
            other => Err(Error::type_mismatch("f64", &other)),
        }
    }
}

// =============================================================================
// Collection implementations - ToRon
// =============================================================================

impl<T: ToRon> ToRon for Vec<T> {
    fn to_ron_value(&self) -> Result<Value> {
        let values: Result<Vec<_>> = self.iter().map(ToRon::to_ron_value).collect();
        Ok(Value::Seq(values?))
    }
}

impl<T: ToRon> ToRon for VecDeque<T> {
    fn to_ron_value(&self) -> Result<Value> {
        let values: Result<Vec<_>> = self.iter().map(ToRon::to_ron_value).collect();
        Ok(Value::Seq(values?))
    }
}

impl<T: ToRon> ToRon for LinkedList<T> {
    fn to_ron_value(&self) -> Result<Value> {
        let values: Result<Vec<_>> = self.iter().map(ToRon::to_ron_value).collect();
        Ok(Value::Seq(values?))
    }
}

#[cfg(feature = "std")]
impl<T: ToRon + Eq + Hash, S: BuildHasher> ToRon for HashSet<T, S> {
    fn to_ron_value(&self) -> Result<Value> {
        let values: Result<Vec<_>> = self.iter().map(ToRon::to_ron_value).collect();
        Ok(Value::Seq(values?))
    }
}

impl<T: ToRon + Ord> ToRon for BTreeSet<T> {
    fn to_ron_value(&self) -> Result<Value> {
        let values: Result<Vec<_>> = self.iter().map(ToRon::to_ron_value).collect();
        Ok(Value::Seq(values?))
    }
}

impl<T: ToRon, const N: usize> ToRon for [T; N] {
    fn to_ron_value(&self) -> Result<Value> {
        let values: Result<Vec<_>> = self.iter().map(ToRon::to_ron_value).collect();
        Ok(Value::Seq(values?))
    }
}

impl<T: ToRon> ToRon for [T] {
    fn to_ron_value(&self) -> Result<Value> {
        let values: Result<Vec<_>> = self.iter().map(ToRon::to_ron_value).collect();
        Ok(Value::Seq(values?))
    }
}

// Map types
#[cfg(feature = "std")]
impl<K: ToRon + Eq + Hash, V: ToRon, S: BuildHasher> ToRon for HashMap<K, V, S> {
    fn to_ron_value(&self) -> Result<Value> {
        let mut map = Map::new();
        for (k, v) in self {
            map.insert(k.to_ron_value()?, v.to_ron_value()?);
        }
        Ok(Value::Map(map))
    }
}

impl<K: ToRon + Ord, V: ToRon> ToRon for BTreeMap<K, V> {
    fn to_ron_value(&self) -> Result<Value> {
        let mut map = Map::new();
        for (k, v) in self {
            map.insert(k.to_ron_value()?, v.to_ron_value()?);
        }
        Ok(Value::Map(map))
    }
}

// =============================================================================
// Collection implementations - FromRon
// =============================================================================

impl<T: FromRon> FromRon for Vec<T> {
    fn from_ron_value(value: Value) -> Result<Self> {
        match value {
            Value::Seq(seq) | Value::Tuple(seq) => seq.into_iter().map(T::from_ron_value).collect(),
            other => Err(Error::type_mismatch("sequence", &other)),
        }
    }
}

impl<T: FromRon> FromRon for VecDeque<T> {
    fn from_ron_value(value: Value) -> Result<Self> {
        match value {
            Value::Seq(seq) | Value::Tuple(seq) => seq.into_iter().map(T::from_ron_value).collect(),
            other => Err(Error::type_mismatch("sequence", &other)),
        }
    }
}

impl<T: FromRon> FromRon for LinkedList<T> {
    fn from_ron_value(value: Value) -> Result<Self> {
        match value {
            Value::Seq(seq) | Value::Tuple(seq) => seq.into_iter().map(T::from_ron_value).collect(),
            other => Err(Error::type_mismatch("sequence", &other)),
        }
    }
}

#[cfg(feature = "std")]
impl<T: FromRon + Eq + Hash, S: BuildHasher + Default> FromRon for HashSet<T, S> {
    fn from_ron_value(value: Value) -> Result<Self> {
        match value {
            Value::Seq(seq) | Value::Tuple(seq) => seq
                .into_iter()
                .map(T::from_ron_value)
                .collect::<Result<HashSet<T, S>>>(),
            other => Err(Error::type_mismatch("sequence", &other)),
        }
    }
}

impl<T: FromRon + Ord> FromRon for BTreeSet<T> {
    fn from_ron_value(value: Value) -> Result<Self> {
        match value {
            Value::Seq(seq) | Value::Tuple(seq) => seq.into_iter().map(T::from_ron_value).collect(),
            other => Err(Error::type_mismatch("sequence", &other)),
        }
    }
}

impl<T: FromRon, const N: usize> FromRon for [T; N] {
    fn from_ron_value(value: Value) -> Result<Self> {
        match value {
            Value::Seq(seq) | Value::Tuple(seq) => {
                if seq.len() != N {
                    return Err(Error::invalid_value(alloc::format!(
                        "expected array of length {N}, got {}",
                        seq.len()
                    )));
                }
                let vec: Vec<T> = seq
                    .into_iter()
                    .map(T::from_ron_value)
                    .collect::<Result<_>>()?;
                vec.try_into().map_err(|_| {
                    Error::invalid_value(alloc::format!("failed to convert to array of length {N}"))
                })
            }
            other => Err(Error::type_mismatch("array", &other)),
        }
    }
}

// Map types
#[cfg(feature = "std")]
impl<K: FromRon + Eq + Hash, V: FromRon, S: BuildHasher + Default> FromRon for HashMap<K, V, S> {
    fn from_ron_value(value: Value) -> Result<Self> {
        match value {
            Value::Map(map) => {
                let mut result = HashMap::with_capacity_and_hasher(map.len(), Default::default());
                for (k, v) in map {
                    result.insert(K::from_ron_value(k)?, V::from_ron_value(v)?);
                }
                Ok(result)
            }
            other => Err(Error::type_mismatch("map", &other)),
        }
    }
}

impl<K: FromRon + Ord, V: FromRon> FromRon for BTreeMap<K, V> {
    fn from_ron_value(value: Value) -> Result<Self> {
        match value {
            Value::Map(map) => {
                let mut result = BTreeMap::new();
                for (k, v) in map {
                    result.insert(K::from_ron_value(k)?, V::from_ron_value(v)?);
                }
                Ok(result)
            }
            other => Err(Error::type_mismatch("map", &other)),
        }
    }
}

// =============================================================================
// Wrapper type implementations
// =============================================================================

impl<T: ToRon> ToRon for Option<T> {
    fn to_ron_value(&self) -> Result<Value> {
        match self {
            Some(v) => Ok(Value::Option(Some(Box::new(v.to_ron_value()?)))),
            None => Ok(Value::Option(None)),
        }
    }
}

impl<T: FromRon> FromRon for Option<T> {
    fn from_ron_value(value: Value) -> Result<Self> {
        match value {
            Value::Option(None) => Ok(None),
            Value::Option(Some(v)) => Ok(Some(T::from_ron_value(*v)?)),
            // Also accept raw values as Some
            other => Ok(Some(T::from_ron_value(other)?)),
        }
    }
}

impl<T: ToRon + ?Sized> ToRon for Box<T> {
    fn to_ron_value(&self) -> Result<Value> {
        (**self).to_ron_value()
    }
}

impl<T: FromRon> FromRon for Box<T> {
    fn from_ron_value(value: Value) -> Result<Self> {
        Ok(Box::new(T::from_ron_value(value)?))
    }
}

impl<T: ToRon + ?Sized> ToRon for &T {
    fn to_ron_value(&self) -> Result<Value> {
        (*self).to_ron_value()
    }
}

impl<T: ToRon + ?Sized> ToRon for &mut T {
    fn to_ron_value(&self) -> Result<Value> {
        (**self).to_ron_value()
    }
}

impl<T: ToRon + Clone> ToRon for Cow<'_, T> {
    fn to_ron_value(&self) -> Result<Value> {
        self.as_ref().to_ron_value()
    }
}

impl<T: FromRon + Clone> FromRon for Cow<'static, T> {
    fn from_ron_value(value: Value) -> Result<Self> {
        Ok(Cow::Owned(T::from_ron_value(value)?))
    }
}

impl<T: ToRon> ToRon for Rc<T> {
    fn to_ron_value(&self) -> Result<Value> {
        (**self).to_ron_value()
    }
}

impl<T: FromRon> FromRon for Rc<T> {
    fn from_ron_value(value: Value) -> Result<Self> {
        Ok(Rc::new(T::from_ron_value(value)?))
    }
}

impl<T: ToRon> ToRon for Arc<T> {
    fn to_ron_value(&self) -> Result<Value> {
        (**self).to_ron_value()
    }
}

impl<T: FromRon> FromRon for Arc<T> {
    fn from_ron_value(value: Value) -> Result<Self> {
        Ok(Arc::new(T::from_ron_value(value)?))
    }
}

impl<T: ToRon + Copy> ToRon for Cell<T> {
    fn to_ron_value(&self) -> Result<Value> {
        self.get().to_ron_value()
    }
}

impl<T: FromRon> FromRon for Cell<T> {
    fn from_ron_value(value: Value) -> Result<Self> {
        Ok(Cell::new(T::from_ron_value(value)?))
    }
}

impl<T: ToRon> ToRon for RefCell<T> {
    fn to_ron_value(&self) -> Result<Value> {
        self.borrow().to_ron_value()
    }
}

impl<T: FromRon> FromRon for RefCell<T> {
    fn from_ron_value(value: Value) -> Result<Self> {
        Ok(RefCell::new(T::from_ron_value(value)?))
    }
}

// =============================================================================
// Tuple implementations
// =============================================================================

macro_rules! impl_to_ron_tuple {
    () => {};
    ($first:ident $(, $rest:ident)*) => {
        impl<$first: ToRon $(, $rest: ToRon)*> ToRon for ($first, $($rest,)*) {
            fn to_ron_value(&self) -> Result<Value> {
                #[allow(non_snake_case)]
                let ($first, $($rest,)*) = self;
                Ok(Value::Tuple(vec![
                    $first.to_ron_value()?,
                    $($rest.to_ron_value()?,)*
                ]))
            }
        }
        impl_to_ron_tuple!($($rest),*);
    };
}

impl_to_ron_tuple!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12);

macro_rules! impl_from_ron_tuple {
    () => {};
    ($first:ident $(, $rest:ident)*) => {
        impl<$first: FromRon $(, $rest: FromRon)*> FromRon for ($first, $($rest,)*) {
            fn from_ron_value(value: Value) -> Result<Self> {
                let mut seq = match value {
                    Value::Tuple(t) => t,
                    Value::Seq(s) => s,
                    other => return Err(Error::type_mismatch("tuple", &other)),
                };
                #[allow(unused_variables, unused_mut)]
                let expected = impl_from_ron_tuple!(@count $first $(, $rest)*);
                if seq.len() != expected {
                    return Err(Error::invalid_value(alloc::format!(
                        "expected tuple of {expected} elements, got {}",
                        seq.len()
                    )));
                }
                seq.reverse();
                Ok((
                    $first::from_ron_value(seq.pop().ok_or_else(|| Error::invalid_value("tuple too short"))?)?,
                    $($rest::from_ron_value(seq.pop().ok_or_else(|| Error::invalid_value("tuple too short"))?)?,)*
                ))
            }
        }
        impl_from_ron_tuple!($($rest),*);
    };
    (@count $first:ident $(, $rest:ident)*) => {
        1 $(+ impl_from_ron_tuple!(@count $rest))*
    };
    (@count) => { 0 };
}

impl_from_ron_tuple!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12);

// =============================================================================
// MapAccess helper for struct deserialization
// =============================================================================

/// Helper struct for deserializing RON maps/structs to Rust structs.
///
/// This is primarily used by the `#[derive(FromRon)]` macro.
///
/// # Example
///
/// ```
/// use ron2::{Value, MapAccess, FromRon};
///
/// // Manually implementing FromRon for a struct
/// struct Point { x: i32, y: i32 }
///
/// impl FromRon for Point {
///     fn from_ron_value(value: Value) -> ron2::error::Result<Self> {
///         let mut access = MapAccess::new(value)?;
///         let x = access.required("x")?;
///         let y = access.required("y")?;
///         access.deny_unknown_fields()?;
///         Ok(Point { x, y })
///     }
/// }
/// ```
#[cfg(feature = "std")]
pub struct MapAccess {
    map: HashMap<String, Value>,
}

#[cfg(feature = "std")]
impl MapAccess {
    /// Create a new `MapAccess` from a RON Value.
    ///
    /// Accepts both `Value::Struct` (named fields) and `Value::Map` (string keys).
    pub fn new(value: Value) -> Result<Self> {
        match value {
            // ron2 parses (name: val) as Value::Struct
            Value::Struct(fields) => {
                let mut result = HashMap::with_capacity(fields.len());
                for (name, v) in fields {
                    result.insert(name, v);
                }
                Ok(Self { map: result })
            }
            // { key: val } is parsed as Value::Map
            Value::Map(map) => {
                let mut result = HashMap::with_capacity(map.len());
                for (k, v) in map {
                    let key = match k {
                        Value::String(s) => s,
                        other => {
                            return Err(Error::invalid_value(alloc::format!(
                                "expected string key, got {other:?}"
                            )));
                        }
                    };
                    result.insert(key, v);
                }
                Ok(Self { map: result })
            }
            // Named struct: Type(field: val)
            Value::Named { content, .. } => match content {
                crate::NamedContent::Struct(fields) => {
                    let mut result = HashMap::with_capacity(fields.len());
                    for (name, v) in fields {
                        result.insert(name, v);
                    }
                    Ok(Self { map: result })
                }
                _ => Err(Error::type_mismatch(
                    "struct",
                    &Value::Named {
                        name: String::new(),
                        content,
                    },
                )),
            },
            other => Err(Error::type_mismatch("map/struct", &other)),
        }
    }

    /// Get a required field from the map.
    ///
    /// Returns an error if the field is missing.
    pub fn required<T: FromRon>(&mut self, name: &str) -> Result<T> {
        match self.map.remove(name) {
            Some(v) => T::from_ron_value(v),
            None => Err(Error::missing_field(name)),
        }
    }

    /// Get an optional field from the map.
    ///
    /// Returns `Ok(None)` if the field is missing.
    pub fn optional<T: FromRon>(&mut self, name: &str) -> Result<Option<T>> {
        match self.map.remove(name) {
            Some(v) => Ok(Some(T::from_ron_value(v)?)),
            None => Ok(None),
        }
    }

    /// Get a field with a default value if missing.
    pub fn with_default<T: FromRon + Default>(&mut self, name: &str) -> Result<T> {
        match self.map.remove(name) {
            Some(v) => T::from_ron_value(v),
            None => Ok(T::default()),
        }
    }

    /// Get a field with a custom default function.
    pub fn with_default_fn<T: FromRon, F: FnOnce() -> T>(
        &mut self,
        name: &str,
        default_fn: F,
    ) -> Result<T> {
        match self.map.remove(name) {
            Some(v) => T::from_ron_value(v),
            None => Ok(default_fn()),
        }
    }

    /// Check for unknown fields and return an error if any exist.
    ///
    /// Call this after extracting all expected fields to ensure no
    /// unexpected fields were present.
    pub fn deny_unknown_fields(&self) -> Result<()> {
        if let Some(key) = self.map.keys().next() {
            return Err(Error::unknown_field(key));
        }
        Ok(())
    }

    /// Get an iterator over remaining (unconsumed) field names.
    pub fn remaining_keys(&self) -> impl Iterator<Item = &String> {
        self.map.keys()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_primitives_to_ron() {
        assert_eq!(true.to_ron().unwrap(), "true");
        assert_eq!(false.to_ron().unwrap(), "false");
        assert_eq!(42i32.to_ron().unwrap(), "42");
        assert_eq!((-10i8).to_ron().unwrap(), "-10");
        assert_eq!('a'.to_ron().unwrap(), "'a'");
        assert_eq!("hello".to_ron().unwrap(), "\"hello\"");
        assert_eq!(().to_ron().unwrap(), "()");
    }

    #[test]
    fn test_primitives_from_ron() {
        assert!(bool::from_ron("true").unwrap());
        assert!(!bool::from_ron("false").unwrap());
        assert_eq!(i32::from_ron("42").unwrap(), 42);
        assert_eq!(i32::from_ron("-10").unwrap(), -10);
        assert_eq!(char::from_ron("'a'").unwrap(), 'a');
        assert_eq!(String::from_ron("\"hello\"").unwrap(), "hello");
        assert_eq!(<()>::from_ron("()").unwrap(), ());
    }

    #[test]
    fn test_floats() {
        assert!((f32::from_ron("1.5").unwrap() - 1.5).abs() < 0.001);
        assert!((f64::from_ron("2.5").unwrap() - 2.5).abs() < 0.00001);
        // Integers should also work as floats
        assert!((f64::from_ron("42").unwrap() - 42.0).abs() < 0.00001);
    }

    #[test]
    fn test_collections() {
        // Note: ron2's compact serializer doesn't add spaces after commas
        assert_eq!(vec![1, 2, 3].to_ron().unwrap(), "[1,2,3]");
        assert_eq!(Vec::<i32>::new().to_ron().unwrap(), "[]");

        assert_eq!(Vec::<i32>::from_ron("[1, 2, 3]").unwrap(), vec![1, 2, 3]);
        assert_eq!(Vec::<i32>::from_ron("[]").unwrap(), Vec::<i32>::new());
    }

    #[test]
    fn test_option() {
        assert_eq!(Some(42).to_ron().unwrap(), "Some(42)");
        assert_eq!(Option::<i32>::None.to_ron().unwrap(), "None");

        assert_eq!(Option::<i32>::from_ron("Some(42)").unwrap(), Some(42));
        assert_eq!(Option::<i32>::from_ron("None").unwrap(), None);
        // Raw value as implicit Some
        assert_eq!(Option::<i32>::from_ron("42").unwrap(), Some(42));
    }

    #[test]
    fn test_tuple() {
        // ron2 serializes tuples as (a, b)
        let tuple_ron = (1, 2).to_ron().unwrap();
        assert!(tuple_ron.contains('1') && tuple_ron.contains('2'));

        assert_eq!(<(i32, i32)>::from_ron("(1, 2)").unwrap(), (1, 2));
        assert_eq!(
            <(i32, String, bool)>::from_ron("(1, \"hello\", true)").unwrap(),
            (1, "hello".to_string(), true)
        );
    }

    #[test]
    fn test_map() {
        let mut map = BTreeMap::new();
        map.insert("a".to_string(), 1);
        map.insert("b".to_string(), 2);
        let ron = map.to_ron().unwrap();
        assert!(ron.contains("\"a\""));
        assert!(ron.contains("\"b\""));

        let ron = r#"{"a": 1, "b": 2}"#;
        let parsed: BTreeMap<String, i32> = BTreeMap::from_ron(ron).unwrap();
        assert_eq!(parsed.get("a"), Some(&1));
        assert_eq!(parsed.get("b"), Some(&2));
    }

    #[test]
    fn test_array() {
        assert_eq!(<[i32; 3]>::from_ron("[1, 2, 3]").unwrap(), [1, 2, 3]);
    }

    #[test]
    fn test_integer_range() {
        assert!(i8::from_ron("127").is_ok());
        assert!(i8::from_ron("128").is_err());
        assert!(u8::from_ron("255").is_ok());
        assert!(u8::from_ron("256").is_err());
        assert!(u8::from_ron("-1").is_err());
    }

    #[cfg(feature = "std")]
    #[test]
    fn test_map_access() {
        let ron = r#"(name: "test", value: 42)"#;
        let value = crate::from_str(ron).unwrap();
        let mut access = MapAccess::new(value).unwrap();

        assert_eq!(access.required::<String>("name").unwrap(), "test");
        assert_eq!(access.required::<i32>("value").unwrap(), 42);
        assert!(access.deny_unknown_fields().is_ok());
    }
}
