//! Deserialization trait for RON format without serde.
//!
//! The [`DeRon`] trait provides a serde-independent way to deserialize
//! RON format into Rust types.

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet, LinkedList, VecDeque};
use std::hash::Hash;
use std::io::Read;

use ron::value::Number;
use ron::Value;

use crate::error::RonError;

/// Trait for types that can be deserialized from RON format without serde.
///
/// # Example
///
/// ```rust
/// use ron_schema::DeRon;
///
/// let values: Vec<i32> = Vec::from_ron("[1, 2, 3]").unwrap();
/// assert_eq!(values, vec![1, 2, 3]);
/// ```
pub trait DeRon: Sized {
    /// Deserialize from a RON string.
    fn from_ron(ron: &str) -> Result<Self, RonError> {
        let value = parse_ron_value(ron)?;
        Self::from_ron_value(value)
    }

    /// Deserialize from a `ron::Value`.
    fn from_ron_value(value: Value) -> Result<Self, RonError>;

    /// Deserialize from a reader.
    fn from_ron_reader<R: Read>(mut reader: R) -> Result<Self, RonError> {
        let mut buf = String::new();
        reader.read_to_string(&mut buf)?;
        Self::from_ron(&buf)
    }
}

/// Parse a RON string into a `ron::Value`.
pub fn parse_ron_value(s: &str) -> Result<Value, RonError> {
    ron::from_str(s).map_err(RonError::from)
}

// =============================================================================
// Primitive implementations
// =============================================================================

impl DeRon for bool {
    fn from_ron_value(value: Value) -> Result<Self, RonError> {
        match value {
            Value::Bool(b) => Ok(b),
            other => Err(RonError::type_mismatch("bool", other)),
        }
    }
}

impl DeRon for char {
    fn from_ron_value(value: Value) -> Result<Self, RonError> {
        match value {
            Value::Char(c) => Ok(c),
            other => Err(RonError::type_mismatch("char", other)),
        }
    }
}

impl DeRon for String {
    fn from_ron_value(value: Value) -> Result<Self, RonError> {
        match value {
            Value::String(s) => Ok(s),
            other => Err(RonError::type_mismatch("String", other)),
        }
    }
}

impl DeRon for () {
    fn from_ron_value(value: Value) -> Result<Self, RonError> {
        match value {
            Value::Unit => Ok(()),
            other => Err(RonError::type_mismatch("()", other)),
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
                        Number::I8(v) => *v as i128,
                        Number::I16(v) => *v as i128,
                        Number::I32(v) => *v as i128,
                        Number::I64(v) => *v as i128,
                        Number::U8(v) => *v as i128,
                        Number::U16(v) => *v as i128,
                        Number::U32(v) => *v as i128,
                        Number::U64(v) => *v as i128,
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
                        Number::I8(v) if *v >= 0 => *v as u128,
                        Number::I16(v) if *v >= 0 => *v as u128,
                        Number::I32(v) if *v >= 0 => *v as u128,
                        Number::I64(v) if *v >= 0 => *v as u128,
                        Number::U8(v) => *v as u128,
                        Number::U16(v) => *v as u128,
                        Number::U32(v) => *v as u128,
                        Number::U64(v) => *v as u128,
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

macro_rules! impl_de_ron_int {
    ($($ty:ty),+ $(,)?) => {
        $(
            impl DeRon for $ty {
                fn from_ron_value(value: Value) -> Result<Self, RonError> {
                    match value {
                        Value::Number(ref n) => {
                            <$ty as FromNumber>::from_number(n).ok_or_else(|| {
                                RonError::IntegerOutOfRange {
                                    ty: <$ty as FromNumber>::type_name(),
                                    value: format!("{:?}", n),
                                }
                            })
                        }
                        other => Err(RonError::type_mismatch(<$ty as FromNumber>::type_name(), other)),
                    }
                }
            }
        )+
    };
}

impl_de_ron_int!(i8, i16, i32, i64, u8, u16, u32, u64);

impl DeRon for f32 {
    fn from_ron_value(value: Value) -> Result<Self, RonError> {
        match value {
            Value::Number(Number::F32(f)) => Ok(f.get()),
            Value::Number(Number::F64(f)) => Ok(f.get() as f32),
            Value::Number(n) => {
                // Also accept integers as floats
                let val: f64 = match n {
                    Number::I8(v) => v as f64,
                    Number::I16(v) => v as f64,
                    Number::I32(v) => v as f64,
                    Number::I64(v) => v as f64,
                    Number::U8(v) => v as f64,
                    Number::U16(v) => v as f64,
                    Number::U32(v) => v as f64,
                    Number::U64(v) => v as f64,
                    _ => return Err(RonError::type_mismatch("f32", Value::Number(n))),
                };
                Ok(val as f32)
            }
            other => Err(RonError::type_mismatch("f32", other)),
        }
    }
}

impl DeRon for f64 {
    fn from_ron_value(value: Value) -> Result<Self, RonError> {
        match value {
            Value::Number(Number::F64(f)) => Ok(f.get()),
            Value::Number(Number::F32(f)) => Ok(f.get() as f64),
            Value::Number(n) => {
                // Also accept integers as floats
                let val: f64 = match n {
                    Number::I8(v) => v as f64,
                    Number::I16(v) => v as f64,
                    Number::I32(v) => v as f64,
                    Number::I64(v) => v as f64,
                    Number::U8(v) => v as f64,
                    Number::U16(v) => v as f64,
                    Number::U32(v) => v as f64,
                    Number::U64(v) => v as f64,
                    _ => return Err(RonError::type_mismatch("f64", Value::Number(n))),
                };
                Ok(val)
            }
            other => Err(RonError::type_mismatch("f64", other)),
        }
    }
}

// =============================================================================
// Collection implementations
// =============================================================================

impl<T: DeRon> DeRon for Vec<T> {
    fn from_ron_value(value: Value) -> Result<Self, RonError> {
        match value {
            Value::Seq(seq) => seq.into_iter().map(T::from_ron_value).collect(),
            other => Err(RonError::type_mismatch("sequence", other)),
        }
    }
}

impl<T: DeRon> DeRon for VecDeque<T> {
    fn from_ron_value(value: Value) -> Result<Self, RonError> {
        match value {
            Value::Seq(seq) => seq.into_iter().map(T::from_ron_value).collect(),
            other => Err(RonError::type_mismatch("sequence", other)),
        }
    }
}

impl<T: DeRon> DeRon for LinkedList<T> {
    fn from_ron_value(value: Value) -> Result<Self, RonError> {
        match value {
            Value::Seq(seq) => seq.into_iter().map(T::from_ron_value).collect(),
            other => Err(RonError::type_mismatch("sequence", other)),
        }
    }
}

impl<T: DeRon + Eq + Hash> DeRon for HashSet<T> {
    fn from_ron_value(value: Value) -> Result<Self, RonError> {
        match value {
            Value::Seq(seq) => seq.into_iter().map(T::from_ron_value).collect(),
            other => Err(RonError::type_mismatch("sequence", other)),
        }
    }
}

impl<T: DeRon + Ord> DeRon for BTreeSet<T> {
    fn from_ron_value(value: Value) -> Result<Self, RonError> {
        match value {
            Value::Seq(seq) => seq.into_iter().map(T::from_ron_value).collect(),
            other => Err(RonError::type_mismatch("sequence", other)),
        }
    }
}

impl<T: DeRon, const N: usize> DeRon for [T; N] {
    fn from_ron_value(value: Value) -> Result<Self, RonError> {
        match value {
            Value::Seq(seq) => {
                if seq.len() != N {
                    return Err(RonError::InvalidValue(format!(
                        "expected array of length {}, got {}",
                        N,
                        seq.len()
                    )));
                }
                let vec: Vec<T> = seq.into_iter().map(T::from_ron_value).collect::<Result<_, _>>()?;
                vec.try_into().map_err(|_| {
                    RonError::InvalidValue(format!("failed to convert to array of length {}", N))
                })
            }
            other => Err(RonError::type_mismatch("array", other)),
        }
    }
}

// Map types
impl<K: DeRon + Eq + Hash, V: DeRon> DeRon for HashMap<K, V> {
    fn from_ron_value(value: Value) -> Result<Self, RonError> {
        match value {
            Value::Map(map) => {
                let mut result = HashMap::with_capacity(map.len());
                for (k, v) in map {
                    result.insert(K::from_ron_value(k)?, V::from_ron_value(v)?);
                }
                Ok(result)
            }
            other => Err(RonError::type_mismatch("map", other)),
        }
    }
}

impl<K: DeRon + Ord, V: DeRon> DeRon for BTreeMap<K, V> {
    fn from_ron_value(value: Value) -> Result<Self, RonError> {
        match value {
            Value::Map(map) => {
                let mut result = BTreeMap::new();
                for (k, v) in map {
                    result.insert(K::from_ron_value(k)?, V::from_ron_value(v)?);
                }
                Ok(result)
            }
            other => Err(RonError::type_mismatch("map", other)),
        }
    }
}

// =============================================================================
// Wrapper type implementations
// =============================================================================

impl<T: DeRon> DeRon for Option<T> {
    fn from_ron_value(value: Value) -> Result<Self, RonError> {
        match value {
            Value::Option(None) => Ok(None),
            Value::Option(Some(v)) => Ok(Some(T::from_ron_value(*v)?)),
            // Also accept raw values as Some
            other => Ok(Some(T::from_ron_value(other)?)),
        }
    }
}

impl<T: DeRon> DeRon for Box<T> {
    fn from_ron_value(value: Value) -> Result<Self, RonError> {
        Ok(Box::new(T::from_ron_value(value)?))
    }
}

impl<T: DeRon> DeRon for std::rc::Rc<T> {
    fn from_ron_value(value: Value) -> Result<Self, RonError> {
        Ok(std::rc::Rc::new(T::from_ron_value(value)?))
    }
}

impl<T: DeRon> DeRon for std::sync::Arc<T> {
    fn from_ron_value(value: Value) -> Result<Self, RonError> {
        Ok(std::sync::Arc::new(T::from_ron_value(value)?))
    }
}

impl<T: DeRon + Clone> DeRon for std::borrow::Cow<'static, T> {
    fn from_ron_value(value: Value) -> Result<Self, RonError> {
        Ok(std::borrow::Cow::Owned(T::from_ron_value(value)?))
    }
}

impl<T: DeRon> DeRon for std::cell::Cell<T> {
    fn from_ron_value(value: Value) -> Result<Self, RonError> {
        Ok(std::cell::Cell::new(T::from_ron_value(value)?))
    }
}

impl<T: DeRon> DeRon for std::cell::RefCell<T> {
    fn from_ron_value(value: Value) -> Result<Self, RonError> {
        Ok(std::cell::RefCell::new(T::from_ron_value(value)?))
    }
}

// =============================================================================
// Tuple implementations
// =============================================================================

macro_rules! impl_de_ron_tuple {
    () => {};
    ($first:ident $(, $rest:ident)*) => {
        impl<$first: DeRon $(, $rest: DeRon)*> DeRon for ($first, $($rest,)*) {
            fn from_ron_value(value: Value) -> Result<Self, RonError> {
                match value {
                    Value::Seq(mut seq) => {
                        #[allow(unused_variables, unused_mut)]
                        let expected = impl_de_ron_tuple!(@count $first $(, $rest)*);
                        if seq.len() != expected {
                            return Err(RonError::InvalidValue(format!(
                                "expected tuple of {} elements, got {}",
                                expected,
                                seq.len()
                            )));
                        }
                        seq.reverse();
                        Ok((
                            $first::from_ron_value(seq.pop().unwrap())?,
                            $($rest::from_ron_value(seq.pop().unwrap())?,)*
                        ))
                    }
                    other => Err(RonError::type_mismatch("tuple", other)),
                }
            }
        }
        impl_de_ron_tuple!($($rest),*);
    };
    (@count $first:ident $(, $rest:ident)*) => {
        1 $(+ impl_de_ron_tuple!(@count $rest))*
    };
    (@count) => { 0 };
}

impl_de_ron_tuple!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12);

// =============================================================================
// Helper for struct deserialization (used by derive macro)
// =============================================================================

/// Helper struct for deserializing RON maps to structs.
///
/// This is primarily used by the `#[derive(DeRon)]` macro.
pub struct MapAccess {
    map: HashMap<String, Value>,
}

impl MapAccess {
    /// Create a new MapAccess from a ron::Value.
    pub fn new(value: Value) -> Result<Self, RonError> {
        match value {
            Value::Map(map) => {
                let mut result = HashMap::with_capacity(map.len());
                for (k, v) in map {
                    let key = match k {
                        Value::String(s) => s,
                        other => {
                            return Err(RonError::InvalidValue(format!(
                                "expected string key, got {:?}",
                                other
                            )))
                        }
                    };
                    result.insert(key, v);
                }
                Ok(Self { map: result })
            }
            other => Err(RonError::type_mismatch("map/struct", other)),
        }
    }

    /// Get a required field from the map.
    pub fn required<T: DeRon>(&mut self, name: &str) -> Result<T, RonError> {
        match self.map.remove(name) {
            Some(v) => T::from_ron_value(v),
            None => Err(RonError::MissingField(name.to_string())),
        }
    }

    /// Get an optional field from the map.
    pub fn optional<T: DeRon>(&mut self, name: &str) -> Result<Option<T>, RonError> {
        match self.map.remove(name) {
            Some(v) => Ok(Some(T::from_ron_value(v)?)),
            None => Ok(None),
        }
    }

    /// Get a field with a default value.
    pub fn with_default<T: DeRon + Default>(&mut self, name: &str) -> Result<T, RonError> {
        match self.map.remove(name) {
            Some(v) => T::from_ron_value(v),
            None => Ok(T::default()),
        }
    }

    /// Get a field with a custom default function.
    pub fn with_default_fn<T: DeRon, F: FnOnce() -> T>(
        &mut self,
        name: &str,
        default_fn: F,
    ) -> Result<T, RonError> {
        match self.map.remove(name) {
            Some(v) => T::from_ron_value(v),
            None => Ok(default_fn()),
        }
    }

    /// Check for unknown fields and return an error if any exist.
    pub fn deny_unknown_fields(&self) -> Result<(), RonError> {
        if let Some(key) = self.map.keys().next() {
            return Err(RonError::UnknownField(key.clone()));
        }
        Ok(())
    }

    /// Get remaining keys (for error reporting).
    pub fn remaining_keys(&self) -> impl Iterator<Item = &String> {
        self.map.keys()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_primitives() {
        assert_eq!(bool::from_ron("true").unwrap(), true);
        assert_eq!(bool::from_ron("false").unwrap(), false);
        assert_eq!(i32::from_ron("42").unwrap(), 42);
        assert_eq!(i32::from_ron("-10").unwrap(), -10);
        assert_eq!(char::from_ron("'a'").unwrap(), 'a');
        assert_eq!(String::from_ron("\"hello\"").unwrap(), "hello");
        assert_eq!(<()>::from_ron("()").unwrap(), ());
    }

    #[test]
    fn test_floats() {
        assert!((f32::from_ron("3.14").unwrap() - 3.14).abs() < 0.001);
        assert!((f64::from_ron("3.14159").unwrap() - 3.14159).abs() < 0.00001);
        // Integers should also work as floats
        assert_eq!(f64::from_ron("42").unwrap(), 42.0);
    }

    #[test]
    fn test_collections() {
        assert_eq!(Vec::<i32>::from_ron("[1, 2, 3]").unwrap(), vec![1, 2, 3]);
        assert_eq!(Vec::<i32>::from_ron("[]").unwrap(), Vec::<i32>::new());
    }

    #[test]
    fn test_option() {
        assert_eq!(Option::<i32>::from_ron("Some(42)").unwrap(), Some(42));
        assert_eq!(Option::<i32>::from_ron("None").unwrap(), None);
        // Raw value as implicit Some
        assert_eq!(Option::<i32>::from_ron("42").unwrap(), Some(42));
    }

    #[test]
    fn test_tuple() {
        assert_eq!(<(i32, i32)>::from_ron("(1, 2)").unwrap(), (1, 2));
        assert_eq!(
            <(i32, String, bool)>::from_ron("(1, \"hello\", true)").unwrap(),
            (1, "hello".to_string(), true)
        );
    }

    #[test]
    fn test_map() {
        let ron = r#"{"a": 1, "b": 2}"#;
        let map: BTreeMap<String, i32> = BTreeMap::from_ron(ron).unwrap();
        assert_eq!(map.get("a"), Some(&1));
        assert_eq!(map.get("b"), Some(&2));
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

    #[test]
    fn test_map_access() {
        let ron = r#"(name: "test", value: 42)"#;
        let value = parse_ron_value(ron).unwrap();
        let mut access = MapAccess::new(value).unwrap();

        assert_eq!(access.required::<String>("name").unwrap(), "test");
        assert_eq!(access.required::<i32>("value").unwrap(), 42);
        assert!(access.deny_unknown_fields().is_ok());
    }
}
