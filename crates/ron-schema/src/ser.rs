//! Serialization trait for RON format without serde.
//!
//! The [`SerRon`] trait provides a serde-independent way to serialize
//! Rust types to RON format.

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet, LinkedList, VecDeque};
use std::hash::Hash;

use ron::value::{Map, Number};
use ron::Value;

use crate::error::RonError;

/// Configuration for pretty-printing RON output.
#[derive(Debug, Clone)]
pub struct PrettyConfig {
    /// String used for indentation (default: 4 spaces).
    pub indent: String,
    /// String used for new lines (default: "\n").
    pub new_line: String,
    /// Whether to include struct names in output.
    pub struct_names: bool,
}

impl Default for PrettyConfig {
    fn default() -> Self {
        Self {
            indent: "    ".to_string(),
            new_line: "\n".to_string(),
            struct_names: false,
        }
    }
}

/// Trait for types that can be serialized to RON format without serde.
///
/// # Example
///
/// ```rust
/// use ron_schema::SerRon;
///
/// let value = vec![1, 2, 3];
/// let ron_string = value.to_ron().unwrap();
/// assert_eq!(ron_string, "[1, 2, 3]");
/// ```
pub trait SerRon {
    /// Serialize this value to a RON string.
    fn to_ron(&self) -> Result<String, RonError> {
        let value = self.to_ron_value()?;
        value_to_string(&value)
    }

    /// Serialize this value to a `ron::Value`.
    fn to_ron_value(&self) -> Result<Value, RonError>;

    /// Serialize this value with pretty formatting.
    fn to_ron_pretty(&self, config: &PrettyConfig) -> Result<String, RonError> {
        let value = self.to_ron_value()?;
        value_to_string_pretty(&value, config)
    }
}

/// Convert a `ron::Value` to a RON string.
pub fn value_to_string(value: &Value) -> Result<String, RonError> {
    let config = ron::ser::PrettyConfig::default()
        .struct_names(false)
        .compact_arrays(true);
    ron::ser::to_string_pretty(value, config).map_err(RonError::from)
}

/// Convert a `ron::Value` to a pretty-printed RON string.
pub fn value_to_string_pretty(value: &Value, config: &PrettyConfig) -> Result<String, RonError> {
    let ron_config = ron::ser::PrettyConfig::default()
        .struct_names(config.struct_names)
        .indentor(config.indent.clone())
        .new_line(config.new_line.clone());
    ron::ser::to_string_pretty(value, ron_config).map_err(RonError::from)
}

// =============================================================================
// Primitive implementations
// =============================================================================

impl SerRon for bool {
    fn to_ron_value(&self) -> Result<Value, RonError> {
        Ok(Value::Bool(*self))
    }
}

impl SerRon for char {
    fn to_ron_value(&self) -> Result<Value, RonError> {
        Ok(Value::Char(*self))
    }
}

impl SerRon for String {
    fn to_ron_value(&self) -> Result<Value, RonError> {
        Ok(Value::String(self.clone()))
    }
}

impl SerRon for str {
    fn to_ron_value(&self) -> Result<Value, RonError> {
        Ok(Value::String(self.to_string()))
    }
}

// Unit type
impl SerRon for () {
    fn to_ron_value(&self) -> Result<Value, RonError> {
        Ok(Value::Unit)
    }
}

// Integer types
macro_rules! impl_ser_ron_int {
    ($($ty:ty => $variant:ident),+ $(,)?) => {
        $(
            impl SerRon for $ty {
                fn to_ron_value(&self) -> Result<Value, RonError> {
                    Ok(Value::Number(Number::$variant(*self)))
                }
            }
        )+
    };
}

impl_ser_ron_int! {
    i8 => I8,
    i16 => I16,
    i32 => I32,
    i64 => I64,
    u8 => U8,
    u16 => U16,
    u32 => U32,
    u64 => U64,
}

// Float types
impl SerRon for f32 {
    fn to_ron_value(&self) -> Result<Value, RonError> {
        Ok(Value::Number(Number::F32((*self).into())))
    }
}

impl SerRon for f64 {
    fn to_ron_value(&self) -> Result<Value, RonError> {
        Ok(Value::Number(Number::F64((*self).into())))
    }
}

// =============================================================================
// Collection implementations
// =============================================================================

impl<T: SerRon> SerRon for Vec<T> {
    fn to_ron_value(&self) -> Result<Value, RonError> {
        let values: Result<Vec<_>, _> = self.iter().map(|v| v.to_ron_value()).collect();
        Ok(Value::Seq(values?))
    }
}

impl<T: SerRon> SerRon for VecDeque<T> {
    fn to_ron_value(&self) -> Result<Value, RonError> {
        let values: Result<Vec<_>, _> = self.iter().map(|v| v.to_ron_value()).collect();
        Ok(Value::Seq(values?))
    }
}

impl<T: SerRon> SerRon for LinkedList<T> {
    fn to_ron_value(&self) -> Result<Value, RonError> {
        let values: Result<Vec<_>, _> = self.iter().map(|v| v.to_ron_value()).collect();
        Ok(Value::Seq(values?))
    }
}

impl<T: SerRon + Eq + Hash> SerRon for HashSet<T> {
    fn to_ron_value(&self) -> Result<Value, RonError> {
        let values: Result<Vec<_>, _> = self.iter().map(|v| v.to_ron_value()).collect();
        Ok(Value::Seq(values?))
    }
}

impl<T: SerRon + Ord> SerRon for BTreeSet<T> {
    fn to_ron_value(&self) -> Result<Value, RonError> {
        let values: Result<Vec<_>, _> = self.iter().map(|v| v.to_ron_value()).collect();
        Ok(Value::Seq(values?))
    }
}

impl<T: SerRon, const N: usize> SerRon for [T; N] {
    fn to_ron_value(&self) -> Result<Value, RonError> {
        let values: Result<Vec<_>, _> = self.iter().map(|v| v.to_ron_value()).collect();
        Ok(Value::Seq(values?))
    }
}

impl<T: SerRon> SerRon for [T] {
    fn to_ron_value(&self) -> Result<Value, RonError> {
        let values: Result<Vec<_>, _> = self.iter().map(|v| v.to_ron_value()).collect();
        Ok(Value::Seq(values?))
    }
}

// Map types
impl<K: SerRon + Eq + Hash, V: SerRon> SerRon for HashMap<K, V> {
    fn to_ron_value(&self) -> Result<Value, RonError> {
        let mut map = Map::new();
        for (k, v) in self {
            map.insert(k.to_ron_value()?, v.to_ron_value()?);
        }
        Ok(Value::Map(map))
    }
}

impl<K: SerRon + Ord, V: SerRon> SerRon for BTreeMap<K, V> {
    fn to_ron_value(&self) -> Result<Value, RonError> {
        let mut map = Map::new();
        for (k, v) in self {
            map.insert(k.to_ron_value()?, v.to_ron_value()?);
        }
        Ok(Value::Map(map))
    }
}

// =============================================================================
// Wrapper type implementations
// =============================================================================

impl<T: SerRon> SerRon for Option<T> {
    fn to_ron_value(&self) -> Result<Value, RonError> {
        match self {
            Some(v) => Ok(Value::Option(Some(Box::new(v.to_ron_value()?)))),
            None => Ok(Value::Option(None)),
        }
    }
}

impl<T: SerRon + ?Sized> SerRon for Box<T> {
    fn to_ron_value(&self) -> Result<Value, RonError> {
        (**self).to_ron_value()
    }
}

impl<T: SerRon + ?Sized> SerRon for &T {
    fn to_ron_value(&self) -> Result<Value, RonError> {
        (*self).to_ron_value()
    }
}

impl<T: SerRon + ?Sized> SerRon for &mut T {
    fn to_ron_value(&self) -> Result<Value, RonError> {
        (**self).to_ron_value()
    }
}

impl<T: SerRon + Clone> SerRon for std::borrow::Cow<'_, T> {
    fn to_ron_value(&self) -> Result<Value, RonError> {
        self.as_ref().to_ron_value()
    }
}

impl<T: SerRon> SerRon for std::rc::Rc<T> {
    fn to_ron_value(&self) -> Result<Value, RonError> {
        (**self).to_ron_value()
    }
}

impl<T: SerRon> SerRon for std::sync::Arc<T> {
    fn to_ron_value(&self) -> Result<Value, RonError> {
        (**self).to_ron_value()
    }
}

impl<T: SerRon> SerRon for std::cell::Cell<T>
where
    T: Copy,
{
    fn to_ron_value(&self) -> Result<Value, RonError> {
        self.get().to_ron_value()
    }
}

impl<T: SerRon> SerRon for std::cell::RefCell<T> {
    fn to_ron_value(&self) -> Result<Value, RonError> {
        self.borrow().to_ron_value()
    }
}

// =============================================================================
// Tuple implementations
// =============================================================================

macro_rules! impl_ser_ron_tuple {
    () => {};
    ($first:ident $(, $rest:ident)*) => {
        impl<$first: SerRon $(, $rest: SerRon)*> SerRon for ($first, $($rest,)*) {
            fn to_ron_value(&self) -> Result<Value, RonError> {
                #[allow(non_snake_case)]
                let ($first, $($rest,)*) = self;
                Ok(Value::Seq(vec![
                    $first.to_ron_value()?,
                    $($rest.to_ron_value()?,)*
                ]))
            }
        }
        impl_ser_ron_tuple!($($rest),*);
    };
}

impl_ser_ron_tuple!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_primitives() {
        assert_eq!(true.to_ron().unwrap(), "true");
        assert_eq!(false.to_ron().unwrap(), "false");
        assert_eq!(42i32.to_ron().unwrap(), "42");
        assert_eq!((-10i8).to_ron().unwrap(), "-10");
        assert_eq!('a'.to_ron().unwrap(), "'a'");
        assert_eq!("hello".to_ron().unwrap(), "\"hello\"");
        assert_eq!(().to_ron().unwrap(), "()");
    }

    #[test]
    fn test_collections() {
        assert_eq!(vec![1, 2, 3].to_ron().unwrap(), "[1, 2, 3]");
        assert_eq!(Vec::<i32>::new().to_ron().unwrap(), "[]");
    }

    #[test]
    fn test_option() {
        assert_eq!(Some(42).to_ron().unwrap(), "Some(42)");
        assert_eq!(Option::<i32>::None.to_ron().unwrap(), "None");
    }

    #[test]
    fn test_tuple() {
        // Note: ron::Value::Seq is used for tuples, which serializes as [...]
        // This is a limitation of the ron::Value intermediate representation
        assert_eq!((1, 2).to_ron().unwrap(), "[1, 2]");
        assert_eq!((1, "hello", true).to_ron().unwrap(), "[1, \"hello\", true]");
    }

    #[test]
    fn test_map() {
        let mut map = BTreeMap::new();
        map.insert("a", 1);
        map.insert("b", 2);
        let ron = map.to_ron().unwrap();
        assert!(ron.contains("\"a\": 1"));
        assert!(ron.contains("\"b\": 2"));
    }
}
