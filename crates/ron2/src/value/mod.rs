//! Value module.

use alloc::{borrow::Cow, boxed::Box, string::String, vec::Vec};
use core::{cmp::Eq, hash::Hash};

mod map;
mod number;

pub use map::Map;
pub use number::{Number, F32, F64};

/// Ordered list of struct fields (name-value pairs).
///
/// Uses a `Vec` to preserve insertion order and support all trait derivations.
/// Field lookup is O(n) but structs typically have few fields.
pub type StructFields = Vec<(String, Value)>;

/// A RON value that can represent any valid RON data.
///
/// This enum distinguishes between all RON syntactic forms:
/// - `Seq`: sequences `[a, b, c]`
/// - `Tuple`: tuples `(a, b, c)`
/// - `Map`: maps with arbitrary keys `{ key: value }`
/// - `Struct`: anonymous structs with named fields `(x: 1, y: 2)`
/// - `Named`: named types (structs/enums) like `Point(x: 1)` or `Option::Some(1)`
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Value {
    // Primitives
    Bool(bool),
    Char(char),
    Number(Number),
    String(String),
    Bytes(Vec<u8>),
    Unit,
    /// `None` or `Some(value)` - special-cased for convenience
    Option(Option<Box<Value>>),

    // Collections (anonymous)
    /// Sequence: `[a, b, c]`
    Seq(Vec<Value>),
    /// Tuple: `(a, b, c)` - positional elements
    Tuple(Vec<Value>),
    /// Map: `{ key: value }` - arbitrary Value keys
    Map(Map),
    /// Anonymous struct: `(x: 1, y: 2)` - named fields, no type name
    Struct(StructFields),

    // Named types (struct or enum)
    /// Named type: `Point`, `Point(1, 2)`, `Point(x: 1)`, or `Type::Variant(...)`
    ///
    /// The `name` field stores the full path as-is (e.g., `"Type::Variant"`).
    Named {
        /// The type/variant name, e.g., `"Point"` or `"Option::Some"`
        name: String,
        /// The content of the named type
        content: NamedContent,
    },
}

/// Content of a named type (struct or enum variant).
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum NamedContent {
    /// Unit: `Point` or `MyEnum::Variant`
    Unit,
    /// Tuple: `Point(1, 2)` or `Some(x)`
    Tuple(Vec<Value>),
    /// Struct: `Point(x: 1, y: 2)`
    Struct(StructFields),
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

impl From<char> for Value {
    fn from(value: char) -> Self {
        Self::Char(value)
    }
}

impl<K: Into<Value>, V: Into<Value>> FromIterator<(K, V)> for Value {
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        Self::Map(iter.into_iter().collect())
    }
}

impl From<Map> for Value {
    fn from(value: Map) -> Self {
        Self::Map(value)
    }
}

impl<T: Into<Number>> From<T> for Value {
    fn from(value: T) -> Self {
        Self::Number(value.into())
    }
}

impl<T: Into<Value>> From<Option<T>> for Value {
    fn from(value: Option<T>) -> Self {
        Self::Option(value.map(Into::into).map(Box::new))
    }
}

impl<'a> From<&'a str> for Value {
    fn from(value: &'a str) -> Self {
        String::from(value).into()
    }
}

impl<'a> From<Cow<'a, str>> for Value {
    fn from(value: Cow<'a, str>) -> Self {
        String::from(value).into()
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

/// Special case to allow `Value::from(b"byte string")`
impl<const N: usize> From<&'static [u8; N]> for Value {
    fn from(value: &'static [u8; N]) -> Self {
        Self::Bytes(Vec::from(*value))
    }
}

impl<T: Into<Value>> FromIterator<T> for Value {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Self::Seq(iter.into_iter().map(Into::into).collect())
    }
}

impl<'a, T: Clone + Into<Value>> From<&'a [T]> for Value {
    fn from(value: &'a [T]) -> Self {
        value.iter().map(Clone::clone).map(Into::into).collect()
    }
}

impl<T: Into<Value>> From<Vec<T>> for Value {
    fn from(value: Vec<T>) -> Self {
        value.into_iter().collect()
    }
}

impl From<()> for Value {
    fn from(_value: ()) -> Self {
        Value::Unit
    }
}

#[cfg(test)]
mod tests {
    use alloc::{vec, vec::Vec};

    use super::*;

    #[test]
    fn boolean() {
        assert_eq!(Value::from(true), Value::Bool(true));
        assert_eq!(Value::from(false), Value::Bool(false));
    }

    #[test]
    fn float() {
        assert_eq!(
            Value::from(42_f32),
            Value::Number(Number::F32(42_f32.into()))
        );
        assert_eq!(
            Value::from(42_f64),
            Value::Number(Number::F64(42_f64.into()))
        );
    }

    #[test]
    fn int() {
        assert_eq!(Value::from(0_i8), Value::Number(Number::I8(0)));
        assert_eq!(Value::from(0_i16), Value::Number(Number::I16(0)));
        assert_eq!(Value::from(0_i32), Value::Number(Number::I32(0)));
        assert_eq!(Value::from(0_i64), Value::Number(Number::I64(0)));
        #[cfg(feature = "integer128")]
        assert_eq!(Value::from(0_i128), Value::Number(Number::I128(0)));
        assert_eq!(Value::from(0_u8), Value::Number(Number::U8(0)));
        assert_eq!(Value::from(0_u16), Value::Number(Number::U16(0)));
        assert_eq!(Value::from(0_u32), Value::Number(Number::U32(0)));
        assert_eq!(Value::from(0_u64), Value::Number(Number::U64(0)));
        #[cfg(feature = "integer128")]
        assert_eq!(Value::from(0_u128), Value::Number(Number::U128(0)));
    }

    #[test]
    fn char_value() {
        assert_eq!(Value::from('a'), Value::Char('a'));
    }

    #[test]
    fn string() {
        assert_eq!(Value::from("slice"), Value::String(String::from("slice")));
        assert_eq!(
            Value::from(String::from("string")),
            Value::String(String::from("string"))
        );
        assert_eq!(
            Value::from(Cow::Borrowed("cow")),
            Value::String(String::from("cow"))
        );
    }

    #[test]
    fn bytes() {
        assert_eq!(Value::from(b"bytes"), Value::Bytes(Vec::from(*b"bytes")));
    }

    #[test]
    fn map() {
        assert_eq!(Value::from(Map::new()), Value::Map(Map::new()));
        assert_eq!(
            Value::from_iter([("a", 42)]),
            Value::Map({
                let mut map = Map::new();
                map.insert(Value::from("a"), Value::from(42));
                map
            })
        );
    }

    #[test]
    fn option() {
        assert_eq!(Value::from(Option::<bool>::None), Value::Option(None));
        assert_eq!(
            Value::from(Some(false)),
            Value::Option(Some(Box::new(Value::Bool(false))))
        );
        assert_eq!(
            Value::from(Some(Option::<bool>::None)),
            Value::Option(Some(Box::new(Value::Option(None))))
        );
    }

    #[test]
    fn seq() {
        assert_eq!(
            Value::from([-1_i8, 2, -3].as_slice()),
            Value::Seq(vec![
                Value::from(-1_i8),
                Value::from(2_i8),
                Value::from(-3_i8)
            ])
        );
        assert_eq!(
            Value::from(vec![-1_i8, 2, -3]),
            Value::Seq(vec![
                Value::from(-1_i8),
                Value::from(2_i8),
                Value::from(-3_i8)
            ])
        );
        assert_eq!(
            Value::from_iter([-1_i8, 2, -3]),
            Value::Seq(vec![
                Value::from(-1_i8),
                Value::from(2_i8),
                Value::from(-3_i8)
            ])
        );
    }

    #[test]
    fn unit() {
        assert_eq!(Value::from(()), Value::Unit);
    }
}
