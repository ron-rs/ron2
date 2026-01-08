//! Value module.

use alloc::{borrow::Cow, boxed::Box, string::String, vec::Vec};
use core::{cmp::Eq, hash::Hash};

mod map;
mod number;

pub use map::Map;
pub use number::{Number, F32, F64};

/// A RON value that can represent any valid RON data.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Value {
    Bool(bool),
    Char(char),
    Map(Map),
    Number(Number),
    Option(Option<Box<Value>>),
    String(String),
    Bytes(Vec<u8>),
    Seq(Vec<Value>),
    Unit,
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
