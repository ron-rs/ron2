//! Value module.

use alloc::{borrow::Cow, boxed::Box, string::String, vec::Vec};
use core::{
    cmp::Eq,
    fmt::{self, Display, Formatter},
    hash::Hash,
    str::FromStr,
};

mod map;
mod number;

pub use map::Map;
pub use number::{F32, F64, Number};

use crate::{
    Error, ErrorKind,
    ast::{ItemTrivia, RonFormatter, SerializeRon, into_value, parse_document},
};

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

impl FromStr for Value {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let doc = parse_document(s)?;

        // Use consuming conversion to avoid unnecessary clones
        match into_value(doc) {
            Some(Ok(value)) => Ok(value),
            Some(Err(e)) => {
                // Conversion error - e already contains span information
                Err(e)
            }
            None => {
                // Empty document - return EOF error (consistent with FromRon::from_ron)
                Err(Error::at_start(ErrorKind::Eof))
            }
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use crate::ast::to_ron_string;

        // Serialize directly using SerializeRon
        let formatted = to_ron_string(self);

        f.write_str(&formatted)
    }
}

/// Content of a named type (struct or enum variant).
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
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

// ============================================================================
// SerializeRon implementation for Value
// ============================================================================

impl SerializeRon for Value {
    fn serialize(&self, fmt: &mut RonFormatter<'_>) {
        match self {
            // Primitives
            Value::Unit => fmt.write_str("()"),
            Value::Bool(b) => fmt.write_str(if *b { "true" } else { "false" }),
            Value::Char(c) => fmt.format_char_value(*c),
            Value::String(s) => fmt.format_string_value(s),
            Value::Bytes(b) => fmt.format_bytes_value(b),
            Value::Number(n) => format_number_to(n, fmt),

            // Option
            Value::Option(opt) => {
                let value = opt
                    .as_ref()
                    .map(|inner| (inner.as_ref(), ItemTrivia::empty()));
                fmt.format_option_with(value);
            }

            // Seq
            Value::Seq(items) => {
                let items = items.iter().map(|v| (ItemTrivia::empty(), v));
                fmt.format_seq_with(None, None, items);
            }

            // Tuple
            Value::Tuple(elements) => {
                let items = elements.iter().map(|v| (ItemTrivia::empty(), v));
                fmt.format_tuple_with(None, None, items);
            }

            // Map
            Value::Map(map) => {
                let entries = map.iter().map(|(k, v)| (ItemTrivia::empty(), k, v));
                fmt.format_map_with(None, None, entries);
            }

            // Anonymous struct
            Value::Struct(fields) => {
                let field_items = fields
                    .iter()
                    .map(|(name, value)| (ItemTrivia::empty(), name.as_str(), value));
                fmt.format_anon_struct_with(None, None, field_items);
            }

            // Named type
            Value::Named { name, content } => {
                fmt.write_str(name);
                match content {
                    NamedContent::Unit => {
                        // Just the name, no body
                    }
                    NamedContent::Tuple(elements) => {
                        let items = elements.iter().map(|v| (ItemTrivia::empty(), v));
                        fmt.format_tuple_with(None, None, items);
                    }
                    NamedContent::Struct(fields) => {
                        let field_items = fields
                            .iter()
                            .map(|(n, v)| (ItemTrivia::empty(), n.as_str(), v));
                        fmt.format_struct_fields_with(None, None, field_items);
                    }
                }
            }
        }
    }
}

/// Format a number directly to the formatter.
fn format_number_to(n: &Number, fmt: &mut RonFormatter<'_>) {
    use alloc::string::ToString;

    match n {
        Number::I8(v) => fmt.write_str(itoa::Buffer::new().format(*v)),
        Number::I16(v) => fmt.write_str(itoa::Buffer::new().format(*v)),
        Number::I32(v) => fmt.write_str(itoa::Buffer::new().format(*v)),
        Number::I64(v) => fmt.write_str(itoa::Buffer::new().format(*v)),
        #[cfg(feature = "integer128")]
        Number::I128(v) => fmt.write_str(itoa::Buffer::new().format(*v)),
        Number::U8(v) => fmt.write_str(itoa::Buffer::new().format(*v)),
        Number::U16(v) => fmt.write_str(itoa::Buffer::new().format(*v)),
        Number::U32(v) => fmt.write_str(itoa::Buffer::new().format(*v)),
        Number::U64(v) => fmt.write_str(itoa::Buffer::new().format(*v)),
        #[cfg(feature = "integer128")]
        Number::U128(v) => fmt.write_str(itoa::Buffer::new().format(*v)),
        Number::F32(f) => {
            let v = f.get();
            if v.is_nan() {
                fmt.write_str("NaN");
            } else if v.is_infinite() {
                fmt.write_str(if v.is_sign_positive() { "inf" } else { "-inf" });
            } else {
                // Format to string first to check for decimal point
                let s = v.to_string();
                if s.contains('.') || s.contains('e') || s.contains('E') {
                    fmt.write_str(&s);
                } else {
                    fmt.write_str(&s);
                    fmt.write_str(".0");
                }
            }
        }
        Number::F64(f) => {
            let v = f.get();
            if v.is_nan() {
                fmt.write_str("NaN");
            } else if v.is_infinite() {
                fmt.write_str(if v.is_sign_positive() { "inf" } else { "-inf" });
            } else {
                let s = v.to_string();
                if s.contains('.') || s.contains('e') || s.contains('E') {
                    fmt.write_str(&s);
                } else {
                    fmt.write_str(&s);
                    fmt.write_str(".0");
                }
            }
        }
        // Handle non-exhaustive variant (future-proofing)
        _ => fmt.write_str("0"),
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
