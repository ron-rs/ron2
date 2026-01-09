//! Type conversion traits for RON.
//!
//! This module provides [`ToRon`] and [`FromRon`] traits for converting
//! between Rust types and RON values, without requiring serde.
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
//! // Deserialize from RON (now returns SpannedResult for precise error locations)
//! let parsed: Vec<i32> = Vec::from_ron("[1, 2, 3]").unwrap();
//! assert_eq!(parsed, vec![1, 2, 3]);
//! ```

use alloc::{
    borrow::Cow,
    boxed::Box,
    collections::{BTreeMap, BTreeSet, LinkedList, VecDeque},
    format,
    rc::Rc,
    string::String,
    sync::Arc,
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
    ast::{parse_document, value_to_expr, Expr, NumberKind},
    error::{Error, Result, SpannedError, SpannedResult},
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
/// The core method is [`from_ast`](FromRon::from_ast), which deserializes from
/// an AST expression with full span information for error reporting.
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
    /// Core method: deserialize from an AST expression.
    ///
    /// This has access to the full span information for precise error reporting.
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self>;

    /// Construct this type from a RON [`Value`].
    ///
    /// This converts the Value to an AST expression with synthetic spans,
    /// then calls [`from_ast`](FromRon::from_ast). Error spans will be synthetic
    /// (line 0) since the Value has no source position information.
    fn from_ron_value(value: Value) -> Result<Self> {
        let expr = value_to_expr(value);
        Self::from_ast(&expr).map_err(|e| e.code)
    }

    /// Parse a RON string and construct this type.
    ///
    /// Returns a [`SpannedResult`] with precise error location information.
    fn from_ron(s: &str) -> SpannedResult<Self> {
        let doc = parse_document(s)?;
        match doc.value {
            Some(ref expr) => Self::from_ast(expr),
            None => Err(SpannedError::at_start(Error::Eof)),
        }
    }

    /// Read RON from a reader and construct this type.
    #[cfg(feature = "std")]
    fn from_ron_reader<R: Read>(mut reader: R) -> SpannedResult<Self> {
        let mut buf = String::new();
        reader
            .read_to_string(&mut buf)
            .map_err(|e| SpannedError::at_start(Error::Io(e.to_string())))?;
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

/// Get a human-readable type name for an AST expression.
fn expr_type_name(expr: &Expr<'_>) -> &'static str {
    match expr {
        Expr::Unit(_) => "unit",
        Expr::Bool(_) => "bool",
        Expr::Char(_) => "char",
        Expr::Byte(_) => "byte",
        Expr::Number(_) => "number",
        Expr::String(_) => "string",
        Expr::Bytes(_) => "bytes",
        Expr::Option(_) => "option",
        Expr::Seq(_) => "sequence",
        Expr::Map(_) => "map",
        Expr::Tuple(_) => "tuple",
        Expr::AnonStruct(_) => "struct",
        Expr::Struct(_) => "named",
    }
}

/// Create a spanned type mismatch error.
fn spanned_type_mismatch(expected: &str, expr: &Expr<'_>) -> SpannedError {
    SpannedError {
        code: Error::InvalidValueForType {
            expected: expected.into(),
            found: expr_type_name(expr).into(),
        },
        span: expr.span().clone(),
    }
}

/// Create a spanned error with the given code and expression's span.
fn spanned_err(code: Error, expr: &Expr<'_>) -> SpannedError {
    SpannedError {
        code,
        span: expr.span().clone(),
    }
}

// =============================================================================
// Number parsing helpers (for FromRon)
// =============================================================================

/// Parse an integer from a raw string representation.
fn parse_integer_from_raw<T>(raw: &str, kind: &NumberKind) -> Result<T>
where
    T: TryFrom<i128> + TryFrom<u128>,
{
    let raw = raw.trim();

    match kind {
        NumberKind::Integer => {
            // Positive integer
            let cleaned: String = raw.chars().filter(|&c| c != '_').collect();
            let (base, digits) = if cleaned.starts_with("0x") || cleaned.starts_with("0X") {
                (16, &cleaned[2..])
            } else if cleaned.starts_with("0b") || cleaned.starts_with("0B") {
                (2, &cleaned[2..])
            } else if cleaned.starts_with("0o") || cleaned.starts_with("0O") {
                (8, &cleaned[2..])
            } else {
                (10, cleaned.as_str())
            };
            let val = u128::from_str_radix(digits, base).map_err(|_| Error::IntegerOutOfBounds)?;
            T::try_from(val).map_err(|_| Error::IntegerOutOfBounds)
        }
        NumberKind::NegativeInteger => {
            // Negative integer
            let raw = raw.strip_prefix('-').unwrap_or(raw);
            let cleaned: String = raw.chars().filter(|&c| c != '_').collect();
            let (base, digits) = if cleaned.starts_with("0x") || cleaned.starts_with("0X") {
                (16, &cleaned[2..])
            } else if cleaned.starts_with("0b") || cleaned.starts_with("0B") {
                (2, &cleaned[2..])
            } else if cleaned.starts_with("0o") || cleaned.starts_with("0O") {
                (8, &cleaned[2..])
            } else {
                (10, cleaned.as_str())
            };
            let val = i128::from_str_radix(digits, base).map_err(|_| Error::IntegerOutOfBounds)?;
            let val = -val;
            T::try_from(val).map_err(|_| Error::IntegerOutOfBounds)
        }
        NumberKind::Float | NumberKind::SpecialFloat => Err(Error::ExpectedInteger),
    }
}

/// Parse a float from a raw string representation.
fn parse_float_from_raw(raw: &str, kind: &NumberKind) -> Result<f64> {
    let raw = raw.trim();
    match kind {
        NumberKind::SpecialFloat => match raw {
            "inf" => Ok(f64::INFINITY),
            "-inf" => Ok(f64::NEG_INFINITY),
            "NaN" => Ok(f64::NAN),
            _ => Err(Error::ExpectedFloat),
        },
        NumberKind::Float => {
            let cleaned: String = raw.chars().filter(|&c| c != '_').collect();
            cleaned.parse().map_err(|_| Error::ExpectedFloat)
        }
        NumberKind::Integer | NumberKind::NegativeInteger => {
            // Also accept integers as floats
            let cleaned: String = raw.chars().filter(|&c| c != '_').collect();
            cleaned.parse().map_err(|_| Error::ExpectedFloat)
        }
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
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        match expr {
            Expr::Bool(b) => Ok(b.value),
            _ => Err(spanned_type_mismatch("bool", expr)),
        }
    }
}

impl FromRon for char {
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        match expr {
            Expr::Char(c) => Ok(c.value),
            _ => Err(spanned_type_mismatch("char", expr)),
        }
    }
}

impl FromRon for String {
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        match expr {
            Expr::String(s) => Ok(s.value.clone()),
            _ => Err(spanned_type_mismatch("String", expr)),
        }
    }
}

impl FromRon for () {
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        match expr {
            Expr::Unit(_) => Ok(()),
            _ => Err(spanned_type_mismatch("()", expr)),
        }
    }
}

macro_rules! impl_from_ron_int {
    ($($ty:ty),+ $(,)?) => {
        $(
            impl FromRon for $ty {
                fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
                    match expr {
                        Expr::Number(n) => {
                            parse_integer_from_raw::<$ty>(&n.raw, &n.kind)
                                .map_err(|e| spanned_err(e, expr))
                        }
                        Expr::Byte(b) => {
                            // Byte literals can be converted to integers
                            <$ty>::try_from(b.value)
                                .map_err(|_| spanned_err(Error::IntegerOutOfBounds, expr))
                        }
                        _ => Err(spanned_type_mismatch(stringify!($ty), expr)),
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
    #[allow(clippy::cast_possible_truncation)]
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        match expr {
            Expr::Number(n) => {
                let val = parse_float_from_raw(&n.raw, &n.kind)
                    .map_err(|e| spanned_err(e, expr))?;
                Ok(val as f32)
            }
            _ => Err(spanned_type_mismatch("f32", expr)),
        }
    }
}

impl FromRon for f64 {
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        match expr {
            Expr::Number(n) => {
                parse_float_from_raw(&n.raw, &n.kind).map_err(|e| spanned_err(e, expr))
            }
            _ => Err(spanned_type_mismatch("f64", expr)),
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

/// Helper to extract elements from a sequence-like expression.
fn extract_seq_elements<'a>(expr: &'a Expr<'a>) -> Option<Vec<&'a Expr<'a>>> {
    match expr {
        Expr::Seq(seq) => Some(seq.items.iter().map(|item| &item.expr).collect()),
        Expr::Tuple(tuple) => Some(tuple.elements.iter().map(|elem| &elem.expr).collect()),
        _ => None,
    }
}

impl<T: FromRon> FromRon for Vec<T> {
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        match extract_seq_elements(expr) {
            Some(elements) => elements.into_iter().map(T::from_ast).collect(),
            None => Err(spanned_type_mismatch("sequence", expr)),
        }
    }
}

impl<T: FromRon> FromRon for VecDeque<T> {
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        match extract_seq_elements(expr) {
            Some(elements) => elements.into_iter().map(T::from_ast).collect(),
            None => Err(spanned_type_mismatch("sequence", expr)),
        }
    }
}

impl<T: FromRon> FromRon for LinkedList<T> {
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        match extract_seq_elements(expr) {
            Some(elements) => elements.into_iter().map(T::from_ast).collect(),
            None => Err(spanned_type_mismatch("sequence", expr)),
        }
    }
}

#[cfg(feature = "std")]
impl<T: FromRon + Eq + Hash, S: BuildHasher + Default> FromRon for HashSet<T, S> {
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        match extract_seq_elements(expr) {
            Some(elements) => elements
                .into_iter()
                .map(T::from_ast)
                .collect::<SpannedResult<HashSet<T, S>>>(),
            None => Err(spanned_type_mismatch("sequence", expr)),
        }
    }
}

impl<T: FromRon + Ord> FromRon for BTreeSet<T> {
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        match extract_seq_elements(expr) {
            Some(elements) => elements.into_iter().map(T::from_ast).collect(),
            None => Err(spanned_type_mismatch("sequence", expr)),
        }
    }
}

impl<T: FromRon, const N: usize> FromRon for [T; N] {
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        match extract_seq_elements(expr) {
            Some(elements) => {
                if elements.len() != N {
                    return Err(spanned_err(
                        Error::invalid_value(format!(
                            "expected array of length {N}, got {}",
                            elements.len()
                        )),
                        expr,
                    ));
                }
                let vec: Vec<T> = elements
                    .into_iter()
                    .map(T::from_ast)
                    .collect::<SpannedResult<_>>()?;
                vec.try_into().map_err(|_| {
                    spanned_err(
                        Error::invalid_value(format!("failed to convert to array of length {N}")),
                        expr,
                    )
                })
            }
            None => Err(spanned_type_mismatch("array", expr)),
        }
    }
}

// Map types
#[cfg(feature = "std")]
impl<K: FromRon + Eq + Hash, V: FromRon, S: BuildHasher + Default> FromRon for HashMap<K, V, S> {
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        match expr {
            Expr::Map(map) => {
                let mut result = HashMap::with_capacity_and_hasher(map.entries.len(), Default::default());
                for entry in &map.entries {
                    let k = K::from_ast(&entry.key)?;
                    let v = V::from_ast(&entry.value)?;
                    result.insert(k, v);
                }
                Ok(result)
            }
            _ => Err(spanned_type_mismatch("map", expr)),
        }
    }
}

impl<K: FromRon + Ord, V: FromRon> FromRon for BTreeMap<K, V> {
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        match expr {
            Expr::Map(map) => {
                let mut result = BTreeMap::new();
                for entry in &map.entries {
                    let k = K::from_ast(&entry.key)?;
                    let v = V::from_ast(&entry.value)?;
                    result.insert(k, v);
                }
                Ok(result)
            }
            _ => Err(spanned_type_mismatch("map", expr)),
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
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        match expr {
            Expr::Option(opt) => match &opt.value {
                None => Ok(None),
                Some(inner) => Ok(Some(T::from_ast(&inner.expr)?)),
            },
            // Also accept raw values as implicit Some
            other => Ok(Some(T::from_ast(other)?)),
        }
    }
}

impl<T: ToRon + ?Sized> ToRon for Box<T> {
    fn to_ron_value(&self) -> Result<Value> {
        (**self).to_ron_value()
    }
}

impl<T: FromRon> FromRon for Box<T> {
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        Ok(Box::new(T::from_ast(expr)?))
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
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        Ok(Cow::Owned(T::from_ast(expr)?))
    }
}

impl<T: ToRon> ToRon for Rc<T> {
    fn to_ron_value(&self) -> Result<Value> {
        (**self).to_ron_value()
    }
}

impl<T: FromRon> FromRon for Rc<T> {
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        Ok(Rc::new(T::from_ast(expr)?))
    }
}

impl<T: ToRon> ToRon for Arc<T> {
    fn to_ron_value(&self) -> Result<Value> {
        (**self).to_ron_value()
    }
}

impl<T: FromRon> FromRon for Arc<T> {
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        Ok(Arc::new(T::from_ast(expr)?))
    }
}

impl<T: ToRon + Copy> ToRon for Cell<T> {
    fn to_ron_value(&self) -> Result<Value> {
        self.get().to_ron_value()
    }
}

impl<T: FromRon> FromRon for Cell<T> {
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        Ok(Cell::new(T::from_ast(expr)?))
    }
}

impl<T: ToRon> ToRon for RefCell<T> {
    fn to_ron_value(&self) -> Result<Value> {
        self.borrow().to_ron_value()
    }
}

impl<T: FromRon> FromRon for RefCell<T> {
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        Ok(RefCell::new(T::from_ast(expr)?))
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
            fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
                let elements = match extract_seq_elements(expr) {
                    Some(e) => e,
                    None => return Err(spanned_type_mismatch("tuple", expr)),
                };
                #[allow(unused_variables)]
                let expected = impl_from_ron_tuple!(@count $first $(, $rest)*);
                if elements.len() != expected {
                    return Err(spanned_err(
                        Error::invalid_value(format!(
                            "expected tuple of {expected} elements, got {}",
                            elements.len()
                        )),
                        expr,
                    ));
                }
                let mut iter = elements.into_iter();
                Ok((
                    $first::from_ast(iter.next().ok_or_else(|| spanned_err(Error::invalid_value("tuple too short"), expr))?)?,
                    $($rest::from_ast(iter.next().ok_or_else(|| spanned_err(Error::invalid_value("tuple too short"), expr))?)?,)*
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
/// use ron2::ast::Expr;
/// use ron2::error::{SpannedResult, SpannedError};
///
/// // Manually implementing FromRon for a struct
/// struct Point { x: i32, y: i32 }
///
/// impl FromRon for Point {
///     fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
///         let value = ron2::ast::expr_to_value(expr).map_err(|e| SpannedError {
///             code: e,
///             span: expr.span().clone(),
///         })?;
///         Self::from_ron_value(value).map_err(|e| SpannedError {
///             code: e,
///             span: expr.span().clone(),
///         })
///     }
///
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
