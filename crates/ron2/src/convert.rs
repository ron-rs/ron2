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
    ast::{
        AnonStructExpr, Expr, FieldsBody, NumberKind, StructField, parse_document, value_to_expr,
    },
    error::{Error, Result, Span, SpannedError, SpannedResult},
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
    /// then calls [`from_ast`](FromRon::from_ast).
    ///
    /// **Note:** Errors will have synthetic spans (line 0, column 0) since
    /// Values have no source position information. For precise error locations,
    /// use [`from_ron`](FromRon::from_ron) to parse from a string directly.
    /// You can check if a span is synthetic using [`Span::is_synthetic()`](crate::error::Span::is_synthetic).
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
///
/// Returns generic category names (e.g., "number" not "i32") to match
/// `expr_type_name` for consistent error messages across AST and Value paths.
fn value_type_name(value: &Value) -> &'static str {
    match value {
        Value::Bool(_) => "bool",
        Value::Char(_) => "char",
        Value::Number(_) => "number",
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
#[must_use]
pub fn expr_type_name(expr: &Expr<'_>) -> &'static str {
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

/// Result of parsing an integer from its raw string representation.
#[derive(Debug, Clone, Copy)]
pub struct ParsedInt {
    /// The absolute value of the parsed integer.
    pub magnitude: u128,
    /// Whether the integer was negative.
    pub negative: bool,
}

/// Parse an integer from its raw string representation.
///
/// Handles decimal, hex (0x), binary (0b), octal (0o), and underscores.
/// Returns the magnitude and sign separately for flexible fitting into target types.
pub fn parse_int_raw(raw: &str) -> Result<ParsedInt> {
    let raw = raw.trim();
    let (negative, unsigned_raw) = match raw.strip_prefix('-') {
        Some(r) => (true, r),
        None => (false, raw),
    };

    // Remove underscores
    let cleaned: String = unsigned_raw.chars().filter(|&c| c != '_').collect();

    // Determine base and strip prefix
    let (base, digits) = if let Some(d) = cleaned
        .strip_prefix("0x")
        .or_else(|| cleaned.strip_prefix("0X"))
    {
        (16, d)
    } else if let Some(d) = cleaned
        .strip_prefix("0b")
        .or_else(|| cleaned.strip_prefix("0B"))
    {
        (2, d)
    } else if let Some(d) = cleaned
        .strip_prefix("0o")
        .or_else(|| cleaned.strip_prefix("0O"))
    {
        (8, d)
    } else {
        (10, cleaned.as_str())
    };

    let magnitude = u128::from_str_radix(digits, base).map_err(|_| Error::IntegerOutOfBounds {
        value: raw.to_string().into(),
        target_type: "u128",
    })?;

    Ok(ParsedInt {
        magnitude,
        negative,
    })
}

/// Parse an integer from a raw string representation into a specific type.
fn parse_integer_from_raw<T>(raw: &str, kind: &NumberKind, target_type: &'static str) -> Result<T>
where
    T: TryFrom<i128> + TryFrom<u128>,
{
    let raw_trimmed = raw.trim();
    match kind {
        NumberKind::Integer => {
            let parsed = parse_int_raw(raw)?;
            T::try_from(parsed.magnitude).map_err(|_| Error::IntegerOutOfBounds {
                value: raw_trimmed.to_string().into(),
                target_type,
            })
        }
        NumberKind::NegativeInteger => {
            let parsed = parse_int_raw(raw)?;
            // Convert magnitude to signed and negate
            let val = i128::try_from(parsed.magnitude).map_err(|_| Error::IntegerOutOfBounds {
                value: raw_trimmed.to_string().into(),
                target_type,
            })?;
            let val = -val;
            T::try_from(val).map_err(|_| Error::IntegerOutOfBounds {
                value: raw_trimmed.to_string().into(),
                target_type,
            })
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
                            parse_integer_from_raw::<$ty>(&n.raw, &n.kind, stringify!($ty))
                                .map_err(|e| spanned_err(e, expr))
                        }
                        Expr::Byte(b) => {
                            // Byte literals can be converted to integers
                            <$ty>::try_from(b.value)
                                .map_err(|_| spanned_err(Error::IntegerOutOfBounds {
                                    value: format!("b'{}'", b.value as char).into(),
                                    target_type: stringify!($ty),
                                }, expr))
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
                let val =
                    parse_float_from_raw(&n.raw, &n.kind).map_err(|e| spanned_err(e, expr))?;
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
                let mut result =
                    HashMap::with_capacity_and_hasher(map.entries.len(), Default::default());
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
                let Some(elements) = extract_seq_elements(expr) else {
                    return Err(spanned_type_mismatch("tuple", expr));
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
// AstMapAccess helper for struct deserialization with span preservation
// =============================================================================

/// Helper struct for deserializing RON structs to Rust structs from AST.
///
/// Unlike Value-based deserialization, this preserves span information for
/// precise error reporting at the field level.
///
/// # Example
///
/// ```
/// use ron2::{AstMapAccess, FromRon};
/// use ron2::ast::{Expr, AnonStructExpr};
/// use ron2::error::SpannedResult;
///
/// // Manually implementing FromRon for a struct
/// struct Point { x: i32, y: i32 }
///
/// impl FromRon for Point {
///     fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
///         match expr {
///             Expr::AnonStruct(s) => {
///                 let mut access = AstMapAccess::from_anon(s);
///                 let x = access.required("x")?;
///                 let y = access.required("y")?;
///                 access.deny_unknown_fields(&["x", "y"])?;
///                 Ok(Point { x, y })
///             }
///             _ => Err(ron2::error::SpannedError {
///                 code: ron2::error::Error::ExpectedStructLike,
///                 span: expr.span().clone(),
///             })
///         }
///     }
/// }
/// ```
#[cfg(feature = "std")]
pub struct AstMapAccess<'a> {
    /// Map from field name to the field struct
    fields: HashMap<&'a str, &'a StructField<'a>>,
    /// Track which fields have been consumed
    consumed: HashSet<&'a str>,
    /// The span of the entire struct (for missing field errors)
    struct_span: Span,
    /// Optional struct name for error messages
    struct_name: Option<&'a str>,
}

#[cfg(feature = "std")]
impl<'a> AstMapAccess<'a> {
    /// Create from an anonymous struct expression: `(field: value, ...)`
    #[must_use]
    pub fn from_anon(s: &'a AnonStructExpr<'a>) -> Self {
        let mut fields = HashMap::with_capacity(s.fields.len());
        for field in &s.fields {
            fields.insert(field.name.name.as_ref(), field);
        }
        Self {
            fields,
            consumed: HashSet::new(),
            struct_span: s.span.clone(),
            struct_name: None,
        }
    }

    /// Create from named struct fields: `Name { field: value, ... }`
    #[must_use]
    pub fn from_fields(
        fields_body: &'a FieldsBody<'a>,
        struct_name: Option<&'a str>,
        struct_span: Span,
    ) -> Self {
        let mut fields = HashMap::with_capacity(fields_body.fields.len());
        for field in &fields_body.fields {
            fields.insert(field.name.name.as_ref(), field);
        }
        Self {
            fields,
            consumed: HashSet::new(),
            struct_span,
            struct_name,
        }
    }

    /// Get a required field.
    ///
    /// Returns an error with the field's span if deserialization fails,
    /// or the struct's span if the field is missing.
    pub fn required<T: FromRon>(&mut self, name: &'static str) -> SpannedResult<T> {
        match self.fields.get(name) {
            Some(field) => {
                self.consumed.insert(name);
                T::from_ast(&field.value)
            }
            None => Err(SpannedError {
                code: Error::MissingStructField {
                    field: Cow::Borrowed(name),
                    outer: self.struct_name.map(|s| Cow::Owned(s.to_string())),
                },
                span: self.struct_span.clone(),
            }),
        }
    }

    /// Get an optional field.
    ///
    /// Returns `Ok(None)` if the field is missing.
    pub fn optional<T: FromRon>(&mut self, name: &'static str) -> SpannedResult<Option<T>> {
        match self.fields.get(name) {
            Some(field) => {
                self.consumed.insert(name);
                Ok(Some(T::from_ast(&field.value)?))
            }
            None => Ok(None),
        }
    }

    /// Get a field with a default value if missing.
    pub fn with_default<T: FromRon + Default>(&mut self, name: &'static str) -> SpannedResult<T> {
        match self.fields.get(name) {
            Some(field) => {
                self.consumed.insert(name);
                T::from_ast(&field.value)
            }
            None => Ok(T::default()),
        }
    }

    /// Get a field with a custom default function.
    pub fn with_default_fn<T: FromRon, F: FnOnce() -> T>(
        &mut self,
        name: &'static str,
        default_fn: F,
    ) -> SpannedResult<T> {
        match self.fields.get(name) {
            Some(field) => {
                self.consumed.insert(name);
                T::from_ast(&field.value)
            }
            None => Ok(default_fn()),
        }
    }

    /// Check for unknown fields.
    ///
    /// Pass the list of expected field names. Returns an error pointing to
    /// the first unknown field's span.
    pub fn deny_unknown_fields(&self, expected: &'static [&'static str]) -> SpannedResult<()> {
        for (&name, &field) in &self.fields {
            if !self.consumed.contains(name) {
                return Err(SpannedError {
                    code: Error::NoSuchStructField {
                        expected,
                        found: Cow::Owned(name.to_string()),
                        outer: self.struct_name.map(|s| Cow::Owned(s.to_string())),
                    },
                    span: field.name.span.clone(),
                });
            }
        }
        Ok(())
    }

    /// Get an iterator over remaining (unconsumed) field names.
    pub fn remaining_keys(&self) -> impl Iterator<Item = &str> {
        self.fields
            .keys()
            .filter(|k| !self.consumed.contains(*k))
            .copied()
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
    fn test_ast_map_access() {
        use crate::ast::parse_document;

        let ron = r#"(name: "test", value: 42)"#;
        let doc = parse_document(ron).unwrap();

        // Get the anonymous struct from the document
        if let Some(Expr::AnonStruct(s)) = &doc.value {
            let mut access = AstMapAccess::from_anon(s);

            assert_eq!(access.required::<String>("name").unwrap(), "test");
            assert_eq!(access.required::<i32>("value").unwrap(), 42);
            assert!(access.deny_unknown_fields(&["name", "value"]).is_ok());
        } else {
            panic!("Expected anonymous struct");
        }
    }

    #[test]
    fn test_spanned_error_line_numbers() {
        // Type mismatch on line 1 - expected i32, got string
        let err = i32::from_ron(r#""not a number""#).unwrap_err();
        assert_eq!(err.span.start.line, 1);
        assert_eq!(err.span.start.col, 1);
        assert_eq!(err.span.end.line, 1);
        assert_eq!(err.span.end.col, 15);

        // Type mismatch on line 3 in a multiline document
        let input = r#"[
    1,
    "wrong",
    3
]"#;
        let err = Vec::<i32>::from_ron(input).unwrap_err();
        assert_eq!(err.span.start.line, 3);
        assert_eq!(err.span.start.col, 5);
        assert_eq!(err.span.end.line, 3);
        assert_eq!(err.span.end.col, 12);

        // Nested structure - tuple with wrong type in second position
        let input = r#"(
    100,
    "not an int"
)"#;
        let err = <(i32, i32)>::from_ron(input).unwrap_err();
        // The error should point to "not an int" on line 3
        assert_eq!(err.span.start.line, 3);

        // Integer out of range
        let err = i8::from_ron("128").unwrap_err();
        assert_eq!(err.span.start.line, 1);
        assert_eq!(err.span.start.col, 1);

        // Boolean type mismatch
        let err = bool::from_ron("42").unwrap_err();
        assert_eq!(err.span.start.line, 1);
        assert!(matches!(
            err.code,
            crate::error::Error::InvalidValueForType { .. }
        ));
    }

    #[test]
    fn test_spanned_error_in_map() {
        let input = r#"{
    "a": 1,
    "b": "wrong"
}"#;
        let err = BTreeMap::<String, i32>::from_ron(input).unwrap_err();
        // Error should point to "wrong" on line 3
        assert_eq!(err.span.start.line, 3);
        assert_eq!(err.span.start.col, 10);
    }

    #[test]
    fn test_from_ron_value_strips_span() {
        use crate::Value;
        // from_ron_value returns Result (not SpannedResult) since Value has no span
        let value = Value::String("not a number".to_string());
        let err = i32::from_ron_value(value).unwrap_err();
        // The error should still have the correct error code
        assert!(matches!(
            err,
            crate::error::Error::InvalidValueForType { .. }
        ));
    }
}
