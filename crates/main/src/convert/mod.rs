// Allow deprecated errors during transition period - internal code uses these variants
// while they remain available for backward compatibility
#![allow(deprecated)]

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

mod impls_collection;
mod impls_primitive;
mod impls_wrapper;
mod map_access;
pub mod number;

pub use map_access::AstMapAccess;
pub use number::{ParsedInt, parse_int_raw};

use alloc::string::String;
use std::io::Read;

use crate::{
    Value,
    ast::{Expr, FormatConfig, expr_to_value, format_expr, parse_document, value_to_expr},
    error::{Error, Result, SpannedError, SpannedResult},
};

/// Trait for types that can be converted to RON.
///
/// The core method is [`to_ast`](ToRon::to_ast), which converts to an AST expression.
/// All other methods derive from this.
///
/// # Example
///
/// ```
/// use ron2::ToRon;
///
/// let value = vec![1, 2, 3];
/// let ron_string = value.to_ron().unwrap();
/// assert!(ron_string.contains("1") && ron_string.contains("2") && ron_string.contains("3"));
/// ```
pub trait ToRon {
    /// Convert this value to a RON AST expression.
    ///
    /// This is the core method that all types must implement.
    fn to_ast(&self) -> Result<Expr<'static>>;

    /// Convert this value to a pretty-printed RON string (default format).
    fn to_ron(&self) -> Result<String> {
        Ok(format_expr(&self.to_ast()?, &FormatConfig::default()))
    }

    /// Convert this value to a RON string with custom formatting.
    ///
    /// # Example
    ///
    /// ```
    /// use ron2::{ToRon, ast::FormatConfig};
    ///
    /// let value = vec![1, 2, 3];
    /// let minimal = value.to_ron_with(&FormatConfig::minimal()).unwrap();
    /// assert_eq!(minimal, "[1,2,3]");
    /// ```
    fn to_ron_with(&self, config: &FormatConfig) -> Result<String> {
        Ok(format_expr(&self.to_ast()?, config))
    }

    /// Convert this value to a RON [`Value`] (via AST).
    fn to_ron_value(&self) -> Result<Value> {
        expr_to_value(&self.to_ast()?).map_err(|e| e.code)
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
    fn from_ron_reader<R: Read>(mut reader: R) -> SpannedResult<Self> {
        let mut buf = String::new();
        reader
            .read_to_string(&mut buf)
            .map_err(|e| SpannedError::at_start(Error::Io(e.to_string())))?;
        Self::from_ron(&buf)
    }
}

/// Trait for types that can be deserialized from struct fields.
///
/// This trait is used by the `#[ron(flatten)]` attribute to allow a struct's
/// fields to be merged into a parent struct. Types implementing this trait
/// can consume fields from an [`AstMapAccess`] rather than requiring a
/// separate struct expression.
///
/// This trait is automatically implemented by the `FromRon` derive macro
/// for named structs.
///
/// # Example
///
/// ```ignore
/// #[derive(FromRon)]
/// struct Inner {
///     value: i32,
/// }
///
/// #[derive(FromRon)]
/// struct Outer {
///     name: String,
///     #[ron(flatten)]
///     inner: Inner,  // Fields from Inner are merged into Outer
/// }
///
/// // RON input: (name: "test", value: 42)
/// // Not: (name: "test", inner: (value: 42))
/// ```
pub trait FromRonFields: Sized {
    /// Consume fields from an existing map access to construct this type.
    ///
    /// This is used for `#[ron(flatten)]` fields where the inner struct's
    /// fields appear directly in the parent struct.
    fn from_fields(access: &mut AstMapAccess<'_>) -> SpannedResult<Self>;
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
pub(crate) fn spanned_type_mismatch(expected: &str, expr: &Expr<'_>) -> SpannedError {
    SpannedError {
        code: Error::InvalidValueForType {
            expected: expected.into(),
            found: expr_type_name(expr).into(),
        },
        span: expr.span().clone(),
    }
}

/// Create a spanned error with the given code and expression's span.
pub(crate) fn spanned_err(code: Error, expr: &Expr<'_>) -> SpannedError {
    SpannedError {
        code,
        span: expr.span().clone(),
    }
}

// =============================================================================
// ToRon implementation for Value
// =============================================================================

impl ToRon for Value {
    fn to_ast(&self) -> Result<Expr<'static>> {
        // Clone self and convert to AST using value_to_expr
        Ok(crate::ast::value_to_expr(self.clone()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::FormatConfig;
    use alloc::collections::BTreeMap;
    use alloc::vec;
    use alloc::vec::Vec;

    /// Helper to get minimal (compact) output
    fn minimal(v: &impl ToRon) -> String {
        v.to_ron_with(&FormatConfig::minimal()).unwrap()
    }

    #[test]
    fn test_primitives_to_ron() {
        assert_eq!(minimal(&true), "true");
        assert_eq!(minimal(&false), "false");
        assert_eq!(minimal(&42i32), "42");
        assert_eq!(minimal(&(-10i8)), "-10");
        assert_eq!(minimal(&'a'), "'a'");
        assert_eq!(minimal(&"hello"), "\"hello\"");
        assert_eq!(minimal(&()), "()");
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
        // Note: ron2's Minimal format doesn't add spaces after commas
        assert_eq!(minimal(&vec![1, 2, 3]), "[1,2,3]");
        assert_eq!(minimal(&Vec::<i32>::new()), "[]");

        assert_eq!(Vec::<i32>::from_ron("[1, 2, 3]").unwrap(), vec![1, 2, 3]);
        assert_eq!(Vec::<i32>::from_ron("[]").unwrap(), Vec::<i32>::new());
    }

    #[test]
    fn test_option() {
        assert_eq!(minimal(&Some(42)), "Some(42)");
        assert_eq!(minimal(&Option::<i32>::None), "None");

        assert_eq!(Option::<i32>::from_ron("Some(42)").unwrap(), Some(42));
        assert_eq!(Option::<i32>::from_ron("None").unwrap(), None);
        // Raw value as implicit Some
        assert_eq!(Option::<i32>::from_ron("42").unwrap(), Some(42));
    }

    #[test]
    fn test_tuple() {
        // ron2 serializes tuples as (a, b)
        let tuple_ron = minimal(&(1, 2));
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
        let ron = minimal(&map);
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

    #[test]
    fn test_ast_map_access() {
        use crate::ast::parse_document;

        let ron = r#"(name: "test", value: 42)"#;
        let doc = parse_document(ron).unwrap();

        // Get the anonymous struct from the document
        if let Some(Expr::AnonStruct(s)) = &doc.value {
            let mut access = AstMapAccess::from_anon(s, Some("TestStruct")).unwrap();

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

    #[test]
    fn test_indexmap() {
        use indexmap::IndexMap;

        // ToRon
        let mut map: IndexMap<String, i32> = IndexMap::default();
        map.insert("first".to_string(), 1);
        map.insert("second".to_string(), 2);
        let ron = map.to_ron().unwrap();
        assert!(ron.contains("\"first\""));
        assert!(ron.contains("\"second\""));

        // FromRon - order should be preserved
        let ron = r#"{"a": 1, "b": 2, "c": 3}"#;
        let parsed: IndexMap<String, i32> = IndexMap::from_ron(ron).unwrap();
        let keys: Vec<_> = parsed.keys().collect();
        assert_eq!(keys, vec!["a", "b", "c"]);
        assert_eq!(parsed.get("a"), Some(&1));
        assert_eq!(parsed.get("b"), Some(&2));
        assert_eq!(parsed.get("c"), Some(&3));
    }

    #[test]
    fn test_indexset() {
        use indexmap::IndexSet;

        // ToRon
        let mut set: IndexSet<i32> = IndexSet::default();
        set.insert(3);
        set.insert(1);
        set.insert(2);
        let ron = set.to_ron().unwrap();
        assert!(ron.contains('3'));
        assert!(ron.contains('1'));
        assert!(ron.contains('2'));

        // FromRon - order should be preserved
        let ron = "[3, 1, 2]";
        let parsed: IndexSet<i32> = IndexSet::from_ron(ron).unwrap();
        let values: Vec<_> = parsed.iter().copied().collect();
        assert_eq!(values, vec![3, 1, 2]);
    }
}
