//! Type conversion traits for RON.
//!
//! This module provides [`ToRon`] and [`FromRon`] traits for converting
//! between Rust types and RON values, without requiring serde.
//!
//! # Example
//!
//! ```
//! use ron2::{ToRon, FromRon, fmt::FormatConfig};
//!
//! // Serialize to RON
//! let numbers = vec![1, 2, 3];
//! let ron_string = numbers.to_ron_with(&FormatConfig::minimal()).unwrap();
//! assert_eq!(ron_string, "[1,2,3]");
//!
//! // Deserialize from RON; errors carry the source location
//! let parsed: Vec<i32> = Vec::from_ron("[1, 2, 3]").unwrap();
//! assert_eq!(parsed, vec![1, 2, 3]);
//!
//! let err = Vec::<i32>::from_ron("[1, \"two\"]").unwrap_err();
//! assert_eq!(err.to_string(), "1:5-1:10: in element 1: expected i32 but found string");
//! ```

mod impls_collection;
mod impls_primitive;
mod impls_wrapper;
mod map_access;
pub(crate) mod number;
mod spanned;

use alloc::{borrow::Cow, string::String, vec::Vec};
use core::{iter::Enumerate, slice};
use std::io::Read;

pub use map_access::AstMapAccess;
pub(crate) use map_access::check_unique_fields;
pub use spanned::Spanned;

use crate::{
    Value,
    ast::{
        Attribute, Document, Expr, MapExpr, SeqItem, Trivia, TupleElement, expr_to_value,
        parse_document, value_to_expr,
    },
    error::{Error, ErrorKind, Result},
    fmt::{FormatConfig, format_document, format_expr},
};

/// Trait for types that can be converted to RON.
///
/// The core method is [`to_ast`](ToRon::to_ast), which converts to an AST expression.
/// All other methods derive from this.
///
/// # Example
///
/// ```
/// use ron2::{ToRon, fmt::FormatConfig};
///
/// let value = vec![1, 2, 3];
/// assert_eq!(value.to_ron_with(&FormatConfig::minimal()).unwrap(), "[1,2,3]");
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
    /// use ron2::{ToRon, fmt::FormatConfig};
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
        expr_to_value(&self.to_ast()?)
    }

    /// Convert this value to a typed RON document.
    ///
    /// If `include_type_attribute` is true in the config, the document will
    /// include a `#![type = "..."]` attribute using [`core::any::type_name`].
    fn to_typed_document(&self, config: &SerializeConfig) -> Result<Document<'static>>
    where
        Self: Sized,
    {
        let mut doc = Document {
            source: Cow::Borrowed(""),
            leading: Trivia::empty(),
            attributes: Vec::new(),
            pre_value: Trivia::empty(),
            value: Some(self.to_ast()?),
            trailing: Trivia::empty(),
        };

        if config.include_type_attribute {
            let type_path = core::any::type_name::<Self>();
            doc.attributes.push(Attribute::synthetic_type(type_path));
        }

        Ok(doc)
    }

    /// Convert this value to a typed RON string with default settings.
    ///
    /// This includes the `#![type = "..."]` attribute.
    ///
    /// # Example
    ///
    /// ```
    /// use ron2::{Ron, ToRon};
    ///
    /// #[derive(Ron)]
    /// struct Config {
    ///     port: u16,
    /// }
    ///
    /// let ron = Config { port: 8080 }.to_typed_ron()?;
    /// assert!(ron.starts_with("#![type = \""));
    /// assert!(ron.contains("port: 8080"));
    /// # Ok::<(), ron2::Error>(())
    /// ```
    fn to_typed_ron(&self) -> Result<String>
    where
        Self: Sized,
    {
        self.to_typed_ron_with(&SerializeConfig::default())
    }

    /// Convert this value to a typed RON string with custom settings.
    fn to_typed_ron_with(&self, config: &SerializeConfig) -> Result<String>
    where
        Self: Sized,
    {
        let doc = self.to_typed_document(config)?;
        Ok(format_document(&doc, &config.format))
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
    fn from_ast(expr: &Expr<'_>) -> Result<Self>;

    /// Construct this type from a RON [`Value`].
    ///
    /// The value is converted to an AST expression with synthetic spans, so
    /// errors carry a [path](Error::path) but no source location
    /// ([`Span::is_synthetic`](crate::error::Span::is_synthetic) is true).
    ///
    /// ```
    /// use ron2::{FromRon, Value};
    ///
    /// let value = Value::from_ron("(port: \"eighty\")").unwrap();
    /// let err = std::collections::HashMap::<String, u16>::from_ron_value(value).unwrap_err();
    /// assert!(err.span().is_synthetic());
    /// ```
    fn from_ron_value(value: Value) -> Result<Self> {
        Self::from_ast(&value_to_expr(&value)?)
    }

    /// Parse a RON string and construct this type.
    ///
    /// Returns a [`Result`] with precise error location information.
    fn from_ron(s: &str) -> Result<Self> {
        let doc = parse_document(s)?;
        match doc.value {
            Some(ref expr) => Self::from_ast(expr),
            None => Err(Error::at_start(ErrorKind::Eof)),
        }
    }

    /// Read RON from a reader and construct this type.
    fn from_ron_reader<R: Read>(mut reader: R) -> Result<Self> {
        let mut buf = String::new();
        reader.read_to_string(&mut buf).map_err(Error::from)?;
        Self::from_ron(&buf)
    }
}

/// Trait for types that can be deserialized from struct fields.
///
/// This trait is used by the `#[ron(flatten)]` attribute to allow a struct's
/// fields to be merged into a parent struct. Types implementing this trait
/// consume fields from an [`AstMapAccess`] rather than requiring a
/// separate struct expression.
///
/// The `FromRon` derive implements it for named structs; `Option<T>` and
/// `HashMap` (a catch-all for the remaining fields) implement it as well.
///
/// # Example
///
/// ```
/// use ron2::FromRon;
///
/// #[derive(FromRon)]
/// struct Inner {
///     value: i32,
/// }
///
/// #[derive(FromRon)]
/// struct Outer {
///     name: String,
///     #[ron(flatten)]
///     inner: Inner,
/// }
///
/// // Inner's fields appear directly in Outer, not as `inner: (value: 42)`.
/// let outer = Outer::from_ron(r#"(name: "test", value: 42)"#)?;
/// assert_eq!(outer.inner.value, 42);
/// # Ok::<(), ron2::Error>(())
/// ```
pub trait FromRonFields: Sized {
    /// Whether this type takes every field nobody else claimed (maps).
    ///
    /// Derived structs read catch-all flattened fields last, so that named
    /// fields get their keys regardless of declaration order.
    const CATCH_ALL: bool = false;

    /// Consume fields from an existing map access to construct this type.
    fn from_fields(access: &mut AstMapAccess<'_>) -> Result<Self>;

    /// Appends the RON field names this type reads to `names`.
    ///
    /// Used to list every accepted field when a parent with
    /// `#[ron(deny_unknown_fields)]` rejects an unknown one, and to decide
    /// whether a flattened `Option<Self>` is present. Types that accept
    /// arbitrary fields (such as maps) add nothing.
    fn field_names(_names: &mut Vec<&'static str>) {}
}

/// The fields of a struct, in the form [`ToRonFields::to_fields`] appends to.
pub type StructFieldList = Vec<(Cow<'static, str>, Expr<'static>)>;

/// Trait for types that can be serialized as a list of struct fields.
///
/// This is the serialization counterpart of [`FromRonFields`], used by
/// `#[ron(flatten)]`: the flattened value appends its fields to the parent's
/// field list. The `ToRon` derive implements it for named structs.
pub trait ToRonFields {
    /// Whether this type holds arbitrary extra fields (maps). Derived structs
    /// write these after all other fields.
    const CATCH_ALL: bool = false;

    /// Append this value's fields to `fields`.
    fn to_fields(&self, fields: &mut StructFieldList) -> Result<()>;
}

impl<T: ToRonFields> ToRonFields for Option<T> {
    const CATCH_ALL: bool = T::CATCH_ALL;

    fn to_fields(&self, fields: &mut StructFieldList) -> Result<()> {
        match self {
            Some(value) => value.to_fields(fields),
            None => Ok(()),
        }
    }
}

/// Append map entries as fields; keys must serialize to strings.
fn map_to_fields<'m, K: ToRon + 'm, V: ToRon + 'm>(
    entries: impl ExactSizeIterator<Item = (&'m K, &'m V)>,
    fields: &mut StructFieldList,
) -> Result<()> {
    fields.reserve(entries.len());
    for (key, value) in entries {
        let Expr::String(key) = key.to_ast()? else {
            return Err(Error::new(ErrorKind::Message(
                "flattened map keys must serialize to strings".into(),
            )));
        };
        fields.push((Cow::Owned(key.value), value.to_ast()?));
    }
    Ok(())
}

impl<K: ToRon, V: ToRon, S> ToRonFields for std::collections::HashMap<K, V, S> {
    const CATCH_ALL: bool = true;

    fn to_fields(&self, fields: &mut StructFieldList) -> Result<()> {
        map_to_fields(self.iter(), fields)
    }
}

impl<K: ToRon, V: ToRon> ToRonFields for alloc::collections::BTreeMap<K, V> {
    const CATCH_ALL: bool = true;

    fn to_fields(&self, fields: &mut StructFieldList) -> Result<()> {
        map_to_fields(self.iter(), fields)
    }
}

impl<K: ToRon, V: ToRon, S> ToRonFields for indexmap::IndexMap<K, V, S> {
    const CATCH_ALL: bool = true;

    fn to_fields(&self, fields: &mut StructFieldList) -> Result<()> {
        map_to_fields(self.iter(), fields)
    }
}

// =============================================================================
// Shared helpers for the impls
// =============================================================================

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
        Expr::Error(_) => "error",
    }
}

/// Create a spanned type mismatch error.
pub(crate) fn spanned_type_mismatch(expected: &str, expr: &Expr<'_>) -> Error {
    Error::with_span(
        ErrorKind::TypeMismatch {
            expected: expected.into(),
            found: expr_type_name(expr).into(),
        },
        *expr.span(),
    )
}

/// The elements of a sequence-like expression (`[a, b]` or `(a, b)`) with
/// their indices.
pub(crate) enum SeqElements<'e, 'a> {
    Seq(Enumerate<slice::Iter<'e, SeqItem<'a>>>),
    Tuple(Enumerate<slice::Iter<'e, TupleElement<'a>>>),
}

impl<'e, 'a> Iterator for SeqElements<'e, 'a> {
    type Item = (usize, &'e Expr<'a>);

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            SeqElements::Seq(items) => items.next().map(|(i, item)| (i, &item.expr)),
            SeqElements::Tuple(items) => items.next().map(|(i, item)| (i, &item.expr)),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            SeqElements::Seq(items) => items.size_hint(),
            SeqElements::Tuple(items) => items.size_hint(),
        }
    }
}

impl ExactSizeIterator for SeqElements<'_, '_> {}

/// The elements of a sequence or tuple, or a type mismatch naming `expected`.
pub(crate) fn seq_elements<'e, 'a>(
    expr: &'e Expr<'a>,
    expected: &str,
) -> Result<SeqElements<'e, 'a>> {
    match expr {
        Expr::Seq(seq) => Ok(SeqElements::Seq(seq.items.iter().enumerate())),
        Expr::Tuple(tuple) => Ok(SeqElements::Tuple(tuple.elements.iter().enumerate())),
        _ => Err(spanned_type_mismatch(expected, expr)),
    }
}

/// Deserialize one element, adding its index to error paths.
pub(crate) fn element<T: FromRon>((index, expr): (usize, &Expr<'_>)) -> Result<T> {
    T::from_ast(expr).map_err(|e| e.in_element(index))
}

/// Deserialize a sequence or tuple into any collection.
pub(crate) fn collect_seq<T: FromRon, C: FromIterator<T>>(expr: &Expr<'_>) -> Result<C> {
    seq_elements(expr, "sequence")?.map(element).collect()
}

/// Fail unless a sequence has exactly `expected` elements.
pub(crate) fn check_len(
    elements: &SeqElements<'_, '_>,
    expected: usize,
    context: &'static str,
    expr: &Expr<'_>,
) -> Result<()> {
    let found = elements.len();
    if found == expected {
        return Ok(());
    }
    Err(Error::with_span(
        ErrorKind::LengthMismatch {
            expected: alloc::format!("{expected} elements"),
            found,
            context: Some(context),
        },
        *expr.span(),
    ))
}

/// Render a map key for error messages.
fn describe_key(key: &Expr<'_>) -> String {
    format_expr(key, &FormatConfig::minimal())
}

/// The map expression, or a type mismatch.
pub(crate) fn expect_map<'e, 'a>(expr: &'e Expr<'a>) -> Result<&'e MapExpr<'a>> {
    match expr {
        Expr::Map(map) => Ok(map),
        _ => Err(spanned_type_mismatch("map", expr)),
    }
}

/// Deserialize map entries into `insert`, which returns the previous value
/// for a repeated key. Repeated keys are an error.
pub(crate) fn read_map_entries<K: FromRon, V: FromRon>(
    map: &MapExpr<'_>,
    mut insert: impl FnMut(K, V) -> Option<V>,
) -> Result<()> {
    for entry in &map.entries {
        let key = K::from_ast(&entry.key).map_err(Error::in_map_key)?;
        let value =
            V::from_ast(&entry.value).map_err(|e| e.in_map_value(describe_key(&entry.key)))?;
        if insert(key, value).is_some() {
            return Err(Error::with_span(
                ErrorKind::DuplicateKey {
                    key: describe_key(&entry.key),
                },
                *entry.key.span(),
            ));
        }
    }
    Ok(())
}

// =============================================================================
// Value
// =============================================================================

impl ToRon for Value {
    fn to_ast(&self) -> Result<Expr<'static>> {
        value_to_expr(self)
    }

    fn to_ron_value(&self) -> Result<Value> {
        Ok(self.clone())
    }
}

impl FromRon for Value {
    fn from_ast(expr: &Expr<'_>) -> Result<Self> {
        expr_to_value(expr)
    }

    fn from_ron_value(value: Value) -> Result<Self> {
        Ok(value)
    }
}

// =============================================================================
// SerializeConfig
// =============================================================================

/// Configuration for serializing typed RON documents.
///
/// This controls how types are serialized to RON format, including whether
/// to include the `#![type = "..."]` attribute for LSP support.
///
/// # Example
///
/// ```
/// use ron2::{Ron, SerializeConfig, ToRon};
///
/// #[derive(Ron)]
/// struct Config {
///     port: u16,
/// }
///
/// let config = Config { port: 8080 };
///
/// // With type attribute (default): `#![type = "..."]` followed by the value
/// let ron = config.to_typed_ron()?;
/// assert!(ron.starts_with("#![type = "));
///
/// // Without type attribute
/// let ron = config.to_typed_ron_with(&SerializeConfig::without_type_attribute())?;
/// assert!(ron.starts_with("Config("));
/// # Ok::<(), ron2::Error>(())
/// ```
#[derive(Clone, Debug)]
pub struct SerializeConfig {
    /// Format configuration for the output.
    pub format: FormatConfig,
    /// Whether to include the `#![type = "..."]` attribute.
    pub include_type_attribute: bool,
}

impl Default for SerializeConfig {
    fn default() -> Self {
        Self {
            format: FormatConfig::default(),
            include_type_attribute: true,
        }
    }
}

impl SerializeConfig {
    /// Create a new `SerializeConfig` with default settings.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a config that excludes the type attribute.
    #[must_use]
    pub fn without_type_attribute() -> Self {
        Self {
            include_type_attribute: false,
            ..Default::default()
        }
    }

    /// Set the format configuration.
    #[must_use]
    pub fn format(mut self, format: FormatConfig) -> Self {
        self.format = format;
        self
    }

    /// Set whether to include the type attribute.
    #[must_use]
    pub fn type_attribute(mut self, include: bool) -> Self {
        self.include_type_attribute = include;
        self
    }
}

#[cfg(test)]
mod tests {
    use alloc::{
        collections::BTreeMap,
        string::{String, ToString},
        vec,
        vec::Vec,
    };
    use std::collections::HashMap;

    use indexmap::{IndexMap, IndexSet};

    use super::*;
    use crate::{
        PathSegment,
        ast::{NumberExpr, NumberKind},
        error::Span,
        value::{F32, F64, Number},
    };

    fn minimal(v: &impl ToRon) -> String {
        v.to_ron_with(&FormatConfig::minimal()).unwrap()
    }

    fn number(raw: &str) -> Expr<'_> {
        let kind = if raw.contains(['.', 'e', 'n', 'N']) && !raw.starts_with("0x") {
            NumberKind::Float
        } else if raw.starts_with('-') {
            NumberKind::NegativeInteger
        } else {
            NumberKind::Integer
        };
        Expr::Number(NumberExpr {
            span: Span::synthetic(),
            raw: raw.into(),
            kind,
        })
    }

    #[test]
    fn test_primitives_round_trip() {
        assert_eq!(minimal(&true), "true");
        assert_eq!(minimal(&42i32), "42");
        assert_eq!(minimal(&(-10i8)), "-10");
        assert_eq!(minimal(&'a'), "'a'");
        assert_eq!(minimal(&"hello"), "\"hello\"");
        assert_eq!(minimal(&()), "()");

        assert!(bool::from_ron("true").unwrap());
        assert_eq!(i32::from_ron("-10").unwrap(), -10);
        assert_eq!(char::from_ron("'a'").unwrap(), 'a');
        assert_eq!(String::from_ron("\"hello\"").unwrap(), "hello");
        assert_eq!(<()>::from_ron("()").unwrap(), ());
    }

    #[test]
    fn test_collections_and_tuples() {
        assert_eq!(minimal(&vec![1, 2, 3]), "[1,2,3]");
        assert_eq!(minimal(&Vec::<i32>::new()), "[]");
        assert_eq!(minimal(&(1, "a")), "(1,\"a\")");
        assert_eq!(minimal(&Some(42)), "Some(42)");
        assert_eq!(minimal(&Option::<i32>::None), "None");

        assert_eq!(Vec::<i32>::from_ron("[1, 2, 3]").unwrap(), vec![1, 2, 3]);
        assert_eq!(<[i32; 3]>::from_ron("[1, 2, 3]").unwrap(), [1, 2, 3]);
        assert_eq!(
            <(i32, String, bool)>::from_ron("(1, \"hello\", true)").unwrap(),
            (1, "hello".to_string(), true)
        );
        assert_eq!(Option::<i32>::from_ron("Some(42)").unwrap(), Some(42));
        assert_eq!(Option::<i32>::from_ron("None").unwrap(), None);
        assert_eq!(Option::<i32>::from_ron("42").unwrap(), Some(42));
    }

    #[test]
    fn test_maps_and_sets_keep_order() {
        let ron = r#"{"b": 2, "a": 1, "c": 3}"#;
        let parsed: IndexMap<String, i32> = IndexMap::from_ron(ron).unwrap();
        assert_eq!(parsed.keys().collect::<Vec<_>>(), ["b", "a", "c"]);
        assert_eq!(minimal(&parsed), ron.replace(' ', ""));

        let parsed: BTreeMap<String, i32> = BTreeMap::from_ron(ron).unwrap();
        assert_eq!(minimal(&parsed), r#"{"a":1,"b":2,"c":3}"#);

        let set: IndexSet<i32> = IndexSet::from_ron("[3, 1, 2]").unwrap();
        assert_eq!(set.iter().copied().collect::<Vec<_>>(), [3, 1, 2]);
        assert_eq!(minimal(&set), "[3,1,2]");
    }

    #[test]
    fn test_length_mismatch() {
        let err = <[i32; 3]>::from_ron("[1, 2]").unwrap_err();
        assert_eq!(
            err.to_string(),
            "1:1-1:7: array expected 3 elements but found 2 elements"
        );
        let err = <(i32, i32)>::from_ron("(1, 2, 3)").unwrap_err();
        assert!(matches!(
            err.kind(),
            ErrorKind::LengthMismatch {
                found: 3,
                context: Some("tuple"),
                ..
            }
        ));
    }

    #[test]
    fn test_spanned_error_line_numbers() {
        let err = i32::from_ron(r#""not a number""#).unwrap_err();
        assert_eq!(
            (err.span().start.line, err.span().start.col),
            (1, 1),
            "{err}"
        );
        assert_eq!((err.span().end.line, err.span().end.col), (1, 15));

        let err = Vec::<i32>::from_ron("[\n    1,\n    \"wrong\",\n    3\n]").unwrap_err();
        assert_eq!((err.span().start.line, err.span().start.col), (3, 5));
        assert_eq!((err.span().end.line, err.span().end.col), (3, 12));

        let err = <(i32, i32)>::from_ron("(\n    100,\n    \"not an int\"\n)").unwrap_err();
        assert_eq!(err.span().start.line, 3);

        let err = i8::from_ron("128").unwrap_err();
        assert_eq!((err.span().start.line, err.span().start.col), (1, 1));

        let err = bool::from_ron("42").unwrap_err();
        assert!(matches!(err.kind(), ErrorKind::TypeMismatch { .. }));
    }

    #[test]
    fn test_errors_carry_path() {
        let err = Vec::<(i32, i32)>::from_ron("[(1, 2), (3, \"x\")]").unwrap_err();
        assert_eq!(
            err.path(),
            &[PathSegment::Element(1), PathSegment::Element(1)]
        );

        let err = BTreeMap::<String, i32>::from_ron("{\n    \"a\": 1,\n    \"b\": \"wrong\"\n}")
            .unwrap_err();
        assert_eq!((err.span().start.line, err.span().start.col), (3, 10));
        assert_eq!(err.path(), &[PathSegment::MapValue("\"b\"".into())]);

        let err = BTreeMap::<i32, i32>::from_ron("{\"k\": 1}").unwrap_err();
        assert_eq!(err.path(), &[PathSegment::MapKey]);

        // `from_ron_value` has no source locations, but keeps the path.
        let value = Value::from_ron("[1, 2, \"three\"]").unwrap();
        let err = Vec::<i32>::from_ron_value(value).unwrap_err();
        assert!(err.span().is_synthetic());
        assert_eq!(
            err.to_string(),
            "in element 2: expected i32 but found string"
        );
    }

    #[test]
    fn test_duplicate_map_keys_are_errors() {
        let ron = r#"{"a": 1, "a": 2}"#;
        for err in [
            HashMap::<String, i32>::from_ron(ron).unwrap_err(),
            BTreeMap::<String, i32>::from_ron(ron).unwrap_err(),
            IndexMap::<String, i32>::from_ron(ron).unwrap_err(),
            Value::from_ron(ron).unwrap_err(),
        ] {
            assert_eq!(
                err.kind(),
                &ErrorKind::DuplicateKey {
                    key: "\"a\"".into()
                }
            );
            assert_eq!(err.span().start.col, 10);
        }

        let err = Value::from_ron("(a: 1, a: 2)").unwrap_err();
        assert!(matches!(err.kind(), ErrorKind::DuplicateField { .. }));
    }

    #[test]
    fn test_integer_literals() {
        assert!(i8::from_ron("127").is_ok());
        assert!(i8::from_ron("128").is_err());
        assert!(u8::from_ron("256").is_err());
        assert!(u8::from_ron("-1").is_err());
        assert_eq!(i32::from_ron("0x1_0").unwrap(), 16);
        assert_eq!(i32::from_ron("-0b101").unwrap(), -5);
        assert_eq!(u8::from_ron("+0x10").unwrap(), 16);
        assert_eq!(Value::from_ron("+0x10").unwrap(), Value::from(16u8));
        assert_eq!(u8::from_ron("5u8").unwrap(), 5);
        assert_eq!(
            Value::from_ron("5i32").unwrap(),
            Value::Number(Number::I32(5))
        );

        let err = u16::from_ron("300u8").unwrap_err();
        assert_eq!(
            err.kind(),
            &ErrorKind::IntegerOutOfBounds {
                value: "300u8".into(),
                target_type: "u8"
            }
        );
        let err = u8::from_ron("1.5").unwrap_err();
        assert_eq!(err.to_string(), "1:1-1:4: expected u8 but found float");
        assert!(i64::from_ron_value(Value::from(1.0)).is_err());
    }

    #[test]
    fn test_i64_extremes() {
        assert_eq!(i64::from_ron("-9223372036854775808").unwrap(), i64::MIN);
        assert_eq!(
            Value::from_ron("-9223372036854775808").unwrap(),
            Value::from(i64::MIN)
        );
        assert_eq!(u64::from_ron("18446744073709551615").unwrap(), u64::MAX);
    }

    #[cfg(feature = "integer128")]
    #[test]
    fn test_i128_extremes() {
        let min = "-170141183460469231731687303715884105728";
        assert_eq!(i128::from_ron(min).unwrap(), i128::MIN);
        assert_eq!(Value::from_ron(min).unwrap(), Value::from(i128::MIN));
        assert_eq!(
            u128::from_ron("340282366920938463463374607431768211455").unwrap(),
            u128::MAX
        );
        assert!(i128::from_ron("-170141183460469231731687303715884105729").is_err());
    }

    #[cfg(not(feature = "integer128"))]
    #[test]
    fn test_integers_beyond_64_bits_rejected_on_both_paths() {
        let big = "18446744073709551616";
        let typed = f64::from_ron(big).unwrap_err();
        let value = Value::from_ron(big).unwrap_err();
        assert_eq!(typed.kind(), value.kind());
        assert!(matches!(
            value.kind(),
            ErrorKind::IntegerOutOfBounds {
                target_type: "u64",
                ..
            }
        ));
    }

    #[test]
    fn test_float_literals() {
        assert_eq!(f64::from_ron("2.5").unwrap(), 2.5);
        assert_eq!(f64::from_ron("42").unwrap(), 42.0);
        assert_eq!(f64::from_ron("1_000.5").unwrap(), 1000.5);
        assert_eq!(f64::from_ron("-inf").unwrap(), f64::NEG_INFINITY);
        assert!(f32::from_ron("NaN").unwrap().is_nan());
        assert_eq!(f32::from_ron("1.5f32").unwrap(), 1.5);
        assert_eq!(
            Value::from_ron("1.5f32").unwrap(),
            Value::Number(Number::F32(F32(1.5)))
        );
        assert_eq!(
            Value::from_ron("1.5").unwrap(),
            Value::Number(Number::F64(F64(1.5)))
        );
    }

    #[test]
    fn test_hex_integers_as_floats_agree_with_value() {
        for raw in ["0x10", "-0x10", "0b11", "1_000", "+5"] {
            let typed = f64::from_ron(raw).unwrap();
            let via_value = f64::from_ron_value(Value::from_ron(raw).unwrap()).unwrap();
            assert_eq!(typed, via_value, "{raw}");
        }
        assert_eq!(f64::from_ron("0x10").unwrap(), 16.0);
    }

    #[test]
    fn test_f32_parses_without_double_rounding() {
        // Rounding to f64 first gives 1.0000000596046448, a tie that rounds to 1.0.
        assert_eq!(f32::from_ron("1.0000000596046448").unwrap(), 1.000_000_1);
        assert_eq!(f32::from_ron("16777217").unwrap(), 16_777_216.0);
    }

    #[test]
    fn test_float_overflow_is_error() {
        let err = f32::from_ron("1e40").unwrap_err();
        assert_eq!(
            err.kind(),
            &ErrorKind::FloatOutOfBounds {
                value: "1e40".into(),
                target_type: "f32"
            }
        );
        assert!(f64::from_ron("1e400").is_err());
        assert!(Value::from_ron("1e400").is_err());
        assert_eq!(f32::from_ron("1e38").unwrap(), 1e38);
        assert_eq!(f32::from_ron("inf").unwrap(), f32::INFINITY);
    }

    #[test]
    fn test_number_expr_edge_cases() {
        assert_eq!(i32::from_ast(&number("+5")).unwrap(), 5);
        assert!(i32::from_ast(&number("0x+5")).is_err());
        assert!(i32::from_ast(&number("--5")).is_err());
    }

    /// Float spellings the lexer accepts, parsed end to end.
    #[test]
    fn test_float_raw_forms() {
        let cases: [(&str, f64); 10] = [
            ("1.", 1.0),
            (".5", 0.5),
            ("-.5", -0.5),
            ("1.e3", 1000.0),
            ("1.5f32", 1.5),
            ("1.f32", 1.0),
            ("2e2f64", 200.0),
            ("+inf", f64::INFINITY),
            ("inff32", f64::INFINITY),
            ("-inf", f64::NEG_INFINITY),
        ];
        for (raw, expected) in cases {
            assert_eq!(f64::from_ron(raw).unwrap(), expected, "{raw}");
            #[allow(clippy::cast_possible_truncation)]
            let expected32 = expected as f32;
            assert_eq!(f32::from_ron(raw).unwrap(), expected32, "{raw}");
        }
        for raw in ["NaN", "-NaN", "NaNf32"] {
            assert!(f64::from_ron(raw).unwrap().is_nan(), "{raw}");
        }

        assert_eq!(
            Value::from_ron("1.f32").unwrap(),
            Value::Number(Number::F32(F32(1.0)))
        );
        assert_eq!(
            Value::from_ron("inff32").unwrap(),
            Value::Number(Number::F32(F32(f32::INFINITY)))
        );
        assert_eq!(
            Value::from_ron(".5").unwrap(),
            Value::Number(Number::F64(F64(0.5)))
        );
        assert!(u8::from_ron("1.").is_err());
    }

    /// Parse -> typed and parse -> `Value` -> typed agree on every literal form.
    #[test]
    fn test_literal_forms_agree_end_to_end() {
        let floats = [
            "1.",
            ".5",
            "1.e3",
            "-.5",
            "inff32",
            "-inf",
            "+inf",
            "-NaN",
            "NaN",
            "NaNf32",
            "1.5f32",
            "1.f32",
            "2e2f64",
            "16777217f32",
            "0.1",
            "1_000.5",
            "42",
            "-0x10",
            "0b1u8",
            "b'a'",
        ];
        for raw in floats {
            for (typed, via_value) in [
                (
                    f64::from_ron(raw).map(f64::to_bits),
                    Value::from_ron(raw)
                        .and_then(f64::from_ron_value)
                        .map(f64::to_bits),
                ),
                (
                    f32::from_ron(raw).map(|f| u64::from(f.to_bits())),
                    Value::from_ron(raw)
                        .and_then(f32::from_ron_value)
                        .map(|f| u64::from(f.to_bits())),
                ),
            ] {
                assert_eq!(typed.unwrap(), via_value.unwrap(), "{raw}");
            }
        }

        let integers = [
            "0b1u8", "0b101i16", "0o17u32", "0x1fi64", "0XFFu16", "-5i8", "+7u64", "1_000", "b'a'",
            "b'\\xff'",
        ];
        for raw in integers {
            let typed = i64::from_ron(raw).unwrap();
            let via_value = i64::from_ron_value(Value::from_ron(raw).unwrap()).unwrap();
            assert_eq!(typed, via_value, "{raw}");
        }
        assert_eq!(
            Value::from_ron("0b1u8").unwrap(),
            Value::Number(Number::U8(1))
        );
        assert_eq!(
            Value::from_ron("b'a'").unwrap(),
            Value::Number(Number::U8(97))
        );
    }

    #[test]
    fn test_paths_and_os_strings() {
        use std::{ffi::OsString, path::PathBuf};

        let path = PathBuf::from("dir/file.ron");
        assert_eq!(minimal(&path), "\"dir/file.ron\"");
        assert_eq!(minimal(&path.as_path()), "\"dir/file.ron\"");
        assert_eq!(PathBuf::from_ron("\"dir/file.ron\"").unwrap(), path);

        let os = OsString::from("name");
        assert_eq!(minimal(&os), "\"name\"");
        assert_eq!(OsString::from_ron("\"name\"").unwrap(), os);
        assert!(PathBuf::from_ron("42").is_err());

        #[cfg(unix)]
        {
            use std::os::unix::ffi::OsStringExt;
            let invalid = OsString::from_vec(vec![0xff]);
            let err = invalid.to_ast().unwrap_err();
            assert!(err.to_string().contains("UTF-8"), "{err}");
        }
    }

    #[test]
    fn test_byte_literals() {
        assert_eq!(u8::from_ron("b'a'").unwrap(), 97);
        assert_eq!(u64::from_ron("b'\\xff'").unwrap(), 255);
        assert_eq!(f64::from_ron("b'\\x07'").unwrap(), 7.0);
        assert_eq!(Value::from_ron("b'\\xff'").unwrap(), Value::from(255u8));

        let err = i8::from_ron("b'\\xff'").unwrap_err();
        assert_eq!(
            err.to_string(),
            "1:1-1:8: Integer b'\\xff' is out of bounds for i8"
        );
    }

    #[test]
    fn test_value_numbers_compare_numerically() {
        assert_eq!(Value::from_ron("1").unwrap(), Value::from(1i32));

        let map = Value::from_ron(r#"{1: "a", -1000: "b"}"#).unwrap();
        let Value::Map(map) = map else {
            panic!("expected map");
        };
        assert_eq!(map.get(&Value::from(1i32)), Some(&Value::from("a")));
        assert_eq!(map.get(&Value::from(-1000i64)), Some(&Value::from("b")));
        assert_eq!(map.get(&Value::from(1.0f64)), None);

        let mut values = Value::from_ron("[300, 2, 0.5, -1, -1000]").unwrap();
        if let Value::Seq(items) = &mut values {
            items.sort();
        }
        assert_eq!(values, Value::from_ron("[-1000, -1, 0.5, 2, 300]").unwrap());
    }

    #[test]
    fn test_f32_value_formats_at_f32_precision() {
        // A `Value` needs the suffix to read back as the same number; a typed
        // `f32` does not.
        assert_eq!(Value::from(0.1f32).to_ron().unwrap(), "0.1f32");
        assert_eq!(Value::from(0.1f32).to_string(), "0.1f32");
        assert_eq!(Value::from(0.5f32).to_string(), "0.5");
        assert_eq!(minimal(&0.1f32), "0.1");
        assert_eq!(f32::from_ron("0.1").unwrap(), 0.1f32);
        assert_eq!(
            f64::from_ron_value(Value::from(0.1f32)).unwrap(),
            f64::from(0.1f32)
        );
    }

    /// A float suffix fixes the literal's type for every target, so typed and
    /// `Value` conversion agree.
    #[test]
    fn test_float_suffix_fixes_type() {
        for raw in [
            "16777217f32",
            "0.1f32",
            "1e-50f32",
            "1.5f64",
            "-NaNf32",
            "3e38f32",
        ] {
            let typed = f64::from_ron(raw).unwrap();
            let via_value = f64::from_ron_value(Value::from_ron(raw).unwrap()).unwrap();
            assert_eq!(typed.to_bits(), via_value.to_bits(), "{raw}");
        }
        assert_eq!(f64::from_ron("16777217f32").unwrap(), 16_777_216.0);
        assert_eq!(f64::from_ron("1e-50f32").unwrap(), 0.0);

        for raw in ["1e39f32", "1e300f64"] {
            let typed = f32::from_ron(raw).unwrap_err();
            assert!(
                matches!(typed.kind(), ErrorKind::FloatOutOfBounds { .. }),
                "{raw}"
            );
        }
        let typed = f64::from_ron("1e39f32").unwrap_err();
        let value = Value::from_ron("1e39f32").unwrap_err();
        assert_eq!(typed.kind(), value.kind());
    }

    #[test]
    fn test_nan_sign_survives() {
        assert!(f32::from_ron("-NaN").unwrap().is_sign_negative());
        assert!(f64::from_ron("NaN").unwrap().is_sign_positive());
        assert_eq!(minimal(&-f64::NAN), "-NaN");
        assert!(
            f64::from_ron(&(-f64::NAN).to_ron().unwrap())
                .unwrap()
                .is_sign_negative()
        );

        let typed = f32::from_ron("-NaN").unwrap();
        let via_value = f32::from_ron_value(Value::from_ron("-NaN").unwrap()).unwrap();
        assert_eq!(typed.to_bits(), via_value.to_bits());
        assert_eq!(Value::from_ron("-NaN").unwrap(), Value::from(-f64::NAN));
        assert_ne!(Value::from_ron("-NaN").unwrap(), Value::from(f64::NAN));
    }

    #[test]
    fn test_large_floats_use_exponent() {
        assert_eq!(minimal(&1e308), "1e308");
        assert_eq!(minimal(&1e-7), "1e-7");
        assert_eq!(f64::from_ron("1e308").unwrap(), 1e308);
        let err = f32::from_ron_value(Value::from(1e40)).unwrap_err();
        assert_eq!(err.to_string(), "Float 1e40 is out of range for f32");
    }

    /// Value -> text -> Value is the identity, map keys included.
    #[test]
    fn test_value_text_round_trip() {
        let mut map = crate::Map::new();
        map.insert(Value::from(0.1f32), Value::from(1u8));
        map.insert(Value::from(0.1f64), Value::from(2u8));
        map.insert(Value::from(-f32::NAN), Value::from(3u8));
        map.insert(Value::from(f64::NAN), Value::from(4u8));
        map.insert(Value::from(1u8), Value::from(5u8));
        map.insert(Value::from(1.0f32), Value::from(6u8));
        let values = [
            Value::Map(map),
            Value::from(vec![
                Value::from(1e-45f32),
                Value::from(f32::MAX),
                Value::from(f64::MIN_POSITIVE),
                Value::from(-0.0f64),
                Value::from(-0.0f32),
                Value::from(i64::MIN),
                Value::from(u64::MAX),
                Value::from(16_777_217.0f64),
            ]),
        ];
        for value in values {
            for text in [value.to_string(), value.to_ron().unwrap()] {
                assert_eq!(Value::from_ron(&text).unwrap(), value, "{text}");
            }
        }
    }

    /// Every radix combines with every integer suffix, on both paths.
    #[test]
    fn test_suffixes_with_all_radixes() {
        let suffixes = ["i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64"];
        for (literal, value) in [
            ("0b101", 5),
            ("0o17", 15),
            ("0x1f", 31),
            ("0X1F", 31),
            ("42", 42),
        ] {
            for suffix in suffixes {
                for sign in ["", "+"] {
                    let raw = alloc::format!("{sign}{literal}{suffix}");
                    assert_eq!(i64::from_ron(&raw).unwrap(), value, "{raw}");
                    let via_value = i64::from_ron_value(Value::from_ron(&raw).unwrap());
                    assert_eq!(via_value.unwrap(), value, "{raw}");
                }
                if suffix.starts_with('i') {
                    let raw = alloc::format!("-{literal}{suffix}");
                    assert_eq!(i64::from_ron(&raw).unwrap(), -value, "{raw}");
                    assert_eq!(Value::from_ron(&raw).unwrap(), Value::from(-value), "{raw}");
                } else {
                    assert!(Value::from_ron(&alloc::format!("-{literal}{suffix}")).is_err());
                }
            }
        }
        assert_eq!(
            Value::from_ron("0b1u8").unwrap(),
            Value::Number(Number::U8(1))
        );
        assert_eq!(
            Value::from_ron("0xffu8").unwrap(),
            Value::Number(Number::U8(255))
        );
        assert!(Value::from_ron("0x100u8").is_err());
    }

    #[test]
    fn test_128_bit_suffixes_keep_signedness() {
        #[cfg(not(feature = "integer128"))]
        {
            assert_eq!(
                Value::from_ron("5i128").unwrap(),
                Value::Number(Number::I64(5))
            );
            assert_eq!(
                Value::from_ron("5u128").unwrap(),
                Value::Number(Number::U64(5))
            );
        }
        #[cfg(feature = "integer128")]
        {
            assert_eq!(
                Value::from_ron("5i128").unwrap(),
                Value::Number(Number::I128(5))
            );
            assert_eq!(
                Value::from_ron("5u128").unwrap(),
                Value::Number(Number::U128(5))
            );
        }
        let err = Value::from_ron("-5u128").unwrap_err();
        assert!(matches!(
            err.kind(),
            ErrorKind::IntegerOutOfBounds {
                target_type: "u128",
                ..
            }
        ));
        assert!(i64::from_ron("-5u128").is_err());
        assert_eq!(i64::from_ron("-5i128").unwrap(), -5);
    }

    #[test]
    fn test_value_to_ron_round_trip() {
        let value =
            Value::from_ron(r#"(a: [1, -2, 3.5], b: {"k": Some('c')}, c: b"\x00")"#).unwrap();
        assert_eq!(value.to_ron_value().unwrap(), value);
        assert_eq!(Value::from_ron(&value.to_ron().unwrap()).unwrap(), value);
        assert_eq!(Value::from_ron_value(value.clone()).unwrap(), value);

        let typed = vec![(1u8, "x".to_string())];
        assert_eq!(
            typed.to_ron_value().unwrap(),
            Value::from_ron(r#"[(1, "x")]"#).unwrap()
        );
    }

    #[test]
    fn test_ast_map_access_via_public_api() {
        let doc = parse_document(r#"(name: "test", value: 42)"#).unwrap();
        let Some(Expr::AnonStruct(s)) = &doc.value else {
            panic!("expected anonymous struct");
        };
        let mut access = AstMapAccess::from_anon(s, Some("TestStruct")).unwrap();
        assert_eq!(access.required::<String>("name").unwrap(), "test");
        assert_eq!(access.required::<i32>("value").unwrap(), 42);
        assert!(access.deny_unknown_fields(&["name", "value"]).is_ok());
    }

    #[test]
    fn test_typed_ron() {
        assert_eq!(42i32.to_typed_ron().unwrap(), "#![type = \"i32\"]\n\n42\n");

        let config = SerializeConfig::new()
            .type_attribute(false)
            .format(FormatConfig::minimal());
        assert_eq!(vec![1, 2, 3].to_typed_ron_with(&config).unwrap(), "[1,2,3]");

        let doc = 42i32
            .to_typed_document(&SerializeConfig::without_type_attribute())
            .unwrap();
        assert_eq!(doc.attributes.len(), 0);
        assert!(doc.value.is_some());
    }
}
