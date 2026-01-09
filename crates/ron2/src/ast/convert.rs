//! Conversion between AST and Value.
//!
//! This module provides bidirectional conversion:
//!
//! **AST → Value** (`expr_to_value`):
//! - Discards span information, trivia, and raw text representations
//! - Struct/enum names are preserved in `Value::Named`
//!
//! **Value → AST** (`value_to_expr`):
//! - Creates AST with synthetic spans (line 0 to distinguish from real spans)
//! - Generates raw text representations for numbers, strings, etc.
//! - Useful for `FromRon::from_ron_value()` default implementation

use alloc::{borrow::Cow, boxed::Box, format, string::String, vec::Vec};

use crate::ast::{
    AnonStructExpr, BoolExpr, BytesExpr, BytesKind, CharExpr, Document, Expr, FieldsBody, Ident,
    MapEntry, MapExpr, NumberExpr, NumberKind, OptionExpr, OptionValue, SeqExpr, SeqItem,
    StringExpr, StringKind, StructBody, StructExpr, StructField, Trivia, TupleBody, TupleElement,
    TupleExpr, UnitExpr,
};
use crate::error::{Error, Result, Span, SpannedError, SpannedResult};
use crate::value::{F32, F64, NamedContent, Number, StructFields, Value};

/// Extension trait to attach span information to `Result` errors.
trait SpanExt<T> {
    /// Convert a `Result<T, Error>` to `SpannedResult<T>` by attaching a span.
    fn with_span(self, span: &Span) -> SpannedResult<T>;
}

impl<T> SpanExt<T> for Result<T> {
    fn with_span(self, span: &Span) -> SpannedResult<T> {
        self.map_err(|code| SpannedError {
            code,
            span: span.clone(),
        })
    }
}

/// Convert an AST document to a Value.
///
/// Returns `None` if the document has no value (empty document or comments only).
/// Errors include span information for precise error reporting.
///
/// # Example
///
/// ```
/// use ron2::ast::{parse_document, to_value};
///
/// let doc = parse_document("42").unwrap();
/// let value = to_value(&doc).unwrap();
/// ```
pub fn to_value(doc: &Document<'_>) -> Option<SpannedResult<Value>> {
    doc.value.as_ref().map(expr_to_value)
}

/// Convert an AST expression to a Value.
///
/// Errors include the span of the expression that caused the error,
/// enabling precise error reporting with source location.
pub fn expr_to_value(expr: &Expr<'_>) -> SpannedResult<Value> {
    match expr {
        Expr::Unit(_) => Ok(Value::Unit),
        Expr::Bool(b) => Ok(Value::Bool(b.value)),
        Expr::Char(c) => Ok(Value::Char(c.value)),
        Expr::Byte(b) => Ok(byte_to_value(b)),
        Expr::Number(n) => number_to_value(n).with_span(&n.span),
        Expr::String(s) => Ok(string_to_value(s)),
        Expr::Bytes(b) => Ok(bytes_to_value(b)),
        Expr::Option(opt) => option_to_value(opt),
        Expr::Seq(seq) => seq_to_value(seq),
        Expr::Map(map) => map_to_value(map),
        Expr::Tuple(tuple) => tuple_to_value(tuple),
        Expr::AnonStruct(s) => anon_struct_to_value(s),
        Expr::Struct(s) => struct_to_value(s),
    }
}

fn byte_to_value(b: &crate::ast::ByteExpr<'_>) -> Value {
    // A byte literal b'x' becomes a Number(U8)
    Value::Number(Number::U8(b.value))
}

fn number_to_value(n: &NumberExpr<'_>) -> Result<Value> {
    // Parse the raw number text
    let raw = n.raw.trim();

    match n.kind {
        NumberKind::Integer | NumberKind::NegativeInteger => parse_integer(raw),
        NumberKind::Float => parse_float(raw),
        NumberKind::SpecialFloat => parse_special_float(raw),
    }
}

fn parse_integer(raw: &str) -> Result<Value> {
    let negative = raw.starts_with('-');
    let raw = if negative { &raw[1..] } else { raw };

    // Remove underscores
    let cleaned: String = raw.chars().filter(|&c| c != '_').collect();

    // Determine base
    let (base, digits) = if cleaned.starts_with("0x") || cleaned.starts_with("0X") {
        (16, &cleaned[2..])
    } else if cleaned.starts_with("0b") || cleaned.starts_with("0B") {
        (2, &cleaned[2..])
    } else if cleaned.starts_with("0o") || cleaned.starts_with("0O") {
        (8, &cleaned[2..])
    } else {
        (10, cleaned.as_str())
    };

    if negative {
        // Try to parse as signed
        let val = i128::from_str_radix(digits, base).map_err(|_| Error::IntegerOutOfBounds)?;
        let val = -val;
        Ok(fit_signed(val))
    } else {
        // Try to parse as unsigned
        let val = u128::from_str_radix(digits, base).map_err(|_| Error::IntegerOutOfBounds)?;
        Ok(fit_unsigned(val))
    }
}

/// Fit a signed value into the smallest Number variant
fn fit_signed(val: i128) -> Value {
    #[cfg(feature = "integer128")]
    if let Ok(v) = i64::try_from(val) {
        return fit_signed_64(v);
    } else {
        return Value::Number(Number::I128(val));
    }

    #[cfg(not(feature = "integer128"))]
    {
        // In non-integer128 mode, we truncate to i64 range
        // This is intentional - values outside i64 range will overflow
        #[allow(clippy::cast_possible_truncation)]
        fit_signed_64(val as i64)
    }
}

fn fit_signed_64(val: i64) -> Value {
    if let Ok(v) = i8::try_from(val) {
        Value::Number(Number::I8(v))
    } else if let Ok(v) = i16::try_from(val) {
        Value::Number(Number::I16(v))
    } else if let Ok(v) = i32::try_from(val) {
        Value::Number(Number::I32(v))
    } else {
        Value::Number(Number::I64(val))
    }
}

/// Fit an unsigned value into the smallest Number variant
fn fit_unsigned(val: u128) -> Value {
    #[cfg(feature = "integer128")]
    if let Ok(v) = u64::try_from(val) {
        return fit_unsigned_64(v);
    } else {
        return Value::Number(Number::U128(val));
    }

    #[cfg(not(feature = "integer128"))]
    {
        // In non-integer128 mode, we truncate to u64 range
        // This is intentional - values outside u64 range will overflow
        #[allow(clippy::cast_possible_truncation)]
        fit_unsigned_64(val as u64)
    }
}

fn fit_unsigned_64(val: u64) -> Value {
    if let Ok(v) = u8::try_from(val) {
        Value::Number(Number::U8(v))
    } else if let Ok(v) = u16::try_from(val) {
        Value::Number(Number::U16(v))
    } else if let Ok(v) = u32::try_from(val) {
        Value::Number(Number::U32(v))
    } else {
        Value::Number(Number::U64(val))
    }
}

fn parse_float(raw: &str) -> Result<Value> {
    // Remove underscores (not in exponent position as that would be invalid)
    let cleaned: String = raw.chars().filter(|&c| c != '_').collect();

    // Try to parse as f64
    let val: f64 = cleaned.parse().map_err(|_| Error::ExpectedFloat)?;

    // Check if it fits in f32 without loss
    // The cast is intentional - we're checking if f32 can represent this value
    #[allow(clippy::cast_possible_truncation)]
    let as_f32 = val as f32;
    if (f64::from(as_f32) - val).abs() < f64::EPSILON {
        Ok(Value::Number(Number::F32(F32(as_f32))))
    } else {
        Ok(Value::Number(Number::F64(F64(val))))
    }
}

fn parse_special_float(raw: &str) -> Result<Value> {
    match raw {
        "inf" => Ok(Value::Number(Number::F64(F64(f64::INFINITY)))),
        "-inf" => Ok(Value::Number(Number::F64(F64(f64::NEG_INFINITY)))),
        "NaN" => Ok(Value::Number(Number::F64(F64(f64::NAN)))),
        _ => Err(Error::ExpectedFloat),
    }
}

fn string_to_value(s: &StringExpr<'_>) -> Value {
    Value::String(s.value.clone())
}

fn bytes_to_value(b: &BytesExpr<'_>) -> Value {
    Value::Bytes(b.value.clone())
}

fn option_to_value(opt: &OptionExpr<'_>) -> SpannedResult<Value> {
    match &opt.value {
        Some(inner) => {
            let val = expr_to_value(&inner.expr)?;
            Ok(Value::Option(Some(Box::new(val))))
        }
        None => Ok(Value::Option(None)),
    }
}

fn seq_to_value(seq: &SeqExpr<'_>) -> SpannedResult<Value> {
    let items: SpannedResult<Vec<Value>> = seq
        .items
        .iter()
        .map(|item| expr_to_value(&item.expr))
        .collect();
    Ok(Value::Seq(items?))
}

fn map_to_value(map: &MapExpr<'_>) -> SpannedResult<Value> {
    let entries: SpannedResult<Vec<(Value, Value)>> = map
        .entries
        .iter()
        .map(|entry| {
            let key = expr_to_value(&entry.key)?;
            let value = expr_to_value(&entry.value)?;
            Ok((key, value))
        })
        .collect();

    Ok(Value::Map(entries?.into_iter().collect()))
}

/// Convert anonymous tuple `(a, b, c)` to `Value::Tuple`.
fn tuple_to_value(tuple: &TupleExpr<'_>) -> SpannedResult<Value> {
    let elements: SpannedResult<Vec<Value>> = tuple
        .elements
        .iter()
        .map(|elem| expr_to_value(&elem.expr))
        .collect();
    Ok(Value::Tuple(elements?))
}

/// Convert anonymous struct `(field: value, ...)` to `Value::Struct`.
fn anon_struct_to_value(s: &AnonStructExpr<'_>) -> SpannedResult<Value> {
    let struct_fields: StructFields = s
        .fields
        .iter()
        .map(|f| {
            let name = f.name.name.to_string();
            let value = expr_to_value(&f.value)?;
            Ok((name, value))
        })
        .collect::<SpannedResult<_>>()?;

    Ok(Value::Struct(struct_fields))
}

/// Convert named struct/enum to `Value::Named`.
fn struct_to_value(s: &StructExpr<'_>) -> SpannedResult<Value> {
    let name = s.name.name.to_string();

    let content = match &s.body {
        None => NamedContent::Unit,
        Some(StructBody::Tuple(tuple)) => {
            let elements = tuple_body_to_vec(tuple)?;
            NamedContent::Tuple(elements)
        }
        Some(StructBody::Fields(fields)) => {
            let struct_fields = fields_body_to_struct_fields(fields)?;
            NamedContent::Struct(struct_fields)
        }
    };

    Ok(Value::Named { name, content })
}

/// Convert tuple body to Vec<Value>.
fn tuple_body_to_vec(tuple: &TupleBody<'_>) -> SpannedResult<Vec<Value>> {
    tuple
        .elements
        .iter()
        .map(|elem| expr_to_value(&elem.expr))
        .collect()
}

/// Convert fields body to `StructFields` (`Vec<(String, Value)>`).
fn fields_body_to_struct_fields(fields: &FieldsBody<'_>) -> SpannedResult<StructFields> {
    let mut result = StructFields::new();
    for field in &fields.fields {
        let key = field.name.name.to_string();
        let value = expr_to_value(&field.value)?;
        result.push((key, value));
    }
    Ok(result)
}

// ============================================================================
// Value → AST conversion
// ============================================================================

/// Convert a `Value` to an AST expression with synthetic spans.
///
/// This is useful for the default `FromRon::from_ron_value()` implementation,
/// allowing Value-based deserialization to reuse AST-based logic.
///
/// All generated spans use line 0 to distinguish from real source spans.
///
/// # Example
///
/// ```
/// use ron2::ast::value_to_expr;
/// use ron2::Value;
///
/// let value = Value::Bool(true);
/// let expr = value_to_expr(value);
/// // expr is Expr::Bool with a synthetic span
/// ```
pub fn value_to_expr(value: Value) -> Expr<'static> {
    let span = Span::synthetic();
    match value {
        Value::Unit => Expr::Unit(UnitExpr { span }),
        Value::Bool(b) => Expr::Bool(BoolExpr { span, value: b }),
        Value::Char(c) => Expr::Char(CharExpr {
            span,
            raw: Cow::Owned(escape_char(c)),
            value: c,
        }),
        Value::Number(n) => {
            let (raw, kind) = format_number(&n);
            Expr::Number(NumberExpr {
                span,
                raw: Cow::Owned(raw),
                kind,
            })
        }
        Value::String(s) => Expr::String(StringExpr {
            span,
            raw: Cow::Owned(escape_string(&s)),
            value: s,
            kind: StringKind::Regular,
        }),
        Value::Bytes(b) => Expr::Bytes(BytesExpr {
            span,
            raw: Cow::Owned(format_bytes(&b)),
            value: b,
            kind: BytesKind::Regular,
        }),
        Value::Option(opt) => Expr::Option(Box::new(match opt {
            None => OptionExpr { span, value: None },
            Some(inner) => {
                let inner_expr = value_to_expr(*inner);
                OptionExpr {
                    span,
                    value: Some(OptionValue {
                        open_paren: Span::synthetic(),
                        leading: Trivia::empty(),
                        expr: inner_expr,
                        trailing: Trivia::empty(),
                        close_paren: Span::synthetic(),
                    }),
                }
            }
        })),
        Value::Seq(items) => {
            let seq_items: Vec<SeqItem<'static>> = items
                .into_iter()
                .map(|v| SeqItem {
                    leading: Trivia::empty(),
                    expr: value_to_expr(v),
                    trailing: Trivia::empty(),
                    comma: None,
                })
                .collect();
            Expr::Seq(SeqExpr {
                span,
                open_bracket: Span::synthetic(),
                leading: Trivia::empty(),
                items: seq_items,
                trailing: Trivia::empty(),
                close_bracket: Span::synthetic(),
            })
        }
        Value::Tuple(elements) => {
            let tuple_elements: Vec<TupleElement<'static>> = elements
                .into_iter()
                .map(|v| TupleElement {
                    leading: Trivia::empty(),
                    expr: value_to_expr(v),
                    trailing: Trivia::empty(),
                    comma: None,
                })
                .collect();
            Expr::Tuple(TupleExpr {
                span,
                open_paren: Span::synthetic(),
                leading: Trivia::empty(),
                elements: tuple_elements,
                trailing: Trivia::empty(),
                close_paren: Span::synthetic(),
            })
        }
        Value::Map(map) => {
            let entries: Vec<MapEntry<'static>> = map
                .into_iter()
                .map(|(k, v)| MapEntry {
                    leading: Trivia::empty(),
                    key: value_to_expr(k),
                    pre_colon: Trivia::empty(),
                    colon: Span::synthetic(),
                    post_colon: Trivia::empty(),
                    value: value_to_expr(v),
                    trailing: Trivia::empty(),
                    comma: None,
                })
                .collect();
            Expr::Map(MapExpr {
                span,
                open_brace: Span::synthetic(),
                leading: Trivia::empty(),
                entries,
                trailing: Trivia::empty(),
                close_brace: Span::synthetic(),
            })
        }
        Value::Struct(fields) => {
            let struct_fields: Vec<StructField<'static>> = fields
                .into_iter()
                .map(|(name, v)| StructField {
                    leading: Trivia::empty(),
                    name: Ident {
                        span: Span::synthetic(),
                        name: Cow::Owned(name),
                    },
                    pre_colon: Trivia::empty(),
                    colon: Span::synthetic(),
                    post_colon: Trivia::empty(),
                    value: value_to_expr(v),
                    trailing: Trivia::empty(),
                    comma: None,
                })
                .collect();
            Expr::AnonStruct(AnonStructExpr {
                span,
                open_paren: Span::synthetic(),
                leading: Trivia::empty(),
                fields: struct_fields,
                trailing: Trivia::empty(),
                close_paren: Span::synthetic(),
            })
        }
        Value::Named { name, content } => {
            let body = match content {
                NamedContent::Unit => None,
                NamedContent::Tuple(elements) => {
                    let tuple_elements: Vec<TupleElement<'static>> = elements
                        .into_iter()
                        .map(|v| TupleElement {
                            leading: Trivia::empty(),
                            expr: value_to_expr(v),
                            trailing: Trivia::empty(),
                            comma: None,
                        })
                        .collect();
                    Some(StructBody::Tuple(TupleBody {
                        open_paren: Span::synthetic(),
                        leading: Trivia::empty(),
                        elements: tuple_elements,
                        trailing: Trivia::empty(),
                        close_paren: Span::synthetic(),
                    }))
                }
                NamedContent::Struct(fields) => {
                    let struct_fields: Vec<StructField<'static>> = fields
                        .into_iter()
                        .map(|(field_name, v)| StructField {
                            leading: Trivia::empty(),
                            name: Ident {
                                span: Span::synthetic(),
                                name: Cow::Owned(field_name),
                            },
                            pre_colon: Trivia::empty(),
                            colon: Span::synthetic(),
                            post_colon: Trivia::empty(),
                            value: value_to_expr(v),
                            trailing: Trivia::empty(),
                            comma: None,
                        })
                        .collect();
                    Some(StructBody::Fields(FieldsBody {
                        open_brace: Span::synthetic(),
                        leading: Trivia::empty(),
                        fields: struct_fields,
                        trailing: Trivia::empty(),
                        close_brace: Span::synthetic(),
                    }))
                }
            };
            Expr::Struct(StructExpr {
                span,
                name: Ident {
                    span: Span::synthetic(),
                    name: Cow::Owned(name),
                },
                pre_body: Trivia::empty(),
                body,
            })
        }
    }
}

/// Format a number to its raw string representation.
fn format_number(n: &Number) -> (String, NumberKind) {
    match n {
        Number::I8(v) => {
            if *v < 0 {
                (format!("{v}"), NumberKind::NegativeInteger)
            } else {
                (format!("{v}"), NumberKind::Integer)
            }
        }
        Number::I16(v) => {
            if *v < 0 {
                (format!("{v}"), NumberKind::NegativeInteger)
            } else {
                (format!("{v}"), NumberKind::Integer)
            }
        }
        Number::I32(v) => {
            if *v < 0 {
                (format!("{v}"), NumberKind::NegativeInteger)
            } else {
                (format!("{v}"), NumberKind::Integer)
            }
        }
        Number::I64(v) => {
            if *v < 0 {
                (format!("{v}"), NumberKind::NegativeInteger)
            } else {
                (format!("{v}"), NumberKind::Integer)
            }
        }
        #[cfg(feature = "integer128")]
        Number::I128(v) => {
            if *v < 0 {
                (format!("{v}"), NumberKind::NegativeInteger)
            } else {
                (format!("{v}"), NumberKind::Integer)
            }
        }
        Number::U8(v) => (format!("{v}"), NumberKind::Integer),
        Number::U16(v) => (format!("{v}"), NumberKind::Integer),
        Number::U32(v) => (format!("{v}"), NumberKind::Integer),
        Number::U64(v) => (format!("{v}"), NumberKind::Integer),
        #[cfg(feature = "integer128")]
        Number::U128(v) => (format!("{v}"), NumberKind::Integer),
        Number::F32(f) => {
            let v = f.get();
            if v.is_nan() {
                ("NaN".into(), NumberKind::SpecialFloat)
            } else if v.is_infinite() {
                if v.is_sign_positive() {
                    ("inf".into(), NumberKind::SpecialFloat)
                } else {
                    ("-inf".into(), NumberKind::SpecialFloat)
                }
            } else {
                // Ensure we always have a decimal point for floats
                let s = format!("{v}");
                if s.contains('.') || s.contains('e') || s.contains('E') {
                    (s, NumberKind::Float)
                } else {
                    (format!("{v}.0"), NumberKind::Float)
                }
            }
        }
        Number::F64(f) => {
            let v = f.get();
            if v.is_nan() {
                ("NaN".into(), NumberKind::SpecialFloat)
            } else if v.is_infinite() {
                if v.is_sign_positive() {
                    ("inf".into(), NumberKind::SpecialFloat)
                } else {
                    ("-inf".into(), NumberKind::SpecialFloat)
                }
            } else {
                let s = format!("{v}");
                if s.contains('.') || s.contains('e') || s.contains('E') {
                    (s, NumberKind::Float)
                } else {
                    (format!("{v}.0"), NumberKind::Float)
                }
            }
        }
        // Handle non-exhaustive variant
        _ => ("0".into(), NumberKind::Integer),
    }
}

/// Escape a character for RON representation (including quotes).
fn escape_char(c: char) -> String {
    match c {
        '\'' => "'\\''".into(),
        '\\' => "'\\\\'".into(),
        '\n' => "'\\n'".into(),
        '\r' => "'\\r'".into(),
        '\t' => "'\\t'".into(),
        '\0' => "'\\0'".into(),
        c if c.is_ascii_control() => format!("'\\x{:02x}'", c as u8),
        c => format!("'{c}'"),
    }
}

/// Escape a string for RON representation (including quotes).
fn escape_string(s: &str) -> String {
    let mut result = String::with_capacity(s.len() + 2);
    result.push('"');
    for c in s.chars() {
        match c {
            '"' => result.push_str("\\\""),
            '\\' => result.push_str("\\\\"),
            '\n' => result.push_str("\\n"),
            '\r' => result.push_str("\\r"),
            '\t' => result.push_str("\\t"),
            '\0' => result.push_str("\\0"),
            c if c.is_ascii_control() => {
                result.push_str(&format!("\\x{:02x}", c as u8));
            }
            c => result.push(c),
        }
    }
    result.push('"');
    result
}

/// Format bytes for RON representation (as byte string).
fn format_bytes(bytes: &[u8]) -> String {
    let mut result = String::with_capacity(bytes.len() + 3);
    result.push_str("b\"");
    for &b in bytes {
        match b {
            b'"' => result.push_str("\\\""),
            b'\\' => result.push_str("\\\\"),
            b'\n' => result.push_str("\\n"),
            b'\r' => result.push_str("\\r"),
            b'\t' => result.push_str("\\t"),
            b'\0' => result.push_str("\\0"),
            b if b.is_ascii_graphic() || b == b' ' => result.push(b as char),
            b => result.push_str(&format!("\\x{b:02x}")),
        }
    }
    result.push('"');
    result
}

#[cfg(test)]
#[allow(clippy::panic)]
mod tests {
    use alloc::vec;

    use super::*;
    use crate::ast::parse_document;

    #[test]
    fn convert_integer() {
        let doc = parse_document("42").unwrap();
        let value = to_value(&doc).unwrap().unwrap();
        assert_eq!(value, Value::Number(Number::U8(42)));
    }

    #[test]
    fn convert_negative_integer() {
        let doc = parse_document("-42").unwrap();
        let value = to_value(&doc).unwrap().unwrap();
        assert_eq!(value, Value::Number(Number::I8(-42)));
    }

    #[test]
    fn convert_hex() {
        let doc = parse_document("0xFF").unwrap();
        let value = to_value(&doc).unwrap().unwrap();
        assert_eq!(value, Value::Number(Number::U8(255)));
    }

    #[test]
    fn convert_float() {
        let doc = parse_document("3.14").unwrap();
        let value = to_value(&doc).unwrap().unwrap();
        match value {
            Value::Number(Number::F32(_) | Number::F64(_)) => {}
            _ => panic!("expected float"),
        }
    }

    #[test]
    fn convert_string() {
        let doc = parse_document(r#""hello""#).unwrap();
        let value = to_value(&doc).unwrap().unwrap();
        assert_eq!(value, Value::String(String::from("hello")));
    }

    #[test]
    fn convert_bool() {
        let doc = parse_document("true").unwrap();
        let value = to_value(&doc).unwrap().unwrap();
        assert_eq!(value, Value::Bool(true));
    }

    #[test]
    fn convert_option_none() {
        let doc = parse_document("None").unwrap();
        let value = to_value(&doc).unwrap().unwrap();
        assert_eq!(value, Value::Option(None));
    }

    #[test]
    fn convert_option_some() {
        let doc = parse_document("Some(42)").unwrap();
        let value = to_value(&doc).unwrap().unwrap();
        assert_eq!(
            value,
            Value::Option(Some(Box::new(Value::Number(Number::U8(42)))))
        );
    }

    #[test]
    fn convert_seq() {
        let doc = parse_document("[1, 2, 3]").unwrap();
        let value = to_value(&doc).unwrap().unwrap();
        assert_eq!(
            value,
            Value::Seq(vec![
                Value::Number(Number::U8(1)),
                Value::Number(Number::U8(2)),
                Value::Number(Number::U8(3)),
            ])
        );
    }

    #[test]
    fn convert_tuple() {
        let doc = parse_document("(1, 2, 3)").unwrap();
        let value = to_value(&doc).unwrap().unwrap();
        assert_eq!(
            value,
            Value::Tuple(vec![
                Value::Number(Number::U8(1)),
                Value::Number(Number::U8(2)),
                Value::Number(Number::U8(3)),
            ])
        );
    }

    #[test]
    fn convert_map() {
        let doc = parse_document(r#"{"a": 1}"#).unwrap();
        let value = to_value(&doc).unwrap().unwrap();
        match value {
            Value::Map(map) => {
                assert_eq!(map.len(), 1);
                assert_eq!(
                    map.get(&Value::String(String::from("a"))),
                    Some(&Value::Number(Number::U8(1)))
                );
            }
            _ => panic!("expected map"),
        }
    }

    #[test]
    fn convert_named_unit() {
        let doc = parse_document("Point").unwrap();
        let value = to_value(&doc).unwrap().unwrap();
        assert_eq!(
            value,
            Value::Named {
                name: String::from("Point"),
                content: NamedContent::Unit,
            }
        );
    }

    #[test]
    fn convert_named_tuple() {
        let doc = parse_document("Point(1, 2)").unwrap();
        let value = to_value(&doc).unwrap().unwrap();
        assert_eq!(
            value,
            Value::Named {
                name: String::from("Point"),
                content: NamedContent::Tuple(vec![
                    Value::Number(Number::U8(1)),
                    Value::Number(Number::U8(2)),
                ]),
            }
        );
    }

    #[test]
    fn convert_named_struct() {
        // RON uses braces for named fields: Point { x: 1 }
        let doc = parse_document("Point { x: 1, y: 2 }").unwrap();
        let value = to_value(&doc).unwrap().unwrap();
        match value {
            Value::Named {
                name,
                content: NamedContent::Struct(fields),
            } => {
                assert_eq!(name, "Point");
                assert_eq!(fields.len(), 2);
                // StructFields is Vec<(String, Value)>
                assert_eq!(fields[0], (String::from("x"), Value::Number(Number::U8(1))));
                assert_eq!(fields[1], (String::from("y"), Value::Number(Number::U8(2))));
            }
            _ => panic!("expected named struct, got {value:?}"),
        }
    }

    #[test]
    fn convert_empty_document() {
        let doc = parse_document("").unwrap();
        assert!(to_value(&doc).is_none());
    }
}
