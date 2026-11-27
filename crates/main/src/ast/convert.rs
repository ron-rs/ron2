//! Conversion between AST expressions and [`Value`], and construction of
//! synthetic expressions.
//!
//! **AST → Value** ([`expr_to_value`]) discards spans, trivia and raw text.
//! Duplicate map keys and duplicate struct fields are errors.
//!
//! **Value → AST** ([`value_to_expr`]) builds expressions with synthetic spans
//! (line 0) and canonical raw text, ready for formatting.

use alloc::{borrow::Cow, boxed::Box, string::String, vec::Vec};

use super::escape;
use crate::{
    ast::{
        AnonStructExpr, BoolExpr, BytesExpr, BytesKind, CharExpr, Document, Expr, FieldsBody,
        Ident, MapEntry, MapExpr, NumberExpr, NumberKind, OptionExpr, OptionValue, SeqExpr,
        SeqItem, StringExpr, StringKind, StructBody, StructExpr, StructField, Trivia, TupleBody,
        TupleElement, TupleExpr, UnitExpr,
    },
    convert::{check_unique_fields, number},
    error::{Error, ErrorKind, Result, Span},
    value::{Map, NamedContent, Number, StructFields, Value},
};

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
/// let value = to_value(&doc).unwrap().unwrap();
/// assert_eq!(value, ron2::Value::from(42));
/// ```
pub fn to_value(doc: &Document<'_>) -> Option<Result<Value>> {
    doc.value.as_ref().map(expr_to_value)
}

/// Maximum nesting depth of parsing, [`expr_to_value`] and [`value_to_expr`]:
/// the number of values from the outermost one to the innermost, inclusive
/// (`[[1]]` is 3 levels deep).
///
/// Deeper input fails with [`ErrorKind::ExceededRecursionLimit`] instead of
/// overflowing the stack. All three share the limit, so a value that converts
/// can be written and parsed again.
pub const MAX_CONVERSION_DEPTH: usize = 128;

fn too_deep(span: Span) -> Error {
    Error::with_span(ErrorKind::ExceededRecursionLimit, span)
}

// ============================================================================
// AST → Value conversion
// ============================================================================

/// Convert an AST expression to a Value.
///
/// Errors include the span of the expression that caused the error,
/// enabling precise error reporting with source location. Duplicate map
/// keys and struct fields are errors, as is nesting deeper than [`MAX_CONVERSION_DEPTH`].
pub fn expr_to_value(expr: &Expr<'_>) -> Result<Value> {
    expr_to_value_at(expr, 0)
}

fn expr_to_value_at(expr: &Expr<'_>, depth: usize) -> Result<Value> {
    let nested = |child: &Expr<'_>| {
        // `depth` counts from 0, so the child is at level `depth + 2`.
        if depth + 2 > MAX_CONVERSION_DEPTH {
            return Err(too_deep(*child.span()));
        }
        expr_to_value_at(child, depth + 1)
    };
    let fields = |fields: &[StructField<'_>], outer: Option<&str>| {
        check_unique_fields(fields, outer)?;
        fields
            .iter()
            .map(|f| Ok((String::from(&*f.name.name), nested(&f.value)?)))
            .collect::<Result<_>>()
    };

    Ok(match expr {
        Expr::Unit(_) => Value::Unit,
        Expr::Bool(b) => Value::Bool(b.value),
        Expr::Char(c) => Value::Char(c.value),
        Expr::Byte(b) => Value::Number(Number::U8(b.value)),
        Expr::Number(n) => Value::Number(number::to_number(n)?),
        Expr::String(s) => Value::String(s.value.clone()),
        Expr::Bytes(b) => Value::Bytes(b.value.clone()),
        Expr::Option(opt) => Value::Option(match &opt.value {
            Some(inner) => Some(Box::new(nested(&inner.expr)?)),
            None => None,
        }),
        Expr::Seq(seq) => Value::Seq(
            seq.items
                .iter()
                .map(|item| nested(&item.expr))
                .collect::<Result<_>>()?,
        ),
        Expr::Tuple(tuple) => Value::Tuple(
            tuple
                .elements
                .iter()
                .map(|e| nested(&e.expr))
                .collect::<Result<_>>()?,
        ),
        Expr::Map(map) => {
            let mut out = Map::with_capacity(map.entries.len());
            for entry in &map.entries {
                let key = nested(&entry.key)?;
                out.insert_unique(key, nested(&entry.value)?)
                    .map_err(|e| e.or_span(*entry.key.span()))?;
            }
            Value::Map(out)
        }
        Expr::AnonStruct(s) => Value::Struct(fields(&s.fields, None)?),
        Expr::Struct(s) => Value::Named {
            name: String::from(&*s.name.name),
            content: match &s.body {
                None => NamedContent::Unit,
                Some(StructBody::Tuple(t)) => NamedContent::Tuple(
                    t.elements
                        .iter()
                        .map(|e| nested(&e.expr))
                        .collect::<Result<_>>()?,
                ),
                Some(StructBody::Fields(body)) => {
                    NamedContent::Struct(fields(&body.fields, Some(&s.name.name))?)
                }
            },
        },
        Expr::Error(err) => return Err(Error::with_span(err.error.kind().clone(), err.span)),
    })
}

// ============================================================================
// Value → AST conversion
// ============================================================================

/// Convert a `Value` to an AST expression with synthetic spans.
///
/// All generated spans use line 0 to distinguish them from real source spans.
/// Nesting deeper than [`MAX_CONVERSION_DEPTH`] is an error.
///
/// # Example
///
/// ```
/// use ron2::{ast::value_to_expr, fmt::{format_expr, FormatConfig}};
/// use ron2::Value;
///
/// let expr = value_to_expr(&Value::from(vec!["a", "b"])).unwrap();
/// assert_eq!(format_expr(&expr, &FormatConfig::minimal()), r#"["a","b"]"#);
/// ```
pub fn value_to_expr(value: &Value) -> Result<Expr<'static>> {
    value_to_expr_at(value, 0)
}

fn value_to_expr_at(value: &Value, depth: usize) -> Result<Expr<'static>> {
    let nested = |child: &Value| {
        // `depth` counts from 0, so the child is at level `depth + 2`.
        if depth + 2 > MAX_CONVERSION_DEPTH {
            return Err(too_deep(Span::synthetic()));
        }
        value_to_expr_at(child, depth + 1)
    };
    let fields = |fields: &StructFields| {
        fields
            .iter()
            .map(|(name, v)| Ok((Cow::Owned(name.clone()), nested(v)?)))
            .collect::<Result<_>>()
    };
    let items = |items: &[Value]| items.iter().map(nested).collect::<Result<_>>();

    Ok(match value {
        Value::Unit => synthetic_unit(),
        Value::Bool(b) => synthetic_bool(*b),
        Value::Char(c) => synthetic_char(*c),
        Value::Number(n) => synthetic_number(*n),
        Value::String(s) => synthetic_string(s.clone()),
        Value::Bytes(b) => synthetic_bytes(b.clone()),
        Value::Option(opt) => synthetic_option(opt.as_deref().map(nested).transpose()?),
        Value::Seq(seq) => synthetic_seq(items(seq)?),
        Value::Tuple(tuple) => synthetic_tuple(items(tuple)?),
        Value::Map(map) => synthetic_map(
            map.iter()
                .map(|(k, v)| Ok((nested(k)?, nested(v)?)))
                .collect::<Result<_>>()?,
        ),
        Value::Struct(f) => synthetic_anon_struct(fields(f)?),
        Value::Named { name, content } => match content {
            NamedContent::Unit => synthetic_named_unit(name.clone()),
            NamedContent::Tuple(tuple) => synthetic_named_tuple(name.clone(), items(tuple)?),
            NamedContent::Struct(f) => synthetic_struct(name.clone(), fields(f)?),
        },
    })
}

// ============================================================================
// Synthetic expression construction
// ============================================================================

/// Create a synthetic boolean expression.
#[must_use]
pub fn synthetic_bool(value: bool) -> Expr<'static> {
    Expr::Bool(BoolExpr {
        span: Span::synthetic(),
        value,
    })
}

/// Create a synthetic character expression.
#[must_use]
pub fn synthetic_char(value: char) -> Expr<'static> {
    let mut raw = String::new();
    escape::push_char(&mut raw, value);
    Expr::Char(CharExpr {
        span: Span::synthetic(),
        raw: Cow::Owned(raw),
        value,
    })
}

/// Create a synthetic string expression.
#[must_use]
pub fn synthetic_string(value: String) -> Expr<'static> {
    let mut raw = String::new();
    escape::push_string(&mut raw, &value);
    Expr::String(StringExpr {
        span: Span::synthetic(),
        raw: Cow::Owned(raw),
        value,
        kind: StringKind::Regular,
    })
}

/// Create a synthetic byte string expression.
#[must_use]
pub fn synthetic_bytes(value: Vec<u8>) -> Expr<'static> {
    let mut raw = String::new();
    escape::push_bytes(&mut raw, &value);
    Expr::Bytes(BytesExpr {
        span: Span::synthetic(),
        raw: Cow::Owned(raw),
        value,
        kind: BytesKind::Regular,
    })
}

/// Create a synthetic unit expression.
#[must_use]
pub fn synthetic_unit() -> Expr<'static> {
    Expr::Unit(UnitExpr {
        span: Span::synthetic(),
        inner: Trivia::empty(),
    })
}

/// Create a synthetic integer expression.
#[must_use]
pub fn synthetic_integer<T: itoa::Integer + PartialOrd + Default>(value: T) -> Expr<'static> {
    let mut raw = String::new();
    let kind = escape::push_int(&mut raw, value);
    number_expr(raw, kind, None)
}

/// Create a synthetic expression for a typed `f32`; unlike a `Value`
/// number it needs no suffix, since the target type is known.
#[must_use]
pub fn synthetic_f32(value: f32) -> Expr<'static> {
    let mut raw = String::new();
    let kind = escape::push_f32(&mut raw, value);
    number_expr(raw, kind, None)
}

/// Create a synthetic `f64` expression.
#[must_use]
pub fn synthetic_f64(value: f64) -> Expr<'static> {
    let mut raw = String::new();
    let kind = escape::push_f64(&mut raw, value);
    number_expr(raw, kind, None)
}

/// Create a synthetic number expression for a `Value` number.
///
/// The expression carries `value` ([`NumberExpr::value`]), so converting it
/// back does not parse the canonical text.
#[must_use]
pub fn synthetic_number(value: Number) -> Expr<'static> {
    let mut raw = String::new();
    let kind = escape::push_number(&mut raw, value);
    number_expr(raw, kind, Some(value))
}

fn number_expr(raw: String, kind: NumberKind, value: Option<Number>) -> Expr<'static> {
    Expr::Number(NumberExpr {
        span: Span::synthetic(),
        raw: Cow::Owned(raw),
        kind,
        value,
    })
}

/// Create a synthetic sequence expression from items.
#[must_use]
pub fn synthetic_seq(items: Vec<Expr<'static>>) -> Expr<'static> {
    let items = items
        .into_iter()
        .map(|expr| SeqItem {
            leading: Trivia::empty(),
            expr,
            trailing: Trivia::empty(),
            comma: None,
        })
        .collect();
    Expr::Seq(SeqExpr {
        span: Span::synthetic(),
        open_bracket: Span::synthetic(),
        leading: Trivia::empty(),
        items,
        trailing: Trivia::empty(),
        close_bracket: Span::synthetic(),
    })
}

/// Create a synthetic map expression from key-value pairs.
#[must_use]
pub fn synthetic_map(entries: Vec<(Expr<'static>, Expr<'static>)>) -> Expr<'static> {
    let entries = entries
        .into_iter()
        .map(|(key, value)| MapEntry {
            leading: Trivia::empty(),
            key,
            pre_colon: Trivia::empty(),
            colon: Span::synthetic(),
            post_colon: Trivia::empty(),
            value,
            trailing: Trivia::empty(),
            comma: None,
        })
        .collect();
    Expr::Map(MapExpr {
        span: Span::synthetic(),
        open_brace: Span::synthetic(),
        leading: Trivia::empty(),
        entries,
        trailing: Trivia::empty(),
        close_brace: Span::synthetic(),
    })
}

/// Create a synthetic option expression (Some or None).
#[must_use]
pub fn synthetic_option(inner: Option<Expr<'static>>) -> Expr<'static> {
    Expr::Option(Box::new(OptionExpr {
        span: Span::synthetic(),
        value: inner.map(|expr| OptionValue {
            pre_paren: Trivia::empty(),
            open_paren: Span::synthetic(),
            leading: Trivia::empty(),
            expr,
            trailing: Trivia::empty(),
            comma: None,
            post_comma: Trivia::empty(),
            close_paren: Span::synthetic(),
        }),
    }))
}

fn tuple_elements(elements: Vec<Expr<'static>>) -> Vec<TupleElement<'static>> {
    elements
        .into_iter()
        .map(|expr| TupleElement {
            leading: Trivia::empty(),
            expr,
            trailing: Trivia::empty(),
            comma: None,
        })
        .collect()
}

fn struct_fields(fields: Vec<(Cow<'static, str>, Expr<'static>)>) -> Vec<StructField<'static>> {
    fields
        .into_iter()
        .map(|(name, value)| StructField {
            leading: Trivia::empty(),
            name: synthetic_ident(name),
            pre_colon: Trivia::empty(),
            colon: Span::synthetic(),
            post_colon: Trivia::empty(),
            value,
            trailing: Trivia::empty(),
            comma: None,
        })
        .collect()
}

fn synthetic_ident(name: Cow<'static, str>) -> Ident<'static> {
    Ident {
        span: Span::synthetic(),
        name,
    }
}

/// Create a synthetic tuple expression from elements.
#[must_use]
pub fn synthetic_tuple(elements: Vec<Expr<'static>>) -> Expr<'static> {
    Expr::Tuple(TupleExpr {
        span: Span::synthetic(),
        open_paren: Span::synthetic(),
        leading: Trivia::empty(),
        elements: tuple_elements(elements),
        trailing: Trivia::empty(),
        close_paren: Span::synthetic(),
    })
}

/// Create a synthetic anonymous struct expression: `(field: value, ...)`.
#[must_use]
pub fn synthetic_anon_struct(fields: Vec<(Cow<'static, str>, Expr<'static>)>) -> Expr<'static> {
    Expr::AnonStruct(AnonStructExpr {
        span: Span::synthetic(),
        open_paren: Span::synthetic(),
        leading: Trivia::empty(),
        fields: struct_fields(fields),
        trailing: Trivia::empty(),
        close_paren: Span::synthetic(),
    })
}

fn synthetic_named(name: Cow<'static, str>, body: Option<StructBody<'static>>) -> Expr<'static> {
    Expr::Struct(StructExpr {
        span: Span::synthetic(),
        name: synthetic_ident(name),
        pre_body: Trivia::empty(),
        body,
    })
}

/// Create a synthetic named struct expression: `Name(field: value, ...)`.
///
/// # Example
///
/// ```
/// use ron2::{ast::synthetic_struct, fmt::{format_expr, FormatConfig}, ToRon};
/// use std::borrow::Cow;
///
/// let fields = vec![
///     (Cow::Borrowed("x"), 10i32.to_ast().unwrap()),
///     (Cow::Borrowed("y"), 20i32.to_ast().unwrap()),
/// ];
/// let expr = synthetic_struct("Point", fields);
/// assert_eq!(format_expr(&expr, &FormatConfig::minimal()), "Point(x:10,y:20)");
/// ```
#[must_use]
pub fn synthetic_struct(
    name: impl Into<Cow<'static, str>>,
    fields: Vec<(Cow<'static, str>, Expr<'static>)>,
) -> Expr<'static> {
    synthetic_named(
        name.into(),
        Some(StructBody::Fields(FieldsBody {
            open_brace: Span::synthetic(),
            leading: Trivia::empty(),
            fields: struct_fields(fields),
            trailing: Trivia::empty(),
            close_brace: Span::synthetic(),
        })),
    )
}

/// Create a synthetic named tuple expression: `Name(a, b, c)`.
///
/// # Example
///
/// ```
/// use ron2::{ast::synthetic_named_tuple, fmt::{format_expr, FormatConfig}, ToRon};
///
/// let elements = vec![10i32.to_ast().unwrap(), 20i32.to_ast().unwrap()];
/// let expr = synthetic_named_tuple("Point", elements);
/// assert_eq!(format_expr(&expr, &FormatConfig::minimal()), "Point(10,20)");
/// ```
#[must_use]
pub fn synthetic_named_tuple(
    name: impl Into<Cow<'static, str>>,
    elements: Vec<Expr<'static>>,
) -> Expr<'static> {
    synthetic_named(
        name.into(),
        Some(StructBody::Tuple(TupleBody {
            open_paren: Span::synthetic(),
            leading: Trivia::empty(),
            elements: tuple_elements(elements),
            trailing: Trivia::empty(),
            close_paren: Span::synthetic(),
        })),
    )
}

/// Create a synthetic named unit expression: `Name`.
///
/// # Example
///
/// ```
/// use ron2::{ast::synthetic_named_unit, fmt::{format_expr, FormatConfig}};
///
/// let expr = synthetic_named_unit("Empty");
/// assert_eq!(format_expr(&expr, &FormatConfig::minimal()), "Empty");
/// ```
#[must_use]
pub fn synthetic_named_unit(name: impl Into<Cow<'static, str>>) -> Expr<'static> {
    synthetic_named(name.into(), None)
}

#[cfg(test)]
mod tests {
    use alloc::{string::ToString, vec};

    use super::*;
    use crate::{ErrorKind, ast::parse_document, value::F64};

    fn convert(source: &str) -> Result<Value> {
        to_value(&parse_document(source).unwrap()).unwrap()
    }

    #[test]
    fn convert_scalars() {
        assert_eq!(convert("42").unwrap(), Value::Number(Number::U8(42)));
        assert_eq!(convert("-42").unwrap(), Value::Number(Number::I8(-42)));
        assert_eq!(convert("0xFF").unwrap(), Value::Number(Number::U8(255)));
        assert_eq!(
            convert("2.5").unwrap(),
            Value::Number(Number::F64(F64(2.5)))
        );
        assert_eq!(convert(r#""hello""#).unwrap(), Value::from("hello"));
        assert_eq!(convert("true").unwrap(), Value::Bool(true));
        assert_eq!(convert("None").unwrap(), Value::Option(None));
        assert_eq!(
            convert("Some(42)").unwrap(),
            Value::Option(Some(Box::new(Value::Number(Number::U8(42)))))
        );
    }

    #[test]
    fn convert_collections() {
        let one_two_three = vec![Value::from(1u8), Value::from(2u8), Value::from(3u8)];
        assert_eq!(
            convert("[1, 2, 3]").unwrap(),
            Value::Seq(one_two_three.clone())
        );
        assert_eq!(convert("(1, 2, 3)").unwrap(), Value::Tuple(one_two_three));

        let Value::Map(map) = convert(r#"{"a": 1}"#).unwrap() else {
            panic!("expected map");
        };
        assert_eq!(map.len(), 1);
        assert_eq!(map.get(&Value::from("a")), Some(&Value::from(1u8)));
    }

    #[test]
    fn convert_named() {
        assert_eq!(
            convert("Point").unwrap(),
            Value::Named {
                name: String::from("Point"),
                content: NamedContent::Unit,
            }
        );
        assert_eq!(
            convert("Point(1, 2)").unwrap(),
            Value::Named {
                name: String::from("Point"),
                content: NamedContent::Tuple(vec![Value::from(1u8), Value::from(2u8)]),
            }
        );
        assert_eq!(
            convert("Point(x: 1, y: 2)").unwrap(),
            Value::Named {
                name: String::from("Point"),
                content: NamedContent::Struct(vec![
                    (String::from("x"), Value::from(1u8)),
                    (String::from("y"), Value::from(2u8)),
                ]),
            }
        );
    }

    #[test]
    fn convert_empty_document() {
        let doc = parse_document("").unwrap();
        assert!(to_value(&doc).is_none());
    }

    #[test]
    fn duplicate_map_key_is_error() {
        let err = convert(r#"{"a": 1, "b": 2, "a": 3}"#).unwrap_err();
        assert_eq!(
            err.kind(),
            &ErrorKind::DuplicateKey {
                key: "\"a\"".to_string()
            }
        );
        assert_eq!(err.span().start.col, 18);

        // Keys compare by numeric value.
        let err = convert("{1: 'a', 1u32: 'b'}").unwrap_err();
        assert!(matches!(err.kind(), ErrorKind::DuplicateKey { .. }));
    }

    #[test]
    fn duplicate_struct_field_is_error() {
        let err = convert("(a: 1, a: 2)").unwrap_err();
        assert_eq!(
            err.kind(),
            &ErrorKind::DuplicateField {
                field: "a".into(),
                outer: None
            }
        );
        assert_eq!(err.span().start.col, 8);

        let err = convert("Point(x: 1, x: 2)").unwrap_err();
        assert_eq!(
            err.kind(),
            &ErrorKind::DuplicateField {
                field: "x".into(),
                outer: Some("Point".into())
            }
        );
    }

    #[test]
    fn byte_literal_is_u8() {
        let expr = Expr::Byte(crate::ast::ByteExpr {
            span: Span::synthetic(),
            raw: Cow::Borrowed("b'\\xff'"),
            value: 0xff,
        });
        assert_eq!(
            expr_to_value(&expr).unwrap(),
            Value::Number(Number::U8(255))
        );
    }

    /// `levels` values deep: `levels - 1` sequences around a unit.
    fn nested_seq(levels: usize) -> Value {
        let mut value = Value::Unit;
        for _ in 1..levels {
            value = Value::Seq(vec![value]);
        }
        value
    }

    /// Regression: the conversions overflowed a 2 MB stack at a nesting
    /// depth of about 200 in debug builds. Deep input now fails cleanly.
    #[test]
    fn nesting_beyond_the_limit_is_an_error() {
        let run = || {
            let deepest = nested_seq(MAX_CONVERSION_DEPTH);
            let expr = value_to_expr(&deepest).unwrap();
            assert_eq!(expr_to_value(&expr).unwrap(), deepest);

            for levels in [MAX_CONVERSION_DEPTH + 1, 200, 1000] {
                let value = nested_seq(levels);
                let err = value_to_expr(&value).unwrap_err();
                assert_eq!(err.kind(), &ErrorKind::ExceededRecursionLimit);
            }

            let too_deep = synthetic_seq(vec![expr]);
            let err = expr_to_value(&too_deep).unwrap_err();
            assert_eq!(err.kind(), &ErrorKind::ExceededRecursionLimit);
        };
        std::thread::Builder::new()
            .stack_size(2 * 1024 * 1024)
            .spawn(run)
            .unwrap()
            .join()
            .unwrap();
    }

    /// Parsing and conversion share the limit: the deepest document the
    /// parser accepts converts, and the deepest value that converts is
    /// written as text that parses again.
    #[test]
    fn parser_and_conversion_limits_agree() {
        let nest = |n: usize| "[".repeat(n) + &"]".repeat(n);
        let deepest = (1..=MAX_CONVERSION_DEPTH + 8)
            .take_while(|&n| parse_document(&nest(n)).is_ok())
            .last()
            .unwrap();
        assert_eq!(deepest, MAX_CONVERSION_DEPTH);
        assert!(convert(&nest(deepest)).is_ok());

        let value = nested_seq(MAX_CONVERSION_DEPTH);
        let expr = value_to_expr(&value).unwrap();
        let text = crate::fmt::format_expr(&expr, &crate::fmt::FormatConfig::minimal());
        assert_eq!(convert(&text).unwrap(), value);
    }

    #[test]
    fn value_round_trips_through_expr() {
        let value = convert(
            r#"Config(
                name: "a\"b\n",
                tags: ["x", 'y', b"\x00z"],
                map: {1: Some(-2.5), "k": None},
                anon: (a: (), b: (1, 2.0)),
                unit: Unit,
                tuple: Tuple(0x10, true),
            )"#,
        )
        .unwrap();
        assert_eq!(
            expr_to_value(&value_to_expr(&value).unwrap()).unwrap(),
            value
        );
    }
}
