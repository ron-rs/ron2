//! Single-line RON text for values of any depth.

use alloc::{string::String, vec::Vec};

use crate::{
    ast::{Ident, escape},
    value::{NamedContent, StructFields, Value},
};

/// Pending output: a value to write or a token.
enum Step<'v> {
    Value(&'v Value),
    Token(&'static str),
    Ident(&'v str),
}

/// Write `value` on one line, as [`format_expr`](crate::fmt::format_expr) does with
/// `FormatConfig::new().compact_from_depth(0)`.
///
/// Unlike the AST conversion this uses an explicit stack instead of recursion, so it
/// works for values of any depth.
pub(super) fn compact(value: &Value) -> String {
    let mut out = String::new();
    let mut stack = Vec::from([Step::Value(value)]);
    while let Some(step) = stack.pop() {
        match step {
            Step::Token(token) => out.push_str(token),
            Step::Ident(name) => out.push_str(&Ident::source_text(name)),
            Step::Value(value) => leaf_or_push(&mut out, &mut stack, value),
        }
    }
    out
}

/// Write a scalar, or write the opening token of a collection and push its rest.
fn leaf_or_push<'v>(out: &mut String, stack: &mut Vec<Step<'v>>, value: &'v Value) {
    match value {
        Value::Bool(b) => out.push_str(if *b { "true" } else { "false" }),
        Value::Char(c) => escape::push_char(out, *c),
        Value::Number(n) => {
            escape::push_number(out, *n);
        }
        Value::String(s) => escape::push_string(out, s),
        Value::Bytes(b) => escape::push_bytes(out, b),
        Value::Unit => out.push_str("()"),
        Value::Option(None) => out.push_str("None"),
        Value::Option(Some(inner)) => {
            out.push_str("Some(");
            stack.push(Step::Token(")"));
            stack.push(Step::Value(inner));
        }
        Value::Seq(items) => {
            out.push('[');
            push_items(stack, "]", items);
        }
        Value::Tuple(items) => {
            out.push('(');
            push_items(stack, ")", items);
        }
        Value::Map(map) => {
            out.push('{');
            stack.push(Step::Token("}"));
            for (i, (key, value)) in map.iter().rev().enumerate() {
                if i > 0 {
                    stack.push(Step::Token(", "));
                }
                stack.push(Step::Value(value));
                stack.push(Step::Token(": "));
                stack.push(Step::Value(key));
            }
        }
        Value::Struct(fields) => {
            out.push('(');
            push_fields(stack, fields);
        }
        Value::Named { name, content } => {
            out.push_str(&Ident::source_text(name));
            match content {
                NamedContent::Unit => {}
                NamedContent::Tuple(items) => {
                    out.push('(');
                    push_items(stack, ")", items);
                }
                NamedContent::Struct(fields) => {
                    out.push('(');
                    push_fields(stack, fields);
                }
            }
        }
    }
}

fn push_items<'v>(stack: &mut Vec<Step<'v>>, close: &'static str, items: &'v [Value]) {
    stack.push(Step::Token(close));
    for (i, item) in items.iter().enumerate().rev() {
        stack.push(Step::Value(item));
        if i > 0 {
            stack.push(Step::Token(", "));
        }
    }
}

fn push_fields<'v>(stack: &mut Vec<Step<'v>>, fields: &'v StructFields) {
    stack.push(Step::Token(")"));
    for (i, (name, value)) in fields.iter().enumerate().rev() {
        stack.push(Step::Value(value));
        stack.push(Step::Token(": "));
        stack.push(Step::Ident(name));
        if i > 0 {
            stack.push(Step::Token(", "));
        }
    }
}

#[cfg(test)]
mod tests {
    use alloc::{string::ToString, vec};

    use super::*;
    use crate::{
        ToRon,
        ast::{MAX_CONVERSION_DEPTH, value_to_expr},
        error::ErrorKind,
        fmt::{FormatConfig, format_expr},
        value::Map,
    };

    /// The fallback writes what the formatter writes with everything compacted.
    #[test]
    fn compact_matches_the_formatter() {
        let values: Vec<Value> = vec![
            "Config(name: \"a\\\"b\\n\", tags: [\"x\", 'y', b\"\\x00z\"], \
             map: {1: Some(-2.5), \"k\": None}, anon: (a: (), b: (1, 2.0)), \
             unit: Unit, tuple: Tuple(0x10, true), empty: [], e: {}, n: Name())"
                .parse()
                .unwrap(),
            "(r#type: r#true, plain: r#None(1), f: 0.1f32, nan: NaN)"
                .parse()
                .unwrap(),
            Value::Tuple(vec![Value::from(1)]),
            Value::Struct(vec![]),
            Value::Named {
                name: "a.b".into(),
                content: NamedContent::Unit,
            },
            Value::Map(Map::new()),
            Value::Option(Some(Box::new(Value::Option(None)))),
        ];
        let config = FormatConfig::new().compact_from_depth(0);
        for value in values {
            let expected = format_expr(&value_to_expr(&value).unwrap(), &config);
            assert_eq!(compact(&value), expected);
        }
    }

    fn nested(levels: usize) -> Value {
        let mut value = Value::Unit;
        for i in 0..levels {
            value = match i % 4 {
                0 => Value::Seq(vec![value]),
                1 => Value::Option(Some(Box::new(value))),
                2 => Value::Named {
                    name: "N".into(),
                    content: NamedContent::Struct(vec![("x".into(), value)]),
                },
                _ => Value::from_iter([(value, Value::from(1))]),
            };
        }
        value
    }

    /// Regression: `Display` returned `fmt::Error` past the conversion depth, so
    /// `to_string()` panicked.
    #[test]
    fn display_never_fails_on_deep_values() {
        let run = || {
            // The unit inside adds a level.
            let within = nested(MAX_CONVERSION_DEPTH - 1);
            assert!(within.to_string().contains('\n'), "pretty up to the limit");
            let beyond = nested(MAX_CONVERSION_DEPTH);
            assert_eq!(
                beyond.to_string(),
                compact(&beyond),
                "compact past the limit"
            );

            let deep = nested(1000);
            let text = deep.to_string();
            assert_eq!(text, compact(&deep));
            assert!(text.starts_with("{N(x: Some([{N(x: "));
            assert_eq!(text.matches("()").count(), 1);

            let err = deep.to_ron().unwrap_err();
            assert_eq!(err.kind(), &ErrorKind::ExceededRecursionLimit);
        };
        std::thread::Builder::new()
            .stack_size(8 * 1024 * 1024)
            .spawn(run)
            .unwrap()
            .join()
            .unwrap();
    }
}
