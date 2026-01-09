//! Roundtrip tests: parse RON text → Value → serialize → parse again.
//!
//! These tests verify that parsing and serialization are consistent.

use ron2::{Map, NamedContent, Number, PrettyConfig, Value, from_str, to_string, to_string_pretty};

/// Helper: parse, serialize, parse again, and verify values match.
fn roundtrip(input: &str) -> Value {
    let value = from_str(input).unwrap_or_else(|e| panic!("Failed to parse '{input}': {e}"));
    let serialized = to_string(&value).unwrap_or_else(|e| panic!("Failed to serialize: {e}"));
    let reparsed = from_str(&serialized)
        .unwrap_or_else(|e| panic!("Failed to reparse '{serialized}' (from '{input}'): {e}"));
    assert_eq!(value, reparsed, "Roundtrip mismatch for '{input}'");
    value
}

/// Helper: roundtrip with pretty printing.
fn roundtrip_pretty(input: &str) -> Value {
    let value = from_str(input).unwrap_or_else(|e| panic!("Failed to parse '{input}': {e}"));
    let serialized = to_string_pretty(&value, PrettyConfig::new())
        .unwrap_or_else(|e| panic!("Failed to serialize: {e}"));
    let reparsed = from_str(&serialized)
        .unwrap_or_else(|e| panic!("Failed to reparse '{serialized}' (from '{input}'): {e}"));
    assert_eq!(value, reparsed, "Roundtrip mismatch for '{input}'");
    value
}

// =============================================================================
// Primitive Types
// =============================================================================

#[test]
fn roundtrip_unit() {
    let value = roundtrip("()");
    assert_eq!(value, Value::Unit);
}

#[test]
fn roundtrip_bool_true() {
    let value = roundtrip("true");
    assert_eq!(value, Value::Bool(true));
}

#[test]
fn roundtrip_bool_false() {
    let value = roundtrip("false");
    assert_eq!(value, Value::Bool(false));
}

#[test]
fn roundtrip_char_simple() {
    let value = roundtrip("'a'");
    assert_eq!(value, Value::Char('a'));
}

#[test]
fn roundtrip_char_escape_newline() {
    let value = roundtrip("'\\n'");
    assert_eq!(value, Value::Char('\n'));
}

#[test]
fn roundtrip_char_escape_tab() {
    let value = roundtrip("'\\t'");
    assert_eq!(value, Value::Char('\t'));
}

#[test]
fn roundtrip_char_escape_quote() {
    let value = roundtrip("'\\''");
    assert_eq!(value, Value::Char('\''));
}

#[test]
fn roundtrip_char_unicode() {
    let value = roundtrip("'\\u{1F600}'");
    assert_eq!(value, Value::Char('\u{1F600}'));
}

#[test]
fn roundtrip_char_emoji() {
    let value = roundtrip("'\u{1F600}'");
    assert_eq!(value, Value::Char('\u{1F600}'));
}

// =============================================================================
// Integers
// =============================================================================

#[test]
fn roundtrip_integer_zero() {
    let value = roundtrip("0");
    assert_eq!(value, Value::Number(Number::U8(0)));
}

#[test]
fn roundtrip_integer_positive() {
    let value = roundtrip("42");
    assert_eq!(value, Value::Number(Number::U8(42)));
}

#[test]
fn roundtrip_integer_negative() {
    let value = roundtrip("-42");
    assert_eq!(value, Value::Number(Number::I8(-42)));
}

#[test]
fn roundtrip_integer_large_positive() {
    let value = roundtrip("1000000");
    assert_eq!(value, Value::Number(Number::U32(1_000_000)));
}

#[test]
fn roundtrip_integer_large_negative() {
    let value = roundtrip("-1000000");
    assert_eq!(value, Value::Number(Number::I32(-1_000_000)));
}

#[test]
fn roundtrip_integer_u8_max() {
    let value = roundtrip("255");
    assert_eq!(value, Value::Number(Number::U8(255)));
}

#[test]
fn roundtrip_integer_u8_overflow() {
    let value = roundtrip("256");
    assert_eq!(value, Value::Number(Number::U16(256)));
}

#[test]
fn roundtrip_integer_i8_min() {
    let value = roundtrip("-128");
    assert_eq!(value, Value::Number(Number::I8(-128)));
}

#[test]
fn roundtrip_integer_i8_overflow() {
    let value = roundtrip("-129");
    assert_eq!(value, Value::Number(Number::I16(-129)));
}

#[test]
fn roundtrip_integer_hex() {
    let value = roundtrip("0xFF");
    assert_eq!(value, Value::Number(Number::U8(255)));
}

#[test]
fn roundtrip_integer_hex_lowercase() {
    let value = roundtrip("0xff");
    assert_eq!(value, Value::Number(Number::U8(255)));
}

#[test]
fn roundtrip_integer_binary() {
    let value = roundtrip("0b1010");
    assert_eq!(value, Value::Number(Number::U8(10)));
}

#[test]
fn roundtrip_integer_octal() {
    let value = roundtrip("0o77");
    assert_eq!(value, Value::Number(Number::U8(63)));
}

#[test]
fn roundtrip_integer_underscores() {
    let value = roundtrip("1_000_000");
    assert_eq!(value, Value::Number(Number::U32(1_000_000)));
}

#[test]
fn roundtrip_integer_hex_underscores() {
    let value = roundtrip("0xFF_FF");
    assert_eq!(value, Value::Number(Number::U16(0xFFFF)));
}

// =============================================================================
// Floats
// =============================================================================

#[test]
fn roundtrip_float_simple() {
    let value = roundtrip("3.14");
    match value {
        Value::Number(Number::F32(_) | Number::F64(_)) => {}
        _ => panic!("Expected float, got {value:?}"),
    }
}

#[test]
fn roundtrip_float_integer_form() {
    // "1.0" should roundtrip as a float
    let value = roundtrip("1.0");
    match value {
        Value::Number(Number::F32(_) | Number::F64(_)) => {}
        _ => panic!("Expected float, got {value:?}"),
    }
}

#[test]
fn roundtrip_float_negative() {
    let value = roundtrip("-2.5");
    match value {
        Value::Number(Number::F32(f)) if (f.get() - (-2.5_f32)).abs() < f32::EPSILON => {}
        Value::Number(Number::F64(f)) if (f.get() - (-2.5_f64)).abs() < f64::EPSILON => {}
        _ => panic!("Expected -2.5, got {value:?}"),
    }
}

#[test]
fn roundtrip_float_exponent() {
    let value = roundtrip("1e10");
    match value {
        Value::Number(Number::F32(_) | Number::F64(_)) => {}
        _ => panic!("Expected float, got {value:?}"),
    }
}

#[test]
fn roundtrip_float_negative_exponent() {
    let value = roundtrip("1e-10");
    match value {
        Value::Number(Number::F32(_) | Number::F64(_)) => {}
        _ => panic!("Expected float, got {value:?}"),
    }
}

#[test]
fn roundtrip_float_infinity() {
    let value = roundtrip("inf");
    match value {
        Value::Number(Number::F64(f)) if f.get().is_infinite() && f.get().is_sign_positive() => {}
        _ => panic!("Expected +inf, got {value:?}"),
    }
}

#[test]
fn roundtrip_float_negative_infinity() {
    let value = roundtrip("-inf");
    match value {
        Value::Number(Number::F64(f)) if f.get().is_infinite() && f.get().is_sign_negative() => {}
        _ => panic!("Expected -inf, got {value:?}"),
    }
}

#[test]
fn roundtrip_float_nan() {
    let value = roundtrip("NaN");
    match value {
        Value::Number(Number::F64(f)) if f.get().is_nan() => {}
        _ => panic!("Expected NaN, got {value:?}"),
    }
}

// =============================================================================
// Strings
// =============================================================================

#[test]
fn roundtrip_string_empty() {
    let value = roundtrip(r#""""#);
    assert_eq!(value, Value::String(String::new()));
}

#[test]
fn roundtrip_string_simple() {
    let value = roundtrip(r#""hello""#);
    assert_eq!(value, Value::String(String::from("hello")));
}

#[test]
fn roundtrip_string_with_spaces() {
    let value = roundtrip(r#""hello world""#);
    assert_eq!(value, Value::String(String::from("hello world")));
}

#[test]
fn roundtrip_string_escape_newline() {
    let value = roundtrip(r#""line1\nline2""#);
    assert_eq!(value, Value::String(String::from("line1\nline2")));
}

#[test]
fn roundtrip_string_escape_tab() {
    let value = roundtrip(r#""col1\tcol2""#);
    assert_eq!(value, Value::String(String::from("col1\tcol2")));
}

#[test]
fn roundtrip_string_escape_quote() {
    let value = roundtrip(r#""say \"hello\"""#);
    assert_eq!(value, Value::String(String::from("say \"hello\"")));
}

#[test]
fn roundtrip_string_escape_backslash() {
    let value = roundtrip(r#""path\\to\\file""#);
    assert_eq!(value, Value::String(String::from("path\\to\\file")));
}

#[test]
fn roundtrip_string_unicode() {
    let value = roundtrip(r#""hello \u{1F600}""#);
    assert_eq!(value, Value::String(String::from("hello \u{1F600}")));
}

#[test]
fn roundtrip_string_raw() {
    let value = roundtrip(r##"r#"raw string"#"##);
    assert_eq!(value, Value::String(String::from("raw string")));
}

#[test]
fn roundtrip_string_raw_multiline() {
    let value = roundtrip(
        r##"r#"line1
line2"#"##,
    );
    assert_eq!(value, Value::String(String::from("line1\nline2")));
}

// =============================================================================
// Bytes
// =============================================================================

#[test]
fn roundtrip_bytes_empty() {
    let value = roundtrip(r#"b"""#);
    assert_eq!(value, Value::Bytes(vec![]));
}

#[test]
fn roundtrip_bytes_simple() {
    let value = roundtrip(r#"b"hello""#);
    assert_eq!(value, Value::Bytes(b"hello".to_vec()));
}

#[test]
fn roundtrip_bytes_escape() {
    let value = roundtrip(r#"b"line1\nline2""#);
    assert_eq!(value, Value::Bytes(b"line1\nline2".to_vec()));
}

#[test]
fn roundtrip_bytes_hex() {
    let value = roundtrip(r#"b"\x00\xFF""#);
    assert_eq!(value, Value::Bytes(vec![0x00, 0xFF]));
}

// =============================================================================
// Option
// =============================================================================

#[test]
fn roundtrip_option_none() {
    let value = roundtrip("None");
    assert_eq!(value, Value::Option(None));
}

#[test]
fn roundtrip_option_some_int() {
    let value = roundtrip("Some(42)");
    assert_eq!(
        value,
        Value::Option(Some(Box::new(Value::Number(Number::U8(42)))))
    );
}

#[test]
fn roundtrip_option_some_string() {
    let value = roundtrip(r#"Some("hello")"#);
    assert_eq!(
        value,
        Value::Option(Some(Box::new(Value::String(String::from("hello")))))
    );
}

#[test]
fn roundtrip_option_some_none() {
    let value = roundtrip("Some(None)");
    assert_eq!(value, Value::Option(Some(Box::new(Value::Option(None)))));
}

#[test]
fn roundtrip_option_nested() {
    let value = roundtrip("Some(Some(42))");
    assert_eq!(
        value,
        Value::Option(Some(Box::new(Value::Option(Some(Box::new(
            Value::Number(Number::U8(42))
        ))))))
    );
}

// =============================================================================
// Sequences
// =============================================================================

#[test]
fn roundtrip_seq_empty() {
    let value = roundtrip("[]");
    assert_eq!(value, Value::Seq(vec![]));
}

#[test]
fn roundtrip_seq_single() {
    let value = roundtrip("[1]");
    assert_eq!(value, Value::Seq(vec![Value::Number(Number::U8(1))]));
}

#[test]
fn roundtrip_seq_integers() {
    let value = roundtrip("[1, 2, 3]");
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
fn roundtrip_seq_mixed_types() {
    let value = roundtrip(r#"[1, "hello", true]"#);
    assert_eq!(
        value,
        Value::Seq(vec![
            Value::Number(Number::U8(1)),
            Value::String(String::from("hello")),
            Value::Bool(true),
        ])
    );
}

#[test]
fn roundtrip_seq_nested() {
    let value = roundtrip("[[1, 2], [3, 4]]");
    assert_eq!(
        value,
        Value::Seq(vec![
            Value::Seq(vec![
                Value::Number(Number::U8(1)),
                Value::Number(Number::U8(2)),
            ]),
            Value::Seq(vec![
                Value::Number(Number::U8(3)),
                Value::Number(Number::U8(4)),
            ]),
        ])
    );
}

#[test]
fn roundtrip_seq_trailing_comma() {
    let value = roundtrip("[1, 2, 3,]");
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
fn roundtrip_seq_with_whitespace() {
    let value = roundtrip("[ 1 , 2 , 3 ]");
    assert_eq!(
        value,
        Value::Seq(vec![
            Value::Number(Number::U8(1)),
            Value::Number(Number::U8(2)),
            Value::Number(Number::U8(3)),
        ])
    );
}

// =============================================================================
// Tuples
// =============================================================================

#[test]
fn roundtrip_tuple_pair() {
    let value = roundtrip("(1, 2)");
    assert_eq!(
        value,
        Value::Tuple(vec![
            Value::Number(Number::U8(1)),
            Value::Number(Number::U8(2)),
        ])
    );
}

#[test]
fn roundtrip_tuple_triple() {
    let value = roundtrip("(1, 2, 3)");
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
fn roundtrip_tuple_mixed_types() {
    let value = roundtrip(r#"(1, "hello", true)"#);
    assert_eq!(
        value,
        Value::Tuple(vec![
            Value::Number(Number::U8(1)),
            Value::String(String::from("hello")),
            Value::Bool(true),
        ])
    );
}

#[test]
fn roundtrip_tuple_nested() {
    let value = roundtrip("((1, 2), (3, 4))");
    assert_eq!(
        value,
        Value::Tuple(vec![
            Value::Tuple(vec![
                Value::Number(Number::U8(1)),
                Value::Number(Number::U8(2)),
            ]),
            Value::Tuple(vec![
                Value::Number(Number::U8(3)),
                Value::Number(Number::U8(4)),
            ]),
        ])
    );
}

#[test]
fn roundtrip_tuple_trailing_comma() {
    let value = roundtrip("(1, 2, 3,)");
    assert_eq!(
        value,
        Value::Tuple(vec![
            Value::Number(Number::U8(1)),
            Value::Number(Number::U8(2)),
            Value::Number(Number::U8(3)),
        ])
    );
}

// =============================================================================
// Maps
// =============================================================================

#[test]
fn roundtrip_map_empty() {
    let value = roundtrip("{}");
    assert_eq!(value, Value::Map(Map::new()));
}

#[test]
fn roundtrip_map_string_keys() {
    let value = roundtrip(r#"{ "x": 1, "y": 2 }"#);
    let mut expected = Map::new();
    expected.insert(
        Value::String(String::from("x")),
        Value::Number(Number::U8(1)),
    );
    expected.insert(
        Value::String(String::from("y")),
        Value::Number(Number::U8(2)),
    );
    assert_eq!(value, Value::Map(expected));
}

#[test]
fn roundtrip_map_integer_keys() {
    let value = roundtrip("{ 1: \"one\", 2: \"two\" }");
    let mut expected = Map::new();
    expected.insert(
        Value::Number(Number::U8(1)),
        Value::String(String::from("one")),
    );
    expected.insert(
        Value::Number(Number::U8(2)),
        Value::String(String::from("two")),
    );
    assert_eq!(value, Value::Map(expected));
}

#[test]
fn roundtrip_map_identifier_keys() {
    // Identifiers in map position become Named units
    let value = roundtrip("{ x: 1, y: 2 }");
    let mut expected = Map::new();
    expected.insert(
        Value::Named {
            name: String::from("x"),
            content: NamedContent::Unit,
        },
        Value::Number(Number::U8(1)),
    );
    expected.insert(
        Value::Named {
            name: String::from("y"),
            content: NamedContent::Unit,
        },
        Value::Number(Number::U8(2)),
    );
    assert_eq!(value, Value::Map(expected));
}

#[test]
fn roundtrip_map_nested() {
    let value = roundtrip(r#"{ "outer": { "inner": 1 } }"#);
    let mut inner = Map::new();
    inner.insert(
        Value::String(String::from("inner")),
        Value::Number(Number::U8(1)),
    );
    let mut expected = Map::new();
    expected.insert(Value::String(String::from("outer")), Value::Map(inner));
    assert_eq!(value, Value::Map(expected));
}

#[test]
fn roundtrip_map_trailing_comma() {
    let value = roundtrip(r#"{ "x": 1, "y": 2, }"#);
    let mut expected = Map::new();
    expected.insert(
        Value::String(String::from("x")),
        Value::Number(Number::U8(1)),
    );
    expected.insert(
        Value::String(String::from("y")),
        Value::Number(Number::U8(2)),
    );
    assert_eq!(value, Value::Map(expected));
}

// =============================================================================
// Named Types - Unit
// =============================================================================

#[test]
fn roundtrip_named_unit_simple() {
    let value = roundtrip("Point");
    assert_eq!(
        value,
        Value::Named {
            name: String::from("Point"),
            content: NamedContent::Unit,
        }
    );
}

// Note: :: paths are not supported by the parser

#[test]
fn roundtrip_named_unit_long_name() {
    let value = roundtrip("MyVeryLongTypeName");
    assert_eq!(
        value,
        Value::Named {
            name: String::from("MyVeryLongTypeName"),
            content: NamedContent::Unit,
        }
    );
}

// =============================================================================
// Named Types - Tuple
// =============================================================================

#[test]
fn roundtrip_named_tuple_single() {
    let value = roundtrip("Wrapper(42)");
    assert_eq!(
        value,
        Value::Named {
            name: String::from("Wrapper"),
            content: NamedContent::Tuple(vec![Value::Number(Number::U8(42))]),
        }
    );
}

#[test]
fn roundtrip_named_tuple_pair() {
    let value = roundtrip("Point(1, 2)");
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
fn roundtrip_named_tuple_triple() {
    let value = roundtrip("Color(255, 128, 0)");
    assert_eq!(
        value,
        Value::Named {
            name: String::from("Color"),
            content: NamedContent::Tuple(vec![
                Value::Number(Number::U8(255)),
                Value::Number(Number::U8(128)),
                Value::Number(Number::U8(0)),
            ]),
        }
    );
}

#[test]
fn roundtrip_named_tuple_mixed() {
    let value = roundtrip(r#"Entry("key", 42)"#);
    assert_eq!(
        value,
        Value::Named {
            name: String::from("Entry"),
            content: NamedContent::Tuple(vec![
                Value::String(String::from("key")),
                Value::Number(Number::U8(42)),
            ]),
        }
    );
}

#[test]
fn roundtrip_named_tuple_nested() {
    let value = roundtrip("Outer(Inner(1))");
    assert_eq!(
        value,
        Value::Named {
            name: String::from("Outer"),
            content: NamedContent::Tuple(vec![Value::Named {
                name: String::from("Inner"),
                content: NamedContent::Tuple(vec![Value::Number(Number::U8(1))]),
            }]),
        }
    );
}

// Note: Option::Some(42) is not valid - Some is parsed as Value::Option

// =============================================================================
// Named Types - Struct
// =============================================================================

// Named structs use brace syntax

#[test]
fn roundtrip_named_struct_single_field() {
    let value = roundtrip("Wrapper { value: 42 }");
    assert_eq!(
        value,
        Value::Named {
            name: String::from("Wrapper"),
            content: NamedContent::Struct(vec![(
                String::from("value"),
                Value::Number(Number::U8(42))
            )]),
        }
    );
}

#[test]
fn roundtrip_named_struct_two_fields() {
    let value = roundtrip("Point { x: 1, y: 2 }");
    assert_eq!(
        value,
        Value::Named {
            name: String::from("Point"),
            content: NamedContent::Struct(vec![
                (String::from("x"), Value::Number(Number::U8(1))),
                (String::from("y"), Value::Number(Number::U8(2))),
            ]),
        }
    );
}

#[test]
fn roundtrip_named_struct_three_fields() {
    let value = roundtrip("Point3D { x: 1, y: 2, z: 3 }");
    assert_eq!(
        value,
        Value::Named {
            name: String::from("Point3D"),
            content: NamedContent::Struct(vec![
                (String::from("x"), Value::Number(Number::U8(1))),
                (String::from("y"), Value::Number(Number::U8(2))),
                (String::from("z"), Value::Number(Number::U8(3))),
            ]),
        }
    );
}

#[test]
fn roundtrip_named_struct_mixed_types() {
    let value = roundtrip(r#"Person { name: "Alice", age: 30, active: true }"#);
    assert_eq!(
        value,
        Value::Named {
            name: String::from("Person"),
            content: NamedContent::Struct(vec![
                (String::from("name"), Value::String(String::from("Alice"))),
                (String::from("age"), Value::Number(Number::U8(30))),
                (String::from("active"), Value::Bool(true)),
            ]),
        }
    );
}

#[test]
fn roundtrip_named_struct_nested() {
    let value = roundtrip("Outer { inner: Inner { x: 1 } }");
    assert_eq!(
        value,
        Value::Named {
            name: String::from("Outer"),
            content: NamedContent::Struct(vec![(
                String::from("inner"),
                Value::Named {
                    name: String::from("Inner"),
                    content: NamedContent::Struct(vec![(
                        String::from("x"),
                        Value::Number(Number::U8(1))
                    )]),
                }
            )]),
        }
    );
}

#[test]
fn roundtrip_named_struct_with_seq() {
    let value = roundtrip("Container { items: [1, 2, 3] }");
    assert_eq!(
        value,
        Value::Named {
            name: String::from("Container"),
            content: NamedContent::Struct(vec![(
                String::from("items"),
                Value::Seq(vec![
                    Value::Number(Number::U8(1)),
                    Value::Number(Number::U8(2)),
                    Value::Number(Number::U8(3)),
                ])
            )]),
        }
    );
}

#[test]
fn roundtrip_named_struct_with_option() {
    let value = roundtrip("Config { value: Some(42) }");
    assert_eq!(
        value,
        Value::Named {
            name: String::from("Config"),
            content: NamedContent::Struct(vec![(
                String::from("value"),
                Value::Option(Some(Box::new(Value::Number(Number::U8(42)))))
            )]),
        }
    );
}

#[test]
fn roundtrip_named_struct_trailing_comma() {
    let value = roundtrip("Point { x: 1, y: 2, }");
    assert_eq!(
        value,
        Value::Named {
            name: String::from("Point"),
            content: NamedContent::Struct(vec![
                (String::from("x"), Value::Number(Number::U8(1))),
                (String::from("y"), Value::Number(Number::U8(2))),
            ]),
        }
    );
}

// =============================================================================
// Pretty Printing Roundtrips
// =============================================================================

#[test]
fn roundtrip_pretty_seq() {
    roundtrip_pretty("[1, 2, 3, 4, 5]");
}

#[test]
fn roundtrip_pretty_nested_seq() {
    roundtrip_pretty("[[1, 2], [3, 4], [5, 6]]");
}

#[test]
fn roundtrip_pretty_map() {
    roundtrip_pretty(r#"{ "a": 1, "b": 2, "c": 3 }"#);
}

#[test]
fn roundtrip_pretty_named_struct() {
    roundtrip_pretty("Config { host: \"localhost\", port: 8080 }");
}

#[test]
fn roundtrip_pretty_complex() {
    roundtrip_pretty(
        r#"Server {
    name: "main",
    endpoints: [
        Endpoint("/api", "GET"),
        Endpoint("/health", "GET")
    ],
    config: Config(30, 3)
}"#,
    );
}

// =============================================================================
// Comments and Whitespace
// =============================================================================

#[test]
fn roundtrip_with_line_comment() {
    let value = roundtrip("42 // this is a comment");
    assert_eq!(value, Value::Number(Number::U8(42)));
}

#[test]
fn roundtrip_with_block_comment() {
    let value = roundtrip("/* comment */ 42 /* another */");
    assert_eq!(value, Value::Number(Number::U8(42)));
}

#[test]
fn roundtrip_with_leading_whitespace() {
    let value = roundtrip("   42");
    assert_eq!(value, Value::Number(Number::U8(42)));
}

#[test]
fn roundtrip_with_trailing_whitespace() {
    let value = roundtrip("42   ");
    assert_eq!(value, Value::Number(Number::U8(42)));
}

#[test]
fn roundtrip_multiline() {
    let value = roundtrip(
        r#"
        [
            1,
            2,
            3
        ]
        "#,
    );
    assert_eq!(
        value,
        Value::Seq(vec![
            Value::Number(Number::U8(1)),
            Value::Number(Number::U8(2)),
            Value::Number(Number::U8(3)),
        ])
    );
}

// =============================================================================
// Complex / Edge Cases
// =============================================================================

#[test]
fn roundtrip_deeply_nested() {
    let value = roundtrip("[[[[[1]]]]]");
    assert_eq!(
        value,
        Value::Seq(vec![Value::Seq(vec![Value::Seq(vec![Value::Seq(vec![
            Value::Seq(vec![Value::Number(Number::U8(1))])
        ])])])])
    );
}

#[test]
fn roundtrip_mixed_collection_types() {
    let value = roundtrip(r#"[(1, 2), { "a": 3 }, [4, 5]]"#);
    let mut map = Map::new();
    map.insert(
        Value::String(String::from("a")),
        Value::Number(Number::U8(3)),
    );
    assert_eq!(
        value,
        Value::Seq(vec![
            Value::Tuple(vec![
                Value::Number(Number::U8(1)),
                Value::Number(Number::U8(2)),
            ]),
            Value::Map(map),
            Value::Seq(vec![
                Value::Number(Number::U8(4)),
                Value::Number(Number::U8(5)),
            ]),
        ])
    );
}

#[test]
fn roundtrip_named_with_all_content_types() {
    // Unit
    let _ = roundtrip("Variant");
    // Tuple
    let _ = roundtrip("Variant(1, 2)");
    // Struct
    let _ = roundtrip("Variant(x: 1, y: 2)");
}

#[test]
fn roundtrip_realistic_config() {
    roundtrip(
        r#"AppConfig(
    server: ServerConfig(
        host: "0.0.0.0",
        port: 8080,
        tls: Some(TlsConfig(
            cert: "/path/to/cert",
            key: "/path/to/key"
        ))
    ),
    database: DatabaseConfig(
        url: "postgres://localhost/db",
        pool_size: 10,
        timeout: 30
    ),
    features: [
        Feature(name: "auth", enabled: true),
        Feature(name: "metrics", enabled: false)
    ],
    metadata: {
        "version": "1.0.0",
        "environment": "production"
    }
)"#,
    );
}

#[test]
fn roundtrip_enum_variants_various() {
    // Different enum variant styles (without :: paths since those aren't supported)
    let _ = roundtrip("Pending");
    let _ = roundtrip("Running(123)");
    let _ = roundtrip("Complete(code: 0, message: \"success\")");
}

#[test]
fn roundtrip_raw_identifier() {
    // Raw identifier (r#type is valid) - the r# prefix is preserved
    let value = roundtrip("r#type");
    assert_eq!(
        value,
        Value::Named {
            name: String::from("r#type"),
            content: NamedContent::Unit,
        }
    );
}

// =============================================================================
// Empty Document
// =============================================================================

#[test]
fn roundtrip_empty_document() {
    // Empty input parses to Unit
    let value = from_str("").unwrap();
    assert_eq!(value, Value::Unit);
}

#[test]
fn roundtrip_whitespace_only() {
    let value = from_str("   \n   \t   ").unwrap();
    assert_eq!(value, Value::Unit);
}

#[test]
fn roundtrip_comment_only() {
    let value = from_str("// just a comment\n").unwrap();
    assert_eq!(value, Value::Unit);
}

// =============================================================================
// Anonymous Structs
// =============================================================================

#[test]
fn roundtrip_anon_struct_simple() {
    let value = roundtrip(r#"(name: "test", value: 42)"#);
    assert_eq!(
        value,
        Value::Struct(vec![
            (String::from("name"), Value::String(String::from("test"))),
            (String::from("value"), Value::Number(Number::U8(42))),
        ])
    );
}

#[test]
fn roundtrip_anon_struct_single_field() {
    let value = roundtrip("(x: 1)");
    assert_eq!(
        value,
        Value::Struct(vec![(String::from("x"), Value::Number(Number::U8(1)))])
    );
}

#[test]
fn roundtrip_anon_struct_trailing_comma() {
    let value = roundtrip("(x: 1, y: 2,)");
    assert_eq!(
        value,
        Value::Struct(vec![
            (String::from("x"), Value::Number(Number::U8(1))),
            (String::from("y"), Value::Number(Number::U8(2))),
        ])
    );
}

#[test]
fn roundtrip_anon_struct_nested() {
    let value = roundtrip("(outer: (inner: 42))");
    assert_eq!(
        value,
        Value::Struct(vec![(
            String::from("outer"),
            Value::Struct(vec![(String::from("inner"), Value::Number(Number::U8(42)))])
        )])
    );
}

#[test]
fn roundtrip_anon_struct_with_seq() {
    let value = roundtrip("(items: [1, 2, 3])");
    assert_eq!(
        value,
        Value::Struct(vec![(
            String::from("items"),
            Value::Seq(vec![
                Value::Number(Number::U8(1)),
                Value::Number(Number::U8(2)),
                Value::Number(Number::U8(3)),
            ])
        )])
    );
}

#[test]
fn roundtrip_anon_struct_with_option() {
    let value = roundtrip("(value: Some(42))");
    assert_eq!(
        value,
        Value::Struct(vec![(
            String::from("value"),
            Value::Option(Some(Box::new(Value::Number(Number::U8(42)))))
        )])
    );
}

#[test]
fn roundtrip_anon_struct_mixed_types() {
    let value = roundtrip(r#"(name: "Alice", age: 30, active: true)"#);
    assert_eq!(
        value,
        Value::Struct(vec![
            (String::from("name"), Value::String(String::from("Alice"))),
            (String::from("age"), Value::Number(Number::U8(30))),
            (String::from("active"), Value::Bool(true)),
        ])
    );
}

#[test]
fn roundtrip_anon_struct_pretty() {
    roundtrip_pretty("(x: 1, y: 2)");
}

#[test]
fn roundtrip_anon_struct_multiline() {
    roundtrip_pretty("(\n    x: 1,\n    y: 2\n)");
}
