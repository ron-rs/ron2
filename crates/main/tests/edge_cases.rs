//! Edge case tests and error handling.
//!
//! These tests cover boundary conditions, error cases, and unusual inputs.

use ron2::{FormatConfig, Map, NamedContent, Number, ToRon, Value};

// =============================================================================
// Number Boundaries
// =============================================================================

#[test]
fn edge_u8_boundary() {
    // 255 fits in u8
    let value: Value = "255".parse().unwrap();
    assert_eq!(value, Value::Number(Number::U8(255)));

    // 256 overflows to u16
    let value: Value = "256".parse().unwrap();
    assert_eq!(value, Value::Number(Number::U16(256)));
}

#[test]
fn edge_u16_boundary() {
    let value: Value = "65535".parse().unwrap();
    assert_eq!(value, Value::Number(Number::U16(65535)));

    let value: Value = "65536".parse().unwrap();
    assert_eq!(value, Value::Number(Number::U32(65536)));
}

#[test]
fn edge_u32_boundary() {
    let value: Value = "4294967295".parse().unwrap();
    assert_eq!(value, Value::Number(Number::U32(4_294_967_295)));

    let value: Value = "4294967296".parse().unwrap();
    assert_eq!(value, Value::Number(Number::U64(4_294_967_296)));
}

#[test]
fn edge_i8_boundary() {
    // -128 fits in i8
    let value: Value = "-128".parse().unwrap();
    assert_eq!(value, Value::Number(Number::I8(-128)));

    // -129 overflows to i16
    let value: Value = "-129".parse().unwrap();
    assert_eq!(value, Value::Number(Number::I16(-129)));
}

#[test]
fn edge_i16_boundary() {
    let value: Value = "-32768".parse().unwrap();
    assert_eq!(value, Value::Number(Number::I16(-32768)));

    let value: Value = "-32769".parse().unwrap();
    assert_eq!(value, Value::Number(Number::I32(-32769)));
}

#[test]
fn edge_i32_boundary() {
    let value: Value = "-2147483648".parse().unwrap();
    assert_eq!(value, Value::Number(Number::I32(-2_147_483_648)));

    let value: Value = "-2147483649".parse().unwrap();
    assert_eq!(value, Value::Number(Number::I64(-2_147_483_649)));
}

// =============================================================================
// Empty Collections
// =============================================================================

#[test]
fn edge_empty_seq() {
    let value: Value = "[]".parse().unwrap();
    assert_eq!(value, Value::Seq(vec![]));
    assert_eq!(value.to_ron_with(&FormatConfig::minimal()).unwrap(), "[]");
}

#[test]
fn edge_empty_map() {
    let value: Value = "{}".parse().unwrap();
    assert_eq!(value, Value::Map(Map::new()));
    assert_eq!(value.to_ron_with(&FormatConfig::minimal()).unwrap(), "{}");
}

#[test]
fn edge_empty_tuple_is_unit() {
    // () is unit, not empty tuple
    let value: Value = "()".parse().unwrap();
    assert_eq!(value, Value::Unit);
}

// =============================================================================
// Singleton Collections
// =============================================================================

#[test]
fn edge_single_element_seq() {
    let value: Value = "[42]".parse().unwrap();
    assert_eq!(value, Value::Seq(vec![Value::Number(Number::U8(42))]));
}

#[test]
fn edge_single_element_map() {
    let value: Value = r#"{ "key": 42 }"#.parse().unwrap();
    let mut expected = Map::new();
    expected.insert(
        Value::String(String::from("key")),
        Value::Number(Number::U8(42)),
    );
    assert_eq!(value, Value::Map(expected));
}

// =============================================================================
// Trailing Commas
// =============================================================================

#[test]
fn edge_trailing_comma_seq() {
    let value: Value = "[1,]".parse().unwrap();
    assert_eq!(value, Value::Seq(vec![Value::Number(Number::U8(1))]));
}

#[test]
fn edge_trailing_comma_tuple() {
    let value: Value = "(1, 2,)".parse().unwrap();
    assert_eq!(
        value,
        Value::Tuple(vec![
            Value::Number(Number::U8(1)),
            Value::Number(Number::U8(2)),
        ])
    );
}

#[test]
fn edge_trailing_comma_map() {
    let value: Value = r#"{ "a": 1, }"#.parse().unwrap();
    let mut expected = Map::new();
    expected.insert(
        Value::String(String::from("a")),
        Value::Number(Number::U8(1)),
    );
    assert_eq!(value, Value::Map(expected));
}

#[test]
fn edge_trailing_comma_named_struct() {
    // Named structs use parentheses: Point(x: 1)
    let value: Value = "Point(x: 1,)".parse().unwrap();
    assert_eq!(
        value,
        Value::Named {
            name: String::from("Point"),
            content: NamedContent::Struct(vec![(String::from("x"), Value::Number(Number::U8(1)))]),
        }
    );
}

#[test]
fn edge_trailing_comma_named_tuple() {
    let value: Value = "Point(1, 2,)".parse().unwrap();
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

// =============================================================================
// String Edge Cases
// =============================================================================

#[test]
fn edge_string_empty() {
    let value: Value = r#""""#.parse().unwrap();
    assert_eq!(value, Value::String(String::new()));
}

#[test]
fn edge_string_all_escapes() {
    let value: Value = r#""\\\"\n\r\t\0""#.parse().unwrap();
    assert_eq!(value, Value::String(String::from("\\\"\n\r\t\0")));
}

#[test]
fn edge_string_unicode_escape() {
    let value: Value = r#""\u{0041}""#.parse().unwrap();
    assert_eq!(value, Value::String(String::from("A")));
}

#[test]
fn edge_string_unicode_escape_zero() {
    let value: Value = r#""\u{0}""#.parse().unwrap();
    assert_eq!(value, Value::String(String::from("\0")));
}

#[test]
fn edge_string_unicode_max_bmp() {
    let value: Value = r#""\u{FFFF}""#.parse().unwrap();
    assert_eq!(value, Value::String(String::from("\u{FFFF}")));
}

#[test]
fn edge_raw_string_with_quotes() {
    let value: Value = r##"r#"hello "world""#"##.parse().unwrap();
    assert_eq!(value, Value::String(String::from("hello \"world\"")));
}

#[test]
fn edge_raw_string_with_hash() {
    let value: Value = r###"r##"hello #world"##"###.parse().unwrap();
    assert_eq!(value, Value::String(String::from("hello #world")));
}

#[test]
fn edge_raw_string_with_triple_hash_in_contexts() {
    let raw_literal = concat!(
        "r###\"\n",
        "hello \"world\"\n",
        "wowweee\"' r##\"\"##\n",
        "lol\"###",
    );
    let raw_text = String::from("\nhello \"world\"\nwowweee\"' r##\"\"##\nlol");
    let raw_value = Value::String(raw_text);
    let input = format!(
        "(value: {raw}, seq: [{raw}], tuple: ({raw},), map: {{ {raw}: {raw} }}, \
named_tuple: Wrapper({raw}), named_struct: Wrapper(value: {raw}),)",
        raw = raw_literal,
    );
    let value: Value = input.parse().unwrap();

    let mut expected_map = Map::new();
    expected_map.insert(raw_value.clone(), raw_value.clone());

    let expected = Value::Struct(vec![
        (String::from("value"), raw_value.clone()),
        (String::from("seq"), Value::Seq(vec![raw_value.clone()])),
        (String::from("tuple"), Value::Tuple(vec![raw_value.clone()])),
        (String::from("map"), Value::Map(expected_map)),
        (
            String::from("named_tuple"),
            Value::Named {
                name: String::from("Wrapper"),
                content: NamedContent::Tuple(vec![raw_value.clone()]),
            },
        ),
        (
            String::from("named_struct"),
            Value::Named {
                name: String::from("Wrapper"),
                content: NamedContent::Struct(vec![(String::from("value"), raw_value)]),
            },
        ),
    ]);

    assert_eq!(value, expected);
}

// =============================================================================
// Char Edge Cases
// =============================================================================

#[test]
fn edge_char_space() {
    let value: Value = "' '".parse().unwrap();
    assert_eq!(value, Value::Char(' '));
}

#[test]
fn edge_char_null() {
    let value: Value = "'\\0'".parse().unwrap();
    assert_eq!(value, Value::Char('\0'));
}

#[test]
fn edge_char_unicode_escape() {
    let value: Value = "'\\u{1F600}'".parse().unwrap();
    assert_eq!(value, Value::Char('\u{1F600}'));
}

// =============================================================================
// Bytes Edge Cases
// =============================================================================

#[test]
fn edge_bytes_empty() {
    let value: Value = r#"b"""#.parse().unwrap();
    assert_eq!(value, Value::Bytes(vec![]));
}

#[test]
fn edge_bytes_all_values() {
    // Test full range via hex escapes
    let value: Value = r#"b"\x00\x7F\x80\xFF""#.parse().unwrap();
    assert_eq!(value, Value::Bytes(vec![0x00, 0x7F, 0x80, 0xFF]));
}

#[test]
fn edge_raw_bytes() {
    let value: Value = r#"br"hello""#.parse().unwrap();
    assert_eq!(value, Value::Bytes(b"hello".to_vec()));
}

#[test]
fn edge_raw_bytes_with_hash() {
    let value: Value = r##"br#"hello"#"##.parse().unwrap();
    assert_eq!(value, Value::Bytes(b"hello".to_vec()));
}

#[test]
fn edge_raw_bytes_with_double_hash() {
    let value: Value = r###"br##"hello"##"###.parse().unwrap();
    assert_eq!(value, Value::Bytes(b"hello".to_vec()));
}

#[test]
fn edge_raw_bytes_with_embedded_quote() {
    let value: Value = r##"br#"hello "world""#"##.parse().unwrap();
    assert_eq!(value, Value::Bytes(br#"hello "world""#.to_vec()));
}

// =============================================================================
// Named Type Edge Cases
// =============================================================================

#[test]
fn edge_named_empty_tuple() {
    // Named type with empty parens is an empty tuple, not unit
    let value: Value = "Wrapper()".parse().unwrap();
    assert_eq!(
        value,
        Value::Named {
            name: String::from("Wrapper"),
            content: NamedContent::Tuple(vec![]),
        }
    );
}

#[test]
fn edge_named_empty_struct() {
    // Named type with empty parens - still parsed as empty tuple
    // There's no way to distinguish empty struct from empty tuple in RON syntax
    let value: Value = "Point()".parse().unwrap();
    assert_eq!(
        value,
        Value::Named {
            name: String::from("Point"),
            content: NamedContent::Tuple(vec![]),
        }
    );
}

#[test]
fn edge_named_long_identifier() {
    // Note: the parser doesn't support `::` paths, only single identifiers
    let value: Value = "VeryLongTypeName".parse().unwrap();
    assert_eq!(
        value,
        Value::Named {
            name: String::from("VeryLongTypeName"),
            content: NamedContent::Unit,
        }
    );
}

#[test]
fn edge_named_underscores() {
    let value: Value = "my_type_name".parse().unwrap();
    assert_eq!(
        value,
        Value::Named {
            name: String::from("my_type_name"),
            content: NamedContent::Unit,
        }
    );
}

#[test]
fn edge_named_numbers_in_name() {
    let value: Value = "Type123".parse().unwrap();
    assert_eq!(
        value,
        Value::Named {
            name: String::from("Type123"),
            content: NamedContent::Unit,
        }
    );
}

// =============================================================================
// Whitespace Handling
// =============================================================================

#[test]
fn edge_multiline_seq() {
    let input = r#"[
        1,
        2,
        3
    ]"#;
    let value: Value = input.parse().unwrap();
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
fn edge_multiline_named_struct() {
    // Named structs use parentheses for fields
    let input = r#"Config(
        host: "localhost",
        port: 8080
    )"#;
    let value: Value = input.parse().unwrap();
    assert_eq!(
        value,
        Value::Named {
            name: String::from("Config"),
            content: NamedContent::Struct(vec![
                (
                    String::from("host"),
                    Value::String(String::from("localhost"))
                ),
                (String::from("port"), Value::Number(Number::U16(8080))),
            ]),
        }
    );
}

#[test]
fn edge_no_whitespace() {
    let value: Value = r#"[1,2,3]"#.parse().unwrap();
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
fn edge_excessive_whitespace() {
    let value: Value = "   [   1   ,   2   ,   3   ]   ".parse().unwrap();
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
// Comment Handling
// =============================================================================

#[test]
fn edge_line_comment_at_end() {
    let value: Value = "42 // comment".parse().unwrap();
    assert_eq!(value, Value::Number(Number::U8(42)));
}

#[test]
fn edge_line_comment_before_value() {
    let value: Value = "// comment\n42".parse().unwrap();
    assert_eq!(value, Value::Number(Number::U8(42)));
}

#[test]
fn edge_block_comment_inline() {
    let value: Value = "42 /* comment */".parse().unwrap();
    assert_eq!(value, Value::Number(Number::U8(42)));
}

#[test]
fn edge_block_comment_multiline() {
    let value: Value = "/* line1\nline2 */ 42".parse().unwrap();
    assert_eq!(value, Value::Number(Number::U8(42)));
}

#[test]
fn edge_nested_block_comment() {
    let value: Value = "/* outer /* inner */ outer */ 42".parse().unwrap();
    assert_eq!(value, Value::Number(Number::U8(42)));
}

#[test]
fn edge_comments_in_seq() {
    let value: Value = r#"[
        1, // first
        2, /* second */
        3  // third
    ]"#.parse()
    .unwrap();
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
// Hex/Binary/Octal Numbers
// =============================================================================

#[test]
fn edge_hex_lowercase() {
    let value: Value = "0xff".parse().unwrap();
    assert_eq!(value, Value::Number(Number::U8(255)));
}

#[test]
fn edge_hex_uppercase() {
    let value: Value = "0XFF".parse().unwrap();
    assert_eq!(value, Value::Number(Number::U8(255)));
}

#[test]
fn edge_hex_mixed_case() {
    let value: Value = "0xAbCd".parse().unwrap();
    assert_eq!(value, Value::Number(Number::U16(0xABCD)));
}

#[test]
fn edge_binary() {
    let value: Value = "0b11111111".parse().unwrap();
    assert_eq!(value, Value::Number(Number::U8(255)));
}

#[test]
fn edge_binary_uppercase() {
    let value: Value = "0B1010".parse().unwrap();
    assert_eq!(value, Value::Number(Number::U8(10)));
}

#[test]
fn edge_octal() {
    let value: Value = "0o377".parse().unwrap();
    assert_eq!(value, Value::Number(Number::U8(255)));
}

#[test]
fn edge_octal_uppercase() {
    let value: Value = "0O77".parse().unwrap();
    assert_eq!(value, Value::Number(Number::U8(63)));
}

#[test]
fn edge_number_with_underscores() {
    let value: Value = "1_000_000".parse().unwrap();
    assert_eq!(value, Value::Number(Number::U32(1_000_000)));
}

#[test]
fn edge_hex_with_underscores() {
    let value: Value = "0xFF_FF_FF_FF".parse().unwrap();
    assert_eq!(value, Value::Number(Number::U32(0xFFFF_FFFF)));
}

// =============================================================================
// Float Edge Cases
// =============================================================================

#[test]
fn edge_float_zero() {
    let value: Value = "0.0".parse().unwrap();
    match value {
        Value::Number(Number::F32(f)) if f.get() == 0.0 => {}
        Value::Number(Number::F64(f)) if f.get() == 0.0 => {}
        _ => panic!("Expected 0.0, got {value:?}"),
    }
}

#[test]
fn edge_float_negative_zero() {
    let value: Value = "-0.0".parse().unwrap();
    match value {
        Value::Number(Number::F32(f)) if f.get() == 0.0 => {}
        Value::Number(Number::F64(f)) if f.get() == 0.0 => {}
        _ => panic!("Expected -0.0, got {value:?}"),
    }
}

#[test]
fn edge_float_very_small() {
    let value: Value = "1e-100".parse().unwrap();
    match value {
        Value::Number(Number::F32(_) | Number::F64(_)) => {}
        _ => panic!("Expected float, got {value:?}"),
    }
}

#[test]
fn edge_float_very_large() {
    let value: Value = "1e100".parse().unwrap();
    match value {
        Value::Number(Number::F32(_) | Number::F64(_)) => {}
        _ => panic!("Expected float, got {value:?}"),
    }
}

// =============================================================================
// Deeply Nested Structures
// =============================================================================

#[test]
fn edge_deeply_nested_seq() {
    let mut input = String::from("1");
    for _ in 0..50 {
        input = format!("[{input}]");
    }
    let value: Value = input.parse().unwrap();
    // Just verify it parses without stack overflow
    assert!(matches!(value, Value::Seq(_)));
}

#[test]
fn edge_deeply_nested_named() {
    let value: Value = "A(B(C(D(E(1)))))".parse().unwrap();
    assert!(matches!(value, Value::Named { .. }));
}

#[test]
fn edge_wide_seq() {
    let elements: Vec<_> = (0..100).map(|i| i.to_string()).collect();
    let input = format!("[{}]", elements.join(", "));
    let value: Value = input.parse().unwrap();
    match value {
        Value::Seq(seq) => assert_eq!(seq.len(), 100),
        _ => panic!("Expected seq"),
    }
}

// =============================================================================
// Map Key Types
// =============================================================================

#[test]
fn edge_map_bool_keys() {
    let value: Value = "{ true: 1, false: 0 }".parse().unwrap();
    let mut expected = Map::new();
    expected.insert(Value::Bool(true), Value::Number(Number::U8(1)));
    expected.insert(Value::Bool(false), Value::Number(Number::U8(0)));
    assert_eq!(value, Value::Map(expected));
}

#[test]
fn edge_map_char_keys() {
    let value: Value = "{ 'a': 1, 'b': 2 }".parse().unwrap();
    let mut expected = Map::new();
    expected.insert(Value::Char('a'), Value::Number(Number::U8(1)));
    expected.insert(Value::Char('b'), Value::Number(Number::U8(2)));
    assert_eq!(value, Value::Map(expected));
}

#[test]
fn edge_map_tuple_keys() {
    let value: Value = "{ (1, 2): \"pair\" }".parse().unwrap();
    let mut expected = Map::new();
    expected.insert(
        Value::Tuple(vec![
            Value::Number(Number::U8(1)),
            Value::Number(Number::U8(2)),
        ]),
        Value::String(String::from("pair")),
    );
    assert_eq!(value, Value::Map(expected));
}

#[test]
fn edge_map_option_keys() {
    let value: Value = "{ None: 0, Some(1): 1 }".parse().unwrap();
    let mut expected = Map::new();
    expected.insert(Value::Option(None), Value::Number(Number::U8(0)));
    expected.insert(
        Value::Option(Some(Box::new(Value::Number(Number::U8(1))))),
        Value::Number(Number::U8(1)),
    );
    assert_eq!(value, Value::Map(expected));
}

// =============================================================================
// Serialization Edge Cases
// =============================================================================

#[test]
fn edge_serialize_special_chars_in_string() {
    let value = Value::String(String::from("tab:\there\nnewline"));
    let serialized = value.to_ron_with(&FormatConfig::minimal()).unwrap();
    assert_eq!(serialized, r#""tab:\there\nnewline""#);
}

#[test]
fn edge_serialize_bytes_non_printable() {
    let value = Value::Bytes(vec![0, 1, 2, 127, 128, 255]);
    let serialized = value.to_ron_with(&FormatConfig::minimal()).unwrap();
    // Should use hex escapes for non-printable bytes
    assert!(serialized.starts_with("b\""));
    assert!(serialized.contains("\\x"));
}

#[test]
fn edge_serialize_float_whole_number() {
    // Float that looks like an integer should still have decimal point
    let value = Value::Number(Number::F64(ron2::value::F64::from(42.0)));
    let serialized = value.to_ron_with(&FormatConfig::minimal()).unwrap();
    assert!(
        serialized.contains('.'),
        "Float should have decimal point: {serialized}"
    );
}
