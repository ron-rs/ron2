//! Error handling tests.
//!
//! These tests verify that invalid RON input produces appropriate errors.

use ron2::{
    Value,
    error::{ErrorKind, Position},
};

// =============================================================================
// Syntax Errors
// =============================================================================

#[test]
fn error_unclosed_brace() {
    let result: Result<Value, _> = "{ \"a\": 1".parse();
    assert_eq!(
        result.unwrap_err().kind(),
        &ErrorKind::Unclosed {
            open: "{".into(),
            close: '}',
            opened_at: Position { line: 1, col: 1 },
        }
    );
}

#[test]
fn error_unclosed_paren() {
    let result: Result<Value, _> = "(1, 2".parse();
    assert_eq!(
        result.unwrap_err().kind(),
        &ErrorKind::Unclosed {
            open: "(".into(),
            close: ')',
            opened_at: Position { line: 1, col: 1 },
        }
    );
}

#[test]
fn error_unclosed_char() {
    let result: Result<Value, _> = "'a".parse();
    assert_eq!(
        result.unwrap_err().kind(),
        &ErrorKind::Expected {
            expected: "closing `'`".into(),
            context: Some("character literal"),
        }
    );
}

#[test]
fn error_extra_closing_bracket() {
    let result: Result<Value, _> = "[1, 2]]".parse();
    assert_eq!(result.unwrap_err().kind(), &ErrorKind::TrailingCharacters);
}

#[test]
fn error_extra_closing_brace() {
    let result: Result<Value, _> = "{ \"a\": 1 }}".parse();
    assert_eq!(result.unwrap_err().kind(), &ErrorKind::TrailingCharacters);
}

#[test]
fn error_extra_closing_paren() {
    let result: Result<Value, _> = "(1, 2))".parse();
    assert_eq!(result.unwrap_err().kind(), &ErrorKind::TrailingCharacters);
}

#[test]
fn error_mismatched_brackets() {
    let result: Result<Value, _> = "[1, 2, 3}".parse();
    assert_eq!(result.unwrap_err().kind(), &ErrorKind::UnexpectedChar('}'));
}

#[test]
fn error_mismatched_braces() {
    let result: Result<Value, _> = "{ \"a\": 1 ]".parse();
    assert_eq!(result.unwrap_err().kind(), &ErrorKind::UnexpectedChar(']'));
}

#[test]
fn error_mismatched_parens() {
    let result: Result<Value, _> = "(1, 2]".parse();
    assert_eq!(result.unwrap_err().kind(), &ErrorKind::UnexpectedChar(']'));
}

// =============================================================================
// Invalid Values
// =============================================================================

#[test]
fn error_invalid_number() {
    let result: Result<Value, _> = "12abc".parse();
    assert_eq!(result.unwrap_err().kind(), &ErrorKind::TrailingCharacters);
}

#[test]
fn error_invalid_binary() {
    let result: Result<Value, _> = "0b123".parse();
    assert_eq!(
        result.unwrap_err().kind(),
        &ErrorKind::InvalidIntegerDigit {
            digit: '2',
            base: 2
        }
    );
}

#[test]
fn error_invalid_octal() {
    let result: Result<Value, _> = "0o999".parse();
    assert_eq!(
        result.unwrap_err().kind(),
        &ErrorKind::InvalidIntegerDigit {
            digit: '9',
            base: 8
        }
    );
}

#[test]
fn error_empty_char() {
    let result: Result<Value, _> = "''".parse();
    assert_eq!(
        result.unwrap_err().kind(),
        &ErrorKind::Expected {
            expected: "a character".into(),
            context: Some("character literal"),
        }
    );
}

#[test]
fn error_multi_char() {
    let result: Result<Value, _> = "'ab'".parse();
    assert_eq!(
        result.unwrap_err().kind(),
        &ErrorKind::Expected {
            expected: "closing `'`".into(),
            context: Some("character literal"),
        }
    );
}

#[test]
fn error_invalid_unicode_escape() {
    let result: Result<Value, _> = "\"\\u{GGGGGG}\"".parse();
    assert_eq!(
        result.unwrap_err().kind(),
        &ErrorKind::InvalidEscape("expected 1 to 6 hex digits in unicode escape".into())
    );
}

#[test]
fn error_unicode_out_of_range() {
    // U+110000 is beyond the valid Unicode range
    let result: Result<Value, _> = "\"\\u{110000}\"".parse();
    assert_eq!(
        result.unwrap_err().kind(),
        &ErrorKind::InvalidEscape("invalid unicode code point".into())
    );
}

// =============================================================================
// Map Errors
// =============================================================================

#[test]
fn error_map_missing_colon() {
    let result: Result<Value, _> = "{ \"a\" 1 }".parse();
    assert_eq!(
        result.unwrap_err().kind(),
        &ErrorKind::Expected {
            expected: "`:` after map key".into(),
            context: Some("map entry"),
        }
    );
}

#[test]
fn error_map_missing_value() {
    let result: Result<Value, _> = "{ \"a\": }".parse();
    assert_eq!(
        result.unwrap_err().kind(),
        &ErrorKind::Expected {
            expected: "value".into(),
            context: Some("map entry"),
        }
    );
}

#[test]
fn error_map_double_comma() {
    let result: Result<Value, _> = "{ \"a\": 1,, \"b\": 2 }".parse();
    assert_eq!(result.unwrap_err().kind(), &ErrorKind::UnexpectedChar(','));
}

// =============================================================================
// Sequence Errors
// =============================================================================

#[test]
fn error_seq_double_comma() {
    let result: Result<Value, _> = "[1,, 2]".parse();
    assert_eq!(result.unwrap_err().kind(), &ErrorKind::UnexpectedChar(','));
}

#[test]
fn error_seq_missing_value() {
    let result: Result<Value, _> = "[1, , 3]".parse();
    assert_eq!(result.unwrap_err().kind(), &ErrorKind::UnexpectedChar(','));
}

// =============================================================================
// Named Type Errors
// =============================================================================

#[test]
fn error_named_struct_missing_colon() {
    let result: Result<Value, _> = "Point(x 1, y: 2)".parse();
    assert_eq!(
        result.unwrap_err().to_string(),
        "1:7-1:8: Expected `:` after field name `x`"
    );
}

#[test]
fn error_named_struct_missing_value() {
    let result: Result<Value, _> = "Point(x: , y: 2)".parse();
    assert_eq!(
        result.unwrap_err().kind(),
        &ErrorKind::Expected {
            expected: "value".into(),
            context: Some("struct field"),
        }
    );
}

#[test]
fn error_named_double_colon_at_end() {
    let result: Result<Value, _> = "Type::".parse();
    assert_eq!(result.unwrap_err().kind(), &ErrorKind::TrailingCharacters);
}

#[test]
fn error_named_triple_colon() {
    let result: Result<Value, _> = "Type:::Variant".parse();
    assert_eq!(result.unwrap_err().kind(), &ErrorKind::TrailingCharacters);
}

// =============================================================================
// Trailing Characters
// =============================================================================

#[test]
fn error_multiple_values() {
    let result: Result<Value, _> = "42 43".parse();
    assert_eq!(result.unwrap_err().kind(), &ErrorKind::TrailingCharacters);
}

// =============================================================================
// Raw String Errors
// =============================================================================

#[test]
fn error_raw_string_unclosed() {
    let result: Result<Value, _> = r##"r#"unclosed"##.parse();
    assert_eq!(result.unwrap_err().kind(), &ErrorKind::ExpectedStringEnd);
}

#[test]
fn error_raw_string_wrong_hashes() {
    let result: Result<Value, _> = r###"r#"text"##"###.parse();
    assert_eq!(result.unwrap_err().kind(), &ErrorKind::TrailingCharacters);
}

// =============================================================================
// Byte Errors
// =============================================================================

#[test]
fn error_byte_string_unclosed() {
    let result: Result<Value, _> = r#"b"unclosed"#.parse();
    assert_eq!(result.unwrap_err().kind(), &ErrorKind::ExpectedStringEnd);
}

#[test]
fn error_byte_invalid_escape() {
    let result: Result<Value, _> = r#"b"\q""#.parse();
    assert_eq!(
        result.unwrap_err().kind(),
        &ErrorKind::InvalidEscape("unknown escape sequence".into())
    );
}

// =============================================================================
// Error Messages Have Useful Info
// =============================================================================

#[test]
fn error_has_position_info() {
    let result: Result<Value, _> = "[1, 2, ".parse();
    let err = result.unwrap_err();
    // Error should have position information
    let msg = format!("{err}");
    // Should contain line:col format
    assert_eq!(msg, "1:8: Unexpected end of RON");
}

#[test]
fn error_multiline_position() {
    let result: Result<Value, _> = r#"[
    1,
    2,

"#
    .parse();
    let err = result.unwrap_err();
    let msg = format!("{err}");
    // Should show position beyond line 1
    assert_eq!(msg, "5:1: Unexpected end of RON");
}

// =============================================================================
// Messages for common mistakes
// =============================================================================

fn message(source: &str) -> String {
    source.parse::<Value>().unwrap_err().to_string()
}

#[test]
fn missing_colon_after_first_field_name() {
    assert_eq!(
        message(r#"Field(name "x")"#),
        "1:7-1:11: Expected `:` after field name `name`"
    );
    assert_eq!(
        message("Definitions(classes ["),
        "1:13-1:20: Expected `:` after field name `classes`"
    );
    // Later fields of a struct name the field too.
    assert_eq!(
        message("Point(x: 1, y 2)"),
        "1:13-1:14: Expected `:` after field name `y`"
    );
    // After a value that is not a bare identifier, a comma is missing.
    assert_eq!(message("Pair(1 2)"), "1:8-1:9: Expected comma in tuple");
    assert_eq!(message("(a, b c)"), "1:7-1:8: Expected comma in tuple");
}

#[test]
fn unclosed_collection_names_its_opener() {
    assert_eq!(
        message("Definitions("),
        "1:13: Expected closing `)` for `Definitions(` opened at 1:12"
    );
    assert_eq!(
        message("Definitions(\n    classes: [\n        Class(name: \"a\")\n"),
        "4:1: Expected closing `]` for `[` opened at 2:14"
    );
    assert_eq!(
        message("Some(1"),
        "1:7: Expected closing `)` for `Some(` opened at 1:5"
    );
    assert_eq!(
        message("{\"a\": 1"),
        "1:8: Expected closing `}` for `{` opened at 1:1"
    );
}
