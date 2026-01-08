//! Error handling tests.
//!
//! These tests verify that invalid RON input produces appropriate errors.

use ron2::from_str;

// =============================================================================
// Syntax Errors
// =============================================================================

#[test]
fn error_unclosed_bracket() {
    let result = from_str("[1, 2, 3");
    assert!(result.is_err());
}

#[test]
fn error_unclosed_brace() {
    let result = from_str("{ \"a\": 1");
    assert!(result.is_err());
}

#[test]
fn error_unclosed_paren() {
    let result = from_str("(1, 2");
    assert!(result.is_err());
}

#[test]
fn error_unclosed_string() {
    let result = from_str("\"hello");
    assert!(result.is_err());
}

#[test]
fn error_unclosed_char() {
    let result = from_str("'a");
    assert!(result.is_err());
}

#[test]
fn error_unclosed_block_comment() {
    let result = from_str("/* unclosed");
    assert!(result.is_err());
}

#[test]
fn error_extra_closing_bracket() {
    let result = from_str("[1, 2]]");
    assert!(result.is_err());
}

#[test]
fn error_extra_closing_brace() {
    let result = from_str("{ \"a\": 1 }}");
    assert!(result.is_err());
}

#[test]
fn error_extra_closing_paren() {
    let result = from_str("(1, 2))");
    assert!(result.is_err());
}

#[test]
fn error_mismatched_brackets() {
    let result = from_str("[1, 2, 3}");
    assert!(result.is_err());
}

#[test]
fn error_mismatched_braces() {
    let result = from_str("{ \"a\": 1 ]");
    assert!(result.is_err());
}

#[test]
fn error_mismatched_parens() {
    let result = from_str("(1, 2]");
    assert!(result.is_err());
}

// =============================================================================
// Invalid Values
// =============================================================================

#[test]
fn error_invalid_number() {
    let result = from_str("12abc");
    assert!(result.is_err());
}

#[test]
fn error_invalid_hex() {
    let result = from_str("0xGGG");
    assert!(result.is_err());
}

#[test]
fn error_invalid_binary() {
    let result = from_str("0b123");
    assert!(result.is_err());
}

#[test]
fn error_invalid_octal() {
    let result = from_str("0o999");
    assert!(result.is_err());
}

#[test]
fn error_empty_char() {
    let result = from_str("''");
    assert!(result.is_err());
}

#[test]
fn error_multi_char() {
    let result = from_str("'ab'");
    assert!(result.is_err());
}

#[test]
fn error_invalid_escape() {
    let result = from_str("\"\\q\"");
    assert!(result.is_err());
}

#[test]
fn error_invalid_unicode_escape() {
    let result = from_str("\"\\u{GGGGGG}\"");
    assert!(result.is_err());
}

#[test]
fn error_unicode_out_of_range() {
    // U+110000 is beyond the valid Unicode range
    let result = from_str("\"\\u{110000}\"");
    assert!(result.is_err());
}

// =============================================================================
// Map Errors
// =============================================================================

#[test]
fn error_map_missing_colon() {
    let result = from_str("{ \"a\" 1 }");
    assert!(result.is_err());
}

#[test]
fn error_map_missing_value() {
    let result = from_str("{ \"a\": }");
    assert!(result.is_err());
}

#[test]
fn error_map_double_comma() {
    let result = from_str("{ \"a\": 1,, \"b\": 2 }");
    assert!(result.is_err());
}

// =============================================================================
// Sequence Errors
// =============================================================================

#[test]
fn error_seq_double_comma() {
    let result = from_str("[1,, 2]");
    assert!(result.is_err());
}

#[test]
fn error_seq_missing_value() {
    let result = from_str("[1, , 3]");
    assert!(result.is_err());
}

// =============================================================================
// Named Type Errors
// =============================================================================

#[test]
fn error_named_struct_missing_colon() {
    let result = from_str("Point(x 1, y: 2)");
    assert!(result.is_err());
}

#[test]
fn error_named_struct_missing_value() {
    let result = from_str("Point(x: , y: 2)");
    assert!(result.is_err());
}

#[test]
fn error_named_double_colon_at_end() {
    let result = from_str("Type::");
    assert!(result.is_err());
}

#[test]
fn error_named_triple_colon() {
    let result = from_str("Type:::Variant");
    assert!(result.is_err());
}

// =============================================================================
// Trailing Characters
// =============================================================================

#[test]
fn error_trailing_junk() {
    let result = from_str("42 junk");
    assert!(result.is_err());
}

#[test]
fn error_multiple_values() {
    let result = from_str("42 43");
    assert!(result.is_err());
}

// =============================================================================
// Raw String Errors
// =============================================================================

#[test]
fn error_raw_string_unclosed() {
    let result = from_str(r##"r#"unclosed"##);
    assert!(result.is_err());
}

#[test]
fn error_raw_string_wrong_hashes() {
    let result = from_str(r###"r#"text"##"###);
    assert!(result.is_err());
}

// =============================================================================
// Byte Errors
// =============================================================================

#[test]
fn error_byte_string_unclosed() {
    let result = from_str(r#"b"unclosed"#);
    assert!(result.is_err());
}

#[test]
fn error_byte_invalid_escape() {
    let result = from_str(r#"b"\q""#);
    assert!(result.is_err());
}

// =============================================================================
// Error Messages Have Useful Info
// =============================================================================

#[test]
fn error_has_position_info() {
    let result = from_str("[1, 2, ");
    let err = result.unwrap_err();
    // Error should have position information
    let msg = format!("{err}");
    // Should contain line:col format
    assert!(msg.contains(':'), "Error should have position: {msg}");
}

#[test]
fn error_multiline_position() {
    let result = from_str(
        r#"[
    1,
    2,

"#,
    );
    let err = result.unwrap_err();
    let msg = format!("{err}");
    // Should show position beyond line 1
    assert!(msg.contains(':'), "Error should have position: {msg}");
}
