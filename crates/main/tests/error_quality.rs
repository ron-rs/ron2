//! Error quality tests - verifying span accuracy and message helpfulness.
//!
//! These tests ensure that errors provide precise location information
//! and helpful messages that enable rustc-like diagnostics.

#![cfg(feature = "derive")]

use ron2::{Result, Value, error::ErrorKind};

// =============================================================================
// Span Accuracy Tests - Parsing Errors
// =============================================================================

/// Helper to verify span points to expected text in source.
#[allow(dead_code)]
fn verify_span_text(source: &str, result: Result<Value>, expected_text: &str) {
    let err = result.expect_err("expected an error");
    let span = err.span();
    let actual_text = &source[span.start_offset..span.end_offset];
    assert_eq!(
        actual_text, expected_text,
        "Span should point to '{}' but points to '{}'\nError: {}\nSpan: {:?}",
        expected_text, actual_text, err, span
    );
}

/// Helper to verify span starts at expected line and column.
#[allow(dead_code)]
fn verify_span_position(
    result: Result<Value>,
    expected_line: usize,
    expected_col: usize,
    context: &str,
) {
    let err = result.expect_err("expected an error");
    assert_eq!(
        err.span().start.line,
        expected_line,
        "{}: expected line {}, got line {}\nError: {}",
        context,
        expected_line,
        err.span().start.line,
        err
    );
    assert_eq!(
        err.span().start.col,
        expected_col,
        "{}: expected col {}, got col {}\nError: {}",
        context,
        expected_col,
        err.span().start.col,
        err
    );
}

#[test]
fn span_points_to_invalid_token() {
    // Invalid character in number - should point to the 'G'
    let source = "0xGGG";
    let result: Result<Value> = source.parse();
    let err = result.unwrap_err();

    // The error should be at the first invalid digit
    assert_eq!(
        err.kind(),
        &ErrorKind::InvalidIntegerDigit {
            digit: 'G',
            base: 16
        }
    );
    assert_eq!(err.span().start.line, 1);
    assert_eq!(err.span().start.col, 3);
    assert_eq!(err.span().slice(source), "G");
}

#[test]
fn span_points_to_unclosed_bracket_content() {
    let source = "[1, 2, 3";
    let result: Result<Value> = source.parse();
    let err = result.unwrap_err();

    // Error should be at end of file
    assert_eq!(
        err.kind(),
        &ErrorKind::Expected {
            expected: "closing `]`".into(),
            context: Some("array")
        }
    );
    assert_eq!(err.span().start.line, 1);
    assert_eq!(err.span().start.col, 9, "Error should be at EOF position");
}

#[test]
fn span_multiline_points_to_correct_line() {
    use ron2::FromRon;

    // Bare identifiers are valid RON at the parsing level (they become unit-like values).
    // The error occurs during deserialization when we try to convert to Vec<i32>.
    let source = r#"[
    1,
    2,
    invalid_ident
]"#;

    // Parsing succeeds - bare identifiers are valid RON
    let result: Result<Value> = source.parse();
    assert!(result.is_ok(), "Parsing should succeed: {:?}", result);

    // Deserialization to Vec<i32> should fail with correct span
    let result = Vec::<i32>::from_ron(source);
    let err = result.expect_err("deserialization to Vec<i32> should fail");

    // Error should be on line 4 where invalid_ident is
    assert_eq!(
        err.span().start.line,
        4,
        "Error should be on line 4, got line {}",
        err.span().start.line
    );
}

#[test]
fn span_points_to_unclosed_string() {
    let source = "\"hello world";
    let result: Result<Value> = source.parse();
    let err = result.unwrap_err();

    // Should cover the whole unterminated string
    assert_eq!(err.kind(), &ErrorKind::ExpectedStringEnd);
    assert_eq!(err.span().slice(source), source);
}

#[test]
fn span_points_to_invalid_escape_sequence() {
    let source = r#""hello\qworld""#;
    let result: Result<Value> = source.parse();
    let err = result.unwrap_err();

    // Error should be in the string where \q appears
    assert_eq!(err.span().start.line, 1);
    // Column should point to the escape sequence
    assert_eq!(err.span().start.col, 7);
    assert_eq!(err.span().slice(source), "\\q");
}

#[test]
fn span_points_to_map_missing_colon() {
    let source = r#"{ "key" "value" }"#;
    let result: Result<Value> = source.parse();
    let err = result.unwrap_err();

    assert_eq!(err.span().start.line, 1);
    // Should point to "value" where colon was expected before
    assert_eq!(err.span().start.col, 9);
    assert_eq!(err.span().slice(source), "\"value\"");
}

#[test]
fn span_points_to_trailing_junk() {
    let source = "42 junk";
    let result: Result<Value> = source.parse();
    let err = result.unwrap_err();

    assert_eq!(err.span().start.line, 1);
    assert_eq!(
        err.span().start.col,
        4,
        "Error should point to 'junk' at col 4"
    );
}

#[test]
fn span_byte_offsets_enable_slicing() {
    let source = "[1, 2, }";
    let result: Result<Value> = source.parse();
    let err = result.unwrap_err();

    // Verify byte offsets slice out the offending token
    assert_eq!(&source[err.span().start_offset..err.span().end_offset], "}");
}

// =============================================================================
// Span Accuracy Tests - Multiline
// =============================================================================

#[test]
fn span_deeply_nested_error() {
    use std::collections::HashMap;

    use ron2::FromRon;

    // Bare identifiers are valid RON at the parsing level.
    // The error occurs during deserialization when we try to convert to the target type.
    let source = r#"{
    "level1": {
        "level2": {
            "level3": [
                1,
                2,
                invalid
            ]
        }
    }
}"#;

    // Parsing succeeds - bare identifiers are valid RON
    let result: Result<Value> = source.parse();
    assert!(result.is_ok(), "Parsing should succeed: {:?}", result);

    // Deserialization should fail with correct span
    type NestedMap = HashMap<String, HashMap<String, HashMap<String, Vec<i32>>>>;
    let result = NestedMap::from_ron(source);
    let err = result.expect_err("deserialization should fail");

    // Error should be on line 7 where "invalid" appears
    assert_eq!(
        err.span().start.line,
        7,
        "Error should be on line 7, got {}",
        err.span().start.line
    );
}

// =============================================================================
// Error Message Quality Tests
// =============================================================================

#[test]
fn error_message_includes_context_for_comma() {
    let source = "[1 2]";
    let result: Result<Value> = source.parse();
    let err = result.unwrap_err();
    let msg = err.to_string();

    // Error should mention what was expected
    assert_eq!(msg, "1:4-1:5: Expected comma in array");
}

#[test]
fn error_message_shows_position() {
    let source = "[1, 2, }";
    let result: Result<Value> = source.parse();
    let err = result.unwrap_err();
    let msg = err.to_string();

    // Should show line:col format
    assert_eq!(msg, "1:8-1:9: Unexpected char '}'");
}

#[test]
fn error_message_eof_is_clear() {
    let source = "[1, 2,";
    let result: Result<Value> = source.parse();
    let err = result.unwrap_err();
    let msg = err.to_string();

    // Should mention EOF or end
    assert_eq!(msg, "1:7: Unexpected end of RON");
}

#[test]
fn type_bounds_checked_at_deserialization_not_parsing() {
    use ron2::{FromRon, Number};

    // Type bounds like u8 are checked during deserialization, not parsing
    // 256 parses as the smallest fitting number but is too big for u8
    let source = "256";
    let result: Result<Value> = source.parse();
    assert_eq!(result.unwrap(), Value::Number(Number::U16(256)));
    assert!(matches!(
        u8::from_ron(source).unwrap_err().kind(),
        ErrorKind::IntegerOutOfBounds { .. }
    ));

    // i64::MAX is valid
    let source = "9223372036854775807";
    let result: Result<Value> = source.parse();
    assert_eq!(
        result.unwrap(),
        Value::Number(Number::U64(9_223_372_036_854_775_807))
    );
}

#[test]
fn error_distinguishes_bracket_types() {
    // Mismatched brackets
    let source1 = "[1, 2}";
    let result1: Result<Value> = source1.parse();
    let err1 = result1.unwrap_err();

    let source2 = "{1: 2]";
    let result2: Result<Value> = source2.parse();
    let err2 = result2.unwrap_err();

    // Each error names the closer that does not match
    assert_eq!(err1.to_string(), "1:6-1:7: Unexpected char '}'");
    assert_eq!(err2.to_string(), "1:6-1:7: Unexpected char ']'");
}

// =============================================================================
// Error Code Specificity Tests
// =============================================================================

#[test]
fn error_code_for_unclosed_string() {
    let source = "\"hello";
    let result: Result<Value> = source.parse();
    let err = result.unwrap_err();

    assert_eq!(err.kind(), &ErrorKind::ExpectedStringEnd);
}

#[test]
fn error_code_for_invalid_escape() {
    let source = r#""\q""#;
    let result: Result<Value> = source.parse();
    let err = result.unwrap_err();

    assert!(
        matches!(err.kind(), ErrorKind::InvalidEscape(_)),
        "Expected InvalidEscape error, got {:?}",
        err.kind()
    );
}

#[test]
fn error_code_for_trailing_characters() {
    let source = "42 extra";
    let result: Result<Value> = source.parse();
    let err = result.unwrap_err();

    assert!(
        matches!(err.kind(), ErrorKind::TrailingCharacters),
        "Expected TrailingCharacters error, got {:?}",
        err.kind()
    );
}

// =============================================================================
// Edge Cases
// =============================================================================

#[test]
fn empty_input_has_sensible_error() {
    let source = "";
    let result: Result<Value> = source.parse();
    let err = result.unwrap_err();

    // Should report EOF at position 1:1
    assert_eq!(err.span().start.line, 1);
    assert_eq!(err.span().start.col, 1);
    assert!(matches!(err.kind(), ErrorKind::Eof));
}

#[test]
fn whitespace_only_has_sensible_error() {
    let source = "   \n\n   ";
    let result: Result<Value> = source.parse();
    let err = result.unwrap_err();

    // Should report EOF
    assert!(matches!(err.kind(), ErrorKind::Eof));
}

#[test]
fn unicode_positions_are_byte_based() {
    // Unicode: "日本語" = 3 characters but 9 bytes
    // Columns are byte-based for performance (O(log n) lookup via LineIndex)
    let source = "\"日本語\" junk";
    let result: Result<Value> = source.parse();
    let err = result.unwrap_err();

    // The error should be at "junk"
    // String bytes: " (1) + 日 (3) + 本 (3) + 語 (3) + " (1) + space (1) = 12 bytes
    // So "junk" starts at byte offset 12, column 13 (1-indexed)
    assert_eq!(err.span().start.line, 1);
    assert_eq!(
        err.span().start.col,
        13,
        "Column {} should be byte-based (expected 13)",
        err.span().start.col
    );
}

#[test]
fn very_long_line_has_correct_position() {
    let long_prefix = "x".repeat(1000);
    let source = format!("[{}]", long_prefix);
    // This should parse fine
    let result: Result<Value> = source.parse();
    assert!(result.is_ok());

    // Now with error at end
    let source_err = format!("[{}", long_prefix);
    let result_err: Result<Value> = source_err.parse();
    let err = result_err.unwrap_err();

    assert_eq!(err.span().start.line, 1);
    // Byte offset 1001 (just past the 1000 `x`s) is column 1002
    assert_eq!(err.span().start.col, 1002);
}

// =============================================================================
// Span Text Extraction Tests
// =============================================================================

#[test]
fn span_slice_method_works() {
    let source = "{ \"key\" value }";
    let result: Result<Value> = source.parse();
    let err = result.unwrap_err();

    // Use the span to extract text
    let sliced = err.span().slice(source);

    // The sliced text is the token where `:` was expected
    assert_eq!(sliced, "value");
}

// =============================================================================
// Comment handling in errors
// =============================================================================

#[test]
fn error_after_comment_has_correct_position() {
    let source = r#"// This is a comment
[1, 2, }"#;
    let result: Result<Value> = source.parse();
    let err = result.unwrap_err();

    // Error should be on line 2
    assert_eq!(
        err.span().start.line,
        2,
        "Error should be on line 2 after comment"
    );
}

#[test]
fn error_inside_block_comment() {
    let source = "/* unclosed comment";
    let result: Result<Value> = source.parse();
    let err = result.unwrap_err();

    // Should report unclosed block comment
    assert!(
        matches!(err.kind(), ErrorKind::UnclosedBlockComment),
        "Expected UnclosedBlockComment, got {:?}",
        err.kind()
    );
}
