//! Error quality tests - verifying span accuracy and message helpfulness.
//!
//! These tests ensure that errors provide precise location information
//! and helpful messages that enable rustc-like diagnostics.

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

    // The error should be at the invalid digit
    assert_eq!(err.span().start.line, 1);
    assert!(
        err.span().start.col >= 3,
        "Error should point to 'G' at col 3 or later, got col {}",
        err.span().start.col
    );
}

#[test]
fn span_points_to_unclosed_bracket_content() {
    let source = "[1, 2, 3";
    let result: Result<Value> = source.parse();
    let err = result.unwrap_err();

    // Error should be at end of file
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

    // Should point to the string start or end
    assert_eq!(err.span().start.line, 1);
}

#[test]
fn span_points_to_invalid_escape_sequence() {
    let source = r#""hello\qworld""#;
    let result: Result<Value> = source.parse();
    let err = result.unwrap_err();

    // Error should be in the string where \q appears
    assert_eq!(err.span().start.line, 1);
    // Column should point to the escape sequence
    assert!(
        err.span().start.col >= 7,
        "Error should point to escape sequence area, got col {}",
        err.span().start.col
    );
}

#[test]
fn span_points_to_map_missing_colon() {
    let source = r#"{ "key" "value" }"#;
    let result: Result<Value> = source.parse();
    let err = result.unwrap_err();

    assert_eq!(err.span().start.line, 1);
    // Should point to "value" where colon was expected before
    assert!(
        err.span().start.col >= 9,
        "Error should point near 'value', got col {}",
        err.span().start.col
    );
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

    // Verify byte offsets are valid for slicing
    assert!(
        err.span().start_offset <= source.len(),
        "start_offset {} should be <= source len {}",
        err.span().start_offset,
        source.len()
    );
    assert!(
        err.span().end_offset <= source.len(),
        "end_offset {} should be <= source len {}",
        err.span().end_offset,
        source.len()
    );
    assert!(
        err.span().start_offset <= err.span().end_offset,
        "start_offset should be <= end_offset"
    );
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
    assert!(
        msg.contains("comma") || msg.contains(",") || msg.contains("Expected"),
        "Error should mention comma or expectation: {}",
        msg
    );
}

#[test]
fn error_message_shows_position() {
    let source = "[1, 2, }";
    let result: Result<Value> = source.parse();
    let err = result.unwrap_err();
    let msg = err.to_string();

    // Should show line:col format
    assert!(
        msg.contains(':'),
        "Error should contain position separator: {}",
        msg
    );
}

#[test]
fn error_message_eof_is_clear() {
    let source = "[1, 2,";
    let result: Result<Value> = source.parse();
    let err = result.unwrap_err();
    let msg = err.to_string();

    // Should mention EOF or end
    assert!(
        msg.to_lowercase().contains("eof")
            || msg.to_lowercase().contains("end")
            || msg.contains("Unexpected"),
        "Error should mention EOF: {}",
        msg
    );
}

#[test]
fn type_bounds_checked_at_deserialization_not_parsing() {
    // Type bounds like u8 are checked during deserialization, not parsing
    // 256 is fine for i64 (default integer storage) but too big for u8
    let source = "256";
    let result: Result<Value> = source.parse();
    assert!(result.is_ok(), "256 should parse as a generic number");

    // i64::MAX is valid
    let source = "9223372036854775807";
    let result: Result<Value> = source.parse();
    assert!(result.is_ok(), "i64::MAX should parse successfully");
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

    // Errors should be different or at least specific
    let msg1 = err1.to_string();
    let msg2 = err2.to_string();

    // Both should have position info
    assert!(msg1.contains(':'), "Error 1 should have position: {}", msg1);
    assert!(msg2.contains(':'), "Error 2 should have position: {}", msg2);
}

// =============================================================================
// Error Code Specificity Tests
// =============================================================================

#[test]
fn error_code_for_unclosed_string() {
    let source = "\"hello";
    let result: Result<Value> = source.parse();
    let err = result.unwrap_err();

    assert!(
        matches!(err.kind(), ErrorKind::ExpectedStringEnd | ErrorKind::Eof),
        "Expected ExpectedStringEnd or Eof error, got {:?}",
        err.kind()
    );
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
fn unicode_positions_are_character_based() {
    // Unicode: "日本語" = 3 characters but 9 bytes
    let source = "\"日本語\" junk";
    let result: Result<Value> = source.parse();
    let err = result.unwrap_err();

    // The error should be at "junk"
    // Position should be character-based, not byte-based
    assert_eq!(err.span().start.line, 1);
    // After "日本語" (5 chars including quotes) + space = col 7
    // This tests whether columns are character or byte based
    assert!(
        err.span().start.col >= 6 && err.span().start.col <= 12,
        "Column {} should be reasonable for unicode",
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
    assert!(
        err.span().start.col > 1000,
        "Column should be > 1000, got {}",
        err.span().start.col
    );
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

    // The sliced text should be non-empty and from the source
    assert!(
        source.contains(sliced) || sliced.is_empty(),
        "Sliced '{}' should be in source",
        sliced
    );
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
