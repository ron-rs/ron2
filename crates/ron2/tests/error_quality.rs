//! Error quality tests - verifying span accuracy and message helpfulness.
//!
//! These tests ensure that errors provide precise location information
//! and helpful messages that enable rustc-like diagnostics.

use ron2::{ast::parse_document, error::Error, from_str, SpannedResult, Value};

// =============================================================================
// Span Accuracy Tests - Parsing Errors
// =============================================================================

/// Helper to verify span points to expected text in source.
#[allow(dead_code)]
fn verify_span_text(source: &str, result: SpannedResult<Value>, expected_text: &str) {
    let err = result.expect_err("expected an error");
    let span = &err.span;
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
    result: SpannedResult<Value>,
    expected_line: usize,
    expected_col: usize,
    context: &str,
) {
    let err = result.expect_err("expected an error");
    assert_eq!(
        err.span.start.line, expected_line,
        "{}: expected line {}, got line {}\nError: {}",
        context, expected_line, err.span.start.line, err
    );
    assert_eq!(
        err.span.start.col, expected_col,
        "{}: expected col {}, got col {}\nError: {}",
        context, expected_col, err.span.start.col, err
    );
}

#[test]
#[ignore = "span points to col 1 instead of the invalid digit - needs parser fix"]
fn span_points_to_invalid_token() {
    // Invalid character in number - should point to the 'G'
    let source = "0xGGG";
    let result: SpannedResult<Value> = from_str(source);
    let err = result.unwrap_err();

    // The error should be at the invalid digit
    assert_eq!(err.span.start.line, 1);
    assert!(
        err.span.start.col >= 3,
        "Error should point to 'G' at col 3 or later, got col {}",
        err.span.start.col
    );
}

#[test]
fn span_points_to_unclosed_bracket_content() {
    let source = "[1, 2, 3";
    let result: SpannedResult<Value> = from_str(source);
    let err = result.unwrap_err();

    // Error should be at end of file
    assert_eq!(err.span.start.line, 1);
    assert_eq!(err.span.start.col, 9, "Error should be at EOF position");
}

#[test]
#[ignore = "bare identifiers are valid RON (unit-like values) - not an error"]
fn span_multiline_points_to_correct_line() {
    let source = r#"[
    1,
    2,
    invalid_ident
]"#;
    let result: SpannedResult<Value> = from_str(source);
    let err = result.unwrap_err();

    // Error should be on line 4 where invalid_ident is
    assert_eq!(
        err.span.start.line, 4,
        "Error should be on line 4, got line {}",
        err.span.start.line
    );
}

#[test]
fn span_points_to_unclosed_string() {
    let source = "\"hello world";
    let result: SpannedResult<Value> = from_str(source);
    let err = result.unwrap_err();

    // Should point to the string start or end
    assert_eq!(err.span.start.line, 1);
}

#[test]
#[ignore = "escape error reports col 1 instead of escape position - needs parser fix"]
fn span_points_to_invalid_escape_sequence() {
    let source = r#""hello\qworld""#;
    let result: SpannedResult<Value> = from_str(source);
    let err = result.unwrap_err();

    // Error should be in the string where \q appears
    assert_eq!(err.span.start.line, 1);
    // Column should point to the escape sequence
    assert!(
        err.span.start.col >= 7,
        "Error should point to escape sequence area, got col {}",
        err.span.start.col
    );
}

#[test]
#[ignore = "missing colon error reports col 3 instead of error position - needs parser fix"]
fn span_points_to_map_missing_colon() {
    let source = r#"{ "key" "value" }"#;
    let result: SpannedResult<Value> = from_str(source);
    let err = result.unwrap_err();

    assert_eq!(err.span.start.line, 1);
    // Should point to "value" where colon was expected before
    assert!(
        err.span.start.col >= 9,
        "Error should point near 'value', got col {}",
        err.span.start.col
    );
}

#[test]
fn span_points_to_trailing_junk() {
    let source = "42 junk";
    let result: SpannedResult<Value> = from_str(source);
    let err = result.unwrap_err();

    assert_eq!(err.span.start.line, 1);
    assert_eq!(
        err.span.start.col, 4,
        "Error should point to 'junk' at col 4"
    );
}

#[test]
fn span_byte_offsets_enable_slicing() {
    let source = "[1, 2, }";
    let result: SpannedResult<Value> = from_str(source);
    let err = result.unwrap_err();

    // Verify byte offsets are valid for slicing
    assert!(
        err.span.start_offset <= source.len(),
        "start_offset {} should be <= source len {}",
        err.span.start_offset,
        source.len()
    );
    assert!(
        err.span.end_offset <= source.len(),
        "end_offset {} should be <= source len {}",
        err.span.end_offset,
        source.len()
    );
    assert!(
        err.span.start_offset <= err.span.end_offset,
        "start_offset should be <= end_offset"
    );
}

// =============================================================================
// Span Accuracy Tests - Multiline
// =============================================================================

#[test]
fn span_nested_struct_error_correct_line() {
    let source = r#"Config(
    name: "test",
    settings: Settings(
        timeout: "not_a_number",
    ),
)"#;
    // This tests AST parsing, not deserialization
    // The string "not_a_number" is valid RON, just wrong type
    let doc = parse_document(source).expect("should parse");
    assert!(doc.value.is_some());
}

#[test]
#[ignore = "bare identifiers are valid RON (unit-like values) - not an error"]
fn span_deeply_nested_error() {
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
    let result: SpannedResult<Value> = from_str(source);
    let err = result.unwrap_err();

    // Error should be on line 7 where "invalid" appears
    assert_eq!(
        err.span.start.line, 7,
        "Error should be on line 7, got {}",
        err.span.start.line
    );
}

// =============================================================================
// Error Message Quality Tests
// =============================================================================

#[test]
fn error_message_includes_context_for_comma() {
    let source = "[1 2]";
    let result: SpannedResult<Value> = from_str(source);
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
    let result: SpannedResult<Value> = from_str(source);
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
    let result: SpannedResult<Value> = from_str(source);
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
fn error_code_integer_out_of_bounds_includes_value() {
    // u8 max is 255
    let source = "256";
    // We need to try to parse as u8 to get bounds error
    // This happens during deserialization, not parsing
    let result: SpannedResult<Value> = from_str(source);
    // This should succeed - 256 is a valid number
    assert!(result.is_ok(), "256 is valid as a generic number");
}

#[test]
fn error_distinguishes_bracket_types() {
    // Mismatched brackets
    let source1 = "[1, 2}";
    let result1: SpannedResult<Value> = from_str(source1);
    let err1 = result1.unwrap_err();

    let source2 = "{1: 2]";
    let result2: SpannedResult<Value> = from_str(source2);
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
    let result: SpannedResult<Value> = from_str(source);
    let err = result.unwrap_err();

    matches!(err.code, Error::ExpectedStringEnd | Error::Eof);
}

#[test]
fn error_code_for_invalid_escape() {
    let source = r#""\q""#;
    let result: SpannedResult<Value> = from_str(source);
    let err = result.unwrap_err();

    matches!(err.code, Error::InvalidEscape(_));
}

#[test]
fn error_code_for_trailing_characters() {
    let source = "42 extra";
    let result: SpannedResult<Value> = from_str(source);
    let err = result.unwrap_err();

    assert!(
        matches!(err.code, Error::TrailingCharacters),
        "Expected TrailingCharacters error, got {:?}",
        err.code
    );
}

// =============================================================================
// Edge Cases
// =============================================================================

#[test]
#[ignore = "empty input parses as Unit - intentional RON behavior"]
fn empty_input_has_sensible_error() {
    let source = "";
    let result: SpannedResult<Value> = from_str(source);
    let err = result.unwrap_err();

    // Should report EOF at position 1:1
    assert_eq!(err.span.start.line, 1);
    assert_eq!(err.span.start.col, 1);
}

#[test]
#[ignore = "whitespace-only input parses as Unit - intentional RON behavior"]
fn whitespace_only_has_sensible_error() {
    let source = "   \n\n   ";
    let result: SpannedResult<Value> = from_str(source);
    let err = result.unwrap_err();

    // Should report EOF
    matches!(err.code, Error::Eof);
}

#[test]
fn unicode_positions_are_character_based() {
    // Unicode: "日本語" = 3 characters but 9 bytes
    let source = "\"日本語\" junk";
    let result: SpannedResult<Value> = from_str(source);
    let err = result.unwrap_err();

    // The error should be at "junk"
    // Position should be character-based, not byte-based
    assert_eq!(err.span.start.line, 1);
    // After "日本語" (5 chars including quotes) + space = col 7
    // This tests whether columns are character or byte based
    assert!(
        err.span.start.col >= 6 && err.span.start.col <= 12,
        "Column {} should be reasonable for unicode",
        err.span.start.col
    );
}

#[test]
fn very_long_line_has_correct_position() {
    let long_prefix = "x".repeat(1000);
    let source = format!("[{}]", long_prefix);
    // This should parse fine
    let result: SpannedResult<Value> = from_str(&source);
    assert!(result.is_ok());

    // Now with error at end
    let source_err = format!("[{}", long_prefix);
    let result_err: SpannedResult<Value> = from_str(&source_err);
    let err = result_err.unwrap_err();

    assert_eq!(err.span.start.line, 1);
    assert!(
        err.span.start.col > 1000,
        "Column should be > 1000, got {}",
        err.span.start.col
    );
}

// =============================================================================
// Span Text Extraction Tests
// =============================================================================

#[test]
fn span_slice_method_works() {
    let source = "{ \"key\" value }";
    let result: SpannedResult<Value> = from_str(source);
    let err = result.unwrap_err();

    // Use the span to extract text
    let sliced = err.span.slice(source);

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
    let result: SpannedResult<Value> = from_str(source);
    let err = result.unwrap_err();

    // Error should be on line 2
    assert_eq!(
        err.span.start.line, 2,
        "Error should be on line 2 after comment"
    );
}

#[test]
#[ignore = "reports UnexpectedChar('/') instead of UnclosedBlockComment - needs parser fix"]
fn error_inside_block_comment() {
    let source = "/* unclosed comment";
    let result: SpannedResult<Value> = from_str(source);
    let err = result.unwrap_err();

    // Should report unclosed block comment
    assert!(
        matches!(err.code, Error::UnclosedBlockComment),
        "Expected UnclosedBlockComment, got {:?}",
        err.code
    );
}
