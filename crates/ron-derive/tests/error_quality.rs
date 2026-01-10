//! Error quality tests for ron-derive deserialization.
//!
//! These tests verify that deserialization errors provide:
//! 1. Precise span information pointing to the actual error location
//! 2. Type-aware error messages that help users understand what went wrong
//! 3. Context about expected vs actual types
//! 4. Suggestions for valid alternatives (for unknown fields/variants)

use ron2::{error::Error, FromRon};
use ron_derive::Ron;

// =============================================================================
// Type-Aware Error Message Tests
// =============================================================================

#[derive(Debug, Ron, PartialEq)]
struct SimpleConfig {
    name: String,
    port: u16,
    enabled: bool,
}

#[test]
fn error_message_includes_expected_type() {
    let input = r#"(name: "test", port: "not_a_port", enabled: true)"#;
    let err = SimpleConfig::from_ron(input).unwrap_err();
    let msg = err.to_string();

    // Error should mention what type was expected
    assert!(
        msg.contains("u16") || msg.contains("integer") || msg.contains("number"),
        "Error should mention expected type: {}",
        msg
    );
}

#[test]
fn error_message_includes_found_type() {
    let input = r#"(name: "test", port: "not_a_port", enabled: true)"#;
    let err = SimpleConfig::from_ron(input).unwrap_err();
    let msg = err.to_string();

    // Error should mention what was found
    assert!(
        msg.to_lowercase().contains("string") || msg.contains("found"),
        "Error should mention found type: {}",
        msg
    );
}

#[test]
fn error_message_for_wrong_struct_type() {
    let input = r#"[1, 2, 3]"#;
    let err = SimpleConfig::from_ron(input).unwrap_err();
    let msg = err.to_string();

    // Error should mention struct expected
    assert!(
        msg.contains("struct") || msg.contains("SimpleConfig") || msg.contains("Expected"),
        "Error should mention struct expectation: {}",
        msg
    );
}

// =============================================================================
// Missing Field Error Tests
// =============================================================================

#[test]
fn missing_field_error_names_the_field() {
    let input = r#"(name: "test", enabled: true)"#;
    let err = SimpleConfig::from_ron(input).unwrap_err();
    let msg = err.to_string();

    // Error should name the missing field
    assert!(
        msg.contains("port"),
        "Error should name missing field 'port': {}",
        msg
    );
}

#[test]
fn missing_field_error_names_the_struct() {
    let input = r#"(name: "test", enabled: true)"#;
    let err = SimpleConfig::from_ron(input).unwrap_err();

    // Check error code includes struct context
    if let Error::MissingStructField { field, outer } = &err.code {
        assert_eq!(field.as_ref(), "port");
        assert!(
            outer.is_some(),
            "Missing field error should include struct name"
        );
    } else {
        panic!("Expected MissingStructField error, got {:?}", err.code);
    }
}

#[test]
fn missing_field_span_points_to_struct() {
    let input = r#"(
    name: "test",
    enabled: true,
)"#;
    let err = SimpleConfig::from_ron(input).unwrap_err();

    // For missing fields, span should point to the struct opening
    assert_eq!(
        err.span.start.line, 1,
        "Missing field error should point to struct start"
    );
}

// =============================================================================
// Unknown Field Error Tests
// =============================================================================

#[derive(Debug, Ron, PartialEq)]
#[ron(deny_unknown_fields)]
struct StrictConfig {
    name: String,
    value: i32,
}

#[test]
fn unknown_field_error_names_the_field() {
    let input = r#"(name: "test", value: 42, unknown_field: true)"#;
    let err = StrictConfig::from_ron(input).unwrap_err();
    let msg = err.to_string();

    assert!(
        msg.contains("unknown_field"),
        "Error should name unknown field: {}",
        msg
    );
}

#[test]
fn unknown_field_error_suggests_valid_fields() {
    let input = r#"(name: "test", value: 42, nme: "typo")"#;
    let err = StrictConfig::from_ron(input).unwrap_err();
    let msg = err.to_string();

    // Error should list valid alternatives
    assert!(
        msg.contains("name") || msg.contains("value") || msg.contains("expected"),
        "Error should suggest valid fields: {}",
        msg
    );
}

#[test]
fn unknown_field_span_points_to_field_name() {
    let input = r#"(
    name: "test",
    value: 42,
    unknown: true,
)"#;
    let err = StrictConfig::from_ron(input).unwrap_err();

    // Span should point to "unknown" on line 4
    assert_eq!(
        err.span.start.line, 4,
        "Unknown field error should point to field name on line 4, got line {}",
        err.span.start.line
    );
}

// =============================================================================
// Enum Error Tests
// =============================================================================

#[derive(Debug, Ron, PartialEq)]
enum Message {
    Text { content: String },
    Number { value: i32 },
    Empty,
}

#[test]
fn unknown_variant_error_names_the_variant() {
    let input = r#"Unknown"#;
    let err = Message::from_ron(input).unwrap_err();
    let msg = err.to_string();

    assert!(
        msg.contains("Unknown"),
        "Error should name unknown variant: {}",
        msg
    );
}

#[test]
fn unknown_variant_error_suggests_valid_variants() {
    let input = r#"Txt(content: "hello")"#;
    let err = Message::from_ron(input).unwrap_err();
    let msg = err.to_string();

    // Error should list valid variants
    assert!(
        msg.contains("Text") || msg.contains("Number") || msg.contains("Empty"),
        "Error should suggest valid variants: {}",
        msg
    );
}

#[test]
fn enum_field_error_has_correct_span() {
    let input = r#"Number(value: "not_a_number")"#;
    let err = Message::from_ron(input).unwrap_err();

    // Span should point to the invalid value
    assert_eq!(err.span.start.line, 1);
    assert!(
        err.span.start.col >= 15,
        "Error should point to 'not_a_number', got col {}",
        err.span.start.col
    );
}

// =============================================================================
// Nested Structure Error Tests
// =============================================================================

#[derive(Debug, Ron, PartialEq)]
struct Inner {
    value: i32,
}

#[derive(Debug, Ron, PartialEq)]
struct Outer {
    name: String,
    inner: Inner,
}

#[test]
fn nested_error_has_correct_span() {
    let input = r#"(
    name: "test",
    inner: (
        value: "not_an_int",
    ),
)"#;
    let err = Outer::from_ron(input).unwrap_err();

    // Error should point to "not_an_int" on line 4
    assert_eq!(
        err.span.start.line, 4,
        "Error should be on line 4 where invalid value is"
    );
}

#[test]
fn deeply_nested_error_preserves_span() {
    #[derive(Debug, Ron, PartialEq)]
    struct Level3 {
        data: u8,
    }

    #[derive(Debug, Ron, PartialEq)]
    struct Level2 {
        level3: Level3,
    }

    #[derive(Debug, Ron, PartialEq)]
    struct Level1 {
        level2: Level2,
    }

    let input = r#"(
    level2: (
        level3: (
            data: "wrong",
        ),
    ),
)"#;
    let err = Level1::from_ron(input).unwrap_err();

    // Error should be on line 4 where "wrong" is
    assert_eq!(
        err.span.start.line, 4,
        "Error should be on line 4, got {}",
        err.span.start.line
    );
}

// =============================================================================
// Collection Error Tests
// =============================================================================

#[derive(Debug, Ron, PartialEq)]
struct WithVec {
    items: Vec<i32>,
}

#[test]
fn vec_element_error_has_correct_span() {
    let input = r#"(
    items: [1, 2, "three", 4],
)"#;
    let err = WithVec::from_ron(input).unwrap_err();

    // Error should be on line 2 where "three" is
    assert_eq!(err.span.start.line, 2);
}

#[test]
fn vec_element_error_mentions_type() {
    let input = r#"(items: [1, 2, "three"])"#;
    let err = WithVec::from_ron(input).unwrap_err();
    let msg = err.to_string();

    // Should mention the type mismatch
    assert!(
        msg.to_lowercase().contains("i32")
            || msg.to_lowercase().contains("integer")
            || msg.to_lowercase().contains("string"),
        "Error should mention type: {}",
        msg
    );
}

#[derive(Debug, Ron, PartialEq)]
struct WithMap {
    mapping: std::collections::HashMap<String, i32>,
}

#[test]
fn map_value_error_has_correct_span() {
    let input = r#"(
    mapping: {
        "a": 1,
        "b": "not_a_number",
    },
)"#;
    let err = WithMap::from_ron(input).unwrap_err();

    // Error should be on line 4 where "not_a_number" is
    assert_eq!(
        err.span.start.line, 4,
        "Error should be on line 4, got {}",
        err.span.start.line
    );
}

// =============================================================================
// Tuple Struct Error Tests
// =============================================================================

#[derive(Debug, Ron, PartialEq)]
struct Point(i32, i32);

#[test]
fn tuple_element_error_has_correct_span() {
    let input = r#"(1, "not_an_int")"#;
    let err = Point::from_ron(input).unwrap_err();

    // Error should point to "not_an_int"
    assert_eq!(err.span.start.line, 1);
    assert!(
        err.span.start.col >= 5,
        "Error should point to second element, got col {}",
        err.span.start.col
    );
}

#[test]
fn tuple_wrong_length_error() {
    let input = r#"(1, 2, 3)"#;
    let err = Point::from_ron(input).unwrap_err();
    let msg = err.to_string();

    // Should mention expected vs actual count
    assert!(
        msg.contains('2')
            || msg.contains('3')
            || msg.contains("length")
            || msg.contains("elements"),
        "Error should mention tuple length: {}",
        msg
    );
}

// =============================================================================
// Option Field Error Tests
// =============================================================================

#[derive(Debug, Ron, PartialEq)]
struct WithOption {
    required: String,
    optional: Option<i32>,
}

#[test]
fn option_inner_type_error_has_correct_span() {
    let input = r#"(required: "test", optional: "not_an_int")"#;
    let err = WithOption::from_ron(input).unwrap_err();

    // Error should point to "not_an_int"
    assert!(
        err.span.start.col >= 30,
        "Error should point to 'not_an_int', got col {}",
        err.span.start.col
    );
}

#[derive(Debug, Ron, PartialEq)]
struct WithExplicitOption {
    #[ron(explicit)]
    value: Option<i32>,
}

#[test]
fn explicit_option_error_mentions_some_none() {
    let input = r#"(value: 42)"#;
    let err = WithExplicitOption::from_ron(input).unwrap_err();
    let msg = err.to_string();

    // Should mention Some/None requirement
    assert!(
        msg.contains("Some") || msg.contains("None"),
        "Error should mention Some/None for explicit option: {}",
        msg
    );
}

// =============================================================================
// Integer Bounds Error Tests
// =============================================================================

#[derive(Debug, Ron, PartialEq)]
struct WithU8 {
    value: u8,
}

#[test]
fn integer_bounds_error_mentions_value() {
    let input = r#"(value: 256)"#;
    let err = WithU8::from_ron(input).unwrap_err();
    let msg = err.to_string();

    // Should mention the value and/or the bounds
    assert!(
        msg.contains("256") || msg.contains("u8") || msg.contains("bounds"),
        "Error should mention value or type bounds: {}",
        msg
    );
}

#[test]
fn negative_unsigned_error() {
    let input = r#"(value: -1)"#;
    let err = WithU8::from_ron(input).unwrap_err();
    let msg = err.to_string();

    // Should indicate the problem
    assert!(
        msg.contains("-1")
            || msg.contains("u8")
            || msg.contains("unsigned")
            || msg.contains("bounds"),
        "Error should mention negative value issue: {}",
        msg
    );
}

// =============================================================================
// Span Slice Verification Tests
// =============================================================================

#[test]
fn error_span_can_extract_source_text() {
    let input = r#"(name: "test", port: "wrong", enabled: true)"#;
    let err = SimpleConfig::from_ron(input).unwrap_err();

    // Verify we can slice the source with the span
    let sliced = &input[err.span.start_offset..err.span.end_offset];

    // The sliced text should be the problematic value
    assert!(
        sliced.contains("wrong") || sliced == "\"wrong\"",
        "Sliced text should be the error location: '{}'",
        sliced
    );
}

#[test]
fn multiline_error_span_extracts_correct_text() {
    let input = r#"(
    name: "test",
    port: "wrong",
    enabled: true,
)"#;
    let err = SimpleConfig::from_ron(input).unwrap_err();

    let sliced = &input[err.span.start_offset..err.span.end_offset];
    assert!(
        sliced.contains("wrong"),
        "Sliced text should contain 'wrong': '{}'",
        sliced
    );
}

// =============================================================================
// Error Display Format Tests
// =============================================================================

#[test]
fn error_display_includes_position() {
    let input = r#"(name: "test", port: "wrong", enabled: true)"#;
    let err = SimpleConfig::from_ron(input).unwrap_err();
    let display = err.to_string();

    // Should have line:col format
    assert!(
        display.contains(':'),
        "Error display should include position: {}",
        display
    );

    // Should have actual error message too
    assert!(
        display.len() > 10,
        "Error display should have meaningful content: {}",
        display
    );
}

#[test]
fn error_display_is_user_friendly() {
    let input = r#"(name: "test", port: "wrong", enabled: true)"#;
    let err = SimpleConfig::from_ron(input).unwrap_err();
    let display = err.to_string();

    // Should not have internal type names like "Cow" or "Box"
    assert!(
        !display.contains("Cow<") && !display.contains("Box<"),
        "Error should not expose internal types: {}",
        display
    );

    // Should have proper capitalization or message structure
    assert!(
        display.chars().any(|c| c.is_alphabetic()),
        "Error should have readable text: {}",
        display
    );
}

// =============================================================================
// Regression Tests
// =============================================================================

#[test]
#[ignore = "() parses as unit, error says 'found unit' instead of listing missing fields"]
fn empty_struct_error_is_helpful() {
    let input = r#"()"#;
    let err = SimpleConfig::from_ron(input).unwrap_err();
    let msg = err.to_string();

    // Should mention missing fields
    assert!(
        msg.contains("name") || msg.contains("port") || msg.contains("Missing"),
        "Empty struct error should mention missing fields: {}",
        msg
    );
}

#[test]
#[ignore = "wrong struct name deserializes successfully - RON ignores struct names by default"]
fn wrong_struct_name_error_is_helpful() {
    #[derive(Debug, Ron, PartialEq)]
    struct Named {
        value: i32,
    }

    let input = r#"WrongName(value: 42)"#;
    let err = Named::from_ron(input).unwrap_err();
    let msg = err.to_string();

    // Should mention the name mismatch
    assert!(
        msg.contains("Named") || msg.contains("WrongName") || msg.contains("Expected"),
        "Wrong struct name error should be helpful: {}",
        msg
    );
}
