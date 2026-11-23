//! Error quality tests for ron-derive deserialization.
//!
//! These tests verify that deserialization errors provide:
//! 1. Precise span information pointing to the actual error location
//! 2. Type-aware error messages that help users understand what went wrong
//! 3. Context about expected vs actual types
//! 4. Suggestions for valid alternatives (for unknown fields/variants)

use std::collections::BTreeMap;

use ron2::{FromRon, ToRon, error::ErrorKind, fmt::FormatConfig};
use ron2_derive::Ron;

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

    // Error should mention the expected type u16
    assert!(
        msg.contains("u16"),
        "Error should mention expected type 'u16': {}",
        msg
    );
}

#[test]
fn error_message_includes_found_type() {
    let input = r#"(name: "test", port: "not_a_port", enabled: true)"#;
    let err = SimpleConfig::from_ron(input).unwrap_err();
    let msg = err.to_string();

    // Error should mention what was found (a string)
    assert!(
        msg.to_lowercase().contains("string"),
        "Error should mention found type 'string': {}",
        msg
    );
}

#[test]
fn error_message_for_wrong_struct_type() {
    let input = r#"[1, 2, 3]"#;
    let err = SimpleConfig::from_ron(input).unwrap_err();
    let msg = err.to_string();

    // Error should mention struct vs seq mismatch
    assert!(
        msg.contains("struct") && msg.to_lowercase().contains("seq"),
        "Error should mention struct vs seq mismatch: {}",
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
    if let ErrorKind::MissingField { field, outer } = &err.kind() {
        assert_eq!(field.as_ref(), "port");
        assert_eq!(
            outer.as_deref(),
            Some("SimpleConfig"),
            "Missing field error should include struct name"
        );
    } else {
        panic!("Expected MissingStructField error, got {:?}", err.kind());
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
        err.span().start.line,
        1,
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
    assert_eq!(
        msg,
        "1:27-1:30: Unknown field `nme` in `StrictConfig`, expected either `name` or `value` instead"
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
        err.span().start.line,
        4,
        "Unknown field error should point to field name on line 4, got line {}",
        err.span().start.line
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
    assert_eq!(
        msg,
        "1:1-1:4: Unknown variant `Txt` in enum `Message`, expected one of `Text`, `Number`, or `Empty` instead"
    );
}

#[test]
fn enum_field_error_has_correct_span() {
    let input = r#"Number(value: "not_a_number")"#;
    let err = Message::from_ron(input).unwrap_err();

    // Span should point to the invalid value
    assert_eq!(err.span().start.line, 1);
    assert_eq!(err.span().start.col, 15);
    assert_eq!(
        &input[err.span().start_offset..err.span().end_offset],
        "\"not_a_number\""
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
        err.span().start.line,
        4,
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
        err.span().start.line,
        4,
        "Error should be on line 4, got {}",
        err.span().start.line
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
    assert_eq!(err.span().start.line, 2);
}

#[test]
fn vec_element_error_mentions_type() {
    let input = r#"(items: [1, 2, "three"])"#;
    let err = WithVec::from_ron(input).unwrap_err();
    let msg = err.to_string();

    // Should mention the expected type (i32) - checking for string is wrong since
    // we want to verify it tells us what we EXPECTED, not what we got
    assert!(
        msg.contains("i32"),
        "Error should mention expected type 'i32': {}",
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
        err.span().start.line,
        4,
        "Error should be on line 4, got {}",
        err.span().start.line
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
    assert_eq!(err.span().start.line, 1);
    assert_eq!(err.span().start.col, 5);
    assert_eq!(
        &input[err.span().start_offset..err.span().end_offset],
        "\"not_an_int\""
    );
}

#[test]
fn tuple_wrong_length_error() {
    let input = r#"(1, 2, 3)"#;
    let err = Point::from_ron(input).unwrap_err();
    let msg = err.to_string();

    // Should mention expected length (2) - checking for '2' or '3' alone is too loose
    assert!(
        msg.contains("2 elements") || msg.contains("length 2") || msg.contains("expected 2"),
        "Error should mention expected tuple length of 2: {}",
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
    assert_eq!(err.span().start.col, 30);
    assert_eq!(
        &input[err.span().start_offset..err.span().end_offset],
        "\"not_an_int\""
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
    assert_eq!(
        msg,
        "1:9-1:11: in field 'value': expected Some(...) or None but found number"
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

    // Should mention both the value AND the type
    assert!(
        msg.contains("256") && msg.contains("u8"),
        "Error should mention both value '256' and type 'u8': {}",
        msg
    );
}

#[test]
fn negative_unsigned_error() {
    let input = r#"(value: -1)"#;
    let err = WithU8::from_ron(input).unwrap_err();
    let msg = err.to_string();

    // Verify error code is IntegerOutOfBounds with correct value and target type
    if let ErrorKind::IntegerOutOfBounds { value, target_type } = &err.kind() {
        assert_eq!(value.as_ref(), "-1", "Error should include the value '-1'");
        assert_eq!(*target_type, "u8", "Error should mention target type 'u8'");
    } else {
        panic!("Expected IntegerOutOfBounds error, got {:?}", err.kind());
    }

    // Verify the message is clear
    assert!(
        msg.contains("-1") && msg.contains("u8"),
        "Error message should mention both value and type: {}",
        msg
    );
}

#[test]
fn negative_unsigned_error_span_points_to_value() {
    let input = r#"(value: -1)"#;
    let err = WithU8::from_ron(input).unwrap_err();

    // Span should point to "-1" specifically
    let sliced = &input[err.span().start_offset..err.span().end_offset];
    assert_eq!(
        sliced, "-1",
        "Span should point exactly to '-1', got '{}'",
        sliced
    );

    // Verify position is correct (line 1, column 9 for the minus sign)
    assert_eq!(err.span().start.line, 1);
    assert_eq!(
        err.span().start.col,
        9,
        "Span should start at column 9 (the '-' in '-1'), got {}",
        err.span().start.col
    );
}

#[derive(Debug, Ron, PartialEq)]
struct WithU16 {
    value: u16,
}

#[derive(Debug, Ron, PartialEq)]
struct WithU32 {
    value: u32,
}

#[derive(Debug, Ron, PartialEq)]
struct WithU64 {
    value: u64,
}

#[test]
fn negative_unsigned_error_u16() {
    let input = r#"(value: -1)"#;
    let err = WithU16::from_ron(input).unwrap_err();

    if let ErrorKind::IntegerOutOfBounds { value, target_type } = &err.kind() {
        assert_eq!(value.as_ref(), "-1");
        assert_eq!(*target_type, "u16");
    } else {
        panic!("Expected IntegerOutOfBounds, got {:?}", err.kind());
    }
}

#[test]
fn negative_unsigned_error_u32() {
    let input = r#"(value: -1)"#;
    let err = WithU32::from_ron(input).unwrap_err();

    if let ErrorKind::IntegerOutOfBounds { value, target_type } = &err.kind() {
        assert_eq!(value.as_ref(), "-1");
        assert_eq!(*target_type, "u32");
    } else {
        panic!("Expected IntegerOutOfBounds, got {:?}", err.kind());
    }
}

#[test]
fn negative_unsigned_error_u64() {
    let input = r#"(value: -1)"#;
    let err = WithU64::from_ron(input).unwrap_err();

    if let ErrorKind::IntegerOutOfBounds { value, target_type } = &err.kind() {
        assert_eq!(value.as_ref(), "-1");
        assert_eq!(*target_type, "u64");
    } else {
        panic!("Expected IntegerOutOfBounds, got {:?}", err.kind());
    }
}

#[test]
fn large_negative_unsigned_error() {
    let input = r#"(value: -1000)"#;
    let err = WithU8::from_ron(input).unwrap_err();

    if let ErrorKind::IntegerOutOfBounds { value, target_type } = &err.kind() {
        assert_eq!(value.as_ref(), "-1000");
        assert_eq!(*target_type, "u8");
    } else {
        panic!("Expected IntegerOutOfBounds, got {:?}", err.kind());
    }

    // Verify span points to the value
    let sliced = &input[err.span().start_offset..err.span().end_offset];
    assert_eq!(sliced, "-1000");
}

#[test]
fn negative_unsigned_multiline_span() {
    let input = r#"(
    value: -42,
)"#;
    let err = WithU8::from_ron(input).unwrap_err();

    // Error should be on line 2 where -42 is
    assert_eq!(
        err.span().start.line,
        2,
        "Error should be on line 2, got line {}",
        err.span().start.line
    );

    // Span should point to "-42"
    let sliced = &input[err.span().start_offset..err.span().end_offset];
    assert_eq!(sliced, "-42");
}

#[test]
fn negative_unsigned_error_message_format() {
    let input = r#"(value: -1)"#;
    let err = WithU8::from_ron(input).unwrap_err();
    let msg = err.to_string();

    // The message format should be: "1:9-1:10: Integer -1 is out of bounds for u8"
    assert!(
        msg.contains("Integer -1 is out of bounds for u8"),
        "Expected clear error message, got: {}",
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
    let sliced = &input[err.span().start_offset..err.span().end_offset];

    // The sliced text should be the problematic value
    assert_eq!(sliced, "\"wrong\"");
}

#[test]
fn multiline_error_span_extracts_correct_text() {
    let input = r#"(
    name: "test",
    port: "wrong",
    enabled: true,
)"#;
    let err = SimpleConfig::from_ron(input).unwrap_err();

    let sliced = &input[err.span().start_offset..err.span().end_offset];
    assert_eq!(sliced, "\"wrong\"");
    assert_eq!(err.span().start.line, 3);
}

// =============================================================================
// Error Display Format Tests
// =============================================================================

#[test]
fn error_display_includes_position() {
    let input = r#"(name: "test", port: "wrong", enabled: true)"#;
    let err = SimpleConfig::from_ron(input).unwrap_err();
    let display = err.to_string();

    // Position (line:col range) followed by the message
    assert_eq!(
        display,
        "1:22-1:29: in field 'port': expected u16 but found string"
    );
}

// =============================================================================
// Regression Tests
// =============================================================================

#[test]
fn empty_struct_error_is_helpful() {
    let input = r#"()"#;
    let err = SimpleConfig::from_ron(input).unwrap_err();
    let msg = err.to_string();

    // Should mention the expected fields when () is passed for a struct
    assert!(
        msg.contains("name") && msg.contains("port") && msg.contains("enabled"),
        "Empty struct error should list expected fields: {}",
        msg
    );
}

#[test]
fn wrong_struct_name_error_is_helpful() {
    #[derive(Debug, Ron, PartialEq)]
    struct Named {
        value: i32,
    }

    let input = r#"WrongName(value: 42)"#;
    let err = Named::from_ron(input).unwrap_err();
    let msg = err.to_string();

    // Should mention the name mismatch
    assert_eq!(
        msg,
        "1:1-1:10: expected struct Named but found struct WrongName"
    );
}

// =============================================================================
// Duplicate Field Error Tests
// =============================================================================

#[test]
fn duplicate_field_produces_error() {
    // Duplicate struct fields should produce an error.
    let input = r#"(name: "alice", name: "bob", port: 8080, enabled: true)"#;
    let err = SimpleConfig::from_ron(input).unwrap_err();

    // Error should be DuplicateStructField
    if let ErrorKind::DuplicateField { field, outer } = &err.kind() {
        assert_eq!(field.as_ref(), "name");
        assert_eq!(
            outer.as_deref(),
            Some("SimpleConfig"),
            "Duplicate field error should include struct name"
        );
    } else {
        panic!("Expected DuplicateStructField error, got {:?}", err.kind());
    }
}

#[test]
fn duplicate_field_span_points_to_second_occurrence() {
    // When duplicate fields are detected, the span should point to the second (duplicate) field,
    // not the first one, since that helps the user identify which one to remove.
    let input = r#"(
    name: "alice",
    port: 8080,
    name: "bob",
    enabled: true,
)"#;
    let err = SimpleConfig::from_ron(input).unwrap_err();

    // Span should point to the duplicate "name" on line 4
    assert_eq!(
        err.span().start.line,
        4,
        "Duplicate field error should point to second occurrence on line 4, got line {}",
        err.span().start.line
    );
}

#[test]
fn duplicate_field_in_nested_struct_produces_error() {
    // Duplicate fields in nested structs should also produce an error.
    let input = r#"(
    name: "test",
    inner: (
        value: 1,
        value: 2,
    ),
)"#;
    let err = Outer::from_ron(input).unwrap_err();

    // Error should be DuplicateStructField
    if let ErrorKind::DuplicateField { field, outer } = &err.kind() {
        assert_eq!(field.as_ref(), "value");
        assert_eq!(outer.as_deref(), Some("Inner"));
    } else {
        panic!("Expected DuplicateStructField error, got {:?}", err.kind());
    }
}

// =============================================================================
// Flatten Field Error Tests
// =============================================================================

/// Inner struct for flattening tests.
#[derive(Debug, Ron, PartialEq)]
struct FlattenInner {
    value: i32,
    count: u16,
}

/// Outer struct with a flattened inner struct.
#[derive(Debug, Ron, PartialEq)]
struct FlattenOuter {
    name: String,
    #[ron(flatten)]
    inner: FlattenInner,
}

#[test]
fn flatten_field_error_points_to_invalid_value() {
    // When a flattened struct has a field with an invalid type,
    // the error span should point directly to the invalid value.
    //
    // With flatten, the RON looks like: (name: "test", value: "not_an_int", count: 1)
    // The "value" field belongs to FlattenInner but appears at the top level.
    let input = r#"(name: "test", value: "not_an_int", count: 1)"#;
    let err = FlattenOuter::from_ron(input).unwrap_err();

    // Error should point to "not_an_int"
    assert_eq!(err.span().start.line, 1);
    assert_eq!(err.span().start.col, 23);
    assert_eq!(
        &input[err.span().start_offset..err.span().end_offset],
        "\"not_an_int\""
    );
}

#[test]
fn flatten_field_error_includes_context() {
    // Error message should provide context about both the outer and inner struct
    // to help users understand the field hierarchy.
    let input = r#"(name: "test", value: "not_an_int", count: 1)"#;
    let err = FlattenOuter::from_ron(input).unwrap_err();
    let msg = err.to_string();

    // The error should mention the type mismatch
    assert_eq!(
        msg,
        "1:23-1:35: in field 'value': expected i32 but found string"
    );
}

#[test]
fn flatten_missing_field_error_names_the_field() {
    // When a flattened struct is missing a required field,
    // the error should clearly name the missing field.
    let input = r#"(name: "test", count: 1)"#; // missing "value"
    let err = FlattenOuter::from_ron(input).unwrap_err();
    let msg = err.to_string();

    assert!(
        msg.contains("value"),
        "Error should name missing field 'value': {}",
        msg
    );
}

#[test]
fn flatten_multiline_error_has_correct_span() {
    // Multiline RON with error in flattened field
    let input = r#"(
    name: "test",
    value: "wrong",
    count: 1,
)"#;
    let err = FlattenOuter::from_ron(input).unwrap_err();

    // Error should point to "wrong" on line 3
    assert_eq!(
        err.span().start.line,
        3,
        "Error should be on line 3, got line {}",
        err.span().start.line
    );
}

/// Nested struct for deeply flattened tests.
#[derive(Debug, Ron, PartialEq)]
struct FlattenLevel2 {
    data: u8,
}

/// Struct with nested flatten.
#[derive(Debug, Ron, PartialEq)]
struct FlattenLevel1 {
    id: i32,
    #[ron(flatten)]
    level2: FlattenLevel2,
}

/// Top level struct with deeply nested flatten.
#[derive(Debug, Ron, PartialEq)]
struct FlattenTop {
    name: String,
    #[ron(flatten)]
    level1: FlattenLevel1,
}

#[test]
fn deeply_flattened_error_preserves_span() {
    // With deeply nested flatten, all fields appear at top level:
    // (name: "test", id: 1, data: "wrong")
    let input = r#"(
    name: "test",
    id: 1,
    data: "wrong",
)"#;
    let err = FlattenTop::from_ron(input).unwrap_err();

    // Error should be on line 4 where "wrong" is
    assert_eq!(
        err.span().start.line,
        4,
        "Error should be on line 4, got {}",
        err.span().start.line
    );
}

// Test current behavior without flatten (baseline)
#[test]
fn nested_struct_without_flatten_has_correct_error() {
    // Without flatten, the nested struct is wrapped in parentheses
    // This test confirms nested errors work correctly as a baseline
    let input = r#"(
    name: "test",
    inner: (
        value: "not_an_int",
    ),
)"#;
    let err = Outer::from_ron(input).unwrap_err();

    // Error should point to "not_an_int" on line 4
    assert_eq!(
        err.span().start.line,
        4,
        "Error should be on line 4 where invalid value is"
    );
}

// =============================================================================
// Flatten with Optional Fields Tests
// =============================================================================

/// Inner struct with optional field for flatten tests.
#[derive(Debug, Ron, PartialEq)]
struct FlattenInnerWithOptional {
    required: i32,
    #[ron(default)]
    optional: Option<String>,
}

/// Outer struct with flattened optional inner.
#[derive(Debug, Ron, PartialEq)]
struct FlattenOuterWithOptional {
    name: String,
    #[ron(flatten)]
    inner: FlattenInnerWithOptional,
}

#[test]
fn flatten_with_optional_field_error() {
    // When optional flattened field has wrong type
    let input = r#"(name: "test", required: 42, optional: 123)"#;
    let err = FlattenOuterWithOptional::from_ron(input).unwrap_err();
    let msg = err.to_string();

    // 123 is wrong type for Option<String> - should expect string or None
    assert_eq!(
        msg,
        "1:40-1:43: in field 'optional': expected String but found number"
    );
}

// =============================================================================
// Flatten Field Conflicts Tests
// =============================================================================

// A field name shared by a struct and its flattened fields is a compile
// error (`tests/ui/flatten_conflicts.rs`). The cases below are no conflicts.

#[derive(Debug, Ron, PartialEq)]
struct ConflictInner {
    shared: i32,
}

/// Skipping the outer field on one side removes the conflict on that side.
#[derive(Debug, FromRon, PartialEq)]
struct SkippedOuter {
    #[ron(skip_deserializing)]
    shared: String,
    #[ron(flatten)]
    inner: ConflictInner,
}

/// A flattened map takes the fields nobody else claimed.
#[derive(Debug, Ron, PartialEq)]
struct CatchAllOuter {
    shared: String,
    #[ron(flatten)]
    rest: BTreeMap<String, i32>,
}

/// The names of a generic flattened field depend on the instantiation.
#[derive(Debug, Ron, PartialEq)]
struct GenericOuter<T> {
    name: String,
    #[ron(flatten)]
    inner: T,
}

#[test]
fn flatten_field_name_conflict() {
    let skipped = SkippedOuter::from_ron("(shared: 1)").unwrap();
    assert_eq!(
        skipped,
        SkippedOuter {
            shared: String::new(),
            inner: ConflictInner { shared: 1 },
        }
    );

    let catch_all = CatchAllOuter::from_ron(r#"(shared: "text", extra: 2)"#).unwrap();
    assert_eq!(
        catch_all,
        CatchAllOuter {
            shared: "text".into(),
            rest: BTreeMap::from([("extra".into(), 2)]),
        }
    );
    assert_eq!(
        catch_all.to_ron_with(&FormatConfig::minimal()).unwrap(),
        r#"CatchAllOuter(shared:"text",extra:2)"#
    );

    let generic = GenericOuter::<ConflictInner>::from_ron(r#"(name: "n", shared: 3)"#).unwrap();
    assert_eq!(
        generic.to_ron_with(&FormatConfig::minimal()).unwrap(),
        r#"GenericOuter(name:"n",shared:3)"#
    );
}

// =============================================================================
// Map Key vs Value Error Context Tests
// =============================================================================

/// Tests for distinguishing and correctly pointing to map KEY errors.
mod map_key_errors {
    use super::*;

    #[derive(Debug, Ron, PartialEq)]
    struct MapWithStringKeys {
        data: std::collections::HashMap<String, i32>,
    }

    #[test]
    fn map_key_error_span_points_to_key() {
        // Using a number as key when String is expected
        let input = r#"(
    data: {
        "valid": 1,
        123: 2,
    },
)"#;
        let err = MapWithStringKeys::from_ron(input).unwrap_err();

        // Error should be on line 4 where the invalid key `123` is
        assert_eq!(
            err.span().start.line,
            4,
            "Key error should point to line 4 where `123` is, got line {}",
            err.span().start.line
        );

        // The column should point to the key (123), not the value (2)
        // The key `123` starts at column 9 (after 8 spaces of indentation)
        assert_eq!(err.span().start.col, 9);
    }

    #[test]
    fn map_key_error_message_indicates_type_mismatch() {
        let input = r#"(data: { 123: 456 })"#;
        let err = MapWithStringKeys::from_ron(input).unwrap_err();
        let msg = err.to_string();

        // Error should mention expected type (string)
        assert_eq!(
            msg,
            "1:10-1:13: in field 'data' -> map key: expected String but found number"
        );
    }

    #[test]
    fn map_key_error_span_extracts_key_text() {
        let input = r#"(data: { 123: 456 })"#;
        let err = MapWithStringKeys::from_ron(input).unwrap_err();

        // Extract the source text at the error span
        let sliced = &input[err.span().start_offset..err.span().end_offset];

        // Should be the key, not the value
        assert_eq!(sliced, "123");
    }

    #[derive(Debug, Ron, PartialEq)]
    struct MapWithI32Keys {
        data: BTreeMap<i32, String>,
    }

    #[test]
    fn map_key_type_error_for_string_key_when_i32_expected() {
        let input = r#"(data: { "not_an_int": "value" })"#;
        let err = MapWithI32Keys::from_ron(input).unwrap_err();
        let msg = err.to_string();

        // Error should indicate integer expected
        assert_eq!(
            msg,
            "1:10-1:22: in field 'data' -> map key: expected i32 but found string"
        );
    }

    #[test]
    fn map_key_error_multiline_correct_span() {
        let input = r#"{
    "valid_key": 1,
    "another_valid": 2,
    3.14: 3,
    "more_valid": 4,
}"#;
        let err = BTreeMap::<String, i32>::from_ron(input).unwrap_err();

        // Error should point to line 4 where `3.14` is
        assert_eq!(
            err.span().start.line,
            4,
            "Key error should be on line 4, got {}",
            err.span().start.line
        );
    }
}

/// Tests for distinguishing and correctly pointing to map VALUE errors.
mod map_value_errors {
    use super::*;

    #[derive(Debug, Ron, PartialEq)]
    struct MapWithI32Values {
        data: std::collections::HashMap<String, i32>,
    }

    #[test]
    fn map_value_error_span_points_to_value() {
        let input = r#"(
    data: {
        "a": 1,
        "b": "not_a_number",
    },
)"#;
        let err = MapWithI32Values::from_ron(input).unwrap_err();

        // Error should be on line 4
        assert_eq!(
            err.span().start.line,
            4,
            "Value error should point to line 4, got line {}",
            err.span().start.line
        );

        // The column should point to the value, not the key
        // The value starts after `"b": ` at column 14
        assert_eq!(err.span().start.col, 14);
    }

    #[test]
    fn map_value_error_span_extracts_value_text() {
        let input = r#"(data: { "key": "wrong_type" })"#;
        let err = MapWithI32Values::from_ron(input).unwrap_err();

        // Extract the source text at the error span
        let sliced = &input[err.span().start_offset..err.span().end_offset];

        // Should be the value, not the key
        assert_eq!(sliced, "\"wrong_type\"");
    }

    #[test]
    fn map_value_error_message_indicates_type_mismatch() {
        let input = r#"(data: { "key": "not_an_int" })"#;
        let err = MapWithI32Values::from_ron(input).unwrap_err();
        let msg = err.to_string();

        // Error should mention expected type (i32/integer)
        assert_eq!(
            msg,
            "1:17-1:29: in field 'data' -> map value for '\"key\"': expected i32 but found string"
        );
    }

    #[derive(Debug, Ron, PartialEq)]
    struct MapWithNestedValues {
        data: BTreeMap<String, MapValueInner>,
    }

    #[derive(Debug, Ron, PartialEq)]
    struct MapValueInner {
        value: i32,
    }

    #[test]
    fn map_value_nested_error_has_correct_span() {
        let input = r#"(
    data: {
        "first": (value: 1),
        "second": (value: "oops"),
    },
)"#;
        let err = MapWithNestedValues::from_ron(input).unwrap_err();

        // Error should be on line 4 where "oops" is
        assert_eq!(
            err.span().start.line,
            4,
            "Nested value error should be on line 4, got {}",
            err.span().start.line
        );
    }

    #[test]
    fn standalone_map_value_error() {
        // Test a map not wrapped in a struct
        let input = r#"{ "a": 1, "b": "wrong" }"#;
        let err = BTreeMap::<String, i32>::from_ron(input).unwrap_err();
        let msg = err.to_string();

        // Should indicate type mismatch
        assert_eq!(
            msg,
            "1:16-1:23: in map value for '\"b\"': expected i32 but found string"
        );
    }
}

/// Tests for distinguishing key errors from value errors.
mod key_vs_value_distinction {
    use super::*;

    #[derive(Debug, Ron, PartialEq)]
    struct StringToI32Map {
        map: BTreeMap<String, i32>,
    }

    #[test]
    fn key_error_column_is_before_colon() {
        // Key error: number instead of string
        let input = r#"(map: { 999: 123 })"#;
        let err = StringToI32Map::from_ron(input).unwrap_err();

        // The span covers exactly the key, which ends before the entry's colon
        assert_eq!((err.span().start_offset, err.span().end_offset), (8, 11));
        assert_eq!(&input[8..11], "999");
    }

    #[test]
    fn value_error_column_is_after_colon() {
        // Value error: string instead of i32
        let input = r#"(map: { "key": "wrong" })"#;
        let err = StringToI32Map::from_ron(input).unwrap_err();

        // The span covers exactly the value, which starts after the entry's colon
        assert_eq!((err.span().start_offset, err.span().end_offset), (15, 22));
        assert_eq!(&input[15..22], "\"wrong\"");
    }

    #[test]
    fn both_key_and_value_wrong_reports_first_error() {
        // Both key and value are wrong types
        let input = r#"(map: { 123: "wrong" })"#;
        let err = StringToI32Map::from_ron(input).unwrap_err();

        // Should report the key error first (since key is deserialized before value)
        let sliced = &input[err.span().start_offset..err.span().end_offset];
        assert_eq!(sliced, "123", "Should report key error first");
    }

    #[derive(Debug, Ron, PartialEq)]
    struct I32ToStringMap {
        map: BTreeMap<i32, String>,
    }

    #[test]
    fn different_key_type_error() {
        // Using string key when i32 is expected
        let input = r#"(map: { "not_int": "value" })"#;
        let err = I32ToStringMap::from_ron(input).unwrap_err();

        // Error span should point to the key
        let sliced = &input[err.span().start_offset..err.span().end_offset];
        assert_eq!(sliced, "\"not_int\"");
    }

    #[test]
    fn different_value_type_error() {
        // Using integer value when String is expected
        let input = r#"(map: { 42: 999 })"#;
        let err = I32ToStringMap::from_ron(input).unwrap_err();

        // Error span should point to the value (999), not the key (42)
        let sliced = &input[err.span().start_offset..err.span().end_offset];
        assert_eq!(sliced, "999");
    }
}

/// Tests for complex map scenarios.
mod complex_map_scenarios {
    use super::*;

    #[test]
    fn empty_map_is_valid() {
        let input = r#"{}"#;
        let result = BTreeMap::<String, i32>::from_ron(input);
        assert_eq!(
            result.unwrap(),
            BTreeMap::new(),
            "Empty map should be valid"
        );
    }

    #[test]
    fn first_entry_key_error() {
        let input = r#"{ 123: "value" }"#;
        let err = BTreeMap::<String, String>::from_ron(input).unwrap_err();

        // Should point to the first entry's key
        assert_eq!(err.span().start.line, 1);
        assert_eq!(err.span().start.col, 3);
        assert_eq!(
            &input[err.span().start_offset..err.span().end_offset],
            "123"
        );
    }

    #[test]
    fn first_entry_value_error() {
        let input = r#"{ "key": 123 }"#;
        let err = BTreeMap::<String, String>::from_ron(input).unwrap_err();

        // Should point to the first entry's value
        let sliced = &input[err.span().start_offset..err.span().end_offset];
        assert_eq!(sliced, "123");
    }

    #[test]
    fn middle_entry_error() {
        let input = r#"{
    "a": "valid1",
    "b": "valid2",
    "c": 12345,
    "d": "valid3",
}"#;
        let err = BTreeMap::<String, String>::from_ron(input).unwrap_err();

        // Error should be on line 4 where the invalid value is
        assert_eq!(
            err.span().start.line,
            4,
            "Error should be on line 4, got {}",
            err.span().start.line
        );
    }

    #[test]
    fn last_entry_error() {
        let input = r#"{ "a": "ok", "b": "ok", "c": 999 }"#;
        let err = BTreeMap::<String, String>::from_ron(input).unwrap_err();

        let sliced = &input[err.span().start_offset..err.span().end_offset];
        assert_eq!(sliced, "999", "Error should point to last value");
    }

    #[derive(Debug, Ron, PartialEq)]
    struct NestedMaps {
        outer: BTreeMap<String, BTreeMap<String, i32>>,
    }

    #[test]
    fn nested_map_inner_key_error() {
        let input = r#"(
    outer: {
        "level1": {
            123: 456,
        },
    },
)"#;
        let err = NestedMaps::from_ron(input).unwrap_err();

        // Error should point to the inner map's invalid key
        assert_eq!(
            err.span().start.line,
            4,
            "Error should be on line 4, got {}",
            err.span().start.line
        );
    }

    #[test]
    fn nested_map_inner_value_error() {
        let input = r#"(
    outer: {
        "level1": {
            "key": "not_an_int",
        },
    },
)"#;
        let err = NestedMaps::from_ron(input).unwrap_err();

        // Error should point to the inner map's invalid value
        assert_eq!(
            err.span().start.line,
            4,
            "Error should be on line 4, got {}",
            err.span().start.line
        );

        let sliced = &input[err.span().start_offset..err.span().end_offset];
        assert_eq!(sliced, "\"not_an_int\"");
    }
}
