# RON Error Quality Report

This report summarizes the current state of error handling across the ron-extras crates and provides recommendations for achieving rustc-like pretty error diagnostics.

**Last Updated**: January 2026

## Executive Summary

**Current State**: The error system has a solid foundation with proper span tracking and type-aware error messages. The deserialization layer provides excellent span accuracy. Some bugs and missing features remain.

| Area | Status | Notes |
|------|--------|-------|
| Span data structure | Excellent | Line/col + byte offsets |
| Deserialization spans | Excellent | Field-level accuracy, nested structures |
| Parsing span accuracy | Excellent | All spans correct |
| Error message content | Excellent | Type-aware, contextual, with suggestions |
| Map key/value errors | Excellent | Correct span distinction between keys and values |
| Duplicate field detection | **BUG** | Silently overwrites instead of erroring |
| Flatten deserialization | **Missing** | Attribute parsed but ignored |
| Schema validation spans | Missing | Uses semantic path, not source spans |
| Rustc-like rendering | Not implemented | No source snippets, no underlines |

---

## Detailed Findings

### 1. What Works Well

#### Parsing Error Spans (ron2)

The parsing layer provides excellent span accuracy:

```
PASSED: span_points_to_unclosed_bracket_content
PASSED: span_points_to_trailing_junk
PASSED: span_byte_offsets_enable_slicing
PASSED: span_slice_method_works
PASSED: span_points_to_unclosed_string
PASSED: span_points_to_invalid_token
PASSED: span_points_to_invalid_escape_sequence
PASSED: span_points_to_map_missing_colon
PASSED: span_multiline_points_to_correct_line
PASSED: span_deeply_nested_error
PASSED: very_long_line_has_correct_position
PASSED: unicode_positions_are_character_based
PASSED: error_after_comment_has_correct_position
PASSED: empty_input_has_sensible_error
PASSED: whitespace_only_has_sensible_error
```

#### Deserialization Error Spans (ron-derive)

The deserialization layer provides excellent span accuracy:

```
PASSED: enum_field_error_has_correct_span
PASSED: nested_error_has_correct_span
PASSED: deeply_nested_error_preserves_span
PASSED: vec_element_error_has_correct_span
PASSED: map_value_error_has_correct_span
PASSED: tuple_element_error_has_correct_span
PASSED: unknown_field_span_points_to_field_name
PASSED: option_inner_type_error_has_correct_span
PASSED: missing_field_span_points_to_struct
PASSED: negative_unsigned_error_span_points_to_value
```

#### Map Key vs Value Error Distinction

Map errors correctly distinguish between key and value errors with accurate spans:

```
PASSED: map_key_error_span_points_to_key
PASSED: map_key_error_span_extracts_key_text
PASSED: map_value_error_span_points_to_value
PASSED: map_value_error_span_extracts_value_text
PASSED: key_error_column_is_before_colon
PASSED: value_error_column_is_after_colon
PASSED: both_key_and_value_wrong_reports_first_error
PASSED: nested_map_inner_key_error
PASSED: nested_map_inner_value_error
```

#### Type-Aware Error Messages

Error messages include helpful context:

```
PASSED: error_message_includes_expected_type (checks for "u16")
PASSED: error_message_includes_found_type (checks for "string")
PASSED: error_message_for_wrong_struct_type (checks for struct vs seq mismatch)
PASSED: unknown_field_error_names_the_field
PASSED: unknown_field_error_suggests_valid_fields
PASSED: unknown_variant_error_names_the_variant
PASSED: unknown_variant_error_suggests_valid_variants
PASSED: explicit_option_error_mentions_some_none
PASSED: integer_bounds_error_mentions_value (checks for both "256" AND "u8")
PASSED: negative_unsigned_error (verifies IntegerOutOfBounds error code)
PASSED: tuple_wrong_length_error
PASSED: vec_element_error_mentions_type (checks for "i32")
PASSED: empty_struct_error_is_helpful (lists expected fields)
```

Example messages:
- `"Expected struct SimpleConfig with fields: name, port, enabled but found unit instead"`
- `"Unknown field 'nme' in 'Config', expected one of 'name' or 'value' instead"`
- `"Integer -1 is out of bounds for u8"`

---

### 2. Known Bugs

#### BUG 1: Duplicate Fields Silently Overwrite

**Severity**: High (data loss)

```rust
// Input: (name: "alice", name: "bob", port: 8080, enabled: true)
// Expected: DuplicateStructField error pointing to second "name"
// Actual: Ok(SimpleConfig { name: "bob", port: 8080, enabled: true })
```

The `DuplicateStructField` error type exists in `error.rs` but is never raised. In `convert.rs`, `AstMapAccess::from_anon` and `from_fields` use `HashMap::insert()` without checking for duplicates.

**Tests documenting this bug:**
- `duplicate_field_produces_error` (ignored - will pass when fixed)
- `duplicate_field_span_points_to_second_occurrence` (ignored - will pass when fixed)
- `duplicate_field_currently_uses_last_value` (documents current buggy behavior)

---

### 3. Missing Features

#### Missing 1: Flatten Deserialization Not Implemented

The `#[ron(flatten)]` attribute is parsed and works for schema generation, but deserialization ignores it and treats the field as a regular nested struct.

```rust
#[derive(FromRon)]
struct Outer {
    name: String,
    #[ron(flatten)]
    inner: Inner,  // Should flatten Inner's fields into Outer
}

// Input: (name: "test", value: 42, count: 1)  // value/count from Inner
// Expected: Ok with flattened fields
// Actual: Error "Missing required field `inner`"
```

**Tests documenting this:**
- `flatten_field_error_points_to_invalid_value` (ignored)
- `flatten_missing_field_error_names_the_field` (ignored)
- `flatten_multiline_error_has_correct_span` (ignored)
- `deeply_flattened_error_preserves_span` (ignored)

#### Missing 2: RON Ignores Struct Names (By Design)

```
IGNORED: wrong_struct_name_error_is_helpful
  - Input: "WrongName(value: 42)"
  - Expected: Error about wrong struct name
  - Got: Ok(Named { value: 42 })
```

**Note**: RON intentionally ignores struct names during deserialization. The struct name is cosmetic.

#### Missing 3: Schema Validation Loses Source Spans

The validation module works on `Value` (semantic data) not AST, so source positions are lost. Errors use logical paths instead:

```rust
// Current output
"in field 'items' -> element 1: type mismatch: expected i32, got String"

// vs rustc-like output would need
"config.ron:15:8: error: expected i32, got String"
```

---

### 4. Recommendations for Rustc-Like Errors

#### Phase 1: Span Accuracy ✅ COMPLETE

All span accuracy issues have been fixed and thoroughly tested.

#### Phase 2: Fix Duplicate Field Bug (High Priority)

In `crates/ron2/src/convert.rs`, modify `AstMapAccess::from_anon` and `from_fields` to check for duplicate keys before inserting into the HashMap. Return `DuplicateStructField` error with span pointing to the second occurrence.

#### Phase 3: Implement Flatten Deserialization (Medium Priority)

In `crates/ron-derive/src/de.rs`, handle the `flatten` attribute by:
1. Collecting fields from the flattened struct at the same level as outer fields
2. Proper error handling with correct spans for flattened field errors

#### Phase 4: Add Error Rendering (Medium Priority)

Create a diagnostic rendering module that produces output like:

```
error[E0308]: expected u16, found string
 --> config.ron:3:11
  |
3 |     port: "not_a_port",
  |           ^^^^^^^^^^^^^ expected u16
  |
  = note: the field `port` in `SimpleConfig` must be a valid port number
```

#### Phase 5: Schema Validation with Spans (Lower Priority)

To get spans in schema validation, would need to:
1. Validate against AST (`Expr`) instead of `Value`
2. Or carry spans through Value somehow
3. Or re-parse to find span when error occurs

---

## Test Coverage Summary

### ron2 (parsing)
- **24/24 tests pass (100%)**
- All parsing errors have accurate spans

### ron-derive (deserialization)
- **60/70 tests pass (86%)**
- 10 tests skipped:
  - 1 intentional RON behavior (`wrong_struct_name_error_is_helpful`)
  - 2 duplicate field bug (`duplicate_field_produces_error`, `duplicate_field_span_points_to_second_occurrence`)
  - 7 flatten not implemented

### ron-schema (validation)
- Good path tracking
- No span preservation (by design - validates Value, not AST)

---

## Files Added/Modified

### Test Files
- `crates/ron2/tests/error_quality.rs` - Parsing error span tests (24 tests)
- `crates/ron-derive/tests/error_quality.rs` - Deserialization error tests (70 tests)

### Key Source Files for Error Handling
- `crates/ron2/src/error.rs` - Error types, Span, Position
- `crates/ron2/src/ast/parse.rs` - Parser error construction
- `crates/ron2/src/convert.rs` - Deserialization errors (contains duplicate field bug)
- `crates/ron-derive/src/de.rs` - Derive macro deserialization (missing flatten support)
- `crates/ron-schema/src/error.rs` - Schema error types
- `crates/ron-schema/src/validation.rs` - Validation errors

---

## Conclusion

The error system is in excellent shape for span accuracy:
- Spans capture line/column and byte offsets correctly
- Error codes are specific and contextual with strong type information
- Map key vs value errors are correctly distinguished
- Integer bound errors include both value and target type
- Empty struct errors list expected fields

**Known bugs to fix:**
1. **Duplicate field detection** - Currently silently overwrites (high priority)

**Missing features to implement:**
1. **Flatten deserialization** - Attribute parsed but not used
2. **Diagnostic renderer** - For rustc-style pretty output

**Test improvements made:**
1. Fixed no-op `matches!` assertions (wrapped in `assert!`)
2. Removed test that didn't test errors (`span_nested_struct_error_correct_line`)
3. Tightened weak assertions (replaced `||` chains with specific checks)
4. Added 50+ new tests for map key/value distinction, integer errors, duplicate fields, and flatten fields
