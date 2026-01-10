# RON Error Quality Report

This report summarizes the current state of error handling across the ron-extras crates and provides recommendations for achieving rustc-like pretty error diagnostics.

## Executive Summary

**Current State**: The error system has a solid foundation with proper span tracking and type-aware error messages, but several gaps exist that prevent truly excellent diagnostics.

| Area | Status | Notes |
|------|--------|-------|
| Span data structure | Excellent | Line/col + byte offsets |
| Deserialization spans | Good | Field-level accuracy |
| Parsing span accuracy | Fair | Some spans point to wrong location |
| Error message content | Good | Type-aware, contextual |
| Schema validation spans | Missing | Uses semantic path, not source spans |
| Rustc-like rendering | Not implemented | No source snippets, no underlines |

---

## Detailed Findings

### 1. What Works Well

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
```

Errors correctly point to:
- Invalid field values (not the whole struct)
- Unknown field names (the actual field, not struct)
- Nested errors at any depth
- Collection elements that fail type conversion

#### Type-Aware Error Messages

Error messages include helpful context:

```
PASSED: error_message_includes_expected_type
PASSED: error_message_includes_found_type
PASSED: unknown_field_error_names_the_field
PASSED: unknown_field_error_suggests_valid_fields
PASSED: unknown_variant_error_names_the_variant
PASSED: unknown_variant_error_suggests_valid_variants
PASSED: explicit_option_error_mentions_some_none
PASSED: integer_bounds_error_mentions_value
```

Example messages:
- `"Expected struct SimpleConfig but found string instead"`
- `"Unknown field 'nme' in 'Config', expected one of 'name' or 'value' instead"`
- `"Integer 256 is out of bounds for u8"`

---

### 2. Issues Found

#### Issue 1: Parsing Spans Sometimes Point to Wrong Location

Several parsing errors report span at column 1 instead of the actual error location:

```
FAILED: span_points_to_invalid_token
  - Input: "0xGGG"
  - Expected: col >= 3 (at 'G')
  - Got: col 1

FAILED: span_points_to_invalid_escape_sequence
  - Input: "hello\qworld"
  - Expected: col >= 7 (at '\q')
  - Got: col 1

FAILED: span_points_to_map_missing_colon
  - Input: { "key" "value" }
  - Expected: col >= 9 (near 'value')
  - Got: col 3
```

**Root cause**: Error spans may be capturing the start of the parent construct rather than the precise error location.

#### Issue 2: EOF Errors Report Column 1

```
FAILED: span_points_to_unclosed_bracket_content
  - Input: "[1, 2, 3" (missing ])
  - Expected: col 9 (EOF)
  - Got: col 1

FAILED: very_long_line_has_correct_position
  - Input: "[" + "x" * 1000
  - Expected: col > 1000
  - Got: col 1
```

**Root cause**: EOF error positions appear to reset to start of line.

#### Issue 3: Bare Identifiers Treated as Valid Values

```
FAILED: span_multiline_points_to_correct_line
  - Input: [1, 2, invalid_ident]
  - Expected: Error
  - Got: Ok(Named { name: "invalid_ident", content: Unit })

FAILED: span_deeply_nested_error
  - Input: { "level3": [1, 2, invalid] }
  - Expected: Error
  - Got: Ok(Named { name: "invalid", content: Unit })
```

**Note**: This is intentional RON behavior - bare identifiers are valid unit-like values. But it means typos in enum variants aren't caught during parsing.

#### Issue 4: Empty/Whitespace Input Parses as Unit

```
FAILED: empty_input_has_sensible_error
  - Input: ""
  - Expected: Error
  - Got: Ok(Unit)

FAILED: whitespace_only_has_sensible_error
  - Input: "   \n\n   "
  - Expected: Error
  - Got: Ok(Unit)
```

**Note**: This may be intentional. Unit `()` can be represented as empty input.

#### Issue 5: Missing Struct Name Context in Some Errors

```
FAILED: missing_field_error_names_the_struct
  - MissingStructField.outer is None
  - Expected: outer to contain struct name for context
```

#### Issue 6: Schema Validation Loses Source Spans

The validation module works on `Value` (semantic data) not AST, so source positions are lost. Errors use logical paths instead:

```rust
// Current output
"in field 'items' -> element 1: type mismatch: expected i32, got String"

// vs rustc-like output would need
"config.ron:15:8: error: expected i32, got String"
```

---

### 3. Recommendations for Rustc-Like Errors

#### Phase 1: Fix Span Accuracy (High Priority)

1. **Fix parsing error spans** to point to actual error tokens, not parent constructs
2. **Fix EOF spans** to point to end of file, not column 1
3. **Add struct name context** to all struct-related errors

#### Phase 2: Add Error Rendering (Medium Priority)

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

Key components needed:
1. **Source snippet extraction** using span byte offsets
2. **Underline rendering** using span start/end columns
3. **Contextual notes** with suggestions
4. **Color support** (optional, for terminal)

#### Phase 3: Unified Error Type (Lower Priority)

Create a unified diagnostic type that works across:
- ron2 parsing errors
- ron-derive deserialization errors
- ron-schema validation errors

```rust
pub struct Diagnostic {
    pub severity: Severity,
    pub code: Option<String>,      // E.g., "E0308"
    pub message: String,
    pub primary_span: Span,
    pub labels: Vec<Label>,        // Additional spans with messages
    pub notes: Vec<String>,
    pub suggestions: Vec<Suggestion>,
}
```

#### Phase 4: Schema Validation with Spans (Lower Priority)

To get spans in schema validation, would need to:
1. Validate against AST (`Expr`) instead of `Value`
2. Or carry spans through Value somehow
3. Or re-parse to find span when error occurs

---

## Test Coverage Summary

### ron2 (parsing)
- 15/25 tests pass (60%)
- Main issues: span accuracy for errors

### ron-derive (deserialization)
- 26/29 tests pass (90%)
- Main issues: struct name context

### ron-schema (validation)
- Good path tracking
- No span preservation (by design)

---

## Files Added/Modified

### New Test Files
- `crates/ron2/tests/error_quality.rs` - Parsing error span tests
- `crates/ron-derive/tests/error_quality.rs` - Deserialization error tests

### Key Source Files for Error Handling
- `crates/ron2/src/error.rs` - Error types, Span, Position
- `crates/ron2/src/ast/parse.rs` - Parser error construction
- `crates/ron2/src/convert.rs` - Deserialization errors
- `crates/ron-schema/src/error.rs` - Schema error types
- `crates/ron-schema/src/validation.rs` - Validation errors

---

## Conclusion

The foundation for excellent error reporting exists:
- Spans capture line/column and byte offsets
- Error codes are specific and contextual
- Type information flows through the system

To achieve rustc-quality diagnostics, focus on:
1. **Fix span accuracy** in the parser
2. **Build a diagnostic renderer** that extracts source snippets
3. **Add helpful notes and suggestions** to common errors

The deserialization layer (ron-derive) already provides very good span accuracy - the parsing layer (ron2) needs improvement to match.
