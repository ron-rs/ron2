# Error Consolidation Design: `ron-error` Crate

This document describes the proposed restructuring of error types across the ron-extras workspace to unify validation errors and add span information throughout.

## Problem Statement

Currently, validation errors are duplicated across crates:

| Purpose | ron2::Error | ron_schema::SchemaErrorKind |
|---------|-------------|----------------------------|
| Type mismatch | `InvalidValueForType { expected, found }` | `TypeMismatch { expected, actual }` |
| Missing field | `MissingStructField { field, outer }` | `MissingField(String)` |
| Unknown field | `NoSuchStructField { expected, found, outer }` | `UnknownField(String)` |
| Unknown variant | `NoSuchEnumVariant { expected, found, outer }` | `UnknownVariant(String)` |
| Length mismatch | `ExpectedDifferentLength { expected, found }` | `TupleLengthMismatch { expected, actual }` |
| Duplicate field | `DuplicateStructField { field, outer }` | *(missing)* |

Issues:
1. **Duplication** - Similar error types defined twice
2. **No spans in ron-schema** - Validation errors lack source location info
3. **Inconsistent context** - ron2 uses `outer` field, ron-schema uses `Vec<PathSegment>`

## Design Review Notes

Key decisions from review:
- **Keep `outer` naming** - More intuitive than `container`, matches existing ron2 convention
- **Use push + reverse for paths** - O(1) append instead of O(n) prepend; reverse on display
- **Keep SpannedError separate** - Wraps any error with span; ValidationError is validation-specific
- **Require alloc** - `Vec<PathSegment>`, `Cow`, `String` need alloc; feature-gate appropriately
- **Fix ron2 no_std** - Currently broken; fix as part of this migration

## Proposed Solution: `ron-error` Crate

Create a new `ron-error` crate containing shared types used across all crates.

### New Dependency Graph

```
ron-error (new)
  - Span, Position
  - ValidationError, ValidationErrorKind, PathSegment
  - no_std compatible, zero dependencies
       ↑
ron2 → ron-error
  - AST types, Parser
  - Value, Number, Map
  - Serialization
  - FromRon/ToRon + AstMapAccess
  - Re-exports ron-error types
       ↑
ron-schema → ron-error, ron2
  - Schema types (Schema, TypeKind, Field, Variant)
  - Validation (returns ValidationError)
  - Storage (separate StorageError)
       ↑
ron-derive → ron-schema, ron2
  - Generated code uses ron_error::ValidationError
```

### Benefits

1. **Single source of truth** - One `ValidationError` type used everywhere
2. **Span + path context unified** - Both available in every validation error
3. **Clean separation** - Parsing errors vs validation errors vs storage errors
4. **no_std compatible** - `ron-error` has zero dependencies
5. **Smaller ron-schema** - No need for custom error types

## Detailed Design

### ron-error Crate Structure

```
crates/ron-error/
├── Cargo.toml
└── src/
    ├── lib.rs
    ├── span.rs      # Span, Position
    ├── path.rs      # PathSegment
    └── error.rs     # ValidationError, ValidationErrorKind
```

### Cargo.toml

```toml
[package]
name = "ron-error"
version.workspace = true
edition.workspace = true
license.workspace = true
description = "Error types for RON (Rusty Object Notation) - spans, validation errors, path context"
rust-version = "1.90.0"

[features]
default = ["alloc"]
alloc = []  # Enables Vec, String, Cow - required for ValidationError
std = ["alloc"]  # Enables std::error::Error impl

[dependencies]
# Zero external dependencies
```

### lib.rs

```rust
// crates/ron-error/src/lib.rs
#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(feature = "alloc")]
extern crate alloc;

mod span;
mod path;
mod error;

pub use span::{Span, Position};

#[cfg(feature = "alloc")]
pub use path::PathSegment;

#[cfg(feature = "alloc")]
pub use error::{ValidationError, ValidationErrorKind};
```

### Type Definitions

#### Span and Position (moved from ron2)

```rust
// crates/ron-error/src/span.rs

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct Position {
    pub line: usize,
    pub col: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct Span {
    pub start: Position,
    pub end: Position,
    pub start_offset: usize,
    pub end_offset: usize,
}

impl Span {
    pub const fn synthetic() -> Self;
    pub const fn is_synthetic(&self) -> bool;
    pub fn slice<'a>(&self, source: &'a str) -> &'a str;
}
```

#### PathSegment (moved from ron-schema)

```rust
// crates/ron-error/src/path.rs

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PathSegment {
    Field(String),
    Element(usize),
    MapKey,
    MapValue(String),
    Variant(String),
    TypeRef(String),
}
```

#### ValidationError (unified)

```rust
// crates/ron-error/src/error.rs

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValidationErrorKind {
    /// Expected one type, found another
    TypeMismatch {
        expected: String,
        found: String,
    },

    /// Missing required struct field
    MissingField {
        field: Cow<'static, str>,
        outer: Option<Cow<'static, str>>,  // containing struct/enum name
    },

    /// Unknown struct field
    UnknownField {
        field: Cow<'static, str>,
        expected: &'static [&'static str],
        outer: Option<Cow<'static, str>>,
    },

    /// Unknown enum variant
    UnknownVariant {
        variant: Cow<'static, str>,
        expected: &'static [&'static str],
        outer: Option<Cow<'static, str>>,
    },

    /// Duplicate struct field
    DuplicateField {
        field: Cow<'static, str>,
        outer: Option<Cow<'static, str>>,
    },

    /// Wrong number of elements (tuple, array, etc.)
    LengthMismatch {
        expected: usize,
        found: usize,
        context: Option<&'static str>,
    },

    /// Integer out of bounds for target type
    IntegerOutOfBounds {
        value: Cow<'static, str>,
        target_type: &'static str,
    },
}

#[derive(Debug, Clone)]
pub struct ValidationError {
    pub kind: ValidationErrorKind,
    pub span: Option<Span>,
    /// Path segments from innermost to outermost (reversed on display)
    pub path: Vec<PathSegment>,
}

impl ValidationError {
    // Constructors
    pub fn new(kind: ValidationErrorKind) -> Self;
    pub fn with_span(kind: ValidationErrorKind, span: Span) -> Self;

    // Context builders - append to path (O(1), reversed on display)
    pub fn in_field(mut self, name: impl Into<String>) -> Self {
        self.path.push(PathSegment::Field(name.into()));
        self
    }
    pub fn in_element(mut self, index: usize) -> Self {
        self.path.push(PathSegment::Element(index));
        self
    }
    pub fn in_variant(mut self, name: impl Into<String>) -> Self {
        self.path.push(PathSegment::Variant(name.into()));
        self
    }
    pub fn in_type_ref(mut self, path: impl Into<String>) -> Self {
        self.path.push(PathSegment::TypeRef(path.into()));
        self
    }
    pub fn in_map_key(mut self) -> Self {
        self.path.push(PathSegment::MapKey);
        self
    }
    pub fn in_map_value(mut self, key: impl Into<String>) -> Self {
        self.path.push(PathSegment::MapValue(key.into()));
        self
    }

    // Convenience constructors
    pub fn type_mismatch(expected: impl Into<String>, found: impl Into<String>) -> Self;
    pub fn missing_field(field: impl Into<Cow<'static, str>>) -> Self;
    pub fn unknown_field(field: impl Into<Cow<'static, str>>, expected: &'static [&'static str]) -> Self;
    pub fn unknown_variant(variant: impl Into<Cow<'static, str>>, expected: &'static [&'static str]) -> Self;
    pub fn duplicate_field(field: impl Into<Cow<'static, str>>) -> Self;
    pub fn length_mismatch(expected: usize, found: usize) -> Self;
}

impl fmt::Display for ValidationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Span prefix
        if let Some(ref span) = self.span {
            if !span.is_synthetic() {
                write!(f, "{}: ", span)?;
            }
        }

        // Path context (reversed - outermost first)
        if !self.path.is_empty() {
            write!(f, "in ")?;
            for (i, seg) in self.path.iter().rev().enumerate() {
                if i > 0 {
                    write!(f, " -> ")?;
                }
                write!(f, "{seg}")?;
            }
            write!(f, ": ")?;
        }

        write!(f, "{}", self.kind)
    }
}
```

### Changes to Existing Crates

#### ron2

```rust
// crates/ron2/src/lib.rs

// Re-export core types at crate root for user convenience
pub use ron_error::{Span, Position, ValidationError, ValidationErrorKind, PathSegment};
```

```rust
// crates/ron2/src/error.rs

// Re-export core types in error module too
pub use ron_error::{Span, Position, ValidationError, ValidationErrorKind, PathSegment};

/// Parsing-specific errors (lexer, parser)
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ParseError {
    Eof,
    ExpectedArray,
    ExpectedArrayEnd,
    ExpectedBoolean,
    ExpectedComma { context: Option<&'static str> },
    ExpectedChar,
    ExpectedFloat,
    ExpectedInteger,
    ExpectedMap,
    ExpectedMapEnd,
    ExpectedString,
    ExpectedIdentifier,
    InvalidEscape(&'static str),
    UnclosedBlockComment,
    UnexpectedChar(char),
    Utf8Error(core::str::Utf8Error),
    TrailingCharacters,
    ExceededRecursionLimit,
    // ... other parsing-specific errors
}

/// All ron2 errors
#[derive(Clone, Debug)]
pub enum Error {
    Parse(ParseError),
    Validation(ValidationError),
    Io(String),
    Fmt,
}

/// Error with source location
#[derive(Clone, Debug)]
pub struct SpannedError {
    pub error: Error,
    pub span: Span,
}

impl From<ValidationError> for SpannedError {
    fn from(e: ValidationError) -> Self {
        let span = e.span.clone().unwrap_or_else(Span::synthetic);
        Self { error: Error::Validation(e), span }
    }
}

pub type Result<T> = core::result::Result<T, Error>;
pub type SpannedResult<T> = core::result::Result<T, SpannedError>;
```

#### ron-schema

```rust
// crates/ron-schema/src/error.rs

// Re-export - ValidationError IS the validation error type
pub use ron_error::{ValidationError, ValidationErrorKind, PathSegment, Span};

/// Storage-specific errors
#[derive(Debug, Clone)]
pub enum StorageError {
    Io(String),
    Parse(String),
    NoSchemaDir,
    SchemaNotFound(String),
}

/// Combined error for schema operations
#[derive(Debug, Clone)]
pub enum SchemaError {
    Storage(StorageError),
    Validation(ValidationError),
}

impl From<ValidationError> for SchemaError {
    fn from(e: ValidationError) -> Self {
        Self::Validation(e)
    }
}

impl From<StorageError> for SchemaError {
    fn from(e: StorageError) -> Self {
        Self::Storage(e)
    }
}

// For validation-only functions
pub type ValidationResult<T> = Result<T, ValidationError>;
```

#### ron-derive Generated Code

**Before:**
```rust
Err(::ron2::error::SpannedError {
    code: ::ron2::error::Error::InvalidValueForType {
        expected: format!("struct {}", expected_name),
        found: format!("struct {}", found_name),
    },
    span: s.name.span.clone(),
})
```

**After:**
```rust
Err(::ron_error::ValidationError::with_span(
    ::ron_error::ValidationErrorKind::TypeMismatch {
        expected: format!("struct {}", expected_name),
        found: format!("struct {}", found_name),
    },
    s.name.span.clone(),
).into())
```

**Missing field - Before:**
```rust
Err(SpannedError {
    code: Error::MissingStructField {
        field: Cow::Borrowed(name),
        outer: self.struct_name.map(|s| Cow::Owned(s.to_string())),
    },
    span: self.struct_span.clone(),
})
```

**After:**
```rust
Err(::ron_error::ValidationError::with_span(
    ::ron_error::ValidationErrorKind::MissingField {
        field: ::std::borrow::Cow::Borrowed(name),
        outer: self.struct_name.map(|s| ::std::borrow::Cow::Owned(s.to_string())),
    },
    self.struct_span.clone(),
).into())
```

**Unknown variant - Before:**
```rust
Err(::ron2::error::SpannedError {
    code: ::ron2::error::Error::NoSuchEnumVariant {
        expected: &[#(#variant_names),*],
        found: ::std::borrow::Cow::Owned(unknown.to_string()),
        outer: Some(::std::borrow::Cow::Borrowed(#enum_name)),
    },
    span: s.name.span.clone(),
})
```

**After:**
```rust
Err(::ron_error::ValidationError::with_span(
    ::ron_error::ValidationErrorKind::UnknownVariant {
        variant: ::std::borrow::Cow::Owned(unknown.to_string()),
        expected: &[#(#variant_names),*],
        outer: Some(::std::borrow::Cow::Borrowed(#enum_name)),
    },
    s.name.span.clone(),
).into())
```

## Migration Steps

1. **Create ron-error crate**
   - Add to workspace
   - Implement Span, Position, PathSegment, ValidationError
   - Add tests

2. **Update ron2**
   - Add ron-error dependency
   - Move Span/Position to re-exports
   - Split Error into ParseError + ValidationError
   - Update SpannedError
   - Update all error construction sites
   - Update tests

3. **Update ron-schema**
   - Add ron-error dependency
   - Remove duplicate error types
   - Simplify to StorageError + ValidationError
   - Update validation.rs to use ValidationError directly
   - Update tests

4. **Update ron-derive**
   - Update generated code templates
   - Use ron_error::ValidationError
   - Update tests

5. **Update ron-lsp and ronfmt**
   - Adapt to new error types
   - Should be minimal changes

## Error Message Format

The unified `ValidationError::Display` format:

```
// With span and path:
3:15: in field 'items' -> element 0: expected i32, found String

// With path only (no span):
in field 'config' -> field 'port': missing required field `port`

// Simple (no path, no span):
expected bool, found number
```

## Open Questions

1. **Should `FromRon::from_ast` return `ValidationError` directly?**
   - Pro: Cleaner, no conversion needed
   - Con: Changes trait signature
   - **Decision:** TBD - evaluate during implementation

2. ~~**Should we keep `SpannedError` or just use `ValidationError`?**~~
   - **Decision:** Keep both. `SpannedError` wraps any error (parse or validation) with span.
     `ValidationError` is specifically for type validation with path context.

3. ~~**Naming: `ron-core` vs `ron-types` vs `ron-error`?**~~
   - **Decision:** `ron-error` - focused name, clear purpose

## Design Decisions

Decisions made during review:

| Question | Decision | Rationale |
|----------|----------|-----------|
| Field naming | Keep `outer` | More intuitive than `container`, matches existing code |
| Path building | Use `push` + reverse on display | O(1) vs O(n) for each context addition |
| Error separation | Keep `SpannedError` | Different purpose: wraps any error with span |
| no_std support | Require `alloc` feature | `Vec`, `Cow`, `String` need allocation |
| Crate naming | `ron-error` | Focused name, clear purpose |
| Path segments | Keep `Vec<PathSegment>` | Zero cost on happy path; useful for tooling (LSP quick-fixes) |

## Alternatives Considered

### Keep Separate Error Types
- Simpler implementation
- But duplication remains, no spans in schema validation
- **Rejected:** Consolidation provides better consistency

### Add Span to SchemaError Only
- Less invasive
- But still duplicated error kinds
- **Rejected:** Doesn't solve duplication

### Full Validation in ron-schema
- Move all validation logic to ron-schema
- ron-derive generates calls to ron-schema validation
- More coupling, larger dependency for simple derives
- **Rejected:** Over-engineering for current needs
