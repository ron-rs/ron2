# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**ron2** is a standalone RON (Rusty Object Notation) parser and serializer that works exclusively with the `Value` type. Unlike the main `ron` crate, it does not depend on or use serde.

Key characteristics:
- Value-only: All parsing/serialization goes through the `Value` enum
- No serde dependency: Lightweight, suitable for contexts where serde isn't needed
- No-std compatible: Works without the standard library (disable `std` feature)
- Strict linting: Denies unsafe code, unwrap/expect (outside tests), and most clippy warnings

## Build Commands

```bash
cargo build                              # Build
cargo test                               # Run all tests
cargo test --features integer128         # Test with i128/u128 support
cargo test --features indexmap           # Test with order-preserving maps
cargo clippy --all-targets               # Lint
cargo fmt --check                        # Check formatting
```

## Architecture

### Core Types

**`Value`** (`src/value/mod.rs`) - Central enum representing any RON data:
```rust
pub enum Value {
    Bool(bool), Char(char), Map(Map), Number(Number),
    Option(Option<Box<Value>>), String(String),
    Bytes(Vec<u8>), Seq(Vec<Value>), Unit,
}
```

**`Number`** (`src/value/number.rs`) - Wrapper supporting all Rust numeric types with total ordering for floats.

**`Map`** (`src/value/map.rs`) - Uses `BTreeMap` by default, or `IndexMap` with the `indexmap` feature for key order preservation.

### Module Responsibilities

| Module | Purpose |
|--------|---------|
| `parse.rs` | Tokenizer and parser (largest module - ~59KB) |
| `ser.rs` | Value to RON text serialization, `PrettyConfig` |
| `de.rs` | RON text to Value deserialization |
| `error.rs` | Error types with position spans (~50 variants) |
| `options.rs` | Roundtrip configuration (extensions, recursion limit) |
| `extensions.rs` | Bitflags for optional syntax features |

### Data Flow

```
RON text → Parser → Value → Serializer → RON text
```

Parsing: `from_str()` / `from_bytes()` → `Options::from_str()` → `Deserializer::parse()`

Serialization: `to_string()` / `to_string_pretty()` → `Serializer::serialize()`

### Extensions

Controlled via `Extensions` bitflags in `Options`:
- `UNWRAP_NEWTYPES` - Auto-unwrap tuple newtypes
- `IMPLICIT_SOME` - Auto-wrap values in `Some()`
- `UNWRAP_VARIANT_NEWTYPES` - Unwrap single-field enum variants
- `EXPLICIT_STRUCT_NAMES` - Require explicit struct names

### Features

| Feature | Effect |
|---------|--------|
| `std` (default) | Enable I/O operations, readers/writers |
| `integer128` | Enable `i128`/`u128` number variants |
| `indexmap` | Use `IndexMap` for order-preserving maps |

### Error Handling

All fallible operations return `Result<T>` or `SpannedResult<T>`. `SpannedError` includes position information (line/column) for error messages.

### Recursion Protection

Default 128-level depth limit prevents stack overflow on deeply nested structures. Configure via `Options::with_recursion_limit()`.

## Code Style

- No unsafe code allowed
- Tests are inline `#[cfg(test)]` modules within each source file
- Builder pattern used for `Options` and `PrettyConfig`
- MSRV: 1.90.0
- Always update CLAUDE.md when we make a design decision! Keep it updated!
