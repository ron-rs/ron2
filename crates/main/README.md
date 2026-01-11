# ron2

A standalone RON (Rusty Object Notation) parser with full AST access, perfect round-trip fidelity, and zero-copy parsing.

## Overview

Unlike the main `ron` crate, ron2:
- **AST-first**: Parses to a complete AST preserving all source information
- **Perfect round-trip**: Comments, whitespace, and formatting are preserved exactly
- **Zero-copy**: Uses `Cow<'a, str>` for borrowing from source without allocation
- **No serde dependency**: Lightweight, works without serde
- **No-std compatible**: Disable `std` feature for embedded use

## Two APIs: AST and Value

ron2 provides two levels of access:

### AST API (full fidelity)

```rust
use ron2::ast::{parse_document, serialize_document};

let source = r#"
// Game settings
GameConfig(
    window_size: (800, 600),
    fullscreen: false,
)
"#;

// Parse to AST - preserves everything
let doc = parse_document(source)?;

// Access spans, comments, whitespace
for attr in &doc.attributes {
    println!("Attribute at {:?}", attr.span);
}

// Serialize back - identical to input
let output = serialize_document(&doc)?;
assert_eq!(source, output);
```

### Value API (semantic only)

```rust
use ron2::{from_str, to_string, Value};

// Parse to Value - discards formatting
let value: Value = from_str("(name: \"example\", count: 42)")?;

// Serialize with configurable formatting
let text = to_string(&value)?;
```

## What the AST Preserves

| Information | AST | Value |
|-------------|-----|-------|
| Comments | Yes | No |
| Whitespace | Yes | No |
| Span/position | Yes | No |
| Raw number format (`0xFF`, `1_000`) | Yes | No |
| String kind (raw vs regular) | Yes | No |
| Round-trip fidelity | 100% | N/A |

## RON Syntax

```ron
GameConfig( // optional struct name
    window_size: (800, 600),
    window_title: "PAC-MAN",
    fullscreen: false,

    mouse_sensitivity: 1.4,
    key_bindings: {
        "up": Up,
        "down": Down,
    },
)
```

### Supported Types

* Numbers: `42`, `3.14`, `0xFF`, `0b0110`, `1_000`
* Strings: `"Hello"`, `"with\\escapes\n"`, `r#"raw string"#`
* Byte Strings: `b"Hello"`, `br#"raw"#`
* Booleans: `true`, `false`
* Chars: `'e'`, `'\n'`
* Optionals: `Some("string")`, `None`
* Tuples: `("abc", 1.23, true)`, `()`
* Lists: `["abc", "def"]`
* Structs: `( foo: 1.0, bar: ( baz: "nested" ) )`
* Maps: `{ "arbitrary": "keys" }`

## Features

| Feature | Effect |
|---------|--------|
| `std` (default) | Enable I/O operations, order-preserving maps via `IndexMap` |
| `integer128` | Enable `i128`/`u128` support |

Note: `IndexMap` is always available as a dependency. With `std` enabled, the internal `Map` type uses `IndexMap` to preserve insertion order. Without `std`, it falls back to `BTreeMap` (sorted order).

## Configuration

### PrettyConfig (serialization)

Control output formatting when serializing Values:
- `indent` - Indentation string (default: 4 spaces)
- `new_line` - Newline string
- `separator` - Item separator
- `struct_names` - Include struct names in output
- `integer_type_suffix` / `float_type_suffix` - Add type suffixes like `42i32`

### Options

- `recursion_limit` - Max nesting depth during serialization (default: 128)

Note: The `default_extensions` field exists for API compatibility but extensions like `UNWRAP_NEWTYPES` and `IMPLICIT_SOME` require type information from serde and have no effect when using ron2's Value or AST APIs directly.

## License

Dual-licensed under Apache-2.0 and MIT.
