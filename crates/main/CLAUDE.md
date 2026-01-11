# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

See [README.md](README.md) for project overview, APIs, syntax reference, and features.

## Build Commands

```bash
cargo build                              # Build
cargo test                               # Run all tests
cargo test --features integer128         # Test with i128/u128 support
cargo clippy --all-targets               # Lint
cargo fmt --check                        # Check formatting
```

## Architecture

### Data Flow

```
RON text → Lexer → Tokens (with trivia)
                      ↓
              AstParser → Document<'a> (complete AST)
                 ├─→ serialize_document() → RON text (perfect round-trip)
                 └─→ to_value() → Value (semantic only)
                                     ↓
                             Serializer → RON text (regenerated)
```

### Module Structure

| Module | Purpose |
|--------|---------|
| `ast/mod.rs` | AST type definitions (Document, Expr, Trivia, Span) |
| `ast/parse.rs` | Tokenizer integration and AST parser |
| `ast/convert.rs` | AST-to-Value conversion |
| `ast/ser.rs` | AST serialization with format preservation |
| `value/mod.rs` | Value enum and operations |
| `value/number.rs` | Number wrapper with total float ordering |
| `value/map.rs` | Map type (BTreeMap or IndexMap) |
| `parse.rs` | Direct Value parser (convenience API) |
| `ser.rs` | Value serialization, PrettyConfig |
| `de.rs` | Value deserialization |
| `error.rs` | Error types with position spans |
| `options.rs` | Extensions, recursion limit |

### Key AST Types

**Document** - Root of parsed RON file:
```rust
pub struct Document<'a> {
    pub source: Cow<'a, str>,           // Original source
    pub leading: Trivia<'a>,            // Leading whitespace/comments
    pub attributes: Vec<Attribute<'a>>, // Inner attributes (#![...])
    pub value: Option<Expr<'a>>,        // Main value
    pub trailing: Trivia<'a>,           // Trailing trivia
}
```

**Expr** - All RON value types:
```rust
pub enum Expr<'a> {
    Unit(UnitExpr), Bool(BoolExpr), Char(CharExpr<'a>),
    Number(NumberExpr<'a>), String(StringExpr<'a>),
    Bytes(BytesExpr<'a>), Option(Box<OptionExpr<'a>>),
    Seq(SeqExpr<'a>), Map(MapExpr<'a>), Tuple(TupleExpr<'a>),
    Struct(StructExpr<'a>),
}
```

**Trivia** - Whitespace and comments:
```rust
pub struct Trivia<'a> {
    pub span: Option<Span>,
    pub whitespace: Cow<'a, str>,
    pub comments: Vec<Comment<'a>>,
}
```

**Span** - Source positions:
```rust
pub struct Span {
    pub start: Position,      // Line/column (1-indexed)
    pub end: Position,
    pub start_offset: usize,  // Byte offset
    pub end_offset: usize,
}
```

### Zero-Copy Design

All AST types use `Cow<'a, str>`:
- Parsing borrows from source (`Cow::Borrowed`)
- Call `.into_owned()` for `'static` lifetime
- Serializer works with both borrowed and owned

### Value Type

```rust
pub enum Value {
    Bool(bool), Char(char), Map(Map), Number(Number),
    Option(Option<Box<Value>>), String(String),
    Bytes(Vec<u8>), Seq(Vec<Value>), Unit,
}
```

### Error Handling

All fallible operations return `Result<T>` or `SpannedResult<T>`. `SpannedError` includes line/column for diagnostics.

### Recursion Protection

Default 128-level depth limit during serialization. Configure via `Options::with_recursion_limit()`.

### Options and Extensions

`Options` has two fields:
- `recursion_limit` - Used during serialization only
- `default_extensions` - Defined for API compatibility

The extensions (`UNWRAP_NEWTYPES`, `IMPLICIT_SOME`, etc.) are serde-level semantic features requiring type information. They have no effect in ron2 since it operates without serde.

## Code Style

- No unsafe code allowed
- Strict linting: denies unwrap/expect outside tests
- Tests are inline `#[cfg(test)]` modules
- Builder pattern for `Options` and `PrettyConfig`
- MSRV: 1.90.0

## Design Notes

Update this section when making significant design decisions.
