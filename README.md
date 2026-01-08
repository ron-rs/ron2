## ron-extras

Extensions for RON (Rusty Object Notation):

- **ron2**: Standalone RON parser with full AST access, perfect round-trip fidelity, and zero-copy parsing (replaces the `ron` crate)
- **ron-derive**: Proc macro that generates schemas at compile time
- **ron-schema**: Core schema types, validation, and storage (like JSON schema but for RON)
- **ron-lsp**: Language server providing auto-completions for RON files

## How it works

We introduce two new attributes:

```ron
#![type = "crate::path::to::MyStruct"]
#![schema = "my_struct.schema.ron"]
```

We define a central location in the system where we store the latest schema for each type. The schema attribute overrides the location.
Our `ron-derive` can generate the schema automatically.

## Why ron2?

Unlike the main `ron` crate, ron2:
- **AST-first**: Parses to a complete AST preserving all source information
- **Perfect round-trip**: Comments, whitespace, and formatting are preserved exactly
- **Zero-copy**: Uses `Cow<'a, str>` for borrowing from source without allocation
- **No serde dependency**: Lightweight, works without serde
- **No-std compatible**: Disable `std` feature for embedded use

## Build Commands

```bash
cargo build                              # Build all crates
cargo test                               # Run all tests
cargo test -p ron2                       # Test ron2
cargo test -p ron2 --features integer128 # Test with i128/u128 support
cargo run -p ron-lsp                     # Run the LSP server
```

## License

Dual-licensed under Apache-2.0 and MIT.
