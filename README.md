# ron-extras

[![License](https://img.shields.io/badge/license-MIT%2FApache--2.0-blue.svg)](LICENSE)
[![MSRV](https://img.shields.io/badge/MSRV-1.90.0-blue.svg)]()

A RON parser with full AST access, schema-driven LSP completions, and formatting tools.

## Quick Start

```toml
[dependencies]
ron2 = "0.1"
ron2-derive = "0.1"
```

```rust
use ron2_derive::Ron;

#[derive(Ron, Debug)]
struct Config {
    /// Server port
    port: u16,
    /// Optional hostname
    host: Option<String>,
}

let config: Config = ron2::from_str("(port: 8080, host: \"localhost\")")?;
println!("{:?}", config);
```

## Crates

| Crate | Description |
|-------|-------------|
| `ron2` | Core parser with AST and Value APIs |
| `ron2-derive` | Derive macros for `FromRon`, `ToRon`, and schema generation |
| `ron2-lsp` | Language server with completions and diagnostics |
| `ron2-doc` | Documentation generator from schema files |
| `ron2-cli` | CLI tools (`ron fmt`, `ron doc`) |

## Why ron2?

- **AST-first**: Parses to a complete AST preserving all source information
- **Perfect round-trip**: Comments, whitespace, and formatting preserved exactly
- **Rich errors**: Type mismatch errors include precise source locations
- **Full RON data model**: Custom derive avoids [serde limitations]

Best for human-edited config files or building custom DSLs.

[serde limitations]: https://github.com/ron-rs/ron?tab=readme-ov-file#limitations

## Schema & LSP

ron2 generates schemas at compile time from your Rust types. The LSP uses these schemas for completions and validation.

Add a type attribute to your RON files to enable editor support:

```ron
#![type = "my_crate::config::Config"]

(
    port: 8080,
    host: "localhost",
)
```

The LSP will provide field name completions, type validation, and documentation on hover.

## CLI Tools

Format RON files:

```bash
ron fmt config.ron              # Format in place
ron fmt --check config.ron      # Check formatting (for CI)
```

Generate documentation from schemas:

```bash
ron doc ./schemas -o ./docs
```

## When NOT to Use ron2

**Don't use ron2 if:**

- You need serde compatibility (your types only implement Serialize/Deserialize)
- You're parsing large data volumes (ron2 prioritizes fidelity over speed)

**Alternatives:**

- **[ron](https://github.com/ron-rs/ron)** — Mature, fast, serde-based
- **[nanoserde](https://github.com/not-fl3/nanoserde)** — Minimal dependencies, fast compile

## Editor Setup

See [EDITOR_SETUP.md](EDITOR_SETUP.md) for Helix, VS Code, and other editor integrations.

## Acknowledgments & License

Derived from [ron](https://github.com/ron-rs/ron). Dual-licensed under Apache-2.0 and MIT.
