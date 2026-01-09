# RON2 Showcase

A simple example demonstrating the ron2 ecosystem.

## Features Demonstrated

- `#[derive(Ron)]` for automatic serialization/deserialization
- Loading configuration from a `.ron` file
- Pretty-printing with customizable formatting
- Schema generation for LSP/editor support

## Running

```bash
cargo run -p ron-showcase
```

## Schema Output Configuration

Schemas are written to a directory configured via the `RON_SCHEMA_DIR` environment variable. This example sets it in `.cargo/config.toml`:

```toml
[env]
RON_SCHEMA_DIR = { value = "schemas/", relative = true }
```

This ensures **all types** (not just the root type) write their schemas to the same directory.

### Alternative: Per-Type Attribute

You can also set the output directory per-type:

```rust
#[derive(Ron)]
#[ron(output = "schemas/")]
pub struct MyConfig { ... }
```

However, this only affects that specific type. Child types won't inherit the setting, so using `RON_SCHEMA_DIR` is recommended for projects with nested types.

### Resolution Order

Schema output location is resolved in this order:

1. `#[ron(output = "path/")]` attribute (highest priority)
2. `RON_SCHEMA_DIR` environment variable
3. `~/.local/share/ron-schemas/` (XDG default)

## Files

```
showcase/
├── .cargo/
│   └── config.toml      # Sets RON_SCHEMA_DIR
├── data/
│   └── game.ron         # Sample configuration file
├── schemas/             # Generated schema files (after running)
│   └── ron_showcase/
│       ├── GameConfig.schema.ron
│       ├── WindowConfig.schema.ron
│       └── ...
└── src/
    └── main.rs          # Example code
```
