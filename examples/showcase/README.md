# RON2 Showcase

A simple example demonstrating the ron2 ecosystem.

## Features Demonstrated

- `#[derive(Ron)]` for automatic serialization/deserialization
- Loading configuration from a `.ron` file
- Validating against generated schemas
- Pretty-printing with customizable formatting
- Recursive schema generation for editor support

## Running

```bash
cargo run -p ron-showcase
cargo run -p ron-showcase --bin write_schemas
```

## Schema Output

The schema writer binary uses `GameConfig::write_schemas(Some("schemas"))` to
emit all schemas into `./schemas/` in the working directory.

## Files

```
showcase/
├── data/
│   └── game.ron         # Sample configuration file
├── schemas/             # Generated schema files (after running)
│   └── ron_showcase/
│       ├── GameConfig.schema.ron
│       ├── WindowConfig.schema.ron
│       └── ...
└── src/
    ├── bin/
    │   └── write_schemas.rs
    ├── lib.rs
    └── main.rs
```
