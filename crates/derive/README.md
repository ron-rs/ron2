# ron-derive

Derive macros for RON serialization, deserialization, and schema generation.

## Usage

Add to your `Cargo.toml`:

```toml
[dependencies]
ron-derive = "0.1"
ron2 = "0.1"
```

## Derive Macros

- `#[derive(Ron)]` - Implements ToRon, FromRon, and RonSchema (all three)
- `#[derive(ToRon)]` - Serialize to RON
- `#[derive(FromRon)]` - Deserialize from RON
- `#[derive(RonSchema)]` - Generate RON schema files

```rust
use ron_derive::Ron;
use ron2::{FromRon, ToRon};

#[derive(Debug, PartialEq, Ron)]
struct Config {
    name: String,
    port: u16,
    #[ron(default)]
    debug: bool,
}

fn main() {
    // Deserialize
    let config: Config = Config::from_ron(r#"(name: "app", port: 8080)"#).unwrap();

    // Serialize
    let ron = config.to_ron().unwrap();
}
```

## Container Attributes

| Attribute | Description |
|-----------|-------------|
| `rename = "Name"` | Use different name in RON |
| `rename_all = "..."` | Apply rename rule to all fields |
| `deny_unknown_fields` | Error on unknown fields |
| `transparent` | Serialize as the single inner field |

Rename rules: `camelCase`, `snake_case`, `PascalCase`, `SCREAMING_SNAKE_CASE`, `kebab-case`, `lowercase`, `UPPERCASE`

## Field Attributes

| Attribute | Description |
|-----------|-------------|
| `rename = "name"` | Use different name in RON |
| `skip` | Skip field entirely |
| `skip_serializing` | Skip during serialization |
| `skip_deserializing` | Skip during deserialization |
| `default` | Use `Default::default()` if missing |
| `default = "path"` | Use custom function if missing |
| `flatten` | Flatten nested struct into parent |
| `skip_serializing_if = "fn"` | Skip if predicate returns true |
| `explicit` | Require explicit `Some(...)` or `None` |

## Variant Attributes

| Attribute | Description |
|-----------|-------------|
| `rename = "Name"` | Use different name in RON |
| `skip` | Skip this variant |

## Extension Behavior

### Implicit Some (Default)

`Option<T>` fields accept bare values:

```rust
#[derive(FromRon)]
struct Config {
    name: Option<String>,
}
```

```ron
(name: "Alice")         // becomes Some("Alice")
(name: Some("Alice"))   // also works
(name: None)            // None
```

### Explicit Option

Use `#[ron(explicit)]` to require `Some(...)` or `None`:

```rust
#[derive(FromRon)]
struct Config {
    #[ron(explicit)]
    value: Option<Option<bool>>,
}
```

```ron
(value: Some(Some(true)))  // ok
(value: Some(None))        // ok
(value: None)              // ok
(value: true)              // ERROR - bare value not allowed
```

### Transparent Newtypes

Use `#[ron(transparent)]` to serialize as the inner type:

```rust
#[derive(FromRon, ToRon)]
#[ron(transparent)]
struct UserId(u64);

#[derive(FromRon, ToRon)]
struct User {
    id: UserId,
    name: String,
}
```

```ron
(id: 42, name: "Alice")  // UserId is just a number
```

## License

MIT OR Apache-2.0
