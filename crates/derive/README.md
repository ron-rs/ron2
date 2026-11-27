# ron2-derive

Derive macros for RON serialization, deserialization, and schema generation.

> **Note:** These macros are re-exported from `ron2` with the default `derive` feature.
> You only need this crate directly if you want to use the macros without the rest of `ron2`.

## Usage

Add to your `Cargo.toml`:

```toml
[dependencies]
ron2 = "0.3"  # Includes derive macros by default
```

Or for standalone use:

```toml
[dependencies]
ron2-derive = "0.4"
ron2 = { version = "0.4", default-features = false }
```

## Derive Macros

- `#[derive(Ron)]` - Implements ToRon, FromRon, and RonSchema (all three)
- `#[derive(ToRon)]` - Serialize to RON
- `#[derive(FromRon)]` - Deserialize from RON
- `#[derive(RonSchema)]` - Describe the type for schema files (written with `write_schemas`), validation and the LSP

Unknown, misplaced or conflicting `#[ron(...)]` attributes are compile errors.

```rust
use ron2_derive::Ron;
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
| `rename = "Name"` | Struct name in RON (structs only) |
| `rename_all = "..."` | Rename the fields of a struct, or the variants of an enum |
| `deny_unknown_fields` | Error on unknown fields |
| `transparent` | Serialize as the single non-skipped field |
| `bound = "T: Trait"` | Replace the inferred where-clause (also `bound(ser = "..", de = "..", schema = "..")`) |

Rename rules: `camelCase`, `snake_case`, `PascalCase`, `SCREAMING_SNAKE_CASE`, `lowercase`, `UPPERCASE`

## Field Attributes

Named fields only; tuple fields allow just `skip` and `skip, default = "path"`.

| Attribute | Description |
|-----------|-------------|
| `rename = "name"` | Use different name in RON |
| `skip` | Skip field entirely |
| `skip_serializing` | Skip during serialization |
| `skip_deserializing` | Skip during deserialization |
| `default` | Use `Default::default()` if missing |
| `default = "path"` | Use custom function if missing (or skipped) |
| `flatten` | Flatten nested struct (or `Option`, or map as catch-all) into parent |
| `skip_serializing_if = "fn"` | Skip if predicate returns true |
| `explicit` | Require explicit `Some(...)` or `None` |
| `implicit_some` | Write `Some(x)` as bare `x` (`Option` fields) |
| `opt` | Default if missing, skip if equals default |

## Variant Attributes

| Attribute | Description |
|-----------|-------------|
| `rename = "Name"` | Use different name in RON |
| `rename_all = "..."` | Rename the fields of this struct variant |
| `skip` | Never deserialize this variant; serializing it is an error |

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
(value: Some(true))        // ok - only the outer Option is explicit
(value: Some(None))        // ok
(value: None)              // ok
(value: true)              // ERROR - bare value not allowed
```

### Writing Bare Values (`#[ron(implicit_some)]`)

`ToRon` writes `Some(x)` as `Some(x)` by default. With `#[ron(implicit_some)]`
it writes bare `x`. Reading is unchanged (both forms are accepted), and `None`
is still written unless `opt` or `skip_serializing_if` skips it. This suits a
field that changed from `T` to `Option<T>`: with `opt`, existing files keep
their text.

```rust
#[derive(Ron, Default, PartialEq)]
struct Style {
    #[ron(opt, implicit_some)]
    font_size: Option<u32>,
}
```

```ron
(font_size: 12)  // Some(12)
()               // None
```

The wrapper is kept where the bare value would read back as an option: an
inner option (`Some(None)`, `Some(Some(1))`) or a struct named `None` or
`Some`. The attribute requires an `Option<T>` field and cannot be combined
with `explicit`, `skip`, `skip_serializing` or `flatten`.

### Flattened Option Fields

Flattened `Option<T>` fields are presence-based: if none of `T`'s fields are
present in the parent struct, the value is `None`. If any inner field is
present, the value is `Some(T)` (missing inner fields use their defaults,
or are errors if they have none).
This means `None` and `Some(T::default())` are indistinguishable in input
when `T` has defaults. Use a non-flattened `Option<T>` or an explicit marker
field if you need to preserve that distinction.

```rust
#[derive(Ron)]
struct Inner {
    #[ron(default)]
    x: i32,
    #[ron(default)]
    y: i32,
}

#[derive(Ron)]
struct Outer {
    name: String,
    #[ron(flatten)]
    inner: Option<Inner>,
}
```

```ron
(name: "none")        // inner = None
(name: "some", x: 1)  // inner = Some(Inner { x: 1, y: 0 })
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

### Optional Fields (`#[ron(opt)]`)

Use `#[ron(opt)]` for fields that have sensible defaults and should be omitted from output when equal to that default. This combines `default` with skip-if-default serialization:

```rust
#[derive(Ron, Default, PartialEq)]
struct Config {
    name: String,
    #[ron(opt)]
    count: i32,      // Omitted when 0
    #[ron(opt)]
    enabled: bool,   // Omitted when false
}
```

```ron
// Input with defaults omitted:
(name: "app")

// Output when count=0, enabled=false:
Config(name: "app")

// Output when count=5, enabled=true:
Config(name: "app", count: 5, enabled: true)
```

Requires the field type to implement `Default + PartialEq`.

## License

MIT OR Apache-2.0
