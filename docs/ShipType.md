# ShipType

Available ship types with their stats.

## Variants

| Variant | Kind | Description |
|---------|------|-------------|
| `Scout` | Unit | Fast but fragile scout ship. |
| [`Fighter`](#fighter) | Struct | Balanced fighter with weapon slots. |
| [`Cruiser`](#cruiser) | Struct | Heavy cruiser with cargo space. |

### `Fighter`

Balanced fighter with weapon slots.

**Fields:**

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `weapon_slots` | `u8` | Yes | — |
| `shield_strength` | `f32` | Yes | — |

### `Cruiser`

Heavy cruiser with cargo space.

**Fields:**

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `cargo_capacity` | `u32` | Yes | — |

## Example

```ron
Scout
```
