# GameConfig

Main game configuration.

## Fields

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `title` | `String` | Yes | Game title displayed in the window. |
| `version` | `String` | Yes | Version string. |
| `window` | [`WindowConfig`](./WindowConfig.md) | Yes | Window settings. |
| `graphics` | [`GraphicsQuality`](./GraphicsQuality.md) | Yes | Graphics quality preset. |
| `audio` | [`AudioConfig`](./AudioConfig.md) | Yes | Audio settings. |
| `keybindings` | Map<`String`, `String`> | Yes | Key bindings (action -> key). |
| `player` | [`PlayerConfig`](./PlayerConfig.md) | Yes | Player configuration. |

## Example

```ron
(
    title: "...",
    version: "...",
    window: /* WindowConfig */,
    graphics: /* GraphicsQuality */,
    audio: /* AudioConfig */,
    keybindings: { "...": "..." },
    player: /* PlayerConfig */,
)
```
