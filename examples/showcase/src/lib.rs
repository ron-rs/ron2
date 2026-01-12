use std::collections::HashMap;

use ron2_derive::Ron;

/// Main game configuration.
#[derive(Debug, Clone, Ron)]
pub struct GameConfig {
    /// Game title displayed in the window.
    pub title: String,
    /// Version string.
    pub version: String,
    /// Window settings.
    pub window: WindowConfig,
    /// Graphics quality preset.
    pub graphics: GraphicsQuality,
    /// Audio settings.
    pub audio: AudioConfig,
    /// Key bindings (action -> key).
    pub keybindings: HashMap<String, String>,
    /// Player configuration.
    pub player: PlayerConfig,
}

/// Window configuration.
#[derive(Debug, Clone, Ron)]
pub struct WindowConfig {
    pub width: u32,
    pub height: u32,
    #[ron(default)]
    pub fullscreen: bool,
}

/// Graphics quality presets.
#[derive(Debug, Clone, Ron)]
pub enum GraphicsQuality {
    Low,
    Medium,
    High,
    Ultra,
}

/// Audio configuration with volume controls.
#[derive(Debug, Clone, Ron)]
pub struct AudioConfig {
    pub master_volume: f32,
    #[ron(default = "default_volume")]
    pub music_volume: f32,
    #[ron(default = "default_volume")]
    pub effects_volume: f32,
}

fn default_volume() -> f32 {
    1.0
}

/// Player configuration.
#[derive(Debug, Clone, Ron)]
pub struct PlayerConfig {
    pub name: String,
    pub ship: ShipType,
}

/// Available ship types with their stats.
#[derive(Debug, Clone, Ron)]
pub enum ShipType {
    /// Fast but fragile scout ship.
    Scout,
    /// Balanced fighter with weapon slots.
    Fighter {
        weapon_slots: u8,
        shield_strength: f32,
    },
    /// Heavy cruiser with cargo space.
    Cruiser { cargo_capacity: u32 },
}
