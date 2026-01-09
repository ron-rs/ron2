//! RON2 Showcase - A simple example demonstrating the ron2 ecosystem.
//!
//! This example shows:
//! - `#[derive(Ron)]` for automatic serialization/deserialization
//! - Loading configuration from a `.ron` file
//! - Pretty-printing with customizable formatting
//! - Schema generation for editor support

use ron_derive::Ron;
use ron_schema::{FromRon, PrettyConfig, ToRon};
use std::collections::HashMap;
use std::fs;

/// Main game configuration.
#[derive(Debug, Clone, Ron)]
pub struct GameConfig {
    /// Game title displayed in the window.
    title: String,
    /// Version string.
    version: String,
    /// Window settings.
    window: WindowConfig,
    /// Graphics quality preset.
    graphics: GraphicsQuality,
    /// Audio settings.
    audio: AudioConfig,
    /// Key bindings (action -> key).
    keybindings: HashMap<String, String>,
    /// Player configuration.
    player: PlayerConfig,
}

/// Window configuration.
#[derive(Debug, Clone, Ron)]
pub struct WindowConfig {
    width: u32,
    height: u32,
    #[ron(default)]
    fullscreen: bool,
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
    master_volume: f32,
    #[ron(default = "default_volume")]
    music_volume: f32,
    #[ron(default = "default_volume")]
    effects_volume: f32,
}

fn default_volume() -> f32 {
    1.0
}

/// Player configuration.
#[derive(Debug, Clone, Ron)]
pub struct PlayerConfig {
    name: String,
    ship: ShipType,
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

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== RON2 Showcase ===\n");

    println!("1. Loading config from file...");

    let ron_content = fs::read_to_string("data/game.ron")?;
    let config: GameConfig = GameConfig::from_ron(&ron_content)?;

    println!("   Loaded: {} v{}", config.title, config.version);
    println!(
        "   Window: {}x{}",
        config.window.width, config.window.height
    );
    println!(
        "   Player: {} flying a {:?}",
        config.player.name, config.player.ship
    );

    println!("\n2. Pretty-printed RON output:");
    println!("{}", "─".repeat(50));

    let pretty = PrettyConfig::default().indent("    ").struct_names(true);

    let output = config.to_ron_pretty(&pretty)?;
    println!("{}", output);

    Ok(())
}
