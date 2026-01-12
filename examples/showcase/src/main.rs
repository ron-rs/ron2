//! RON2 Showcase - A simple example demonstrating the ron2 ecosystem.
//!
//! This example shows:
//! - `#[derive(Ron)]` for automatic serialization/deserialization
//! - Loading configuration from a `.ron` file
//! - Pretty-printing with customizable formatting
//! - Schema generation for editor support
//! - Extension attributes: `transparent`, `explicit`, implicit Some

use std::{collections::HashMap, fs};

use ron2::{
    ast::FormatConfig,
    schema::{FromRon, RonSchemaType, ToRon},
};
use ron2_derive::Ron;

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

// =============================================================================
// Extension Attribute Examples
// =============================================================================

/// Transparent newtype - serializes as just the inner value.
/// Instead of `PlayerId(42)`, this serializes as just `42`.
#[derive(Debug, Clone, PartialEq, Ron)]
#[ron(transparent)]
pub struct PlayerId(u64);

/// Score newtype - demonstrates transparent with named field.
#[derive(Debug, Clone, PartialEq, Ron)]
#[ron(transparent)]
pub struct Score {
    value: u32,
}

/// Settings with various Option handling.
#[derive(Debug, Clone, PartialEq, Ron)]
pub struct NotificationSettings {
    /// Accepts: "email", Some("email"), or None (implicit Some is default)
    email: Option<String>,

    /// Requires explicit Some(...) or None - useful for nested Options
    #[ron(explicit)]
    push_enabled: Option<Option<bool>>,

    /// Optional with default - missing = None
    #[ron(default)]
    slack_webhook: Option<String>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== RON2 Showcase ===\n");

    GameConfig::write_schema(None).unwrap();

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

    let pretty = FormatConfig::new().indent("    ");

    let output = config.to_ron_with(&pretty)?;
    println!("{}", output);

    // Demonstrate extension attributes
    println!("\n3. Extension Attributes Demo:");
    println!("{}", "─".repeat(50));

    // Transparent newtypes
    let player_id: PlayerId = PlayerId::from_ron("42")?;
    println!("   PlayerId from '42': {:?}", player_id);
    println!("   Serialized: {}", player_id.to_ron()?);

    let score: Score = Score::from_ron("1000")?;
    println!("   Score from '1000': {:?}", score);

    // Implicit Some (default behavior)
    let settings: NotificationSettings = NotificationSettings::from_ron(
        r#"(email: "user@example.com", push_enabled: Some(Some(true)))"#,
    )?;
    println!("\n   NotificationSettings with implicit Some:");
    println!("   - email: {:?}", settings.email);
    println!("   - push_enabled: {:?}", settings.push_enabled);
    println!("   - slack_webhook: {:?}", settings.slack_webhook);

    // Explicit Option - nested Options
    let settings2: NotificationSettings =
        NotificationSettings::from_ron(r#"(email: None, push_enabled: Some(None))"#)?;
    println!("\n   With Some(None) for push_enabled:");
    println!("   - push_enabled: {:?}", settings2.push_enabled);

    Ok(())
}
