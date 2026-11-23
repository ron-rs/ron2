//! RON2 Showcase - A simple example demonstrating the ron2 ecosystem.
//!
//! This example shows:
//! - `#[derive(Ron)]` for automatic serialization/deserialization
//! - Loading configuration from a `.ron` file
//! - Validating against generated schemas
//! - Pretty-printing with customizable formatting
//! - Schema-driven serialization output

use std::{fs, path::Path};

use example_showcase::GameConfig;
use ron2::{
    FromRon, ToRon,
    ast::parse_document,
    fmt::FormatConfig,
    schema::{RonSchema, collect::collect_schemas, validate},
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== RON2 Showcase ===\n");

    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("data/game.ron");
    let ron_content = fs::read_to_string(&path)?;
    let document = parse_document(&ron_content)?;
    let value = document.value.as_ref().ok_or("empty document")?;

    println!("1. Loading and validating config...");

    // The catalog holds GameConfig's schema and every schema it references.
    let catalog = collect_schemas::<GameConfig>()?;
    let root_path = GameConfig::type_path().ok_or("missing GameConfig type path")?;
    let root_schema = catalog
        .schemas
        .get(root_path)
        .ok_or("missing GameConfig schema")?;

    validate(value, root_schema, &catalog)?;

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

    // don't do this at home
    let pretty = FormatConfig::new().indent("\t");
    let output = config.to_ron_with(&pretty)?;
    println!("{}", output);

    Ok(())
}
