//! RON2 Showcase - A simple example demonstrating the ron2 ecosystem.
//!
//! This example shows:
//! - `#[derive(Ron)]` for automatic serialization/deserialization
//! - Loading configuration from a `.ron` file
//! - Validating against generated schemas
//! - Pretty-printing with customizable formatting
//! - Schema-driven serialization output

use std::{collections::BTreeMap, fs};

use ron2::{
    ast::FormatConfig,
    schema::{
        collect_schemas, validate_with_resolver, FromRon, RonSchemaType, Schema, SchemaResolver,
        ToRon,
    },
};
use ron_showcase::GameConfig;

struct CatalogResolver {
    schemas: BTreeMap<String, Schema>,
}

impl SchemaResolver for CatalogResolver {
    fn resolve(&self, type_path: &str) -> Option<Schema> {
        self.schemas.get(type_path).cloned()
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== RON2 Showcase ===\n");

    let ron_content = fs::read_to_string("data/game.ron")?;
    let value = ron2::from_str(&ron_content)?;

    println!("1. Loading and validating config...");

    let catalog = collect_schemas::<GameConfig>()?;
    let root_path = GameConfig::type_path().ok_or("missing GameConfig type path")?;
    let root_schema = catalog
        .schemas
        .get(root_path)
        .ok_or("missing GameConfig schema")?
        .clone();
    let resolver = CatalogResolver {
        schemas: catalog.schemas,
    };

    validate_with_resolver(&value, &root_schema, &resolver)?;

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
