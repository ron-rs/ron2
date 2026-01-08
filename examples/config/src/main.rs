//! Complete example demonstrating ron-derive usage.
//!
//! This example shows:
//! - Using `#[derive(RonSchema, SerRon, DeRon)]` on custom types
//! - Serializing and deserializing RON data
//! - Generating schema files
//! - Using various `#[ron(...)]` attributes

use ron_derive::{DeRon, RonSchema, SerRon};
use ron_schema::{DeRon, RonSchema, SerRon};

/// Application configuration for a web server.
#[derive(Debug, Clone, PartialEq, RonSchema, SerRon, DeRon)]
#[ron(output = "schemas/")]
pub struct AppConfig {
    /// Server port number.
    port: u16,
    /// Server hostname.
    #[ron(default)]
    host: String,
    /// Whether debug mode is enabled.
    #[ron(default)]
    debug: bool,
    /// List of enabled features.
    features: Vec<Feature>,
    /// Database configuration.
    database: DatabaseConfig,
}

/// A feature that can be enabled.
#[derive(Debug, Clone, PartialEq, RonSchema, SerRon, DeRon)]
pub enum Feature {
    /// Enable logging.
    Logging,
    /// Enable metrics collection.
    Metrics,
    /// Enable authentication with a specific provider.
    Auth {
        /// The authentication provider.
        provider: String,
        /// Whether to require email verification.
        #[ron(default)]
        require_email: bool,
    },
    /// Rate limiting with requests per minute.
    RateLimit(u32),
}

/// Database connection configuration.
#[derive(Debug, Clone, PartialEq, RonSchema, SerRon, DeRon)]
pub struct DatabaseConfig {
    /// Database URL.
    url: String,
    /// Maximum number of connections.
    #[ron(default)]
    max_connections: u32,
    /// Connection timeout in seconds.
    #[ron(default = "default_timeout")]
    timeout_secs: u64,
}

/// Default timeout for database connections.
fn default_timeout() -> u64 {
    30
}

fn main() {
    println!("=== RON Derive Example ===\n");

    // Create a config programmatically
    let config = AppConfig {
        port: 8080,
        host: "localhost".to_string(),
        debug: true,
        features: vec![
            Feature::Logging,
            Feature::Metrics,
            Feature::Auth {
                provider: "oauth2".to_string(),
                require_email: true,
            },
            Feature::RateLimit(100),
        ],
        database: DatabaseConfig {
            url: "postgres://localhost/mydb".to_string(),
            max_connections: 20,
            timeout_secs: 60,
        },
    };

    // Serialize to RON
    println!("1. Serializing config to RON:");
    println!("------------------------------");
    let ron_string = config.to_ron().unwrap();
    println!("{}\n", ron_string);

    // Pretty print
    println!("2. Pretty-printed RON:");
    println!("----------------------");
    let pretty_config = ron_schema::PrettyConfig::default();
    let pretty_ron = config.to_ron_pretty(&pretty_config).unwrap();
    println!("{}\n", pretty_ron);

    // Deserialize from RON
    println!("3. Deserializing from RON:");
    println!("--------------------------");
    let ron_input = r#"(
        port: 3000,
        host: "0.0.0.0",
        debug: false,
        features: [
            {"Logging": ()},
            {"RateLimit": 50},
        ],
        database: (
            url: "sqlite://./data.db",
            max_connections: 5,
        ),
    )"#;

    let parsed: AppConfig = AppConfig::from_ron(ron_input).unwrap();
    println!("Parsed config: {:?}\n", parsed);

    // Verify defaults were applied
    println!("4. Checking defaults:");
    println!("--------------------");
    println!("  database.timeout_secs (should be 30): {}", parsed.database.timeout_secs);

    // Write schema files
    println!("\n5. Writing schema files:");
    println!("------------------------");

    match AppConfig::write_schema() {
        Ok(path) => println!("  AppConfig schema: {}", path.display()),
        Err(e) => println!("  Error writing AppConfig schema: {}", e),
    }

    match Feature::write_schema() {
        Ok(path) => println!("  Feature schema: {}", path.display()),
        Err(e) => println!("  Error writing Feature schema: {}", e),
    }

    match DatabaseConfig::write_schema() {
        Ok(path) => println!("  DatabaseConfig schema: {}", path.display()),
        Err(e) => println!("  Error writing DatabaseConfig schema: {}", e),
    }

    // Display schema
    println!("\n6. Generated AppConfig schema:");
    println!("------------------------------");
    let schema = AppConfig::schema();
    let schema_ron = ron::ser::to_string_pretty(&schema, Default::default()).unwrap();
    println!("{}", schema_ron);

    // Roundtrip test
    println!("\n7. Roundtrip test:");
    println!("------------------");
    let serialized = config.to_ron().unwrap();
    let deserialized: AppConfig = AppConfig::from_ron(&serialized).unwrap();
    if config == deserialized {
        println!("  SUCCESS: Roundtrip serialization works!");
    } else {
        println!("  FAILURE: Roundtrip produced different result");
        println!("  Original: {:?}", config);
        println!("  Roundtrip: {:?}", deserialized);
    }

    println!("\n=== Example Complete ===");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_serialize_deserialize() {
        let config = AppConfig {
            port: 8080,
            host: "localhost".to_string(),
            debug: false,
            features: vec![Feature::Logging],
            database: DatabaseConfig {
                url: "postgres://localhost/test".to_string(),
                max_connections: 10,
                timeout_secs: 30,
            },
        };

        let ron = config.to_ron().unwrap();
        let parsed: AppConfig = AppConfig::from_ron(&ron).unwrap();
        assert_eq!(config, parsed);
    }

    #[test]
    fn test_default_values() {
        // Missing optional fields should get defaults
        let ron = r#"(
            port: 8080,
            features: [],
            database: (url: "test://db"),
        )"#;

        let config: AppConfig = AppConfig::from_ron(ron).unwrap();
        assert_eq!(config.host, "");  // Default String
        assert_eq!(config.debug, false);  // Default bool
        assert_eq!(config.database.timeout_secs, 30);  // Custom default
    }

    #[test]
    fn test_enum_variants() {
        let features = vec![
            Feature::Logging,
            Feature::Metrics,
            Feature::Auth {
                provider: "google".to_string(),
                require_email: false,
            },
            Feature::RateLimit(100),
        ];

        for feature in features {
            let ron = feature.to_ron().unwrap();
            let parsed: Feature = Feature::from_ron(&ron).unwrap();
            assert_eq!(feature, parsed);
        }
    }
}
