//! Tests for named types: structs and enum variants.
//!
//! This module tests the new Value::Named type and its interactions with
//! other Value types.
//!
//! Note: The current parser does not support `::` paths (e.g., `Module::Type`).
//!
//! RON Syntax:
//! - Struct fields use PARENTHESES: `Point(x: 1, y: 2)`
//! - Curly braces are for MAPS only: `{ "key": value }`

use ron2::{FormatConfig, NamedContent, Number, ToRon, Value};

// =============================================================================
// Unit Named Types - Edge Cases
// (Basic cases covered in roundtrip.rs)
// =============================================================================

#[test]
fn named_unit_single_char() {
    let value: Value = "A".parse().unwrap();
    assert_eq!(
        value,
        Value::Named {
            name: String::from("A"),
            content: NamedContent::Unit,
        }
    );
}

#[test]
fn named_unit_with_underscore() {
    let value: Value = "my_type".parse().unwrap();
    assert_eq!(
        value,
        Value::Named {
            name: String::from("my_type"),
            content: NamedContent::Unit,
        }
    );
}

#[test]
fn named_unit_leading_underscore() {
    let value: Value = "_private".parse().unwrap();
    assert_eq!(
        value,
        Value::Named {
            name: String::from("_private"),
            content: NamedContent::Unit,
        }
    );
}

#[test]
fn named_unit_with_numbers() {
    let value: Value = "Type2D".parse().unwrap();
    assert_eq!(
        value,
        Value::Named {
            name: String::from("Type2D"),
            content: NamedContent::Unit,
        }
    );
}

#[test]
fn named_unit_raw_identifier() {
    // Raw identifiers preserve the r# prefix in the parsed name
    let value: Value = "r#type".parse().unwrap();
    assert_eq!(
        value,
        Value::Named {
            name: String::from("r#type"),
            content: NamedContent::Unit,
        }
    );
}

#[test]
fn named_unit_raw_identifier_match() {
    let value: Value = "r#match".parse().unwrap();
    assert_eq!(
        value,
        Value::Named {
            name: String::from("r#match"),
            content: NamedContent::Unit,
        }
    );
}

// =============================================================================
// Tuple Named Types - Edge Cases
// (Basic cases covered in roundtrip.rs)
// =============================================================================

#[test]
fn named_tuple_empty() {
    // Empty parens are an empty tuple (not the same as unit)
    let value: Value = "Empty()".parse().unwrap();
    assert_eq!(
        value,
        Value::Named {
            name: String::from("Empty"),
            content: NamedContent::Tuple(vec![]),
        }
    );
}

#[test]
fn named_tuple_with_seq() {
    let value: Value = "Container([1, 2, 3])".parse().unwrap();
    assert_eq!(
        value,
        Value::Named {
            name: String::from("Container"),
            content: NamedContent::Tuple(vec![Value::Seq(vec![
                Value::Number(Number::U8(1)),
                Value::Number(Number::U8(2)),
                Value::Number(Number::U8(3)),
            ])]),
        }
    );
}

#[test]
fn named_tuple_with_option() {
    let value: Value = "Wrapper(Some(42))".parse().unwrap();
    assert_eq!(
        value,
        Value::Named {
            name: String::from("Wrapper"),
            content: NamedContent::Tuple(vec![Value::Option(Some(Box::new(Value::Number(
                Number::U8(42)
            ))))]),
        }
    );
}

// =============================================================================
// Struct Named Types - Edge Cases
// (Basic cases covered in roundtrip.rs)
// =============================================================================

#[test]
fn named_struct_with_none_field() {
    let value: Value = "Config(value: None)".parse().unwrap();
    assert_eq!(
        value,
        Value::Named {
            name: String::from("Config"),
            content: NamedContent::Struct(vec![(String::from("value"), Value::Option(None))]),
        }
    );
}

#[test]
fn named_struct_multiline() {
    let input = r#"Config(
        host: "localhost",
        port: 8080,
        debug: true
    )"#;
    let value: Value = input.parse().unwrap();
    assert_eq!(
        value,
        Value::Named {
            name: String::from("Config"),
            content: NamedContent::Struct(vec![
                (
                    String::from("host"),
                    Value::String(String::from("localhost"))
                ),
                (String::from("port"), Value::Number(Number::U16(8080))),
                (String::from("debug"), Value::Bool(true)),
            ]),
        }
    );
}

// =============================================================================
// Serialization of Named Types
// =============================================================================

#[test]
fn serialize_named_unit() {
    let value = Value::Named {
        name: String::from("MyType"),
        content: NamedContent::Unit,
    };
    assert_eq!(
        value.to_ron_with(&FormatConfig::minimal()).unwrap(),
        "MyType"
    );
}

#[test]
fn serialize_named_tuple() {
    let value = Value::Named {
        name: String::from("Point"),
        content: NamedContent::Tuple(vec![
            Value::Number(Number::U8(1)),
            Value::Number(Number::U8(2)),
        ]),
    };
    assert_eq!(
        value.to_ron_with(&FormatConfig::minimal()).unwrap(),
        "Point(1,2)"
    );
}

#[test]
fn serialize_named_struct() {
    let value = Value::Named {
        name: String::from("Point"),
        content: NamedContent::Struct(vec![
            (String::from("x"), Value::Number(Number::U8(1))),
            (String::from("y"), Value::Number(Number::U8(2))),
        ]),
    };
    assert_eq!(
        value.to_ron_with(&FormatConfig::minimal()).unwrap(),
        "Point(x:1,y:2)"
    );
}

#[test]
fn serialize_named_struct_pretty() {
    let value = Value::Named {
        name: String::from("Point"),
        content: NamedContent::Struct(vec![
            (String::from("x"), Value::Number(Number::U8(1))),
            (String::from("y"), Value::Number(Number::U8(2))),
        ]),
    };
    let pretty = value.to_ron().unwrap();
    assert!(pretty.contains("x: 1"));
    assert!(pretty.contains("y: 2"));
    assert!(pretty.contains('\n'));
}

// =============================================================================
// Option is Special
// =============================================================================

#[test]
fn option_none_is_special() {
    // None is parsed as Value::Option(None), not Named { name: "None", ... }
    let value: Value = "None".parse().unwrap();
    assert_eq!(value, Value::Option(None));
}

#[test]
fn option_some_is_special() {
    // Some(x) is parsed as Value::Option(Some(x)), not Named { name: "Some", ... }
    let value: Value = "Some(42)".parse().unwrap();
    assert_eq!(
        value,
        Value::Option(Some(Box::new(Value::Number(Number::U8(42)))))
    );
}

#[test]
fn option_none_in_struct() {
    let value: Value = "Config(value: None)".parse().unwrap();
    match value {
        Value::Named {
            content: NamedContent::Struct(fields),
            ..
        } => {
            assert_eq!(fields[0].1, Value::Option(None));
        }
        _ => panic!("Expected named struct"),
    }
}

#[test]
fn option_some_in_struct() {
    let value: Value = "Config(value: Some(42))".parse().unwrap();
    match value {
        Value::Named {
            content: NamedContent::Struct(fields),
            ..
        } => {
            assert_eq!(
                fields[0].1,
                Value::Option(Some(Box::new(Value::Number(Number::U8(42)))))
            );
        }
        _ => panic!("Expected named struct"),
    }
}

// =============================================================================
// Complex Realistic Examples
// =============================================================================

#[test]
fn realistic_game_entity() {
    let value: Value = r#"Entity(
        id: 12345,
        name: "Player",
        position: Vec3(10.5, 0.0, -3.2),
        components: [
            Health(100, 100),
            Inventory(["sword", "shield", "potion"]),
            Transform(1.0, Quaternion(0.0, 0.0, 0.0, 1.0))
        ],
        active: true
    )"#
    .parse()
    .unwrap();

    match value {
        Value::Named { name, content } => {
            assert_eq!(name, "Entity");
            match content {
                NamedContent::Struct(fields) => {
                    assert_eq!(fields.len(), 5);
                    assert_eq!(fields[0].0, "id");
                    assert_eq!(fields[1].0, "name");
                    assert_eq!(fields[2].0, "position");
                    assert_eq!(fields[3].0, "components");
                    assert_eq!(fields[4].0, "active");
                }
                _ => panic!("Expected struct content"),
            }
        }
        _ => panic!("Expected named value"),
    }
}

#[test]
fn realistic_api_response() {
    let value: Value = r#"Response(
        status: Ok,
        data: Some(User(
            id: 1,
            username: "alice",
            email: "alice@example.com",
            roles: [Admin, User]
        )),
        metadata: {
            "request_id": "abc123",
            "timestamp": 1234567890
        }
    )"#
    .parse()
    .unwrap();

    // Just verify it parses correctly
    assert!(matches!(value, Value::Named { .. }));
}

#[test]
fn realistic_config_file() {
    let input = r#"AppConfig(
        // Server settings
        server: ServerConfig(
            host: "0.0.0.0",
            port: 8080,
            tls: Some(TlsConfig(
                cert_path: "/etc/ssl/cert.pem",
                key_path: "/etc/ssl/key.pem"
            ))
        ),
        // Database settings
        database: DatabaseConfig(
            driver: Postgres,
            url: "postgres://localhost/mydb",
            pool_size: 10,
            timeout_secs: 30
        ),
        // Feature flags
        features: {
            "new_ui": true,
            "beta_api": false,
            "dark_mode": true
        },
        // Logging
        log_level: Info
    )"#;

    let value: Value = input.parse().unwrap();
    let serialized = value.to_ron_with(&FormatConfig::minimal()).unwrap();
    let reparsed: Value = serialized.parse().unwrap();
    assert_eq!(value, reparsed);
}
