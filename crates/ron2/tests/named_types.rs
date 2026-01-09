//! Tests for named types: structs and enum variants.
//!
//! This module tests the new Value::Named type and its interactions with
//! other Value types.
//!
//! Note: The current parser does not support `::` paths (e.g., `Module::Type`).
//! Named structs use brace syntax: `Point { x: 1, y: 2 }` not paren syntax.

use ron2::{from_str, to_string, to_string_pretty, NamedContent, Number, PrettyConfig, Value};

// =============================================================================
// Unit Named Types
// =============================================================================

#[test]
fn named_unit_simple() {
    let value = from_str("MyType").unwrap();
    assert_eq!(
        value,
        Value::Named {
            name: String::from("MyType"),
            content: NamedContent::Unit,
        }
    );
}

#[test]
fn named_unit_single_char() {
    let value = from_str("A").unwrap();
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
    let value = from_str("my_type").unwrap();
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
    let value = from_str("_private").unwrap();
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
    let value = from_str("Type2D").unwrap();
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
    let value = from_str("r#type").unwrap();
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
    let value = from_str("r#match").unwrap();
    assert_eq!(
        value,
        Value::Named {
            name: String::from("r#match"),
            content: NamedContent::Unit,
        }
    );
}

// =============================================================================
// Tuple Named Types
// =============================================================================

#[test]
fn named_tuple_single_element() {
    let value = from_str("Newtype(42)").unwrap();
    assert_eq!(
        value,
        Value::Named {
            name: String::from("Newtype"),
            content: NamedContent::Tuple(vec![Value::Number(Number::U8(42))]),
        }
    );
}

#[test]
fn named_tuple_two_elements() {
    let value = from_str("Pair(1, 2)").unwrap();
    assert_eq!(
        value,
        Value::Named {
            name: String::from("Pair"),
            content: NamedContent::Tuple(vec![
                Value::Number(Number::U8(1)),
                Value::Number(Number::U8(2)),
            ]),
        }
    );
}

#[test]
fn named_tuple_three_elements() {
    let value = from_str("Triple(1, 2, 3)").unwrap();
    assert_eq!(
        value,
        Value::Named {
            name: String::from("Triple"),
            content: NamedContent::Tuple(vec![
                Value::Number(Number::U8(1)),
                Value::Number(Number::U8(2)),
                Value::Number(Number::U8(3)),
            ]),
        }
    );
}

#[test]
fn named_tuple_mixed_types() {
    let value = from_str(r#"Mixed("hello", 42, true)"#).unwrap();
    assert_eq!(
        value,
        Value::Named {
            name: String::from("Mixed"),
            content: NamedContent::Tuple(vec![
                Value::String(String::from("hello")),
                Value::Number(Number::U8(42)),
                Value::Bool(true),
            ]),
        }
    );
}

#[test]
fn named_tuple_empty() {
    // Empty parens are an empty tuple (not the same as unit)
    let value = from_str("Empty()").unwrap();
    assert_eq!(
        value,
        Value::Named {
            name: String::from("Empty"),
            content: NamedContent::Tuple(vec![]),
        }
    );
}

#[test]
fn named_tuple_nested() {
    let value = from_str("Outer(Inner(1))").unwrap();
    assert_eq!(
        value,
        Value::Named {
            name: String::from("Outer"),
            content: NamedContent::Tuple(vec![Value::Named {
                name: String::from("Inner"),
                content: NamedContent::Tuple(vec![Value::Number(Number::U8(1))]),
            }]),
        }
    );
}

#[test]
fn named_tuple_with_seq() {
    let value = from_str("Container([1, 2, 3])").unwrap();
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
    let value = from_str("Wrapper(Some(42))").unwrap();
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

#[test]
fn named_tuple_trailing_comma() {
    let value = from_str("Point(1, 2,)").unwrap();
    assert_eq!(
        value,
        Value::Named {
            name: String::from("Point"),
            content: NamedContent::Tuple(vec![
                Value::Number(Number::U8(1)),
                Value::Number(Number::U8(2)),
            ]),
        }
    );
}

// =============================================================================
// Struct Named Types (using brace syntax)
// =============================================================================

#[test]
fn named_struct_single_field() {
    // Named structs use braces for fields
    let value = from_str("Wrapper { value: 42 }").unwrap();
    assert_eq!(
        value,
        Value::Named {
            name: String::from("Wrapper"),
            content: NamedContent::Struct(vec![(
                String::from("value"),
                Value::Number(Number::U8(42))
            )]),
        }
    );
}

#[test]
fn named_struct_two_fields() {
    let value = from_str("Point { x: 1, y: 2 }").unwrap();
    assert_eq!(
        value,
        Value::Named {
            name: String::from("Point"),
            content: NamedContent::Struct(vec![
                (String::from("x"), Value::Number(Number::U8(1))),
                (String::from("y"), Value::Number(Number::U8(2))),
            ]),
        }
    );
}

#[test]
fn named_struct_three_fields() {
    let value = from_str("Point3D { x: 1, y: 2, z: 3 }").unwrap();
    assert_eq!(
        value,
        Value::Named {
            name: String::from("Point3D"),
            content: NamedContent::Struct(vec![
                (String::from("x"), Value::Number(Number::U8(1))),
                (String::from("y"), Value::Number(Number::U8(2))),
                (String::from("z"), Value::Number(Number::U8(3))),
            ]),
        }
    );
}

#[test]
fn named_struct_mixed_types() {
    let value = from_str(r#"Person { name: "Alice", age: 30 }"#).unwrap();
    assert_eq!(
        value,
        Value::Named {
            name: String::from("Person"),
            content: NamedContent::Struct(vec![
                (String::from("name"), Value::String(String::from("Alice"))),
                (String::from("age"), Value::Number(Number::U8(30))),
            ]),
        }
    );
}

#[test]
fn named_struct_nested_struct() {
    let value = from_str("Outer { inner: Inner { value: 1 } }").unwrap();
    assert_eq!(
        value,
        Value::Named {
            name: String::from("Outer"),
            content: NamedContent::Struct(vec![(
                String::from("inner"),
                Value::Named {
                    name: String::from("Inner"),
                    content: NamedContent::Struct(vec![(
                        String::from("value"),
                        Value::Number(Number::U8(1))
                    )]),
                }
            )]),
        }
    );
}

#[test]
fn named_struct_with_seq_field() {
    let value = from_str("Container { items: [1, 2, 3] }").unwrap();
    assert_eq!(
        value,
        Value::Named {
            name: String::from("Container"),
            content: NamedContent::Struct(vec![(
                String::from("items"),
                Value::Seq(vec![
                    Value::Number(Number::U8(1)),
                    Value::Number(Number::U8(2)),
                    Value::Number(Number::U8(3)),
                ])
            )]),
        }
    );
}

#[test]
fn named_struct_with_option_field() {
    let value = from_str("Config { value: Some(42) }").unwrap();
    assert_eq!(
        value,
        Value::Named {
            name: String::from("Config"),
            content: NamedContent::Struct(vec![(
                String::from("value"),
                Value::Option(Some(Box::new(Value::Number(Number::U8(42)))))
            )]),
        }
    );
}

#[test]
fn named_struct_with_none_field() {
    let value = from_str("Config { value: None }").unwrap();
    assert_eq!(
        value,
        Value::Named {
            name: String::from("Config"),
            content: NamedContent::Struct(vec![(String::from("value"), Value::Option(None))]),
        }
    );
}

#[test]
fn named_struct_trailing_comma() {
    let value = from_str("Point { x: 1, y: 2, }").unwrap();
    assert_eq!(
        value,
        Value::Named {
            name: String::from("Point"),
            content: NamedContent::Struct(vec![
                (String::from("x"), Value::Number(Number::U8(1))),
                (String::from("y"), Value::Number(Number::U8(2))),
            ]),
        }
    );
}

#[test]
fn named_struct_multiline() {
    let input = r#"Config {
        host: "localhost",
        port: 8080,
        debug: true
    }"#;
    let value = from_str(input).unwrap();
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
    assert_eq!(to_string(&value).unwrap(), "MyType");
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
    assert_eq!(to_string(&value).unwrap(), "Point(1,2)");
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
    assert_eq!(to_string(&value).unwrap(), "Point(x:1,y:2)");
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
    let pretty = to_string_pretty(&value, PrettyConfig::new()).unwrap();
    assert!(pretty.contains("x: 1"));
    assert!(pretty.contains("y: 2"));
    assert!(pretty.contains('\n'));
}

// =============================================================================
// Roundtrip Named Types
// =============================================================================

fn roundtrip(input: &str) -> Value {
    let value = from_str(input).unwrap();
    let serialized = to_string(&value).unwrap();
    let reparsed = from_str(&serialized).unwrap();
    assert_eq!(value, reparsed);
    value
}

#[test]
fn roundtrip_named_unit() {
    roundtrip("MyType");
}

#[test]
fn roundtrip_named_tuple() {
    roundtrip("Point(1, 2)");
}

#[test]
fn roundtrip_named_struct() {
    roundtrip("Point { x: 1, y: 2 }");
}

#[test]
fn roundtrip_named_complex() {
    roundtrip(
        r#"Config {
        server: Server { host: "localhost", port: 8080 },
        items: [1, 2, 3],
        enabled: true
    }"#,
    );
}

// =============================================================================
// Option is Special
// =============================================================================

#[test]
fn option_none_is_special() {
    // None is parsed as Value::Option(None), not Named { name: "None", ... }
    let value = from_str("None").unwrap();
    assert_eq!(value, Value::Option(None));
}

#[test]
fn option_some_is_special() {
    // Some(x) is parsed as Value::Option(Some(x)), not Named { name: "Some", ... }
    let value = from_str("Some(42)").unwrap();
    assert_eq!(
        value,
        Value::Option(Some(Box::new(Value::Number(Number::U8(42)))))
    );
}

#[test]
fn option_none_in_struct() {
    let value = from_str("Config { value: None }").unwrap();
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
    let value = from_str("Config { value: Some(42) }").unwrap();
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
    let value = from_str(
        r#"Entity {
        id: 12345,
        name: "Player",
        position: Vec3(10.5, 0.0, -3.2),
        components: [
            Health(100, 100),
            Inventory(["sword", "shield", "potion"]),
            Transform(1.0, Quaternion(0.0, 0.0, 0.0, 1.0))
        ],
        active: true
    }"#,
    )
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
    let value = from_str(
        r#"Response {
        status: Ok,
        data: Some(User {
            id: 1,
            username: "alice",
            email: "alice@example.com",
            roles: [Admin, User]
        }),
        metadata: {
            "request_id": "abc123",
            "timestamp": 1234567890
        }
    }"#,
    )
    .unwrap();

    // Just verify it parses correctly
    assert!(matches!(value, Value::Named { .. }));
}

#[test]
fn realistic_config_file() {
    let input = r#"AppConfig {
        // Server settings
        server: ServerConfig {
            host: "0.0.0.0",
            port: 8080,
            tls: Some(TlsConfig {
                cert_path: "/etc/ssl/cert.pem",
                key_path: "/etc/ssl/key.pem"
            })
        },
        // Database settings
        database: DatabaseConfig {
            driver: Postgres,
            url: "postgres://localhost/mydb",
            pool_size: 10,
            timeout_secs: 30
        },
        // Feature flags
        features: {
            "new_ui": true,
            "beta_api": false,
            "dark_mode": true
        },
        // Logging
        log_level: Info
    }"#;

    let value = from_str(input).unwrap();
    let serialized = to_string(&value).unwrap();
    let reparsed = from_str(&serialized).unwrap();
    assert_eq!(value, reparsed);
}
