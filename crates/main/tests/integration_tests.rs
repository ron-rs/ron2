//! End-to-end integration tests for ron-schema.
//!
//! Tests cover:
//! - Create schema, write to disk, read back, validate RON value
//! - Schema resolution precedence
//! - Complete workflows from schema definition to validation

#![cfg(feature = "derive")]

use std::{env, ffi::OsStr, fs};

use ron2::{
    FromRon,
    ast::parse_document,
    error::ErrorKind,
    schema::{
        Field, NoResolver, SCHEMA_DIR_ENV, Schema, TypeKind, ValidationError, Variant, find_schema,
        read_schema, validate, write_schema,
    },
};
use serial_test::serial;

/// Parse `src` and validate it against `schema`.
fn check(src: &str, schema: &Schema) -> Result<(), ValidationError> {
    let doc = parse_document(src).expect("valid RON");
    validate(doc.value.as_ref().expect("a value"), schema, &NoResolver)
}

/// Create a temporary directory for testing.
fn create_temp_dir() -> tempfile::TempDir {
    tempfile::tempdir().expect("Failed to create temp directory")
}

fn set_schema_dir(value: impl AsRef<OsStr>) {
    // Safe in tests that run serially and don't spawn threads touching env vars.
    unsafe {
        env::set_var(SCHEMA_DIR_ENV, value);
    }
}

fn clear_schema_dir() {
    // Safe in tests that run serially and don't spawn threads touching env vars.
    unsafe {
        env::remove_var(SCHEMA_DIR_ENV);
    }
}

// ============================================================================
// Complete workflow tests
// ============================================================================

#[test]
fn test_complete_workflow_simple_struct() {
    let temp_dir = create_temp_dir();

    // Step 1: Define a schema
    let schema = Schema::with_doc(
        "User configuration",
        TypeKind::Struct {
            fields: vec![
                Field::new("username", TypeKind::String).with_doc("The user's name"),
                Field::new("port", TypeKind::U16).with_doc("Server port"),
                Field::optional("debug", TypeKind::Bool).with_doc("Enable debug mode"),
            ],
        },
    );

    // Step 2: Write schema to disk
    let schema_path =
        write_schema("my_app::config::UserConfig", &schema, Some(temp_dir.path())).unwrap();

    // Verify file exists
    assert!(schema_path.exists());

    // Step 3: Read schema back
    let loaded_schema = read_schema(&schema_path).unwrap();
    assert_eq!(schema, loaded_schema);

    // Step 4: Validate a RON value against the schema
    assert!(check(r#"(username: "alice", port: 8080)"#, &loaded_schema).is_ok());

    // Step 5: Validate with optional field
    let valid_ron_with_optional = r#"(username: "bob", port: 9000, debug: true)"#;
    assert!(check(valid_ron_with_optional, &loaded_schema).is_ok());

    // Step 6: Invalid RON should fail validation
    let invalid_ron = r#"(username: "charlie")"#; // missing required 'port'
    assert_eq!(
        check(invalid_ron, &loaded_schema).unwrap_err().kind(),
        &ErrorKind::MissingField {
            field: "port".into(),
            outer: None
        }
    );
}

#[test]
fn test_complete_workflow_complex_enum() {
    let temp_dir = create_temp_dir();

    // Define an enum schema with all variant types
    let schema = Schema::with_doc(
        "API Response",
        TypeKind::Enum {
            variants: vec![
                Variant::unit("Loading").with_doc("Request in progress"),
                Variant::struct_variant(
                    "Success",
                    vec![
                        Field::new("data", TypeKind::String),
                        Field::optional(
                            "metadata",
                            TypeKind::Map {
                                key: Box::new(TypeKind::String),
                                value: Box::new(TypeKind::String),
                            },
                        ),
                    ],
                )
                .with_doc("Successful response"),
                Variant::struct_variant(
                    "Error",
                    vec![
                        Field::new("code", TypeKind::I32),
                        Field::new("message", TypeKind::String),
                    ],
                )
                .with_doc("Error response"),
            ],
        },
    );

    // Write and read back
    let schema_path = write_schema("api::Response", &schema, Some(temp_dir.path())).unwrap();
    let loaded_schema = read_schema(&schema_path).unwrap();

    // Test unit variant
    assert!(check("Loading", &loaded_schema).is_ok());

    // Test struct variant with all fields
    let success = r#"Success(data: "hello", metadata: { "key": "value" })"#;
    assert!(check(success, &loaded_schema).is_ok());

    // Test struct variant with optional field omitted
    assert!(check(r#"Success(data: "hello")"#, &loaded_schema).is_ok());

    // Test error variant
    assert!(check(r#"Error(code: 404, message: "Not found")"#, &loaded_schema).is_ok());

    // Test invalid variants
    assert!(matches!(
        check("Unknown", &loaded_schema).unwrap_err().kind(),
        ErrorKind::UnknownVariant { variant, .. } if variant == "Unknown"
    ));
    // A string is not an enum variant in RON.
    assert_eq!(
        check(r#""Loading""#, &loaded_schema).unwrap_err().kind(),
        &ErrorKind::TypeMismatch {
            expected: "enum".into(),
            found: "string".into()
        }
    );
}

#[test]
fn test_complete_workflow_nested_structures() {
    let temp_dir = create_temp_dir();

    // Define a schema with deeply nested structures
    let schema = Schema::with_doc(
        "Application configuration",
        TypeKind::Struct {
            fields: vec![
                Field::new("app_name", TypeKind::String),
                Field::new(
                    "servers",
                    TypeKind::List(Box::new(TypeKind::Struct {
                        fields: vec![
                            Field::new("host", TypeKind::String),
                            Field::new("port", TypeKind::U16),
                            Field::optional("tags", TypeKind::List(Box::new(TypeKind::String))),
                        ],
                    })),
                ),
                Field::optional(
                    "settings",
                    TypeKind::Map {
                        key: Box::new(TypeKind::String),
                        value: Box::new(TypeKind::Option(Box::new(TypeKind::String))),
                    },
                ),
            ],
        },
    );

    // Write and read back
    let schema_path = write_schema("config::AppConfig", &schema, Some(temp_dir.path())).unwrap();
    let loaded_schema = read_schema(&schema_path).unwrap();

    // Valid complex configuration
    let valid_config = r#"(
        app_name: "MyApp",
        servers: [
            (host: "localhost", port: 8080, tags: ["dev", "primary"]),
            (host: "192.168.1.1", port: 9000)
        ],
        settings: {
            "log_level": Some("debug"),
            "optional_feature": None
        }
    )"#;
    assert!(check(valid_config, &loaded_schema).is_ok());

    // Minimal valid configuration
    let minimal_config = r#"(
        app_name: "Minimal",
        servers: []
    )"#;
    assert!(check(minimal_config, &loaded_schema).is_ok());

    // Invalid: wrong type in nested structure
    let invalid_config = r#"(
        app_name: "Bad",
        servers: [
            (host: "localhost", port: "not a number")
        ]
    )"#;
    assert_eq!(
        check(invalid_config, &loaded_schema)
            .unwrap_err()
            .to_string(),
        "4:39-4:53: in field 'servers' -> element 0 -> field 'port': expected u16 but found string"
    );
}

// ============================================================================
// Schema resolution precedence tests
// ============================================================================

#[test]
fn test_find_schema_in_specific_directory() {
    let temp_dir = create_temp_dir();
    let schema = Schema::new(TypeKind::Bool);

    write_schema("test::Type", &schema, Some(temp_dir.path())).unwrap();

    let found = find_schema("test::Type", &[temp_dir.path().to_path_buf()]).unwrap();
    assert_eq!(schema, found);
}

#[test]
#[serial]
fn test_schema_resolution_with_env_var() {
    let temp_dir = create_temp_dir();
    let schema = Schema::new(TypeKind::String);

    // Store original env var
    let original = env::var(SCHEMA_DIR_ENV).ok();

    // Set env var to temp directory
    set_schema_dir(temp_dir.path());

    // Write schema (should use env var directory)
    write_schema("env_test::Type", &schema, None).unwrap();

    // Schema should exist in temp dir
    let schema_file = temp_dir.path().join("env_test/Type.schema.ron");
    assert!(schema_file.exists());

    // Read it back
    let loaded = read_schema(&schema_file).unwrap();
    assert_eq!(schema, loaded);

    // Restore original env var
    match original {
        Some(val) => set_schema_dir(val),
        None => clear_schema_dir(),
    }
}

#[test]
#[serial]
fn test_output_dir_takes_precedence_over_env_var() {
    let temp_dir1 = create_temp_dir();
    let temp_dir2 = create_temp_dir();
    let schema = Schema::new(TypeKind::I32);

    // Store original env var
    let original = env::var(SCHEMA_DIR_ENV).ok();

    // Set env var to temp_dir1
    set_schema_dir(temp_dir1.path());

    // Write schema with explicit output_dir (temp_dir2)
    let path = write_schema("precedence::Type", &schema, Some(temp_dir2.path())).unwrap();

    // Schema should be in temp_dir2, not temp_dir1
    assert!(path.starts_with(temp_dir2.path()));
    assert!(path.exists());

    // Should NOT exist in temp_dir1
    let env_path = temp_dir1.path().join("precedence/Type.schema.ron");
    assert!(!env_path.exists());

    // Restore original env var
    match original {
        Some(val) => set_schema_dir(val),
        None => clear_schema_dir(),
    }
}

// ============================================================================
// Multiple schemas in project tests
// ============================================================================

#[test]
fn test_multiple_schemas_in_project() {
    let temp_dir = create_temp_dir();

    // Create multiple related schemas
    let user_schema = Schema::with_doc(
        "User",
        TypeKind::Struct {
            fields: vec![
                Field::new("id", TypeKind::U64),
                Field::new("name", TypeKind::String),
                Field::optional("email", TypeKind::String),
            ],
        },
    );

    let post_schema = Schema::with_doc(
        "Post",
        TypeKind::Struct {
            fields: vec![
                Field::new("id", TypeKind::U64),
                Field::new("title", TypeKind::String),
                Field::new("content", TypeKind::String),
                Field::new("author_id", TypeKind::U64),
                Field::new("tags", TypeKind::List(Box::new(TypeKind::String))),
            ],
        },
    );

    let feed_schema = Schema::with_doc(
        "Feed",
        TypeKind::Struct {
            fields: vec![
                Field::new(
                    "posts",
                    TypeKind::List(Box::new(TypeKind::TypeRef("blog::Post".to_string()))),
                ),
                Field::optional("cursor", TypeKind::String),
            ],
        },
    );

    // Write all schemas
    write_schema("blog::User", &user_schema, Some(temp_dir.path())).unwrap();
    write_schema("blog::Post", &post_schema, Some(temp_dir.path())).unwrap();
    write_schema("blog::Feed", &feed_schema, Some(temp_dir.path())).unwrap();

    // Verify all schemas can be found
    let dirs = [temp_dir.path().to_path_buf()];
    let found_user = find_schema("blog::User", &dirs).unwrap();
    let found_post = find_schema("blog::Post", &dirs).unwrap();
    let found_feed = find_schema("blog::Feed", &dirs).unwrap();

    assert_eq!(user_schema, found_user);
    assert_eq!(post_schema, found_post);
    assert_eq!(feed_schema, found_feed);

    // Validate data against schemas
    let user_data = r#"(id: 1, name: "Alice", email: "alice@example.com")"#;
    assert!(check(user_data, &found_user).is_ok());

    let post_data = r#"(
        id: 1,
        title: "Hello World",
        content: "This is my first post",
        author_id: 1,
        tags: ["intro", "hello"]
    )"#;
    assert!(check(post_data, &found_post).is_ok());
}

// ============================================================================
// Schema file format verification tests
// ============================================================================

#[test]
fn test_schema_file_is_valid_ron() {
    let temp_dir = create_temp_dir();

    let schema = Schema::with_doc(
        "Test",
        TypeKind::Struct {
            fields: vec![Field::new("value", TypeKind::I32)],
        },
    );

    let path = write_schema("format::Test", &schema, Some(temp_dir.path())).unwrap();

    // Read the raw file content
    let content = fs::read_to_string(&path).unwrap();

    // Verify it's valid RON that can be parsed
    let _parsed: ron2::Value = content
        .parse::<ron2::Value>()
        .expect("Schema file should be valid RON");

    // Also verify it can be parsed as a Schema using DeRon trait
    let value: ron2::Value = content
        .parse::<ron2::Value>()
        .expect("Schema file should be valid RON");
    let parsed = Schema::from_ron_value(value).expect("Schema file should parse as Schema");
    assert_eq!(parsed, schema);
}

#[test]
fn test_schema_file_preserves_documentation() {
    let temp_dir = create_temp_dir();

    let schema = Schema::with_doc(
        "This is the main documentation",
        TypeKind::Struct {
            fields: vec![
                Field::new("field1", TypeKind::I32).with_doc("Field one documentation"),
                Field::new("field2", TypeKind::String).with_doc("Field two documentation"),
            ],
        },
    );

    let path = write_schema("doc::Test", &schema, Some(temp_dir.path())).unwrap();
    let loaded = read_schema(&path).unwrap();

    assert_eq!(
        loaded.doc,
        Some("This is the main documentation".to_string())
    );

    if let TypeKind::Struct { fields } = loaded.kind {
        assert_eq!(fields[0].doc, Some("Field one documentation".to_string()));
        assert_eq!(fields[1].doc, Some("Field two documentation".to_string()));
    } else {
        panic!("Expected Struct type kind");
    }
}

// ============================================================================
// Error handling integration tests
// ============================================================================

#[test]
fn test_validation_error_provides_useful_context() {
    let temp_dir = create_temp_dir();

    let schema = Schema::new(TypeKind::Struct {
        fields: vec![Field::new(
            "items",
            TypeKind::List(Box::new(TypeKind::Struct {
                fields: vec![
                    Field::new("id", TypeKind::I32),
                    Field::new("name", TypeKind::String),
                ],
            })),
        )],
    });

    write_schema("error_test::Type", &schema, Some(temp_dir.path())).unwrap();
    let loaded = find_schema("error_test::Type", &[temp_dir.path().to_path_buf()]).unwrap();

    // Invalid data with error deep in structure
    let invalid_data = r#"(
        items: [
            (id: 1, name: "first"),
            (id: "not an int", name: "second")
        ]
    )"#;
    let result = check(invalid_data, &loaded);

    // Error should provide context about where the error occurred
    let error = result.unwrap_err();
    let error_message = format!("{}", error);

    // The error should mention the field path
    assert_eq!(
        error_message,
        "4:18-4:30: in field 'items' -> element 1 -> field 'id': expected i32 but found string"
    );
}

// ============================================================================
// Round-trip consistency tests
// ============================================================================

#[test]
fn test_write_read_write_produces_same_content() {
    let temp_dir1 = create_temp_dir();
    let temp_dir2 = create_temp_dir();

    let schema = Schema::with_doc(
        "Test",
        TypeKind::Enum {
            variants: vec![
                Variant::unit("A").with_doc("Variant A"),
                Variant::tuple("B", vec![TypeKind::I32, TypeKind::String]),
                Variant::struct_variant(
                    "C",
                    vec![
                        Field::new("x", TypeKind::F64),
                        Field::optional("y", TypeKind::F64),
                    ],
                ),
            ],
        },
    );

    // Write first time
    let path1 = write_schema("roundtrip::Test", &schema, Some(temp_dir1.path())).unwrap();
    let content1 = fs::read_to_string(&path1).unwrap();

    // Read and write second time
    let loaded = read_schema(&path1).unwrap();
    let path2 = write_schema("roundtrip::Test", &loaded, Some(temp_dir2.path())).unwrap();
    let content2 = fs::read_to_string(&path2).unwrap();

    // Content should be identical
    assert_eq!(content1, content2);
}

#[test]
fn test_all_type_kinds_roundtrip_through_file() {
    let temp_dir = create_temp_dir();

    let schemas = vec![
        ("bool", Schema::new(TypeKind::Bool)),
        ("i8", Schema::new(TypeKind::I8)),
        ("i16", Schema::new(TypeKind::I16)),
        ("i32", Schema::new(TypeKind::I32)),
        ("i64", Schema::new(TypeKind::I64)),
        ("i128", Schema::new(TypeKind::I128)),
        ("u8", Schema::new(TypeKind::U8)),
        ("u16", Schema::new(TypeKind::U16)),
        ("u32", Schema::new(TypeKind::U32)),
        ("u64", Schema::new(TypeKind::U64)),
        ("u128", Schema::new(TypeKind::U128)),
        ("f32", Schema::new(TypeKind::F32)),
        ("f64", Schema::new(TypeKind::F64)),
        ("char", Schema::new(TypeKind::Char)),
        ("string", Schema::new(TypeKind::String)),
        ("unit", Schema::new(TypeKind::Unit)),
        (
            "option",
            Schema::new(TypeKind::Option(Box::new(TypeKind::I32))),
        ),
        (
            "vec",
            Schema::new(TypeKind::List(Box::new(TypeKind::String))),
        ),
        (
            "map",
            Schema::new(TypeKind::Map {
                key: Box::new(TypeKind::String),
                value: Box::new(TypeKind::I32),
            }),
        ),
        (
            "tuple",
            Schema::new(TypeKind::Tuple(vec![TypeKind::I32, TypeKind::String])),
        ),
        (
            "struct",
            Schema::new(TypeKind::Struct {
                fields: vec![Field::new("x", TypeKind::I32)],
            }),
        ),
        (
            "enum",
            Schema::new(TypeKind::Enum {
                variants: vec![Variant::unit("A")],
            }),
        ),
        (
            "typeref",
            Schema::new(TypeKind::TypeRef("other::Type".to_string())),
        ),
    ];

    for (name, schema) in schemas {
        let type_path = format!("types::{}", name);
        let path = write_schema(&type_path, &schema, Some(temp_dir.path())).unwrap();
        let loaded = read_schema(&path).unwrap();
        assert_eq!(schema, loaded, "Failed for type: {}", name);
    }
}
