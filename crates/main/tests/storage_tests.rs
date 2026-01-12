//! Comprehensive tests for ron-schema storage module.
//!
//! Tests cover:
//! - write_schema and read_schema roundtrip
//! - find_schema with different path configurations
//! - type_path_to_file_path edge cases
//! - Environment variable override
//! - XDG fallback

use std::{env, ffi::OsStr, fs, path::PathBuf};

use ron2::schema::{
    Field, SCHEMA_DIR_ENV, Schema, TypeKind, find_schema_in, read_schema, resolve_schema_dir,
    write_schema,
};
use serial_test::serial;

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
// type_path_to_file_path tests
// ============================================================================

#[test]
fn test_type_path_to_file_path_simple_type() {
    use ron2::schema::storage::type_path_to_file_path;

    let path = type_path_to_file_path("MyType");
    assert_eq!(path, PathBuf::from("MyType.schema.ron"));
}

#[test]
fn test_type_path_to_file_path_single_module() {
    use ron2::schema::storage::type_path_to_file_path;

    let path = type_path_to_file_path("my_crate::MyType");
    assert_eq!(path, PathBuf::from("my_crate/MyType.schema.ron"));
}

#[test]
fn test_type_path_to_file_path_nested_modules() {
    use ron2::schema::storage::type_path_to_file_path;

    let path = type_path_to_file_path("my_crate::config::AppConfig");
    assert_eq!(path, PathBuf::from("my_crate/config/AppConfig.schema.ron"));
}

#[test]
fn test_type_path_to_file_path_deeply_nested() {
    use ron2::schema::storage::type_path_to_file_path;

    let path = type_path_to_file_path("a::b::c::d::e::Type");
    assert_eq!(path, PathBuf::from("a/b/c/d/e/Type.schema.ron"));
}

#[test]
fn test_type_path_to_file_path_with_underscores() {
    use ron2::schema::storage::type_path_to_file_path;

    let path = type_path_to_file_path("my_crate::my_module::MyType");
    assert_eq!(path, PathBuf::from("my_crate/my_module/MyType.schema.ron"));
}

#[test]
fn test_type_path_to_file_path_with_numbers() {
    use ron2::schema::storage::type_path_to_file_path;

    let path = type_path_to_file_path("v2::api::Response");
    assert_eq!(path, PathBuf::from("v2/api/Response.schema.ron"));
}

// ============================================================================
// write_schema and read_schema roundtrip tests
// ============================================================================

#[test]
fn test_write_and_read_schema_simple() {
    let temp_dir = create_temp_dir();
    let schema = Schema::new(TypeKind::Bool);

    let path = write_schema("test::Simple", &schema, Some(temp_dir.path())).unwrap();

    assert!(path.exists());
    assert!(path.ends_with("test/Simple.schema.ron"));

    let read_back = read_schema(&path).unwrap();
    assert_eq!(schema, read_back);
}

#[test]
fn test_write_and_read_schema_with_doc() {
    let temp_dir = create_temp_dir();
    let schema = Schema::with_doc("A documented type", TypeKind::String);

    let path = write_schema("documented::Type", &schema, Some(temp_dir.path())).unwrap();

    let read_back = read_schema(&path).unwrap();
    assert_eq!(schema.doc, read_back.doc);
    assert_eq!(schema.kind, read_back.kind);
}

#[test]
fn test_write_and_read_schema_complex() {
    let temp_dir = create_temp_dir();
    let schema = Schema::with_doc(
        "Application configuration",
        TypeKind::Struct {
            fields: vec![
                Field::new("port", TypeKind::U16).with_doc("Server port"),
                Field::optional("host", TypeKind::String).with_doc("Hostname"),
                Field::new("tags", TypeKind::List(Box::new(TypeKind::String))),
            ],
        },
    );

    let path = write_schema("my_app::config::Config", &schema, Some(temp_dir.path())).unwrap();

    assert!(path.exists());
    assert!(path.ends_with("my_app/config/Config.schema.ron"));

    let read_back = read_schema(&path).unwrap();
    assert_eq!(schema, read_back);
}

#[test]
fn test_write_creates_parent_directories() {
    let temp_dir = create_temp_dir();
    let schema = Schema::new(TypeKind::I32);

    // Write to deeply nested path
    let path = write_schema("a::b::c::d::e::DeepType", &schema, Some(temp_dir.path())).unwrap();

    assert!(path.exists());
    assert!(path.ends_with("a/b/c/d/e/DeepType.schema.ron"));
}

#[test]
fn test_write_overwrites_existing_file() {
    let temp_dir = create_temp_dir();

    let schema1 = Schema::new(TypeKind::I32);
    let schema2 = Schema::new(TypeKind::String);

    let path1 = write_schema("test::Overwrite", &schema1, Some(temp_dir.path())).unwrap();
    let path2 = write_schema("test::Overwrite", &schema2, Some(temp_dir.path())).unwrap();

    assert_eq!(path1, path2);

    let read_back = read_schema(&path1).unwrap();
    // Should have the second schema's type
    assert_eq!(read_back.kind, TypeKind::String);
}

#[test]
fn test_read_nonexistent_file_fails() {
    let result = read_schema(&PathBuf::from("/nonexistent/path/schema.ron"));
    assert!(result.is_err());
}

#[test]
fn test_read_invalid_ron_fails() {
    let temp_dir = create_temp_dir();
    let file_path = temp_dir.path().join("invalid.schema.ron");
    fs::write(&file_path, "this is not valid RON {{{").unwrap();

    let result = read_schema(&file_path);
    assert!(result.is_err());
}

// ============================================================================
// find_schema_in tests
// ============================================================================

#[test]
fn test_find_schema_in_directory() {
    let temp_dir = create_temp_dir();
    let schema = Schema::new(TypeKind::Bool);

    write_schema("findme::Type", &schema, Some(temp_dir.path())).unwrap();

    let found = find_schema_in("findme::Type", temp_dir.path()).unwrap();
    assert_eq!(schema, found);
}

#[test]
fn test_find_schema_not_found_in_directory() {
    let temp_dir = create_temp_dir();

    let result = find_schema_in("nonexistent::Type", temp_dir.path());
    // Should fail since it's not in the temp dir and not in the default location
    assert!(result.is_err());
}

// ============================================================================
// Environment variable tests
// ============================================================================

#[test]
#[serial]
fn test_resolve_schema_dir_with_env_var() {
    let temp_dir = create_temp_dir();
    let temp_path = temp_dir.path().to_string_lossy().to_string();

    // Store original value
    let original = env::var(SCHEMA_DIR_ENV).ok();

    // Set environment variable
    set_schema_dir(&temp_path);

    let resolved = resolve_schema_dir().unwrap();
    assert_eq!(resolved, PathBuf::from(&temp_path));

    // Restore original value
    match original {
        Some(val) => set_schema_dir(val),
        None => clear_schema_dir(),
    }
}

#[test]
#[serial]
fn test_resolve_schema_dir_without_env_var_uses_xdg() {
    // Store original value
    let original = env::var(SCHEMA_DIR_ENV).ok();

    // Remove environment variable
    clear_schema_dir();

    let resolved = resolve_schema_dir();

    // Should succeed with XDG fallback (on most systems)
    // The exact path depends on the system, but it should end with "ron-schemas"
    if let Ok(path) = resolved {
        let path_str = path.to_string_lossy();
        assert!(
            path_str.ends_with("ron-schemas"),
            "Expected path to end with 'ron-schemas', got: {}",
            path_str
        );
    }

    // Restore original value
    if let Some(val) = original {
        set_schema_dir(val);
    }
}

// ============================================================================
// Schema file format tests
// ============================================================================

#[test]
fn test_written_schema_is_human_readable() {
    let temp_dir = create_temp_dir();
    let schema = Schema::with_doc(
        "Test schema",
        TypeKind::Struct {
            fields: vec![
                Field::new("name", TypeKind::String),
                Field::new("value", TypeKind::I32),
            ],
        },
    );

    let path = write_schema("format::Test", &schema, Some(temp_dir.path())).unwrap();
    let contents = fs::read_to_string(&path).unwrap();

    // Pretty-printed RON should be multi-line and readable
    assert!(contents.contains('\n'));
    assert!(contents.contains("doc:"));
    assert!(contents.contains("Test schema"));
    assert!(contents.contains("Struct"));
    assert!(contents.contains("name"));
    assert!(contents.contains("value"));
}

#[test]
fn test_schema_file_can_be_manually_edited_and_read() {
    let temp_dir = create_temp_dir();
    let file_path = temp_dir.path().join("manual.schema.ron");

    // Write a manually crafted schema file
    // Note: RON requires Some(...) syntax for Option fields when present
    let manual_content = r#"(
    doc: Some("Manually written schema"),
    kind: Struct(
        fields: [
            (
                name: "id",
                ty: U64,
            ),
            (
                name: "data",
                ty: String,
                optional: true,
            ),
        ],
    ),
)"#;

    fs::write(&file_path, manual_content).unwrap();

    let schema = read_schema(&file_path).unwrap();

    assert_eq!(schema.doc, Some("Manually written schema".to_string()));

    match &schema.kind {
        TypeKind::Struct { fields } => {
            assert_eq!(fields.len(), 2);
            assert_eq!(fields[0].name, "id");
            assert_eq!(fields[0].ty, TypeKind::U64);
            assert!(!fields[0].optional);
            assert_eq!(fields[1].name, "data");
            assert_eq!(fields[1].ty, TypeKind::String);
            assert!(fields[1].optional);
        }
        _ => panic!("Expected Struct type kind"),
    }
}

// ============================================================================
// Edge case tests
// ============================================================================

#[test]
fn test_type_path_with_empty_string() {
    use ron2::schema::storage::type_path_to_file_path;

    // Edge case: empty string should produce just the extension
    let path = type_path_to_file_path("");
    assert_eq!(path, PathBuf::from(".schema.ron"));
}

#[test]
fn test_write_schema_with_special_characters_in_type_name() {
    // Note: Rust type names can't contain special characters,
    // but we test with underscores and numbers which are valid
    let temp_dir = create_temp_dir();
    let schema = Schema::new(TypeKind::Bool);

    let path = write_schema(
        "my_crate_2::v2_api::Type_V2",
        &schema,
        Some(temp_dir.path()),
    )
    .unwrap();

    assert!(path.exists());
    let read_back = read_schema(&path).unwrap();
    assert_eq!(schema, read_back);
}

#[test]
fn test_multiple_schemas_in_same_directory() {
    let temp_dir = create_temp_dir();

    let schema1 = Schema::new(TypeKind::I32);
    let schema2 = Schema::new(TypeKind::String);
    let schema3 = Schema::new(TypeKind::Bool);

    write_schema("multi::Type1", &schema1, Some(temp_dir.path())).unwrap();
    write_schema("multi::Type2", &schema2, Some(temp_dir.path())).unwrap();
    write_schema("multi::Type3", &schema3, Some(temp_dir.path())).unwrap();

    let found1 = find_schema_in("multi::Type1", temp_dir.path()).unwrap();
    let found2 = find_schema_in("multi::Type2", temp_dir.path()).unwrap();
    let found3 = find_schema_in("multi::Type3", temp_dir.path()).unwrap();

    assert_eq!(schema1, found1);
    assert_eq!(schema2, found2);
    assert_eq!(schema3, found3);
}

#[test]
fn test_write_enum_schema() {
    use ron2::schema::Variant;

    let temp_dir = create_temp_dir();
    let schema = Schema::with_doc(
        "Status enum",
        TypeKind::Enum {
            variants: vec![
                Variant::unit("Pending"),
                Variant::unit("Running"),
                Variant::struct_variant("Completed", vec![Field::new("result", TypeKind::String)]),
                Variant::struct_variant("Failed", vec![Field::new("error", TypeKind::String)]),
            ],
        },
    );

    let path = write_schema("status::Status", &schema, Some(temp_dir.path())).unwrap();

    let read_back = read_schema(&path).unwrap();
    assert_eq!(schema, read_back);
}
