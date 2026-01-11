//! Tests for the RonSchema derive macro.

#![allow(dead_code)]

use ron2::ast::FormatConfig;
use ron2::schema::{RonSchemaType, TypeKind, VariantKind};
use ron2::{FromRon, ToRon};
use ron2_derive::{Ron, RonSchema};

/// A simple struct for testing.
#[derive(RonSchema)]
struct SimpleStruct {
    /// The name field.
    name: String,
    /// The count field.
    count: u32,
}

#[test]
fn test_simple_struct() {
    let schema = SimpleStruct::schema();

    assert_eq!(schema.doc, Some("A simple struct for testing.".to_string()));

    match &schema.kind {
        TypeKind::Struct { fields } => {
            assert_eq!(fields.len(), 2);

            assert_eq!(fields[0].name, "name");
            assert_eq!(fields[0].ty, TypeKind::String);
            assert_eq!(fields[0].doc, Some("The name field.".to_string()));
            assert!(!fields[0].optional);

            assert_eq!(fields[1].name, "count");
            assert_eq!(fields[1].ty, TypeKind::U32);
            assert_eq!(fields[1].doc, Some("The count field.".to_string()));
            assert!(!fields[1].optional);
        }
        _ => panic!("Expected struct type kind"),
    }
}

/// A struct with optional fields.
#[derive(RonSchema)]
struct StructWithOptional {
    /// Required field.
    required: i32,
    /// Optional field with default.
    #[ron_schema(default)]
    optional: Option<String>,
}

#[test]
fn test_optional_field() {
    let schema = StructWithOptional::schema();

    match &schema.kind {
        TypeKind::Struct { fields } => {
            assert_eq!(fields.len(), 2);

            assert!(!fields[0].optional);
            assert!(fields[1].optional);

            match &fields[1].ty {
                TypeKind::Option(inner) => {
                    assert_eq!(**inner, TypeKind::String);
                }
                _ => panic!("Expected Option type"),
            }
        }
        _ => panic!("Expected struct type kind"),
    }
}

/// Test enum with different variant types.
#[derive(Debug, Ron, PartialEq)]
enum TestEnum {
    /// Unit variant.
    Unit,
    /// Tuple variant.
    Tuple(i32, String),
    /// Struct variant.
    Struct {
        /// X coordinate.
        x: f32,
        /// Y coordinate.
        y: f32,
    },
}

#[test]
fn test_enum_tofrom() {
    assert_eq!(
        TestEnum::Struct { x: 1.0, y: 2.0 }
            .to_ron_with(&FormatConfig::minimal())
            .unwrap(),
        "Struct(x:1.0,y:2.0)"
    );

    let a: TestEnum = TestEnum::from_ron("Unit").unwrap();
    assert_eq!(a, TestEnum::Unit);

    let b: TestEnum = TestEnum::from_ron(r#"Tuple(1, "hello")"#).unwrap();
    assert_eq!(b, TestEnum::Tuple(1, "hello".to_string()));

    let c: TestEnum = TestEnum::from_ron(r#"Struct(x: 1.0, y: 2.0)"#).unwrap();
    assert_eq!(c, TestEnum::Struct { x: 1.0, y: 2.0 });
}

#[test]
fn test_enum() {
    let schema = TestEnum::schema();

    assert_eq!(
        schema.doc,
        Some("Test enum with different variant types.".to_string())
    );

    match &schema.kind {
        TypeKind::Enum { variants } => {
            assert_eq!(variants.len(), 3);

            // Unit variant
            assert_eq!(variants[0].name, "Unit");
            assert_eq!(variants[0].doc, Some("Unit variant.".to_string()));
            assert_eq!(variants[0].kind, VariantKind::Unit);

            // Tuple variant
            assert_eq!(variants[1].name, "Tuple");
            assert_eq!(variants[1].doc, Some("Tuple variant.".to_string()));
            match &variants[1].kind {
                VariantKind::Tuple(types) => {
                    assert_eq!(types.len(), 2);
                    assert_eq!(types[0], TypeKind::I32);
                    assert_eq!(types[1], TypeKind::String);
                }
                _ => panic!("Expected tuple variant"),
            }

            // Struct variant
            assert_eq!(variants[2].name, "Struct");
            match &variants[2].kind {
                VariantKind::Struct(fields) => {
                    assert_eq!(fields.len(), 2);
                    assert_eq!(fields[0].name, "x");
                    assert_eq!(fields[0].ty, TypeKind::F32);
                    assert_eq!(fields[1].name, "y");
                    assert_eq!(fields[1].ty, TypeKind::F32);
                }
                _ => panic!("Expected struct variant"),
            }
        }
        _ => panic!("Expected enum type kind"),
    }
}

/// Test struct with various collection types.
#[derive(RonSchema)]
struct CollectionTypes {
    /// A vector of integers.
    numbers: Vec<i32>,
    /// A map from strings to booleans.
    flags: std::collections::HashMap<String, bool>,
}

#[test]
fn test_collection_types() {
    let schema = CollectionTypes::schema();

    match &schema.kind {
        TypeKind::Struct { fields } => {
            // Vec field (represented as List)
            match &fields[0].ty {
                TypeKind::List(inner) => {
                    assert_eq!(**inner, TypeKind::I32);
                }
                _ => panic!("Expected List type"),
            }

            // HashMap field
            match &fields[1].ty {
                TypeKind::Map { key, value } => {
                    assert_eq!(**key, TypeKind::String);
                    assert_eq!(**value, TypeKind::Bool);
                }
                _ => panic!("Expected Map type"),
            }
        }
        _ => panic!("Expected struct type kind"),
    }
}

/// Unit struct test.
#[derive(RonSchema)]
struct UnitStruct;

#[test]
fn test_unit_struct() {
    let schema = UnitStruct::schema();
    assert_eq!(schema.kind, TypeKind::Unit);
}

/// Tuple struct test.
#[derive(RonSchema)]
struct TupleStruct(i32, String, bool);

#[test]
fn test_tuple_struct() {
    let schema = TupleStruct::schema();

    match &schema.kind {
        TypeKind::Tuple(types) => {
            assert_eq!(types.len(), 3);
            assert_eq!(types[0], TypeKind::I32);
            assert_eq!(types[1], TypeKind::String);
            assert_eq!(types[2], TypeKind::Bool);
        }
        _ => panic!("Expected tuple type kind"),
    }
}

/// Test type_path method.
#[test]
fn test_type_path() {
    let path = SimpleStruct::type_path().expect("type_path should return Some for derived types");
    assert!(path.ends_with("SimpleStruct"));
}

/// Struct with nested custom type.
struct CustomType {
    value: i32,
}

/// Test struct with type reference.
#[derive(RonSchema)]
struct WithTypeRef {
    /// A custom type field.
    custom: CustomType,
}

#[test]
fn test_type_ref() {
    let schema = WithTypeRef::schema();

    match &schema.kind {
        TypeKind::Struct { fields } => match &fields[0].ty {
            TypeKind::TypeRef(path) => {
                // TypeRef should be fully qualified with crate name
                assert!(
                    path.ends_with("::CustomType"),
                    "Expected path to end with ::CustomType, got: {}",
                    path
                );
            }
            _ => panic!("Expected TypeRef for custom type"),
        },
        _ => panic!("Expected struct type kind"),
    }
}

/// Test all primitive types.
#[derive(RonSchema)]
struct AllPrimitives {
    b: bool,
    i8: i8,
    i16: i16,
    i32: i32,
    i64: i64,
    i128: i128,
    u8: u8,
    u16: u16,
    u32: u32,
    u64: u64,
    u128: u128,
    f32: f32,
    f64: f64,
    c: char,
    s: String,
}

#[test]
fn test_all_primitives() {
    let schema = AllPrimitives::schema();

    match &schema.kind {
        TypeKind::Struct { fields } => {
            assert_eq!(fields[0].ty, TypeKind::Bool);
            assert_eq!(fields[1].ty, TypeKind::I8);
            assert_eq!(fields[2].ty, TypeKind::I16);
            assert_eq!(fields[3].ty, TypeKind::I32);
            assert_eq!(fields[4].ty, TypeKind::I64);
            assert_eq!(fields[5].ty, TypeKind::I128);
            assert_eq!(fields[6].ty, TypeKind::U8);
            assert_eq!(fields[7].ty, TypeKind::U16);
            assert_eq!(fields[8].ty, TypeKind::U32);
            assert_eq!(fields[9].ty, TypeKind::U64);
            assert_eq!(fields[10].ty, TypeKind::U128);
            assert_eq!(fields[11].ty, TypeKind::F32);
            assert_eq!(fields[12].ty, TypeKind::F64);
            assert_eq!(fields[13].ty, TypeKind::Char);
            assert_eq!(fields[14].ty, TypeKind::String);
        }
        _ => panic!("Expected struct type kind"),
    }
}

/// Struct with tuple field.
#[derive(RonSchema)]
struct WithTupleField {
    /// A tuple of (x, y) coordinates.
    coords: (f32, f32),
}

#[test]
fn test_tuple_field() {
    let schema = WithTupleField::schema();

    match &schema.kind {
        TypeKind::Struct { fields } => match &fields[0].ty {
            TypeKind::Tuple(types) => {
                assert_eq!(types.len(), 2);
                assert_eq!(types[0], TypeKind::F32);
                assert_eq!(types[1], TypeKind::F32);
            }
            _ => panic!("Expected Tuple type"),
        },
        _ => panic!("Expected struct type kind"),
    }
}

/// Struct with Box field.
#[derive(RonSchema)]
struct WithBox {
    /// A boxed value.
    boxed: Box<i32>,
}

#[test]
fn test_box_unwrapping() {
    let schema = WithBox::schema();

    match &schema.kind {
        TypeKind::Struct { fields } => {
            // Box<T> should be unwrapped to just T
            assert_eq!(fields[0].ty, TypeKind::I32);
        }
        _ => panic!("Expected struct type kind"),
    }
}

// =============================================================================
// New Feature Tests: flatten, skip, default
// =============================================================================

/// Base struct for flattening test.
#[derive(RonSchema)]
struct BaseConfig {
    /// The name.
    name: String,
    /// The version.
    version: u32,
}

/// Struct with flattened field.
#[derive(RonSchema)]
struct ExtendedConfig {
    /// Base configuration (flattened).
    #[ron_schema(flatten)]
    base: BaseConfig,
    /// Additional setting.
    extra: String,
}

#[test]
fn test_flatten_attribute() {
    let schema = ExtendedConfig::schema();

    match &schema.kind {
        TypeKind::Struct { fields } => {
            assert_eq!(fields.len(), 2);

            // First field should be flattened
            assert_eq!(fields[0].name, "base");
            assert!(fields[0].flattened, "Expected base field to be flattened");

            // Second field should not be flattened
            assert_eq!(fields[1].name, "extra");
            assert!(!fields[1].flattened);
        }
        _ => panic!("Expected struct type kind"),
    }
}

/// Struct with skipped field.
#[derive(RonSchema)]
struct WithSkippedField {
    /// Visible field.
    visible: String,
    /// Internal field (skipped).
    #[ron_schema(skip)]
    internal: i32,
    /// Another visible field.
    another: bool,
}

#[test]
fn test_skip_attribute() {
    let schema = WithSkippedField::schema();

    match &schema.kind {
        TypeKind::Struct { fields } => {
            // Should only have 2 fields, not 3
            assert_eq!(fields.len(), 2);
            assert_eq!(fields[0].name, "visible");
            assert_eq!(fields[1].name, "another");
        }
        _ => panic!("Expected struct type kind"),
    }
}

/// Struct with multiple attributes.
#[derive(RonSchema)]
struct WithMultipleAttrs {
    /// Normal field.
    normal: String,
    /// Optional flattened field.
    #[ron_schema(default, flatten)]
    optional_nested: BaseConfig,
}

#[test]
fn test_multiple_attributes() {
    let schema = WithMultipleAttrs::schema();

    match &schema.kind {
        TypeKind::Struct { fields } => {
            assert_eq!(fields.len(), 2);

            assert_eq!(fields[0].name, "normal");
            assert!(!fields[0].optional);
            assert!(!fields[0].flattened);

            assert_eq!(fields[1].name, "optional_nested");
            assert!(
                fields[1].optional,
                "Expected optional_nested to be optional"
            );
            assert!(
                fields[1].flattened,
                "Expected optional_nested to be flattened"
            );
        }
        _ => panic!("Expected struct type kind"),
    }
}

/// Enum with skipped variant field.
#[derive(RonSchema)]
enum EnumWithSkippedField {
    /// Variant with internal field skipped.
    Data {
        /// Public data.
        public: String,
        /// Internal cache (skipped).
        #[ron_schema(skip)]
        cache: Vec<u8>,
    },
}

#[test]
fn test_enum_skip_variant_field() {
    let schema = EnumWithSkippedField::schema();

    match &schema.kind {
        TypeKind::Enum { variants } => {
            assert_eq!(variants.len(), 1);
            assert_eq!(variants[0].name, "Data");

            match &variants[0].kind {
                VariantKind::Struct(fields) => {
                    // Should only have 1 field (cache is skipped)
                    assert_eq!(fields.len(), 1);
                    assert_eq!(fields[0].name, "public");
                }
                _ => panic!("Expected struct variant"),
            }
        }
        _ => panic!("Expected enum type kind"),
    }
}

// =============================================================================
// Field-Level Error Span Tests
// =============================================================================

/// Struct for testing field-level error spans.
#[derive(Debug, Ron, PartialEq)]
struct ConfigWithPort {
    name: String,
    port: u16,
    enabled: bool,
}

#[test]
fn test_field_level_error_span_invalid_type() {
    // The error should point to "not_a_number" (line 2, col 11), not the whole struct
    let input = r#"(
    port: "not_a_number",
    name: "test",
    enabled: true,
)"#;

    let err = ConfigWithPort::from_ron(input).unwrap_err();

    // The span should point to the invalid value "not_a_number"
    // which starts at line 2, column 11
    assert_eq!(err.span.start.line, 2, "Error should be on line 2");
    assert_eq!(
        err.span.start.col, 11,
        "Error should point to column 11 (start of string)"
    );
}

#[test]
fn test_field_level_error_span_missing_field() {
    // Missing required field 'port'
    let input = r#"(
    name: "test",
    enabled: true,
)"#;

    let err = ConfigWithPort::from_ron(input).unwrap_err();

    // For missing fields, the error points to the struct itself
    // The span should be at line 1 where the struct starts
    assert_eq!(err.span.start.line, 1, "Error should be on line 1");
}

#[test]
fn test_nested_field_error_span() {
    #[derive(Debug, Ron, PartialEq)]
    struct Inner {
        value: i32,
    }

    #[derive(Debug, Ron, PartialEq)]
    struct Outer {
        name: String,
        inner: Inner,
    }

    // Error in nested struct field
    let input = r#"(
    name: "test",
    inner: (
        value: "not_an_int",
    ),
)"#;

    let err = Outer::from_ron(input).unwrap_err();

    // Error should point to "not_an_int" at line 4
    assert_eq!(err.span.start.line, 4, "Error should be on line 4");
}

#[test]
fn test_enum_variant_field_error_span() {
    #[derive(Debug, Ron, PartialEq)]
    enum Message {
        Text { content: String },
        Number { value: i32 },
    }

    // Error in enum variant field
    let input = r#"Number(value: "not_a_number")"#;

    let err = Message::from_ron(input).unwrap_err();

    // Error should point to "not_a_number"
    assert_eq!(err.span.start.line, 1, "Error should be on line 1");
    assert_eq!(
        err.span.start.col, 15,
        "Error should point to the invalid string"
    );
}

#[test]
fn test_sequence_element_error_span() {
    #[derive(Debug, Ron, PartialEq)]
    struct WithVec {
        numbers: Vec<i32>,
    }

    // Error in sequence element
    let input = r#"(
    numbers: [1, 2, "three", 4],
)"#;

    let err = WithVec::from_ron(input).unwrap_err();

    // Error should point to "three" at line 2
    assert_eq!(err.span.start.line, 2, "Error should be on line 2");
}

#[test]
fn test_tuple_struct_element_error_span() {
    #[derive(Debug, Ron, PartialEq)]
    struct Point(i32, i32);

    // Error in second tuple element
    let input = r#"(1, "not_an_int")"#;

    let err = Point::from_ron(input).unwrap_err();

    // Error should point to "not_an_int"
    assert_eq!(err.span.start.line, 1, "Error should be on line 1");
    assert_eq!(
        err.span.start.col, 5,
        "Error should point to the invalid string"
    );
}
