//! Comprehensive tests for ron-schema types module.
//!
//! Tests cover:
//! - Schema serialization/deserialization roundtrips
//! - All TypeKind variants
//! - Field and Variant builders
//! - Nested types (Vec of Option of Struct, etc.)

#![cfg(feature = "derive")]

use ron2::{
    FromRon, ToRon,
    fmt::FormatConfig,
    schema::{Field, Schema, TypeKind, Variant, VariantKind},
};

// ============================================================================
// Schema tests
// ============================================================================

#[test]
fn test_schema_new() {
    let schema = Schema::new(TypeKind::Bool);
    assert_eq!(schema.doc, None);
    assert_eq!(schema.kind, TypeKind::Bool);
}

#[test]
fn test_schema_with_doc() {
    let schema = Schema::with_doc("A boolean value", TypeKind::Bool);
    assert_eq!(schema.doc, Some("A boolean value".to_string()));
    assert_eq!(schema.kind, TypeKind::Bool);
}

#[test]
fn test_schema_serialization_roundtrip_simple() {
    let schema = Schema::with_doc("Test schema", TypeKind::String);

    let serialized = schema.to_ron_with(&FormatConfig::default()).unwrap();
    let deserialized = Schema::from_ron(&serialized).unwrap();

    assert_eq!(schema, deserialized);
}

#[test]
fn test_schema_serialization_without_doc() {
    let schema = Schema::new(TypeKind::I32);

    let serialized = schema.to_ron_with(&FormatConfig::default()).unwrap();
    // doc should not appear in serialized output when None
    assert!(!serialized.contains("doc:"));

    let deserialized = Schema::from_ron(&serialized).unwrap();
    assert_eq!(schema, deserialized);
}

// ============================================================================
// TypeKind primitive tests
// ============================================================================

#[test]
fn test_type_kind_bool_roundtrip() {
    let kind = TypeKind::Bool;
    let serialized = kind.to_ron().unwrap();
    let deserialized = TypeKind::from_ron(&serialized).unwrap();
    assert_eq!(kind, deserialized);
}

#[test]
fn test_type_kind_all_integers_roundtrip() {
    let integer_types = vec![
        TypeKind::I8,
        TypeKind::I16,
        TypeKind::I32,
        TypeKind::I64,
        TypeKind::I128,
        TypeKind::U8,
        TypeKind::U16,
        TypeKind::U32,
        TypeKind::U64,
        TypeKind::U128,
    ];

    for kind in integer_types {
        let serialized = kind.to_ron().unwrap();
        let deserialized = TypeKind::from_ron(&serialized).unwrap();
        assert_eq!(kind, deserialized, "Failed for {:?}", kind);
    }
}

#[test]
fn test_type_kind_floats_roundtrip() {
    let float_types = vec![TypeKind::F32, TypeKind::F64];

    for kind in float_types {
        let serialized = kind.to_ron().unwrap();
        let deserialized = TypeKind::from_ron(&serialized).unwrap();
        assert_eq!(kind, deserialized, "Failed for {:?}", kind);
    }
}

#[test]
fn test_type_kind_char_roundtrip() {
    let kind = TypeKind::Char;
    let serialized = kind.to_ron().unwrap();
    let deserialized = TypeKind::from_ron(&serialized).unwrap();
    assert_eq!(kind, deserialized);
}

#[test]
fn test_type_kind_string_roundtrip() {
    let kind = TypeKind::String;
    let serialized = kind.to_ron().unwrap();
    let deserialized = TypeKind::from_ron(&serialized).unwrap();
    assert_eq!(kind, deserialized);
}

#[test]
fn test_type_kind_unit_roundtrip() {
    let kind = TypeKind::Unit;
    let serialized = kind.to_ron().unwrap();
    let deserialized = TypeKind::from_ron(&serialized).unwrap();
    assert_eq!(kind, deserialized);
}

// ============================================================================
// TypeKind compound types tests
// ============================================================================

#[test]
fn test_type_kind_option_roundtrip() {
    let kind = TypeKind::Option(Box::new(TypeKind::String));
    let serialized = kind.to_ron().unwrap();
    let deserialized = TypeKind::from_ron(&serialized).unwrap();
    assert_eq!(kind, deserialized);
}

#[test]
fn test_type_kind_vec_roundtrip() {
    let kind = TypeKind::List(Box::new(TypeKind::I32));
    let serialized = kind.to_ron().unwrap();
    let deserialized = TypeKind::from_ron(&serialized).unwrap();
    assert_eq!(kind, deserialized);
}

#[test]
fn test_type_kind_map_roundtrip() {
    let kind = TypeKind::Map {
        key: Box::new(TypeKind::String),
        value: Box::new(TypeKind::I64),
    };
    let serialized = kind.to_ron().unwrap();
    let deserialized = TypeKind::from_ron(&serialized).unwrap();
    assert_eq!(kind, deserialized);
}

#[test]
fn test_type_kind_tuple_roundtrip() {
    let kind = TypeKind::Tuple(vec![TypeKind::I32, TypeKind::String, TypeKind::Bool]);
    let serialized = kind.to_ron().unwrap();
    let deserialized = TypeKind::from_ron(&serialized).unwrap();
    assert_eq!(kind, deserialized);
}

#[test]
fn test_type_kind_empty_tuple_roundtrip() {
    let kind = TypeKind::Tuple(vec![]);
    let serialized = kind.to_ron().unwrap();
    let deserialized = TypeKind::from_ron(&serialized).unwrap();
    assert_eq!(kind, deserialized);
}

#[test]
fn test_type_kind_struct_roundtrip() {
    let kind = TypeKind::Struct {
        fields: vec![
            Field::new("name", TypeKind::String),
            Field::new("age", TypeKind::U8),
        ],
    };
    let serialized = kind.to_ron().unwrap();
    let deserialized = TypeKind::from_ron(&serialized).unwrap();
    assert_eq!(kind, deserialized);
}

#[test]
fn test_type_kind_empty_struct_roundtrip() {
    let kind = TypeKind::Struct { fields: vec![] };
    let serialized = kind.to_ron().unwrap();
    let deserialized = TypeKind::from_ron(&serialized).unwrap();
    assert_eq!(kind, deserialized);
}

#[test]
fn test_type_kind_enum_roundtrip() {
    let kind = TypeKind::Enum {
        variants: vec![
            Variant::unit("None"),
            Variant::tuple("Some", vec![TypeKind::String]),
        ],
    };
    let serialized = kind.to_ron().unwrap();
    let deserialized = TypeKind::from_ron(&serialized).unwrap();
    assert_eq!(kind, deserialized);
}

#[test]
fn test_type_kind_type_ref_roundtrip() {
    let kind = TypeKind::TypeRef("my_crate::config::AppConfig".to_string());
    let serialized = kind.to_ron().unwrap();
    let deserialized = TypeKind::from_ron(&serialized).unwrap();
    assert_eq!(kind, deserialized);
}

// ============================================================================
// Field builder tests
// ============================================================================

#[test]
fn test_field_new() {
    let field = Field::new("port", TypeKind::U16);
    assert_eq!(field.name, "port");
    assert_eq!(field.ty, TypeKind::U16);
    assert_eq!(field.doc, None);
    assert!(!field.optional);
}

#[test]
fn test_field_optional() {
    let field = Field::optional("host", TypeKind::String);
    assert_eq!(field.name, "host");
    assert_eq!(field.ty, TypeKind::String);
    assert_eq!(field.doc, None);
    assert!(field.optional);
}

#[test]
fn test_field_with_doc() {
    let field = Field::new("port", TypeKind::U16).with_doc("The server port number");
    assert_eq!(field.name, "port");
    assert_eq!(field.ty, TypeKind::U16);
    assert_eq!(field.doc, Some("The server port number".to_string()));
    assert!(!field.optional);
}

#[test]
fn test_field_optional_with_doc() {
    let field = Field::optional("timeout", TypeKind::U64).with_doc("Request timeout in ms");
    assert_eq!(field.name, "timeout");
    assert_eq!(field.ty, TypeKind::U64);
    assert_eq!(field.doc, Some("Request timeout in ms".to_string()));
    assert!(field.optional);
}

#[test]
fn test_field_serialization_skips_none_doc() {
    let field = Field::new("value", TypeKind::I32);
    let serialized = field.to_ron_with(&FormatConfig::default()).unwrap();
    assert!(!serialized.contains("doc:"));
}

#[test]
fn test_field_serialization_skips_false_optional() {
    let field = Field::new("value", TypeKind::I32);
    let serialized = field.to_ron_with(&FormatConfig::default()).unwrap();
    assert!(!serialized.contains("optional:"));
}

#[test]
fn test_field_serialization_includes_true_optional() {
    let field = Field::optional("value", TypeKind::I32);
    let serialized = field.to_ron_with(&FormatConfig::default()).unwrap();
    assert!(serialized.contains("optional: true"));
}

#[test]
fn test_field_roundtrip() {
    let field = Field::optional("config", TypeKind::String).with_doc("Configuration path");
    let serialized = field.to_ron().unwrap();
    let deserialized = Field::from_ron(&serialized).unwrap();
    assert_eq!(field, deserialized);
}

// ============================================================================
// Variant builder tests
// ============================================================================

#[test]
fn test_variant_unit() {
    let variant = Variant::unit("None");
    assert_eq!(variant.name, "None");
    assert_eq!(variant.doc, None);
    assert_eq!(variant.kind, VariantKind::Unit);
}

#[test]
fn test_variant_tuple() {
    let variant = Variant::tuple("Point", vec![TypeKind::F64, TypeKind::F64]);
    assert_eq!(variant.name, "Point");
    assert_eq!(variant.doc, None);
    assert_eq!(
        variant.kind,
        VariantKind::Tuple(vec![TypeKind::F64, TypeKind::F64])
    );
}

#[test]
fn test_variant_struct() {
    let fields = vec![
        Field::new("x", TypeKind::F64),
        Field::new("y", TypeKind::F64),
    ];
    let variant = Variant::struct_variant("Point", fields.clone());
    assert_eq!(variant.name, "Point");
    assert_eq!(variant.doc, None);
    assert_eq!(variant.kind, VariantKind::Struct(fields));
}

#[test]
fn test_variant_with_doc() {
    let variant = Variant::unit("Success").with_doc("Operation completed successfully");
    assert_eq!(variant.name, "Success");
    assert_eq!(
        variant.doc,
        Some("Operation completed successfully".to_string())
    );
    assert_eq!(variant.kind, VariantKind::Unit);
}

#[test]
fn test_variant_roundtrip_unit() {
    let variant = Variant::unit("Empty").with_doc("An empty value");
    let serialized = variant.to_ron().unwrap();
    let deserialized = Variant::from_ron(&serialized).unwrap();
    assert_eq!(variant, deserialized);
}

#[test]
fn test_variant_roundtrip_tuple() {
    let variant = Variant::tuple("Rgb", vec![TypeKind::U8, TypeKind::U8, TypeKind::U8]);
    let serialized = variant.to_ron().unwrap();
    let deserialized = Variant::from_ron(&serialized).unwrap();
    assert_eq!(variant, deserialized);
}

#[test]
fn test_variant_roundtrip_struct() {
    let variant = Variant::struct_variant(
        "Person",
        vec![
            Field::new("name", TypeKind::String),
            Field::optional("age", TypeKind::U8),
        ],
    );
    let serialized = variant.to_ron().unwrap();
    let deserialized = Variant::from_ron(&serialized).unwrap();
    assert_eq!(variant, deserialized);
}

// ============================================================================
// Nested types tests
// ============================================================================

#[test]
fn test_nested_vec_of_option() {
    let kind = TypeKind::List(Box::new(TypeKind::Option(Box::new(TypeKind::String))));
    let serialized = kind.to_ron().unwrap();
    let deserialized = TypeKind::from_ron(&serialized).unwrap();
    assert_eq!(kind, deserialized);
}

#[test]
fn test_nested_option_of_vec() {
    let kind = TypeKind::Option(Box::new(TypeKind::List(Box::new(TypeKind::I32))));
    let serialized = kind.to_ron().unwrap();
    let deserialized = TypeKind::from_ron(&serialized).unwrap();
    assert_eq!(kind, deserialized);
}

#[test]
fn test_nested_map_with_complex_value() {
    let kind = TypeKind::Map {
        key: Box::new(TypeKind::String),
        value: Box::new(TypeKind::List(Box::new(TypeKind::Option(Box::new(
            TypeKind::I32,
        ))))),
    };
    let serialized = kind.to_ron().unwrap();
    let deserialized = TypeKind::from_ron(&serialized).unwrap();
    assert_eq!(kind, deserialized);
}

#[test]
fn test_nested_struct_with_complex_fields() {
    let kind = TypeKind::Struct {
        fields: vec![
            Field::new("items", TypeKind::List(Box::new(TypeKind::String))),
            Field::optional(
                "metadata",
                TypeKind::Map {
                    key: Box::new(TypeKind::String),
                    value: Box::new(TypeKind::String),
                },
            ),
            Field::new(
                "nested",
                TypeKind::Option(Box::new(TypeKind::TypeRef("OtherType".to_string()))),
            ),
        ],
    };
    let serialized = kind.to_ron().unwrap();
    let deserialized = TypeKind::from_ron(&serialized).unwrap();
    assert_eq!(kind, deserialized);
}

#[test]
fn test_deeply_nested_types() {
    // Vec<Option<Map<String, Vec<Option<i32>>>>>
    let kind = TypeKind::List(Box::new(TypeKind::Option(Box::new(TypeKind::Map {
        key: Box::new(TypeKind::String),
        value: Box::new(TypeKind::List(Box::new(TypeKind::Option(Box::new(
            TypeKind::I32,
        ))))),
    }))));

    let serialized = kind.to_ron().unwrap();
    let deserialized = TypeKind::from_ron(&serialized).unwrap();
    assert_eq!(kind, deserialized);
}

#[test]
fn test_enum_with_all_variant_types() {
    let kind = TypeKind::Enum {
        variants: vec![
            Variant::unit("Unit").with_doc("A unit variant"),
            Variant::tuple("Tuple", vec![TypeKind::I32, TypeKind::String]),
            Variant::struct_variant(
                "Struct",
                vec![
                    Field::new("field1", TypeKind::Bool),
                    Field::optional("field2", TypeKind::F64).with_doc("An optional field"),
                ],
            ),
        ],
    };

    let serialized = kind.to_ron().unwrap();
    let deserialized = TypeKind::from_ron(&serialized).unwrap();
    assert_eq!(kind, deserialized);
}

// ============================================================================
// Complex schema roundtrip tests
// ============================================================================

#[test]
fn test_complex_schema_roundtrip() {
    let schema = Schema::with_doc(
        "Application configuration",
        TypeKind::Struct {
            fields: vec![
                Field::new("port", TypeKind::U16).with_doc("Server port"),
                Field::optional("host", TypeKind::String).with_doc("Hostname"),
                Field::new(
                    "endpoints",
                    TypeKind::List(Box::new(TypeKind::Struct {
                        fields: vec![
                            Field::new("path", TypeKind::String),
                            Field::new("method", TypeKind::String),
                        ],
                    })),
                ),
                Field::optional(
                    "settings",
                    TypeKind::Map {
                        key: Box::new(TypeKind::String),
                        value: Box::new(TypeKind::String),
                    },
                ),
            ],
        },
    );

    let serialized = schema.to_ron_with(&FormatConfig::default()).unwrap();
    let deserialized = Schema::from_ron(&serialized).unwrap();
    assert_eq!(schema, deserialized);
}

#[test]
fn test_enum_schema_roundtrip() {
    let schema = Schema::with_doc(
        "A result type",
        TypeKind::Enum {
            variants: vec![
                Variant::struct_variant(
                    "Ok",
                    vec![Field::new("value", TypeKind::TypeRef("T".to_string()))],
                )
                .with_doc("Success with value"),
                Variant::struct_variant("Err", vec![Field::new("error", TypeKind::String)])
                    .with_doc("Error with message"),
            ],
        },
    );

    let serialized = schema.to_ron_with(&FormatConfig::default()).unwrap();
    let deserialized = Schema::from_ron(&serialized).unwrap();
    assert_eq!(schema, deserialized);
}

#[test]
fn test_tuple_type_inside_struct() {
    let schema = Schema::new(TypeKind::Struct {
        fields: vec![
            Field::new(
                "position",
                TypeKind::Tuple(vec![TypeKind::F64, TypeKind::F64, TypeKind::F64]),
            ),
            Field::new(
                "color",
                TypeKind::Tuple(vec![TypeKind::U8, TypeKind::U8, TypeKind::U8, TypeKind::U8]),
            ),
        ],
    });

    let serialized = schema.to_ron().unwrap();
    let deserialized = Schema::from_ron(&serialized).unwrap();
    assert_eq!(schema, deserialized);
}
