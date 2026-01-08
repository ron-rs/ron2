//! Comprehensive tests for ron-schema validation module.
//!
//! Tests cover:
//! - All primitive type validations
//! - Complex nested structure validation
//! - All error types are produced correctly
//! - Optional field handling
//! - Enum validation with all variant types
//! - Map validation with various key/value types
//! - Tuple validation with length checking

use ron::Value;
use ron_schema::{validate, validate_type, Field, Schema, TypeKind, ValidationError, Variant};

/// Helper to parse RON string into Value
fn parse_ron(s: &str) -> Value {
    ron::from_str(s).expect("Failed to parse RON")
}

// ============================================================================
// Primitive type validation tests
// ============================================================================

#[test]
fn test_validate_bool_true() {
    assert!(validate_type(&Value::Bool(true), &TypeKind::Bool).is_ok());
}

#[test]
fn test_validate_bool_false() {
    assert!(validate_type(&Value::Bool(false), &TypeKind::Bool).is_ok());
}

#[test]
fn test_validate_bool_mismatch() {
    let result = validate_type(&Value::String("true".into()), &TypeKind::Bool);
    assert!(matches!(result, Err(ValidationError::TypeMismatch { .. })));
}

#[test]
fn test_validate_all_signed_integers() {
    let value = Value::Number(42.into());

    assert!(validate_type(&value, &TypeKind::I8).is_ok());
    assert!(validate_type(&value, &TypeKind::I16).is_ok());
    assert!(validate_type(&value, &TypeKind::I32).is_ok());
    assert!(validate_type(&value, &TypeKind::I64).is_ok());
    assert!(validate_type(&value, &TypeKind::I128).is_ok());
}

#[test]
fn test_validate_all_unsigned_integers() {
    let value = Value::Number(42.into());

    assert!(validate_type(&value, &TypeKind::U8).is_ok());
    assert!(validate_type(&value, &TypeKind::U16).is_ok());
    assert!(validate_type(&value, &TypeKind::U32).is_ok());
    assert!(validate_type(&value, &TypeKind::U64).is_ok());
    assert!(validate_type(&value, &TypeKind::U128).is_ok());
}

#[test]
fn test_validate_integer_mismatch() {
    let result = validate_type(&Value::String("42".into()), &TypeKind::I32);
    assert!(matches!(result, Err(ValidationError::TypeMismatch { .. })));
}

#[test]
fn test_validate_f32() {
    let value = parse_ron("3.14");
    assert!(validate_type(&value, &TypeKind::F32).is_ok());
}

#[test]
fn test_validate_f64() {
    let value = parse_ron("3.14159265359");
    assert!(validate_type(&value, &TypeKind::F64).is_ok());
}

#[test]
fn test_validate_float_accepts_integer() {
    // RON parses integers as numbers, which can be validated as floats
    let value = Value::Number(42.into());
    assert!(validate_type(&value, &TypeKind::F32).is_ok());
    assert!(validate_type(&value, &TypeKind::F64).is_ok());
}

#[test]
fn test_validate_float_mismatch() {
    let result = validate_type(&Value::String("3.14".into()), &TypeKind::F64);
    assert!(matches!(result, Err(ValidationError::TypeMismatch { .. })));
}

#[test]
fn test_validate_char() {
    assert!(validate_type(&Value::Char('a'), &TypeKind::Char).is_ok());
    assert!(validate_type(&Value::Char('\n'), &TypeKind::Char).is_ok());
    assert!(validate_type(&Value::Char('\u{1F600}'), &TypeKind::Char).is_ok());
}

#[test]
fn test_validate_char_mismatch() {
    let result = validate_type(&Value::String("a".into()), &TypeKind::Char);
    assert!(matches!(result, Err(ValidationError::TypeMismatch { .. })));
}

#[test]
fn test_validate_string() {
    assert!(validate_type(&Value::String("hello".into()), &TypeKind::String).is_ok());
    assert!(validate_type(&Value::String("".into()), &TypeKind::String).is_ok());
    assert!(validate_type(
        &Value::String("unicode: \u{1F600}".into()),
        &TypeKind::String
    )
    .is_ok());
}

#[test]
fn test_validate_string_mismatch() {
    let result = validate_type(&Value::Number(42.into()), &TypeKind::String);
    assert!(matches!(result, Err(ValidationError::TypeMismatch { .. })));
}

#[test]
fn test_validate_unit() {
    assert!(validate_type(&Value::Unit, &TypeKind::Unit).is_ok());
}

#[test]
fn test_validate_unit_mismatch() {
    let result = validate_type(&Value::Bool(true), &TypeKind::Unit);
    assert!(matches!(result, Err(ValidationError::TypeMismatch { .. })));
}

// ============================================================================
// Option type validation tests
// ============================================================================

#[test]
fn test_validate_option_none() {
    let kind = TypeKind::Option(Box::new(TypeKind::String));
    assert!(validate_type(&Value::Option(None), &kind).is_ok());
}

#[test]
fn test_validate_option_some() {
    let kind = TypeKind::Option(Box::new(TypeKind::String));
    let value = Value::Option(Some(Box::new(Value::String("hello".into()))));
    assert!(validate_type(&value, &kind).is_ok());
}

#[test]
fn test_validate_option_some_wrong_inner_type() {
    let kind = TypeKind::Option(Box::new(TypeKind::String));
    let value = Value::Option(Some(Box::new(Value::Number(42.into()))));
    let result = validate_type(&value, &kind);
    assert!(matches!(result, Err(ValidationError::TypeMismatch { .. })));
}

#[test]
fn test_validate_option_mismatch() {
    let kind = TypeKind::Option(Box::new(TypeKind::String));
    let result = validate_type(&Value::String("hello".into()), &kind);
    assert!(matches!(result, Err(ValidationError::TypeMismatch { .. })));
}

#[test]
fn test_validate_nested_option() {
    let kind = TypeKind::Option(Box::new(TypeKind::Option(Box::new(TypeKind::I32))));

    // None
    assert!(validate_type(&Value::Option(None), &kind).is_ok());

    // Some(None)
    let value = Value::Option(Some(Box::new(Value::Option(None))));
    assert!(validate_type(&value, &kind).is_ok());

    // Some(Some(42))
    let value = Value::Option(Some(Box::new(Value::Option(Some(Box::new(Value::Number(
        42.into(),
    )))))));
    assert!(validate_type(&value, &kind).is_ok());
}

// ============================================================================
// Vec type validation tests
// ============================================================================

#[test]
fn test_validate_vec_empty() {
    let kind = TypeKind::Vec(Box::new(TypeKind::I32));
    assert!(validate_type(&Value::Seq(vec![]), &kind).is_ok());
}

#[test]
fn test_validate_vec_with_elements() {
    let kind = TypeKind::Vec(Box::new(TypeKind::I32));
    let value = Value::Seq(vec![
        Value::Number(1.into()),
        Value::Number(2.into()),
        Value::Number(3.into()),
    ]);
    assert!(validate_type(&value, &kind).is_ok());
}

#[test]
fn test_validate_vec_with_wrong_element_type() {
    let kind = TypeKind::Vec(Box::new(TypeKind::I32));
    let value = Value::Seq(vec![
        Value::Number(1.into()),
        Value::String("bad".into()),
        Value::Number(3.into()),
    ]);
    let result = validate_type(&value, &kind);
    assert!(matches!(
        result,
        Err(ValidationError::ElementError { index: 1, .. })
    ));
}

#[test]
fn test_validate_vec_all_wrong_type() {
    let kind = TypeKind::Vec(Box::new(TypeKind::String));
    let value = Value::Seq(vec![Value::Number(1.into()), Value::Number(2.into())]);
    let result = validate_type(&value, &kind);
    assert!(matches!(
        result,
        Err(ValidationError::ElementError { index: 0, .. })
    ));
}

#[test]
fn test_validate_vec_mismatch() {
    let kind = TypeKind::Vec(Box::new(TypeKind::I32));
    let result = validate_type(&Value::String("not a vec".into()), &kind);
    assert!(matches!(result, Err(ValidationError::TypeMismatch { .. })));
}

#[test]
fn test_validate_vec_of_options() {
    let kind = TypeKind::Vec(Box::new(TypeKind::Option(Box::new(TypeKind::String))));
    let value = Value::Seq(vec![
        Value::Option(Some(Box::new(Value::String("hello".into())))),
        Value::Option(None),
        Value::Option(Some(Box::new(Value::String("world".into())))),
    ]);
    assert!(validate_type(&value, &kind).is_ok());
}

// ============================================================================
// Tuple type validation tests
// ============================================================================

#[test]
fn test_validate_tuple_empty() {
    let kind = TypeKind::Tuple(vec![]);
    assert!(validate_type(&Value::Seq(vec![]), &kind).is_ok());
}

#[test]
fn test_validate_tuple_single_element() {
    let kind = TypeKind::Tuple(vec![TypeKind::I32]);
    let value = Value::Seq(vec![Value::Number(42.into())]);
    assert!(validate_type(&value, &kind).is_ok());
}

#[test]
fn test_validate_tuple_multiple_elements() {
    let kind = TypeKind::Tuple(vec![TypeKind::I32, TypeKind::String, TypeKind::Bool]);
    let value = Value::Seq(vec![
        Value::Number(42.into()),
        Value::String("hello".into()),
        Value::Bool(true),
    ]);
    assert!(validate_type(&value, &kind).is_ok());
}

#[test]
fn test_validate_tuple_length_too_short() {
    let kind = TypeKind::Tuple(vec![TypeKind::I32, TypeKind::String]);
    let value = Value::Seq(vec![Value::Number(42.into())]);
    let result = validate_type(&value, &kind);
    assert!(matches!(
        result,
        Err(ValidationError::TupleLengthMismatch {
            expected: 2,
            actual: 1
        })
    ));
}

#[test]
fn test_validate_tuple_length_too_long() {
    let kind = TypeKind::Tuple(vec![TypeKind::I32]);
    let value = Value::Seq(vec![Value::Number(1.into()), Value::Number(2.into())]);
    let result = validate_type(&value, &kind);
    assert!(matches!(
        result,
        Err(ValidationError::TupleLengthMismatch {
            expected: 1,
            actual: 2
        })
    ));
}

#[test]
fn test_validate_tuple_wrong_element_type() {
    let kind = TypeKind::Tuple(vec![TypeKind::I32, TypeKind::String]);
    let value = Value::Seq(vec![Value::Number(42.into()), Value::Number(100.into())]);
    let result = validate_type(&value, &kind);
    assert!(matches!(
        result,
        Err(ValidationError::ElementError { index: 1, .. })
    ));
}

#[test]
fn test_validate_tuple_mismatch() {
    let kind = TypeKind::Tuple(vec![TypeKind::I32]);
    let result = validate_type(&Value::String("not a tuple".into()), &kind);
    assert!(matches!(result, Err(ValidationError::TypeMismatch { .. })));
}

// ============================================================================
// Map type validation tests
// ============================================================================

#[test]
fn test_validate_map_empty() {
    let kind = TypeKind::Map {
        key: Box::new(TypeKind::String),
        value: Box::new(TypeKind::I32),
    };
    let value = Value::Map(Default::default());
    assert!(validate_type(&value, &kind).is_ok());
}

#[test]
fn test_validate_map_with_entries() {
    let kind = TypeKind::Map {
        key: Box::new(TypeKind::String),
        value: Box::new(TypeKind::I32),
    };
    let value: Value = parse_ron(r#"{ "a": 1, "b": 2, "c": 3 }"#);
    assert!(validate_type(&value, &kind).is_ok());
}

#[test]
fn test_validate_map_wrong_key_type() {
    let kind = TypeKind::Map {
        key: Box::new(TypeKind::String),
        value: Box::new(TypeKind::I32),
    };
    // RON maps with integer keys
    let value: Value = parse_ron(r#"{ 1: 100 }"#);
    let result = validate_type(&value, &kind);
    assert!(matches!(result, Err(ValidationError::MapKeyError { .. })));
}

#[test]
fn test_validate_map_wrong_value_type() {
    let kind = TypeKind::Map {
        key: Box::new(TypeKind::String),
        value: Box::new(TypeKind::I32),
    };
    let value: Value = parse_ron(r#"{ "key": "not an int" }"#);
    let result = validate_type(&value, &kind);
    assert!(matches!(result, Err(ValidationError::MapValueError { .. })));
}

#[test]
fn test_validate_map_integer_keys() {
    let kind = TypeKind::Map {
        key: Box::new(TypeKind::I32),
        value: Box::new(TypeKind::String),
    };
    let value: Value = parse_ron(r#"{ 1: "one", 2: "two" }"#);
    assert!(validate_type(&value, &kind).is_ok());
}

#[test]
fn test_validate_map_mismatch() {
    let kind = TypeKind::Map {
        key: Box::new(TypeKind::String),
        value: Box::new(TypeKind::I32),
    };
    let result = validate_type(&Value::Seq(vec![]), &kind);
    assert!(matches!(result, Err(ValidationError::TypeMismatch { .. })));
}

#[test]
fn test_validate_map_complex_value() {
    let kind = TypeKind::Map {
        key: Box::new(TypeKind::String),
        value: Box::new(TypeKind::Vec(Box::new(TypeKind::I32))),
    };
    let value: Value = parse_ron(r#"{ "nums": [1, 2, 3], "more": [4, 5] }"#);
    assert!(validate_type(&value, &kind).is_ok());
}

// ============================================================================
// Struct validation tests
// ============================================================================

#[test]
fn test_validate_struct_all_required_fields() {
    let schema = Schema::new(TypeKind::Struct {
        fields: vec![
            Field::new("name", TypeKind::String),
            Field::new("age", TypeKind::U8),
        ],
    });
    let value: Value = parse_ron(r#"(name: "Alice", age: 30)"#);
    assert!(validate(&value, &schema).is_ok());
}

#[test]
fn test_validate_struct_missing_required_field() {
    let schema = Schema::new(TypeKind::Struct {
        fields: vec![
            Field::new("name", TypeKind::String),
            Field::new("age", TypeKind::U8),
        ],
    });
    let value: Value = parse_ron(r#"(name: "Alice")"#);
    let result = validate(&value, &schema);
    assert!(matches!(
        result,
        Err(ValidationError::MissingField(ref f)) if f == "age"
    ));
}

#[test]
fn test_validate_struct_optional_field_present() {
    let schema = Schema::new(TypeKind::Struct {
        fields: vec![
            Field::new("name", TypeKind::String),
            Field::optional("nickname", TypeKind::String),
        ],
    });
    let value: Value = parse_ron(r#"(name: "Alice", nickname: "Ali")"#);
    assert!(validate(&value, &schema).is_ok());
}

#[test]
fn test_validate_struct_optional_field_absent() {
    let schema = Schema::new(TypeKind::Struct {
        fields: vec![
            Field::new("name", TypeKind::String),
            Field::optional("nickname", TypeKind::String),
        ],
    });
    let value: Value = parse_ron(r#"(name: "Alice")"#);
    assert!(validate(&value, &schema).is_ok());
}

#[test]
fn test_validate_struct_unknown_field() {
    let schema = Schema::new(TypeKind::Struct {
        fields: vec![Field::new("name", TypeKind::String)],
    });
    let value: Value = parse_ron(r#"(name: "Alice", extra: "field")"#);
    let result = validate(&value, &schema);
    assert!(matches!(
        result,
        Err(ValidationError::UnknownField(ref f)) if f == "extra"
    ));
}

#[test]
fn test_validate_struct_wrong_field_type() {
    let schema = Schema::new(TypeKind::Struct {
        fields: vec![Field::new("age", TypeKind::U8)],
    });
    let value: Value = parse_ron(r#"(age: "not a number")"#);
    let result = validate(&value, &schema);
    assert!(matches!(
        result,
        Err(ValidationError::FieldError { ref field, .. }) if field == "age"
    ));
}

#[test]
fn test_validate_struct_empty() {
    let schema = Schema::new(TypeKind::Struct { fields: vec![] });
    // RON parses () as Unit, so use {} for an empty map which represents an empty struct
    let value: Value = parse_ron(r#"{}"#);
    assert!(validate(&value, &schema).is_ok());
}

#[test]
fn test_validate_struct_all_optional() {
    let schema = Schema::new(TypeKind::Struct {
        fields: vec![
            Field::optional("a", TypeKind::I32),
            Field::optional("b", TypeKind::String),
        ],
    });
    // RON parses () as Unit, so use {} for an empty map which represents a struct with all optional fields omitted
    let value: Value = parse_ron(r#"{}"#);
    assert!(validate(&value, &schema).is_ok());
}

#[test]
fn test_validate_struct_nested() {
    let schema = Schema::new(TypeKind::Struct {
        fields: vec![
            Field::new("name", TypeKind::String),
            Field::new(
                "address",
                TypeKind::Struct {
                    fields: vec![
                        Field::new("street", TypeKind::String),
                        Field::new("city", TypeKind::String),
                    ],
                },
            ),
        ],
    });
    let value: Value = parse_ron(
        r#"(
        name: "Alice",
        address: (
            street: "123 Main St",
            city: "Springfield"
        )
    )"#,
    );
    assert!(validate(&value, &schema).is_ok());
}

#[test]
fn test_validate_struct_nested_error_propagation() {
    let schema = Schema::new(TypeKind::Struct {
        fields: vec![Field::new(
            "address",
            TypeKind::Struct {
                fields: vec![Field::new("city", TypeKind::String)],
            },
        )],
    });
    let value: Value = parse_ron(r#"(address: (city: 123))"#);
    let result = validate(&value, &schema);
    assert!(matches!(
        result,
        Err(ValidationError::FieldError { ref field, .. }) if field == "address"
    ));
}

// ============================================================================
// Enum validation tests
// ============================================================================

#[test]
fn test_validate_enum_unit_variant_as_string() {
    let schema = Schema::new(TypeKind::Enum {
        variants: vec![Variant::unit("None"), Variant::unit("Some")],
    });
    let value: Value = parse_ron(r#""None""#);
    assert!(validate(&value, &schema).is_ok());
}

#[test]
fn test_validate_enum_unknown_variant() {
    let schema = Schema::new(TypeKind::Enum {
        variants: vec![Variant::unit("Known")],
    });
    let value: Value = parse_ron(r#""Unknown""#);
    let result = validate(&value, &schema);
    assert!(matches!(
        result,
        Err(ValidationError::UnknownVariant(ref v)) if v == "Unknown"
    ));
}

#[test]
fn test_validate_enum_tuple_variant() {
    let schema = Schema::new(TypeKind::Enum {
        variants: vec![Variant::tuple("Point", vec![TypeKind::I32, TypeKind::I32])],
    });
    let value: Value = parse_ron(r#"{ "Point": [10, 20] }"#);
    assert!(validate(&value, &schema).is_ok());
}

#[test]
fn test_validate_enum_tuple_variant_wrong_length() {
    let schema = Schema::new(TypeKind::Enum {
        variants: vec![Variant::tuple("Pair", vec![TypeKind::I32, TypeKind::I32])],
    });
    let value: Value = parse_ron(r#"{ "Pair": [1, 2, 3] }"#);
    let result = validate(&value, &schema);
    assert!(matches!(
        result,
        Err(ValidationError::VariantError { ref variant, .. }) if variant == "Pair"
    ));
}

#[test]
fn test_validate_enum_tuple_variant_wrong_type() {
    let schema = Schema::new(TypeKind::Enum {
        variants: vec![Variant::tuple("Data", vec![TypeKind::I32])],
    });
    let value: Value = parse_ron(r#"{ "Data": ["not an int"] }"#);
    let result = validate(&value, &schema);
    assert!(matches!(
        result,
        Err(ValidationError::VariantError { ref variant, .. }) if variant == "Data"
    ));
}

#[test]
fn test_validate_enum_struct_variant() {
    let schema = Schema::new(TypeKind::Enum {
        variants: vec![Variant::struct_variant(
            "Person",
            vec![
                Field::new("name", TypeKind::String),
                Field::new("age", TypeKind::U8),
            ],
        )],
    });
    let value: Value = parse_ron(r#"{ "Person": (name: "Alice", age: 30) }"#);
    assert!(validate(&value, &schema).is_ok());
}

#[test]
fn test_validate_enum_struct_variant_missing_field() {
    let schema = Schema::new(TypeKind::Enum {
        variants: vec![Variant::struct_variant(
            "Person",
            vec![
                Field::new("name", TypeKind::String),
                Field::new("age", TypeKind::U8),
            ],
        )],
    });
    let value: Value = parse_ron(r#"{ "Person": (name: "Alice") }"#);
    let result = validate(&value, &schema);
    assert!(matches!(
        result,
        Err(ValidationError::VariantError { ref variant, .. }) if variant == "Person"
    ));
}

#[test]
fn test_validate_enum_struct_variant_with_optional() {
    let schema = Schema::new(TypeKind::Enum {
        variants: vec![Variant::struct_variant(
            "Config",
            vec![
                Field::new("path", TypeKind::String),
                Field::optional("debug", TypeKind::Bool),
            ],
        )],
    });
    let value: Value = parse_ron(r#"{ "Config": (path: "/etc/config") }"#);
    assert!(validate(&value, &schema).is_ok());
}

#[test]
fn test_validate_enum_unit_variant_with_unit_value() {
    let schema = Schema::new(TypeKind::Enum {
        variants: vec![Variant::unit("Empty")],
    });
    let value: Value = parse_ron(r#"{ "Empty": () }"#);
    assert!(validate(&value, &schema).is_ok());
}

#[test]
fn test_validate_enum_all_variant_types() {
    let schema = Schema::new(TypeKind::Enum {
        variants: vec![
            Variant::unit("Unit"),
            Variant::tuple("Tuple", vec![TypeKind::I32, TypeKind::String]),
            Variant::struct_variant(
                "Struct",
                vec![Field::new("value", TypeKind::Bool)],
            ),
        ],
    });

    // Unit
    let value: Value = parse_ron(r#""Unit""#);
    assert!(validate(&value, &schema).is_ok());

    // Tuple
    let value: Value = parse_ron(r#"{ "Tuple": [42, "hello"] }"#);
    assert!(validate(&value, &schema).is_ok());

    // Struct
    let value: Value = parse_ron(r#"{ "Struct": (value: true) }"#);
    assert!(validate(&value, &schema).is_ok());
}

// ============================================================================
// TypeRef validation tests
// ============================================================================

#[test]
fn test_validate_type_ref_accepts_any() {
    // TypeRef currently accepts any value since it requires loading external schemas
    let kind = TypeKind::TypeRef("some::Type".to_string());

    assert!(validate_type(&Value::Bool(true), &kind).is_ok());
    assert!(validate_type(&Value::Number(42.into()), &kind).is_ok());
    assert!(validate_type(&Value::String("hello".into()), &kind).is_ok());
    assert!(validate_type(&Value::Seq(vec![]), &kind).is_ok());
}

// ============================================================================
// Complex nested validation tests
// ============================================================================

#[test]
fn test_validate_deeply_nested_structure() {
    let schema = Schema::new(TypeKind::Struct {
        fields: vec![Field::new(
            "data",
            TypeKind::Vec(Box::new(TypeKind::Map {
                key: Box::new(TypeKind::String),
                value: Box::new(TypeKind::Option(Box::new(TypeKind::Struct {
                    fields: vec![
                        Field::new("id", TypeKind::I32),
                        Field::optional("tags", TypeKind::Vec(Box::new(TypeKind::String))),
                    ],
                }))),
            })),
        )],
    });

    let value: Value = parse_ron(
        r#"(
        data: [
            {
                "item1": Some((id: 1, tags: ["a", "b"])),
                "item2": None
            },
            {
                "item3": Some((id: 2))
            }
        ]
    )"#,
    );

    assert!(validate(&value, &schema).is_ok());
}

#[test]
fn test_validate_error_path_in_nested_structure() {
    let schema = Schema::new(TypeKind::Struct {
        fields: vec![Field::new(
            "items",
            TypeKind::Vec(Box::new(TypeKind::Struct {
                fields: vec![Field::new("value", TypeKind::I32)],
            })),
        )],
    });

    let value: Value = parse_ron(
        r#"(
        items: [
            (value: 1),
            (value: "bad"),
            (value: 3)
        ]
    )"#,
    );

    let result = validate(&value, &schema);
    // Should report error in items field, element 1, value field
    assert!(matches!(
        result,
        Err(ValidationError::FieldError { ref field, .. }) if field == "items"
    ));
}

// ============================================================================
// Error type tests
// ============================================================================

#[test]
fn test_error_type_mismatch_message() {
    let result = validate_type(&Value::Bool(true), &TypeKind::String);
    if let Err(ValidationError::TypeMismatch { expected, actual }) = result {
        assert_eq!(expected, "String");
        assert_eq!(actual, "Bool");
    } else {
        panic!("Expected TypeMismatch error");
    }
}

#[test]
fn test_error_missing_field_message() {
    let schema = Schema::new(TypeKind::Struct {
        fields: vec![Field::new("required_field", TypeKind::I32)],
    });
    // Use {} for empty map (struct with no fields provided)
    let value: Value = parse_ron(r#"{}"#);
    let result = validate(&value, &schema);
    if let Err(ValidationError::MissingField(field)) = result {
        assert_eq!(field, "required_field");
    } else {
        panic!("Expected MissingField error");
    }
}

#[test]
fn test_error_unknown_field_message() {
    let schema = Schema::new(TypeKind::Struct { fields: vec![] });
    let value: Value = parse_ron(r#"(unknown: 42)"#);
    let result = validate(&value, &schema);
    if let Err(ValidationError::UnknownField(field)) = result {
        assert_eq!(field, "unknown");
    } else {
        panic!("Expected UnknownField error");
    }
}

#[test]
fn test_error_unknown_variant_message() {
    let schema = Schema::new(TypeKind::Enum {
        variants: vec![Variant::unit("Known")],
    });
    let value: Value = parse_ron(r#""UnknownVariant""#);
    let result = validate(&value, &schema);
    if let Err(ValidationError::UnknownVariant(variant)) = result {
        assert_eq!(variant, "UnknownVariant");
    } else {
        panic!("Expected UnknownVariant error");
    }
}

#[test]
fn test_error_tuple_length_mismatch_message() {
    let kind = TypeKind::Tuple(vec![TypeKind::I32, TypeKind::I32]);
    let value = Value::Seq(vec![Value::Number(1.into())]);
    let result = validate_type(&value, &kind);
    if let Err(ValidationError::TupleLengthMismatch { expected, actual }) = result {
        assert_eq!(expected, 2);
        assert_eq!(actual, 1);
    } else {
        panic!("Expected TupleLengthMismatch error");
    }
}

#[test]
fn test_error_element_error_index() {
    let kind = TypeKind::Vec(Box::new(TypeKind::I32));
    let value = Value::Seq(vec![
        Value::Number(1.into()),
        Value::Number(2.into()),
        Value::String("bad".into()),
    ]);
    let result = validate_type(&value, &kind);
    if let Err(ValidationError::ElementError { index, .. }) = result {
        assert_eq!(index, 2);
    } else {
        panic!("Expected ElementError");
    }
}

#[test]
fn test_error_field_error_name() {
    let schema = Schema::new(TypeKind::Struct {
        fields: vec![Field::new("my_field", TypeKind::I32)],
    });
    let value: Value = parse_ron(r#"(my_field: "not int")"#);
    let result = validate(&value, &schema);
    if let Err(ValidationError::FieldError { field, .. }) = result {
        assert_eq!(field, "my_field");
    } else {
        panic!("Expected FieldError");
    }
}

#[test]
fn test_error_variant_error_name() {
    let schema = Schema::new(TypeKind::Enum {
        variants: vec![Variant::tuple("MyVariant", vec![TypeKind::I32])],
    });
    let value: Value = parse_ron(r#"{ "MyVariant": ["not int"] }"#);
    let result = validate(&value, &schema);
    if let Err(ValidationError::VariantError { variant, .. }) = result {
        assert_eq!(variant, "MyVariant");
    } else {
        panic!("Expected VariantError");
    }
}
