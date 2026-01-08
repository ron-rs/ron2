use ron2::Value;

use crate::{Field, Schema, TypeKind, VariantKind};

/// Errors that can occur during validation.
#[derive(Debug, thiserror::Error)]
pub enum ValidationError {
    #[error("Type mismatch: expected {expected}, got {actual}")]
    TypeMismatch { expected: String, actual: String },
    #[error("Missing required field: {0}")]
    MissingField(String),
    #[error("Unknown field: {0}")]
    UnknownField(String),
    #[error("Unknown enum variant: {0}")]
    UnknownVariant(String),
    #[error("Invalid tuple length: expected {expected}, got {actual}")]
    TupleLengthMismatch { expected: usize, actual: usize },
    #[error("Validation error in field '{field}': {source}")]
    FieldError {
        field: String,
        source: Box<ValidationError>,
    },
    #[error("Validation error in element {index}: {source}")]
    ElementError {
        index: usize,
        source: Box<ValidationError>,
    },
    #[error("Validation error in map key: {source}")]
    MapKeyError { source: Box<ValidationError> },
    #[error("Validation error in map value for key '{key}': {source}")]
    MapValueError {
        key: String,
        source: Box<ValidationError>,
    },
    #[error("Validation error in variant '{variant}': {source}")]
    VariantError {
        variant: String,
        source: Box<ValidationError>,
    },
}

/// Result type for validation operations.
pub type Result<T> = std::result::Result<T, ValidationError>;

/// Validate a RON value against a schema.
pub fn validate(value: &Value, schema: &Schema) -> Result<()> {
    validate_type(value, &schema.kind)
}

/// Validate a RON value against a type kind.
pub fn validate_type(value: &Value, kind: &TypeKind) -> Result<()> {
    match kind {
        TypeKind::Bool => match value {
            Value::Bool(_) => Ok(()),
            _ => Err(type_mismatch("Bool", value)),
        },
        TypeKind::I8
        | TypeKind::I16
        | TypeKind::I32
        | TypeKind::I64
        | TypeKind::I128
        | TypeKind::U8
        | TypeKind::U16
        | TypeKind::U32
        | TypeKind::U64
        | TypeKind::U128 => match value {
            Value::Number(_) => Ok(()),
            _ => Err(type_mismatch("integer", value)),
        },
        TypeKind::F32 | TypeKind::F64 => match value {
            Value::Number(_) => Ok(()),
            _ => Err(type_mismatch("float", value)),
        },
        TypeKind::Char => match value {
            Value::Char(_) => Ok(()),
            _ => Err(type_mismatch("Char", value)),
        },
        TypeKind::String => match value {
            Value::String(_) => Ok(()),
            _ => Err(type_mismatch("String", value)),
        },
        TypeKind::Unit => match value {
            Value::Unit => Ok(()),
            _ => Err(type_mismatch("Unit", value)),
        },
        TypeKind::Option(inner) => match value {
            Value::Option(None) => Ok(()),
            Value::Option(Some(v)) => validate_type(v, inner),
            _ => Err(type_mismatch("Option", value)),
        },
        TypeKind::List(inner) => match value {
            Value::Seq(items) => {
                for (i, item) in items.iter().enumerate() {
                    validate_type(item, inner).map_err(|e| ValidationError::ElementError {
                        index: i,
                        source: Box::new(e),
                    })?;
                }
                Ok(())
            }
            _ => Err(type_mismatch("List", value)),
        },
        TypeKind::Map { key, value: val_ty } => match value {
            Value::Map(map) => {
                for (k, v) in map.iter() {
                    validate_type(k, key).map_err(|e| ValidationError::MapKeyError {
                        source: Box::new(e),
                    })?;
                    validate_type(v, val_ty).map_err(|e| ValidationError::MapValueError {
                        key: format!("{:?}", k),
                        source: Box::new(e),
                    })?;
                }
                Ok(())
            }
            _ => Err(type_mismatch("Map", value)),
        },
        TypeKind::Tuple(types) => match value {
            Value::Tuple(items) | Value::Seq(items) => {
                if items.len() != types.len() {
                    return Err(ValidationError::TupleLengthMismatch {
                        expected: types.len(),
                        actual: items.len(),
                    });
                }
                for (i, (item, ty)) in items.iter().zip(types.iter()).enumerate() {
                    validate_type(item, ty).map_err(|e| ValidationError::ElementError {
                        index: i,
                        source: Box::new(e),
                    })?;
                }
                Ok(())
            }
            _ => Err(type_mismatch("Tuple", value)),
        },
        TypeKind::Struct { fields } => validate_struct(value, fields),
        TypeKind::Enum { variants } => validate_enum(value, variants),
        TypeKind::TypeRef(_) => {
            // TypeRef validation requires loading the referenced schema
            // For now, we accept any value for TypeRef
            Ok(())
        }
    }
}

fn validate_struct(value: &Value, fields: &[Field]) -> Result<()> {
    // Helper to validate struct fields from an iterator of (name, value) pairs
    fn validate_struct_fields<'a>(
        field_iter: impl Iterator<Item = (&'a str, &'a Value)>,
        fields: &[Field],
        has_field: impl Fn(&str) -> bool,
    ) -> Result<()> {
        // Check all provided fields are valid
        for (key_str, val) in field_iter {
            let field = fields
                .iter()
                .find(|f| f.name == key_str)
                .ok_or_else(|| ValidationError::UnknownField(key_str.to_string()))?;

            validate_type(val, &field.ty).map_err(|e| ValidationError::FieldError {
                field: key_str.to_string(),
                source: Box::new(e),
            })?;
        }

        // Check all required fields are present
        for field in fields {
            if !field.optional && !has_field(&field.name) {
                return Err(ValidationError::MissingField(field.name.clone()));
            }
        }

        Ok(())
    }

    match value {
        // Anonymous struct: (x: 1, y: 2)
        Value::Struct(struct_fields) => {
            validate_struct_fields(
                struct_fields.iter().map(|(k, v)| (k.as_str(), v)),
                fields,
                |name| struct_fields.iter().any(|(k, _)| k == name),
            )
        }
        // Named struct: Point(x: 1, y: 2)
        Value::Named {
            content: ron2::value::NamedContent::Struct(struct_fields),
            ..
        } => {
            validate_struct_fields(
                struct_fields.iter().map(|(k, v)| (k.as_str(), v)),
                fields,
                |name| struct_fields.iter().any(|(k, _)| k == name),
            )
        }
        // Map with string keys (legacy RON format)
        Value::Map(map) => {
            // Extract string keys only
            let string_fields: std::result::Result<Vec<_>, _> = map
                .iter()
                .map(|(k, v)| match k {
                    Value::String(s) => Ok((s.as_str(), v)),
                    _ => Err(type_mismatch("String (field name)", k)),
                })
                .collect();
            let string_fields = string_fields?;

            validate_struct_fields(
                string_fields.iter().copied(),
                fields,
                |name| map.iter().any(|(k, _)| matches!(k, Value::String(s) if s == name)),
            )
        }
        _ => Err(type_mismatch("Struct", value)),
    }
}

fn validate_enum(value: &Value, variants: &[crate::Variant]) -> Result<()> {
    use ron2::value::NamedContent;

    // Variant content can come from either NamedContent (for Named values)
    // or directly as a Value (for Map-based representation)
    enum VariantContent<'a> {
        None,
        Named(&'a NamedContent),
        Value(&'a Value),
    }

    // Extract variant name and content from the value
    let (variant_name, content): (&str, VariantContent) = match value {
        // Unit variant as string
        Value::String(s) => (s.as_str(), VariantContent::None),
        // Named variant (ron2 style): Name, Name(values), Name(x: y)
        Value::Named { name, content } => (name.as_str(), VariantContent::Named(content)),
        // Map with single string key: { "Variant": content }
        Value::Map(map) if map.len() == 1 => {
            let (k, v) = map.iter().next().unwrap();
            match k {
                Value::String(s) => (s.as_str(), VariantContent::Value(v)),
                _ => return Err(type_mismatch("Enum variant name", k)),
            }
        }
        _ => return Err(type_mismatch("Enum", value)),
    };

    let variant = variants
        .iter()
        .find(|v| v.name == variant_name)
        .ok_or_else(|| ValidationError::UnknownVariant(variant_name.to_string()))?;

    // Helper to validate struct fields
    let validate_struct_fields =
        |fields: &[crate::Field], struct_fields: &[(String, Value)]| -> Result<()> {
            for (key, val) in struct_fields.iter() {
                let field = fields.iter().find(|f| f.name == *key).ok_or_else(|| {
                    ValidationError::VariantError {
                        variant: variant_name.to_string(),
                        source: Box::new(ValidationError::UnknownField(key.clone())),
                    }
                })?;

                validate_type(val, &field.ty).map_err(|e| ValidationError::VariantError {
                    variant: variant_name.to_string(),
                    source: Box::new(ValidationError::FieldError {
                        field: key.clone(),
                        source: Box::new(e),
                    }),
                })?;
            }
            // Check required fields
            for field in fields {
                if !field.optional && !struct_fields.iter().any(|(k, _)| k == &field.name) {
                    return Err(ValidationError::VariantError {
                        variant: variant_name.to_string(),
                        source: Box::new(ValidationError::MissingField(field.name.clone())),
                    });
                }
            }
            Ok(())
        };

    // Helper to validate tuple elements
    let validate_tuple_elements =
        |types: &[TypeKind], items: &[Value]| -> Result<()> {
            if items.len() != types.len() {
                return Err(ValidationError::VariantError {
                    variant: variant_name.to_string(),
                    source: Box::new(ValidationError::TupleLengthMismatch {
                        expected: types.len(),
                        actual: items.len(),
                    }),
                });
            }
            for (i, (item, ty)) in items.iter().zip(types.iter()).enumerate() {
                validate_type(item, ty).map_err(|e| ValidationError::VariantError {
                    variant: variant_name.to_string(),
                    source: Box::new(ValidationError::ElementError {
                        index: i,
                        source: Box::new(e),
                    }),
                })?;
            }
            Ok(())
        };

    match (&variant.kind, content) {
        // Unit variants
        (VariantKind::Unit, VariantContent::None) => Ok(()),
        (VariantKind::Unit, VariantContent::Named(NamedContent::Unit)) => Ok(()),
        (VariantKind::Unit, VariantContent::Value(Value::Unit)) => Ok(()),

        // Tuple variants from NamedContent
        (VariantKind::Tuple(types), VariantContent::Named(NamedContent::Tuple(items))) => {
            validate_tuple_elements(types, items)
        }
        // Tuple variants from Value (Map-style)
        (VariantKind::Tuple(types), VariantContent::Value(Value::Seq(items))) => {
            validate_tuple_elements(types, items)
        }
        (VariantKind::Tuple(types), VariantContent::Value(Value::Tuple(items))) => {
            validate_tuple_elements(types, items)
        }

        // Struct variants from NamedContent
        (VariantKind::Struct(fields), VariantContent::Named(NamedContent::Struct(struct_fields))) => {
            validate_struct_fields(fields, struct_fields)
        }
        // Struct variants from Value (Map-style with anonymous struct)
        (VariantKind::Struct(fields), VariantContent::Value(Value::Struct(struct_fields))) => {
            validate_struct_fields(fields, struct_fields)
        }

        // Error cases
        (VariantKind::Unit, _) => Err(ValidationError::VariantError {
            variant: variant_name.to_string(),
            source: Box::new(ValidationError::TypeMismatch {
                expected: "Unit".to_string(),
                actual: "non-unit content".to_string(),
            }),
        }),
        (_, VariantContent::None) => Err(ValidationError::VariantError {
            variant: variant_name.to_string(),
            source: Box::new(ValidationError::TypeMismatch {
                expected: "variant content".to_string(),
                actual: "none".to_string(),
            }),
        }),
        (_, _) => Err(ValidationError::VariantError {
            variant: variant_name.to_string(),
            source: Box::new(ValidationError::TypeMismatch {
                expected: format!("{:?}", variant.kind),
                actual: "mismatched content".to_string(),
            }),
        }),
    }
}

fn type_mismatch(expected: &str, value: &Value) -> ValidationError {
    let actual = match value {
        Value::Bool(_) => "Bool",
        Value::Char(_) => "Char",
        Value::Map(_) => "Map",
        Value::Number(_) => "Number",
        Value::Option(_) => "Option",
        Value::String(_) => "String",
        Value::Seq(_) => "Seq",
        Value::Unit => "Unit",
        Value::Bytes(_) => "Bytes",
        Value::Tuple(_) => "Tuple",
        Value::Struct(_) => "Struct",
        Value::Named { .. } => "Named",
    };
    ValidationError::TypeMismatch {
        expected: expected.to_string(),
        actual: actual.to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Field, Schema, TypeKind, Variant};

    #[test]
    fn test_validate_primitives() {
        assert!(validate_type(&Value::Bool(true), &TypeKind::Bool).is_ok());
        assert!(validate_type(&Value::Number(42.into()), &TypeKind::I32).is_ok());
        assert!(validate_type(&Value::String("hello".into()), &TypeKind::String).is_ok());
        assert!(validate_type(&Value::Char('a'), &TypeKind::Char).is_ok());
        assert!(validate_type(&Value::Unit, &TypeKind::Unit).is_ok());
    }

    #[test]
    fn test_validate_type_mismatch() {
        assert!(validate_type(&Value::Bool(true), &TypeKind::String).is_err());
        assert!(validate_type(&Value::String("hello".into()), &TypeKind::Bool).is_err());
    }

    #[test]
    fn test_validate_list() {
        let list_type = TypeKind::List(Box::new(TypeKind::I32));
        let value = Value::Seq(vec![Value::Number(1.into()), Value::Number(2.into())]);
        assert!(validate_type(&value, &list_type).is_ok());

        let bad_value = Value::Seq(vec![Value::Number(1.into()), Value::String("bad".into())]);
        assert!(validate_type(&bad_value, &list_type).is_err());
    }

    #[test]
    fn test_validate_struct() {
        let schema = Schema::new(TypeKind::Struct {
            fields: vec![
                Field::new("port", TypeKind::U16),
                Field::optional("host", TypeKind::String),
            ],
        });

        // Valid struct with all fields
        let value: Value = ron2::from_str("(port: 8080, host: \"localhost\")").unwrap();
        assert!(validate(&value, &schema).is_ok());

        // Valid struct with only required fields
        let value: Value = ron2::from_str("(port: 8080)").unwrap();
        assert!(validate(&value, &schema).is_ok());

        // Missing required field
        let value: Value = ron2::from_str("(host: \"localhost\")").unwrap();
        assert!(validate(&value, &schema).is_err());
    }

    #[test]
    fn test_validate_enum() {
        let schema = Schema::new(TypeKind::Enum {
            variants: vec![
                Variant::unit("None"),
                Variant::tuple("Some", vec![TypeKind::I32]),
                Variant::struct_variant(
                    "Complex",
                    vec![Field::new("value", TypeKind::String)],
                ),
            ],
        });

        // Unit variant
        let value: Value = ron2::from_str("\"None\"").unwrap();
        assert!(validate(&value, &schema).is_ok());

        // Unknown variant
        let value: Value = ron2::from_str("\"Unknown\"").unwrap();
        assert!(validate(&value, &schema).is_err());
    }
}
