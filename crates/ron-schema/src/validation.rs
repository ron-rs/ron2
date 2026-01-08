use ron::Value;

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
            Value::Seq(items) => {
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
    let map = match value {
        Value::Map(m) => m,
        _ => return Err(type_mismatch("Struct", value)),
    };

    // Check all provided fields are valid
    for (key, val) in map.iter() {
        let key_str = match key {
            Value::String(s) => s.as_str(),
            _ => return Err(type_mismatch("String (field name)", key)),
        };

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
        if !field.optional {
            let has_field = map.iter().any(|(k, _)| {
                matches!(k, Value::String(s) if s == &field.name)
            });
            if !has_field {
                return Err(ValidationError::MissingField(field.name.clone()));
            }
        }
    }

    Ok(())
}

fn validate_enum(value: &Value, variants: &[crate::Variant]) -> Result<()> {
    // RON enums can be represented as:
    // - Unit: just the variant name as a string, or Name
    // - Tuple: Name(value1, value2)
    // - Struct: Name(field: value)

    let (variant_name, variant_value) = match value {
        // Unit variant as string
        Value::String(s) => (s.as_str(), None),
        // Named variant with optional value
        Value::Map(map) if map.len() == 1 => {
            let (k, v) = map.iter().next().unwrap();
            match k {
                Value::String(s) => (s.as_str(), Some(v)),
                _ => return Err(type_mismatch("Enum variant name", k)),
            }
        }
        _ => return Err(type_mismatch("Enum", value)),
    };

    let variant = variants
        .iter()
        .find(|v| v.name == variant_name)
        .ok_or_else(|| ValidationError::UnknownVariant(variant_name.to_string()))?;

    match (&variant.kind, variant_value) {
        (VariantKind::Unit, None) => Ok(()),
        (VariantKind::Unit, Some(Value::Unit)) => Ok(()),
        (VariantKind::Unit, Some(_)) => Err(ValidationError::VariantError {
            variant: variant_name.to_string(),
            source: Box::new(type_mismatch("Unit", variant_value.unwrap())),
        }),
        (VariantKind::Tuple(types), Some(Value::Seq(items))) => {
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
        }
        (VariantKind::Struct(fields), Some(v)) => {
            validate_struct(v, fields).map_err(|e| ValidationError::VariantError {
                variant: variant_name.to_string(),
                source: Box::new(e),
            })
        }
        (_, None) => Err(ValidationError::VariantError {
            variant: variant_name.to_string(),
            source: Box::new(type_mismatch("variant value", &Value::Unit)),
        }),
        (_, Some(v)) => Err(ValidationError::VariantError {
            variant: variant_name.to_string(),
            source: Box::new(type_mismatch("matching variant kind", v)),
        }),
    }
}

fn type_mismatch(expected: &str, value: &Value) -> ValidationError {
    let actual = match value {
        Value::Bool(_) => "Bool",
        Value::Char(_) => "Char",
        Value::Map(_) => "Map/Struct",
        Value::Number(_) => "Number",
        Value::Option(_) => "Option",
        Value::String(_) => "String",
        Value::Seq(_) => "Seq/Tuple",
        Value::Unit => "Unit",
        Value::Bytes(_) => "Bytes",
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
        let value: Value = ron::from_str("(port: 8080, host: \"localhost\")").unwrap();
        assert!(validate(&value, &schema).is_ok());

        // Valid struct with only required fields
        let value: Value = ron::from_str("(port: 8080)").unwrap();
        assert!(validate(&value, &schema).is_ok());

        // Missing required field
        let value: Value = ron::from_str("(host: \"localhost\")").unwrap();
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
        let value: Value = ron::from_str("\"None\"").unwrap();
        assert!(validate(&value, &schema).is_ok());

        // Unknown variant
        let value: Value = ron::from_str("\"Unknown\"").unwrap();
        assert!(validate(&value, &schema).is_err());
    }
}
