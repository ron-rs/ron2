use std::collections::HashSet;
use std::path::{Path, PathBuf};

use ron2::Value;

use crate::error::{Result, SchemaError};
use crate::{Field, Schema, TypeKind, VariantKind};

// =============================================================================
// Schema Resolver Trait
// =============================================================================

/// Trait for resolving TypeRef schemas during validation.
///
/// Implement this trait to provide schema resolution for `TypeRef` validation.
/// When `resolve` returns `None`, the TypeRef validation passes (accepts any value).
/// When it returns `Some(schema)`, the value is validated against that schema.
///
/// # Example
///
/// ```rust
/// use std::collections::HashMap;
/// use ron_schema::{Schema, SchemaResolver};
///
/// struct MyResolver {
///     schemas: HashMap<String, Schema>,
/// }
///
/// impl SchemaResolver for MyResolver {
///     fn resolve(&self, type_path: &str) -> Option<Schema> {
///         self.schemas.get(type_path).cloned()
///     }
/// }
/// ```
pub trait SchemaResolver {
    /// Resolve a type path to its schema.
    ///
    /// Returns `Some(schema)` if the schema is found, `None` otherwise.
    /// When `None` is returned, TypeRef validation accepts any value.
    fn resolve(&self, type_path: &str) -> Option<Schema>;
}

/// A resolver that accepts any value for all TypeRefs.
///
/// This is the default behavior when no resolver is provided,
/// maintaining backward compatibility.
pub struct AcceptAllResolver;

impl SchemaResolver for AcceptAllResolver {
    fn resolve(&self, _type_path: &str) -> Option<Schema> {
        None
    }
}

/// A resolver that loads schemas from the file system.
///
/// This resolver uses the storage functions to find and load schemas
/// based on type paths.
pub struct StorageResolver {
    /// Optional additional search directory (checked before default locations).
    search_dir: Option<PathBuf>,
}

impl StorageResolver {
    /// Create a new storage resolver using default schema locations.
    pub fn new() -> Self {
        Self { search_dir: None }
    }

    /// Create a storage resolver that searches a specific directory first.
    pub fn with_search_dir(dir: impl Into<PathBuf>) -> Self {
        Self {
            search_dir: Some(dir.into()),
        }
    }

    /// Set the search directory.
    pub fn search_dir(&self) -> Option<&Path> {
        self.search_dir.as_deref()
    }
}

impl Default for StorageResolver {
    fn default() -> Self {
        Self::new()
    }
}

impl SchemaResolver for StorageResolver {
    fn resolve(&self, type_path: &str) -> Option<Schema> {
        if let Some(ref dir) = self.search_dir {
            crate::storage::find_schema_in(type_path, dir).ok()
        } else {
            crate::storage::find_schema(type_path).ok()
        }
    }
}

// =============================================================================
// Validation Context
// =============================================================================

/// Internal validation context that tracks resolver and visited types.
struct ValidationContext<'a, R: SchemaResolver> {
    resolver: &'a R,
    /// Set of type paths currently being validated (for circular reference detection).
    visiting: HashSet<String>,
}

impl<'a, R: SchemaResolver> ValidationContext<'a, R> {
    fn new(resolver: &'a R) -> Self {
        Self {
            resolver,
            visiting: HashSet::new(),
        }
    }

    /// Check if we're currently validating this type (circular reference).
    fn is_visiting(&self, type_path: &str) -> bool {
        self.visiting.contains(type_path)
    }

    /// Mark a type as being validated.
    fn start_visiting(&mut self, type_path: &str) {
        self.visiting.insert(type_path.to_string());
    }

    /// Unmark a type after validation completes.
    fn stop_visiting(&mut self, type_path: &str) {
        self.visiting.remove(type_path);
    }
}

// =============================================================================
// Public Validation Functions
// =============================================================================

/// Validate a RON value against a schema.
///
/// TypeRef fields accept any value. Use [`validate_with_resolver`] for
/// full TypeRef validation.
pub fn validate(value: &Value, schema: &Schema) -> Result<()> {
    validate_with_resolver(value, schema, &AcceptAllResolver)
}

/// Validate a RON value against a type kind.
///
/// TypeRef fields accept any value. Use [`validate_type_with_resolver`] for
/// full TypeRef validation.
pub fn validate_type(value: &Value, kind: &TypeKind) -> Result<()> {
    validate_type_with_resolver(value, kind, &AcceptAllResolver)
}

/// Validate a RON value against a schema with TypeRef resolution.
///
/// This function resolves TypeRef references using the provided resolver,
/// enabling validation of nested/referenced schemas.
///
/// # Example
///
/// ```rust
/// use ron_schema::{Schema, TypeKind, Field, StorageResolver, validate_with_resolver};
///
/// let schema = Schema::new(TypeKind::Struct {
///     fields: vec![
///         Field::new("data", TypeKind::TypeRef("my::Type".to_string())),
///     ],
/// });
///
/// let resolver = StorageResolver::new();
/// let value = ron2::from_str("(data: 42)").unwrap();
///
/// // If "my::Type" schema is not found, accepts any value for 'data'
/// let result = validate_with_resolver(&value, &schema, &resolver);
/// ```
pub fn validate_with_resolver<R: SchemaResolver>(
    value: &Value,
    schema: &Schema,
    resolver: &R,
) -> Result<()> {
    let mut ctx = ValidationContext::new(resolver);
    validate_type_internal(value, &schema.kind, &mut ctx)
}

/// Validate a RON value against a type kind with TypeRef resolution.
pub fn validate_type_with_resolver<R: SchemaResolver>(
    value: &Value,
    kind: &TypeKind,
    resolver: &R,
) -> Result<()> {
    let mut ctx = ValidationContext::new(resolver);
    validate_type_internal(value, kind, &mut ctx)
}

// =============================================================================
// Internal Validation Implementation
// =============================================================================

fn validate_type_internal<R: SchemaResolver>(
    value: &Value,
    kind: &TypeKind,
    ctx: &mut ValidationContext<R>,
) -> Result<()> {
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
            Value::Option(Some(v)) => validate_type_internal(v, inner, ctx),
            _ => Err(type_mismatch("Option", value)),
        },
        TypeKind::List(inner) => match value {
            Value::Seq(items) => {
                for (i, item) in items.iter().enumerate() {
                    validate_type_internal(item, inner, ctx).map_err(|e| {
                        SchemaError::ElementError {
                            index: i,
                            source: Box::new(e),
                        }
                    })?;
                }
                Ok(())
            }
            _ => Err(type_mismatch("List", value)),
        },
        TypeKind::Map { key, value: val_ty } => match value {
            Value::Map(map) => {
                for (k, v) in map.iter() {
                    validate_type_internal(k, key, ctx).map_err(|e| {
                        SchemaError::MapKeyError {
                            source: Box::new(e),
                        }
                    })?;
                    validate_type_internal(v, val_ty, ctx).map_err(|e| {
                        SchemaError::MapValueError {
                            key: format!("{:?}", k),
                            source: Box::new(e),
                        }
                    })?;
                }
                Ok(())
            }
            _ => Err(type_mismatch("Map", value)),
        },
        TypeKind::Tuple(types) => match value {
            Value::Tuple(items) | Value::Seq(items) => {
                if items.len() != types.len() {
                    return Err(SchemaError::TupleLengthMismatch {
                        expected: types.len(),
                        actual: items.len(),
                    });
                }
                for (i, (item, ty)) in items.iter().zip(types.iter()).enumerate() {
                    validate_type_internal(item, ty, ctx).map_err(|e| {
                        SchemaError::ElementError {
                            index: i,
                            source: Box::new(e),
                        }
                    })?;
                }
                Ok(())
            }
            _ => Err(type_mismatch("Tuple", value)),
        },
        TypeKind::Struct { fields } => validate_struct_internal(value, fields, ctx),
        TypeKind::Enum { variants } => validate_enum_internal(value, variants, ctx),
        TypeKind::TypeRef(type_path) => {
            // Check for circular reference
            if ctx.is_visiting(type_path) {
                // Circular reference detected - the type structure is valid,
                // just recursive. Accept the value as valid to avoid infinite loop.
                return Ok(());
            }

            // Try to resolve the schema
            match ctx.resolver.resolve(type_path) {
                Some(schema) => {
                    // Mark as visiting to detect cycles
                    ctx.start_visiting(type_path);

                    // Validate against the resolved schema
                    let result = validate_type_internal(value, &schema.kind, ctx).map_err(|e| {
                        SchemaError::TypeRefError {
                            type_path: type_path.clone(),
                            source: Box::new(e),
                        }
                    });

                    // Unmark after validation
                    ctx.stop_visiting(type_path);

                    result
                }
                None => {
                    // No schema found - accept any value (backward compatible)
                    Ok(())
                }
            }
        }
    }
}

fn validate_struct_internal<R: SchemaResolver>(
    value: &Value,
    fields: &[Field],
    ctx: &mut ValidationContext<R>,
) -> Result<()> {
    // Helper to validate struct fields from an iterator of (name, value) pairs
    fn validate_struct_fields_inner<'a, R: SchemaResolver>(
        field_iter: impl Iterator<Item = (&'a str, &'a Value)>,
        fields: &[Field],
        has_field: impl Fn(&str) -> bool,
        ctx: &mut ValidationContext<R>,
    ) -> Result<()> {
        // Check all provided fields are valid
        for (key_str, val) in field_iter {
            let field = fields
                .iter()
                .find(|f| f.name == key_str)
                .ok_or_else(|| SchemaError::UnknownField(key_str.to_string()))?;

            validate_type_internal(val, &field.ty, ctx).map_err(|e| {
                SchemaError::FieldError {
                    field: key_str.to_string(),
                    source: Box::new(e),
                }
            })?;
        }

        // Check all required fields are present
        for field in fields {
            if !field.optional && !has_field(&field.name) {
                return Err(SchemaError::MissingField(field.name.clone()));
            }
        }

        Ok(())
    }

    match value {
        // Empty struct: () - parsed as Unit
        Value::Unit => validate_struct_fields_inner(std::iter::empty(), fields, |_| false, ctx),
        // Anonymous struct: (x: 1, y: 2)
        Value::Struct(struct_fields) => validate_struct_fields_inner(
            struct_fields.iter().map(|(k, v)| (k.as_str(), v)),
            fields,
            |name| struct_fields.iter().any(|(k, _)| k == name),
            ctx,
        ),
        // Named struct: Point(x: 1, y: 2)
        Value::Named {
            content: ron2::value::NamedContent::Struct(struct_fields),
            ..
        } => validate_struct_fields_inner(
            struct_fields.iter().map(|(k, v)| (k.as_str(), v)),
            fields,
            |name| struct_fields.iter().any(|(k, _)| k == name),
            ctx,
        ),
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

            validate_struct_fields_inner(
                string_fields.iter().copied(),
                fields,
                |name| {
                    map.iter()
                        .any(|(k, _)| matches!(k, Value::String(s) if s == name))
                },
                ctx,
            )
        }
        _ => Err(type_mismatch("Struct", value)),
    }
}

fn validate_enum_internal<R: SchemaResolver>(
    value: &Value,
    variants: &[crate::Variant],
    ctx: &mut ValidationContext<R>,
) -> Result<()> {
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
        .ok_or_else(|| SchemaError::UnknownVariant(variant_name.to_string()))?;

    // Helper to validate struct fields
    let validate_struct_fields = |fields: &[crate::Field],
                                  struct_fields: &[(String, Value)],
                                  ctx: &mut ValidationContext<R>|
     -> Result<()> {
        for (key, val) in struct_fields.iter() {
            let field = fields.iter().find(|f| f.name == *key).ok_or_else(|| {
                SchemaError::VariantError {
                    variant: variant_name.to_string(),
                    source: Box::new(SchemaError::UnknownField(key.clone())),
                }
            })?;

            validate_type_internal(val, &field.ty, ctx).map_err(|e| {
                SchemaError::VariantError {
                    variant: variant_name.to_string(),
                    source: Box::new(SchemaError::FieldError {
                        field: key.clone(),
                        source: Box::new(e),
                    }),
                }
            })?;
        }
        // Check required fields
        for field in fields {
            if !field.optional && !struct_fields.iter().any(|(k, _)| k == &field.name) {
                return Err(SchemaError::VariantError {
                    variant: variant_name.to_string(),
                    source: Box::new(SchemaError::MissingField(field.name.clone())),
                });
            }
        }
        Ok(())
    };

    // Helper to validate tuple elements
    let validate_tuple_elements = |types: &[TypeKind],
                                   items: &[Value],
                                   ctx: &mut ValidationContext<R>|
     -> Result<()> {
        if items.len() != types.len() {
            return Err(SchemaError::VariantError {
                variant: variant_name.to_string(),
                source: Box::new(SchemaError::TupleLengthMismatch {
                    expected: types.len(),
                    actual: items.len(),
                }),
            });
        }
        for (i, (item, ty)) in items.iter().zip(types.iter()).enumerate() {
            validate_type_internal(item, ty, ctx).map_err(|e| SchemaError::VariantError {
                variant: variant_name.to_string(),
                source: Box::new(SchemaError::ElementError {
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
            validate_tuple_elements(types, items, ctx)
        }
        // Tuple variants from Value (Map-style)
        (VariantKind::Tuple(types), VariantContent::Value(Value::Seq(items))) => {
            validate_tuple_elements(types, items, ctx)
        }
        (VariantKind::Tuple(types), VariantContent::Value(Value::Tuple(items))) => {
            validate_tuple_elements(types, items, ctx)
        }

        // Struct variants from NamedContent
        (
            VariantKind::Struct(fields),
            VariantContent::Named(NamedContent::Struct(struct_fields)),
        ) => validate_struct_fields(fields, struct_fields, ctx),
        // Struct variants from Value (Map-style with anonymous struct)
        (VariantKind::Struct(fields), VariantContent::Value(Value::Struct(struct_fields))) => {
            validate_struct_fields(fields, struct_fields, ctx)
        }

        // Error cases
        (VariantKind::Unit, _) => Err(SchemaError::VariantError {
            variant: variant_name.to_string(),
            source: Box::new(SchemaError::TypeMismatch {
                expected: "Unit".to_string(),
                actual: "non-unit content".to_string(),
            }),
        }),
        (_, VariantContent::None) => Err(SchemaError::VariantError {
            variant: variant_name.to_string(),
            source: Box::new(SchemaError::TypeMismatch {
                expected: "variant content".to_string(),
                actual: "none".to_string(),
            }),
        }),
        (_, _) => Err(SchemaError::VariantError {
            variant: variant_name.to_string(),
            source: Box::new(SchemaError::TypeMismatch {
                expected: format!("{:?}", variant.kind),
                actual: "mismatched content".to_string(),
            }),
        }),
    }
}

fn type_mismatch(expected: &str, value: &Value) -> SchemaError {
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
    SchemaError::TypeMismatch {
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
                Variant::struct_variant("Complex", vec![Field::new("value", TypeKind::String)]),
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
