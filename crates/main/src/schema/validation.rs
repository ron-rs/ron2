use std::{
    collections::HashSet,
    path::{Path, PathBuf},
};

use super::{
    Field, Schema, TypeKind, Variant, VariantKind,
    error::{Result, SchemaError, ValidationError, ValidationResult},
};
use crate::Value;

// =============================================================================
// Schema Resolver Trait
// =============================================================================

/// Trait for resolving `TypeRef` schemas during validation.
///
/// Implement this trait to provide schema resolution for `TypeRef` validation.
/// When `resolve` returns `None`, the `TypeRef` validation passes (accepts any value).
/// When it returns `Some(schema)`, the value is validated against that schema.
///
/// # Example
///
/// ```rust
/// use std::collections::HashMap;
/// use crate::schema::{Schema, SchemaResolver};
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
    /// When `None` is returned, `TypeRef` validation accepts any value.
    fn resolve(&self, type_path: &str) -> Option<Schema>;
}

/// A resolver that accepts any value for all `TypeRefs`.
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
    #[must_use]
    pub fn new() -> Self {
        Self { search_dir: None }
    }

    /// Create a storage resolver that searches a specific directory first.
    #[must_use]
    pub fn with_search_dir(dir: impl Into<PathBuf>) -> Self {
        Self {
            search_dir: Some(dir.into()),
        }
    }

    /// Set the search directory.
    #[must_use]
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
            super::storage::find_schema_in(type_path, dir).ok()
        } else {
            super::storage::find_schema(type_path).ok()
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
/// `TypeRef` fields accept any value. Use [`validate_with_resolver`] for
/// full `TypeRef` validation.
pub fn validate(value: &Value, schema: &Schema) -> Result<()> {
    validate_with_resolver(value, schema, &AcceptAllResolver)
}

/// Validate a RON value against a type kind.
///
/// `TypeRef` fields accept any value. Use [`validate_type_with_resolver`] for
/// full `TypeRef` validation.
pub fn validate_type(value: &Value, kind: &TypeKind) -> Result<()> {
    validate_type_with_resolver(value, kind, &AcceptAllResolver)
}

/// Validate a RON value against a schema with `TypeRef` resolution.
///
/// This function resolves `TypeRef` references using the provided resolver,
/// enabling validation of nested/referenced schemas.
///
/// # Example
///
/// ```rust
/// use crate::schema::{Schema, TypeKind, Field, StorageResolver, validate_with_resolver};
///
/// let schema = Schema::new(TypeKind::Struct {
///     fields: vec![
///         Field::new("data", TypeKind::TypeRef("my::Type".to_string())),
///     ],
/// });
///
/// let resolver = StorageResolver::new();
/// let value = crate::from_str("(data: 42)").unwrap();
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
        .map_err(|e| SchemaError::Validation(Box::new(e)))
}

/// Validate a RON value against a type kind with `TypeRef` resolution.
pub fn validate_type_with_resolver<R: SchemaResolver>(
    value: &Value,
    kind: &TypeKind,
    resolver: &R,
) -> Result<()> {
    let mut ctx = ValidationContext::new(resolver);
    validate_type_internal(value, kind, &mut ctx).map_err(|e| SchemaError::Validation(Box::new(e)))
}

// =============================================================================
// Internal Validation Implementation
// =============================================================================

// Internal functions return ValidationResult<()> which contains a large error type.
// This is acceptable for internal use since errors are boxed at the public API boundary.
#[allow(clippy::result_large_err)]
fn validate_type_internal<R: SchemaResolver>(
    value: &Value,
    kind: &TypeKind,
    ctx: &mut ValidationContext<R>,
) -> ValidationResult<()> {
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
                    validate_type_internal(item, inner, ctx).map_err(|e| e.in_element(i))?;
                }
                Ok(())
            }
            _ => Err(type_mismatch("List", value)),
        },
        TypeKind::Map { key, value: val_ty } => match value {
            Value::Map(map) => {
                for (k, v) in map.iter() {
                    validate_type_internal(k, key, ctx).map_err(ValidationError::in_map_key)?;
                    validate_type_internal(v, val_ty, ctx)
                        .map_err(|e| e.in_map_value(format!("{k:?}")))?;
                }
                Ok(())
            }
            _ => Err(type_mismatch("Map", value)),
        },
        TypeKind::Tuple(types) => match value {
            Value::Tuple(items) | Value::Seq(items) => {
                if items.len() != types.len() {
                    return Err(ValidationError::length_mismatch(types.len(), items.len()));
                }
                for (i, (item, ty)) in items.iter().zip(types.iter()).enumerate() {
                    validate_type_internal(item, ty, ctx).map_err(|e| e.in_element(i))?;
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
                    let result = validate_type_internal(value, &schema.kind, ctx)
                        .map_err(|e| e.in_type_ref(type_path));

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

#[derive(Debug)]
enum FlattenedTarget {
    Struct {
        fields: Vec<Field>,
        presence_based: bool,
    },
    MapValue(TypeKind),
}

fn resolve_flattened_kind<R: SchemaResolver>(
    kind: &TypeKind,
    ctx: &mut ValidationContext<R>,
) -> Option<(TypeKind, bool)> {
    let mut presence_based = false;
    let mut current = kind.clone();
    let mut seen_refs = HashSet::new();

    for _ in 0..8 {
        match current {
            TypeKind::Option(inner) => {
                presence_based = true;
                current = *inner;
            }
            TypeKind::TypeRef(path) => {
                if !seen_refs.insert(path.clone()) {
                    return None;
                }
                match ctx.resolver.resolve(&path) {
                    Some(schema) => {
                        current = schema.kind;
                    }
                    None => return None,
                }
            }
            _ => return Some((current, presence_based)),
        }
    }

    Some((current, presence_based))
}

fn collect_flattened_targets<R: SchemaResolver>(
    fields: &[Field],
    ctx: &mut ValidationContext<R>,
) -> Vec<FlattenedTarget> {
    let mut targets = Vec::new();

    for field in fields {
        if !field.flattened {
            continue;
        }

        let Some((kind, mut presence_based)) = resolve_flattened_kind(&field.ty, ctx) else {
            continue;
        };
        presence_based |= field.optional;

        match kind {
            TypeKind::Struct { fields } => {
                targets.push(FlattenedTarget::Struct {
                    fields,
                    presence_based,
                });
            }
            TypeKind::Map { key, value } => {
                if matches!(*key, TypeKind::String) {
                    targets.push(FlattenedTarget::MapValue(*value));
                }
            }
            _ => {}
        }
    }

    targets
}

#[allow(clippy::result_large_err)]
fn validate_struct_fields_inner<'a, R: SchemaResolver>(
    field_iter: impl Iterator<Item = (&'a str, &'a Value)>,
    fields: &[Field],
    flattened_targets: &[FlattenedTarget],
    map_value_type: Option<&TypeKind>,
    has_field: impl Fn(&str) -> bool,
    ctx: &mut ValidationContext<R>,
) -> ValidationResult<()> {
    let explicit_fields: Vec<_> = fields.iter().filter(|f| !f.flattened).collect();

    // Check all provided fields are valid
    for (key_str, val) in field_iter {
        let field = explicit_fields.iter().find(|f| f.name == key_str);
        if let Some(field) = field {
            validate_type_internal(val, &field.ty, ctx).map_err(|e| e.in_field(key_str))?;
        }

        let mut matched_flattened_struct = false;
        for target in flattened_targets {
            if let FlattenedTarget::Struct { fields, .. } = target
                && let Some(inner) = fields.iter().find(|f| f.name == key_str)
            {
                matched_flattened_struct = true;
                validate_type_internal(val, &inner.ty, ctx)
                    .map_err(|e| e.in_field(key_str))?;
            }
        }

        if field.is_none() && !matched_flattened_struct {
            if let Some(map_value_type) = map_value_type {
                validate_type_internal(val, map_value_type, ctx)
                    .map_err(|e| e.in_field(key_str))?;
            } else {
                return Err(ValidationError::unknown_field(key_str.to_owned(), &[] as &[&str]));
            }
        }
    }

    // Check all required fields are present
    for field in &explicit_fields {
        if !field.optional && !has_field(&field.name) {
            return Err(ValidationError::missing_field(field.name.clone()));
        }
    }

    for target in flattened_targets {
        let FlattenedTarget::Struct {
            fields,
            presence_based,
        } = target
        else {
            continue;
        };

        if *presence_based && !fields.iter().any(|field| has_field(&field.name)) {
            continue;
        }

        for field in fields {
            if !field.optional && !has_field(&field.name) {
                return Err(ValidationError::missing_field(field.name.clone()));
            }
        }
    }

    Ok(())
}

#[allow(clippy::result_large_err)]
fn validate_struct_internal<R: SchemaResolver>(
    value: &Value,
    fields: &[Field],
    ctx: &mut ValidationContext<R>,
) -> ValidationResult<()> {
    let flattened_targets = collect_flattened_targets(fields, ctx);
    let map_value_type = flattened_targets.iter().find_map(|target| {
        if let FlattenedTarget::MapValue(value_type) = target {
            Some(value_type)
        } else {
            None
        }
    });

    match value {
        // Empty struct: () - parsed as Unit
        Value::Unit => validate_struct_fields_inner(
            core::iter::empty(),
            fields,
            &flattened_targets,
            map_value_type,
            |_| false,
            ctx,
        ),
        // Anonymous struct: (x: 1, y: 2)
        Value::Struct(struct_fields) => validate_struct_fields_inner(
            struct_fields.iter().map(|(k, v)| (k.as_str(), v)),
            fields,
            &flattened_targets,
            map_value_type,
            |name| struct_fields.iter().any(|(k, _)| k == name),
            ctx,
        ),
        // Named struct: Point(x: 1, y: 2)
        Value::Named {
            content: crate::value::NamedContent::Struct(struct_fields),
            ..
        } => validate_struct_fields_inner(
            struct_fields.iter().map(|(k, v)| (k.as_str(), v)),
            fields,
            &flattened_targets,
            map_value_type,
            |name| struct_fields.iter().any(|(k, _)| k == name),
            ctx,
        ),
        // Map with string keys (legacy RON format)
        Value::Map(map) => {
            // Extract string keys only
            let string_fields: core::result::Result<Vec<_>, _> = map
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
                &flattened_targets,
                map_value_type,
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

#[allow(clippy::result_large_err)]
fn validate_enum_internal<R: SchemaResolver>(
    value: &Value,
    variants: &[Variant],
    ctx: &mut ValidationContext<R>,
) -> ValidationResult<()> {
    use crate::value::NamedContent;

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
        // Named variant (crate style): Name, Name(values), Name(x: y)
        Value::Named { name, content } => (name.as_str(), VariantContent::Named(content)),
        // Map with single string key: { "Variant": content }
        Value::Map(map) if map.len() == 1 => {
            let Some((k, v)) = map.iter().next() else {
                return Err(type_mismatch("Enum", value));
            };
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
        .ok_or_else(|| ValidationError::unknown_variant(variant_name.to_owned(), &[] as &[&str]))?;

    // Helper to validate struct fields
    let validate_struct_fields = |fields: &[Field],
                                  struct_fields: &[(String, Value)],
                                  ctx: &mut ValidationContext<R>|
     -> ValidationResult<()> {
        for (key, val) in struct_fields {
            let field = fields.iter().find(|f| f.name == *key).ok_or_else(|| {
                ValidationError::unknown_field(key.clone(), &[] as &[&str]).in_variant(variant_name)
            })?;

            validate_type_internal(val, &field.ty, ctx)
                .map_err(|e| e.in_field(key).in_variant(variant_name))?;
        }
        // Check required fields
        for field in fields {
            if !field.optional && !struct_fields.iter().any(|(k, _)| k == &field.name) {
                return Err(
                    ValidationError::missing_field(field.name.clone()).in_variant(variant_name)
                );
            }
        }
        Ok(())
    };

    // Helper to validate tuple elements
    let validate_tuple_elements = |types: &[TypeKind],
                                   items: &[Value],
                                   ctx: &mut ValidationContext<R>|
     -> ValidationResult<()> {
        if items.len() != types.len() {
            return Err(
                ValidationError::length_mismatch(types.len(), items.len()).in_variant(variant_name)
            );
        }
        for (i, (item, ty)) in items.iter().zip(types.iter()).enumerate() {
            validate_type_internal(item, ty, ctx)
                .map_err(|e| e.in_element(i).in_variant(variant_name))?;
        }
        Ok(())
    };

    match (&variant.kind, content) {
        // Unit variants
        (
            VariantKind::Unit,
            VariantContent::None
            | VariantContent::Named(NamedContent::Unit)
            | VariantContent::Value(Value::Unit),
        ) => Ok(()),

        // Tuple variants from NamedContent
        (
            VariantKind::Tuple(types),
            VariantContent::Named(NamedContent::Tuple(items))
            | VariantContent::Value(Value::Seq(items) | Value::Tuple(items)),
        ) => validate_tuple_elements(types, items, ctx),

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
        (VariantKind::Unit, _) => {
            Err(ValidationError::type_mismatch("Unit", "non-unit content").in_variant(variant_name))
        }
        (_, VariantContent::None) => {
            Err(ValidationError::type_mismatch("variant content", "none").in_variant(variant_name))
        }
        (_, _) => Err(ValidationError::type_mismatch(
            format!("{variant_kind:?}", variant_kind = variant.kind),
            "mismatched content",
        )
        .in_variant(variant_name)),
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
    ValidationError::type_mismatch(expected, actual)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::schema::{Field, Schema, TypeKind, Variant};

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
        let value: Value = crate::from_str("(port: 8080, host: \"localhost\")").unwrap();
        assert!(validate(&value, &schema).is_ok());

        // Valid struct with only required fields
        let value: Value = crate::from_str("(port: 8080)").unwrap();
        assert!(validate(&value, &schema).is_ok());

        // Missing required field
        let value: Value = crate::from_str("(host: \"localhost\")").unwrap();
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
        let value: Value = crate::from_str("\"None\"").unwrap();
        assert!(validate(&value, &schema).is_ok());

        // Unknown variant
        let value: Value = crate::from_str("\"Unknown\"").unwrap();
        assert!(validate(&value, &schema).is_err());
    }

    #[test]
    fn test_error_path_context() {
        let schema = Schema::new(TypeKind::Struct {
            fields: vec![Field::new(
                "items",
                TypeKind::List(Box::new(TypeKind::String)),
            )],
        });

        // Error in nested structure should have path context
        let value: Value = crate::from_str("(items: [\"ok\", 42])").unwrap();
        let err = validate(&value, &schema).unwrap_err();

        // Should contain path information
        let msg = err.to_string();
        assert!(msg.contains("field 'items'"), "Error: {msg}");
        assert!(msg.contains("element 1"), "Error: {msg}");
    }
}
