#[cfg(feature = "derive")]
use std::path::{Path, PathBuf};

use ahash::{HashSet, HashSetExt};

use super::{
    Field, Schema, TypeKind, Variant, VariantKind,
    error::{Result, SchemaError, ValidationError, ValidationResult},
};
use crate::Value;
use crate::ast::{Expr, StructBody};
use crate::error::Span;

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
///
/// This type is only available when the `derive` feature is enabled.
#[cfg(feature = "derive")]
pub struct StorageResolver {
    /// Optional additional search directory (checked before default locations).
    search_dir: Option<PathBuf>,
}

#[cfg(feature = "derive")]
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

#[cfg(feature = "derive")]
impl Default for StorageResolver {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(feature = "derive")]
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
/// let value = "(data: 42)".parse::<Value>().unwrap();
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
// AST-Based Validation Functions
// =============================================================================

/// Validate an AST expression against a schema.
///
/// This function validates directly against the AST, providing precise error spans
/// that point to the exact location of errors in the source.
///
/// `TypeRef` fields accept any value. Use [`validate_expr_with_resolver`] for
/// full `TypeRef` validation.
pub fn validate_expr(expr: &Expr<'_>, schema: &Schema) -> Result<()> {
    validate_expr_with_resolver(expr, schema, &AcceptAllResolver)
}

/// Validate an AST expression against a type kind.
///
/// `TypeRef` fields accept any value. Use [`validate_expr_type_with_resolver`] for
/// full `TypeRef` validation.
pub fn validate_expr_type(expr: &Expr<'_>, kind: &TypeKind) -> Result<()> {
    validate_expr_type_with_resolver(expr, kind, &AcceptAllResolver)
}

/// Validate an AST expression against a schema with `TypeRef` resolution.
///
/// This function resolves `TypeRef` references using the provided resolver,
/// enabling validation of nested/referenced schemas. Error spans point directly
/// to the problematic AST nodes.
///
/// # Example
///
/// ```rust,ignore
/// use ron2::ast::parse_document;
/// use ron2::schema::{Schema, TypeKind, Field, validate_expr_with_resolver, AcceptAllResolver};
///
/// let source = "(port: 8080, host: \"localhost\")";
/// let doc = parse_document(source).unwrap();
///
/// let schema = Schema::new(TypeKind::Struct {
///     fields: vec![
///         Field::new("port", TypeKind::U16),
///         Field::optional("host", TypeKind::String),
///     ],
/// });
///
/// if let Some(ref expr) = doc.value {
///     let result = validate_expr_with_resolver(expr, &schema, &AcceptAllResolver);
/// }
/// ```
pub fn validate_expr_with_resolver<R: SchemaResolver>(
    expr: &Expr<'_>,
    schema: &Schema,
    resolver: &R,
) -> Result<()> {
    let mut ctx = ValidationContext::new(resolver);
    validate_expr_internal(expr, &schema.kind, &mut ctx)
        .map_err(|e| SchemaError::Validation(Box::new(e)))
}

/// Validate an AST expression against a type kind with `TypeRef` resolution.
pub fn validate_expr_type_with_resolver<R: SchemaResolver>(
    expr: &Expr<'_>,
    kind: &TypeKind,
    resolver: &R,
) -> Result<()> {
    let mut ctx = ValidationContext::new(resolver);
    validate_expr_internal(expr, kind, &mut ctx).map_err(|e| SchemaError::Validation(Box::new(e)))
}

/// Validate an AST expression against a schema, collecting ALL validation errors.
///
/// Unlike [`validate_expr_with_resolver`] which returns on the first error, this function
/// continues validation and collects all errors. This is useful for LSP diagnostics
/// where you want to show all problems at once.
///
/// - Skips `Expr::Error` nodes (parser already reported those errors)
/// - Continues validating siblings after finding an error
/// - Returns empty vec if validation succeeds
pub fn validate_expr_collect_all<R: SchemaResolver>(
    expr: &Expr<'_>,
    schema: &Schema,
    resolver: &R,
) -> Vec<ValidationError> {
    let mut ctx = ValidationContext::new(resolver);
    let mut errors = Vec::new();
    validate_expr_collect_internal(expr, &schema.kind, &mut ctx, &mut errors);
    errors
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
                    return Err(ValidationError::length_mismatch(
                        types.len().to_string(),
                        items.len(),
                    ));
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
                validate_type_internal(val, &inner.ty, ctx).map_err(|e| e.in_field(key_str))?;
            }
        }

        if field.is_none() && !matched_flattened_struct {
            if let Some(map_value_type) = map_value_type {
                validate_type_internal(val, map_value_type, ctx)
                    .map_err(|e| e.in_field(key_str))?;
            } else {
                return Err(ValidationError::unknown_field(
                    key_str.to_owned(),
                    &[] as &[&str],
                ));
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
                ValidationError::length_mismatch(types.len().to_string(), items.len())
                    .in_variant(variant_name),
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

// =============================================================================
// Internal AST Validation Implementation
// =============================================================================

/// Get the type name of an expression for error messages.
fn expr_type_name(expr: &Expr<'_>) -> &'static str {
    match expr {
        Expr::Unit(_) => "Unit",
        Expr::Bool(_) => "Bool",
        Expr::Char(_) => "Char",
        Expr::Byte(_) => "Byte",
        Expr::Number(_) => "Number",
        Expr::String(_) => "String",
        Expr::Bytes(_) => "Bytes",
        Expr::Option(_) => "Option",
        Expr::Seq(_) => "Seq",
        Expr::Map(_) => "Map",
        Expr::Tuple(_) => "Tuple",
        Expr::AnonStruct(_) => "Struct",
        Expr::Struct(_) => "Named",
        Expr::Error(_) => "Error",
    }
}

/// Create a type mismatch error with the expression's span.
fn expr_type_mismatch(expected: &str, expr: &Expr<'_>) -> ValidationError {
    ValidationError::with_span(
        crate::error::ErrorKind::TypeMismatch {
            expected: expected.to_string(),
            found: expr_type_name(expr).to_string(),
        },
        *expr.span(),
    )
}

/// Create a missing field error with a span.
fn missing_field_error(field: &str, span: Span) -> ValidationError {
    ValidationError::with_span(
        crate::error::ErrorKind::MissingField {
            field: field.to_string().into(),
            outer: None,
        },
        span,
    )
}

/// Create an unknown field error with a span.
fn unknown_field_error(field: &str, span: Span) -> ValidationError {
    ValidationError::with_span(
        crate::error::ErrorKind::UnknownField {
            field: field.to_string().into(),
            expected: &[],
            outer: None,
        },
        span,
    )
}

/// Create an unknown variant error with a span.
fn unknown_variant_error(variant: &str, span: Span) -> ValidationError {
    ValidationError::with_span(
        crate::error::ErrorKind::UnknownVariant {
            variant: variant.to_string().into(),
            expected: &[],
            outer: None,
        },
        span,
    )
}

/// Create a length mismatch error with a span.
fn length_mismatch_error(expected: usize, found: usize, span: Span) -> ValidationError {
    ValidationError::with_span(
        crate::error::ErrorKind::LengthMismatch {
            expected: expected.to_string(),
            found,
            context: None,
        },
        span,
    )
}

#[allow(clippy::result_large_err)]
fn validate_expr_internal<R: SchemaResolver>(
    expr: &Expr<'_>,
    kind: &TypeKind,
    ctx: &mut ValidationContext<R>,
) -> ValidationResult<()> {
    match kind {
        TypeKind::Bool => match expr {
            Expr::Bool(_) => Ok(()),
            _ => Err(expr_type_mismatch("Bool", expr)),
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
        | TypeKind::U128 => match expr {
            Expr::Number(_) => Ok(()),
            _ => Err(expr_type_mismatch("integer", expr)),
        },
        TypeKind::F32 | TypeKind::F64 => match expr {
            Expr::Number(_) => Ok(()),
            _ => Err(expr_type_mismatch("float", expr)),
        },
        TypeKind::Char => match expr {
            Expr::Char(_) => Ok(()),
            _ => Err(expr_type_mismatch("Char", expr)),
        },
        TypeKind::String => match expr {
            Expr::String(_) => Ok(()),
            _ => Err(expr_type_mismatch("String", expr)),
        },
        TypeKind::Unit => match expr {
            Expr::Unit(_) => Ok(()),
            _ => Err(expr_type_mismatch("Unit", expr)),
        },
        TypeKind::Option(inner) => validate_option_expr(expr, inner, ctx),
        TypeKind::List(inner) => validate_list_expr(expr, inner, ctx),
        TypeKind::Map { key, value: val_ty } => validate_map_expr(expr, key, val_ty, ctx),
        TypeKind::Tuple(types) => validate_tuple_expr(expr, types, ctx),
        TypeKind::Struct { fields } => validate_struct_expr(expr, fields, ctx),
        TypeKind::Enum { variants } => validate_enum_expr(expr, variants, ctx),
        TypeKind::TypeRef(type_path) => validate_typeref_expr(expr, type_path, ctx),
    }
}

#[allow(clippy::result_large_err)]
fn validate_option_expr<R: SchemaResolver>(
    expr: &Expr<'_>,
    inner: &TypeKind,
    ctx: &mut ValidationContext<R>,
) -> ValidationResult<()> {
    match expr {
        Expr::Option(opt) => match &opt.value {
            None => Ok(()),
            Some(val) => validate_expr_internal(&val.expr, inner, ctx),
        },
        // Allow named None/Some
        Expr::Struct(s) if s.name.name == "None" && s.body.is_none() => Ok(()),
        Expr::Struct(s) if s.name.name == "Some" => {
            if let Some(StructBody::Tuple(body)) = &s.body
                && body.elements.len() == 1
            {
                return validate_expr_internal(&body.elements[0].expr, inner, ctx);
            }
            Err(expr_type_mismatch("Option", expr))
        }
        _ => Err(expr_type_mismatch("Option", expr)),
    }
}

#[allow(clippy::result_large_err)]
fn validate_list_expr<R: SchemaResolver>(
    expr: &Expr<'_>,
    inner: &TypeKind,
    ctx: &mut ValidationContext<R>,
) -> ValidationResult<()> {
    match expr {
        Expr::Seq(seq) => {
            for (i, item) in seq.items.iter().enumerate() {
                validate_expr_internal(&item.expr, inner, ctx).map_err(|e| e.in_element(i))?;
            }
            Ok(())
        }
        _ => Err(expr_type_mismatch("List", expr)),
    }
}

#[allow(clippy::result_large_err)]
fn validate_map_expr<R: SchemaResolver>(
    expr: &Expr<'_>,
    key_ty: &TypeKind,
    val_ty: &TypeKind,
    ctx: &mut ValidationContext<R>,
) -> ValidationResult<()> {
    match expr {
        Expr::Map(map) => {
            for entry in &map.entries {
                validate_expr_internal(&entry.key, key_ty, ctx)
                    .map_err(ValidationError::in_map_key)?;
                let key_str = format_expr_as_key(&entry.key);
                validate_expr_internal(&entry.value, val_ty, ctx)
                    .map_err(|e| e.in_map_value(key_str))?;
            }
            Ok(())
        }
        _ => Err(expr_type_mismatch("Map", expr)),
    }
}

/// Format an expression as a key string for error messages.
fn format_expr_as_key(expr: &Expr<'_>) -> String {
    match expr {
        Expr::String(s) => s.value.clone(),
        Expr::Number(n) => n.raw.to_string(),
        Expr::Bool(b) => b.value.to_string(),
        Expr::Char(c) => c.value.to_string(),
        _ => "<complex>".to_string(),
    }
}

#[allow(clippy::result_large_err)]
fn validate_tuple_expr<R: SchemaResolver>(
    expr: &Expr<'_>,
    types: &[TypeKind],
    ctx: &mut ValidationContext<R>,
) -> ValidationResult<()> {
    let items: &[_] = match expr {
        Expr::Tuple(t) => &t.elements,
        Expr::Seq(s) => {
            // Sequences can also be used as tuples
            if s.items.len() != types.len() {
                return Err(length_mismatch_error(types.len(), s.items.len(), *expr.span()));
            }
            for (i, (item, ty)) in s.items.iter().zip(types.iter()).enumerate() {
                validate_expr_internal(&item.expr, ty, ctx).map_err(|e| e.in_element(i))?;
            }
            return Ok(());
        }
        Expr::Struct(s) => {
            // Named tuple struct: Name(a, b, c)
            if let Some(StructBody::Tuple(body)) = &s.body {
                &body.elements
            } else {
                return Err(expr_type_mismatch("Tuple", expr));
            }
        }
        _ => return Err(expr_type_mismatch("Tuple", expr)),
    };

    if items.len() != types.len() {
        return Err(length_mismatch_error(types.len(), items.len(), *expr.span()));
    }

    for (i, (item, ty)) in items.iter().zip(types.iter()).enumerate() {
        validate_expr_internal(&item.expr, ty, ctx).map_err(|e| e.in_element(i))?;
    }

    Ok(())
}

/// Flattened target for AST-based validation.
#[derive(Debug)]
enum ExprFlattenedTarget {
    Struct {
        fields: Vec<Field>,
        presence_based: bool,
    },
    MapValue(TypeKind),
}

fn collect_expr_flattened_targets<R: SchemaResolver>(
    fields: &[Field],
    ctx: &mut ValidationContext<R>,
) -> Vec<ExprFlattenedTarget> {
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
                targets.push(ExprFlattenedTarget::Struct {
                    fields,
                    presence_based,
                });
            }
            TypeKind::Map { key, value } => {
                if matches!(*key, TypeKind::String) {
                    targets.push(ExprFlattenedTarget::MapValue(*value));
                }
            }
            _ => {}
        }
    }

    targets
}

#[allow(clippy::result_large_err)]
fn validate_struct_expr<R: SchemaResolver>(
    expr: &Expr<'_>,
    fields: &[Field],
    ctx: &mut ValidationContext<R>,
) -> ValidationResult<()> {
    let flattened_targets = collect_expr_flattened_targets(fields, ctx);
    let map_value_type = flattened_targets.iter().find_map(|target| {
        if let ExprFlattenedTarget::MapValue(value_type) = target {
            Some(value_type)
        } else {
            None
        }
    });

    match expr {
        // Unit: () - empty struct
        Expr::Unit(_) => validate_struct_fields_expr(
            &[],
            fields,
            &flattened_targets,
            map_value_type,
            *expr.span(),
            ctx,
        ),
        // Anonymous struct: (field: value, ...)
        Expr::AnonStruct(anon) => validate_struct_fields_expr(
            &anon.fields,
            fields,
            &flattened_targets,
            map_value_type,
            *expr.span(),
            ctx,
        ),
        // Named struct: Name(field: value, ...)
        Expr::Struct(s) => {
            match &s.body {
                Some(StructBody::Fields(f)) => validate_struct_fields_body_expr(
                    &f.fields,
                    fields,
                    &flattened_targets,
                    map_value_type,
                    *expr.span(),
                    ctx,
                ),
                Some(StructBody::Tuple(_)) => {
                    // Tuple struct isn't a fields struct
                    Err(expr_type_mismatch("Struct", expr))
                }
                None => {
                    // Unit struct: Name - treated as empty fields
                    validate_struct_fields_expr(
                        &[],
                        fields,
                        &flattened_targets,
                        map_value_type,
                        *expr.span(),
                        ctx,
                    )
                }
            }
        }
        _ => Err(expr_type_mismatch("Struct", expr)),
    }
}

#[allow(clippy::result_large_err)]
fn validate_struct_fields_expr<R: SchemaResolver>(
    ast_fields: &[crate::ast::StructField<'_>],
    schema_fields: &[Field],
    flattened_targets: &[ExprFlattenedTarget],
    map_value_type: Option<&TypeKind>,
    struct_span: Span,
    ctx: &mut ValidationContext<R>,
) -> ValidationResult<()> {
    let explicit_fields: Vec<_> = schema_fields.iter().filter(|f| !f.flattened).collect();

    // Check all provided fields are valid
    for ast_field in ast_fields {
        let key_str = ast_field.name.name.as_ref();
        let schema_field = explicit_fields.iter().find(|f| f.name == key_str);

        if let Some(field) = schema_field {
            validate_expr_internal(&ast_field.value, &field.ty, ctx)
                .map_err(|e| e.in_field(key_str))?;
        }

        let mut matched_flattened_struct = false;
        for target in flattened_targets {
            if let ExprFlattenedTarget::Struct { fields, .. } = target
                && let Some(inner) = fields.iter().find(|f| f.name == key_str)
            {
                matched_flattened_struct = true;
                validate_expr_internal(&ast_field.value, &inner.ty, ctx)
                    .map_err(|e| e.in_field(key_str))?;
            }
        }

        if schema_field.is_none() && !matched_flattened_struct {
            if let Some(map_value_type) = map_value_type {
                validate_expr_internal(&ast_field.value, map_value_type, ctx)
                    .map_err(|e| e.in_field(key_str))?;
            } else {
                return Err(unknown_field_error(key_str, ast_field.name.span));
            }
        }
    }

    // Check all required fields are present
    let has_field = |name: &str| ast_fields.iter().any(|f| f.name.name == name);

    for field in &explicit_fields {
        if !field.optional && !has_field(&field.name) {
            return Err(missing_field_error(&field.name, struct_span));
        }
    }

    for target in flattened_targets {
        let ExprFlattenedTarget::Struct {
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
                return Err(missing_field_error(&field.name, struct_span));
            }
        }
    }

    Ok(())
}

#[allow(clippy::result_large_err)]
fn validate_struct_fields_body_expr<R: SchemaResolver>(
    ast_fields: &[crate::ast::StructField<'_>],
    schema_fields: &[Field],
    flattened_targets: &[ExprFlattenedTarget],
    map_value_type: Option<&TypeKind>,
    struct_span: Span,
    ctx: &mut ValidationContext<R>,
) -> ValidationResult<()> {
    // This is identical to validate_struct_fields_expr but works with FieldsBody fields
    validate_struct_fields_expr(
        ast_fields,
        schema_fields,
        flattened_targets,
        map_value_type,
        struct_span,
        ctx,
    )
}

#[allow(clippy::result_large_err)]
fn validate_enum_expr<R: SchemaResolver>(
    expr: &Expr<'_>,
    variants: &[Variant],
    ctx: &mut ValidationContext<R>,
) -> ValidationResult<()> {
    // Extract variant name and content
    let (variant_name, variant_span, content): (&str, Span, EnumExprContent<'_>) = match expr {
        // String as unit variant
        Expr::String(s) => (s.value.as_str(), s.span, EnumExprContent::Unit),
        // Option None/Some
        Expr::Option(opt) => {
            if let Some(val) = &opt.value {
                ("Some", opt.span, EnumExprContent::Tuple(&[&val.expr]))
            } else {
                ("None", opt.span, EnumExprContent::Unit)
            }
        }
        // Named struct/variant
        Expr::Struct(s) => {
            let name = s.name.name.as_ref();
            let span = s.name.span;
            match &s.body {
                None => (name, span, EnumExprContent::Unit),
                Some(StructBody::Tuple(body)) => {
                    let exprs: Vec<_> = body.elements.iter().map(|e| &e.expr).collect();
                    (name, span, EnumExprContent::TupleOwned(exprs))
                }
                Some(StructBody::Fields(body)) => {
                    (name, span, EnumExprContent::Struct(&body.fields))
                }
            }
        }
        _ => return Err(expr_type_mismatch("Enum", expr)),
    };

    let variant = variants
        .iter()
        .find(|v| v.name == variant_name)
        .ok_or_else(|| unknown_variant_error(variant_name, variant_span))?;

    // Helper to validate tuple content
    let validate_tuple_content =
        |items: &[&Expr<'_>], types: &[TypeKind], ctx: &mut ValidationContext<R>| {
            for (i, (item, ty)) in items.iter().zip(types.iter()).enumerate() {
                validate_expr_internal(item, ty, ctx)
                    .map_err(|e| e.in_element(i).in_variant(variant_name))?;
            }
            Ok(())
        };

    match (&variant.kind, content) {
        // Unit variants
        (VariantKind::Unit, EnumExprContent::Unit) => Ok(()),
        // Tuple variants (slice)
        (VariantKind::Tuple(types), EnumExprContent::Tuple(items)) if items.len() == types.len() => {
            validate_tuple_content(items, types, ctx)
        }
        // Tuple variants (vec)
        (VariantKind::Tuple(types), EnumExprContent::TupleOwned(ref items))
            if items.len() == types.len() =>
        {
            validate_tuple_content(items, types, ctx)
        }
        // Struct variants
        (VariantKind::Struct(fields), EnumExprContent::Struct(ast_fields)) => {
            validate_variant_struct_fields(ast_fields, fields, variant_name, variant_span, ctx)
        }
        // Type mismatches
        (VariantKind::Unit, _) => Err(ValidationError::with_span(
            crate::error::ErrorKind::TypeMismatch {
                expected: "Unit".to_string(),
                found: "non-unit content".to_string(),
            },
            variant_span,
        )
        .in_variant(variant_name)),
        (_, EnumExprContent::Unit) => Err(ValidationError::with_span(
            crate::error::ErrorKind::TypeMismatch {
                expected: "variant content".to_string(),
                found: "none".to_string(),
            },
            variant_span,
        )
        .in_variant(variant_name)),
        (VariantKind::Tuple(types), _) => Err(ValidationError::with_span(
            crate::error::ErrorKind::TypeMismatch {
                expected: format!("Tuple({} elements)", types.len()),
                found: "mismatched content".to_string(),
            },
            variant_span,
        )
        .in_variant(variant_name)),
        (VariantKind::Struct(_), _) => Err(ValidationError::with_span(
            crate::error::ErrorKind::TypeMismatch {
                expected: "Struct".to_string(),
                found: "mismatched content".to_string(),
            },
            variant_span,
        )
        .in_variant(variant_name)),
    }
}

/// Content of an enum variant expression.
enum EnumExprContent<'a> {
    Unit,
    Tuple(&'a [&'a Expr<'a>]),
    TupleOwned(Vec<&'a Expr<'a>>),
    Struct(&'a [crate::ast::StructField<'a>]),
}

#[allow(clippy::result_large_err)]
fn validate_variant_struct_fields<R: SchemaResolver>(
    ast_fields: &[crate::ast::StructField<'_>],
    schema_fields: &[Field],
    variant_name: &str,
    variant_span: Span,
    ctx: &mut ValidationContext<R>,
) -> ValidationResult<()> {
    for ast_field in ast_fields {
        let key = ast_field.name.name.as_ref();
        let field = schema_fields
            .iter()
            .find(|f| f.name == key)
            .ok_or_else(|| unknown_field_error(key, ast_field.name.span).in_variant(variant_name))?;

        validate_expr_internal(&ast_field.value, &field.ty, ctx)
            .map_err(|e| e.in_field(key).in_variant(variant_name))?;
    }

    // Check required fields
    for field in schema_fields {
        if !field.optional && !ast_fields.iter().any(|f| f.name.name == field.name) {
            return Err(missing_field_error(&field.name, variant_span).in_variant(variant_name));
        }
    }

    Ok(())
}

#[allow(clippy::result_large_err)]
fn validate_typeref_expr<R: SchemaResolver>(
    expr: &Expr<'_>,
    type_path: &str,
    ctx: &mut ValidationContext<R>,
) -> ValidationResult<()> {
    // Check for circular reference
    if ctx.is_visiting(type_path) {
        return Ok(());
    }

    // Try to resolve the schema
    match ctx.resolver.resolve(type_path) {
        Some(schema) => {
            ctx.start_visiting(type_path);
            let result = validate_expr_internal(expr, &schema.kind, ctx)
                .map_err(|e| e.in_type_ref(type_path));
            ctx.stop_visiting(type_path);
            result
        }
        None => Ok(()),
    }
}

// =============================================================================
// Multi-Error Validation Implementation (collects ALL errors)
// =============================================================================

/// Internal function that validates an expression and collects all errors.
/// Skips `Expr::Error` nodes since the parser already reported those.
fn validate_expr_collect_internal<R: SchemaResolver>(
    expr: &Expr<'_>,
    kind: &TypeKind,
    ctx: &mut ValidationContext<R>,
    errors: &mut Vec<ValidationError>,
) {
    // Skip error nodes - parser already reported these
    if matches!(expr, Expr::Error(_)) {
        return;
    }

    match kind {
        TypeKind::Bool => {
            if !matches!(expr, Expr::Bool(_)) {
                errors.push(expr_type_mismatch("Bool", expr));
            }
        }
        TypeKind::I8
        | TypeKind::I16
        | TypeKind::I32
        | TypeKind::I64
        | TypeKind::I128
        | TypeKind::U8
        | TypeKind::U16
        | TypeKind::U32
        | TypeKind::U64
        | TypeKind::U128 => {
            if !matches!(expr, Expr::Number(_)) {
                errors.push(expr_type_mismatch("integer", expr));
            }
        }
        TypeKind::F32 | TypeKind::F64 => {
            if !matches!(expr, Expr::Number(_)) {
                errors.push(expr_type_mismatch("float", expr));
            }
        }
        TypeKind::Char => {
            if !matches!(expr, Expr::Char(_)) {
                errors.push(expr_type_mismatch("Char", expr));
            }
        }
        TypeKind::String => {
            if !matches!(expr, Expr::String(_)) {
                errors.push(expr_type_mismatch("String", expr));
            }
        }
        TypeKind::Unit => {
            if !matches!(expr, Expr::Unit(_)) {
                errors.push(expr_type_mismatch("Unit", expr));
            }
        }
        TypeKind::Option(inner) => {
            validate_option_expr_collect(expr, inner, ctx, errors);
        }
        TypeKind::List(inner) => {
            validate_list_expr_collect(expr, inner, ctx, errors);
        }
        TypeKind::Map { key, value: val_ty } => {
            validate_map_expr_collect(expr, key, val_ty, ctx, errors);
        }
        TypeKind::Tuple(types) => {
            validate_tuple_expr_collect(expr, types, ctx, errors);
        }
        TypeKind::Struct { fields } => {
            validate_struct_expr_collect(expr, fields, ctx, errors);
        }
        TypeKind::Enum { variants } => {
            validate_enum_expr_collect(expr, variants, ctx, errors);
        }
        TypeKind::TypeRef(type_path) => {
            validate_typeref_expr_collect(expr, type_path, ctx, errors);
        }
    }
}

fn validate_option_expr_collect<R: SchemaResolver>(
    expr: &Expr<'_>,
    inner: &TypeKind,
    ctx: &mut ValidationContext<R>,
    errors: &mut Vec<ValidationError>,
) {
    match expr {
        Expr::Error(_) => {} // Skip error nodes
        Expr::Option(opt) => {
            if let Some(val) = &opt.value {
                validate_expr_collect_internal(&val.expr, inner, ctx, errors);
            }
        }
        Expr::Struct(s) if s.name.name == "None" && s.body.is_none() => {}
        Expr::Struct(s) if s.name.name == "Some" => {
            if let Some(StructBody::Tuple(body)) = &s.body
                && body.elements.len() == 1
            {
                validate_expr_collect_internal(&body.elements[0].expr, inner, ctx, errors);
                return;
            }
            errors.push(expr_type_mismatch("Option", expr));
        }
        _ => {
            errors.push(expr_type_mismatch("Option", expr));
        }
    }
}

fn validate_list_expr_collect<R: SchemaResolver>(
    expr: &Expr<'_>,
    inner: &TypeKind,
    ctx: &mut ValidationContext<R>,
    errors: &mut Vec<ValidationError>,
) {
    match expr {
        Expr::Error(_) => {} // Skip error nodes
        Expr::Seq(seq) => {
            for (i, item) in seq.items.iter().enumerate() {
                let mut item_errors = Vec::new();
                validate_expr_collect_internal(&item.expr, inner, ctx, &mut item_errors);
                for e in item_errors {
                    errors.push(e.in_element(i));
                }
            }
        }
        _ => {
            errors.push(expr_type_mismatch("List", expr));
        }
    }
}

fn validate_map_expr_collect<R: SchemaResolver>(
    expr: &Expr<'_>,
    key_ty: &TypeKind,
    val_ty: &TypeKind,
    ctx: &mut ValidationContext<R>,
    errors: &mut Vec<ValidationError>,
) {
    match expr {
        Expr::Error(_) => {} // Skip error nodes
        Expr::Map(map) => {
            for entry in &map.entries {
                let mut key_errors = Vec::new();
                validate_expr_collect_internal(&entry.key, key_ty, ctx, &mut key_errors);
                for e in key_errors {
                    errors.push(ValidationError::in_map_key(e));
                }

                let key_str = format_expr_as_key(&entry.key);
                let mut val_errors = Vec::new();
                validate_expr_collect_internal(&entry.value, val_ty, ctx, &mut val_errors);
                for e in val_errors {
                    errors.push(e.in_map_value(key_str.clone()));
                }
            }
        }
        _ => {
            errors.push(expr_type_mismatch("Map", expr));
        }
    }
}

fn validate_tuple_expr_collect<R: SchemaResolver>(
    expr: &Expr<'_>,
    types: &[TypeKind],
    ctx: &mut ValidationContext<R>,
    errors: &mut Vec<ValidationError>,
) {
    let items: &[_] = match expr {
        Expr::Error(_) => return, // Skip error nodes
        Expr::Tuple(t) => &t.elements,
        Expr::Seq(s) => {
            if s.items.len() != types.len() {
                errors.push(length_mismatch_error(types.len(), s.items.len(), *expr.span()));
                return;
            }
            for (i, (item, ty)) in s.items.iter().zip(types.iter()).enumerate() {
                let mut item_errors = Vec::new();
                validate_expr_collect_internal(&item.expr, ty, ctx, &mut item_errors);
                for e in item_errors {
                    errors.push(e.in_element(i));
                }
            }
            return;
        }
        Expr::Struct(s) => {
            if let Some(StructBody::Tuple(body)) = &s.body {
                &body.elements
            } else {
                errors.push(expr_type_mismatch("Tuple", expr));
                return;
            }
        }
        _ => {
            errors.push(expr_type_mismatch("Tuple", expr));
            return;
        }
    };

    if items.len() != types.len() {
        errors.push(length_mismatch_error(types.len(), items.len(), *expr.span()));
        return;
    }

    for (i, (item, ty)) in items.iter().zip(types.iter()).enumerate() {
        let mut item_errors = Vec::new();
        validate_expr_collect_internal(&item.expr, ty, ctx, &mut item_errors);
        for e in item_errors {
            errors.push(e.in_element(i));
        }
    }
}

fn validate_struct_expr_collect<R: SchemaResolver>(
    expr: &Expr<'_>,
    fields: &[Field],
    ctx: &mut ValidationContext<R>,
    errors: &mut Vec<ValidationError>,
) {
    let flattened_targets = collect_expr_flattened_targets(fields, ctx);
    let map_value_type = flattened_targets.iter().find_map(|target| {
        if let ExprFlattenedTarget::MapValue(value_type) = target {
            Some(value_type)
        } else {
            None
        }
    });

    match expr {
        Expr::Error(_) => {} // Skip error nodes
        Expr::Unit(_) => {
            validate_struct_fields_expr_collect(
                &[],
                fields,
                &flattened_targets,
                map_value_type,
                *expr.span(),
                ctx,
                errors,
            );
        }
        Expr::AnonStruct(anon) => {
            validate_struct_fields_expr_collect(
                &anon.fields,
                fields,
                &flattened_targets,
                map_value_type,
                *expr.span(),
                ctx,
                errors,
            );
        }
        Expr::Struct(s) => match &s.body {
            Some(StructBody::Fields(f)) => {
                validate_struct_fields_expr_collect(
                    &f.fields,
                    fields,
                    &flattened_targets,
                    map_value_type,
                    *expr.span(),
                    ctx,
                    errors,
                );
            }
            Some(StructBody::Tuple(_)) => {
                errors.push(expr_type_mismatch("Struct", expr));
            }
            None => {
                validate_struct_fields_expr_collect(
                    &[],
                    fields,
                    &flattened_targets,
                    map_value_type,
                    *expr.span(),
                    ctx,
                    errors,
                );
            }
        },
        _ => {
            errors.push(expr_type_mismatch("Struct", expr));
        }
    }
}

fn validate_struct_fields_expr_collect<R: SchemaResolver>(
    ast_fields: &[crate::ast::StructField<'_>],
    schema_fields: &[Field],
    flattened_targets: &[ExprFlattenedTarget],
    map_value_type: Option<&TypeKind>,
    struct_span: Span,
    ctx: &mut ValidationContext<R>,
    errors: &mut Vec<ValidationError>,
) {
    let explicit_fields: Vec<_> = schema_fields.iter().filter(|f| !f.flattened).collect();

    // Check all provided fields are valid
    for ast_field in ast_fields {
        // Skip error nodes in field values
        if matches!(ast_field.value, Expr::Error(_)) {
            continue;
        }

        let key_str = ast_field.name.name.as_ref();
        let schema_field = explicit_fields.iter().find(|f| f.name == key_str);

        if let Some(field) = schema_field {
            let mut field_errors = Vec::new();
            validate_expr_collect_internal(&ast_field.value, &field.ty, ctx, &mut field_errors);
            for e in field_errors {
                errors.push(e.in_field(key_str));
            }
        }

        let mut matched_flattened_struct = false;
        for target in flattened_targets {
            if let ExprFlattenedTarget::Struct {
                fields: inner_fields,
                ..
            } = target
                && let Some(inner) = inner_fields.iter().find(|f| f.name == key_str)
            {
                matched_flattened_struct = true;
                let mut field_errors = Vec::new();
                validate_expr_collect_internal(&ast_field.value, &inner.ty, ctx, &mut field_errors);
                for e in field_errors {
                    errors.push(e.in_field(key_str));
                }
            }
        }

        if schema_field.is_none() && !matched_flattened_struct {
            if let Some(map_value_type) = map_value_type {
                let mut field_errors = Vec::new();
                validate_expr_collect_internal(&ast_field.value, map_value_type, ctx, &mut field_errors);
                for e in field_errors {
                    errors.push(e.in_field(key_str));
                }
            } else {
                errors.push(unknown_field_error(key_str, ast_field.name.span));
            }
        }
    }

    // Check all required fields are present
    let has_field = |name: &str| ast_fields.iter().any(|f| f.name.name == name);

    for field in &explicit_fields {
        if !field.optional && !has_field(&field.name) {
            errors.push(missing_field_error(&field.name, struct_span));
        }
    }

    for target in flattened_targets {
        let ExprFlattenedTarget::Struct {
            fields: inner_fields,
            presence_based,
        } = target
        else {
            continue;
        };

        if *presence_based && !inner_fields.iter().any(|field| has_field(&field.name)) {
            continue;
        }

        for field in inner_fields {
            if !field.optional && !has_field(&field.name) {
                errors.push(missing_field_error(&field.name, struct_span));
            }
        }
    }
}

/// Validate tuple variant content against expected types.
fn validate_enum_tuple_content<R: SchemaResolver>(
    items: &[&Expr<'_>],
    types: &[TypeKind],
    variant_name: &str,
    ctx: &mut ValidationContext<R>,
    errors: &mut Vec<ValidationError>,
) {
    for (i, (item, ty)) in items.iter().zip(types.iter()).enumerate() {
        let mut item_errors = Vec::new();
        validate_expr_collect_internal(item, ty, ctx, &mut item_errors);
        for e in item_errors {
            errors.push(e.in_element(i).in_variant(variant_name));
        }
    }
}

/// Create a variant kind mismatch error.
fn variant_kind_mismatch_error(
    expected: &str,
    found: &str,
    variant_name: &str,
    variant_span: Span,
) -> ValidationError {
    ValidationError::with_span(
        crate::error::ErrorKind::TypeMismatch {
            expected: expected.to_string(),
            found: found.to_string(),
        },
        variant_span,
    )
    .in_variant(variant_name)
}

fn validate_enum_expr_collect<R: SchemaResolver>(
    expr: &Expr<'_>,
    variants: &[Variant],
    ctx: &mut ValidationContext<R>,
    errors: &mut Vec<ValidationError>,
) {
    // Skip error nodes
    if matches!(expr, Expr::Error(_)) {
        return;
    }

    // Extract variant name and content
    let (variant_name, variant_span, content): (&str, Span, EnumExprContent<'_>) = match expr {
        Expr::String(s) => (s.value.as_str(), s.span, EnumExprContent::Unit),
        Expr::Option(opt) => {
            if let Some(val) = &opt.value {
                ("Some", opt.span, EnumExprContent::Tuple(&[&val.expr]))
            } else {
                ("None", opt.span, EnumExprContent::Unit)
            }
        }
        Expr::Struct(s) => {
            let name = s.name.name.as_ref();
            let span = s.name.span;
            match &s.body {
                None => (name, span, EnumExprContent::Unit),
                Some(StructBody::Tuple(body)) => {
                    let exprs: Vec<_> = body.elements.iter().map(|e| &e.expr).collect();
                    (name, span, EnumExprContent::TupleOwned(exprs))
                }
                Some(StructBody::Fields(body)) => {
                    (name, span, EnumExprContent::Struct(&body.fields))
                }
            }
        }
        _ => {
            errors.push(expr_type_mismatch("Enum", expr));
            return;
        }
    };

    let Some(variant) = variants.iter().find(|v| v.name == variant_name) else {
        errors.push(unknown_variant_error(variant_name, variant_span));
        return;
    };

    match (&variant.kind, content) {
        (VariantKind::Unit, EnumExprContent::Unit) => {}
        (VariantKind::Tuple(types), EnumExprContent::Tuple(items)) if items.len() == types.len() => {
            validate_enum_tuple_content(items, types, variant_name, ctx, errors);
        }
        (VariantKind::Tuple(types), EnumExprContent::TupleOwned(ref items))
            if items.len() == types.len() =>
        {
            validate_enum_tuple_content(items, types, variant_name, ctx, errors);
        }
        (VariantKind::Struct(fields), EnumExprContent::Struct(ast_fields)) => {
            validate_variant_struct_fields_collect(
                ast_fields,
                fields,
                variant_name,
                variant_span,
                ctx,
                errors,
            );
        }
        (VariantKind::Unit, _) => {
            errors.push(variant_kind_mismatch_error(
                "Unit",
                "non-unit content",
                variant_name,
                variant_span,
            ));
        }
        (_, EnumExprContent::Unit) => {
            errors.push(variant_kind_mismatch_error(
                "variant content",
                "none",
                variant_name,
                variant_span,
            ));
        }
        (VariantKind::Tuple(types), _) => {
            errors.push(variant_kind_mismatch_error(
                &format!("Tuple({} elements)", types.len()),
                "mismatched content",
                variant_name,
                variant_span,
            ));
        }
        (VariantKind::Struct(_), _) => {
            errors.push(variant_kind_mismatch_error(
                "Struct",
                "mismatched content",
                variant_name,
                variant_span,
            ));
        }
    }
}

fn validate_variant_struct_fields_collect<R: SchemaResolver>(
    ast_fields: &[crate::ast::StructField<'_>],
    schema_fields: &[Field],
    variant_name: &str,
    variant_span: Span,
    ctx: &mut ValidationContext<R>,
    errors: &mut Vec<ValidationError>,
) {
    for ast_field in ast_fields {
        // Skip error nodes in field values
        if matches!(ast_field.value, Expr::Error(_)) {
            continue;
        }

        let key = ast_field.name.name.as_ref();
        let Some(field) = schema_fields.iter().find(|f| f.name == key) else {
            errors.push(unknown_field_error(key, ast_field.name.span).in_variant(variant_name));
            continue;
        };

        let mut field_errors = Vec::new();
        validate_expr_collect_internal(&ast_field.value, &field.ty, ctx, &mut field_errors);
        for e in field_errors {
            errors.push(e.in_field(key).in_variant(variant_name));
        }
    }

    // Check required fields
    for field in schema_fields {
        if !field.optional && !ast_fields.iter().any(|f| f.name.name == field.name) {
            errors.push(missing_field_error(&field.name, variant_span).in_variant(variant_name));
        }
    }
}

fn validate_typeref_expr_collect<R: SchemaResolver>(
    expr: &Expr<'_>,
    type_path: &str,
    ctx: &mut ValidationContext<R>,
    errors: &mut Vec<ValidationError>,
) {
    // Skip error nodes
    if matches!(expr, Expr::Error(_)) {
        return;
    }

    // Check for circular reference
    if ctx.is_visiting(type_path) {
        return;
    }

    // Try to resolve the schema
    if let Some(schema) = ctx.resolver.resolve(type_path) {
        ctx.start_visiting(type_path);
        let mut inner_errors = Vec::new();
        validate_expr_collect_internal(expr, &schema.kind, ctx, &mut inner_errors);
        for e in inner_errors {
            errors.push(e.in_type_ref(type_path));
        }
        ctx.stop_visiting(type_path);
    }
    // If no schema found, accept any value (backward compatible)
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
        let value: Value = "(port: 8080, host: \"localhost\")"
            .parse::<Value>()
            .unwrap();
        assert!(validate(&value, &schema).is_ok());

        // Valid struct with only required fields
        let value: Value = "(port: 8080)".parse::<Value>().unwrap();
        assert!(validate(&value, &schema).is_ok());

        // Missing required field
        let value: Value = "(host: \"localhost\")".parse::<Value>().unwrap();
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
        let value: Value = "\"None\"".parse::<Value>().unwrap();
        assert!(validate(&value, &schema).is_ok());

        // Unknown variant
        let value: Value = "\"Unknown\"".parse::<Value>().unwrap();
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
        let value: Value = "(items: [\"ok\", 42])".parse::<Value>().unwrap();
        let err = validate(&value, &schema).unwrap_err();

        // Should contain path information
        let msg = err.to_string();
        assert!(msg.contains("field 'items'"), "Error: {msg}");
        assert!(msg.contains("element 1"), "Error: {msg}");
    }

    // =========================================================================
    // AST-Based Validation Tests
    // =========================================================================

    mod ast_validation {
        use super::*;
        use crate::ast::parse_document;
        use crate::error::PathSegment;

        #[test]
        fn test_ast_validation_basic() {
            let source = "(port: 8080, host: \"localhost\")";
            let doc = parse_document(source).unwrap();
            let schema = Schema::new(TypeKind::Struct {
                fields: vec![
                    Field::new("port", TypeKind::U16),
                    Field::optional("host", TypeKind::String),
                ],
            });

            let result = validate_expr(doc.value.as_ref().unwrap(), &schema);
            assert!(result.is_ok());
        }

        #[test]
        fn test_ast_validation_type_mismatch_has_span() {
            let source = "(value: \"wrong\")";
            let doc = parse_document(source).unwrap();
            let schema = Schema::new(TypeKind::Struct {
                fields: vec![Field::new("value", TypeKind::I32)],
            });

            let err = validate_expr(doc.value.as_ref().unwrap(), &schema).unwrap_err();

            // The error should have a real span (not synthetic)
            if let crate::schema::SchemaError::Validation(v) = &err {
                let span = v.span();
                // Span should point to the "wrong" string value
                assert!(!span.is_synthetic(), "Expected non-synthetic span");
                assert_eq!(&source[span.start_offset..span.end_offset], "\"wrong\"");
            } else {
                panic!("Expected validation error");
            }
        }

        #[test]
        fn test_ast_validation_nested_error_has_correct_span() {
            let source = r#"(items: [1, "wrong", 3])"#;
            let doc = parse_document(source).unwrap();
            let schema = Schema::new(TypeKind::Struct {
                fields: vec![Field::new("items", TypeKind::List(Box::new(TypeKind::I32)))],
            });

            let err = validate_expr(doc.value.as_ref().unwrap(), &schema).unwrap_err();

            if let crate::schema::SchemaError::Validation(v) = &err {
                let span = v.span();
                // Span should point to "wrong", not "items" or the whole list
                assert!(!span.is_synthetic(), "Expected non-synthetic span");
                assert_eq!(
                    &source[span.start_offset..span.end_offset],
                    "\"wrong\"",
                    "Span should point to the wrong value"
                );
            } else {
                panic!("Expected validation error");
            }
        }

        #[test]
        fn test_ast_validation_unknown_field_has_span() {
            let source = "(known: 1, unknown_field: 2)";
            let doc = parse_document(source).unwrap();
            let schema = Schema::new(TypeKind::Struct {
                fields: vec![Field::new("known", TypeKind::I32)],
            });

            let err = validate_expr(doc.value.as_ref().unwrap(), &schema).unwrap_err();

            if let crate::schema::SchemaError::Validation(v) = &err {
                let span = v.span();
                // Span should point to the unknown field name
                assert!(!span.is_synthetic(), "Expected non-synthetic span");
                assert_eq!(
                    &source[span.start_offset..span.end_offset],
                    "unknown_field"
                );
            } else {
                panic!("Expected validation error");
            }
        }

        #[test]
        fn test_ast_validation_missing_field_uses_struct_span() {
            let source = "(optional_field: 1)";
            let doc = parse_document(source).unwrap();
            let schema = Schema::new(TypeKind::Struct {
                fields: vec![
                    Field::new("required", TypeKind::I32),
                    Field::optional("optional_field", TypeKind::I32),
                ],
            });

            let err = validate_expr(doc.value.as_ref().unwrap(), &schema).unwrap_err();

            if let crate::schema::SchemaError::Validation(v) = &err {
                let span = v.span();
                // For missing field, span should be the whole struct
                assert!(!span.is_synthetic(), "Expected non-synthetic span");
                assert_eq!(&source[span.start_offset..span.end_offset], source);
            } else {
                panic!("Expected validation error");
            }
        }

        #[test]
        fn test_ast_validation_deeply_nested_error() {
            let source = r#"Config(
                data: (
                    items: [
                        (name: "first"),
                        (name: 123),
                    ],
                ),
            )"#;
            let doc = parse_document(source).unwrap();
            let item_schema = TypeKind::Struct {
                fields: vec![Field::new("name", TypeKind::String)],
            };
            let schema = Schema::new(TypeKind::Struct {
                fields: vec![Field::new(
                    "data",
                    TypeKind::Struct {
                        fields: vec![Field::new("items", TypeKind::List(Box::new(item_schema)))],
                    },
                )],
            });

            let err = validate_expr(doc.value.as_ref().unwrap(), &schema).unwrap_err();

            if let crate::schema::SchemaError::Validation(v) = &err {
                let span = v.span();
                // Span should point to 123, not the whole structure
                assert!(!span.is_synthetic(), "Expected non-synthetic span");
                assert_eq!(&source[span.start_offset..span.end_offset], "123");

                // Check path context
                assert!(v.path().iter().any(|p| matches!(p, PathSegment::Field(f) if f == "name")));
                assert!(v.path().iter().any(|p| matches!(p, PathSegment::Element(1))));
            } else {
                panic!("Expected validation error");
            }
        }

        #[test]
        fn test_ast_validation_option_some_with_span() {
            let source = "Some(\"wrong\")";
            let doc = parse_document(source).unwrap();
            let schema = Schema::new(TypeKind::Option(Box::new(TypeKind::I32)));

            let err = validate_expr(doc.value.as_ref().unwrap(), &schema).unwrap_err();

            if let crate::schema::SchemaError::Validation(v) = &err {
                let span = v.span();
                assert!(!span.is_synthetic(), "Expected non-synthetic span");
                assert_eq!(&source[span.start_offset..span.end_offset], "\"wrong\"");
            } else {
                panic!("Expected validation error");
            }
        }

        #[test]
        fn test_ast_validation_enum_unknown_variant_span() {
            let source = "BadVariant";
            let doc = parse_document(source).unwrap();
            let schema = Schema::new(TypeKind::Enum {
                variants: vec![
                    crate::schema::Variant::unit("GoodVariant"),
                    crate::schema::Variant::unit("OtherVariant"),
                ],
            });

            let err = validate_expr(doc.value.as_ref().unwrap(), &schema).unwrap_err();

            if let crate::schema::SchemaError::Validation(v) = &err {
                let span = v.span();
                assert!(!span.is_synthetic(), "Expected non-synthetic span");
                assert_eq!(&source[span.start_offset..span.end_offset], "BadVariant");
            } else {
                panic!("Expected validation error");
            }
        }
    }
}
