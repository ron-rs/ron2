use std::fmt;

use crate::ast::{Expr, expr_to_value, value_to_expr};
use crate::error::{Error, Result, SpannedError, SpannedResult};
use crate::value::{NamedContent, StructFields};
use crate::{FromRon, ToRon, Value};

use ron2_derive::{FromRon, ToRon};

/// Root schema definition for a Rust type.
#[derive(Debug, Clone, PartialEq, FromRon, ToRon)]
pub struct Schema {
    /// Documentation from the Rust type's doc comments.
    #[ron(default, skip_serializing_if = "Option::is_none")]
    pub doc: Option<String>,
    /// The kind of type this schema represents.
    pub kind: TypeKind,
}

/// Represents all possible Rust types that can be serialized to RON.
///
/// # Trait-Based Design
///
/// This enum works alongside the trait system in `crate::traits`:
/// - `List` represents any type implementing `RonList` (Vec, VecDeque, custom types)
/// - `Map` represents any type implementing `RonMap` (HashMap, BTreeMap, custom types)
///
/// Custom types implementing these traits will serialize to the same `TypeKind`
/// variants, allowing the validation and LSP systems to work uniformly.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TypeKind {
    // Primitives
    Bool,
    I8,
    I16,
    I32,
    I64,
    I128,
    U8,
    U16,
    U32,
    U64,
    U128,
    F32,
    F64,
    Char,
    String,

    // Unit type
    Unit,

    // Compound types
    Option(Box<TypeKind>),

    /// List/sequence type - represents any type implementing `RonList`.
    ///
    /// This includes `Vec<T>`, `VecDeque<T>`, `HashSet<T>`, `BTreeSet<T>`,
    /// arrays, and any custom types implementing `RonList`.
    List(Box<TypeKind>),

    /// Map/dictionary type - represents any type implementing `RonMap`.
    ///
    /// This includes `HashMap<K, V>`, `BTreeMap<K, V>`, and any custom types
    /// implementing `RonMap`.
    Map {
        key: Box<TypeKind>,
        value: Box<TypeKind>,
    },
    Tuple(Vec<TypeKind>),

    // Named types
    Struct {
        fields: Vec<Field>,
    },
    Enum {
        variants: Vec<Variant>,
    },

    /// Reference to another schema by fully-qualified type path.
    TypeRef(String),
}

impl TypeKind {
    /// Get inner type for Option or List.
    pub fn inner_type(&self) -> Option<&TypeKind> {
        match self {
            TypeKind::Option(inner) | TypeKind::List(inner) => Some(inner),
            _ => None,
        }
    }

    /// Get key/value types for Map.
    pub fn map_types(&self) -> Option<(&TypeKind, &TypeKind)> {
        match self {
            TypeKind::Map { key, value } => Some((key, value)),
            _ => None,
        }
    }

    /// Get tuple element types.
    pub fn tuple_types(&self) -> Option<&[TypeKind]> {
        match self {
            TypeKind::Tuple(types) => Some(types),
            _ => None,
        }
    }

    /// Get struct fields.
    pub fn struct_fields(&self) -> Option<&[Field]> {
        match self {
            TypeKind::Struct { fields } => Some(fields),
            _ => None,
        }
    }

    /// Get enum variants.
    pub fn enum_variants(&self) -> Option<&[Variant]> {
        match self {
            TypeKind::Enum { variants } => Some(variants),
            _ => None,
        }
    }

    /// Get TypeRef path.
    pub fn type_ref_path(&self) -> Option<&str> {
        match self {
            TypeKind::TypeRef(path) => Some(path),
            _ => None,
        }
    }

    /// Check if this is a primitive type (no nested types).
    pub fn is_primitive(&self) -> bool {
        matches!(
            self,
            TypeKind::Bool
                | TypeKind::I8
                | TypeKind::I16
                | TypeKind::I32
                | TypeKind::I64
                | TypeKind::I128
                | TypeKind::U8
                | TypeKind::U16
                | TypeKind::U32
                | TypeKind::U64
                | TypeKind::U128
                | TypeKind::F32
                | TypeKind::F64
                | TypeKind::Char
                | TypeKind::String
                | TypeKind::Unit
        )
    }
}

impl fmt::Display for TypeKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeKind::Bool => write!(f, "bool"),
            TypeKind::I8 => write!(f, "i8"),
            TypeKind::I16 => write!(f, "i16"),
            TypeKind::I32 => write!(f, "i32"),
            TypeKind::I64 => write!(f, "i64"),
            TypeKind::I128 => write!(f, "i128"),
            TypeKind::U8 => write!(f, "u8"),
            TypeKind::U16 => write!(f, "u16"),
            TypeKind::U32 => write!(f, "u32"),
            TypeKind::U64 => write!(f, "u64"),
            TypeKind::U128 => write!(f, "u128"),
            TypeKind::F32 => write!(f, "f32"),
            TypeKind::F64 => write!(f, "f64"),
            TypeKind::Char => write!(f, "char"),
            TypeKind::String => write!(f, "String"),
            TypeKind::Unit => write!(f, "()"),
            TypeKind::Option(inner) => write!(f, "Option<{}>", inner),
            TypeKind::List(inner) => write!(f, "List<{}>", inner),
            TypeKind::Map { key, value } => write!(f, "Map<{}, {}>", key, value),
            TypeKind::Tuple(types) => {
                write!(f, "(")?;
                for (i, ty) in types.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", ty)?;
                }
                write!(f, ")")
            }
            TypeKind::Struct { .. } => write!(f, "Struct"),
            TypeKind::Enum { .. } => write!(f, "Enum"),
            TypeKind::TypeRef(path) => write!(f, "{}", path),
        }
    }
}

/// A field in a struct.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Field {
    /// Field name.
    pub name: String,
    /// Field type.
    pub ty: TypeKind,
    /// Documentation from doc comments.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub doc: Option<String>,
    /// Whether this field is optional (has a default value).
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub optional: bool,
    /// Whether this field is flattened (its fields are merged into the parent).
    /// Only valid when `ty` is a Struct or TypeRef to a struct.
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub flattened: bool,
}

/// A variant in an enum.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Variant {
    /// Variant name.
    pub name: String,
    /// Documentation from doc comments.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub doc: Option<String>,
    /// The kind of variant.
    pub kind: VariantKind,
}

/// The kind of enum variant.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum VariantKind {
    /// Unit variant: `Variant`
    Unit,
    /// Tuple variant: `Variant(T1, T2, ...)`
    Tuple(Vec<TypeKind>),
    /// Struct variant: `Variant { field: T, ... }`
    Struct(Vec<Field>),
}

impl Schema {
    /// Create a new schema with the given kind.
    pub fn new(kind: TypeKind) -> Self {
        Self { doc: None, kind }
    }

    /// Create a new schema with documentation.
    pub fn with_doc(doc: impl Into<String>, kind: TypeKind) -> Self {
        Self {
            doc: Some(doc.into()),
            kind,
        }
    }
}

impl Field {
    /// Create a new required field.
    pub fn new(name: impl Into<String>, ty: TypeKind) -> Self {
        Self {
            name: name.into(),
            ty,
            doc: None,
            optional: false,
            flattened: false,
        }
    }

    /// Create a new optional field.
    pub fn optional(name: impl Into<String>, ty: TypeKind) -> Self {
        Self {
            name: name.into(),
            ty,
            doc: None,
            optional: true,
            flattened: false,
        }
    }

    /// Create a new flattened field.
    /// Flattened fields have their inner struct fields merged into the parent.
    pub fn flattened(name: impl Into<String>, ty: TypeKind) -> Self {
        Self {
            name: name.into(),
            ty,
            doc: None,
            optional: false,
            flattened: true,
        }
    }

    /// Add documentation to this field.
    pub fn with_doc(mut self, doc: impl Into<String>) -> Self {
        self.doc = Some(doc.into());
        self
    }

    /// Mark this field as flattened.
    pub fn with_flatten(mut self) -> Self {
        self.flattened = true;
        self
    }
}

impl Variant {
    /// Create a unit variant.
    pub fn unit(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            doc: None,
            kind: VariantKind::Unit,
        }
    }

    /// Create a tuple variant.
    pub fn tuple(name: impl Into<String>, fields: Vec<TypeKind>) -> Self {
        Self {
            name: name.into(),
            doc: None,
            kind: VariantKind::Tuple(fields),
        }
    }

    /// Create a struct variant.
    pub fn struct_variant(name: impl Into<String>, fields: Vec<Field>) -> Self {
        Self {
            name: name.into(),
            doc: None,
            kind: VariantKind::Struct(fields),
        }
    }

    /// Add documentation to this variant.
    pub fn with_doc(mut self, doc: impl Into<String>) -> Self {
        self.doc = Some(doc.into());
        self
    }
}

// =============================================================================
// ToRon implementations
// =============================================================================

impl ToRon for Schema {
    fn to_ast(&self) -> Result<Expr<'static>> {
        let mut fields: StructFields = Vec::new();
        if let Some(ref doc) = self.doc {
            fields.push(("doc".to_string(), Value::String(doc.clone())));
        }
        fields.push(("kind".to_string(), self.kind.to_ron_value()?));
        let value = Value::Named {
            name: "Schema".to_string(),
            content: NamedContent::Struct(fields),
        };
        Ok(value_to_expr(value))
    }
}

impl ToRon for TypeKind {
    fn to_ast(&self) -> Result<Expr<'static>> {
        let value = match self {
            TypeKind::Bool => Value::Named {
                name: "Bool".to_string(),
                content: NamedContent::Unit,
            },
            TypeKind::I8 => Value::Named {
                name: "I8".to_string(),
                content: NamedContent::Unit,
            },
            TypeKind::I16 => Value::Named {
                name: "I16".to_string(),
                content: NamedContent::Unit,
            },
            TypeKind::I32 => Value::Named {
                name: "I32".to_string(),
                content: NamedContent::Unit,
            },
            TypeKind::I64 => Value::Named {
                name: "I64".to_string(),
                content: NamedContent::Unit,
            },
            TypeKind::I128 => Value::Named {
                name: "I128".to_string(),
                content: NamedContent::Unit,
            },
            TypeKind::U8 => Value::Named {
                name: "U8".to_string(),
                content: NamedContent::Unit,
            },
            TypeKind::U16 => Value::Named {
                name: "U16".to_string(),
                content: NamedContent::Unit,
            },
            TypeKind::U32 => Value::Named {
                name: "U32".to_string(),
                content: NamedContent::Unit,
            },
            TypeKind::U64 => Value::Named {
                name: "U64".to_string(),
                content: NamedContent::Unit,
            },
            TypeKind::U128 => Value::Named {
                name: "U128".to_string(),
                content: NamedContent::Unit,
            },
            TypeKind::F32 => Value::Named {
                name: "F32".to_string(),
                content: NamedContent::Unit,
            },
            TypeKind::F64 => Value::Named {
                name: "F64".to_string(),
                content: NamedContent::Unit,
            },
            TypeKind::Char => Value::Named {
                name: "Char".to_string(),
                content: NamedContent::Unit,
            },
            TypeKind::String => Value::Named {
                name: "String".to_string(),
                content: NamedContent::Unit,
            },
            TypeKind::Unit => Value::Named {
                name: "Unit".to_string(),
                content: NamedContent::Unit,
            },
            TypeKind::Option(inner) => Value::Named {
                name: "Option".to_string(),
                content: NamedContent::Tuple(vec![inner.to_ron_value()?]),
            },
            TypeKind::List(inner) => Value::Named {
                name: "List".to_string(),
                content: NamedContent::Tuple(vec![inner.to_ron_value()?]),
            },
            TypeKind::Map { key, value } => {
                let fields: StructFields = vec![
                    ("key".to_string(), key.to_ron_value()?),
                    ("value".to_string(), value.to_ron_value()?),
                ];
                Value::Named {
                    name: "Map".to_string(),
                    content: NamedContent::Struct(fields),
                }
            }
            TypeKind::Tuple(types) => {
                let values: Result<Vec<_>, _> = types.iter().map(|t| t.to_ron_value()).collect();
                Value::Named {
                    name: "Tuple".to_string(),
                    content: NamedContent::Tuple(vec![Value::Seq(values?)]),
                }
            }
            TypeKind::Struct { fields } => {
                let field_values: Result<Vec<_>, _> =
                    fields.iter().map(|f| f.to_ron_value()).collect();
                Value::Named {
                    name: "Struct".to_string(),
                    content: NamedContent::Struct(vec![(
                        "fields".to_string(),
                        Value::Seq(field_values?),
                    )]),
                }
            }
            TypeKind::Enum { variants } => {
                let variant_values: Result<Vec<_>, _> =
                    variants.iter().map(|v| v.to_ron_value()).collect();
                Value::Named {
                    name: "Enum".to_string(),
                    content: NamedContent::Struct(vec![(
                        "variants".to_string(),
                        Value::Seq(variant_values?),
                    )]),
                }
            }
            TypeKind::TypeRef(path) => Value::Named {
                name: "TypeRef".to_string(),
                content: NamedContent::Tuple(vec![Value::String(path.clone())]),
            },
        };
        Ok(value_to_expr(value))
    }
}

impl ToRon for Field {
    fn to_ast(&self) -> Result<Expr<'static>> {
        let mut fields: StructFields = Vec::new();
        fields.push(("name".to_string(), Value::String(self.name.clone())));
        fields.push(("ty".to_string(), self.ty.to_ron_value()?));
        if let Some(ref doc) = self.doc {
            fields.push(("doc".to_string(), Value::String(doc.clone())));
        }
        if self.optional {
            fields.push(("optional".to_string(), Value::Bool(true)));
        }
        if self.flattened {
            fields.push(("flattened".to_string(), Value::Bool(true)));
        }
        let value = Value::Named {
            name: "Field".to_string(),
            content: NamedContent::Struct(fields),
        };
        Ok(value_to_expr(value))
    }
}

impl ToRon for Variant {
    fn to_ast(&self) -> Result<Expr<'static>> {
        let mut fields: StructFields = Vec::new();
        fields.push(("name".to_string(), Value::String(self.name.clone())));
        if let Some(ref doc) = self.doc {
            fields.push(("doc".to_string(), Value::String(doc.clone())));
        }
        fields.push(("kind".to_string(), self.kind.to_ron_value()?));
        let value = Value::Named {
            name: "Variant".to_string(),
            content: NamedContent::Struct(fields),
        };
        Ok(value_to_expr(value))
    }
}

impl ToRon for VariantKind {
    fn to_ast(&self) -> Result<Expr<'static>> {
        let value = match self {
            VariantKind::Unit => Value::Named {
                name: "Unit".to_string(),
                content: NamedContent::Unit,
            },
            VariantKind::Tuple(types) => {
                let values: Result<Vec<_>, _> = types.iter().map(|t| t.to_ron_value()).collect();
                Value::Named {
                    name: "Tuple".to_string(),
                    content: NamedContent::Tuple(vec![Value::Seq(values?)]),
                }
            }
            VariantKind::Struct(fields) => {
                let field_values: Result<Vec<_>, _> =
                    fields.iter().map(|f| f.to_ron_value()).collect();
                Value::Named {
                    name: "Struct".to_string(),
                    content: NamedContent::Tuple(vec![Value::Seq(field_values?)]),
                }
            }
        };
        Ok(value_to_expr(value))
    }
}

// =============================================================================
// FromRon implementations
// =============================================================================

impl FromRon for Schema {
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        let value = expr_to_value(expr)?;
        Self::from_ron_value(value).map_err(|e| SpannedError {
            code: e,
            span: expr.span().clone(),
        })
    }

    fn from_ron_value(value: Value) -> Result<Self> {
        let (name, content) = match value {
            Value::Named { name, content } => (name, content),
            Value::Struct(fields) => ("Schema".to_string(), NamedContent::Struct(fields)),
            other => return Err(Error::type_mismatch("Schema", &other)),
        };

        if name != "Schema" {
            return Err(Error::invalid_value(format!("expected Schema, got {name}")));
        }

        let fields = match content {
            NamedContent::Struct(f) => f,
            _ => return Err(Error::invalid_value("expected struct content")),
        };

        let mut doc = None;
        let mut kind = None;

        for (key, val) in fields {
            match key.as_str() {
                "doc" => {
                    // Handle both `doc: "text"` and `doc: Some("text")` / `doc: None`
                    doc = match val {
                        Value::Option(None) => None,
                        Value::Option(Some(inner)) => Some(String::from_ron_value(*inner)?),
                        other => Some(String::from_ron_value(other)?),
                    };
                }
                "kind" => kind = Some(TypeKind::from_ron_value(val)?),
                _ => {}
            }
        }

        Ok(Schema {
            doc,
            kind: kind.ok_or_else(|| Error::missing_field("kind"))?,
        })
    }
}

impl FromRon for TypeKind {
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        let value = expr_to_value(expr)?;
        Self::from_ron_value(value).map_err(|e| SpannedError {
            code: e,
            span: expr.span().clone(),
        })
    }

    fn from_ron_value(value: Value) -> Result<Self> {
        let (name, content) = match value {
            Value::Named { name, content } => (name, content),
            other => return Err(Error::type_mismatch("TypeKind", &other)),
        };

        match name.as_str() {
            "Bool" => Ok(TypeKind::Bool),
            "I8" => Ok(TypeKind::I8),
            "I16" => Ok(TypeKind::I16),
            "I32" => Ok(TypeKind::I32),
            "I64" => Ok(TypeKind::I64),
            "I128" => Ok(TypeKind::I128),
            "U8" => Ok(TypeKind::U8),
            "U16" => Ok(TypeKind::U16),
            "U32" => Ok(TypeKind::U32),
            "U64" => Ok(TypeKind::U64),
            "U128" => Ok(TypeKind::U128),
            "F32" => Ok(TypeKind::F32),
            "F64" => Ok(TypeKind::F64),
            "Char" => Ok(TypeKind::Char),
            "String" => Ok(TypeKind::String),
            "Unit" => Ok(TypeKind::Unit),
            "Option" => {
                let inner = extract_tuple_arg(content)?;
                Ok(TypeKind::Option(Box::new(TypeKind::from_ron_value(inner)?)))
            }
            "List" | "Vec" => {
                let inner = extract_tuple_arg(content)?;
                Ok(TypeKind::List(Box::new(TypeKind::from_ron_value(inner)?)))
            }
            "Map" => {
                let fields = extract_struct_fields(content)?;
                let mut key = None;
                let mut value = None;
                for (k, v) in fields {
                    match k.as_str() {
                        "key" => key = Some(TypeKind::from_ron_value(v)?),
                        "value" => value = Some(TypeKind::from_ron_value(v)?),
                        _ => {}
                    }
                }
                Ok(TypeKind::Map {
                    key: Box::new(key.ok_or_else(|| Error::missing_field("key"))?),
                    value: Box::new(value.ok_or_else(|| Error::missing_field("value"))?),
                })
            }
            "Tuple" => {
                let inner = extract_tuple_arg(content)?;
                let types = Vec::<TypeKind>::from_ron_value(inner)?;
                Ok(TypeKind::Tuple(types))
            }
            "Struct" => {
                let fields = extract_struct_fields(content)?;
                for (k, v) in fields {
                    if k == "fields" {
                        let field_list = Vec::<Field>::from_ron_value(v)?;
                        return Ok(TypeKind::Struct { fields: field_list });
                    }
                }
                Err(Error::missing_field("fields"))
            }
            "Enum" => {
                let fields = extract_struct_fields(content)?;
                for (k, v) in fields {
                    if k == "variants" {
                        let variant_list = Vec::<Variant>::from_ron_value(v)?;
                        return Ok(TypeKind::Enum {
                            variants: variant_list,
                        });
                    }
                }
                Err(Error::missing_field("variants"))
            }
            "TypeRef" => {
                let path = extract_tuple_arg(content)?;
                Ok(TypeKind::TypeRef(String::from_ron_value(path)?))
            }
            _ => Err(Error::invalid_value(format!("unknown variant: {name}"))),
        }
    }
}

impl FromRon for Field {
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        let value = expr_to_value(expr)?;
        Self::from_ron_value(value).map_err(|e| SpannedError {
            code: e,
            span: expr.span().clone(),
        })
    }

    fn from_ron_value(value: Value) -> Result<Self> {
        let fields = match value {
            Value::Named {
                content: NamedContent::Struct(f),
                ..
            } => f,
            Value::Struct(f) => f,
            other => return Err(Error::type_mismatch("Field", &other)),
        };

        let mut name = None;
        let mut ty = None;
        let mut doc = None;
        let mut optional = false;
        let mut flattened = false;

        for (key, val) in fields {
            match key.as_str() {
                "name" => name = Some(String::from_ron_value(val)?),
                "ty" => ty = Some(TypeKind::from_ron_value(val)?),
                "doc" => doc = Some(String::from_ron_value(val)?),
                "optional" => optional = bool::from_ron_value(val)?,
                "flattened" => flattened = bool::from_ron_value(val)?,
                _ => {}
            }
        }

        Ok(Field {
            name: name.ok_or_else(|| Error::missing_field("name"))?,
            ty: ty.ok_or_else(|| Error::missing_field("ty"))?,
            doc,
            optional,
            flattened,
        })
    }
}

impl FromRon for Variant {
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        let value = expr_to_value(expr)?;
        Self::from_ron_value(value).map_err(|e| SpannedError {
            code: e,
            span: expr.span().clone(),
        })
    }

    fn from_ron_value(value: Value) -> Result<Self> {
        let fields = match value {
            Value::Named {
                content: NamedContent::Struct(f),
                ..
            } => f,
            Value::Struct(f) => f,
            other => return Err(Error::type_mismatch("Variant", &other)),
        };

        let mut name = None;
        let mut doc = None;
        let mut kind = None;

        for (key, val) in fields {
            match key.as_str() {
                "name" => name = Some(String::from_ron_value(val)?),
                "doc" => doc = Some(String::from_ron_value(val)?),
                "kind" => kind = Some(VariantKind::from_ron_value(val)?),
                _ => {}
            }
        }

        Ok(Variant {
            name: name.ok_or_else(|| Error::missing_field("name"))?,
            doc,
            kind: kind.ok_or_else(|| Error::missing_field("kind"))?,
        })
    }
}

impl FromRon for VariantKind {
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        let value = expr_to_value(expr)?;
        Self::from_ron_value(value).map_err(|e| SpannedError {
            code: e,
            span: expr.span().clone(),
        })
    }

    fn from_ron_value(value: Value) -> Result<Self> {
        let (name, content) = match value {
            Value::Named { name, content } => (name, content),
            other => return Err(Error::type_mismatch("VariantKind", &other)),
        };

        match name.as_str() {
            "Unit" => Ok(VariantKind::Unit),
            "Tuple" => {
                let inner = extract_tuple_arg(content)?;
                let types = Vec::<TypeKind>::from_ron_value(inner)?;
                Ok(VariantKind::Tuple(types))
            }
            "Struct" => {
                let inner = extract_tuple_arg(content)?;
                let fields = Vec::<Field>::from_ron_value(inner)?;
                Ok(VariantKind::Struct(fields))
            }
            _ => Err(Error::invalid_value(format!("unknown variant: {name}"))),
        }
    }
}

// Helper functions for deserializing
fn extract_tuple_arg(content: NamedContent) -> Result<Value> {
    match content {
        NamedContent::Tuple(mut args) if !args.is_empty() => Ok(args.remove(0)),
        _ => Err(Error::invalid_value("expected tuple with one argument")),
    }
}

fn extract_struct_fields(content: NamedContent) -> Result<StructFields> {
    match content {
        NamedContent::Struct(fields) => Ok(fields),
        _ => Err(Error::invalid_value("expected struct content")),
    }
}
