use serde::{Deserialize, Serialize};

/// Root schema definition for a Rust type.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Schema {
    /// Documentation from the Rust type's doc comments.
    #[serde(default, skip_serializing_if = "Option::is_none")]
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
    ///
    /// # Serde Compatibility
    ///
    /// For backwards compatibility, this variant also deserializes from "Vec"
    /// in existing schema files.
    #[serde(alias = "Vec")]
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
