//! Shared utilities for formatting schema types.

use ron2::schema::{Field, TypeKind, Variant, VariantKind};

/// Format a list of types as comma-separated strings.
pub fn format_type_list(types: &[TypeKind]) -> String {
    types
        .iter()
        .map(|t| t.to_string())
        .collect::<Vec<_>>()
        .join(", ")
}

/// Format struct fields as "name: type" pairs.
pub fn format_field_list(fields: &[Field]) -> String {
    fields
        .iter()
        .map(|f| format!("{}: {}", f.name, f.ty))
        .collect::<Vec<_>>()
        .join(", ")
}

/// Extracted variant representation for different rendering contexts.
pub struct VariantParts {
    pub name: String,
    pub kind_label: &'static str,
    pub params: Option<String>,
}

impl VariantParts {
    pub fn from_variant(variant: &Variant) -> Self {
        match &variant.kind {
            VariantKind::Unit => Self {
                name: variant.name.clone(),
                kind_label: "Unit variant",
                params: None,
            },
            VariantKind::Tuple(types) => Self {
                name: variant.name.clone(),
                kind_label: "Tuple variant",
                params: Some(format!("({})", format_type_list(types))),
            },
            VariantKind::Struct(fields) => Self {
                name: variant.name.clone(),
                kind_label: "Struct variant",
                params: Some(format!("{{ {} }}", format_field_list(fields))),
            },
        }
    }

    /// For completion insert text: "Name" or "Name()"
    pub fn insert_text(&self) -> String {
        match &self.params {
            None => self.name.clone(),
            Some(_) => format!("{}()", self.name),
        }
    }

    /// For completion detail: "Tuple variant(i32, String)"
    pub fn detail(&self) -> String {
        match &self.params {
            None => self.kind_label.to_string(),
            Some(p) => format!("{}{}", self.kind_label, p),
        }
    }

    /// For hover markdown: "**Name**(i32, String)"
    pub fn markdown_signature(&self) -> String {
        match &self.params {
            None => format!("**{}**", self.name),
            Some(p) => format!("**{}**{}", self.name, p),
        }
    }
}
