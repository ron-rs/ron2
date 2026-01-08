//! Hover provider for the RON Language Server.
//!
//! Shows documentation from the schema when hovering over
//! field names or enum variants.

use ron_schema::{Field, Schema, TypeKind, Variant, VariantKind};
use tower_lsp::lsp_types::*;

use crate::document::Document;
use crate::schema_resolver::SchemaResolver;

/// Provide hover information for a position in the document.
pub fn provide_hover(
    doc: &Document,
    position: Position,
    resolver: &SchemaResolver,
) -> Option<Hover> {
    let word = doc.word_at_position(position.line, position.character)?;
    let schema = resolver.resolve_schema(doc)?;

    // Try to find documentation for this word in the schema
    let info = find_hover_info(word, &schema)?;

    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: info,
        }),
        range: None,
    })
}

/// Find hover information for a word in the schema.
fn find_hover_info(word: &str, schema: &Schema) -> Option<String> {
    // First check if the word matches the root type
    if let Some(info) = hover_for_type_kind(word, &schema.kind, schema.doc.as_deref()) {
        return Some(info);
    }

    // Search within the schema for matching fields or variants
    search_in_type_kind(word, &schema.kind)
}

/// Generate hover info if the word matches something at this type level.
fn hover_for_type_kind(word: &str, kind: &TypeKind, doc: Option<&str>) -> Option<String> {
    match kind {
        TypeKind::Struct { fields } => {
            // Check if word matches a field name
            for field in fields {
                if field.name == word {
                    return Some(format_field_hover(field));
                }
            }
            None
        }
        TypeKind::Enum { variants } => {
            // Check if word matches a variant name
            for variant in variants {
                if variant.name == word {
                    return Some(format_variant_hover(variant));
                }
            }
            None
        }
        TypeKind::TypeRef(path) => {
            // If the word matches the type name, show the doc
            let type_name = path.rsplit("::").next().unwrap_or(path);
            if type_name == word {
                return doc.map(|d| format!("**{}**\n\n{}", type_name, d));
            }
            None
        }
        _ => None,
    }
}

/// Recursively search for hover info in nested types.
fn search_in_type_kind(word: &str, kind: &TypeKind) -> Option<String> {
    match kind {
        TypeKind::Struct { fields } => {
            // First check direct fields
            for field in fields {
                if field.name == word {
                    return Some(format_field_hover(field));
                }
                // Then check nested types
                if let Some(info) = search_in_type_kind(word, &field.ty) {
                    return Some(info);
                }
            }
            None
        }
        TypeKind::Enum { variants } => {
            // First check variant names
            for variant in variants {
                if variant.name == word {
                    return Some(format_variant_hover(variant));
                }
                // Then check variant contents
                if let Some(info) = search_in_variant(word, variant) {
                    return Some(info);
                }
            }
            None
        }
        TypeKind::Option(inner) => search_in_type_kind(word, inner),
        TypeKind::Vec(inner) => search_in_type_kind(word, inner),
        TypeKind::Map { key, value } => search_in_type_kind(word, key)
            .or_else(|| search_in_type_kind(word, value)),
        TypeKind::Tuple(types) => {
            for ty in types {
                if let Some(info) = search_in_type_kind(word, ty) {
                    return Some(info);
                }
            }
            None
        }
        _ => None,
    }
}

/// Search for hover info within an enum variant.
fn search_in_variant(word: &str, variant: &Variant) -> Option<String> {
    match &variant.kind {
        VariantKind::Unit => None,
        VariantKind::Tuple(types) => {
            for ty in types {
                if let Some(info) = search_in_type_kind(word, ty) {
                    return Some(info);
                }
            }
            None
        }
        VariantKind::Struct(fields) => {
            for field in fields {
                if field.name == word {
                    return Some(format_field_hover(field));
                }
                if let Some(info) = search_in_type_kind(word, &field.ty) {
                    return Some(info);
                }
            }
            None
        }
    }
}

/// Format hover information for a field.
fn format_field_hover(field: &Field) -> String {
    let mut parts = Vec::new();

    // Field name and type
    let type_str = type_kind_to_string(&field.ty);
    let optional = if field.optional { " (optional)" } else { "" };
    parts.push(format!("**{}**: `{}`{}", field.name, type_str, optional));

    // Documentation
    if let Some(ref doc) = field.doc {
        parts.push(String::new()); // Empty line
        parts.push(doc.clone());
    }

    parts.join("\n")
}

/// Format hover information for a variant.
fn format_variant_hover(variant: &Variant) -> String {
    let mut parts = Vec::new();

    // Variant signature
    let sig = match &variant.kind {
        VariantKind::Unit => format!("**{}**", variant.name),
        VariantKind::Tuple(types) => {
            let type_list = types
                .iter()
                .map(type_kind_to_string)
                .collect::<Vec<_>>()
                .join(", ");
            format!("**{}**({})", variant.name, type_list)
        }
        VariantKind::Struct(fields) => {
            let field_list = fields
                .iter()
                .map(|f| format!("{}: {}", f.name, type_kind_to_string(&f.ty)))
                .collect::<Vec<_>>()
                .join(", ");
            format!("**{}** {{ {} }}", variant.name, field_list)
        }
    };
    parts.push(sig);

    // Documentation
    if let Some(ref doc) = variant.doc {
        parts.push(String::new()); // Empty line
        parts.push(doc.clone());
    }

    // For struct variants, also show field docs
    if let VariantKind::Struct(fields) = &variant.kind {
        let field_docs: Vec<String> = fields
            .iter()
            .filter(|f| f.doc.is_some())
            .map(|f| format!("- **{}**: {}", f.name, f.doc.as_ref().unwrap()))
            .collect();

        if !field_docs.is_empty() {
            parts.push(String::new());
            parts.push("**Fields:**".to_string());
            parts.extend(field_docs);
        }
    }

    parts.join("\n")
}

/// Convert a TypeKind to a human-readable string.
fn type_kind_to_string(kind: &TypeKind) -> String {
    match kind {
        TypeKind::Bool => "bool".to_string(),
        TypeKind::I8 => "i8".to_string(),
        TypeKind::I16 => "i16".to_string(),
        TypeKind::I32 => "i32".to_string(),
        TypeKind::I64 => "i64".to_string(),
        TypeKind::I128 => "i128".to_string(),
        TypeKind::U8 => "u8".to_string(),
        TypeKind::U16 => "u16".to_string(),
        TypeKind::U32 => "u32".to_string(),
        TypeKind::U64 => "u64".to_string(),
        TypeKind::U128 => "u128".to_string(),
        TypeKind::F32 => "f32".to_string(),
        TypeKind::F64 => "f64".to_string(),
        TypeKind::Char => "char".to_string(),
        TypeKind::String => "String".to_string(),
        TypeKind::Unit => "()".to_string(),
        TypeKind::Option(inner) => format!("Option<{}>", type_kind_to_string(inner)),
        TypeKind::Vec(inner) => format!("Vec<{}>", type_kind_to_string(inner)),
        TypeKind::Map { key, value } => {
            format!(
                "Map<{}, {}>",
                type_kind_to_string(key),
                type_kind_to_string(value)
            )
        }
        TypeKind::Tuple(types) => {
            let inner = types
                .iter()
                .map(type_kind_to_string)
                .collect::<Vec<_>>()
                .join(", ");
            format!("({})", inner)
        }
        TypeKind::Struct { .. } => "Struct".to_string(),
        TypeKind::Enum { .. } => "Enum".to_string(),
        TypeKind::TypeRef(path) => path.clone(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ron_schema::{Field, TypeKind};

    #[test]
    fn test_format_field_hover() {
        let field = Field::new("port", TypeKind::U16).with_doc("The server port");
        let hover = format_field_hover(&field);

        assert!(hover.contains("**port**"));
        assert!(hover.contains("`u16`"));
        assert!(hover.contains("The server port"));
    }

    #[test]
    fn test_format_optional_field_hover() {
        let field = Field::optional("host", TypeKind::String).with_doc("The hostname");
        let hover = format_field_hover(&field);

        assert!(hover.contains("(optional)"));
    }
}
