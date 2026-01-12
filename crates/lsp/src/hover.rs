//! Hover provider for the RON Language Server.
//!
//! Shows documentation from the schema when hovering over
//! field names or enum variants.

use ron2::schema::{Field, Schema, TypeKind, Variant, VariantKind};
use tower_lsp::lsp_types::*;

use crate::{
    document::Document, lsp_utils::find_word_range_from_ast, schema_resolver::SchemaResolver,
    schema_utils::VariantParts,
};

/// Provide hover information for a position in the document.
pub fn provide_hover(
    doc: &Document,
    position: Position,
    resolver: &SchemaResolver,
) -> Option<Hover> {
    let word = doc.word_at_position(position.line, position.character)?;
    let schema = resolver.resolve_schema(doc)?;

    // Try to find documentation for this word in the schema
    let info = find_hover_info(word, &schema, resolver)?;

    // Try to get a precise range from AST
    let range = find_word_range_from_ast(doc, word);

    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: info,
        }),
        range,
    })
}

/// Find hover information for a word in the schema.
fn find_hover_info(word: &str, schema: &Schema, resolver: &SchemaResolver) -> Option<String> {
    // First check if the word matches the root type
    if let Some(info) = hover_for_type_kind(word, &schema.kind, schema.doc.as_deref(), resolver) {
        return Some(info);
    }

    // Search within the schema for matching fields or variants
    search_in_type_kind(word, &schema.kind, resolver)
}

/// Generate hover info if the word matches something at this type level.
fn hover_for_type_kind(
    word: &str,
    kind: &TypeKind,
    doc: Option<&str>,
    resolver: &SchemaResolver,
) -> Option<String> {
    // Resolve TypeRef if present
    if let TypeKind::TypeRef(type_path) = kind {
        if let Some(ref_schema) = resolver.load_schema_by_type(type_path) {
            return hover_for_type_kind(
                word,
                &ref_schema.kind,
                ref_schema.doc.as_deref(),
                resolver,
            );
        }
        // TypeRef couldn't be resolved - check if word matches the type name
        let type_name = type_path.rsplit("::").next().unwrap_or(type_path);
        if type_name == word {
            return doc.map(|d| format!("**{}**\n\n{}", type_name, d));
        }
        return None;
    }

    // Check if word matches a struct field
    if let Some(fields) = kind.struct_fields() {
        for field in fields {
            if field.name == word {
                return Some(format_field_hover(field));
            }
        }
        return None;
    }

    // Check if word matches an enum variant
    if let Some(variants) = kind.enum_variants() {
        for variant in variants {
            if variant.name == word {
                return Some(format_variant_hover(variant));
            }
        }
        return None;
    }

    None
}

/// Recursively search for hover info in nested types.
fn search_in_type_kind(word: &str, kind: &TypeKind, resolver: &SchemaResolver) -> Option<String> {
    // Resolve TypeRef if present
    if let TypeKind::TypeRef(type_path) = kind {
        if let Some(ref_schema) = resolver.load_schema_by_type(type_path) {
            return search_in_type_kind(word, &ref_schema.kind, resolver);
        }
        return None;
    }

    // Check struct fields
    if let Some(fields) = kind.struct_fields() {
        for field in fields {
            if field.name == word {
                return Some(format_field_hover(field));
            }
            if let Some(info) = search_in_type_kind(word, &field.ty, resolver) {
                return Some(info);
            }
        }
        return None;
    }

    // Check enum variants
    if let Some(variants) = kind.enum_variants() {
        for variant in variants {
            if variant.name == word {
                return Some(format_variant_hover(variant));
            }
            if let Some(info) = search_in_variant(word, variant, resolver) {
                return Some(info);
            }
        }
        return None;
    }

    // Recurse into container types
    if let Some(inner) = kind.inner_type() {
        return search_in_type_kind(word, inner, resolver);
    }

    if let Some((key, value)) = kind.map_types() {
        return search_in_type_kind(word, key, resolver)
            .or_else(|| search_in_type_kind(word, value, resolver));
    }

    if let Some(types) = kind.tuple_types() {
        for ty in types {
            if let Some(info) = search_in_type_kind(word, ty, resolver) {
                return Some(info);
            }
        }
    }

    None
}

/// Search for hover info within an enum variant.
fn search_in_variant(word: &str, variant: &Variant, resolver: &SchemaResolver) -> Option<String> {
    match &variant.kind {
        VariantKind::Unit => None,
        VariantKind::Tuple(types) => {
            for ty in types {
                if let Some(info) = search_in_type_kind(word, ty, resolver) {
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
                if let Some(info) = search_in_type_kind(word, &field.ty, resolver) {
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
    let type_str = field.ty.to_string();
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
    let parts = VariantParts::from_variant(variant);
    let mut output = vec![parts.markdown_signature()];

    // Documentation
    if let Some(ref doc) = variant.doc {
        output.push(String::new()); // Empty line
        output.push(doc.clone());
    }

    // For struct variants, also show field docs
    if let VariantKind::Struct(fields) = &variant.kind {
        let field_docs: Vec<String> = fields
            .iter()
            .filter_map(|f| f.doc.as_ref().map(|d| format!("- **{}**: {}", f.name, d)))
            .collect();

        if !field_docs.is_empty() {
            output.push(String::new());
            output.push("**Fields:**".to_string());
            output.extend(field_docs);
        }
    }

    output.join("\n")
}

#[cfg(test)]
mod tests {
    use ron2::schema::{Field, TypeKind};

    use super::*;

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
