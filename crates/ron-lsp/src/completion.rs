//! Completion provider for the RON Language Server.
//!
//! Provides auto-completion for struct fields and enum variants
//! based on the schema.

use ron2::Value;
use ron_schema::{Field, Schema, TypeKind, Variant, VariantKind};
use tower_lsp::lsp_types::*;

use crate::document::{CompletionContext, Document};
use crate::schema_resolver::SchemaResolver;

/// Provide completions for a document at a given position.
pub fn provide_completions(
    doc: &Document,
    position: Position,
    resolver: &SchemaResolver,
) -> Vec<CompletionItem> {
    let Some(schema) = resolver.resolve_schema(doc) else {
        return vec![];
    };

    let context = doc.context_at_position(position.line, position.character);

    match context {
        CompletionContext::Root => completions_for_type(&schema.kind, &schema),
        CompletionContext::FieldName => {
            // Get fields that haven't been used yet
            let used_fields = get_used_fields(doc);
            completions_for_fields(&schema.kind, &used_fields)
        }
        CompletionContext::Value => completions_for_value(&schema.kind),
        CompletionContext::InString | CompletionContext::Unknown => vec![],
    }
}

/// Generate completions for a type at root level.
fn completions_for_type(kind: &TypeKind, schema: &Schema) -> Vec<CompletionItem> {
    match kind {
        TypeKind::Struct { fields } => {
            // At root, suggest starting the struct
            let mut items = vec![];

            // Suggest a struct snippet
            let field_snippets: Vec<String> = fields
                .iter()
                .enumerate()
                .filter(|(_, f)| !f.optional)
                .map(|(i, f)| format!("    {}: ${{{}}}", f.name, i + 1))
                .collect();

            if !field_snippets.is_empty() {
                items.push(CompletionItem {
                    label: "(...)".to_string(),
                    kind: Some(CompletionItemKind::STRUCT),
                    detail: Some("Struct value".to_string()),
                    documentation: schema.doc.as_ref().map(|d| {
                        Documentation::MarkupContent(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: d.clone(),
                        })
                    }),
                    insert_text: Some(format!("(\n{}\n)", field_snippets.join(",\n"))),
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    ..Default::default()
                });
            }

            items
        }
        TypeKind::Enum { variants } => completions_for_enum_variants(variants),
        _ => vec![],
    }
}

/// Generate completions for struct fields.
fn completions_for_fields(kind: &TypeKind, used_fields: &[String]) -> Vec<CompletionItem> {
    let TypeKind::Struct { fields } = kind else {
        return vec![];
    };

    fields
        .iter()
        .filter(|f| !used_fields.contains(&f.name))
        .map(|f| completion_for_field(f))
        .collect()
}

/// Generate a completion item for a struct field.
fn completion_for_field(field: &Field) -> CompletionItem {
    let type_hint = type_kind_to_string(&field.ty);
    let optional_marker = if field.optional { " (optional)" } else { "" };

    CompletionItem {
        label: field.name.clone(),
        kind: Some(CompletionItemKind::FIELD),
        detail: Some(format!("{}{}", type_hint, optional_marker)),
        documentation: field.doc.as_ref().map(|d| {
            Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: d.clone(),
            })
        }),
        insert_text: Some(format!("{}: ", field.name)),
        insert_text_format: Some(InsertTextFormat::PLAIN_TEXT),
        ..Default::default()
    }
}

/// Generate completions for enum variants.
fn completions_for_enum_variants(variants: &[Variant]) -> Vec<CompletionItem> {
    variants.iter().map(completion_for_variant).collect()
}

/// Generate a completion item for an enum variant.
fn completion_for_variant(variant: &Variant) -> CompletionItem {
    let (insert_text, detail) = match &variant.kind {
        VariantKind::Unit => (variant.name.clone(), "Unit variant".to_string()),
        VariantKind::Tuple(types) => {
            let type_list = types
                .iter()
                .map(type_kind_to_string)
                .collect::<Vec<_>>()
                .join(", ");
            (
                format!("{}()", variant.name),
                format!("Tuple variant({})", type_list),
            )
        }
        VariantKind::Struct(fields) => {
            let field_list = fields
                .iter()
                .map(|f| format!("{}: {}", f.name, type_kind_to_string(&f.ty)))
                .collect::<Vec<_>>()
                .join(", ");
            (
                format!("{}()", variant.name),
                format!("Struct variant {{ {} }}", field_list),
            )
        }
    };

    CompletionItem {
        label: variant.name.clone(),
        kind: Some(CompletionItemKind::ENUM_MEMBER),
        detail: Some(detail),
        documentation: variant.doc.as_ref().map(|d| {
            Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: d.clone(),
            })
        }),
        insert_text: Some(insert_text),
        insert_text_format: Some(InsertTextFormat::PLAIN_TEXT),
        ..Default::default()
    }
}

/// Generate completions for value context (after a colon).
fn completions_for_value(kind: &TypeKind) -> Vec<CompletionItem> {
    match kind {
        TypeKind::Bool => vec![
            CompletionItem {
                label: "true".to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                ..Default::default()
            },
            CompletionItem {
                label: "false".to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                ..Default::default()
            },
        ],
        TypeKind::Option(inner) => {
            let mut items = vec![
                CompletionItem {
                    label: "None".to_string(),
                    kind: Some(CompletionItemKind::KEYWORD),
                    detail: Some("No value".to_string()),
                    ..Default::default()
                },
                CompletionItem {
                    label: "Some(...)".to_string(),
                    kind: Some(CompletionItemKind::KEYWORD),
                    detail: Some(format!("Some({})", type_kind_to_string(inner))),
                    insert_text: Some("Some($1)".to_string()),
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    ..Default::default()
                },
            ];
            // Also add completions for the inner type
            items.extend(completions_for_value(inner));
            items
        }
        TypeKind::Enum { variants } => completions_for_enum_variants(variants),
        TypeKind::List(_) => vec![CompletionItem {
            label: "[]".to_string(),
            kind: Some(CompletionItemKind::SNIPPET),
            detail: Some("Empty list".to_string()),
            insert_text: Some("[$1]".to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        }],
        TypeKind::Map { .. } => vec![CompletionItem {
            label: "{}".to_string(),
            kind: Some(CompletionItemKind::SNIPPET),
            detail: Some("Empty map".to_string()),
            insert_text: Some("{$1}".to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        }],
        TypeKind::Struct { .. } => vec![CompletionItem {
            label: "(...)".to_string(),
            kind: Some(CompletionItemKind::STRUCT),
            detail: Some("Struct value".to_string()),
            insert_text: Some("(\n    $1\n)".to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        }],
        TypeKind::Unit => vec![CompletionItem {
            label: "()".to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some("Unit value".to_string()),
            ..Default::default()
        }],
        _ => vec![],
    }
}

/// Get the list of field names already used in the document.
fn get_used_fields(doc: &Document) -> Vec<String> {
    let Some(ref value) = doc.parsed_value else {
        return vec![];
    };

    extract_field_names(value)
}

/// Extract field names from a RON value.
fn extract_field_names(value: &Value) -> Vec<String> {
    match value {
        Value::Map(map) => map
            .iter()
            .filter_map(|(k, _)| {
                if let Value::String(s) = k {
                    Some(s.clone())
                } else {
                    None
                }
            })
            .collect(),
        _ => vec![],
    }
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
        TypeKind::List(inner) => format!("List<{}>", type_kind_to_string(inner)),
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
