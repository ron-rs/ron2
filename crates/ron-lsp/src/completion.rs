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
        CompletionContext::Root => completions_for_type(&schema.kind, &schema, resolver),
        CompletionContext::FieldName => {
            // Get fields that haven't been used yet
            let used_fields = get_used_fields(doc);
            completions_for_fields(&schema.kind, &used_fields)
        }
        CompletionContext::Value => {
            // Try to determine which field we're completing a value for
            if let Some(field_name) = find_field_at_cursor(doc, position.line, position.character) {
                if let Some(field_type) = get_field_type(&schema.kind, &field_name) {
                    return completions_for_value_type(field_type, resolver);
                }
            }
            // Fall back to root type completions
            completions_for_value_type(&schema.kind, resolver)
        }
        CompletionContext::InString | CompletionContext::Unknown => vec![],
    }
}

/// Find the field name at the cursor position (for Value context).
///
/// Uses AST-based lookup when available, falling back to text-based heuristics.
fn find_field_at_cursor(doc: &Document, line: u32, col: u32) -> Option<String> {
    // Prefer AST-based lookup (more accurate)
    if let Some(field) = doc.find_field_at_position(line, col) {
        return Some(field);
    }

    // Fall back to text-based heuristics
    find_field_at_cursor_text(doc, line, col)
}

/// Text-based fallback for finding field at cursor (used when AST unavailable).
fn find_field_at_cursor_text(doc: &Document, line: u32, col: u32) -> Option<String> {
    let offset = doc.position_to_offset(line, col)?;
    let before = &doc.content[..offset];

    // Find the most recent colon that's not inside a string
    let mut in_string = false;
    let mut escape_next = false;
    let mut paren_depth = 0i32;
    let mut colon_pos = None;

    for (i, c) in before.char_indices() {
        if escape_next {
            escape_next = false;
            continue;
        }
        match c {
            '\\' if in_string => escape_next = true,
            '"' => in_string = !in_string,
            '(' if !in_string => paren_depth += 1,
            ')' if !in_string => paren_depth -= 1,
            ':' if !in_string && paren_depth > 0 => colon_pos = Some(i),
            _ => {}
        }
    }

    let colon_pos = colon_pos?;

    // Extract the field name before the colon
    let before_colon = &before[..colon_pos];
    let trimmed = before_colon.trim_end();

    // Find the start of the identifier
    let field_start = trimmed
        .rfind(|c: char| !c.is_ascii_alphanumeric() && c != '_')
        .map(|i| i + 1)
        .unwrap_or(0);

    let field_name = &trimmed[field_start..];
    if field_name.is_empty() {
        return None;
    }

    Some(field_name.to_string())
}

/// Get the type of a field from a struct schema.
fn get_field_type<'a>(kind: &'a TypeKind, field_name: &str) -> Option<&'a TypeKind> {
    let TypeKind::Struct { fields } = kind else {
        return None;
    };

    fields.iter().find(|f| f.name == field_name).map(|f| &f.ty)
}

/// Generate completions for a type at root level.
fn completions_for_type(
    kind: &TypeKind,
    schema: &Schema,
    resolver: &SchemaResolver,
) -> Vec<CompletionItem> {
    // Resolve TypeRef if present
    let resolved = match kind {
        TypeKind::TypeRef(type_path) => {
            if let Some(ref_schema) = resolver.load_schema_by_type(type_path) {
                return completions_for_type(&ref_schema.kind, &ref_schema, resolver);
            }
            return vec![];
        }
        _ => kind,
    };

    match resolved {
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
        .map(completion_for_field)
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

/// Generate completions for a value type, resolving TypeRefs.
fn completions_for_value_type(kind: &TypeKind, resolver: &SchemaResolver) -> Vec<CompletionItem> {
    // First resolve TypeRef if present
    if let TypeKind::TypeRef(type_path) = kind {
        if let Some(schema) = resolver.load_schema_by_type(type_path) {
            return completions_for_value_type(&schema.kind, resolver);
        }
        // TypeRef couldn't be resolved, no completions
        return vec![];
    }

    // Now handle the actual type
    completions_for_value(kind, resolver)
}

/// Generate completions for value context (after a colon).
fn completions_for_value(kind: &TypeKind, resolver: &SchemaResolver) -> Vec<CompletionItem> {
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
            // Also add completions for the inner type (resolving TypeRefs)
            items.extend(completions_for_value_type(inner, resolver));
            items
        }
        TypeKind::Enum { variants } => completions_for_enum_variants(variants),
        TypeKind::TypeRef(type_path) => {
            // Resolve the TypeRef and recurse
            if let Some(schema) = resolver.load_schema_by_type(type_path) {
                return completions_for_value(&schema.kind, resolver);
            }
            vec![]
        }
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
    // Prefer AST-based extraction (more reliable)
    let ast_fields = doc.get_ast_field_names();
    if !ast_fields.is_empty() {
        return ast_fields;
    }

    // Fall back to Value-based extraction
    let Some(ref value) = doc.parsed_value else {
        return vec![];
    };

    extract_field_names_from_value(value)
}

/// Extract field names from a RON value (fallback when AST unavailable).
fn extract_field_names_from_value(value: &Value) -> Vec<String> {
    match value {
        // Anonymous struct: (field: value, ...)
        Value::Struct(fields) => fields.iter().map(|(name, _)| name.clone()).collect(),
        // Named struct: TypeName(field: value, ...)
        Value::Named { content, .. } => {
            if let ron2::NamedContent::Struct(fields) = content {
                fields.iter().map(|(name, _)| name.clone()).collect()
            } else {
                vec![]
            }
        }
        // Map with string keys: { "field": value, ... }
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
