//! Completion provider for the RON Language Server.
//!
//! Provides struct field and value completions for the type at the cursor,
//! found by following the cursor's AST path through the schema.

use ron2::schema::{Field, TypeKind, Variant};
use tower_lsp::lsp_types::*;

use crate::{
    cursor::{CursorContext, PathStep},
    document::Document,
    schema_nav::{DynResolver, Resolved, Target, follow, resolve, struct_fields},
    schema_resolver::SchemaResolver,
    schema_utils::VariantParts,
};

/// Provide completions for a document at a given position.
pub fn provide_completions(
    doc: &Document,
    position: Position,
    resolver: &SchemaResolver,
) -> Vec<CompletionItem> {
    let Some(schema) = resolver.resolve_schema(doc) else {
        return vec![];
    };
    let root = Target::root(&schema);
    let resolver: &DynResolver = resolver;

    match doc.context_at_position(position.line, position.character) {
        CursorContext::FieldName { path, used, .. } => follow(root, &path, resolver)
            .map(|target| field_completions(&target, &used, resolver))
            .unwrap_or_default(),
        CursorContext::Value { path, .. } => follow(root, &path, resolver)
            .map(|target| value_completions(&target, resolver))
            .unwrap_or_default(),
        CursorContext::Parens { path, element } => {
            let Some(target) = follow(root, &path, resolver) else {
                return vec![];
            };
            // `()` or `(name)` inside a struct is a struct body being typed.
            match struct_fields(&target, resolver) {
                Some(fields) => fields.iter().map(field_completion).collect(),
                None => follow(target, &[PathStep::Element(element)], resolver)
                    .map(|t| value_completions(&t, resolver))
                    .unwrap_or_default(),
            }
        }
        CursorContext::None => vec![],
    }
}

fn markdown(text: &str) -> Documentation {
    Documentation::MarkupContent(MarkupContent {
        kind: MarkupKind::Markdown,
        value: text.to_string(),
    })
}

/// Completions for the fields of a struct (or struct variant) not yet used.
fn field_completions(
    target: &Target,
    used: &[String],
    resolver: &DynResolver,
) -> Vec<CompletionItem> {
    struct_fields(target, resolver)
        .unwrap_or_default()
        .iter()
        .filter(|f| !used.contains(&f.name))
        .map(field_completion)
        .collect()
}

fn field_completion(field: &Field) -> CompletionItem {
    let optional_marker = if field.optional { " (optional)" } else { "" };
    CompletionItem {
        label: field.name.clone(),
        kind: Some(CompletionItemKind::FIELD),
        detail: Some(format!("{}{}", field.ty, optional_marker)),
        documentation: field.doc.as_deref().map(markdown),
        insert_text: Some(format!("{}: ", field.name)),
        insert_text_format: Some(InsertTextFormat::PLAIN_TEXT),
        ..Default::default()
    }
}

fn variant_completion(variant: &Variant) -> CompletionItem {
    let parts = VariantParts::from_variant(variant);
    CompletionItem {
        label: variant.name.clone(),
        kind: Some(CompletionItemKind::ENUM_MEMBER),
        detail: Some(parts.detail()),
        documentation: variant.doc.as_deref().map(markdown),
        insert_text: Some(parts.insert_text()),
        insert_text_format: Some(InsertTextFormat::PLAIN_TEXT),
        ..Default::default()
    }
}

fn keyword(label: &str, detail: Option<&str>) -> CompletionItem {
    CompletionItem {
        label: label.to_string(),
        kind: Some(CompletionItemKind::KEYWORD),
        detail: detail.map(str::to_string),
        ..Default::default()
    }
}

fn snippet(label: &str, kind: CompletionItemKind, detail: &str, body: String) -> CompletionItem {
    CompletionItem {
        label: label.to_string(),
        kind: Some(kind),
        detail: Some(detail.to_string()),
        insert_text: Some(body),
        insert_text_format: Some(InsertTextFormat::SNIPPET),
        ..Default::default()
    }
}

/// Completions for a value of the target type.
fn value_completions(target: &Target, resolver: &DynResolver) -> Vec<CompletionItem> {
    let Target::Type { kind, doc } = target else {
        return vec![];
    };
    let Some(Resolved { kind, doc }) = resolve(kind, doc.clone(), false, resolver) else {
        return vec![];
    };

    match &kind {
        TypeKind::Option(inner) => {
            let mut items = vec![
                keyword("None", Some("No value")),
                CompletionItem {
                    insert_text: Some("Some($1)".to_string()),
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    ..keyword("Some(...)", Some(&format!("Some({inner})")))
                },
            ];
            // `Some` is implicit, so the inner type's values are valid too.
            if let Some(inner) = resolve(inner, None, true, resolver) {
                items.extend(concrete_value_completions(&inner, resolver));
            }
            items
        }
        _ => concrete_value_completions(&Resolved { kind, doc }, resolver),
    }
}

/// Completions for a resolved type that is not an `Option`.
fn concrete_value_completions(resolved: &Resolved, resolver: &DynResolver) -> Vec<CompletionItem> {
    match &resolved.kind {
        TypeKind::Bool => vec![keyword("true", None), keyword("false", None)],
        TypeKind::Unit => vec![keyword("()", Some("Unit value"))],
        TypeKind::Enum { variants } => variants.iter().map(variant_completion).collect(),
        TypeKind::List(_) => vec![snippet(
            "[]",
            CompletionItemKind::SNIPPET,
            "List",
            "[$1]".to_string(),
        )],
        TypeKind::Map { .. } => vec![snippet(
            "{}",
            CompletionItemKind::SNIPPET,
            "Map",
            "{$1}".to_string(),
        )],
        TypeKind::Struct { .. } => {
            let target = Target::Type {
                kind: resolved.kind.clone(),
                doc: None,
            };
            let required: Vec<String> = struct_fields(&target, resolver)
                .unwrap_or_default()
                .iter()
                .filter(|f| !f.optional)
                .enumerate()
                .map(|(i, f)| format!("    {}: ${{{}}}", f.name, i + 1))
                .collect();
            let body = if required.is_empty() {
                "($1)".to_string()
            } else {
                format!("(\n{}\n)", required.join(",\n"))
            };
            vec![CompletionItem {
                documentation: resolved.doc.as_deref().map(markdown),
                ..snippet("(...)", CompletionItemKind::STRUCT, "Struct value", body)
            }]
        }
        _ => vec![],
    }
}
