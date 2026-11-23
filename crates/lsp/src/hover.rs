//! Hover provider for the RON Language Server.
//!
//! Shows schema documentation for the field name, enum variant or struct name
//! under the cursor, resolved through the cursor's AST path.

use ron2::schema::{Field, Schema, TypeKind, Variant, VariantKind};
use tower_lsp::lsp_types::*;

use crate::{
    cursor::{CursorContext, Ident},
    document::Document,
    schema_nav::{DynResolver, Resolved, Target, follow, resolve, struct_fields},
    schema_resolver::SchemaResolver,
    schema_utils::VariantParts,
};

/// Provide hover information for a position in the document.
pub fn provide_hover(
    doc: &Document,
    position: Position,
    resolver: &SchemaResolver,
) -> Option<Hover> {
    let schema = resolver.resolve_schema(doc)?;
    let context = doc.context_at_position(position.line, position.character);
    let (info, ident) = hover_info(&schema, context, resolver)?;

    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: info,
        }),
        range: Some(doc.span_to_range(&ident.span)),
    })
}

/// Hover text for the identifier in `context`, and that identifier.
fn hover_info(
    schema: &Schema,
    context: CursorContext,
    resolver: &DynResolver,
) -> Option<(String, Ident)> {
    let root = Target::root(schema);
    match context {
        CursorContext::FieldName {
            path,
            ident: Some(ident),
            ..
        } => {
            let target = follow(root, &path, resolver)?;
            let field = struct_fields(&target, resolver)?
                .into_iter()
                .find(|f| f.name == ident.name)?;
            Some((format_field_hover(&field), ident))
        }
        CursorContext::Value {
            path,
            ident: Some(ident),
        } => {
            let Target::Type { kind, doc } = follow(root, &path, resolver)? else {
                return None;
            };
            let Resolved { kind, doc } = resolve(&kind, doc, true, resolver)?;
            let info = match kind {
                TypeKind::Enum { variants } => {
                    format_variant_hover(variants.iter().find(|v| v.name == ident.name)?)
                }
                TypeKind::Struct { .. } => match doc {
                    Some(doc) => format!("**{}**\n\n{}", ident.name, doc),
                    None => format!("**{}**", ident.name),
                },
                _ => return None,
            };
            Some((info, ident))
        }
        _ => None,
    }
}

/// Format hover information for a field.
fn format_field_hover(field: &Field) -> String {
    let optional = if field.optional { " (optional)" } else { "" };
    let mut out = format!("**{}**: `{}`{}", field.name, field.ty, optional);
    if let Some(doc) = &field.doc {
        out.push_str("\n\n");
        out.push_str(doc);
    }
    out
}

/// Format hover information for a variant.
fn format_variant_hover(variant: &Variant) -> String {
    let mut output = vec![VariantParts::from_variant(variant).markdown_signature()];

    if let Some(doc) = &variant.doc {
        output.push(String::new());
        output.push(doc.clone());
    }

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
    use ron2::{ast::parse_document_lossy, schema::Field};

    use super::*;
    use crate::{cursor::context_at_offset, schema_nav::tests::resolver};

    fn type_ref(path: &str) -> TypeKind {
        TypeKind::TypeRef(path.to_string())
    }

    /// Hover at the `|` marker in `source`.
    fn hover_at(source: &str, root: &str, resolver: &DynResolver) -> Option<String> {
        let offset = source.find('|').expect("cursor marker");
        let source = source.replacen('|', "", 1);
        let (doc, _) = parse_document_lossy(&source);
        let schema = Schema::new(type_ref(root));
        hover_info(&schema, context_at_offset(&doc, offset), resolver).map(|(info, _)| info)
    }

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

    fn tree_resolver() -> Box<DynResolver> {
        Box::new(resolver(&[(
            "t::Tree",
            TypeKind::Struct {
                fields: vec![
                    Field::new("value", TypeKind::I32),
                    Field::new("children", TypeKind::List(Box::new(type_ref("t::Tree")))),
                ],
            },
        )]))
    }

    #[test]
    fn test_hover_on_self_referential_schema_terminates() {
        let r = tree_resolver();
        // A word that is not in the schema used to recurse through the TypeRef forever.
        assert_eq!(hover_at("(unknown|: 1)", "t::Tree", &*r), None);
        assert_eq!(hover_at("(value: Unkn|own)", "t::Tree", &*r), None);
        let nested = hover_at("(children: [(val|ue: 1)])", "t::Tree", &*r).expect("hover");
        assert!(nested.contains("**value**: `i32`"), "{nested}");
    }

    #[test]
    fn test_hover_uses_field_at_cursor_not_first_match() {
        let r = resolver(&[
            (
                "t::Root",
                TypeKind::Struct {
                    fields: vec![
                        Field::new("name", TypeKind::String).with_doc("Root name"),
                        Field::new("child", type_ref("t::Child")),
                    ],
                },
            ),
            (
                "t::Child",
                TypeKind::Struct {
                    fields: vec![Field::new("name", TypeKind::U8).with_doc("Child name")],
                },
            ),
        ]);
        let info = hover_at("(name: \"a\", child: (na|me: 1))", "t::Root", &r).expect("hover");
        assert!(info.contains("Child name"), "{info}");
        let info = hover_at("(na|me: \"a\", child: (name: 1))", "t::Root", &r).expect("hover");
        assert!(info.contains("Root name"), "{info}");
    }
}
