//! Diagnostics provider for the RON Language Server.
//!
//! Validates RON files against their schemas and produces
//! diagnostic messages for errors.

use ron2::schema::{Severity, ValidationErrorKind, ValidationIssue, validate_all};
use tower_lsp::lsp_types::*;

use crate::{
    document::Document,
    schema_resolver::{SchemaLookupError, SchemaResolver},
};

/// Validate a document and return diagnostics.
pub fn validate_document(doc: &Document, resolver: &SchemaResolver) -> Vec<Diagnostic> {
    let mut diagnostics: Vec<Diagnostic> = doc
        .parse_errors
        .iter()
        .map(|error| Diagnostic {
            range: Range {
                start: Position {
                    line: error.line as u32,
                    character: error.col as u32,
                },
                end: Position {
                    line: error.end_line as u32,
                    character: error.end_col as u32,
                },
            },
            severity: Some(DiagnosticSeverity::ERROR),
            source: Some("ron".to_string()),
            message: error.message.clone(),
            ..Default::default()
        })
        .collect();

    // Schema validation continues after parse errors: the parser recovers and
    // produces a partial AST.
    let attr = match (&doc.type_attr, &doc.schema_attr) {
        (_, Some(path)) => ("schema", path),
        (Some(type_path), None) => ("type", type_path),
        (None, None) => return diagnostics,
    };

    let schema = match resolver.lookup_schema(doc) {
        Ok(schema) => schema,
        Err(error) => {
            let range = doc
                .ast
                .attributes
                .iter()
                .find(|a| a.name == attr.0)
                .map(|a| doc.span_to_range(&a.span))
                .unwrap_or_default();
            let (severity, message) = match error {
                SchemaLookupError::NotFound => (
                    DiagnosticSeverity::WARNING,
                    format!("Could not find schema for {} = \"{}\"", attr.0, attr.1),
                ),
                invalid @ SchemaLookupError::Invalid { .. } => {
                    (DiagnosticSeverity::ERROR, invalid.to_string())
                }
            };
            diagnostics.push(Diagnostic {
                range,
                severity: Some(severity),
                source: Some("ron-schema".to_string()),
                message,
                ..Default::default()
            });
            return diagnostics;
        }
    };

    let Some(expr) = doc.ast.value.as_ref() else {
        return diagnostics;
    };

    diagnostics.extend(
        validate_all(expr, &schema, resolver)
            .iter()
            .map(|issue| issue_to_diagnostic(doc, issue)),
    );
    diagnostics
}

/// Convert a validation issue to a diagnostic at the issue's span.
fn issue_to_diagnostic(doc: &Document, issue: &ValidationIssue) -> Diagnostic {
    Diagnostic {
        range: doc.span_to_range(issue.error.span()),
        severity: Some(match issue.severity {
            Severity::Error => DiagnosticSeverity::ERROR,
            Severity::Warning => DiagnosticSeverity::WARNING,
        }),
        source: Some("ron-schema".to_string()),
        message: format_error_message(issue.error.kind()),
        ..Default::default()
    }
}

/// Format a validation error message.
///
/// The diagnostic's range already points at the offending expression, so the
/// message omits the path context.
fn format_error_message(kind: &ValidationErrorKind) -> String {
    match kind {
        ValidationErrorKind::TypeMismatch { expected, found } => {
            format!("expected {}, found {}", expected, found)
        }
        ValidationErrorKind::MissingField { field, .. } => {
            format!("missing required field `{}`", field)
        }
        ValidationErrorKind::UnknownField { field, .. } => {
            format!("unknown field `{}`", field)
        }
        ValidationErrorKind::UnknownVariant { variant, .. } => {
            format!("unknown variant `{}`", variant)
        }
        ValidationErrorKind::LengthMismatch {
            expected, found, ..
        } => {
            format!("expected {}, found {}", expected, found)
        }
        ValidationErrorKind::DuplicateField { field, .. } => {
            format!("duplicate field `{}`", field)
        }
        ValidationErrorKind::IntegerOutOfBounds { value, target_type } => {
            format!("{} out of range for {}", value, target_type)
        }
        ValidationErrorKind::DuplicateKey { key } => format!("duplicate key `{}`", key),
        ValidationErrorKind::UnresolvedTypeRef(type_path) => {
            format!("no schema found for `{}`; value not checked", type_path)
        }
        other => other.to_string(),
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use ron2::schema::{Field, Schema, SchemaResolver as _, TypeKind};
    use tower_lsp::lsp_types::Url;

    use super::*;

    #[test]
    fn test_parse_error_diagnostic() {
        let content = r#"(
    port: 8080,
    invalid syntax here
)"#;
        let doc = Document::new(
            Url::parse("file:///test.ron").unwrap(),
            content.to_string(),
            1,
        );
        let resolver = SchemaResolver::new();

        let diagnostics = validate_document(&doc, &resolver);
        assert!(!diagnostics.is_empty());
        assert_eq!(diagnostics[0].severity, Some(DiagnosticSeverity::ERROR));
        // The first error points at `invalid`, which has no `:` after it.
        assert_eq!(
            diagnostics[0].message,
            "Expected `:` after field name in struct field"
        );
        assert_eq!(
            diagnostics[0].range,
            Range::new(Position::new(2, 4), Position::new(2, 11))
        );
    }

    #[test]
    fn test_no_schema_no_diagnostics() {
        let content = r#"(
    port: 8080,
)"#;
        let doc = Document::new(
            Url::parse("file:///test.ron").unwrap(),
            content.to_string(),
            1,
        );
        let resolver = SchemaResolver::new();

        let diagnostics = validate_document(&doc, &resolver);
        // No diagnostics because there's no schema to validate against
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn missing_schema_warning_is_at_the_attribute() {
        let doc = Document::new(
            Url::parse("file:///test.ron").unwrap(),
            "// config\n#![type = \"no_such::Type\"]\n(port: 1)".to_string(),
            1,
        );
        let diagnostics = validate_document(&doc, &SchemaResolver::new());
        assert_eq!(diagnostics.len(), 1);
        assert_eq!(diagnostics[0].severity, Some(DiagnosticSeverity::WARNING));
        assert_eq!(diagnostics[0].range.start.line, 1);
        assert_eq!(diagnostics[0].range.end.line, 1);
    }

    #[test]
    fn malformed_schema_file_is_an_error_not_a_missing_schema() {
        let dir = std::env::temp_dir().join(format!("ron2-lsp-diag-{}", std::process::id()));
        std::fs::create_dir_all(dir.join("app")).unwrap();
        std::fs::write(dir.join("app/Half.schema.ron"), "(kind: Str").unwrap();
        let resolver = SchemaResolver::new();
        resolver.set_schema_dirs(vec![dir.clone()]);

        let doc = Document::new(
            Url::parse("file:///test.ron").unwrap(),
            "#![type = \"app::Half\"]\n(port: 1)".to_string(),
            1,
        );
        let diagnostics = validate_document(&doc, &resolver);
        let _ = std::fs::remove_dir_all(&dir);

        assert_eq!(diagnostics.len(), 1);
        assert_eq!(diagnostics[0].severity, Some(DiagnosticSeverity::ERROR));
        assert!(
            diagnostics[0].message.starts_with("invalid schema file"),
            "{}",
            diagnostics[0].message
        );
        assert_eq!(diagnostics[0].range.start.line, 0);
    }

    #[test]
    fn warnings_map_to_warning_severity() {
        let doc = Document::new(
            Url::parse("file:///test.ron").unwrap(),
            "(port: 1, prot: 2, inner: 3)".to_string(),
            1,
        );
        let schema = Schema::new(TypeKind::Struct {
            fields: vec![
                Field::new("port", TypeKind::U16),
                Field::new("inner", TypeKind::TypeRef("missing::Type".into())),
            ],
        });
        let resolver = SchemaResolver::new();
        assert!(resolver.resolve("missing::Type").is_none());

        let expr = doc.ast.value.as_ref().unwrap();
        let diagnostics: Vec<_> = validate_all(expr, &Arc::new(schema), &resolver)
            .iter()
            .map(|issue| issue_to_diagnostic(&doc, issue))
            .collect();

        let summary: Vec<_> = diagnostics
            .iter()
            .map(|d| (d.message.as_str(), d.severity))
            .collect();
        assert_eq!(
            summary,
            [
                ("unknown field `prot`", Some(DiagnosticSeverity::WARNING)),
                (
                    "no schema found for `missing::Type`; value not checked",
                    Some(DiagnosticSeverity::WARNING)
                ),
            ]
        );
    }
}
