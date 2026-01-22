//! Diagnostics provider for the RON Language Server.
//!
//! Validates RON files against their schemas and produces
//! diagnostic messages for errors.

use ron2::schema::{ValidationError, ValidationErrorKind, validate_expr_collect_all};
use tower_lsp::lsp_types::*;

use crate::{document::Document, schema_resolver::SchemaResolver};

/// Validate a document and return diagnostics.
pub fn validate_document(doc: &Document, resolver: &SchemaResolver) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    // Add ALL parse error diagnostics
    for error in &doc.parse_errors {
        diagnostics.push(Diagnostic {
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
        });
    }

    // Continue with schema validation even if there are parse errors
    // (the parser recovers and produces partial AST)

    // If no type or schema attribute, we can't validate
    if doc.type_attr.is_none() && doc.schema_attr.is_none() {
        // This is not an error, just no schema to validate against
        return diagnostics;
    }

    // Try to load schema
    let Some(schema) = resolver.resolve_schema(doc) else {
        // Schema not found - add a warning
        let attr_name = if doc.type_attr.is_some() {
            "type"
        } else {
            "schema"
        };
        let attr_value = doc.type_attr.as_ref().or(doc.schema_attr.as_ref()).unwrap();

        diagnostics.push(Diagnostic {
            range: Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: Position {
                    line: 0,
                    character: 0,
                },
            },
            severity: Some(DiagnosticSeverity::WARNING),
            source: Some("ron-schema".to_string()),
            message: format!(
                "Could not find schema for {} = \"{}\"",
                attr_name, attr_value
            ),
            ..Default::default()
        });
        return diagnostics;
    };

    // Validate the AST against the schema (provides precise error spans)
    let Some(ref ast) = doc.ast else {
        return diagnostics;
    };
    let Some(ref expr) = ast.value else {
        return diagnostics;
    };

    // Use multi-error validation that collects all errors
    let validation_errors = validate_expr_collect_all(expr, &schema, resolver);
    for error in validation_errors {
        diagnostics.push(validation_error_to_diagnostic(doc, &error));
    }

    diagnostics
}

/// Convert a validation error to a diagnostic.
///
/// The error span comes directly from the AST, providing precise positioning.
fn validation_error_to_diagnostic(doc: &Document, error: &ValidationError) -> Diagnostic {
    let range = doc.span_to_range(error.span());

    // Format the error message
    let message = format_error_message(error);

    Diagnostic {
        range,
        severity: Some(DiagnosticSeverity::ERROR),
        source: Some("ron-schema".to_string()),
        message,
        ..Default::default()
    }
}

/// Format a validation error message.
///
/// Since AST validation provides precise spans pointing directly to the error location,
/// path context is not needed - just return concise error messages.
fn format_error_message(error: &ValidationError) -> String {
    match error.kind() {
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
            format!("expected {} elements, found {}", expected, found)
        }
        ValidationErrorKind::DuplicateField { field, .. } => {
            format!("duplicate field `{}`", field)
        }
        ValidationErrorKind::IntegerOutOfBounds { value, target_type } => {
            format!("{} out of range for {}", value, target_type)
        }
        _ => format!("{}", error.kind()),
    }
}

#[cfg(test)]
mod tests {
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
}
