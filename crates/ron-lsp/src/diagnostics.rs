//! Diagnostics provider for the RON Language Server.
//!
//! Validates RON files against their schemas and produces
//! diagnostic messages for errors.

use ron_schema::{validate_with_resolver, SchemaError};
use tower_lsp::lsp_types::*;

use crate::document::Document;
use crate::lsp_utils::{find_field_position, find_text_position};
use crate::schema_resolver::SchemaResolver;

/// Validate a document and return diagnostics.
pub fn validate_document(doc: &Document, resolver: &SchemaResolver) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    // Add parse error diagnostic if present
    if let Some(ref error) = doc.parse_error {
        diagnostics.push(Diagnostic {
            range: Range {
                start: Position {
                    line: error.line as u32,
                    character: error.col as u32,
                },
                end: Position {
                    line: error.line as u32,
                    character: (error.col + 1) as u32,
                },
            },
            severity: Some(DiagnosticSeverity::ERROR),
            source: Some("ron".to_string()),
            message: error.message.clone(),
            ..Default::default()
        });
        // Don't continue validation if we can't parse the file
        return diagnostics;
    }

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

    // Validate the parsed value against the schema
    let Some(ref value) = doc.parsed_value else {
        return diagnostics;
    };

    if let Err(error) = validate_with_resolver(value, &schema, resolver) {
        diagnostics.extend(validation_error_to_diagnostics(&error, doc));
    }

    diagnostics
}

/// Convert a validation error to diagnostics.
fn validation_error_to_diagnostics(error: &SchemaError, doc: &Document) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    // For now, we report all errors at position 0,0 since we don't have
    // position information from the RON parser for values.
    // A more sophisticated implementation would track value positions.
    let range = Range {
        start: Position {
            line: 0,
            character: 0,
        },
        end: Position {
            line: 0,
            character: 0,
        },
    };

    // Try to find a better position based on the error type
    let (message, improved_range) = format_validation_error(error, doc);

    diagnostics.push(Diagnostic {
        range: improved_range.unwrap_or(range),
        severity: Some(DiagnosticSeverity::ERROR),
        source: Some("ron-schema".to_string()),
        message,
        ..Default::default()
    });

    diagnostics
}

/// Format a validation error and try to find its position.
fn format_validation_error(error: &SchemaError, doc: &Document) -> (String, Option<Range>) {
    match error {
        SchemaError::TypeMismatch { expected, actual } => (
            format!("Type mismatch: expected {}, got {}", expected, actual),
            None,
        ),
        SchemaError::MissingField(field) => {
            // Try to find a good position for missing field error
            let range = find_field_insert_position(doc, field);
            (format!("Missing required field: {}", field), range)
        }
        SchemaError::UnknownField(field) => {
            let range = find_field_position(doc, field);
            (format!("Unknown field: {}", field), range)
        }
        SchemaError::UnknownVariant(variant) => {
            let range = find_text_position(doc, variant);
            (format!("Unknown enum variant: {}", variant), range)
        }
        SchemaError::TupleLengthMismatch { expected, actual } => (
            format!(
                "Tuple length mismatch: expected {} elements, got {}",
                expected, actual
            ),
            None,
        ),
        SchemaError::FieldError { field, source } => {
            let range = find_field_position(doc, field);
            let (inner_msg, _) = format_validation_error(source, doc);
            (format!("Error in field '{}': {}", field, inner_msg), range)
        }
        SchemaError::ElementError { index, source } => {
            let (inner_msg, _) = format_validation_error(source, doc);
            (format!("Error in element {}: {}", index, inner_msg), None)
        }
        SchemaError::MapKeyError { source } => {
            let (inner_msg, _) = format_validation_error(source, doc);
            (format!("Error in map key: {}", inner_msg), None)
        }
        SchemaError::MapValueError { key, source } => {
            let (inner_msg, _) = format_validation_error(source, doc);
            (
                format!("Error in map value for '{}': {}", key, inner_msg),
                None,
            )
        }
        SchemaError::VariantError { variant, source } => {
            let range = find_text_position(doc, variant);
            let (inner_msg, _) = format_validation_error(source, doc);
            (
                format!("Error in variant '{}': {}", variant, inner_msg),
                range,
            )
        }
        SchemaError::TypeRefError { type_path, source } => {
            let (inner_msg, inner_range) = format_validation_error(source, doc);
            (
                format!("Error in type '{}': {}", type_path, inner_msg),
                inner_range,
            )
        }
        // Storage errors shouldn't occur during validation, but handle gracefully
        _ => (error.to_string(), None),
    }
}

/// Find a position where a missing field could be inserted.
fn find_field_insert_position(doc: &Document, _field: &str) -> Option<Range> {
    // If we have an AST, use the root expression span
    if let Some(ref ast) = doc.ast {
        if let Some(ref expr) = ast.value {
            let span = expr.span();
            // Return the opening delimiter position
            return Some(Range {
                start: Position {
                    line: span.start.line.saturating_sub(1) as u32,
                    character: span.start.col.saturating_sub(1) as u32,
                },
                end: Position {
                    line: span.start.line.saturating_sub(1) as u32,
                    character: span.start.col as u32,
                },
            });
        }
    }

    // Fall back to text search
    for (line_idx, line) in doc.content.lines().enumerate() {
        if line.trim().starts_with("#![") {
            continue;
        }
        if let Some(col) = line.find('(') {
            return Some(Range {
                start: Position {
                    line: line_idx as u32,
                    character: col as u32,
                },
                end: Position {
                    line: line_idx as u32,
                    character: (col + 1) as u32,
                },
            });
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use tower_lsp::lsp_types::Url;

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
