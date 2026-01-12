//! Diagnostics provider for the RON Language Server.
//!
//! Validates RON files against their schemas and produces
//! diagnostic messages for errors.

use ron2::schema::{
    validate_with_resolver, PathSegment, SchemaError, ValidationError, ValidationErrorKind,
};
use tower_lsp::lsp_types::*;

use crate::{
    document::Document,
    lsp_utils::{find_field_position, find_text_position},
    schema_resolver::SchemaResolver,
};

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

    // Default range at position 0,0
    let default_range = Range {
        start: Position {
            line: 0,
            character: 0,
        },
        end: Position {
            line: 0,
            character: 0,
        },
    };

    // Try to find a better position based on the error path and kind
    let (message, improved_range) = format_validation_error(error, doc);

    diagnostics.push(Diagnostic {
        range: improved_range.unwrap_or(default_range),
        severity: Some(DiagnosticSeverity::ERROR),
        source: Some("ron-schema".to_string()),
        message,
        ..Default::default()
    });

    diagnostics
}

/// Format a validation error and try to find its position.
fn format_validation_error(error: &SchemaError, doc: &Document) -> (String, Option<Range>) {
    // Handle storage errors (shouldn't occur during validation, but handle gracefully)
    let validation_error = match error {
        SchemaError::Validation(v) => v,
        SchemaError::Storage(_) => return (error.to_string(), None),
    };

    format_validation_error_inner(validation_error, doc)
}

/// Format a ValidationError and try to find its position.
fn format_validation_error_inner(
    error: &ValidationError,
    doc: &Document,
) -> (String, Option<Range>) {
    // Build context prefix from path (path is stored innermost-first, display outermost-first)
    let context_prefix = if error.path.is_empty() {
        String::new()
    } else {
        let path_str: Vec<String> = error.path.iter().rev().map(format_path_segment).collect();
        format!("{}: ", path_str.join(" -> "))
    };

    // Try to find position based on innermost path segment
    let range = find_position_from_path(&error.path, doc);

    // Format the message based on error kind
    let message = match &error.kind {
        ValidationErrorKind::TypeMismatch { expected, found } => {
            format!(
                "{}Type mismatch: expected {}, got {}",
                context_prefix, expected, found
            )
        }
        ValidationErrorKind::MissingField { field, .. } => {
            format!("{}Missing required field: {}", context_prefix, field)
        }
        ValidationErrorKind::UnknownField { field, .. } => {
            format!("{}Unknown field: {}", context_prefix, field)
        }
        ValidationErrorKind::UnknownVariant { variant, .. } => {
            format!("{}Unknown enum variant: {}", context_prefix, variant)
        }
        ValidationErrorKind::LengthMismatch {
            expected, found, ..
        } => {
            format!(
                "{}Tuple length mismatch: expected {} elements, got {}",
                context_prefix, expected, found
            )
        }
        ValidationErrorKind::DuplicateField { field, .. } => {
            format!("{}Duplicate field: {}", context_prefix, field)
        }
        ValidationErrorKind::IntegerOutOfBounds { value, target_type } => {
            format!(
                "{}Integer {} out of bounds for {}",
                context_prefix, value, target_type
            )
        }
    };

    // Try to find better position for specific error kinds if path didn't help
    let final_range = range.or_else(|| find_position_from_kind(&error.kind, doc));

    (message, final_range)
}

/// Format a path segment for display.
fn format_path_segment(segment: &PathSegment) -> String {
    match segment {
        PathSegment::Field(name) => format!("field '{}'", name),
        PathSegment::Element(idx) => format!("element {}", idx),
        PathSegment::MapKey => "map key".to_string(),
        PathSegment::MapValue(key) => format!("value for '{}'", key),
        PathSegment::Variant(name) => format!("variant '{}'", name),
        PathSegment::TypeRef(path) => format!("type '{}'", path),
    }
}

/// Try to find a position based on the error path.
fn find_position_from_path(path: &[PathSegment], doc: &Document) -> Option<Range> {
    // Try to find position from innermost segment first
    for segment in path.iter().rev() {
        match segment {
            PathSegment::Field(name) => {
                if let Some(range) = find_field_position(doc, name) {
                    return Some(range);
                }
            }
            PathSegment::Variant(name) => {
                if let Some(range) = find_text_position(doc, name) {
                    return Some(range);
                }
            }
            _ => continue,
        }
    }
    None
}

/// Try to find a position based on the error kind.
fn find_position_from_kind(kind: &ValidationErrorKind, doc: &Document) -> Option<Range> {
    match kind {
        ValidationErrorKind::MissingField { .. } => find_field_insert_position(doc),
        ValidationErrorKind::UnknownField { field, .. } => find_field_position(doc, field),
        ValidationErrorKind::UnknownVariant { variant, .. } => find_text_position(doc, variant),
        _ => None,
    }
}

/// Find a position where a missing field could be inserted.
fn find_field_insert_position(doc: &Document) -> Option<Range> {
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

    #[test]
    fn test_format_path_segment() {
        assert_eq!(
            format_path_segment(&PathSegment::Field("name".to_string())),
            "field 'name'"
        );
        assert_eq!(format_path_segment(&PathSegment::Element(0)), "element 0");
        assert_eq!(format_path_segment(&PathSegment::MapKey), "map key");
        assert_eq!(
            format_path_segment(&PathSegment::Variant("Some".to_string())),
            "variant 'Some'"
        );
    }
}
