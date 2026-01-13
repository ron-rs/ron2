//! Document management for the RON Language Server.
//!
//! This module handles parsing RON documents and extracting metadata
//! such as inner attributes for schema resolution.

use std::path::PathBuf;

use ron2::ast::{self, AttributeContent, Document as AstDocument};
use tower_lsp::lsp_types::Url;

/// A parsed RON document.
#[derive(Debug)]
pub struct Document {
    /// The document URI.
    pub uri: Url,
    /// The document content.
    pub content: String,
    /// The document version.
    pub version: i32,
    /// Extracted type attribute (`#![type = "..."]`).
    pub type_attr: Option<String>,
    /// Extracted schema attribute (`#![schema = "..."]`).
    pub schema_attr: Option<String>,
    /// Parsed AST document (if parsing succeeded).
    pub ast: Option<AstDocument<'static>>,
    /// Parsed RON value (if parsing succeeded, converted from AST).
    pub parsed_value: Option<ron2::Value>,
    /// Parse error (if parsing failed).
    pub parse_error: Option<ParseError>,
    /// Line offsets for position calculations.
    line_offsets: Vec<usize>,
}

/// A parse error with location information.
#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub line: usize,
    pub col: usize,
}

impl Document {
    /// Create a new document from content.
    pub fn new(uri: Url, content: String, version: i32) -> Self {
        let mut doc = Self {
            uri,
            content: String::new(),
            version,
            type_attr: None,
            schema_attr: None,
            ast: None,
            parsed_value: None,
            parse_error: None,
            line_offsets: Vec::new(),
        };
        doc.update(content, version);
        doc
    }

    /// Update document content.
    pub fn update(&mut self, content: String, version: i32) {
        self.content = content;
        self.version = version;
        self.compute_line_offsets();
        self.parse_content();
    }

    /// Get the file path for this document.
    pub fn file_path(&self) -> Option<PathBuf> {
        self.uri.to_file_path().ok()
    }

    /// Compute line offsets for position calculations.
    fn compute_line_offsets(&mut self) {
        self.line_offsets.clear();
        self.line_offsets.push(0);

        for (i, c) in self.content.char_indices() {
            if c == '\n' {
                self.line_offsets.push(i + 1);
            }
        }
    }

    /// Convert a line and column (0-based) to a byte offset.
    pub fn position_to_offset(&self, line: u32, col: u32) -> Option<usize> {
        let line = line as usize;
        if line >= self.line_offsets.len() {
            return None;
        }
        let line_start = self.line_offsets[line];
        let col = col as usize;

        // Calculate byte offset accounting for UTF-8
        let line_content = if line + 1 < self.line_offsets.len() {
            &self.content[line_start..self.line_offsets[line + 1]]
        } else {
            &self.content[line_start..]
        };

        for (char_count, (i, _)) in line_content.char_indices().enumerate() {
            if char_count == col {
                return Some(line_start + i);
            }
        }

        // Column is at or past end of line
        Some(line_start + line_content.len())
    }

    /// Parse the RON content using the AST parser.
    fn parse_content(&mut self) {
        self.ast = None;
        self.parsed_value = None;
        self.parse_error = None;
        self.type_attr = None;
        self.schema_attr = None;

        let (doc, errors) = ast::parse_document_lossy(&self.content);

        let mut type_attr = None;
        let mut schema_attr = None;

        for attr in &doc.attributes {
            match attr.name.as_ref() {
                "type" => {
                    if let AttributeContent::Value(v) = &attr.content {
                        type_attr = Some(strip_string_quotes(v));
                    }
                }
                "schema" => {
                    if let AttributeContent::Value(v) = &attr.content {
                        schema_attr = Some(strip_string_quotes(v));
                    }
                }
                _ => {}
            }
        }

        let parsed_value = ast::to_value(&doc).and_then(|r| r.ok());
        let ast_doc = doc.into_owned();

        let parse_error = errors.first().map(|err| ParseError {
            message: format!("{}", err.kind()),
            line: err.span().start.line.saturating_sub(1),
            col: err.span().start.col.saturating_sub(1),
        });

        self.type_attr = type_attr;
        self.schema_attr = schema_attr;
        self.parsed_value = parsed_value;
        self.ast = Some(ast_doc);
        self.parse_error = parse_error;

        if self.parse_error.is_some() {
            // AST parsing failed - fall back to text-based attribute extraction
            // so completions and schema resolution still work
            self.extract_attributes_from_text();
        }
    }

    /// Extract attributes from text (fallback when AST parsing fails).
    fn extract_attributes_from_text(&mut self) {
        for line in self.content.lines() {
            let trimmed = line.trim();

            if let Some(rest) = trimmed.strip_prefix("#![type") {
                if self.type_attr.is_none() {
                    if let Some(value) = extract_attribute_value_from_text(rest) {
                        self.type_attr = Some(value);
                    }
                }
            } else if let Some(rest) = trimmed.strip_prefix("#![schema") {
                if self.schema_attr.is_none() {
                    if let Some(value) = extract_attribute_value_from_text(rest) {
                        self.schema_attr = Some(value);
                    }
                }
            }
        }
    }

    /// Get the word at a position.
    pub fn word_at_position(&self, line: u32, col: u32) -> Option<&str> {
        let offset = self.position_to_offset(line, col)?;
        let bytes = self.content.as_bytes();

        // Find word boundaries
        let mut start = offset;
        while start > 0 && is_identifier_char(bytes[start - 1]) {
            start -= 1;
        }

        let mut end = offset;
        while end < bytes.len() && is_identifier_char(bytes[end]) {
            end += 1;
        }

        if start == end {
            return None;
        }

        Some(&self.content[start..end])
    }

    /// Get the context at a position (for completions).
    pub fn context_at_position(&self, line: u32, col: u32) -> CompletionContext {
        let Some(offset) = self.position_to_offset(line, col) else {
            return CompletionContext::Unknown;
        };

        let before = &self.content[..offset];

        // Count open parens and braces to determine nesting
        let mut paren_depth = 0i32;
        let mut brace_depth = 0i32;
        let mut in_string = false;
        let mut escape_next = false;

        for c in before.chars() {
            if escape_next {
                escape_next = false;
                continue;
            }
            match c {
                '\\' if in_string => escape_next = true,
                '"' => in_string = !in_string,
                '(' if !in_string => paren_depth += 1,
                ')' if !in_string => paren_depth -= 1,
                '{' if !in_string => brace_depth += 1,
                '}' if !in_string => brace_depth -= 1,
                _ => {}
            }
        }

        if in_string {
            return CompletionContext::InString;
        }

        // Check what's immediately before the cursor
        let trimmed = before.trim_end();

        // After a colon, we're expecting a value
        if trimmed.ends_with(':') {
            return CompletionContext::Value;
        }

        if let Some(ast) = self.ast.as_ref() {
            if let Some(expr) = ast.value.as_ref() {
                let span = expr.span();
                if offset >= span.start_offset && offset <= span.end_offset {
                    if find_field_containing_offset(expr, offset).is_some() {
                        return CompletionContext::Value;
                    }
                    match expr {
                        ast::Expr::AnonStruct(_) => return CompletionContext::FieldName,
                        ast::Expr::Struct(s) => {
                            if matches!(s.body, Some(ast::StructBody::Fields(_))) {
                                return CompletionContext::FieldName;
                            }
                        }
                        _ => {}
                    }
                }
            }
        }

        // After an open paren, comma, or at root level in a struct
        if paren_depth > 0 || brace_depth > 0 {
            // Check if we just typed a comma or open paren
            if trimmed.ends_with(',') || trimmed.ends_with('(') || trimmed.ends_with('{') {
                return CompletionContext::FieldName;
            }

            // Check if we're in a field position (not after a colon)
            let last_colon = trimmed.rfind(':');
            let last_comma = trimmed.rfind(',');
            let last_paren = trimmed.rfind('(');

            match (last_colon, last_comma, last_paren) {
                (Some(c), Some(comma), _) if comma > c => CompletionContext::FieldName,
                (Some(c), _, Some(paren)) if paren > c => CompletionContext::FieldName,
                (None, _, _) => CompletionContext::FieldName,
                _ => CompletionContext::Value,
            }
        } else {
            CompletionContext::Root
        }
    }

    /// Get field names from the root AST expression.
    ///
    /// Returns field names for anonymous structs `(field: value, ...)` or
    /// named structs with fields `Name(field: value, ...)`.
    pub fn get_ast_field_names(&self) -> Vec<String> {
        let Some(ref ast) = self.ast else {
            return vec![];
        };
        let Some(ref expr) = ast.value else {
            return vec![];
        };
        extract_field_names_from_expr(expr)
    }

    /// Get field names with their spans from the root AST expression.
    pub fn get_ast_fields_with_spans(&self) -> Vec<(String, ron2::error::Span)> {
        let Some(ref ast) = self.ast else {
            return vec![];
        };
        let Some(ref expr) = ast.value else {
            return vec![];
        };
        extract_fields_with_spans_from_expr(expr)
    }

    /// Find the field name at a given position (for value completions).
    ///
    /// Walks the AST to find which field's value contains the cursor position.
    pub fn find_field_at_position(&self, line: u32, col: u32) -> Option<String> {
        let offset = self.position_to_offset(line, col)?;
        let ast = self.ast.as_ref()?;
        let expr = ast.value.as_ref()?;
        find_field_containing_offset(expr, offset)
    }
}

/// Extract field names from an AST expression.
fn extract_field_names_from_expr(expr: &ast::Expr<'_>) -> Vec<String> {
    match expr {
        ast::Expr::AnonStruct(s) => s.fields.iter().map(|f| f.name.name.to_string()).collect(),
        ast::Expr::Struct(s) => {
            if let Some(ast::StructBody::Fields(fields)) = &s.body {
                fields
                    .fields
                    .iter()
                    .map(|f| f.name.name.to_string())
                    .collect()
            } else {
                vec![]
            }
        }
        _ => vec![],
    }
}

/// Extract field names with their spans from an AST expression.
fn extract_fields_with_spans_from_expr(expr: &ast::Expr<'_>) -> Vec<(String, ron2::error::Span)> {
    match expr {
        ast::Expr::AnonStruct(s) => s
            .fields
            .iter()
            .map(|f| (f.name.name.to_string(), f.name.span.clone()))
            .collect(),
        ast::Expr::Struct(s) => {
            if let Some(ast::StructBody::Fields(fields)) = &s.body {
                fields
                    .fields
                    .iter()
                    .map(|f| (f.name.name.to_string(), f.name.span.clone()))
                    .collect()
            } else {
                vec![]
            }
        }
        _ => vec![],
    }
}

/// Check if a span is synthetic (zero-width, created during error recovery).
fn is_synthetic_span(span: &ron2::error::Span) -> bool {
    span.start_offset == span.end_offset
}

/// Find the field name whose value contains the given offset.
fn find_field_containing_offset(expr: &ast::Expr<'_>, offset: usize) -> Option<String> {
    let fields: Vec<&ast::StructField<'_>> = match expr {
        ast::Expr::AnonStruct(s) => s.fields.iter().collect(),
        ast::Expr::Struct(s) => {
            if let Some(ast::StructBody::Fields(f)) = &s.body {
                f.fields.iter().collect()
            } else {
                return None;
            }
        }
        _ => return None,
    };

    // Find the field whose value span contains the offset
    for field in &fields {
        if is_synthetic_span(&field.colon) {
            continue;
        }
        let value_span = field.value.span();
        if offset >= value_span.start_offset && offset <= value_span.end_offset {
            return Some(field.name.name.to_string());
        }
    }

    // Also check if we're after the colon but before any value (typing a new value)
    for field in &fields {
        if is_synthetic_span(&field.colon) {
            continue;
        }
        // If offset is after the colon and before or at the value start
        if offset > field.colon.end_offset {
            let value_span = field.value.span();
            // Check if we're between colon and value, or the cursor is at value start
            if offset <= value_span.start_offset {
                return Some(field.name.name.to_string());
            }
        }
    }

    None
}

/// Context for completion suggestions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompletionContext {
    /// At the root level of the document.
    Root,
    /// Expecting a field name.
    FieldName,
    /// Expecting a value.
    Value,
    /// Inside a string literal.
    InString,
    /// Unknown context.
    Unknown,
}

/// Strip surrounding quotes from a string literal (e.g., `"value"` -> `value`).
fn strip_string_quotes(s: &str) -> String {
    let s = s.trim();
    if s.starts_with('"') && s.ends_with('"') && s.len() >= 2 {
        s[1..s.len() - 1].to_string()
    } else {
        s.to_string()
    }
}

/// Extract the value from an attribute like `= "value"]` (text-based fallback).
fn extract_attribute_value_from_text(rest: &str) -> Option<String> {
    let rest = rest.trim();
    let rest = rest.strip_prefix('=')?;
    let rest = rest.trim();
    let rest = rest.strip_prefix('"')?;
    let end = rest.find('"')?;
    Some(rest[..end].to_string())
}

/// Check if a byte is a valid identifier character.
fn is_identifier_char(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_'
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_type_attribute() {
        let content = r#"#![type = "my_crate::config::AppConfig"]

(
    port: 8080,
)
"#;
        let doc = Document::new(
            Url::parse("file:///test.ron").unwrap(),
            content.to_string(),
            1,
        );
        assert_eq!(
            doc.type_attr,
            Some("my_crate::config::AppConfig".to_string())
        );
    }

    #[test]
    fn test_extract_schema_attribute() {
        let content = r#"#![schema = "./schemas/app.schema.ron"]

(
    port: 8080,
)
"#;
        let doc = Document::new(
            Url::parse("file:///test.ron").unwrap(),
            content.to_string(),
            1,
        );
        assert_eq!(
            doc.schema_attr,
            Some("./schemas/app.schema.ron".to_string())
        );
    }

    #[test]
    fn test_position_to_offset() {
        let content = "line1\nline2\nline3";
        let doc = Document::new(
            Url::parse("file:///test.ron").unwrap(),
            content.to_string(),
            1,
        );

        assert_eq!(doc.position_to_offset(0, 0), Some(0));
        assert_eq!(doc.position_to_offset(0, 5), Some(5));
        assert_eq!(doc.position_to_offset(1, 0), Some(6));
        assert_eq!(doc.position_to_offset(2, 0), Some(12));
    }

    #[test]
    fn test_completion_context() {
        let content = r#"(
    port: 8080,

)"#;
        let doc = Document::new(
            Url::parse("file:///test.ron").unwrap(),
            content.to_string(),
            1,
        );

        // After comma, should be field name context
        assert_eq!(doc.context_at_position(2, 4), CompletionContext::FieldName);
    }
}
