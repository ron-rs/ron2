//! Document management for the RON Language Server.
//!
//! This module handles parsing RON documents and extracting metadata
//! such as inner attributes for schema resolution.

use std::path::PathBuf;

use ron2::ast::{self, AttributeContent, Document as AstDocument};
use tower_lsp::lsp_types::{Position, Range, Url};

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
    /// Parse errors (if any parsing failed).
    pub parse_errors: Vec<ParseError>,
    /// Line offsets for position calculations.
    line_offsets: Vec<usize>,
}

/// A parse error with location information.
#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub line: usize,
    pub col: usize,
    pub end_line: usize,
    pub end_col: usize,
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
            parse_errors: Vec::new(),
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

    fn line_bounds(&self, line: usize) -> Option<(usize, usize, usize)> {
        let start = *self.line_offsets.get(line)?;
        let end_with_break = if line + 1 < self.line_offsets.len() {
            self.line_offsets[line + 1]
        } else {
            self.content.len()
        };
        let mut end = end_with_break;
        if end > start {
            let bytes = self.content.as_bytes();
            if bytes[end - 1] == b'\n' {
                end -= 1;
                if end > start && bytes[end - 1] == b'\r' {
                    end -= 1;
                }
            }
        }
        Some((start, end, end_with_break))
    }

    /// Convert a line and column (0-based) to a byte offset.
    pub fn position_to_offset(&self, line: u32, col: u32) -> Option<usize> {
        let line = line as usize;
        let col = col as usize;
        let (line_start, line_end, _) = self.line_bounds(line)?;

        let line_content = &self.content[line_start..line_end];
        let mut utf16_count = 0usize;

        for (byte_idx, ch) in line_content.char_indices() {
            if utf16_count >= col {
                return Some(line_start + byte_idx);
            }
            let units = if (ch as u32) >= 0x10000 { 2 } else { 1 };
            if utf16_count + units > col {
                return Some(line_start + byte_idx);
            }
            utf16_count += units;
        }

        // Column is at or past end of line
        Some(line_end)
    }

    /// Convert a byte offset to an LSP position (0-based, UTF-16 code units).
    pub fn offset_to_position(&self, offset: usize) -> Position {
        let offset = offset.min(self.content.len());
        let line = self
            .line_offsets
            .partition_point(|&start| start <= offset)
            .saturating_sub(1);
        let (line_start, line_end, _) = self.line_bounds(line).unwrap_or((0, 0, 0));
        let offset = offset.min(line_end);
        let line_content = &self.content[line_start..offset];
        let mut utf16_count = 0usize;
        for ch in line_content.chars() {
            utf16_count += if (ch as u32) >= 0x10000 { 2 } else { 1 };
        }

        Position {
            line: line as u32,
            character: utf16_count as u32,
        }
    }

    /// Convert a ron2 span to an LSP range.
    pub fn span_to_range(&self, span: &ron2::error::Span) -> Range {
        Range {
            start: self.offset_to_position(span.start_offset),
            end: self.offset_to_position(span.end_offset),
        }
    }

    /// Parse the RON content using the AST parser.
    fn parse_content(&mut self) {
        self.ast = None;
        self.parsed_value = None;
        self.parse_errors = Vec::new();
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

        // Collect ALL parse errors
        let parse_errors: Vec<ParseError> = errors
            .iter()
            .map(|err| {
                let start = self.offset_to_position(err.span().start_offset);
                let end = self.offset_to_position(err.span().end_offset);
                ParseError {
                    message: format!("{}", err.kind()),
                    line: start.line as usize,
                    col: start.character as usize,
                    end_line: end.line as usize,
                    end_col: end.character as usize,
                }
            })
            .collect();

        self.type_attr = type_attr;
        self.schema_attr = schema_attr;
        self.parsed_value = parsed_value;
        self.ast = Some(ast_doc);
        self.parse_errors = parse_errors;

        if !self.parse_errors.is_empty() {
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
                if self.type_attr.is_none()
                    && let Some(value) = extract_attribute_value_from_text(rest)
                {
                    self.type_attr = Some(value);
                }
            } else if let Some(rest) = trimmed.strip_prefix("#![schema")
                && self.schema_attr.is_none()
                && let Some(value) = extract_attribute_value_from_text(rest)
            {
                self.schema_attr = Some(value);
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

        if let Some(ast) = self.ast.as_ref()
            && let Some(expr) = ast.value.as_ref()
        {
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
            .map(|f| (f.name.name.to_string(), f.name.span))
            .collect(),
        ast::Expr::Struct(s) => {
            if let Some(ast::StructBody::Fields(fields)) = &s.body {
                fields
                    .fields
                    .iter()
                    .map(|f| (f.name.name.to_string(), f.name.span))
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
///
/// Handles both regular strings and raw strings (`r"..."`, `r#"..."#`).
/// Processes escape sequences in regular strings.
fn strip_string_quotes(s: &str) -> String {
    let s = s.trim();
    // Use ron2's decode_string which handles raw strings and escape sequences
    match ron2::ast::decode_string(s) {
        Ok((value, _kind)) => value,
        Err(_) => {
            // Fallback for malformed strings - just strip simple quotes
            if s.starts_with('"') && s.ends_with('"') && s.len() >= 2 {
                s[1..s.len() - 1].to_string()
            } else {
                s.to_string()
            }
        }
    }
}

/// Extract the value from an attribute like `= "value"]` (text-based fallback).
///
/// Handles both regular strings and raw strings (`r"..."`, `r#"..."#`).
fn extract_attribute_value_from_text(rest: &str) -> Option<String> {
    let rest = rest.trim();
    let rest = rest.strip_prefix('=')?;
    let rest = rest.trim();

    // Handle raw strings: r"...", r#"..."#, etc.
    if let Some(raw_rest) = rest.strip_prefix('r') {
        let hash_count = raw_rest.chars().take_while(|&c| c == '#').count();
        let after_hashes = &raw_rest[hash_count..];
        if !after_hashes.starts_with('"') {
            return None;
        }
        let content_start = hash_count + 1; // hashes + opening quote
        let remaining = &raw_rest[content_start..];
        let end_delim = format!("\"{}", "#".repeat(hash_count));
        let end_pos = remaining.find(&end_delim)?;
        return Some(remaining[..end_pos].to_string());
    }

    // Handle regular strings with escape processing
    if !rest.starts_with('"') {
        return None;
    }
    // Find the closing quote (not preceded by unescaped backslash)
    let content = &rest[1..]; // Skip opening quote
    let mut end = 0;
    let mut chars = content.chars().peekable();
    while let Some(c) = chars.next() {
        if c == '"' {
            // Found closing quote - extract and decode
            let raw = &rest[..end + 2]; // Include both quotes
            return ron2::ast::decode_string(raw).ok().map(|(v, _)| v);
        }
        if c == '\\' {
            // Skip escaped character
            if let Some(escaped) = chars.next() {
                end += escaped.len_utf8();
            }
        }
        end += c.len_utf8();
    }
    None
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

    #[test]
    fn test_extract_raw_string_type_attribute() {
        let content = r###"#![type = r#"my_crate::config::AppConfig"#]

(
    port: 8080,
)
"###;
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
    fn test_extract_escaped_string_type_attribute() {
        let content = r#"#![type = "my_crate::config::App\"Config"]

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
            Some("my_crate::config::App\"Config".to_string())
        );
    }

    #[test]
    fn test_utf16_position_conversion_with_emoji() {
        // Emoji like 😀 takes 4 bytes in UTF-8 but 2 UTF-16 code units
        let content = "a😀b";
        let doc = Document::new(
            Url::parse("file:///test.ron").unwrap(),
            content.to_string(),
            1,
        );

        // Position of 'b' in UTF-16: a (1) + 😀 (2) = 3
        let pos = doc.offset_to_position(5); // byte offset of 'b' (a=1, 😀=4)
        assert_eq!(pos.character, 3); // UTF-16 column

        // Converting back: UTF-16 position 3 should give byte offset 5
        let offset = doc.position_to_offset(0, 3);
        assert_eq!(offset, Some(5));
    }

    #[test]
    fn test_span_to_range_with_emoji() {
        // Test that span_to_range correctly converts byte offsets to UTF-16
        let content = "(emoji: \"😀\", next: 42)";
        let doc = Document::new(
            Url::parse("file:///test.ron").unwrap(),
            content.to_string(),
            1,
        );

        // Get AST fields with spans
        let fields = doc.get_ast_fields_with_spans();
        assert_eq!(fields.len(), 2);

        // Find the "next" field
        let (name, span) = fields.iter().find(|(n, _)| n == "next").unwrap();
        assert_eq!(name, "next");

        // Convert span to LSP range
        let range = doc.span_to_range(span);

        // The "next" field starts after: (emoji: "😀", - that's byte offset 14
        // In UTF-16: ( (1) + emoji (5) + : (1) + space (1) + " (1) + 😀 (2) + " (1) + , (1) + space (1) = 14
        // But wait, emoji is 5 chars = 5 code units, 😀 is 2 code units
        // Let me recalculate: ( + e + m + o + j + i + : + " " + " + 😀 + " + , + " " + n
        // Position of 'n' in next: 1 + 5 + 1 + 1 + 1 + 2 + 1 + 1 + 1 = 14
        assert_eq!(range.start.line, 0);
        assert_eq!(range.start.character, 14); // UTF-16 position
    }
}
