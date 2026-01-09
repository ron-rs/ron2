//! Document management for the RON Language Server.
//!
//! This module handles parsing RON documents and extracting metadata
//! such as inner attributes for schema resolution.

use std::path::PathBuf;

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
    /// Parsed RON value (if parsing succeeded).
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
        self.extract_attributes();
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

    /// Convert a byte offset to line and column (0-based).
    #[allow(dead_code)]
    pub fn offset_to_position(&self, offset: usize) -> (u32, u32) {
        let line = self
            .line_offsets
            .iter()
            .rposition(|&o| o <= offset)
            .unwrap_or(0);

        let line_start = self.line_offsets[line];
        let col = self.content[line_start..offset].chars().count();

        (line as u32, col as u32)
    }

    /// Extract inner attributes from the document.
    fn extract_attributes(&mut self) {
        self.type_attr = None;
        self.schema_attr = None;

        // Look for #![type = "..."] and #![schema = "..."]
        for line in self.content.lines() {
            let trimmed = line.trim();

            if let Some(rest) = trimmed.strip_prefix("#![type") {
                if let Some(value) = extract_attribute_value(rest) {
                    self.type_attr = Some(value);
                }
            } else if let Some(rest) = trimmed.strip_prefix("#![schema") {
                if let Some(value) = extract_attribute_value(rest) {
                    self.schema_attr = Some(value);
                }
            }
        }
    }

    /// Parse the RON content.
    fn parse_content(&mut self) {
        self.parsed_value = None;
        self.parse_error = None;

        // Strip attributes before parsing
        let content_without_attrs = strip_inner_attributes(&self.content);

        match ron2::from_str(&content_without_attrs) {
            Ok(value) => {
                self.parsed_value = Some(value);
            }
            Err(err) => {
                let position = err.span.start;
                self.parse_error = Some(ParseError {
                    message: format!("{}", err.code),
                    line: position.line.saturating_sub(1),
                    col: position.col.saturating_sub(1),
                });
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

/// Extract the value from an attribute like `= "value"]`.
fn extract_attribute_value(rest: &str) -> Option<String> {
    let rest = rest.trim();
    let rest = rest.strip_prefix('=')?;
    let rest = rest.trim();
    let rest = rest.strip_prefix('"')?;
    let end = rest.find('"')?;
    Some(rest[..end].to_string())
}

/// Strip inner attributes from RON content.
fn strip_inner_attributes(content: &str) -> String {
    content
        .lines()
        .filter(|line| {
            let trimmed = line.trim();
            !trimmed.starts_with("#![")
        })
        .collect::<Vec<_>>()
        .join("\n")
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
