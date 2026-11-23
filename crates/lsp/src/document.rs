//! Document management for the RON Language Server.
//!
//! This module parses RON documents, extracts the inner attributes used for
//! schema resolution, and converts between LSP positions and byte offsets.

use std::path::PathBuf;

use ron2::ast::{self, AttributeContent, Document as AstDocument};
use tower_lsp::lsp_types::{Position, Range, Url};

use crate::cursor::{self, CursorContext};

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
    /// Parsed AST. The parser recovers from errors, so there always is one.
    /// It is owned so that diagnostics, completion and hover share one parse
    /// per edit.
    pub ast: AstDocument<'static>,
    /// Parse errors (if any parsing failed).
    pub parse_errors: Vec<ParseError>,
    /// Byte offset of the start of each line.
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
            // Replaced by `update` below.
            ast: ast::parse_document_lossy("").0.into_owned(),
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
        self.line_offsets = std::iter::once(0)
            .chain(self.content.match_indices('\n').map(|(i, _)| i + 1))
            .collect();
        self.parse_content();
    }

    /// Get the file path for this document.
    pub fn file_path(&self) -> Option<PathBuf> {
        self.uri.to_file_path().ok()
    }

    /// The byte range of a line, without its line break.
    fn line_bounds(&self, line: usize) -> Option<(usize, usize)> {
        let start = *self.line_offsets.get(line)?;
        let next = self
            .line_offsets
            .get(line + 1)
            .copied()
            .unwrap_or(self.content.len());
        let text = &self.content[start..next];
        let text = text.strip_suffix('\n').unwrap_or(text);
        let text = text.strip_suffix('\r').unwrap_or(text);
        Some((start, start + text.len()))
    }

    /// Convert a line and column (0-based, UTF-16 code units) to a byte offset.
    ///
    /// Columns past the end of the line clamp to the line end; a column in the
    /// middle of a surrogate pair maps to the start of that character.
    pub fn position_to_offset(&self, line: u32, col: u32) -> Option<usize> {
        let (line_start, line_end) = self.line_bounds(line as usize)?;
        let col = col as usize;
        let mut utf16 = 0;
        for (byte_idx, ch) in self.content[line_start..line_end].char_indices() {
            utf16 += ch.len_utf16();
            if utf16 > col {
                return Some(line_start + byte_idx);
            }
        }
        Some(line_end)
    }

    /// Convert a byte offset to an LSP position (0-based, UTF-16 code units).
    pub fn offset_to_position(&self, offset: usize) -> Position {
        let offset = offset.min(self.content.len());
        let line = self
            .line_offsets
            .partition_point(|&start| start <= offset)
            .saturating_sub(1);
        let (line_start, line_end) = self.line_bounds(line).unwrap_or((0, 0));
        let mut end = offset.clamp(line_start, line_end);
        while !self.content.is_char_boundary(end) {
            end -= 1;
        }
        Position {
            line: line as u32,
            character: self.content[line_start..end].encode_utf16().count() as u32,
        }
    }

    /// Convert a ron2 span to an LSP range.
    pub fn span_to_range(&self, span: &ron2::Span) -> Range {
        Range {
            start: self.offset_to_position(span.start_offset),
            end: self.offset_to_position(span.end_offset),
        }
    }

    /// Parse the content with the error-recovering AST parser.
    ///
    /// The parser keeps attributes even when the value has errors, so schema
    /// resolution keeps working while the user types.
    fn parse_content(&mut self) {
        let (doc, errors) = ast::parse_document_lossy(&self.content);

        let attribute = |name: &str| {
            doc.attributes
                .iter()
                .find(|a| a.name == name)
                .and_then(|a| match &a.content {
                    AttributeContent::Value(v) => Some(decode_attribute_string(v)),
                    _ => None,
                })
        };
        self.type_attr = attribute("type");
        self.schema_attr = attribute("schema");

        self.parse_errors = errors
            .iter()
            .map(|err| {
                let start = self.offset_to_position(err.span().start_offset);
                let end = self.offset_to_position(err.span().end_offset);
                ParseError {
                    message: err.kind().to_string(),
                    line: start.line as usize,
                    col: start.character as usize,
                    end_line: end.line as usize,
                    end_col: end.character as usize,
                }
            })
            .collect();
        self.ast = doc.into_owned();
    }

    /// Get the cursor context at a position (for completion and hover).
    pub fn context_at_position(&self, line: u32, col: u32) -> CursorContext {
        match self.position_to_offset(line, col) {
            Some(offset) => cursor::context_at_offset(&self.ast, offset),
            None => CursorContext::None,
        }
    }
}

/// Decode an attribute's string literal (e.g., `"value"` -> `value`).
///
/// Handles raw strings (`r"..."`, `r#"..."#`) and escape sequences.
fn decode_attribute_string(s: &str) -> String {
    let s = s.trim();
    match ast::decode_string(s) {
        Ok((value, _kind)) => value,
        Err(_) => s
            .strip_prefix('"')
            .and_then(|s| s.strip_suffix('"'))
            .unwrap_or(s)
            .to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cursor::{Ident, PathStep};

    fn doc(content: &str) -> Document {
        Document::new(
            Url::parse("file:///test.ron").unwrap(),
            content.to_string(),
            1,
        )
    }

    #[test]
    fn test_extract_type_attribute() {
        let doc = doc("#![type = \"my_crate::config::AppConfig\"]\n\n(\n    port: 8080,\n)\n");
        assert_eq!(
            doc.type_attr,
            Some("my_crate::config::AppConfig".to_string())
        );
    }

    #[test]
    fn test_extract_schema_attribute() {
        let doc = doc("#![schema = \"./schemas/app.schema.ron\"]\n\n(\n    port: 8080,\n)\n");
        assert_eq!(
            doc.schema_attr,
            Some("./schemas/app.schema.ron".to_string())
        );
    }

    #[test]
    fn test_attribute_survives_value_errors() {
        let doc = doc("#![type = \"my_crate::Config\"]\n\n(port: @, host: [}\n");
        assert!(!doc.parse_errors.is_empty());
        assert_eq!(doc.type_attr, Some("my_crate::Config".to_string()));
    }

    #[test]
    fn test_position_to_offset() {
        let doc = doc("line1\nline2\nline3");
        assert_eq!(doc.position_to_offset(0, 0), Some(0));
        assert_eq!(doc.position_to_offset(0, 5), Some(5));
        assert_eq!(doc.position_to_offset(0, 99), Some(5));
        assert_eq!(doc.position_to_offset(1, 0), Some(6));
        assert_eq!(doc.position_to_offset(2, 0), Some(12));
        assert_eq!(doc.position_to_offset(3, 0), None);
    }

    #[test]
    fn test_position_conversion_with_crlf() {
        let doc = doc("ab\r\ncd");
        assert_eq!(doc.position_to_offset(0, 9), Some(2));
        assert_eq!(doc.offset_to_position(3), Position::new(0, 2));
        assert_eq!(doc.offset_to_position(4), Position::new(1, 0));
    }

    #[test]
    fn test_completion_context() {
        let doc = doc("(\n    port: 8080,\n\n)");
        assert!(matches!(
            doc.context_at_position(2, 4),
            CursorContext::FieldName { .. }
        ));
    }

    #[test]
    fn test_extract_raw_string_type_attribute() {
        let doc = doc("#![type = r#\"my_crate::config::AppConfig\"#]\n\n(\n    port: 8080,\n)\n");
        assert_eq!(
            doc.type_attr,
            Some("my_crate::config::AppConfig".to_string())
        );
    }

    #[test]
    fn test_extract_escaped_string_type_attribute() {
        let doc = doc("#![type = \"my_crate::config::App\\\"Config\"]\n\n(\n    port: 8080,\n)\n");
        assert_eq!(
            doc.type_attr,
            Some("my_crate::config::App\"Config".to_string())
        );
    }

    #[test]
    fn test_utf16_position_conversion_with_emoji() {
        // 😀 is 4 bytes in UTF-8 and 2 code units in UTF-16.
        let doc = doc("a😀b");
        assert_eq!(doc.offset_to_position(5).character, 3);
        assert_eq!(doc.position_to_offset(0, 3), Some(5));
        // A column inside the surrogate pair maps to the start of the emoji.
        assert_eq!(doc.position_to_offset(0, 2), Some(1));
        // A byte offset inside the emoji maps to the start of the emoji.
        assert_eq!(doc.offset_to_position(3).character, 1);
    }

    #[test]
    fn test_span_to_range_with_emoji() {
        let doc = doc("(emoji: \"😀\", next: 42)");
        // Byte offset 17 is `next`; in UTF-16 the emoji counts 2, so the column is 14.
        let CursorContext::FieldName {
            ident: Some(Ident { name, span }),
            ..
        } = doc.context_at_position(0, 15)
        else {
            panic!("expected field name context");
        };
        assert_eq!(name, "next");
        let range = doc.span_to_range(&span);
        assert_eq!(range.start, Position::new(0, 14));
        assert_eq!(range.end, Position::new(0, 18));
    }

    #[test]
    fn test_nested_context_path() {
        let doc = doc("(server: (tls: ))");
        assert_eq!(
            doc.context_at_position(0, 15),
            CursorContext::Value {
                path: vec![
                    PathStep::Field("server".into()),
                    PathStep::Field("tls".into())
                ],
                ident: None,
            }
        );
    }
}
