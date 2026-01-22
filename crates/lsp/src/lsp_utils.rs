//! Shared LSP utility functions.
//!
//! Common conversions and helpers used across multiple LSP modules.

use tower_lsp::lsp_types::{Position, Range};

use crate::document::Document;

/// Find the position of a field name in the document.
///
/// Uses AST-based lookup when available, falling back to text search.
#[allow(dead_code)]
pub fn find_field_position(doc: &Document, field: &str) -> Option<Range> {
    // Prefer AST-based position (more accurate)
    if let Some(range) = find_word_range_from_ast(doc, field) {
        return Some(range);
    }

    // Fall back to text search
    let pattern = format!("{}:", field);
    find_text_position(doc, &pattern).or_else(|| find_text_position(doc, field))
}

/// Find the range of a field name from the AST.
///
/// Searches the AST field names and returns the span of the matching field.
pub fn find_word_range_from_ast(doc: &Document, name: &str) -> Option<Range> {
    let fields = doc.get_ast_fields_with_spans();
    for (field_name, span) in fields {
        if field_name == name {
            return Some(doc.span_to_range(&span));
        }
    }
    None
}

/// Convert a byte column offset to UTF-16 code units for a given line.
fn byte_col_to_utf16(line: &str, byte_col: usize) -> u32 {
    line[..byte_col]
        .chars()
        .map(|c| if (c as u32) >= 0x10000 { 2u32 } else { 1u32 })
        .sum()
}

/// Find the position of text in the document using simple text search.
///
/// Converts byte offsets to UTF-16 code units for LSP compatibility.
#[allow(dead_code)]
pub fn find_text_position(doc: &Document, text: &str) -> Option<Range> {
    for (line_idx, line) in doc.content.lines().enumerate() {
        if let Some(byte_col) = line.find(text) {
            let utf16_start = byte_col_to_utf16(line, byte_col);
            let utf16_end = byte_col_to_utf16(line, byte_col + text.len());
            return Some(Range {
                start: Position {
                    line: line_idx as u32,
                    character: utf16_start,
                },
                end: Position {
                    line: line_idx as u32,
                    character: utf16_end,
                },
            });
        }
    }
    None
}

/// Find the field name at the cursor position.
///
/// Uses AST-based lookup when available, falling back to text-based heuristics.
pub fn find_field_at_cursor(doc: &Document, line: u32, col: u32) -> Option<String> {
    // Prefer AST-based lookup (more accurate)
    if let Some(field) = doc.find_field_at_position(line, col) {
        return Some(field);
    }

    // Fall back to text-based heuristics
    find_field_at_cursor_text(doc, line, col)
}

/// Text-based fallback for finding field at cursor (used when AST unavailable).
fn find_field_at_cursor_text(doc: &Document, line: u32, col: u32) -> Option<String> {
    let offset = doc.position_to_offset(line, col)?;
    let before = &doc.content[..offset];

    // Find the most recent colon that's not inside a string
    let mut in_string = false;
    let mut escape_next = false;
    let mut paren_depth = 0i32;
    let mut brace_depth = 0i32;
    let mut colon_pos = None;

    for (i, c) in before.char_indices() {
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
            ':' if !in_string && (paren_depth > 0 || brace_depth == 0) => {
                colon_pos = Some(i);
            }
            _ => {}
        }
    }

    let colon_pos = colon_pos?;

    // Extract the field name before the colon
    let before_colon = &before[..colon_pos];
    let trimmed = before_colon.trim_end();

    // Find the start of the identifier
    let field_start = trimmed
        .rfind(|c: char| !c.is_ascii_alphanumeric() && c != '_')
        .map(|i| i + 1)
        .unwrap_or(0);

    let field_name = &trimmed[field_start..];
    if field_name.is_empty() {
        return None;
    }

    Some(field_name.to_string())
}
