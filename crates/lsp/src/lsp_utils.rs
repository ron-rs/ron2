//! Shared LSP utility functions.
//!
//! Common conversions and helpers used across multiple LSP modules.

use tower_lsp::lsp_types::{Position, Range};

use crate::document::Document;

/// Convert a ron2 Span to an LSP Range.
///
/// ron2 uses 1-indexed lines and columns, while LSP uses 0-indexed.
pub fn span_to_range(span: &ron2::error::Span) -> Range {
    Range {
        start: Position {
            line: span.start.line.saturating_sub(1) as u32,
            character: span.start.col.saturating_sub(1) as u32,
        },
        end: Position {
            line: span.end.line.saturating_sub(1) as u32,
            character: span.end.col.saturating_sub(1) as u32,
        },
    }
}

/// Find the position of a field name in the document.
///
/// Uses AST-based lookup when available, falling back to text search.
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
            return Some(span_to_range(&span));
        }
    }
    None
}

/// Find the position of text in the document using simple text search.
pub fn find_text_position(doc: &Document, text: &str) -> Option<Range> {
    for (line_idx, line) in doc.content.lines().enumerate() {
        if let Some(col) = line.find(text) {
            return Some(Range {
                start: Position {
                    line: line_idx as u32,
                    character: col as u32,
                },
                end: Position {
                    line: line_idx as u32,
                    character: (col + text.len()) as u32,
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
