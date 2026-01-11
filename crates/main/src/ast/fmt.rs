//! AST formatter for RON documents.
//!
//! This module provides formatting of AST expressions and documents.
//! All serialization flows through this module via [`format_expr`] or [`format_document`].
//!
//! # Format Modes
//!
//! - [`FormatConfig::Minimal`]: No whitespace, no comments. Most compact output.
//! - [`FormatConfig::Compact`]: Single line with readable spacing.
//! - [`FormatConfig::Pretty`]: Multi-line with indentation and comment preservation.

use alloc::string::String;

use crate::ast::{
    AnonStructExpr, Attribute, AttributeContent, Comment, CommentKind, Document, Expr, FieldsBody,
    MapEntry, MapExpr, OptionExpr, SeqExpr, StructBody, StructExpr, StructField, Trivia, TupleBody,
    TupleElement, TupleExpr,
};

/// Controls how RON output is formatted.
#[derive(Clone, Debug)]
pub enum FormatConfig {
    /// No whitespace, no comments. Most compact output.
    ///
    /// Output example: `Point(x:1,y:2)`
    Minimal,

    /// Single line with readable spacing. No line wrapping.
    ///
    /// Output example: `Point(x: 1, y: 2)`
    Compact,

    /// Multi-line with indentation and line wrapping.
    ///
    /// Output example:
    /// ```ron
    /// Point(
    ///     x: 1,
    ///     y: 2,
    /// )
    /// ```
    Pretty(PrettyConfig),
}

/// Configuration for pretty-printed output.
#[derive(Clone, Debug)]
pub struct PrettyConfig {
    /// Indentation string (default: 4 spaces).
    pub indent: String,
    /// Line width before wrapping (default: 80).
    /// Collections that fit within this limit are formatted on a single line.
    pub char_limit: usize,
}

impl Default for FormatConfig {
    fn default() -> Self {
        Self::Pretty(PrettyConfig::default())
    }
}

impl Default for PrettyConfig {
    fn default() -> Self {
        Self {
            indent: String::from("    "),
            char_limit: 80,
        }
    }
}

impl FormatConfig {
    /// Create a new pretty `FormatConfig` with default settings.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the indentation string (only applies to Pretty mode).
    #[must_use]
    pub fn indent(self, indent: impl Into<String>) -> Self {
        match self {
            Self::Pretty(mut config) => {
                config.indent = indent.into();
                Self::Pretty(config)
            }
            other => other,
        }
    }

    /// Set the character limit for compact formatting (only applies to Pretty mode).
    #[must_use]
    pub fn char_limit(self, char_limit: usize) -> Self {
        match self {
            Self::Pretty(mut config) => {
                config.char_limit = char_limit;
                Self::Pretty(config)
            }
            other => other,
        }
    }
}

impl PrettyConfig {
    /// Create a new `PrettyConfig` with default settings.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the indentation string.
    #[must_use]
    pub fn with_indent(mut self, indent: impl Into<String>) -> Self {
        self.indent = indent.into();
        self
    }

    /// Set the character limit for compact formatting.
    #[must_use]
    pub fn with_char_limit(mut self, char_limit: usize) -> Self {
        self.char_limit = char_limit;
        self
    }
}

/// Format a RON document with the given configuration.
///
/// This preserves comments while applying consistent formatting.
/// For Pretty mode, the output always ends with a newline.
///
/// Root-level collections are always formatted multiline in Pretty mode,
/// while nested collections use compact format if they fit within the character limit.
///
/// # Example
///
/// ```
/// use ron2::ast::{parse_document, format_document, FormatConfig};
///
/// let source = "Config(x:1,y:2)";
/// let doc = parse_document(source).unwrap();
/// let formatted = format_document(&doc, &FormatConfig::default());
/// // Root collections are always multiline in Pretty mode
/// assert_eq!(formatted, "Config(\n    x: 1,\n    y: 2,\n)\n");
/// ```
#[must_use]
pub fn format_document(doc: &Document<'_>, config: &FormatConfig) -> String {
    let mut formatter = Formatter::new(config);
    formatter.format_document(doc);
    formatter.output
}

/// Format a RON expression with the given configuration.
///
/// This is the primary entry point for serializing values to RON strings.
/// Unlike [`format_document`], this does not include document-level concerns
/// like attributes or trailing newlines.
///
/// # Example
///
/// ```
/// use ron2::ast::{format_expr, FormatConfig, value_to_expr};
/// use ron2::Value;
///
/// let value = Value::Seq(vec![Value::Number(1.into()), Value::Number(2.into())]);
/// let expr = value_to_expr(value);
/// let minimal = format_expr(&expr, &FormatConfig::Minimal);
/// assert_eq!(minimal, "[1,2]");
/// ```
#[must_use]
pub fn format_expr(expr: &Expr<'_>, config: &FormatConfig) -> String {
    let mut formatter = Formatter::new(config);
    // For standalone expression formatting, don't force root to be multiline
    // unless we're in Pretty mode (handled by is_root flag)
    formatter.is_root = matches!(config, FormatConfig::Pretty(_));
    formatter.format_expr(expr);
    formatter.output
}

/// Internal formatter state.
struct Formatter<'a> {
    config: &'a FormatConfig,
    output: String,
    indent_level: usize,
    /// Whether we're formatting the root expression (forces multiline in Pretty mode).
    is_root: bool,
}

impl<'a> Formatter<'a> {
    fn new(config: &'a FormatConfig) -> Self {
        Self {
            config,
            output: String::new(),
            indent_level: 0,
            is_root: true,
        }
    }

    /// Returns the char limit, or MAX for non-Pretty modes (always compact).
    fn char_limit(&self) -> usize {
        match self.config {
            FormatConfig::Pretty(config) => config.char_limit,
            _ => usize::MAX,
        }
    }

    fn format_document(&mut self, doc: &Document<'_>) {
        let is_pretty = matches!(self.config, FormatConfig::Pretty(_));

        // Format leading comments (before attributes) - Pretty mode only
        self.format_leading_comments(&doc.leading);

        // Format attributes (one per line in Pretty mode, inline in others)
        for attr in &doc.attributes {
            self.format_attribute(attr);
        }

        // Empty line between attributes and value (if both exist) - Pretty mode only
        if is_pretty && !doc.attributes.is_empty() && doc.value.is_some() {
            self.output.push('\n');
        }

        // Format pre-value comments - Pretty mode only
        self.format_leading_comments(&doc.pre_value);

        // Format the main value
        if let Some(ref value) = doc.value {
            self.format_expr(value);
        }

        // Format trailing comments (at end of document) - Pretty mode only
        self.format_leading_comments(&doc.trailing);

        // Ensure file ends with newline if it has content - Pretty mode only
        if is_pretty && !self.output.is_empty() && !self.output.ends_with('\n') {
            self.output.push('\n');
        }
    }

    fn format_attribute(&mut self, attr: &Attribute<'_>) {
        // Leading comments for this attribute
        self.format_leading_comments(&attr.leading);

        self.output.push_str("#![");
        self.output.push_str(&attr.name);

        match &attr.content {
            AttributeContent::None => {}
            AttributeContent::Value(v) => {
                self.output.push_str(" = ");
                self.output.push_str(v);
            }
            AttributeContent::Args(args) => {
                self.output.push('(');
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        self.output.push_str(", ");
                    }
                    self.output.push_str(arg);
                }
                self.output.push(')');
            }
        }

        self.output.push_str("]\n");
    }

    fn format_expr(&mut self, expr: &Expr<'_>) {
        match expr {
            Expr::Unit(_) => self.output.push_str("()"),
            Expr::Bool(b) => {
                if b.value {
                    self.output.push_str("true");
                } else {
                    self.output.push_str("false");
                }
            }
            Expr::Char(c) => {
                // Use the raw representation to preserve escaping
                self.output.push_str(&c.raw);
            }
            Expr::Byte(b) => {
                self.output.push_str(&b.raw);
            }
            Expr::Number(n) => {
                // Use raw to preserve hex, binary, underscores
                self.output.push_str(&n.raw);
            }
            Expr::String(s) => {
                // Use raw to preserve raw strings, escapes
                self.output.push_str(&s.raw);
            }
            Expr::Bytes(b) => {
                self.output.push_str(&b.raw);
            }
            Expr::Option(opt) => self.format_option(opt),
            Expr::Seq(seq) => self.format_seq(seq),
            Expr::Map(map) => self.format_map(map),
            Expr::Tuple(tuple) => self.format_tuple(tuple),
            Expr::AnonStruct(s) => self.format_anon_struct(s),
            Expr::Struct(s) => self.format_struct(s),
        }
    }

    fn format_option(&mut self, opt: &OptionExpr<'_>) {
        if let Some(inner) = &opt.value {
            self.output.push_str("Some(");
            self.format_leading_comments(&inner.leading);
            self.format_expr(&inner.expr);
            self.format_trailing_inline_comment(&inner.trailing);
            self.output.push(')');
        } else {
            self.output.push_str("None");
        }
    }

    fn format_seq(&mut self, seq: &SeqExpr<'_>) {
        self.format_collection(
            '[',
            ']',
            &seq.leading,
            &seq.trailing,
            &seq.items,
            |f, item| {
                // Leading comments go on their own lines
                f.format_leading_comments(&item.leading);
                // Indent for the item itself
                f.write_indent();
                f.format_expr(&item.expr);
                f.format_trailing_inline_comment(&item.trailing);
            },
            |item| has_line_comment(&item.leading) || has_line_comment(&item.trailing),
        );
    }

    fn format_map(&mut self, map: &MapExpr<'_>) {
        self.format_collection(
            '{',
            '}',
            &map.leading,
            &map.trailing,
            &map.entries,
            |f, entry| {
                f.format_map_entry(entry);
            },
            |entry| {
                has_line_comment(&entry.leading)
                    || has_line_comment(&entry.pre_colon)
                    || has_line_comment(&entry.post_colon)
                    || has_line_comment(&entry.trailing)
            },
        );
    }

    fn format_map_entry(&mut self, entry: &MapEntry<'_>) {
        // Leading comments go on their own lines
        self.format_leading_comments(&entry.leading);
        // Indent for the entry itself
        self.write_indent();
        self.format_expr(&entry.key);
        self.format_trailing_inline_comment(&entry.pre_colon);
        self.write_colon();
        self.format_leading_comments_inline(&entry.post_colon);
        self.format_expr(&entry.value);
        self.format_trailing_inline_comment(&entry.trailing);
    }

    fn format_tuple(&mut self, tuple: &TupleExpr<'_>) {
        self.format_collection(
            '(',
            ')',
            &tuple.leading,
            &tuple.trailing,
            &tuple.elements,
            |f, elem| {
                f.format_tuple_element(elem);
            },
            |elem| has_line_comment(&elem.leading) || has_line_comment(&elem.trailing),
        );
    }

    fn format_tuple_element(&mut self, elem: &TupleElement<'_>) {
        // Leading comments go on their own lines
        self.format_leading_comments(&elem.leading);
        // Indent for the element itself
        self.write_indent();
        self.format_expr(&elem.expr);
        self.format_trailing_inline_comment(&elem.trailing);
    }

    fn format_anon_struct(&mut self, s: &AnonStructExpr<'_>) {
        self.format_collection(
            '(',
            ')',
            &s.leading,
            &s.trailing,
            &s.fields,
            |f, field| {
                f.format_struct_field(field);
            },
            |field| {
                has_line_comment(&field.leading)
                    || has_line_comment(&field.pre_colon)
                    || has_line_comment(&field.post_colon)
                    || has_line_comment(&field.trailing)
            },
        );
    }

    fn format_struct(&mut self, s: &StructExpr<'_>) {
        self.output.push_str(&s.name.name);

        if let Some(ref body) = s.body {
            match body {
                StructBody::Tuple(tuple) => self.format_tuple_body(tuple),
                StructBody::Fields(fields) => self.format_fields_body(fields),
            }
        }
    }

    fn format_tuple_body(&mut self, tuple: &TupleBody<'_>) {
        self.format_collection(
            '(',
            ')',
            &tuple.leading,
            &tuple.trailing,
            &tuple.elements,
            |f, elem| {
                f.format_tuple_element(elem);
            },
            |elem| has_line_comment(&elem.leading) || has_line_comment(&elem.trailing),
        );
    }

    fn format_fields_body(&mut self, fields: &FieldsBody<'_>) {
        self.format_collection(
            '(',
            ')',
            &fields.leading,
            &fields.trailing,
            &fields.fields,
            |f, field| {
                f.format_struct_field(field);
            },
            |field| {
                has_line_comment(&field.leading)
                    || has_line_comment(&field.pre_colon)
                    || has_line_comment(&field.post_colon)
                    || has_line_comment(&field.trailing)
            },
        );
    }

    fn format_struct_field(&mut self, field: &StructField<'_>) {
        // Leading comments go on their own lines
        self.format_leading_comments(&field.leading);
        // Indent for the field itself
        self.write_indent();
        self.output.push_str(&field.name.name);
        self.format_trailing_inline_comment(&field.pre_colon);
        self.write_colon();
        self.format_leading_comments_inline(&field.post_colon);
        self.format_expr(&field.value);
        self.format_trailing_inline_comment(&field.trailing);
    }

    /// Generic collection formatter that handles compact vs multiline.
    #[allow(clippy::too_many_arguments)]
    fn format_collection<T, F, C>(
        &mut self,
        open: char,
        close: char,
        leading: &Trivia<'_>,
        trailing: &Trivia<'_>,
        items: &[T],
        format_item: F,
        has_line_comment_fn: C,
    ) where
        F: Fn(&mut Self, &T),
        C: Fn(&T) -> bool,
    {
        // Root collections with items are always multiline (compact only for nested)
        let is_root = self.is_root;
        if is_root {
            self.is_root = false;
        }

        // Check if any item has a line comment (forces multiline)
        let has_line_comments = items.iter().any(&has_line_comment_fn);

        // Check if leading/trailing trivia has line comments
        let trivia_has_line_comments = has_line_comment(leading) || has_line_comment(trailing);

        // Force multiline for root collections with items, or when comments are present
        if (is_root && !items.is_empty()) || has_line_comments || trivia_has_line_comments {
            self.format_collection_multiline(open, close, leading, trailing, items, format_item);
            return;
        }

        // Try compact format (only for nested collections)
        let compact = self.try_format_compact(open, close, items, &format_item);
        if compact.len() <= self.char_limit() {
            self.output.push_str(&compact);
        } else {
            self.format_collection_multiline(open, close, leading, trailing, items, format_item);
        }
    }

    fn try_format_compact<T, F>(
        &self,
        open: char,
        close: char,
        items: &[T],
        format_item: F,
    ) -> String
    where
        F: Fn(&mut Self, &T),
    {
        let mut compact_formatter = Formatter::new(self.config);
        compact_formatter.is_root = false; // Nested collections in compact mode
        compact_formatter.output.push(open);

        for (i, item) in items.iter().enumerate() {
            if i > 0 {
                compact_formatter.write_separator();
            }
            format_item(&mut compact_formatter, item);
        }

        compact_formatter.output.push(close);
        compact_formatter.output
    }

    fn format_collection_multiline<T, F>(
        &mut self,
        open: char,
        close: char,
        leading: &Trivia<'_>,
        trailing: &Trivia<'_>,
        items: &[T],
        format_item: F,
    ) where
        F: Fn(&mut Self, &T),
    {
        self.output.push(open);

        if items.is_empty() {
            // Handle leading/trailing comments for empty collections
            self.format_leading_comments(leading);
            self.format_trailing_inline_comment(trailing);
            self.output.push(close);
            return;
        }

        self.output.push('\n');
        self.indent_level += 1;

        // Leading trivia inside the collection
        self.format_leading_comments(leading);

        for (i, item) in items.iter().enumerate() {
            // format_item handles its own indentation (including for leading comments)
            format_item(self, item);
            self.output.push(',');
            if i < items.len() - 1 {
                self.output.push('\n');
            }
        }

        // Trailing trivia before closing delimiter
        if has_line_comment(trailing) {
            self.output.push('\n');
            self.format_leading_comments(trailing);
        }

        self.output.push('\n');
        self.indent_level -= 1;
        self.write_indent();
        self.output.push(close);
    }

    fn write_indent(&mut self) {
        let indent = match self.config {
            FormatConfig::Pretty(config) => &config.indent,
            _ => return, // No indentation in Minimal/Compact modes
        };
        for _ in 0..self.indent_level {
            self.output.push_str(indent);
        }
    }

    fn write_colon(&mut self) {
        match self.config {
            FormatConfig::Minimal => self.output.push(':'),
            FormatConfig::Compact | FormatConfig::Pretty(_) => self.output.push_str(": "),
        }
    }

    fn write_separator(&mut self) {
        match self.config {
            FormatConfig::Minimal => self.output.push(','),
            FormatConfig::Compact | FormatConfig::Pretty(_) => self.output.push_str(", "),
        }
    }

    /// Format leading comments (comments that appear before an item on their own lines).
    fn format_leading_comments(&mut self, trivia: &Trivia<'_>) {
        // Only Pretty mode preserves leading comments
        if !matches!(self.config, FormatConfig::Pretty(_)) {
            return;
        }
        for comment in &trivia.comments {
            self.format_comment(comment);
        }
    }

    /// Format leading comments inline (for comments after colons, don't add newlines).
    fn format_leading_comments_inline(&mut self, trivia: &Trivia<'_>) {
        // No comments in Minimal mode
        if matches!(self.config, FormatConfig::Minimal) {
            return;
        }
        for comment in &trivia.comments {
            match comment.kind {
                CommentKind::Block => {
                    self.output.push(' ');
                    self.output.push_str(&comment.text);
                    self.output.push(' ');
                }
                CommentKind::Line => {
                    // Line comments can't really be inline, but if they're here,
                    // they'll force a newline (only in Pretty mode)
                    if matches!(self.config, FormatConfig::Pretty(_)) {
                        self.output.push_str("  ");
                        self.output.push_str(&comment.text);
                    }
                }
            }
        }
    }

    /// Format trailing inline comments (comments on the same line after a value).
    fn format_trailing_inline_comment(&mut self, trivia: &Trivia<'_>) {
        // No comments in Minimal mode
        if matches!(self.config, FormatConfig::Minimal) {
            return;
        }
        for comment in &trivia.comments {
            match comment.kind {
                CommentKind::Line => {
                    // Line comments only in Pretty mode (they need newlines)
                    if matches!(self.config, FormatConfig::Pretty(_)) {
                        self.output.push_str("  ");
                        self.output.push_str(&comment.text);
                    }
                }
                CommentKind::Block => {
                    self.output.push(' ');
                    self.output.push_str(&comment.text);
                }
            }
        }
    }

    fn format_comment(&mut self, comment: &Comment<'_>) {
        // Only called from format_leading_comments which already checks for Pretty mode
        match comment.kind {
            CommentKind::Line => {
                self.write_indent();
                self.output.push_str(&comment.text);
                if !comment.text.ends_with('\n') {
                    self.output.push('\n');
                }
            }
            CommentKind::Block => {
                self.write_indent();
                self.output.push_str(&comment.text);
                self.output.push('\n');
            }
        }
    }
}

/// Check if trivia contains any line comments.
fn has_line_comment(trivia: &Trivia<'_>) -> bool {
    trivia.comments.iter().any(|c| c.kind == CommentKind::Line)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::parse_document;

    fn format(source: &str) -> String {
        let doc = parse_document(source).unwrap();
        format_document(&doc, &FormatConfig::default())
    }

    fn format_with(source: &str, config: &FormatConfig) -> String {
        let doc = parse_document(source).unwrap();
        format_document(&doc, config)
    }

    // =========================================================================
    // Basic values
    // =========================================================================

    #[test]
    fn test_simple_values() {
        assert_eq!(format("42"), "42\n");
        assert_eq!(format("true"), "true\n");
        assert_eq!(format("false"), "false\n");
        assert_eq!(format("\"hello\""), "\"hello\"\n");
        assert_eq!(format("'c'"), "'c'\n");
        assert_eq!(format("()"), "()\n");
        assert_eq!(format("None"), "None\n");
        assert_eq!(format("Some(42)"), "Some(42)\n");
    }

    #[test]
    fn test_preserves_number_format() {
        // Hex, binary, octal are preserved
        assert_eq!(format("0xFF"), "0xFF\n");
        assert_eq!(format("0b1010"), "0b1010\n");
        assert_eq!(format("0o777"), "0o777\n");
        assert_eq!(format("1_000_000"), "1_000_000\n");
    }

    #[test]
    fn test_preserves_string_format() {
        // Raw strings are preserved
        assert_eq!(format(r#"r"raw string""#), "r\"raw string\"\n");
        assert_eq!(format(r##"r#"hash raw"#"##), "r#\"hash raw\"#\n");
    }

    // =========================================================================
    // Sequences
    // =========================================================================

    #[test]
    fn test_root_seq_multiline() {
        // Root collections are always multiline
        assert_eq!(format("[1, 2, 3]"), "[\n    1,\n    2,\n    3,\n]\n");
        assert_eq!(format("[1,2,3]"), "[\n    1,\n    2,\n    3,\n]\n");
        assert_eq!(format("[ 1 , 2 , 3 ]"), "[\n    1,\n    2,\n    3,\n]\n");
    }

    #[test]
    fn test_empty_seq() {
        assert_eq!(format("[]"), "[]\n");
        assert_eq!(format("[  ]"), "[]\n");
    }

    #[test]
    fn test_seq_exceeds_limit() {
        let config = FormatConfig::default().char_limit(10);
        let formatted = format_with("[1, 2, 3, 4, 5]", &config);
        assert_eq!(formatted, "[\n    1,\n    2,\n    3,\n    4,\n    5,\n]\n");
    }

    #[test]
    fn test_seq_with_line_comment() {
        let source = "[\n    // first item\n    1,\n    2,\n]";
        let formatted = format(source);
        assert!(formatted.contains("// first item"));
        assert!(formatted.contains("    1,"));
    }

    // =========================================================================
    // Structs
    // =========================================================================

    #[test]
    fn test_root_struct_multiline() {
        // Root collections are always multiline
        assert_eq!(
            format("Point(x:1,y:2)"),
            "Point(\n    x: 1,\n    y: 2,\n)\n"
        );
        assert_eq!(
            format("Point(x: 1, y: 2)"),
            "Point(\n    x: 1,\n    y: 2,\n)\n"
        );
        assert_eq!(
            format("Point( x : 1 , y : 2 )"),
            "Point(\n    x: 1,\n    y: 2,\n)\n"
        );
    }

    #[test]
    fn test_unit_struct() {
        assert_eq!(format("Empty"), "Empty\n");
    }

    #[test]
    fn test_tuple_struct() {
        // Root tuple structs are multiline
        assert_eq!(
            format("Point(1, 2, 3)"),
            "Point(\n    1,\n    2,\n    3,\n)\n"
        );
    }

    #[test]
    fn test_struct_exceeds_limit() {
        let config = FormatConfig::default().char_limit(20);
        let formatted = format_with("Config(name: \"test\", value: 42)", &config);
        assert!(
            formatted.contains('\n'),
            "Expected multiline, got: {formatted:?}"
        );
        assert!(formatted.contains("name: \"test\","));
        assert!(formatted.contains("value: 42,"));
    }

    #[test]
    fn test_struct_with_comments() {
        let source = r#"Config(
    // Server port
    port: 8080,
    host: "localhost",
)"#;
        let formatted = format(source);
        assert!(formatted.contains("// Server port"));
        assert!(formatted.contains("port: 8080,"));
        assert!(formatted.contains("host: \"localhost\","));
    }

    #[test]
    fn test_nested_struct() {
        // Root is multiline, nested stays compact
        let source = "Config(inner: Point(x: 1, y: 2))";
        let formatted = format(source);
        assert_eq!(formatted, "Config(\n    inner: Point(x: 1, y: 2),\n)\n");
    }

    #[test]
    fn test_deeply_nested() {
        let config = FormatConfig::default().char_limit(30);
        let source = "A(b: B(c: C(d: D(e: 1))))";
        let formatted = format_with(source, &config);
        // Should expand due to length
        assert!(formatted.contains('\n'));
    }

    // =========================================================================
    // Maps
    // =========================================================================

    #[test]
    fn test_root_map_multiline() {
        // Root collections are always multiline
        let source = "{\"a\": 1, \"b\": 2}";
        let formatted = format(source);
        assert_eq!(formatted, "{\n    \"a\": 1,\n    \"b\": 2,\n}\n");
    }

    #[test]
    fn test_empty_map() {
        assert_eq!(format("{}"), "{}\n");
    }

    #[test]
    fn test_map_exceeds_limit() {
        let config = FormatConfig::default().char_limit(15);
        let formatted = format_with("{\"key\": \"value\"}", &config);
        assert!(formatted.contains('\n'));
    }

    // =========================================================================
    // Tuples
    // =========================================================================

    #[test]
    fn test_root_tuple_multiline() {
        // Root collections are always multiline
        assert_eq!(format("(1, 2, 3)"), "(\n    1,\n    2,\n    3,\n)\n");
        assert_eq!(format("(1,2,3)"), "(\n    1,\n    2,\n    3,\n)\n");
    }

    #[test]
    fn test_single_element_tuple() {
        // Root collections are always multiline
        assert_eq!(format("(42,)"), "(\n    42,\n)\n");
    }

    // =========================================================================
    // Comments
    // =========================================================================

    #[test]
    fn test_leading_comment() {
        let source = "// header comment\n42";
        let formatted = format(source);
        assert!(formatted.starts_with("// header comment\n"));
        assert!(formatted.contains("42"));
    }

    #[test]
    fn test_block_comment() {
        let source = "/* block */ 42";
        let formatted = format(source);
        assert!(formatted.contains("/* block */"));
    }

    #[test]
    fn test_comment_between_fields() {
        let source = "(
    x: 1,
    // separator
    y: 2,
)";
        let formatted = format(source);
        assert!(formatted.contains("// separator"));
        assert!(formatted.contains("x: 1,"));
        assert!(formatted.contains("y: 2,"));
    }

    // =========================================================================
    // Attributes
    // =========================================================================

    #[test]
    fn test_single_attribute() {
        let source = "#![type = \"Foo\"]\n42";
        let formatted = format(source);
        assert!(formatted.starts_with("#![type = \"Foo\"]"));
        assert!(formatted.contains("\n\n42"));
    }

    #[test]
    fn test_multiple_attributes() {
        let source = "#![type = \"Foo\"]\n#![enable(unwrap_newtypes)]\n42";
        let formatted = format(source);
        assert!(formatted.contains("#![type = \"Foo\"]"));
        assert!(formatted.contains("#![enable(unwrap_newtypes)]"));
    }

    #[test]
    fn test_attribute_with_args() {
        let source = "#![enable(implicit_some, unwrap_newtypes)]\n42";
        let formatted = format(source);
        assert!(formatted.contains("#![enable(implicit_some, unwrap_newtypes)]"));
    }

    // =========================================================================
    // Configuration
    // =========================================================================

    #[test]
    fn test_custom_indent() {
        let config = FormatConfig::new().indent("  ").char_limit(5);
        let formatted = format_with("[1, 2, 3]", &config);
        assert!(
            formatted.contains("  1,"),
            "Expected 2-space indent in: {formatted:?}"
        );
    }

    #[test]
    fn test_tab_indent() {
        let config = FormatConfig::new().indent("\t").char_limit(5);
        let formatted = format_with("[1, 2, 3]", &config);
        assert!(
            formatted.contains("\t1,"),
            "Expected tab indent in: {formatted:?}"
        );
    }

    #[test]
    fn test_large_char_limit_nested() {
        // Char limit only applies to nested collections, not root
        let config = FormatConfig::new().char_limit(1000);
        // Test with nested array inside a struct
        let long_array = (1..50)
            .map(|n| n.to_string())
            .collect::<Vec<_>>()
            .join(", ");
        let source = format!("Config(items: [{long_array}])");
        let formatted = format_with(&source, &config);
        // Root should be multiline, but nested array stays compact
        assert!(formatted.contains(&format!("[{long_array}]")));
    }

    // =========================================================================
    // Edge cases
    // =========================================================================

    #[test]
    fn test_empty_document() {
        // Empty document produces empty output
        assert_eq!(format(""), "");
    }

    #[test]
    fn test_comment_only_document() {
        let source = "// just a comment\n";
        let formatted = format(source);
        assert!(formatted.contains("// just a comment"));
    }

    #[test]
    fn test_trailing_comma_preserved_multiline() {
        let config = FormatConfig::default().char_limit(5);
        let formatted = format_with("[1, 2]", &config);
        // All items should have trailing commas in multiline mode
        assert!(formatted.contains("1,"), "Expected '1,' in: {formatted:?}");
        assert!(formatted.contains("2,"), "Expected '2,' in: {formatted:?}");
    }

    #[test]
    fn test_no_trailing_comma_compact_nested() {
        // Test compact nested collection has no trailing comma
        let formatted = format("Config(items: [1, 2, 3])");
        // Root is multiline, nested is compact without trailing comma
        assert!(formatted.contains("[1, 2, 3]"));
    }

    #[test]
    fn test_anonymous_struct() {
        // Root collections are always multiline
        let source = "(x: 1, y: 2)";
        let formatted = format(source);
        assert_eq!(formatted, "(\n    x: 1,\n    y: 2,\n)\n");
    }

    #[test]
    fn test_option_some() {
        assert_eq!(format("Some(42)"), "Some(42)\n");
        assert_eq!(format("Some( 42 )"), "Some(42)\n");
    }

    #[test]
    fn test_option_none() {
        assert_eq!(format("None"), "None\n");
    }

    #[test]
    fn test_bytes() {
        assert_eq!(format("b\"hello\""), "b\"hello\"\n");
    }

    #[test]
    fn test_empty_collections() {
        assert_eq!(format("[]"), "[]\n");
        assert_eq!(format("{}"), "{}\n");
        assert_eq!(format("()"), "()\n");
    }
}
