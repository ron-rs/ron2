//! AST formatter for RON documents.
//!
//! This module provides formatting of AST expressions and documents.
//! All serialization flows through this module via [`format_expr`] or [`format_document`].
//!
//! # Configuration
//!
//! Use [`FormatConfig::new()`] for pretty output with sensible defaults, or
//! [`FormatConfig::minimal()`] for the most compact output (no whitespace, no comments).

use alloc::string::String;

use crate::ast::{
    AnonStructExpr, Attribute, AttributeContent, Comment, CommentKind, Document, Expr, FieldsBody,
    MapEntry, MapExpr, OptionExpr, SeqExpr, StructBody, StructExpr, StructField, Trivia, TupleBody,
    TupleElement, TupleExpr,
};

/// Controls how RON output is formatted.
#[derive(Clone, Debug)]
pub struct FormatConfig {
    /// Indentation string. Empty string = no indentation.
    pub indent: String,

    /// Spacing style for compact mode.
    pub spacing: Spacing,

    /// How to handle comments during formatting.
    pub comments: CommentMode,

    /// Rules for when to use compact (single-line) formatting.
    pub compaction: Compaction,
}

/// Spacing style for compact output.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum Spacing {
    /// No spaces: `x:1,y:2`
    None,
    /// Normal spacing: `x: 1, y: 2`
    #[default]
    Normal,
}

/// How to handle comments during formatting.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum CommentMode {
    /// Delete all comments from output.
    Delete,
    /// Preserve comments. Line comments prevent ANY compaction.
    Preserve,
    /// Smart mode: preserve in multiline, convert `// foo` → `/* foo */`
    /// only when depth/type rules force compaction. Length-based compaction
    /// is still blocked by line comments.
    #[default]
    Auto,
}

/// Rules for when to switch from multiline to compact formatting.
///
/// Compaction is triggered when ANY of these conditions is met (OR logic):
/// - Collection depth >= `compact_from_depth`
/// - Collection type is in `compact_types`
/// - Collection fits within `char_limit`
///
/// In compact mode:
/// - No newlines or indentation
/// - Line comments converted to block: `// foo` → `/* foo */`
#[derive(Clone, Debug)]
pub struct Compaction {
    /// Maximum line length before forcing multiline.
    /// Set to 0 to disable length-based compaction.
    /// Default: 20
    pub char_limit: usize,

    /// Depth at which to start compacting collections.
    /// - `None`: No depth-based compaction (default)
    /// - `Some(0)`: Compact even root collections
    /// - `Some(1)`: Compact first nesting level and deeper
    /// - `Some(2)`: Compact second nesting level and deeper
    pub compact_from_depth: Option<usize>,

    /// Collection types to always compact regardless of length/depth.
    /// Default: none
    pub compact_types: CompactTypes,
}

/// Collection types that can be marked for automatic compaction.
#[derive(Clone, Debug, Default)]
#[allow(clippy::struct_excessive_bools)]
pub struct CompactTypes {
    /// Compact tuple expressions: `(1, 2, 3)` and `Point(1, 2, 3)`
    pub tuples: bool,
    /// Compact array/sequence expressions: `[1, 2, 3]`
    pub arrays: bool,
    /// Compact map expressions: `{"a": 1}`
    pub maps: bool,
    /// Compact struct field expressions: `Point(x: 1, y: 2)` and `(x: 1, y: 2)`
    pub structs: bool,
}

impl Default for FormatConfig {
    fn default() -> Self {
        Self {
            indent: String::from("    "),
            spacing: Spacing::Normal,
            comments: CommentMode::Auto,
            compaction: Compaction::default(),
        }
    }
}

impl Default for Compaction {
    fn default() -> Self {
        Self {
            char_limit: 20,
            compact_from_depth: None,
            compact_types: CompactTypes::default(),
        }
    }
}

impl CompactTypes {
    /// Create `CompactTypes` with all types enabled.
    #[must_use]
    pub fn all() -> Self {
        Self {
            tuples: true,
            arrays: true,
            maps: true,
            structs: true,
        }
    }

    /// Create `CompactTypes` with no types enabled (default).
    #[must_use]
    pub fn none() -> Self {
        Self::default()
    }
}

impl FormatConfig {
    /// Create a new `FormatConfig` with default pretty settings.
    ///
    /// Defaults: 4-space indent, normal spacing, auto comments, 80 char limit.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a minimal `FormatConfig` for most compact output.
    ///
    /// No whitespace, no comments, fully compact.
    /// Output example: `Point(x:1,y:2)`
    #[must_use]
    pub fn minimal() -> Self {
        Self {
            indent: String::new(),
            spacing: Spacing::None,
            comments: CommentMode::Delete,
            compaction: Compaction {
                char_limit: usize::MAX,
                compact_from_depth: Some(0),
                compact_types: CompactTypes::default(),
            },
        }
    }

    /// Set the indentation string.
    #[must_use]
    pub fn indent(mut self, indent: impl Into<String>) -> Self {
        self.indent = indent.into();
        self
    }

    /// Set the spacing style.
    #[must_use]
    pub fn spacing(mut self, spacing: Spacing) -> Self {
        self.spacing = spacing;
        self
    }

    /// Set the comment handling mode.
    #[must_use]
    pub fn comments(mut self, comments: CommentMode) -> Self {
        self.comments = comments;
        self
    }

    /// Set the character limit for compact formatting.
    /// Set to 0 to disable length-based compaction.
    #[must_use]
    pub fn char_limit(mut self, char_limit: usize) -> Self {
        self.compaction.char_limit = char_limit;
        self
    }

    /// Set the depth at which to start compacting.
    #[must_use]
    pub fn compact_from_depth(mut self, depth: usize) -> Self {
        self.compaction.compact_from_depth = Some(depth);
        self
    }

    /// Set the collection types to always compact.
    #[must_use]
    pub fn compact_types(mut self, types: CompactTypes) -> Self {
        self.compaction.compact_types = types;
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
/// let minimal = format_expr(&expr, &FormatConfig::minimal());
/// assert_eq!(minimal, "[1,2]");
/// ```
#[must_use]
pub fn format_expr(expr: &Expr<'_>, config: &FormatConfig) -> String {
    let mut formatter = Formatter::new(config);
    // For standalone expression formatting, don't force root to be multiline
    // Set is_root based on whether we have a non-empty indent (pretty mode behavior)
    formatter.is_root = !config.indent.is_empty();
    formatter.format_expr(expr);
    formatter.output
}

/// Collection types for compaction decisions.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum CollectionType {
    Tuple,
    Array,
    Map,
    Struct,
}

/// Internal formatter state.
struct Formatter<'a> {
    config: &'a FormatConfig,
    output: String,
    indent_level: usize,
    /// Current nesting depth (0 = root level).
    depth: usize,
    /// Whether we're formatting the root expression (forces multiline in Pretty mode).
    is_root: bool,
    /// Whether we're in compact mode (single line, no indentation).
    is_compact: bool,
}

impl<'a> Formatter<'a> {
    fn new(config: &'a FormatConfig) -> Self {
        Self {
            config,
            output: String::new(),
            indent_level: 0,
            depth: 0,
            is_root: true,
            is_compact: false,
        }
    }

    /// Returns the char limit from config.
    fn char_limit(&self) -> usize {
        self.config.compaction.char_limit
    }

    /// Determine if compaction is requested by depth or type rules.
    ///
    /// Returns true if depth-based or type-based compaction rules apply.
    /// Length-based compaction is handled separately in `format_collection`.
    fn wants_compact(&self, collection_type: CollectionType, depth: usize) -> bool {
        let compaction = &self.config.compaction;

        // 1. Depth-based
        if let Some(threshold) = compaction.compact_from_depth
            && depth >= threshold
        {
            return true;
        }

        // 2. Type-based
        match collection_type {
            CollectionType::Tuple => compaction.compact_types.tuples,
            CollectionType::Array => compaction.compact_types.arrays,
            CollectionType::Map => compaction.compact_types.maps,
            CollectionType::Struct => compaction.compact_types.structs,
        }
    }

    /// Check if we're in "pretty" mode (non-empty indent).
    fn is_pretty(&self) -> bool {
        !self.config.indent.is_empty()
    }

    fn format_document(&mut self, doc: &Document<'_>) {
        let is_pretty = self.is_pretty();

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
            Expr::Error(_) => {
                self.output.push_str("/* parse error */");
            }
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
            CollectionType::Array,
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
            CollectionType::Map,
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
            CollectionType::Tuple,
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
            CollectionType::Struct,
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
            CollectionType::Tuple,
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
            CollectionType::Struct,
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
    ///
    /// Compaction behavior depends on `CommentMode`:
    /// - `Delete`: comments stripped, all compaction proceeds freely
    /// - `Preserve`: line comments prevent ANY compaction
    /// - `Auto`: line comments prevent length-based, but depth/type rules convert and compact
    #[allow(clippy::too_many_arguments)]
    fn format_collection<T, F, C>(
        &mut self,
        collection_type: CollectionType,
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
        // Track depth for nested collections
        let current_depth = self.depth;
        self.depth += 1;

        let is_root = self.is_root;
        if is_root {
            self.is_root = false;
        }

        // Check if depth/type rules want compaction
        let wants_compact = self.wants_compact(collection_type, current_depth);

        // Check for line comments
        let has_line_comments = items.iter().any(&has_line_comment_fn)
            || has_line_comment(leading)
            || has_line_comment(trailing);

        // Determine if we can actually compact based on comment mode
        let can_compact = match self.config.comments {
            CommentMode::Delete => true,                 // No comment constraints
            CommentMode::Preserve => !has_line_comments, // Line comments prevent all compaction
            CommentMode::Auto => {
                if wants_compact {
                    true // Depth/type rules: can compact (will convert comments)
                } else {
                    !has_line_comments // Length-based: line comments prevent
                }
            }
        };

        // 1. If depth/type rules want compaction AND we can compact
        if wants_compact && can_compact {
            let compact = self.try_format_compact_with_trivia(
                open,
                close,
                leading,
                trailing,
                items,
                &format_item,
            );
            self.output.push_str(&compact);
            self.depth = current_depth;
            return;
        }

        // 2. Root collections default to multiline (unless compaction rule was triggered above)
        if is_root && !items.is_empty() {
            self.format_collection_multiline(open, close, leading, trailing, items, format_item);
            self.depth = current_depth;
            return;
        }

        // 3. Can't compact due to comments - use multiline
        if !can_compact {
            self.format_collection_multiline(open, close, leading, trailing, items, format_item);
            self.depth = current_depth;
            return;
        }

        // 4. Try length-based compaction
        let compact = self.try_format_compact_with_trivia(
            open,
            close,
            leading,
            trailing,
            items,
            &format_item,
        );
        if compact.len() <= self.char_limit() {
            self.output.push_str(&compact);
        } else {
            self.format_collection_multiline(open, close, leading, trailing, items, format_item);
        }
        self.depth = current_depth;
    }

    /// Format a collection in compact mode, including trivia with comment conversion.
    fn try_format_compact_with_trivia<T, F>(
        &self,
        open: char,
        close: char,
        leading: &Trivia<'_>,
        trailing: &Trivia<'_>,
        items: &[T],
        format_item: F,
    ) -> String
    where
        F: Fn(&mut Self, &T),
    {
        let mut compact_formatter = Formatter::new(self.config);
        compact_formatter.is_root = false;
        compact_formatter.is_compact = true;
        compact_formatter.depth = self.depth;
        compact_formatter.output.push(open);

        // Leading trivia (converted to compact format)
        compact_formatter.format_trivia_compact(leading);

        for (i, item) in items.iter().enumerate() {
            if i > 0 {
                compact_formatter.write_separator();
            }
            format_item(&mut compact_formatter, item);
        }

        // Trailing trivia (converted to compact format)
        compact_formatter.format_trivia_compact(trailing);

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
        // No indentation in compact mode or if indent is empty
        if self.is_compact || self.config.indent.is_empty() {
            return;
        }
        for _ in 0..self.indent_level {
            self.output.push_str(&self.config.indent);
        }
    }

    fn write_colon(&mut self) {
        match self.config.spacing {
            Spacing::None => self.output.push(':'),
            Spacing::Normal => self.output.push_str(": "),
        }
    }

    fn write_separator(&mut self) {
        match self.config.spacing {
            Spacing::None => self.output.push(','),
            Spacing::Normal => self.output.push_str(", "),
        }
    }

    /// Format leading comments (comments that appear before an item on their own lines).
    fn format_leading_comments(&mut self, trivia: &Trivia<'_>) {
        // Delete mode strips all comments
        if self.config.comments == CommentMode::Delete {
            return;
        }
        if trivia.comments.is_empty() {
            return;
        }
        // In compact mode, format comments inline with conversion
        if self.is_compact {
            self.format_trivia_compact(trivia);
            return;
        }
        for comment in &trivia.comments {
            self.format_comment(comment);
        }
    }

    /// Format leading comments inline (for comments after colons, don't add newlines).
    fn format_leading_comments_inline(&mut self, trivia: &Trivia<'_>) {
        // Delete mode strips all comments
        if self.config.comments == CommentMode::Delete {
            return;
        }
        if trivia.comments.is_empty() {
            return;
        }
        // In compact mode, use compact comment formatting
        if self.is_compact {
            self.format_trivia_compact(trivia);
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
                    // they'll force a newline (only if we're not in minimal-like mode)
                    if self.is_pretty() {
                        self.output.push_str("  ");
                        self.output.push_str(&comment.text);
                    }
                }
            }
        }
    }

    /// Format trailing inline comments (comments on the same line after a value).
    fn format_trailing_inline_comment(&mut self, trivia: &Trivia<'_>) {
        // Delete mode strips all comments
        if self.config.comments == CommentMode::Delete {
            return;
        }
        if trivia.comments.is_empty() {
            return;
        }
        // In compact mode, use compact comment formatting
        if self.is_compact {
            self.format_trivia_compact(trivia);
            return;
        }
        for comment in &trivia.comments {
            match comment.kind {
                CommentKind::Line => {
                    // Line comments only in pretty mode (they need newlines)
                    if self.is_pretty() {
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

    /// Format a comment in compact mode, converting line comments to block comments.
    ///
    /// Line comments `// foo\n` become `/* foo */` (no newline).
    fn format_comment_compact(&mut self, comment: &Comment<'_>) {
        match comment.kind {
            CommentKind::Line => {
                // Convert: "// foo\n" → "/* foo */"
                let text = comment
                    .text
                    .trim_start_matches("//")
                    .trim_end_matches('\n')
                    .trim();
                self.output.push_str("/* ");
                self.output.push_str(text);
                self.output.push_str(" */");
            }
            CommentKind::Block => {
                self.output.push_str(&comment.text);
            }
        }
    }

    /// Format trivia (comments) in compact mode.
    ///
    /// All comments are formatted inline with space separators,
    /// and line comments are converted to block comments.
    fn format_trivia_compact(&mut self, trivia: &Trivia<'_>) {
        for comment in &trivia.comments {
            self.output.push(' ');
            self.format_comment_compact(comment);
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

    // =========================================================================
    // Compaction - Depth-based
    // =========================================================================

    #[test]
    fn test_compact_from_depth_0() {
        // Compact from depth 0 means compact even the root (unless root which is always multiline)
        let config = FormatConfig::new().compact_from_depth(0);
        // Nested collections should be compacted
        let source = "Config(items: [1, 2, 3], point: (1, 2))";
        let formatted = format_with(source, &config);
        // Root is still multiline, but nested should be compact
        assert!(formatted.contains("[1, 2, 3]"));
        assert!(formatted.contains("(1, 2)"));
    }

    #[test]
    fn test_compact_from_depth_1() {
        // Compact from depth 1 means first level nested compacts
        let config = FormatConfig::new().compact_from_depth(1);
        let source = "Config(items: [1, 2, 3])";
        let formatted = format_with(source, &config);
        // Root multiline, nested compact
        assert!(
            formatted.contains("items: [1, 2, 3]"),
            "Expected compact array: {formatted:?}"
        );
    }

    // =========================================================================
    // Compaction - Type-based
    // =========================================================================

    #[test]
    fn test_compact_types_arrays() {
        let config = FormatConfig::new()
            .char_limit(5) // Would normally expand due to length
            .compact_types(CompactTypes { arrays: true, ..Default::default() });
        let source = "Config(items: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])";
        let formatted = format_with(source, &config);
        // Arrays should be compact despite exceeding char limit
        assert!(
            formatted.contains("[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]"),
            "Expected compact array: {formatted:?}"
        );
    }

    #[test]
    fn test_compact_types_tuples() {
        let config = FormatConfig::new()
            .char_limit(5)
            .compact_types(CompactTypes {
                tuples: true,
                ..Default::default()
            });
        let source = "Config(point: (1, 2, 3, 4, 5, 6, 7, 8, 9, 10))";
        let formatted = format_with(source, &config);
        // Tuples should be compact
        assert!(
            formatted.contains("(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)"),
            "Expected compact tuple: {formatted:?}"
        );
    }

    #[test]
    fn test_compact_types_structs() {
        let config = FormatConfig::new()
            .char_limit(5)
            .compact_types(CompactTypes {
                structs: true,
                ..Default::default()
            });
        let source = "Outer(inner: Inner(x: 1, y: 2, z: 3))";
        let formatted = format_with(source, &config);
        // Nested struct should be compact
        assert!(
            formatted.contains("Inner(x: 1, y: 2, z: 3)"),
            "Expected compact struct: {formatted:?}"
        );
    }

    #[test]
    fn test_compact_types_all() {
        let config = FormatConfig::new()
            .char_limit(5)
            .compact_types(CompactTypes::all());
        let source = "Config(a: [1, 2], b: (3, 4), c: Point(x: 5))";
        let formatted = format_with(source, &config);
        assert!(formatted.contains("[1, 2]"));
        assert!(formatted.contains("(3, 4)"));
        assert!(formatted.contains("Point(x: 5)"));
    }

    // =========================================================================
    // Compaction - Comment conversion
    // =========================================================================

    #[test]
    fn test_compact_converts_line_comments_to_block() {
        // When compacting with a line comment, it should convert to block comment
        let config = FormatConfig::new().compact_from_depth(1);
        let source = "Config(
    items: [
        // comment
        1,
        2,
    ],
)";
        let formatted = format_with(source, &config);
        // In compact mode, line comment should become block comment
        assert!(
            formatted.contains("/* comment */"),
            "Expected block comment: {formatted:?}"
        );
        // Should not have newlines in the compact section
        assert!(formatted.contains("items: ["));
    }

    #[test]
    fn test_compact_preserves_block_comments() {
        let config = FormatConfig::new().compact_from_depth(1);
        let source = "Config(
    items: [
        /* existing block */
        1,
    ],
)";
        let formatted = format_with(source, &config);
        assert!(
            formatted.contains("/* existing block */"),
            "Expected block comment preserved: {formatted:?}"
        );
    }

    // =========================================================================
    // AR-1: Minimal Mode
    // =========================================================================

    #[test]
    fn test_minimal_no_whitespace() {
        // AR-1.1, AR-1.3, AR-1.4: No whitespace between tokens
        let source = "Point(x: 1, y: 2)";
        let formatted = format_with(source, &FormatConfig::minimal());
        assert_eq!(formatted, "Point(x:1,y:2)");
    }

    #[test]
    fn test_minimal_strips_comments() {
        // AR-1.2: Comments are stripped
        let source = "// header\nPoint(x: 1, /* inline */ y: 2)";
        let formatted = format_with(source, &FormatConfig::minimal());
        assert_eq!(formatted, "Point(x:1,y:2)");
    }

    #[test]
    fn test_minimal_nested_collections() {
        // AR-1.1: Nested collections also have no whitespace
        let source = "Config(items: [1, 2], point: (3, 4), map: {\"a\": 1})";
        let formatted = format_with(source, &FormatConfig::minimal());
        assert_eq!(formatted, "Config(items:[1,2],point:(3,4),map:{\"a\":1})");
    }

    // =========================================================================
    // AR-2: Root Collection Behavior
    // =========================================================================

    #[test]
    fn test_root_multiline_by_default() {
        // AR-2.1: Root collections are multiline by default
        let config = FormatConfig::new(); // No compaction rules
        let formatted = format_with("[1, 2, 3]", &config);
        assert!(
            formatted.contains('\n'),
            "Root should be multiline: {formatted:?}"
        );
    }

    #[test]
    fn test_root_compacts_with_depth_0() {
        // AR-2.2: compact_from_depth(0) forces root to be compact
        let config = FormatConfig::new().compact_from_depth(0);
        let formatted = format_with("[1, 2, 3]", &config);
        assert_eq!(formatted, "[1, 2, 3]\n");
    }

    #[test]
    fn test_root_compacts_with_matching_type() {
        // AR-2.3: compact_types matching root forces root to be compact
        let config = FormatConfig::new().compact_types(CompactTypes {
            arrays: true,
            ..Default::default()
        });
        let formatted = format_with("[1, 2, 3]", &config);
        assert_eq!(formatted, "[1, 2, 3]\n");
    }

    // =========================================================================
    // AR-3: Depth Counting
    // =========================================================================

    #[test]
    fn test_depth_counting_nested() {
        // AR-3.3, AR-3.4: Depth increments for each nested collection
        // depth 0 = root, depth 1 = first nested, depth 2 = second nested
        let config = FormatConfig::new().compact_from_depth(2).char_limit(0);
        let source = "Outer(a: Inner(b: [1, 2, 3]))";
        let formatted = format_with(source, &config);
        // Root (depth 0): multiline
        // Inner (depth 1): multiline (below threshold)
        // Array (depth 2): compact (at threshold)
        assert!(
            formatted.contains("[1, 2, 3]"),
            "Array should be compact: {formatted:?}"
        );
        assert!(
            formatted.contains("a: Inner("),
            "Should have newlines: {formatted:?}"
        );
    }

    #[test]
    fn test_compact_from_depth_2() {
        // AR-3.1: compact_from_depth(N) compacts at depth >= N
        let config = FormatConfig::new().compact_from_depth(2).char_limit(0);
        let source = "Config(items: [1, 2, 3])";
        let formatted = format_with(source, &config);
        // Root (depth 0): multiline
        // Array (depth 1): still multiline (1 < 2)
        assert!(
            formatted.contains("items: [\n"),
            "Depth 1 should be multiline: {formatted:?}"
        );
    }

    #[test]
    fn test_compact_from_depth_3_deep_nesting() {
        // AR-3.4: Deep nesting respects depth threshold
        let config = FormatConfig::new().compact_from_depth(3).char_limit(0);
        let source = "A(b: B(c: C(d: [1, 2])))";
        let formatted = format_with(source, &config);
        // Root A (depth 0): multiline
        // B (depth 1): multiline
        // C (depth 2): multiline
        // Array (depth 3): compact
        assert!(
            formatted.contains("[1, 2]"),
            "Depth 3 array should be compact: {formatted:?}"
        );
    }

    // =========================================================================
    // AR-4: Type-Based Compaction
    // =========================================================================

    #[test]
    fn test_compact_types_maps() {
        // AR-4.3: compact_types.maps compacts all {...} expressions
        let config = FormatConfig::new()
            .char_limit(0) // Disable length-based
            .compact_types(CompactTypes {
                maps: true,
                ..Default::default()
            });
        let source = "Config(data: {\"a\": 1, \"b\": 2})";
        let formatted = format_with(source, &config);
        assert!(
            formatted.contains("{\"a\": 1, \"b\": 2}"),
            "Map should be compact: {formatted:?}"
        );
    }

    #[test]
    fn test_compact_types_anonymous_struct() {
        // AR-4.4: compact_types.structs compacts (x: 1) anonymous structs
        let config = FormatConfig::new()
            .char_limit(0)
            .compact_types(CompactTypes {
                structs: true,
                ..Default::default()
            });
        let source = "Config(point: (x: 1, y: 2))";
        let formatted = format_with(source, &config);
        assert!(
            formatted.contains("(x: 1, y: 2)"),
            "Anonymous struct should be compact: {formatted:?}"
        );
    }

    #[test]
    fn test_compact_types_ignores_char_limit() {
        // AR-4.5: Type-based compaction ignores char_limit
        let config = FormatConfig::new()
            .char_limit(5) // Very small limit
            .compact_types(CompactTypes {
                arrays: true,
                ..Default::default()
            });
        let source = "Config(items: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])";
        let formatted = format_with(source, &config);
        // Array should be compact despite exceeding char_limit
        assert!(
            formatted.contains("[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]"),
            "Type rule should override char_limit: {formatted:?}"
        );
    }

    // =========================================================================
    // AR-5: Length-Based Compaction
    // =========================================================================

    #[test]
    fn test_char_limit_zero_disables() {
        // AR-5.3: char_limit = 0 disables length-based compaction
        let config = FormatConfig::new().char_limit(0);
        let source = "Config(items: [1])"; // Very short
        let formatted = format_with(source, &config);
        // Nested should be multiline since no length-based compaction
        assert!(
            formatted.contains("items: [\n"),
            "char_limit=0 should disable length-based: {formatted:?}"
        );
    }

    #[test]
    fn test_default_char_limit_80() {
        // AR-5.4: Default char_limit is 80
        let config = FormatConfig::default();
        // 75 chars fits in 80
        let short = "Config(items: [1, 2, 3, 4, 5])";
        let formatted_short = format_with(short, &config);
        assert!(
            formatted_short.contains("[1, 2, 3, 4, 5]"),
            "Should fit: {formatted_short:?}"
        );
    }

    // =========================================================================
    // AR-6: OR Logic (Rule Combination)
    // =========================================================================

    #[test]
    fn test_or_logic_depth_triggers() {
        // AR-6.1: Compaction triggers if depth rule matches
        let config = FormatConfig::new().compact_from_depth(1).char_limit(0); // Disable length
        let source = "Config(items: [1, 2, 3])";
        let formatted = format_with(source, &config);
        assert!(
            formatted.contains("[1, 2, 3]"),
            "Depth rule should trigger: {formatted:?}"
        );
    }

    #[test]
    fn test_or_logic_type_triggers() {
        // AR-6.2: Compaction triggers if type rule matches
        let config = FormatConfig::new()
            .compact_types(CompactTypes {
                arrays: true,
                ..Default::default()
            })
            .char_limit(0); // Disable length
        let source = "Config(items: [1, 2, 3])";
        let formatted = format_with(source, &config);
        assert!(
            formatted.contains("[1, 2, 3]"),
            "Type rule should trigger: {formatted:?}"
        );
    }

    #[test]
    fn test_or_logic_length_triggers() {
        // AR-6.3: Compaction triggers if length rule matches
        let config = FormatConfig::new().char_limit(100); // No depth/type rules
        let source = "Config(items: [1, 2, 3])";
        let formatted = format_with(source, &config);
        assert!(
            formatted.contains("[1, 2, 3]"),
            "Length rule should trigger: {formatted:?}"
        );
    }

    #[test]
    fn test_no_rules_match_multiline() {
        // AR-6.4: When NO rule matches, collection uses multiline
        let config = FormatConfig::new().char_limit(0); // Disable all
        let source = "Config(items: [1, 2, 3])";
        let formatted = format_with(source, &config);
        assert!(
            formatted.contains("items: [\n"),
            "No rules = multiline: {formatted:?}"
        );
    }

    // =========================================================================
    // AR-7: Soft/Hard Compaction & Comments
    // =========================================================================

    #[test]
    fn test_length_based_soft_line_comments_prevent() {
        // AR-7.1: Length-based compaction is "soft" - line comments prevent it
        let config = FormatConfig::new().char_limit(1000); // Large limit
        let source = "Config(
    items: [
        // comment
        1,
        2,
    ],
)";
        let formatted = format_with(source, &config);
        // Line comment should PREVENT compact mode (stay multiline)
        assert!(
            formatted.contains("// comment"),
            "Line comment preserved: {formatted:?}"
        );
        assert!(
            formatted.contains("items: [\n"),
            "Should stay multiline: {formatted:?}"
        );
    }

    #[test]
    fn test_depth_based_hard_converts_comments() {
        // AR-7.2, AR-7.4: Depth-based is "hard" - converts line comments to block
        let config = FormatConfig::new().compact_from_depth(1);
        let source = "Config(
    items: [
        // comment
        1,
    ],
)";
        let formatted = format_with(source, &config);
        // Hard compaction converts line comment to block
        assert!(
            formatted.contains("/* comment */"),
            "Line → block: {formatted:?}"
        );
        assert!(
            !formatted.contains("// comment"),
            "No line comment: {formatted:?}"
        );
    }

    #[test]
    fn test_type_based_hard_converts_comments() {
        // AR-7.3, AR-7.4: Type-based is "hard" - converts line comments to block
        let config = FormatConfig::new()
            .char_limit(0)
            .compact_types(CompactTypes {
                arrays: true,
                ..Default::default()
            });
        let source = "Config(
    items: [
        // comment
        1,
    ],
)";
        let formatted = format_with(source, &config);
        // Hard compaction converts line comment to block
        assert!(
            formatted.contains("/* comment */"),
            "Line → block: {formatted:?}"
        );
    }

    #[test]
    fn test_compact_multiple_line_comments() {
        // AR-7.7: Multiple comments in compact mode are space-separated
        let config = FormatConfig::new().compact_from_depth(1);
        let source = "Config(
    items: [
        // first
        1,
        // second
        2,
    ],
)";
        let formatted = format_with(source, &config);
        assert!(
            formatted.contains("/* first */"),
            "First comment: {formatted:?}"
        );
        assert!(
            formatted.contains("/* second */"),
            "Second comment: {formatted:?}"
        );
    }

    #[test]
    fn test_minimal_strips_all_comments() {
        // AR-7.8: Minimal mode strips ALL comments
        let source = "Config(
    // line comment
    items: [
        /* block comment */
        1,
    ],
)";
        let formatted = format_with(source, &FormatConfig::minimal());
        assert!(
            !formatted.contains("comment"),
            "No comments in minimal: {formatted:?}"
        );
    }

    #[test]
    fn test_hard_compact_block_comments_preserved() {
        // AR-7.5: Block comments are preserved unchanged in hard compact
        let config = FormatConfig::new().compact_from_depth(1);
        let source = "Config(
    items: [
        /* existing block */
        1,
    ],
)";
        let formatted = format_with(source, &config);
        assert!(
            formatted.contains("/* existing block */"),
            "Block comment unchanged: {formatted:?}"
        );
    }
}
