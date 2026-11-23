//! Formatting of RON documents and expressions.
//!
//! - [`format_document`] formats a parsed [`Document`]: comments, attributes and a
//!   trailing newline (in pretty mode).
//! - [`format_expr`] formats a single expression, e.g. one built with
//!   [`value_to_expr`](crate::ast::value_to_expr).
//!
//! [`FormatConfig`] controls indentation, spacing, comment handling and when collections
//! are written on a single line (see [`Compaction`]).
//!
//! # Example
//!
//! ```
//! use ron2::ast::parse_document;
//! use ron2::fmt::{FormatConfig, format_document};
//!
//! let doc = parse_document("Config(x:1,y:2)").unwrap();
//!
//! // In pretty mode the root collection is always multiline.
//! let pretty = format_document(&doc, &FormatConfig::default());
//! assert_eq!(pretty, "Config(\n    x: 1,\n    y: 2,\n)\n");
//!
//! let minimal = format_document(&doc, &FormatConfig::minimal());
//! assert_eq!(minimal, "Config(x:1,y:2)");
//! ```

use alloc::{
    collections::BTreeMap,
    string::{String, ToString},
};

use crate::{
    ast::{
        Comment, CommentKind, Document, Expr, MapEntry, OptionExpr, SeqItem, StructBody,
        StructExpr, StructField, Trivia, TupleElement,
    },
    error::Span,
};

/// Controls how RON output is formatted.
#[derive(Clone, Debug)]
pub struct FormatConfig {
    /// Indentation for one nesting level.
    ///
    /// A non-empty indent selects *pretty* mode: the root collection is written multiline
    /// (unless a depth or type rule compacts it) and documents end with a newline.
    pub indent: String,

    /// Spacing style after `:` and `,` on a single line.
    pub spacing: Spacing,

    /// How to handle comments.
    pub comments: CommentMode,

    /// Rules for when to write a collection on a single line.
    pub compaction: Compaction,
}

/// Spacing style after `:` and `,`.
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
    /// Delete all comments.
    Delete,
    /// Preserve comments. A line comment keeps its collection multiline, even when a
    /// depth or type rule asks for compaction.
    Preserve,
    /// Preserve comments. A line comment keeps its collection multiline for length-based
    /// compaction; when a depth or type rule compacts it, `// foo` becomes `/* foo */`.
    /// Line comments whose text contains `/*` or `*/` cannot be converted (block
    /// comments nest) and keep the collection multiline.
    #[default]
    Auto,
}

/// Rules for when a collection is written on a single line ("compact").
///
/// A collection is compacted when ANY of these holds:
/// - its depth is at least `compact_from_depth` (hard rule)
/// - its type is in `compact_types` (hard rule)
/// - its line fits within `char_limit` (soft rule)
///
/// In pretty mode the root collection only follows the hard rules. Comments can veto
/// compaction, see [`CommentMode`]; block comments spanning several lines always do
/// (except in [`CommentMode::Delete`]).
#[derive(Clone, Debug)]
pub struct Compaction {
    /// Maximum line width in characters for length-based compaction.
    ///
    /// A collection is written on one line if the whole line — indentation, field name
    /// and trailing comma included — stays within this width. Lines can still be longer
    /// when a single item does not fit. `0` disables length-based compaction.
    ///
    /// Default: 80
    pub char_limit: usize,

    /// Depth from which collections are always compacted.
    /// - `None`: no depth-based compaction (default)
    /// - `Some(0)`: compact everything, including the root
    /// - `Some(1)`: compact the first nesting level and deeper
    pub compact_from_depth: Option<usize>,

    /// Collection types that are always compacted. Default: none.
    pub compact_types: CompactTypes,
}

/// Collection types that can be marked for automatic compaction.
#[derive(Clone, Debug, Default)]
#[allow(clippy::struct_excessive_bools)]
pub struct CompactTypes {
    /// Tuples: `(1, 2, 3)` and `Point(1, 2, 3)`
    pub tuples: bool,
    /// Sequences: `[1, 2, 3]`
    pub arrays: bool,
    /// Maps: `{"a": 1}`
    pub maps: bool,
    /// Structs with named fields: `Point(x: 1, y: 2)` and `(x: 1, y: 2)`
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
            char_limit: 80,
            compact_from_depth: None,
            compact_types: CompactTypes::default(),
        }
    }
}

impl CompactTypes {
    /// All collection types.
    #[must_use]
    pub fn all() -> Self {
        Self {
            tuples: true,
            arrays: true,
            maps: true,
            structs: true,
        }
    }

    /// No collection types (default).
    #[must_use]
    pub fn none() -> Self {
        Self::default()
    }
}

impl FormatConfig {
    /// Pretty output: 4-space indent, normal spacing, [`CommentMode::Auto`], 80 character
    /// line width. Same as [`FormatConfig::default`].
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Most compact output: no whitespace, no comments, everything on one line.
    ///
    /// Output example: `Point(x:1,y:2)`
    #[must_use]
    pub fn minimal() -> Self {
        Self {
            indent: String::new(),
            spacing: Spacing::None,
            comments: CommentMode::Delete,
            compaction: Compaction {
                char_limit: 0,
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

    /// Set the maximum line width for length-based compaction (`0` disables it).
    #[must_use]
    pub fn char_limit(mut self, char_limit: usize) -> Self {
        self.compaction.char_limit = char_limit;
        self
    }

    /// Set the depth from which collections are always compacted.
    #[must_use]
    pub fn compact_from_depth(mut self, depth: usize) -> Self {
        self.compaction.compact_from_depth = Some(depth);
        self
    }

    /// Set the collection types that are always compacted.
    #[must_use]
    pub fn compact_types(mut self, types: CompactTypes) -> Self {
        self.compaction.compact_types = types;
        self
    }

    fn is_pretty(&self) -> bool {
        !self.indent.is_empty()
    }

    /// Whether a depth or type rule compacts this collection.
    fn wants_compact(&self, kind: CollectionKind, depth: usize) -> bool {
        let rules = &self.compaction;
        rules.compact_from_depth.is_some_and(|from| depth >= from)
            || match kind {
                CollectionKind::Tuple => rules.compact_types.tuples,
                CollectionKind::Array => rules.compact_types.arrays,
                CollectionKind::Map => rules.compact_types.maps,
                CollectionKind::Struct => rules.compact_types.structs,
            }
    }

    /// The comments of `trivia` that appear in the output.
    fn visible<'t, 'a>(&self, trivia: &'t Trivia<'a>) -> &'t [Comment<'a>] {
        if self.comments == CommentMode::Delete {
            &[]
        } else {
            &trivia.comments
        }
    }
}

/// Format a RON document.
///
/// Comments are kept according to [`FormatConfig::comments`]. In pretty mode (non-empty
/// indent) the root collection is multiline and the output ends with a newline.
///
/// # Example
///
/// ```
/// use ron2::ast::parse_document;
/// use ron2::fmt::{FormatConfig, format_document};
///
/// let doc = parse_document("Config(port:8080, // the port\nhost:\"localhost\")").unwrap();
/// let formatted = format_document(&doc, &FormatConfig::default());
/// assert_eq!(
///     formatted,
///     "Config(\n    port: 8080, // the port\n    host: \"localhost\",\n)\n"
/// );
/// ```
#[must_use]
pub fn format_document(doc: &Document<'_>, config: &FormatConfig) -> String {
    let mut formatter = Formatter::new(config);
    formatter.document(doc);
    formatter.out
}

/// Format a single RON expression.
///
/// Unlike [`format_document`], there are no attributes and no trailing newline. The root
/// collection follows the same rules as in [`format_document`].
///
/// # Example
///
/// ```
/// use ron2::ast::value_to_expr;
/// use ron2::fmt::{FormatConfig, format_expr};
/// use ron2::Value;
///
/// let expr = value_to_expr(Value::Seq(vec![Value::from(1), Value::from(2)]));
/// assert_eq!(format_expr(&expr, &FormatConfig::minimal()), "[1,2]");
/// assert_eq!(format_expr(&expr, &FormatConfig::default()), "[\n    1,\n    2,\n]");
/// ```
#[must_use]
pub fn format_expr(expr: &Expr<'_>, config: &FormatConfig) -> String {
    let mut formatter = Formatter::new(config);
    formatter.expr(expr, 0, 0);
    formatter.out
}

// ============================================================================
// Uniform view of expressions
// ============================================================================

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum CollectionKind {
    Tuple,
    Array,
    Map,
    Struct,
}

/// What the formatter needs to know about an expression.
enum Shape<'x, 'a> {
    /// A single token.
    Scalar(&'x str),
    Option(&'x OptionExpr<'a>),
    Named(&'x StructExpr<'a>),
    Collection(Collection<'x, 'a>),
}

impl<'x, 'a> Shape<'x, 'a> {
    fn of(expr: &'x Expr<'a>) -> Self {
        let collection = |kind, open, close, leading, items, trailing| {
            Shape::Collection(Collection {
                kind,
                open,
                close,
                leading,
                items,
                trailing,
            })
        };
        match expr {
            Expr::Unit(_) => Shape::Scalar("()"),
            Expr::Bool(b) => Shape::Scalar(if b.value { "true" } else { "false" }),
            Expr::Char(c) => Shape::Scalar(&c.raw),
            Expr::Byte(b) => Shape::Scalar(&b.raw),
            Expr::Number(n) => Shape::Scalar(&n.raw),
            Expr::String(s) => Shape::Scalar(&s.raw),
            Expr::Bytes(b) => Shape::Scalar(&b.raw),
            Expr::Error(_) => Shape::Scalar("/* parse error */"),
            Expr::Option(opt) => Shape::Option(opt),
            Expr::Struct(s) => Shape::Named(s),
            Expr::Seq(s) => collection(
                CollectionKind::Array,
                "[",
                "]",
                &s.leading,
                Items::Seq(&s.items),
                &s.trailing,
            ),
            Expr::Map(m) => collection(
                CollectionKind::Map,
                "{",
                "}",
                &m.leading,
                Items::Map(&m.entries),
                &m.trailing,
            ),
            Expr::Tuple(t) => collection(
                CollectionKind::Tuple,
                "(",
                ")",
                &t.leading,
                Items::Tuple(&t.elements),
                &t.trailing,
            ),
            Expr::AnonStruct(s) => collection(
                CollectionKind::Struct,
                "(",
                ")",
                &s.leading,
                Items::Fields(&s.fields),
                &s.trailing,
            ),
        }
    }
}

/// A bracketed list of items, whatever its syntax.
#[derive(Clone, Copy)]
struct Collection<'x, 'a> {
    kind: CollectionKind,
    open: &'static str,
    close: &'static str,
    leading: &'x Trivia<'a>,
    items: Items<'x, 'a>,
    trailing: &'x Trivia<'a>,
}

impl<'x, 'a> Collection<'x, 'a> {
    fn of_body(body: &'x StructBody<'a>) -> Self {
        match body {
            StructBody::Tuple(t) => Self {
                kind: CollectionKind::Tuple,
                open: "(",
                close: ")",
                leading: &t.leading,
                items: Items::Tuple(&t.elements),
                trailing: &t.trailing,
            },
            StructBody::Fields(f) => Self {
                kind: CollectionKind::Struct,
                open: "(",
                close: ")",
                leading: &f.leading,
                items: Items::Fields(&f.fields),
                trailing: &f.trailing,
            },
        }
    }
}

#[derive(Clone, Copy)]
enum Items<'x, 'a> {
    Seq(&'x [SeqItem<'a>]),
    Tuple(&'x [TupleElement<'a>]),
    Map(&'x [MapEntry<'a>]),
    Fields(&'x [StructField<'a>]),
}

impl<'x, 'a> Items<'x, 'a> {
    fn len(self) -> usize {
        match self {
            Items::Seq(items) => items.len(),
            Items::Tuple(items) => items.len(),
            Items::Map(items) => items.len(),
            Items::Fields(items) => items.len(),
        }
    }

    fn is_empty(self) -> bool {
        self.len() == 0
    }

    fn get(self, index: usize) -> Option<Item<'x, 'a>> {
        match self {
            Items::Seq(items) => items.get(index).map(|item| Item {
                leading: &item.leading,
                start: item.expr.span(),
                key: None,
                value: &item.expr,
                trailing: &item.trailing,
            }),
            Items::Tuple(items) => items.get(index).map(|element| Item {
                leading: &element.leading,
                start: element.expr.span(),
                key: None,
                value: &element.expr,
                trailing: &element.trailing,
            }),
            Items::Map(items) => items.get(index).map(|entry| Item {
                leading: &entry.leading,
                start: entry.key.span(),
                key: Some((Key::Expr(&entry.key), &entry.pre_colon, &entry.post_colon)),
                value: &entry.value,
                trailing: &entry.trailing,
            }),
            Items::Fields(items) => items.get(index).map(|field| Item {
                leading: &field.leading,
                start: &field.name.span,
                key: Some((
                    Key::Ident(&field.name.name),
                    &field.pre_colon,
                    &field.post_colon,
                )),
                value: &field.value,
                trailing: &field.trailing,
            }),
        }
    }

    fn iter(self) -> impl Iterator<Item = Item<'x, 'a>> {
        (0..self.len()).map_while(move |index| self.get(index))
    }
}

#[derive(Clone, Copy)]
enum Key<'x, 'a> {
    Ident(&'x str),
    Expr(&'x Expr<'a>),
}

/// One item of a collection: `value` or `key: value`, with its trivia.
#[derive(Clone, Copy)]
struct Item<'x, 'a> {
    leading: &'x Trivia<'a>,
    /// Span of the item's first token.
    start: &'x Span,
    /// The key with the trivia before and after its colon.
    key: Option<(Key<'x, 'a>, &'x Trivia<'a>, &'x Trivia<'a>)>,
    value: &'x Expr<'a>,
    trailing: &'x Trivia<'a>,
}

// ============================================================================
// Single-line output
// ============================================================================

/// The expression cannot be written on one line because of a comment.
struct NotFlat;

/// Single-line widths of collections, keyed by the collection's address and whether
/// line comments may be converted inside it. `None`: a comment forbids a single line.
type Widths = BTreeMap<(usize, bool), Option<usize>>;

/// Destination of single-line output: the real output or a width probe.
///
/// Measuring and writing share one code path, so a measured width is exactly what gets
/// written.
trait Sink: Sized {
    fn put(&mut self, s: &str) -> Result<(), NotFlat>;

    /// Writes a nested collection.
    fn collection(
        &mut self,
        flat: Flat<'_>,
        c: &Collection<'_, '_>,
        depth: usize,
    ) -> Result<(), NotFlat> {
        flat.collection(self, c, depth)
    }
}

impl Sink for String {
    fn put(&mut self, s: &str) -> Result<(), NotFlat> {
        self.push_str(s);
        Ok(())
    }
}

/// Counts characters. Nested collections are measured through the memo, so each
/// collection is measured at most twice (with and without comment conversion) and
/// formatting stays linear in the input size regardless of nesting depth.
struct Measure<'m> {
    width: usize,
    widths: &'m mut Widths,
}

impl Sink for Measure<'_> {
    fn put(&mut self, s: &str) -> Result<(), NotFlat> {
        self.width = self.width.saturating_add(s.chars().count());
        Ok(())
    }

    fn collection(
        &mut self,
        flat: Flat<'_>,
        c: &Collection<'_, '_>,
        depth: usize,
    ) -> Result<(), NotFlat> {
        let width = flat.measure(self.widths, c, depth).ok_or(NotFlat)?;
        self.width = self.width.saturating_add(width);
        Ok(())
    }
}

/// Writes (or measures) expressions on a single line.
#[derive(Clone, Copy)]
struct Flat<'c> {
    config: &'c FormatConfig,
    /// A depth or type rule compacts this collection or an enclosing one, which allows
    /// converting line comments.
    hard: bool,
}

impl Flat<'_> {
    /// The context for collection `c` at `depth`: its own depth and type rules apply in
    /// addition to those of the enclosing collections.
    fn enter(self, c: &Collection<'_, '_>, depth: usize) -> Self {
        Self {
            hard: self.hard || self.config.wants_compact(c.kind, depth),
            ..self
        }
    }

    /// Single-line width of `c`, or `None` if a comment forbids a single line.
    fn measure(self, widths: &mut Widths, c: &Collection<'_, '_>, depth: usize) -> Option<usize> {
        let flat = self.enter(c, depth);
        let key = (core::ptr::from_ref(c.leading).addr(), flat.hard);
        if let Some(&width) = widths.get(&key) {
            return width;
        }
        let mut measure = Measure { width: 0, widths };
        let width = flat
            .collection(&mut measure, c, depth)
            .ok()
            .map(|()| measure.width);
        widths.insert(key, width);
        width
    }

    fn expr<S: Sink>(self, out: &mut S, expr: &Expr<'_>, depth: usize) -> Result<(), NotFlat> {
        match Shape::of(expr) {
            Shape::Scalar(text) => out.put(text),
            Shape::Option(opt) => match &opt.value {
                None => out.put("None"),
                Some(inner) => {
                    out.put("Some(")?;
                    self.comments_before(out, &inner.leading)?;
                    self.expr(out, &inner.expr, depth)?;
                    self.comments_after(out, &inner.trailing)?;
                    out.put(")")
                }
            },
            Shape::Named(s) => {
                out.put(&s.name.name)?;
                if !self.config.visible(&s.pre_body).is_empty() {
                    self.comments_after(out, &s.pre_body)?;
                    out.put(" ")?;
                }
                match &s.body {
                    Some(body) => out.collection(self, &Collection::of_body(body), depth),
                    None => Ok(()),
                }
            }
            Shape::Collection(collection) => out.collection(self, &collection, depth),
        }
    }

    fn collection<S: Sink>(
        self,
        out: &mut S,
        c: &Collection<'_, '_>,
        depth: usize,
    ) -> Result<(), NotFlat> {
        let flat = self.enter(c, depth);
        out.put(c.open)?;
        flat.comments_before(out, c.leading)?;
        let separator = match flat.config.spacing {
            Spacing::None => ",",
            Spacing::Normal => ", ",
        };
        for (i, item) in c.items.iter().enumerate() {
            if i > 0 {
                out.put(separator)?;
            }
            flat.item(out, item, depth)?;
        }
        if c.items.is_empty() {
            for (i, comment) in flat.config.visible(c.trailing).iter().enumerate() {
                if i > 0 {
                    out.put(" ")?;
                }
                flat.comment(out, comment)?;
            }
        } else {
            flat.comments_after(out, c.trailing)?;
        }
        out.put(c.close)
    }

    fn item<S: Sink>(self, out: &mut S, item: Item<'_, '_>, depth: usize) -> Result<(), NotFlat> {
        self.comments_before(out, item.leading)?;
        if let Some((key, pre_colon, post_colon)) = item.key {
            match key {
                Key::Ident(name) => out.put(name)?,
                Key::Expr(expr) => self.expr(out, expr, depth + 1)?,
            }
            self.comments_after(out, pre_colon)?;
            out.put(":")?;
            self.comments_after(out, post_colon)?;
            if self.config.spacing == Spacing::Normal || !self.config.visible(post_colon).is_empty()
            {
                out.put(" ")?;
            }
        }
        self.expr(out, item.value, depth + 1)?;
        self.comments_after(out, item.trailing)
    }

    /// Comments placed before a token: `/* c */ token`.
    fn comments_before<S: Sink>(self, out: &mut S, trivia: &Trivia<'_>) -> Result<(), NotFlat> {
        for comment in self.config.visible(trivia) {
            self.comment(out, comment)?;
            out.put(" ")?;
        }
        Ok(())
    }

    /// Comments placed after a token: `token /* c */`.
    fn comments_after<S: Sink>(self, out: &mut S, trivia: &Trivia<'_>) -> Result<(), NotFlat> {
        for comment in self.config.visible(trivia) {
            out.put(" ")?;
            self.comment(out, comment)?;
        }
        Ok(())
    }

    fn comment<S: Sink>(self, out: &mut S, comment: &Comment<'_>) -> Result<(), NotFlat> {
        match comment.kind {
            CommentKind::Block if comment.text.contains('\n') => Err(NotFlat),
            CommentKind::Block => out.put(&comment.text),
            CommentKind::Line => {
                if !self.hard || self.config.comments != CommentMode::Auto {
                    return Err(NotFlat);
                }
                let text = comment
                    .text
                    .strip_prefix("//")
                    .unwrap_or(&comment.text)
                    .trim();
                // Block comments nest, so these would unbalance the converted comment.
                if text.contains("/*") || text.contains("*/") {
                    return Err(NotFlat);
                }
                if text.is_empty() {
                    return out.put("/* */");
                }
                out.put("/* ")?;
                out.put(text)?;
                out.put(" */")
            }
        }
    }
}

// ============================================================================
// Layout
// ============================================================================

/// Writes the output line by line.
///
/// Indentation, spaces and the newline that must follow a line comment are emitted
/// lazily by [`Formatter::prepare`] right before the next token. That guarantees that
/// nothing is ever written after a line comment on the same line, and that no line ends
/// in whitespace.
struct Formatter<'c> {
    config: &'c FormatConfig,
    out: String,
    indent: usize,
    /// Byte offset of the current line in `out`.
    line_start: usize,
    /// A space separates the next token from the previous one (dropped at line start).
    pending_space: bool,
    /// The current line is finished (it ends in a comment); the next token starts a new line.
    pending_newline: bool,
    widths: Widths,
}

impl<'c> Formatter<'c> {
    fn new(config: &'c FormatConfig) -> Self {
        Self {
            config,
            out: String::new(),
            indent: 0,
            line_start: 0,
            pending_space: false,
            pending_newline: false,
            widths: Widths::new(),
        }
    }

    // ------------------------------------------------------------------------
    // Low-level output
    // ------------------------------------------------------------------------

    fn newline(&mut self) {
        self.out.push('\n');
        self.line_start = self.out.len();
        self.pending_space = false;
        self.pending_newline = false;
    }

    /// Starts a new line unless the current one is still empty.
    fn ensure_line_start(&mut self) {
        if self.out.len() > self.line_start {
            self.newline();
        }
    }

    /// Emits what is due before the next token: a newline, indentation or a space.
    fn prepare(&mut self) {
        if self.pending_newline {
            self.newline();
        }
        if self.out.len() == self.line_start {
            for _ in 0..self.indent {
                self.out.push_str(&self.config.indent);
            }
        } else if self.pending_space {
            self.out.push(' ');
        }
        self.pending_space = false;
    }

    fn token(&mut self, text: &str) {
        self.prepare();
        self.out.push_str(text);
    }

    /// Current column in characters. Searches for the last newline rather than using
    /// `line_start`, because multi-line raw strings and block comments contain newlines.
    fn column(&self) -> usize {
        let line_start = self.out.rfind('\n').map_or(0, |newline| newline + 1);
        self.out
            .get(line_start..)
            .map_or(0, |line| line.chars().count())
    }

    // ------------------------------------------------------------------------
    // Comments
    // ------------------------------------------------------------------------

    fn comment(&mut self, comment: &Comment<'_>) {
        match comment.kind {
            CommentKind::Line => {
                self.token(comment.text.trim_end());
                self.pending_newline = true;
            }
            CommentKind::Block => self.token(&comment.text),
        }
    }

    /// A comment on the current line, after the previous token.
    fn comment_inline(&mut self, comment: &Comment<'_>) {
        self.pending_space = true;
        self.comment(comment);
    }

    /// A comment on a line of its own.
    fn comment_own_line(&mut self, comment: &Comment<'_>) {
        self.ensure_line_start();
        self.comment(comment);
        self.pending_newline = true;
    }

    /// Comments following a token: those that started on the token's line stay at the
    /// end of that line, the others get their own lines.
    fn comments_after_token(&mut self, trivia: &Trivia<'_>) {
        self.comments_after_token_until(trivia, usize::MAX);
    }

    /// Like [`Self::comments_after_token`], for the first `count` comments only.
    fn comments_after_token_until(&mut self, trivia: &Trivia<'_>, count: usize) {
        let same_line = same_line_count(trivia);
        for (i, comment) in self.config.visible(trivia).iter().take(count).enumerate() {
            if i < same_line {
                self.comment_inline(comment);
            } else {
                self.comment_own_line(comment);
            }
        }
    }

    // ------------------------------------------------------------------------
    // Document and expressions
    // ------------------------------------------------------------------------

    fn document(&mut self, doc: &Document<'_>) {
        for comment in self.config.visible(&doc.leading) {
            self.comment_own_line(comment);
        }
        for attr in &doc.attributes {
            for comment in self.config.visible(&attr.leading) {
                self.comment_own_line(comment);
            }
            self.ensure_line_start();
            self.token(&attr.to_string());
            self.pending_newline = true;
        }
        if self.config.is_pretty() && !doc.attributes.is_empty() && doc.value.is_some() {
            self.ensure_line_start();
            self.newline();
        }
        for comment in self.config.visible(&doc.pre_value) {
            self.comment_own_line(comment);
        }
        if let Some(value) = &doc.value {
            self.expr(value, 0, 0);
            self.comments_after_token(&doc.trailing);
        } else {
            for comment in self.config.visible(&doc.trailing) {
                self.comment_own_line(comment);
            }
        }
        if self.config.is_pretty() && !self.out.is_empty() {
            self.ensure_line_start();
        }
    }

    /// Writes `expr` at nesting `depth`; `suffix` is the width of what must follow it on
    /// the same line (e.g. the `,` after an item), for length-based compaction.
    fn expr(&mut self, expr: &Expr<'_>, depth: usize, suffix: usize) {
        match Shape::of(expr) {
            Shape::Scalar(text) => self.token(text),
            Shape::Option(opt) => self.option(opt, depth, suffix),
            Shape::Named(s) => {
                self.token(&s.name.name);
                let pre_body = self.config.visible(&s.pre_body);
                for comment in pre_body {
                    self.comment_inline(comment);
                }
                self.pending_space = !pre_body.is_empty();
                if let Some(body) = &s.body {
                    self.collection(&Collection::of_body(body), depth, suffix);
                }
            }
            Shape::Collection(collection) => self.collection(&collection, depth, suffix),
        }
    }

    fn option(&mut self, opt: &OptionExpr<'_>, depth: usize, suffix: usize) {
        let Some(inner) = &opt.value else {
            self.token("None");
            return;
        };
        let leading = self.config.visible(&inner.leading);
        let trailing = self.config.visible(&inner.trailing);
        self.token("Some(");
        if has_line_comment(leading) || has_line_comment(trailing) {
            self.indent += 1;
            self.comments_after_token(&inner.leading);
            self.ensure_line_start();
            self.expr(&inner.expr, depth, 0);
            for comment in trailing {
                self.comment_inline(comment);
            }
            self.indent -= 1;
            self.ensure_line_start();
        } else {
            for comment in leading {
                self.comment(comment);
                self.pending_space = true;
            }
            self.expr(&inner.expr, depth, suffix + 1 + inline_width(trailing));
            for comment in trailing {
                self.comment_inline(comment);
            }
        }
        self.token(")");
    }

    fn collection(&mut self, c: &Collection<'_, '_>, depth: usize, suffix: usize) {
        self.prepare();
        // Enclosing collections are multiline, so only this collection's rules apply.
        let flat = Flat {
            config: self.config,
            hard: false,
        };
        let limit = self.config.compaction.char_limit;
        let compact = if self.config.wants_compact(c.kind, depth) {
            flat.measure(&mut self.widths, c, depth).is_some()
        } else if (depth == 0 && self.config.is_pretty() && !c.items.is_empty()) || limit == 0 {
            false
        } else {
            let remaining = limit.saturating_sub(self.column() + suffix);
            flat.measure(&mut self.widths, c, depth)
                .is_some_and(|width| width <= remaining)
        };

        if compact {
            let written = flat.collection(&mut self.out, c, depth);
            debug_assert!(written.is_ok(), "measured collection must be writable");
        } else {
            self.multiline(c, depth);
        }
    }

    fn multiline(&mut self, c: &Collection<'_, '_>, depth: usize) {
        self.token(c.open);
        if c.items.is_empty()
            && self.config.visible(c.leading).is_empty()
            && self.config.visible(c.trailing).is_empty()
        {
            self.token(c.close);
            return;
        }
        self.indent += 1;
        self.comments_after_token(c.leading);
        for item in c.items.iter() {
            // Block comments on the item's own line stay in front of it; the others follow
            // the previous token.
            let leading = self.config.visible(item.leading);
            let attached = attached_count(leading, item.start);
            self.comments_after_token_until(item.leading, leading.len() - attached);
            self.ensure_line_start();
            for comment in &leading[leading.len() - attached..] {
                self.comment(comment);
                self.pending_space = true;
            }
            self.item(item, depth);
        }
        self.comments_after_token(c.trailing);
        self.indent -= 1;
        self.ensure_line_start();
        self.token(c.close);
    }

    fn item(&mut self, item: Item<'_, '_>, depth: usize) {
        // Block comments before the comma stay there; a line comment has to move after it.
        let trailing = self.config.visible(item.trailing);
        let split = trailing
            .iter()
            .position(|c| c.kind == CommentKind::Line)
            .unwrap_or(trailing.len());
        let (before_comma, after_comma) = trailing.split_at(split);

        if let Some((key, pre_colon, post_colon)) = item.key {
            match key {
                Key::Ident(name) => self.token(name),
                Key::Expr(expr) => self.expr(expr, depth + 1, 1),
            }
            for comment in self.config.visible(pre_colon) {
                self.comment_inline(comment);
            }
            self.token(":");
            let post_colon = self.config.visible(post_colon);
            for comment in post_colon {
                self.comment_inline(comment);
            }
            self.pending_space = self.config.spacing == Spacing::Normal || !post_colon.is_empty();
        }
        self.expr(item.value, depth + 1, inline_width(before_comma) + 1);
        for comment in before_comma {
            self.comment_inline(comment);
        }
        self.token(",");
        for (i, comment) in after_comma.iter().enumerate() {
            if i == 0 {
                self.comment_inline(comment);
            } else {
                self.comment_own_line(comment);
            }
        }
    }
}

/// Width of `comments` written inline after a token: ` /* a */ /* b */`.
fn inline_width(comments: &[Comment<'_>]) -> usize {
    comments.iter().map(|c| 1 + c.text.chars().count()).sum()
}

/// Number of block comments at the end of `comments` that end on the line where the
/// item starts, e.g. `/* a */` in `1, /* a */ 2`.
fn attached_count(comments: &[Comment<'_>], item_start: &Span) -> usize {
    if item_start.is_synthetic() {
        return 0;
    }
    comments
        .iter()
        .rev()
        .take_while(|c| {
            c.kind == CommentKind::Block
                && !c.span.is_synthetic()
                && c.span.end.line == item_start.start.line
        })
        .count()
}

fn has_line_comment(comments: &[Comment<'_>]) -> bool {
    comments.iter().any(|c| c.kind == CommentKind::Line)
}

/// Number of leading comments in `trivia` that start on the line where the trivia
/// starts, i.e. on the line of the preceding token.
fn same_line_count(trivia: &Trivia<'_>) -> usize {
    match trivia.span {
        Some(span) if !span.is_synthetic() => trivia
            .comments
            .iter()
            .take_while(|c| c.span.start.line == span.start.line)
            .count(),
        _ => 0,
    }
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
            .compact_types(CompactTypes {
                arrays: true,
                ..Default::default()
            });
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

    /// `Config(items: ["xx…"])` whose `items` line is `line_width` characters long.
    fn items_line(line_width: usize, fill: char) -> String {
        // `    items: ["` + fill + `"],`
        let fill: String = core::iter::repeat_n(fill, line_width - 16).collect();
        format!("Config(items: [\"{fill}\"])")
    }

    #[test]
    fn test_default_char_limit_80() {
        assert_eq!(FormatConfig::default().compaction.char_limit, 80);
        let fits = format(&items_line(80, 'x'));
        assert!(
            fits.lines()
                .any(|l| l.chars().count() == 80 && l.ends_with("\"],")),
            "{fits}"
        );
        let too_wide = format(&items_line(81, 'x'));
        assert!(too_wide.contains("items: [\n"), "{too_wide}");
    }

    #[test]
    fn char_limit_counts_characters_not_bytes() {
        let fits = format(&items_line(80, 'é'));
        assert!(!fits.contains("items: [\n"), "{fits}");
    }

    #[test]
    fn char_limit_counts_indentation_and_prefix() {
        // The array alone is 15 characters, its line is 27.
        let source = "Config(items: [1, 2, 3, 4, 5])";
        let config = FormatConfig::default().char_limit(26);
        assert!(format_with(source, &config).contains("items: [\n"));
        let config = FormatConfig::default().char_limit(27);
        assert!(format_with(source, &config).contains("    items: [1, 2, 3, 4, 5],\n"));
        // The struct name counts too: `    point: VeryLongName(1, 2),` is 30 characters.
        let source = "Config(point: VeryLongName(1, 2))";
        let config = FormatConfig::default().char_limit(29);
        assert!(format_with(source, &config).contains("point: VeryLongName(\n"));
        let config = FormatConfig::default().char_limit(30);
        assert!(format_with(source, &config).contains("point: VeryLongName(1, 2),\n"));
    }

    #[test]
    fn lines_stay_within_char_limit() {
        let source = "Config(short: [1, 2, 3], long: [\"aaaaaaaaaa\", \"bbbbbbbbbb\", \
                      \"cccccccccc\", \"dddddddddd\"], nested: (inner: (deep: [1111111111, \
                      2222222222, 3333333333, 4444444444])))";
        for width in [20, 40, 60, 80] {
            let formatted = format_with(source, &FormatConfig::default().char_limit(width));
            for line in formatted.lines() {
                // Only a line holding a single unbreakable item may be longer.
                assert!(
                    line.chars().count() <= width || !line.contains(['[', '(']),
                    "width {width}: {line:?} in\n{formatted}"
                );
            }
        }
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

    // =========================================================================
    // Line comments end their line
    // =========================================================================

    #[test]
    fn line_comment_before_comma_moves_after_it() {
        assert_eq!(
            format("[\n 1 // one\n , 2]"),
            "[\n    1, // one\n    2,\n]\n"
        );
        assert_eq!(format("[1 // c\n, 2]"), "[\n    1, // c\n    2,\n]\n");
    }

    #[test]
    fn line_comment_before_colon() {
        assert_eq!(format("(a // c\n : 1)"), "(\n    a // c\n    : 1,\n)\n");
    }

    #[test]
    fn line_comment_after_colon() {
        assert_eq!(format("(\n a: // c\n 1,\n)"), "(\n    a: // c\n    1,\n)\n");
    }

    #[test]
    fn line_comment_inside_some() {
        assert_eq!(
            format("(a: Some(1 // c\n))"),
            "(\n    a: Some(\n        1 // c\n    ),\n)\n"
        );
        assert_eq!(
            format("(a: Some( // c\n 1))"),
            "(\n    a: Some( // c\n        1\n    ),\n)\n"
        );
    }

    #[test]
    fn line_comment_in_empty_collection() {
        assert_eq!(
            format("(a: [ // c\n ], b: 1)"),
            "(\n    a: [ // c\n    ],\n    b: 1,\n)\n"
        );
    }

    #[test]
    fn end_of_line_comments_stay_at_end_of_line() {
        let source = "(\n port: 8080, // the port\n host: \"h\",\n)";
        assert_eq!(
            format(source),
            "(\n    port: 8080, // the port\n    host: \"h\",\n)\n"
        );
        assert_eq!(format("42 // answer"), "42 // answer\n");
        assert_eq!(format("(a: 1) /* x */"), "(\n    a: 1,\n) /* x */\n");
    }

    #[test]
    #[ignore = "needs W1 parser trailing-trivia fix"]
    fn end_of_line_comment_after_last_comma() {
        let source = "(\n port: 8080, // the port\n host: \"h\", // the host\n)";
        assert_eq!(
            format(source),
            "(\n    port: 8080, // the port\n    host: \"h\", // the host\n)\n"
        );
        assert_eq!(
            format("[\n 1,\n 2, // two\n]"),
            "[\n    1,\n    2, // two\n]\n"
        );
        assert_eq!(format("[\n 1,\n // last\n]"), "[\n    1,\n    // last\n]\n");
    }

    #[test]
    fn no_indent_preserves_line_comments() {
        let config = FormatConfig::new()
            .indent("")
            .comments(CommentMode::Preserve);
        assert_eq!(format_with("[1 // c\n, 2]", &config), "[\n1, // c\n2,\n]");
    }

    // =========================================================================
    // Compact output is one line
    // =========================================================================

    #[test]
    fn hard_compaction_is_single_line() {
        let config = FormatConfig::new()
            .char_limit(10)
            .compact_types(CompactTypes {
                tuples: true,
                ..CompactTypes::default()
            });
        assert_eq!(
            format_with("(a: (1, [1111111, 2222222, 3333333]))", &config),
            "(\n    a: (1, [1111111, 2222222, 3333333]),\n)\n"
        );
    }

    #[test]
    fn preserve_keeps_line_comments_out_of_compact_output() {
        let config = FormatConfig::new()
            .comments(CommentMode::Preserve)
            .compact_from_depth(1);
        assert_eq!(
            format_with("(a: (b: [\n // c\n 1,\n]))", &config),
            "(\n    a: (\n        b: [\n            // c\n            1,\n        ],\n    ),\n)\n"
        );
    }

    #[test]
    fn multiline_block_comment_prevents_compaction() {
        let source = "Config(items: [\n    /* a\n       b */\n    1,\n])";
        assert!(format(source).contains("items: [\n"));
    }

    // =========================================================================
    // Line to block comment conversion
    // =========================================================================

    #[test]
    fn unsafe_line_comments_are_not_converted() {
        let config = FormatConfig::new().compact_from_depth(1);
        let formatted = format_with(
            "(a: [\n // see a*/b\n 1,\n], b: [ // open /* here\n 2], c: [ // fine\n 3])",
            &config,
        );
        assert_eq!(
            formatted,
            "(\n    a: [\n        // see a*/b\n        1,\n    ],\n    \
             b: [ // open /* here\n        2,\n    ],\n    c: [/* fine */ 3],\n)\n"
        );
    }

    // =========================================================================
    // Comments in less common places
    // =========================================================================

    #[test]
    fn comment_between_struct_name_and_body() {
        assert_eq!(
            format("Point /* c */ (x: 1)"),
            "Point /* c */ (\n    x: 1,\n)\n"
        );
        assert_eq!(
            format("A(p: Point /* c */ (x: 1))"),
            "A(\n    p: Point /* c */ (x: 1),\n)\n"
        );
    }

    #[test]
    fn block_comment_after_last_comma() {
        assert_eq!(format("A(a: 1, /* c */)"), "A(\n    a: 1, /* c */\n)\n");
        assert_eq!(
            format("A(b: A(a: 1, /* c */))"),
            "A(\n    b: A(a: 1 /* c */),\n)\n"
        );
    }

    // =========================================================================
    // Root handling
    // =========================================================================

    #[test]
    fn format_expr_matches_format_document() {
        let doc = parse_document("Config(items: [1, 2], name: \"x\")").unwrap();
        let Some(value) = &doc.value else {
            panic!("expected a value");
        };
        for config in [
            FormatConfig::default(),
            FormatConfig::minimal(),
            FormatConfig::default().indent(""),
        ] {
            let document = format_document(&doc, &config);
            assert_eq!(document.trim_end(), format_expr(value, &config));
        }
    }

    // =========================================================================
    // Review fixes
    // =========================================================================

    /// Formats twice and checks that the second pass changes nothing.
    fn format_stable(source: &str, config: &FormatConfig) -> String {
        let once = format_with(source, config);
        assert_eq!(
            format_with(&once, config),
            once,
            "not idempotent: {source:?}"
        );
        once
    }

    #[test]
    fn forced_compaction_inside_length_compaction() {
        let arrays = FormatConfig::new().compact_types(CompactTypes {
            arrays: true,
            ..CompactTypes::default()
        });
        let depth_2 = FormatConfig::new().compact_from_depth(2);
        for config in [arrays, depth_2] {
            assert_eq!(
                format_stable("(a: (b: [1 // c\n, 2]))", &config),
                "(\n    a: (b: [1 /* c */, 2]),\n)\n"
            );
        }
        let no_indent = FormatConfig::new().indent("").compact_from_depth(1);
        assert_eq!(format_stable("[[1 // c\n]]", &no_indent), "[[1 /* c */]]");
    }

    #[test]
    fn line_comment_trailing_whitespace_is_trimmed() {
        assert_eq!(
            format("[1, // trailing spaces   \n 2]"),
            "[\n    1, // trailing spaces\n    2,\n]\n"
        );
    }

    #[test]
    fn empty_line_comment_converts_to_valid_block() {
        let config = FormatConfig::new().compact_from_depth(1);
        assert_eq!(
            format_stable("(a: [ //\n 1])", &config),
            "(\n    a: [/* */ 1],\n)\n"
        );
    }

    #[test]
    fn char_limit_counts_block_comment_before_comma() {
        // `    a: [0, 1, 2] /* a fairly long comment */,` is 45 characters.
        let source = "(a: [0, 1, 2] /* a fairly long comment */, b: 1)";
        let config = FormatConfig::new().char_limit(44);
        assert!(format_stable(source, &config).contains("a: [\n"));
        let config = FormatConfig::new().char_limit(45);
        assert!(
            format_stable(source, &config)
                .contains("    a: [0, 1, 2] /* a fairly long comment */,\n")
        );
    }

    #[test]
    fn column_restarts_after_multiline_raw_string() {
        // The key ends on a new line: `": [1, 2, 3],` is 13 characters.
        let source = "{r\"k\n\": [1, 2, 3]}";
        let config = FormatConfig::new().char_limit(13);
        assert_eq!(
            format_stable(source, &config),
            "{\n    r\"k\n\": [1, 2, 3],\n}\n"
        );
    }

    #[test]
    fn block_comment_stays_with_following_item() {
        assert_eq!(
            format_stable("[1, /* a */ 2]", &FormatConfig::new()),
            "[\n    1,\n    /* a */ 2,\n]\n"
        );
        assert_eq!(
            format_stable(
                "[\n    /* own line */\n    1, // end\n    2,\n]",
                &FormatConfig::new()
            ),
            "[\n    /* own line */\n    1, // end\n    2,\n]\n"
        );
    }
}
