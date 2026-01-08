//! AST types for RON documents with copy-on-write semantics.
//!
//! This module provides a complete AST representation that preserves:
//! - Original source text via `Cow` (zero-copy when borrowed, owned when mutated)
//! - Trivia (whitespace and comments) attached to nodes
//! - Span information for all nodes
//!
//! The AST uses `Cow<'a, str>` for all string data, enabling:
//! - Zero-copy parsing (strings borrow from source)
//! - In-place mutation (strings become owned on write)
//! - Single serializer implementation for both cases
//!
//! # Example
//!
//! ```
//! use ron2::ast::parse_document;
//!
//! // Parse a RON document into an AST (zero-copy)
//! let source = "// comment\n42";
//! let doc = parse_document(source).unwrap();
//! assert!(doc.value.is_some());
//! ```
//!
//! # Mutation Example
//!
//! ```
//! use ron2::ast::parse_document;
//! use std::borrow::Cow;
//!
//! let source = "Config { name: \"old\" }";
//! let mut doc = parse_document(source).unwrap();
//!
//! // Convert to owned for mutation (can outlive source)
//! let mut owned = doc.into_owned();
//! ```

mod convert;
mod parse;
mod ser;

pub use convert::{expr_to_value, to_value};
pub use parse::parse_document;
pub use ser::{serialize_document, serialize_document_to};

use alloc::{borrow::Cow, boxed::Box, string::String, vec::Vec};

use crate::error::Span;

// ============================================================================
// Trivia (whitespace and comments)
// ============================================================================

/// Trivia represents non-semantic content: whitespace and comments.
///
/// Trivia is attached to AST nodes to preserve formatting information
/// for round-tripping and LSP features.
///
/// Uses `Cow` for copy-on-write semantics - borrowed when parsed, owned when mutated.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Trivia<'a> {
    /// The span covering all trivia content.
    pub span: Option<Span>,
    /// The raw whitespace text (for exact round-tripping).
    /// This includes all characters between tokens that aren't comments.
    pub whitespace: Cow<'a, str>,
    /// Comments within this trivia section.
    pub comments: Vec<Comment<'a>>,
}

impl Trivia<'_> {
    /// Creates empty trivia with no comments or whitespace.
    #[must_use]
    pub fn empty() -> Self {
        Self {
            span: None,
            whitespace: Cow::Borrowed(""),
            comments: Vec::new(),
        }
    }

    /// Returns true if this trivia contains no content.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.span.is_none() && self.whitespace.is_empty() && self.comments.is_empty()
    }

    /// Converts this trivia to an owned version with `'static` lifetime.
    #[must_use]
    pub fn into_owned(self) -> Trivia<'static> {
        Trivia {
            span: self.span,
            whitespace: Cow::Owned(self.whitespace.into_owned()),
            comments: self.comments.into_iter().map(Comment::into_owned).collect(),
        }
    }
}

/// A comment in the source code.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Comment<'a> {
    /// The span of the comment including delimiters.
    pub span: Span,
    /// The comment text including delimiters (`//` or `/* */`).
    /// This is the raw source text for exact round-tripping.
    pub text: Cow<'a, str>,
    /// The kind of comment.
    pub kind: CommentKind,
}

impl Comment<'_> {
    /// Returns the comment content without delimiters.
    ///
    /// For line comments, this strips the leading `//`.
    /// For block comments, this strips `/*` and `*/`.
    #[must_use]
    pub fn content(&self) -> &str {
        match self.kind {
            CommentKind::Line => self.text.strip_prefix("//").unwrap_or(&self.text),
            CommentKind::Block => self
                .text
                .strip_prefix("/*")
                .and_then(|s| s.strip_suffix("*/"))
                .unwrap_or(&self.text),
        }
    }

    /// Converts this comment to an owned version with `'static` lifetime.
    #[must_use]
    pub fn into_owned(self) -> Comment<'static> {
        Comment {
            span: self.span,
            text: Cow::Owned(self.text.into_owned()),
            kind: self.kind,
        }
    }
}

/// The kind of comment.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CommentKind {
    /// A line comment starting with `//`.
    Line,
    /// A block comment delimited by `/*` and `*/`.
    Block,
}

// ============================================================================
// Document (root)
// ============================================================================

/// A complete RON document.
///
/// This is the root of the AST, containing the source text,
/// any inner attributes, and the main value expression.
///
/// Documents may be empty (containing only comments/whitespace),
/// in which case `value` is `None`.
#[derive(Clone, Debug, PartialEq)]
pub struct Document<'a> {
    /// The original source text (for zero-copy access).
    pub source: Cow<'a, str>,
    /// Leading trivia before any content.
    pub leading: Trivia<'a>,
    /// Inner attributes (`#![...]`).
    pub attributes: Vec<Attribute<'a>>,
    /// Trivia between attributes/leading and the value.
    pub pre_value: Trivia<'a>,
    /// The main value expression, if present.
    /// This is `None` for documents containing only comments/whitespace.
    pub value: Option<Expr<'a>>,
    /// Trailing trivia after the value.
    pub trailing: Trivia<'a>,
}

impl Document<'_> {
    /// Converts this document to an owned version with `'static` lifetime.
    #[must_use]
    pub fn into_owned(self) -> Document<'static> {
        Document {
            source: Cow::Owned(self.source.into_owned()),
            leading: self.leading.into_owned(),
            attributes: self.attributes.into_iter().map(Attribute::into_owned).collect(),
            pre_value: self.pre_value.into_owned(),
            value: self.value.map(Expr::into_owned),
            trailing: self.trailing.into_owned(),
        }
    }
}

// ============================================================================
// Attributes
// ============================================================================

/// An inner attribute (`#![name]`, `#![name = "value"]`, or `#![name(args)]`).
///
/// Attributes provide metadata about the RON document, such as
/// the expected Rust type, schema path, or enabled extensions.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Attribute<'a> {
    /// The span of the entire attribute.
    pub span: Span,
    /// Leading trivia before this attribute.
    pub leading: Trivia<'a>,
    /// The attribute name (e.g., "type", "schema", "enable").
    pub name: Cow<'a, str>,
    /// The attribute content.
    pub content: AttributeContent<'a>,
}

impl Attribute<'_> {
    /// Converts this attribute to an owned version with `'static` lifetime.
    #[must_use]
    pub fn into_owned(self) -> Attribute<'static> {
        Attribute {
            span: self.span,
            leading: self.leading.into_owned(),
            name: Cow::Owned(self.name.into_owned()),
            content: self.content.into_owned(),
        }
    }
}

/// The content of an attribute after the name.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AttributeContent<'a> {
    /// No content: `#![name]`
    None,
    /// Equals value: `#![name = "value"]`
    Value(Cow<'a, str>),
    /// Parenthesized arguments: `#![enable(implicit_some, unwrap_newtypes)]`
    Args(Vec<Cow<'a, str>>),
}

impl AttributeContent<'_> {
    /// Converts this content to an owned version with `'static` lifetime.
    #[must_use]
    pub fn into_owned(self) -> AttributeContent<'static> {
        match self {
            Self::None => AttributeContent::None,
            Self::Value(v) => AttributeContent::Value(Cow::Owned(v.into_owned())),
            Self::Args(args) => AttributeContent::Args(
                args.into_iter().map(|a| Cow::Owned(a.into_owned())).collect(),
            ),
        }
    }
}

// ============================================================================
// Expressions
// ============================================================================

/// An expression representing any RON value.
#[derive(Clone, Debug, PartialEq)]
pub enum Expr<'a> {
    /// Unit value: `()`.
    Unit(UnitExpr),
    /// Boolean value: `true` or `false`.
    Bool(BoolExpr),
    /// Character literal: `'a'`.
    Char(CharExpr<'a>),
    /// Byte literal: `b'x'`.
    Byte(ByteExpr<'a>),
    /// Numeric literal: `42`, `3.14`, `0xFF`, `1_000`.
    Number(NumberExpr<'a>),
    /// String literal: `"hello"` or `r#"raw"#`.
    String(StringExpr<'a>),
    /// Byte string literal: `b"bytes"` or `br#"raw"#`.
    Bytes(BytesExpr<'a>),
    /// Option value: `None` or `Some(value)`.
    Option(Box<OptionExpr<'a>>),
    /// Sequence: `[a, b, c]`.
    Seq(SeqExpr<'a>),
    /// Map: `{key: value, ...}`.
    Map(MapExpr<'a>),
    /// Tuple: `(a, b, c)`.
    Tuple(TupleExpr<'a>),
    /// Anonymous struct: `(field: value, ...)`.
    AnonStruct(AnonStructExpr<'a>),
    /// Named struct: `Name(...)` or `Name { ... }`.
    Struct(StructExpr<'a>),
}

impl Expr<'_> {
    /// Returns the span of this expression.
    #[must_use]
    pub fn span(&self) -> &Span {
        match self {
            Self::Unit(e) => &e.span,
            Self::Bool(e) => &e.span,
            Self::Char(e) => &e.span,
            Self::Byte(e) => &e.span,
            Self::Number(e) => &e.span,
            Self::String(e) => &e.span,
            Self::Bytes(e) => &e.span,
            Self::Option(e) => &e.span,
            Self::Seq(e) => &e.span,
            Self::Map(e) => &e.span,
            Self::Tuple(e) => &e.span,
            Self::AnonStruct(e) => &e.span,
            Self::Struct(e) => &e.span,
        }
    }

    /// Converts this expression to an owned version with `'static` lifetime.
    #[must_use]
    pub fn into_owned(self) -> Expr<'static> {
        match self {
            Self::Unit(u) => Expr::Unit(u),
            Self::Bool(b) => Expr::Bool(b),
            Self::Char(c) => Expr::Char(c.into_owned()),
            Self::Byte(b) => Expr::Byte(b.into_owned()),
            Self::Number(n) => Expr::Number(n.into_owned()),
            Self::String(s) => Expr::String(s.into_owned()),
            Self::Bytes(b) => Expr::Bytes(b.into_owned()),
            Self::Option(o) => Expr::Option(Box::new(o.into_owned())),
            Self::Seq(s) => Expr::Seq(s.into_owned()),
            Self::Map(m) => Expr::Map(m.into_owned()),
            Self::Tuple(t) => Expr::Tuple(t.into_owned()),
            Self::AnonStruct(a) => Expr::AnonStruct(a.into_owned()),
            Self::Struct(s) => Expr::Struct(s.into_owned()),
        }
    }
}

// ============================================================================
// Primitive expressions
// ============================================================================

/// Unit expression: `()`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct UnitExpr {
    /// The span of `()`.
    pub span: Span,
}

/// Boolean expression: `true` or `false`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BoolExpr {
    /// The span of the boolean literal.
    pub span: Span,
    /// The parsed boolean value.
    pub value: bool,
}

/// Character expression: `'a'`, `'\n'`, `'\x00'`, `'\u{1F600}'`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CharExpr<'a> {
    /// The span of the character literal including quotes.
    pub span: Span,
    /// The raw source text (includes quotes).
    pub raw: Cow<'a, str>,
    /// The parsed character value.
    pub value: char,
}

impl CharExpr<'_> {
    /// Converts to an owned version with `'static` lifetime.
    #[must_use]
    pub fn into_owned(self) -> CharExpr<'static> {
        CharExpr {
            span: self.span,
            raw: Cow::Owned(self.raw.into_owned()),
            value: self.value,
        }
    }
}

/// Byte literal expression: `b'x'`, `b'\n'`, `b'\x00'`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ByteExpr<'a> {
    /// The span of the byte literal including `b` prefix and quotes.
    pub span: Span,
    /// The raw source text (includes `b` prefix and quotes).
    pub raw: Cow<'a, str>,
    /// The parsed byte value.
    pub value: u8,
}

impl ByteExpr<'_> {
    /// Converts to an owned version with `'static` lifetime.
    #[must_use]
    pub fn into_owned(self) -> ByteExpr<'static> {
        ByteExpr {
            span: self.span,
            raw: Cow::Owned(self.raw.into_owned()),
            value: self.value,
        }
    }
}

/// Numeric expression: `42`, `3.14`, `0xFF`, `0b1010`, `0o777`, `1_000`.
///
/// The raw text is preserved to maintain the original format
/// (hex, binary, octal, underscores, etc.).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NumberExpr<'a> {
    /// The span of the number literal.
    pub span: Span,
    /// The raw source text (preserves format like `0xFF`, `1_000`).
    pub raw: Cow<'a, str>,
    /// The parsed numeric kind, if available.
    pub kind: NumberKind,
}

impl NumberExpr<'_> {
    /// Converts to an owned version with `'static` lifetime.
    #[must_use]
    pub fn into_owned(self) -> NumberExpr<'static> {
        NumberExpr {
            span: self.span,
            raw: Cow::Owned(self.raw.into_owned()),
            kind: self.kind,
        }
    }
}

/// The kind of number parsed.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum NumberKind {
    /// A positive integer.
    Integer,
    /// A negative integer.
    NegativeInteger,
    /// A floating-point number.
    Float,
    /// Special float values: `inf`, `-inf`, `NaN`.
    SpecialFloat,
}

/// String expression: `"hello"`, `"with\nescape"`, `r#"raw"#`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StringExpr<'a> {
    /// The span of the string literal including quotes.
    pub span: Span,
    /// The raw source text (includes quotes and any `r#` prefix).
    pub raw: Cow<'a, str>,
    /// The parsed string value (with escapes processed).
    pub value: String,
    /// The kind of string literal.
    pub kind: StringKind,
}

impl StringExpr<'_> {
    /// Converts to an owned version with `'static` lifetime.
    #[must_use]
    pub fn into_owned(self) -> StringExpr<'static> {
        StringExpr {
            span: self.span,
            raw: Cow::Owned(self.raw.into_owned()),
            value: self.value,
            kind: self.kind,
        }
    }
}

/// The kind of string literal.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StringKind {
    /// A regular quoted string: `"hello"`.
    Regular,
    /// A raw string: `r"raw"` or `r#"raw"#`.
    Raw {
        /// Number of `#` characters used.
        hash_count: u8,
    },
}

/// Byte string expression: `b"bytes"`, `br#"raw"#`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BytesExpr<'a> {
    /// The span of the byte string literal including prefix and quotes.
    pub span: Span,
    /// The raw source text.
    pub raw: Cow<'a, str>,
    /// The parsed byte values.
    pub value: Vec<u8>,
    /// The kind of byte string literal.
    pub kind: BytesKind,
}

impl BytesExpr<'_> {
    /// Converts to an owned version with `'static` lifetime.
    #[must_use]
    pub fn into_owned(self) -> BytesExpr<'static> {
        BytesExpr {
            span: self.span,
            raw: Cow::Owned(self.raw.into_owned()),
            value: self.value,
            kind: self.kind,
        }
    }
}

/// The kind of byte string literal.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BytesKind {
    /// A regular byte string: `b"bytes"`.
    Regular,
    /// A raw byte string: `br"raw"` or `br#"raw"#`.
    Raw {
        /// Number of `#` characters used.
        hash_count: u8,
    },
}

// ============================================================================
// Option expression
// ============================================================================

/// Option expression: `None` or `Some(value)`.
#[derive(Clone, Debug, PartialEq)]
pub struct OptionExpr<'a> {
    /// The span of the entire option expression.
    pub span: Span,
    /// The value, if `Some`.
    pub value: Option<OptionValue<'a>>,
}

impl OptionExpr<'_> {
    /// Converts to an owned version with `'static` lifetime.
    #[must_use]
    pub fn into_owned(self) -> OptionExpr<'static> {
        OptionExpr {
            span: self.span,
            value: self.value.map(OptionValue::into_owned),
        }
    }
}

/// The inner value of a `Some(...)` expression.
#[derive(Clone, Debug, PartialEq)]
pub struct OptionValue<'a> {
    /// Span of the opening parenthesis.
    pub open_paren: Span,
    /// Leading trivia before the inner value.
    pub leading: Trivia<'a>,
    /// The inner expression.
    pub expr: Expr<'a>,
    /// Trailing trivia after the inner value.
    pub trailing: Trivia<'a>,
    /// Span of the closing parenthesis.
    pub close_paren: Span,
}

impl OptionValue<'_> {
    /// Converts to an owned version with `'static` lifetime.
    #[must_use]
    pub fn into_owned(self) -> OptionValue<'static> {
        OptionValue {
            open_paren: self.open_paren,
            leading: self.leading.into_owned(),
            expr: self.expr.into_owned(),
            trailing: self.trailing.into_owned(),
            close_paren: self.close_paren,
        }
    }
}

// ============================================================================
// Collection expressions
// ============================================================================

/// Sequence expression: `[a, b, c]`.
#[derive(Clone, Debug, PartialEq)]
pub struct SeqExpr<'a> {
    /// The span of the entire sequence including brackets.
    pub span: Span,
    /// Span of the opening bracket `[`.
    pub open_bracket: Span,
    /// Leading trivia after the opening bracket.
    pub leading: Trivia<'a>,
    /// The sequence items.
    pub items: Vec<SeqItem<'a>>,
    /// Trailing trivia before the closing bracket.
    pub trailing: Trivia<'a>,
    /// Span of the closing bracket `]`.
    pub close_bracket: Span,
}

impl SeqExpr<'_> {
    /// Converts to an owned version with `'static` lifetime.
    #[must_use]
    pub fn into_owned(self) -> SeqExpr<'static> {
        SeqExpr {
            span: self.span,
            open_bracket: self.open_bracket,
            leading: self.leading.into_owned(),
            items: self.items.into_iter().map(SeqItem::into_owned).collect(),
            trailing: self.trailing.into_owned(),
            close_bracket: self.close_bracket,
        }
    }
}

/// An item in a sequence.
#[derive(Clone, Debug, PartialEq)]
pub struct SeqItem<'a> {
    /// Leading trivia before this item.
    pub leading: Trivia<'a>,
    /// The item expression.
    pub expr: Expr<'a>,
    /// Trailing trivia after the expression (before comma).
    pub trailing: Trivia<'a>,
    /// The trailing comma, if present.
    pub comma: Option<Span>,
}

impl SeqItem<'_> {
    /// Converts to an owned version with `'static` lifetime.
    #[must_use]
    pub fn into_owned(self) -> SeqItem<'static> {
        SeqItem {
            leading: self.leading.into_owned(),
            expr: self.expr.into_owned(),
            trailing: self.trailing.into_owned(),
            comma: self.comma,
        }
    }
}

/// Map expression: `{key: value, ...}`.
#[derive(Clone, Debug, PartialEq)]
pub struct MapExpr<'a> {
    /// The span of the entire map including braces.
    pub span: Span,
    /// Span of the opening brace `{`.
    pub open_brace: Span,
    /// Leading trivia after the opening brace.
    pub leading: Trivia<'a>,
    /// The map entries.
    pub entries: Vec<MapEntry<'a>>,
    /// Trailing trivia before the closing brace.
    pub trailing: Trivia<'a>,
    /// Span of the closing brace `}`.
    pub close_brace: Span,
}

impl MapExpr<'_> {
    /// Converts to an owned version with `'static` lifetime.
    #[must_use]
    pub fn into_owned(self) -> MapExpr<'static> {
        MapExpr {
            span: self.span,
            open_brace: self.open_brace,
            leading: self.leading.into_owned(),
            entries: self.entries.into_iter().map(MapEntry::into_owned).collect(),
            trailing: self.trailing.into_owned(),
            close_brace: self.close_brace,
        }
    }
}

/// An entry in a map: `key: value`.
#[derive(Clone, Debug, PartialEq)]
pub struct MapEntry<'a> {
    /// Leading trivia before the key.
    pub leading: Trivia<'a>,
    /// The key expression.
    pub key: Expr<'a>,
    /// Trivia between key and colon.
    pub pre_colon: Trivia<'a>,
    /// Span of the colon `:`.
    pub colon: Span,
    /// Trivia between colon and value.
    pub post_colon: Trivia<'a>,
    /// The value expression.
    pub value: Expr<'a>,
    /// Trailing trivia after the value (before comma).
    pub trailing: Trivia<'a>,
    /// The trailing comma, if present.
    pub comma: Option<Span>,
}

impl MapEntry<'_> {
    /// Converts to an owned version with `'static` lifetime.
    #[must_use]
    pub fn into_owned(self) -> MapEntry<'static> {
        MapEntry {
            leading: self.leading.into_owned(),
            key: self.key.into_owned(),
            pre_colon: self.pre_colon.into_owned(),
            colon: self.colon,
            post_colon: self.post_colon.into_owned(),
            value: self.value.into_owned(),
            trailing: self.trailing.into_owned(),
            comma: self.comma,
        }
    }
}

/// Tuple expression: `(a, b, c)`.
///
/// Note: In RON, a tuple is an unnamed parenthesized list of values.
/// Named tuples are represented as `StructExpr` with tuple style.
#[derive(Clone, Debug, PartialEq)]
pub struct TupleExpr<'a> {
    /// The span of the entire tuple including parentheses.
    pub span: Span,
    /// Span of the opening parenthesis `(`.
    pub open_paren: Span,
    /// Leading trivia after the opening parenthesis.
    pub leading: Trivia<'a>,
    /// The tuple elements.
    pub elements: Vec<TupleElement<'a>>,
    /// Trailing trivia before the closing parenthesis.
    pub trailing: Trivia<'a>,
    /// Span of the closing parenthesis `)`.
    pub close_paren: Span,
}

impl TupleExpr<'_> {
    /// Converts to an owned version with `'static` lifetime.
    #[must_use]
    pub fn into_owned(self) -> TupleExpr<'static> {
        TupleExpr {
            span: self.span,
            open_paren: self.open_paren,
            leading: self.leading.into_owned(),
            elements: self.elements.into_iter().map(TupleElement::into_owned).collect(),
            trailing: self.trailing.into_owned(),
            close_paren: self.close_paren,
        }
    }
}

/// Anonymous struct expression: `(field: value, ...)`.
///
/// This represents a struct-like value with named fields but no type name.
#[derive(Clone, Debug, PartialEq)]
pub struct AnonStructExpr<'a> {
    /// The span of the entire struct including parentheses.
    pub span: Span,
    /// Span of the opening parenthesis `(`.
    pub open_paren: Span,
    /// Leading trivia after the opening parenthesis.
    pub leading: Trivia<'a>,
    /// The struct fields.
    pub fields: Vec<StructField<'a>>,
    /// Trailing trivia before the closing parenthesis.
    pub trailing: Trivia<'a>,
    /// Span of the closing parenthesis `)`.
    pub close_paren: Span,
}

impl AnonStructExpr<'_> {
    /// Converts to an owned version with `'static` lifetime.
    #[must_use]
    pub fn into_owned(self) -> AnonStructExpr<'static> {
        AnonStructExpr {
            span: self.span,
            open_paren: self.open_paren,
            leading: self.leading.into_owned(),
            fields: self.fields.into_iter().map(StructField::into_owned).collect(),
            trailing: self.trailing.into_owned(),
            close_paren: self.close_paren,
        }
    }
}

/// An element in a tuple.
#[derive(Clone, Debug, PartialEq)]
pub struct TupleElement<'a> {
    /// Leading trivia before this element.
    pub leading: Trivia<'a>,
    /// The element expression.
    pub expr: Expr<'a>,
    /// Trailing trivia after the expression (before comma).
    pub trailing: Trivia<'a>,
    /// The trailing comma, if present.
    pub comma: Option<Span>,
}

impl TupleElement<'_> {
    /// Converts to an owned version with `'static` lifetime.
    #[must_use]
    pub fn into_owned(self) -> TupleElement<'static> {
        TupleElement {
            leading: self.leading.into_owned(),
            expr: self.expr.into_owned(),
            trailing: self.trailing.into_owned(),
            comma: self.comma,
        }
    }
}

// ============================================================================
// Struct expression
// ============================================================================

/// Struct expression: `Name(...)`, `Name { ... }`, or `Name`.
///
/// This covers:
/// - Named tuple structs: `Point(1, 2)`
/// - Named structs with fields: `Config { x: 1, y: 2 }`
/// - Unit structs: `None` (when not the built-in None)
/// - Enum variants: `Variant(...)` or `Variant { ... }`
#[derive(Clone, Debug, PartialEq)]
pub struct StructExpr<'a> {
    /// The span of the entire struct expression.
    pub span: Span,
    /// The struct/enum name.
    pub name: Ident<'a>,
    /// Trivia between name and body (e.g., space in `Point { ... }`).
    pub pre_body: Trivia<'a>,
    /// The body of the struct, if any.
    pub body: Option<StructBody<'a>>,
}

impl StructExpr<'_> {
    /// Converts to an owned version with `'static` lifetime.
    #[must_use]
    pub fn into_owned(self) -> StructExpr<'static> {
        StructExpr {
            span: self.span,
            name: self.name.into_owned(),
            pre_body: self.pre_body.into_owned(),
            body: self.body.map(StructBody::into_owned),
        }
    }
}

/// An identifier (struct name, enum variant, field name).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ident<'a> {
    /// The span of the identifier.
    pub span: Span,
    /// The identifier text.
    pub name: Cow<'a, str>,
}

impl Ident<'_> {
    /// Converts to an owned version with `'static` lifetime.
    #[must_use]
    pub fn into_owned(self) -> Ident<'static> {
        Ident {
            span: self.span,
            name: Cow::Owned(self.name.into_owned()),
        }
    }
}

/// The body of a struct expression.
#[derive(Clone, Debug, PartialEq)]
pub enum StructBody<'a> {
    /// Tuple-style body: `(a, b, c)`.
    Tuple(TupleBody<'a>),
    /// Named fields body: `{ x: 1, y: 2 }`.
    Fields(FieldsBody<'a>),
}

impl StructBody<'_> {
    /// Converts to an owned version with `'static` lifetime.
    #[must_use]
    pub fn into_owned(self) -> StructBody<'static> {
        match self {
            Self::Tuple(t) => StructBody::Tuple(t.into_owned()),
            Self::Fields(f) => StructBody::Fields(f.into_owned()),
        }
    }
}

/// Tuple-style struct body: `(a, b, c)`.
#[derive(Clone, Debug, PartialEq)]
pub struct TupleBody<'a> {
    /// Span of the opening parenthesis `(`.
    pub open_paren: Span,
    /// Leading trivia after the opening parenthesis.
    pub leading: Trivia<'a>,
    /// The tuple elements.
    pub elements: Vec<TupleElement<'a>>,
    /// Trailing trivia before the closing parenthesis.
    pub trailing: Trivia<'a>,
    /// Span of the closing parenthesis `)`.
    pub close_paren: Span,
}

impl TupleBody<'_> {
    /// Converts to an owned version with `'static` lifetime.
    #[must_use]
    pub fn into_owned(self) -> TupleBody<'static> {
        TupleBody {
            open_paren: self.open_paren,
            leading: self.leading.into_owned(),
            elements: self.elements.into_iter().map(TupleElement::into_owned).collect(),
            trailing: self.trailing.into_owned(),
            close_paren: self.close_paren,
        }
    }
}

/// Named fields struct body: `{ x: 1, y: 2 }`.
#[derive(Clone, Debug, PartialEq)]
pub struct FieldsBody<'a> {
    /// Span of the opening brace `{`.
    pub open_brace: Span,
    /// Leading trivia after the opening brace.
    pub leading: Trivia<'a>,
    /// The struct fields.
    pub fields: Vec<StructField<'a>>,
    /// Trailing trivia before the closing brace.
    pub trailing: Trivia<'a>,
    /// Span of the closing brace `}`.
    pub close_brace: Span,
}

impl FieldsBody<'_> {
    /// Converts to an owned version with `'static` lifetime.
    #[must_use]
    pub fn into_owned(self) -> FieldsBody<'static> {
        FieldsBody {
            open_brace: self.open_brace,
            leading: self.leading.into_owned(),
            fields: self.fields.into_iter().map(StructField::into_owned).collect(),
            trailing: self.trailing.into_owned(),
            close_brace: self.close_brace,
        }
    }
}

/// A field in a struct: `name: value`.
#[derive(Clone, Debug, PartialEq)]
pub struct StructField<'a> {
    /// Leading trivia before the field name.
    pub leading: Trivia<'a>,
    /// The field name.
    pub name: Ident<'a>,
    /// Trivia between name and colon.
    pub pre_colon: Trivia<'a>,
    /// Span of the colon `:`.
    pub colon: Span,
    /// Trivia between colon and value.
    pub post_colon: Trivia<'a>,
    /// The field value.
    pub value: Expr<'a>,
    /// Trailing trivia after the value (before comma).
    pub trailing: Trivia<'a>,
    /// The trailing comma, if present.
    pub comma: Option<Span>,
}

impl StructField<'_> {
    /// Converts to an owned version with `'static` lifetime.
    #[must_use]
    pub fn into_owned(self) -> StructField<'static> {
        StructField {
            leading: self.leading.into_owned(),
            name: self.name.into_owned(),
            pre_colon: self.pre_colon.into_owned(),
            colon: self.colon,
            post_colon: self.post_colon.into_owned(),
            value: self.value.into_owned(),
            trailing: self.trailing.into_owned(),
            comma: self.comma,
        }
    }
}
