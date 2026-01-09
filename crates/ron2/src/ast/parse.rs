//! AST parser for RON documents.
//!
//! This module provides parsing of RON source into a full AST that preserves
//! all trivia (whitespace and comments) for round-trip editing.

use alloc::{borrow::Cow, boxed::Box, string::String, vec::Vec};
use core::iter::Peekable;

use crate::ast::{
    AnonStructExpr, Attribute, AttributeContent, BoolExpr, ByteExpr, BytesExpr, BytesKind,
    CharExpr, Comment, CommentKind, Document, Expr, FieldsBody, Ident, MapEntry, MapExpr,
    NumberExpr, NumberKind, OptionExpr, OptionValue, SeqExpr, SeqItem, StringExpr, StringKind,
    StructBody, StructExpr, StructField, Trivia, TupleBody, TupleElement, TupleExpr, UnitExpr,
};
use crate::error::{Error, Position, Result, Span, SpannedError, SpannedResult};
use crate::lexer::Lexer;
use crate::token::{Token, TokenKind};

/// Parse RON source into an AST document.
///
/// This is the main entry point for AST parsing. It returns a fully-formed
/// AST with all trivia preserved.
///
/// # Errors
///
/// Returns an error if the source contains invalid RON syntax.
pub fn parse_document(source: &str) -> SpannedResult<Document<'_>> {
    let lexer = Lexer::new(source).with_trivia(true);
    let mut parser = AstParser::new(source, lexer);
    parser.parse_document()
}

/// AST parser that consumes tokens and builds AST nodes.
struct AstParser<'a> {
    source: &'a str,
    tokens: Peekable<Lexer<'a>>,
    /// Buffer for collecting trivia before a token.
    trivia_buffer: Vec<Token<'a>>,
    /// Lookahead buffer for tokens that were peeked but need to be returned.
    lookahead: Vec<Token<'a>>,
}

impl<'a> AstParser<'a> {
    fn new(source: &'a str, lexer: Lexer<'a>) -> Self {
        Self {
            source,
            tokens: lexer.peekable(),
            trivia_buffer: Vec::new(),
            lookahead: Vec::new(),
        }
    }

    /// Peek at the next non-trivia token kind without consuming.
    fn peek_kind(&mut self) -> TokenKind {
        // Check lookahead buffer first
        if let Some(tok) = self.lookahead.last() {
            return tok.kind;
        }

        // Collect trivia into buffer and return next non-trivia token
        while let Some(tok) = self.tokens.peek() {
            if tok.kind.is_trivia() {
                // Safe: we just peeked and confirmed there's a token
                if let Some(tok) = self.tokens.next() {
                    self.trivia_buffer.push(tok);
                }
            } else {
                return tok.kind;
            }
        }
        TokenKind::Eof
    }

    /// Peek at the next two non-trivia token kinds without consuming.
    /// Returns (`first_kind`, `second_kind`).
    fn peek_two_kinds(&mut self) -> (TokenKind, TokenKind) {
        // First, ensure we have at least one token in lookahead
        if self.lookahead.is_empty() {
            // Collect trivia and find the first non-trivia token
            while let Some(tok) = self.tokens.peek() {
                if tok.kind.is_trivia() {
                    if let Some(tok) = self.tokens.next() {
                        self.trivia_buffer.push(tok);
                    }
                } else {
                    break;
                }
            }
            if let Some(tok) = self.tokens.next() {
                self.lookahead.push(tok);
            }
        }

        let first_kind = self.lookahead.last().map_or(TokenKind::Eof, |t| t.kind);

        // Now find the second non-trivia token
        // Skip trivia to find second token
        while let Some(tok) = self.tokens.peek() {
            if tok.kind.is_trivia() {
                if let Some(tok) = self.tokens.next() {
                    self.trivia_buffer.push(tok);
                }
            } else {
                break;
            }
        }

        let second_kind = self.tokens.peek().map_or(TokenKind::Eof, |t| t.kind);

        (first_kind, second_kind)
    }

    /// Consume and return the next non-trivia token, collecting trivia into buffer.
    fn next_token(&mut self) -> Token<'a> {
        // Check lookahead buffer first
        if let Some(tok) = self.lookahead.pop() {
            return tok;
        }

        // First collect any trivia
        while let Some(tok) = self.tokens.peek() {
            if tok.kind.is_trivia() {
                // Safe: we just peeked and confirmed there's a token
                if let Some(tok) = self.tokens.next() {
                    self.trivia_buffer.push(tok);
                }
            } else {
                break;
            }
        }
        // Now return the non-trivia token (or Eof)
        self.tokens.next().unwrap_or_else(|| Token {
            kind: TokenKind::Eof,
            text: "",
            span: self.eof_span(),
        })
    }

    /// Drain the trivia buffer into a Trivia struct.
    fn drain_trivia(&mut self) -> Trivia<'a> {
        if self.trivia_buffer.is_empty() {
            return Trivia::empty();
        }

        let tokens = core::mem::take(&mut self.trivia_buffer);
        let start = tokens.first().map(|t| t.span.clone());
        let end = tokens.last().map(|t| t.span.clone());

        let span = match (start, end) {
            (Some(s), Some(e)) => Some(Span {
                start: s.start,
                end: e.end,
                start_offset: s.start_offset,
                end_offset: e.end_offset,
            }),
            _ => None,
        };

        // Build whitespace string from all whitespace tokens
        let mut whitespace_ranges: Vec<(usize, usize)> = Vec::new();
        let mut comments = Vec::new();

        for tok in tokens {
            match tok.kind {
                TokenKind::Whitespace => {
                    whitespace_ranges.push((tok.span.start_offset, tok.span.end_offset));
                }
                TokenKind::LineComment => {
                    comments.push(Comment {
                        span: tok.span,
                        text: Cow::Borrowed(tok.text),
                        kind: CommentKind::Line,
                    });
                }
                TokenKind::BlockComment => {
                    comments.push(Comment {
                        span: tok.span,
                        text: Cow::Borrowed(tok.text),
                        kind: CommentKind::Block,
                    });
                }
                _ => {}
            }
        }

        // For whitespace, we concatenate the ranges into a single slice if contiguous
        // For simplicity, just use the first whitespace token's text or empty
        let whitespace = if let Some((start, end)) = whitespace_ranges.first() {
            Cow::Borrowed(&self.source[*start..*end])
        } else {
            Cow::Borrowed("")
        };

        Trivia {
            span,
            whitespace,
            comments,
        }
    }

    /// Create a span at EOF position.
    fn eof_span(&self) -> Span {
        let len = self.source.len();
        let pos = Position::from_src_end(self.source);
        Span {
            start: pos,
            end: pos,
            start_offset: len,
            end_offset: len,
        }
    }

    /// Create a spanned error.
    fn error(span: Span, code: Error) -> SpannedError {
        SpannedError { code, span }
    }

    /// Parse a complete document.
    fn parse_document(&mut self) -> SpannedResult<Document<'a>> {
        // Collect leading trivia
        let leading = self.collect_leading_trivia();

        // Parse attributes
        let attributes = self.parse_attributes()?;

        // Collect trivia between attributes and value
        let pre_value = self.collect_leading_trivia();

        // Parse the main value if present
        let value = match self.peek_kind() {
            TokenKind::Eof => None,
            _ => Some(self.parse_expr()?),
        };

        // Collect trailing trivia
        let trailing = self.collect_leading_trivia();

        // Ensure we're at EOF
        if self.peek_kind() != TokenKind::Eof {
            let tok = self.next_token();
            return Err(Self::error(tok.span, Error::TrailingCharacters));
        }

        Ok(Document {
            source: Cow::Borrowed(self.source),
            leading,
            attributes,
            pre_value,
            value,
            trailing,
        })
    }

    /// Collect leading trivia without consuming a non-trivia token.
    fn collect_leading_trivia(&mut self) -> Trivia<'a> {
        // Peek to force trivia collection
        let _ = self.peek_kind();
        self.drain_trivia()
    }

    /// Parse inner attributes (`#![...]`).
    fn parse_attributes(&mut self) -> SpannedResult<Vec<Attribute<'a>>> {
        let mut attributes = Vec::new();

        while self.peek_kind() == TokenKind::Hash {
            let leading = self.drain_trivia();
            let attr = self.parse_attribute(leading)?;
            attributes.push(attr);
        }

        Ok(attributes)
    }

    /// Parse a single attribute.
    fn parse_attribute(&mut self, leading: Trivia<'a>) -> SpannedResult<Attribute<'a>> {
        let hash = self.next_token();
        debug_assert_eq!(hash.kind, TokenKind::Hash);

        // Expect `!`
        if self.peek_kind() != TokenKind::Bang {
            return Err(Self::error(hash.span, Error::ExpectedAttribute));
        }
        let _bang = self.next_token();

        // Expect `[`
        if self.peek_kind() != TokenKind::LBracket {
            return Err(Self::error(hash.span, Error::ExpectedAttribute));
        }
        let _lbracket = self.next_token();

        // Expect identifier
        if self.peek_kind() != TokenKind::Ident {
            return Err(Self::error(hash.span, Error::ExpectedIdentifier));
        }
        let name_tok = self.next_token();
        let name = name_tok.text;

        // Parse content based on what follows
        let content = match self.peek_kind() {
            TokenKind::Eq => {
                let _eq = self.next_token();
                // Expect string value
                if self.peek_kind() != TokenKind::String {
                    return Err(Self::error(name_tok.span, Error::ExpectedString));
                }
                let value_tok = self.next_token();
                AttributeContent::Value(Cow::Borrowed(value_tok.text))
            }
            TokenKind::LParen => {
                let _lparen = self.next_token();
                let mut args = Vec::new();

                // Parse comma-separated identifiers
                loop {
                    match self.peek_kind() {
                        TokenKind::RParen => break,
                        TokenKind::Ident => {
                            let arg_tok = self.next_token();
                            args.push(Cow::Borrowed(arg_tok.text));

                            // Check for comma
                            if self.peek_kind() == TokenKind::Comma {
                                let _comma = self.next_token();
                            }
                        }
                        _ => {
                            let tok = self.next_token();
                            return Err(Self::error(tok.span, Error::ExpectedIdentifier));
                        }
                    }
                }

                // Expect `)`
                if self.peek_kind() != TokenKind::RParen {
                    return Err(Self::error(name_tok.span, Error::ExpectedAttributeEnd));
                }
                let _rparen = self.next_token();

                AttributeContent::Args(args)
            }
            _ => AttributeContent::None,
        };

        // Expect `]`
        if self.peek_kind() != TokenKind::RBracket {
            return Err(Self::error(hash.span, Error::ExpectedAttributeEnd));
        }
        let rbracket = self.next_token();

        let span = Span {
            start: hash.span.start,
            end: rbracket.span.end,
            start_offset: hash.span.start_offset,
            end_offset: rbracket.span.end_offset,
        };

        Ok(Attribute {
            span,
            leading,
            name: Cow::Borrowed(name),
            content,
        })
    }

    /// Parse an expression.
    fn parse_expr(&mut self) -> SpannedResult<Expr<'a>> {
        match self.peek_kind() {
            TokenKind::LParen => self.parse_tuple_or_unit(),
            TokenKind::LBracket => self.parse_seq(),
            TokenKind::LBrace => self.parse_map(),
            TokenKind::Ident => self.parse_ident_expr(),
            TokenKind::Integer => Ok(self.parse_integer()),
            TokenKind::Float => Ok(self.parse_float()),
            TokenKind::String => self.parse_string(),
            TokenKind::ByteString => self.parse_bytes(),
            TokenKind::Char => self.parse_char(),
            TokenKind::Eof => Err(Self::error(self.eof_span(), Error::Eof)),
            _ => {
                let tok = self.next_token();
                Err(Self::error(
                    tok.span,
                    Error::UnexpectedChar(tok.text.chars().next().unwrap_or('?')),
                ))
            }
        }
    }

    /// Parse a tuple `(a, b, c)` or unit `()`.
    fn parse_tuple_or_unit(&mut self) -> SpannedResult<Expr<'a>> {
        let open_paren = self.next_token();
        debug_assert_eq!(open_paren.kind, TokenKind::LParen);

        let leading = self.collect_leading_trivia();

        // Check for empty tuple (unit)
        if self.peek_kind() == TokenKind::RParen {
            let close_paren = self.next_token();
            return Ok(Expr::Unit(UnitExpr {
                span: Span {
                    start: open_paren.span.start,
                    end: close_paren.span.end,
                    start_offset: open_paren.span.start_offset,
                    end_offset: close_paren.span.end_offset,
                },
            }));
        }

        // Check for named field syntax: `x: value`
        let is_named_fields = self.peek_is_named_field();

        if is_named_fields {
            // Parse as anonymous struct with named fields
            self.parse_fields_body_inner(open_paren, leading)
        } else {
            // Parse as tuple
            self.parse_tuple_inner(open_paren, leading)
        }
    }

    /// Check if the next tokens look like `ident:` (a named field).
    fn peek_is_named_field(&mut self) -> bool {
        // Check if we have `ident:` pattern using two-token lookahead
        let (first, second) = self.peek_two_kinds();
        first == TokenKind::Ident && second == TokenKind::Colon
    }

    /// Parse a tuple expression starting after the opening paren.
    fn parse_tuple_inner(
        &mut self,
        open_paren: Token<'a>,
        mut leading: Trivia<'a>,
    ) -> SpannedResult<Expr<'a>> {
        let mut elements = Vec::new();

        loop {
            if self.peek_kind() == TokenKind::RParen {
                break;
            }

            let expr = self.parse_expr()?;
            let trailing = self.collect_leading_trivia();

            let comma = if self.peek_kind() == TokenKind::Comma {
                let comma_tok = self.next_token();
                Some(comma_tok.span)
            } else {
                None
            };
            let has_comma = comma.is_some();

            elements.push(TupleElement {
                leading,
                expr,
                trailing,
                comma,
            });

            leading = self.collect_leading_trivia();

            if !has_comma {
                break;
            }
        }

        let trailing = if elements.is_empty() {
            leading
        } else {
            self.collect_leading_trivia()
        };

        if self.peek_kind() != TokenKind::RParen {
            return Err(Self::error(open_paren.span, Error::ExpectedStructLikeEnd));
        }
        let close_paren = self.next_token();

        Ok(Expr::Tuple(TupleExpr {
            span: Span {
                start: open_paren.span.start,
                end: close_paren.span.end,
                start_offset: open_paren.span.start_offset,
                end_offset: close_paren.span.end_offset,
            },
            open_paren: open_paren.span,
            leading: Trivia::empty(),
            elements,
            trailing,
            close_paren: close_paren.span,
        }))
    }

    /// Parse a fields body starting after opening paren.
    fn parse_fields_body_inner(
        &mut self,
        open_paren: Token<'a>,
        mut leading: Trivia<'a>,
    ) -> SpannedResult<Expr<'a>> {
        let mut fields = Vec::new();

        loop {
            if self.peek_kind() == TokenKind::RParen {
                break;
            }

            // Expect field name
            if self.peek_kind() != TokenKind::Ident {
                let tok = self.next_token();
                return Err(Self::error(tok.span, Error::ExpectedIdentifier));
            }
            let name_tok = self.next_token();

            let pre_colon = self.collect_leading_trivia();

            // Expect colon
            if self.peek_kind() != TokenKind::Colon {
                return Err(Self::error(
                    name_tok.span,
                    Error::ExpectedMapColon {
                        context: Some("struct field"),
                    },
                ));
            }
            let colon_tok = self.next_token();

            let post_colon = self.collect_leading_trivia();

            // Parse value
            let value = self.parse_expr()?;
            let trailing = self.collect_leading_trivia();

            let comma = if self.peek_kind() == TokenKind::Comma {
                let comma_tok = self.next_token();
                Some(comma_tok.span)
            } else {
                None
            };
            let has_comma = comma.is_some();

            fields.push(StructField {
                leading,
                name: Ident {
                    span: name_tok.span.clone(),
                    name: Cow::Borrowed(name_tok.text),
                },
                pre_colon,
                colon: colon_tok.span,
                post_colon,
                value,
                trailing,
                comma,
            });

            leading = self.collect_leading_trivia();

            if !has_comma {
                break;
            }
        }

        let trailing = if fields.is_empty() {
            leading
        } else {
            self.collect_leading_trivia()
        };

        if self.peek_kind() != TokenKind::RParen {
            return Err(Self::error(open_paren.span, Error::ExpectedStructLikeEnd));
        }
        let close_paren = self.next_token();

        // Anonymous struct with fields
        Ok(Expr::AnonStruct(AnonStructExpr {
            span: Span {
                start: open_paren.span.start,
                end: close_paren.span.end,
                start_offset: open_paren.span.start_offset,
                end_offset: close_paren.span.end_offset,
            },
            open_paren: open_paren.span,
            leading: Trivia::empty(),
            fields,
            trailing,
            close_paren: close_paren.span,
        }))
    }

    /// Parse a sequence `[a, b, c]`.
    fn parse_seq(&mut self) -> SpannedResult<Expr<'a>> {
        let open_bracket = self.next_token();
        debug_assert_eq!(open_bracket.kind, TokenKind::LBracket);

        let mut leading = self.collect_leading_trivia();
        let mut items = Vec::new();

        loop {
            if self.peek_kind() == TokenKind::RBracket {
                break;
            }

            let expr = self.parse_expr()?;
            let trailing = self.collect_leading_trivia();

            let comma = if self.peek_kind() == TokenKind::Comma {
                let comma_tok = self.next_token();
                Some(comma_tok.span)
            } else {
                None
            };
            let has_comma = comma.is_some();

            items.push(SeqItem {
                leading,
                expr,
                trailing,
                comma,
            });

            leading = self.collect_leading_trivia();

            if !has_comma {
                break;
            }
        }

        let trailing = if items.is_empty() {
            leading
        } else {
            self.collect_leading_trivia()
        };

        if self.peek_kind() != TokenKind::RBracket {
            return Err(Self::error(open_bracket.span, Error::ExpectedArrayEnd));
        }
        let close_bracket = self.next_token();

        Ok(Expr::Seq(SeqExpr {
            span: Span {
                start: open_bracket.span.start,
                end: close_bracket.span.end,
                start_offset: open_bracket.span.start_offset,
                end_offset: close_bracket.span.end_offset,
            },
            open_bracket: open_bracket.span,
            leading: Trivia::empty(),
            items,
            trailing,
            close_bracket: close_bracket.span,
        }))
    }

    /// Parse a map `{key: value, ...}`.
    fn parse_map(&mut self) -> SpannedResult<Expr<'a>> {
        let open_brace = self.next_token();
        debug_assert_eq!(open_brace.kind, TokenKind::LBrace);

        let mut leading = self.collect_leading_trivia();
        let mut entries = Vec::new();

        loop {
            if self.peek_kind() == TokenKind::RBrace {
                break;
            }

            let key = self.parse_expr()?;
            let pre_colon = self.collect_leading_trivia();

            if self.peek_kind() != TokenKind::Colon {
                return Err(Self::error(
                    key.span().clone(),
                    Error::ExpectedMapColon {
                        context: Some("map entry"),
                    },
                ));
            }
            let colon_tok = self.next_token();

            let post_colon = self.collect_leading_trivia();
            let value = self.parse_expr()?;
            let trailing = self.collect_leading_trivia();

            let comma = if self.peek_kind() == TokenKind::Comma {
                let comma_tok = self.next_token();
                Some(comma_tok.span)
            } else {
                None
            };
            let has_comma = comma.is_some();

            entries.push(MapEntry {
                leading,
                key,
                pre_colon,
                colon: colon_tok.span,
                post_colon,
                value,
                trailing,
                comma,
            });

            leading = self.collect_leading_trivia();

            if !has_comma {
                break;
            }
        }

        let trailing = if entries.is_empty() {
            leading
        } else {
            self.collect_leading_trivia()
        };

        if self.peek_kind() != TokenKind::RBrace {
            return Err(Self::error(open_brace.span, Error::ExpectedMapEnd));
        }
        let close_brace = self.next_token();

        Ok(Expr::Map(MapExpr {
            span: Span {
                start: open_brace.span.start,
                end: close_brace.span.end,
                start_offset: open_brace.span.start_offset,
                end_offset: close_brace.span.end_offset,
            },
            open_brace: open_brace.span,
            leading: Trivia::empty(),
            entries,
            trailing,
            close_brace: close_brace.span,
        }))
    }

    /// Parse an identifier-starting expression.
    /// Could be: bool, option, named struct, enum variant.
    fn parse_ident_expr(&mut self) -> SpannedResult<Expr<'a>> {
        let ident_tok = self.next_token();
        debug_assert_eq!(ident_tok.kind, TokenKind::Ident);

        match ident_tok.text {
            "true" => Ok(Expr::Bool(BoolExpr {
                span: ident_tok.span,
                value: true,
            })),
            "false" => Ok(Expr::Bool(BoolExpr {
                span: ident_tok.span,
                value: false,
            })),
            "None" => Ok(Expr::Option(Box::new(OptionExpr {
                span: ident_tok.span,
                value: None,
            }))),
            "Some" => self.parse_some(ident_tok),
            "inf" | "NaN" => Ok(Expr::Number(NumberExpr {
                span: ident_tok.span,
                raw: Cow::Borrowed(ident_tok.text),
                kind: NumberKind::SpecialFloat,
            })),
            _ => self.parse_struct_or_variant(&ident_tok),
        }
    }

    /// Parse `Some(value)`.
    fn parse_some(&mut self, some_tok: Token<'a>) -> SpannedResult<Expr<'a>> {
        if self.peek_kind() != TokenKind::LParen {
            return Err(Self::error(some_tok.span, Error::ExpectedOption));
        }
        let open_paren = self.next_token();

        let leading = self.collect_leading_trivia();
        let expr = self.parse_expr()?;
        let trailing = self.collect_leading_trivia();

        if self.peek_kind() != TokenKind::RParen {
            return Err(Self::error(open_paren.span, Error::ExpectedOptionEnd));
        }
        let close_paren = self.next_token();

        Ok(Expr::Option(Box::new(OptionExpr {
            span: Span {
                start: some_tok.span.start,
                end: close_paren.span.end,
                start_offset: some_tok.span.start_offset,
                end_offset: close_paren.span.end_offset,
            },
            value: Some(OptionValue {
                open_paren: open_paren.span,
                leading,
                expr,
                trailing,
                close_paren: close_paren.span,
            }),
        })))
    }

    /// Parse a struct or enum variant: `Name(...)` or `Name`.
    ///
    /// In RON, named struct fields use paren syntax: `Point(x: 1, y: 2)`.
    /// Tuple structs also use parens: `Point(1, 2)`.
    /// The parser looks ahead to detect if the first element is `ident: value` (struct)
    /// or just a value (tuple).
    fn parse_struct_or_variant(&mut self, name_tok: &Token<'a>) -> SpannedResult<Expr<'a>> {
        let name = Ident {
            span: name_tok.span.clone(),
            name: Cow::Borrowed(name_tok.text),
        };

        // Collect trivia between name and body (e.g., "Point ( ... )" has space)
        let pre_body = self.collect_leading_trivia();

        let body = match self.peek_kind() {
            TokenKind::LParen => {
                let open_paren = self.next_token();
                let leading = self.collect_leading_trivia();

                // Check for empty tuple/struct
                if self.peek_kind() == TokenKind::RParen {
                    let close_paren = self.next_token();
                    Some(StructBody::Tuple(TupleBody {
                        open_paren: open_paren.span,
                        leading,
                        elements: Vec::new(),
                        trailing: Trivia::empty(),
                        close_paren: close_paren.span,
                    }))
                } else {
                    // Determine if this is named fields or tuple elements
                    // by looking at the first token and what follows
                    self.parse_struct_body_contents(open_paren, leading)?
                }
            }
            _ => None,
        };

        let span = if let Some(ref b) = body {
            let end_span = match b {
                StructBody::Tuple(t) => &t.close_paren,
                StructBody::Fields(f) => &f.close_brace,
            };
            Span {
                start: name_tok.span.start,
                end: end_span.end,
                start_offset: name_tok.span.start_offset,
                end_offset: end_span.end_offset,
            }
        } else {
            name_tok.span.clone()
        };

        Ok(Expr::Struct(StructExpr {
            span,
            name,
            pre_body,
            body,
        }))
    }

    /// Parse tuple elements until closing paren.
    fn parse_tuple_elements(
        &mut self,
        mut leading: Trivia<'a>,
    ) -> SpannedResult<Vec<TupleElement<'a>>> {
        let mut elements = Vec::new();

        loop {
            if self.peek_kind() == TokenKind::RParen {
                break;
            }

            let expr = self.parse_expr()?;
            let trailing = self.collect_leading_trivia();

            let comma = if self.peek_kind() == TokenKind::Comma {
                let comma_tok = self.next_token();
                Some(comma_tok.span)
            } else {
                None
            };
            let has_comma = comma.is_some();

            elements.push(TupleElement {
                leading,
                expr,
                trailing,
                comma,
            });

            leading = self.collect_leading_trivia();

            if !has_comma {
                break;
            }
        }

        Ok(elements)
    }

    /// Parse the body contents of a named struct: either named fields or tuple elements.
    /// Determines the type by looking at the first token: if it's `ident:`, it's named fields.
    fn parse_struct_body_contents(
        &mut self,
        open_paren: Token<'a>,
        leading: Trivia<'a>,
    ) -> SpannedResult<Option<StructBody<'a>>> {
        // Check if first token is an identifier
        if self.peek_kind() == TokenKind::Ident {
            // Consume the identifier
            let first_tok = self.next_token();
            let post_ident_trivia = self.collect_leading_trivia();

            // Check if followed by colon (named field)
            if self.peek_kind() == TokenKind::Colon {
                // Named fields: Point(x: 1, y: 2)
                let (fields, trailing) =
                    self.parse_struct_fields_from_first(leading, &first_tok, post_ident_trivia)?;

                if self.peek_kind() != TokenKind::RParen {
                    return Err(Self::error(open_paren.span, Error::ExpectedStructLikeEnd));
                }
                let close_paren = self.next_token();

                Ok(Some(StructBody::Fields(FieldsBody {
                    open_brace: open_paren.span,
                    leading: Trivia::empty(),
                    fields,
                    trailing,
                    close_brace: close_paren.span,
                })))
            } else {
                // Tuple elements: Point(x, y) where x is a struct/enum name
                // Convert the identifier to an expression
                let first_expr = self.ident_token_to_expr(first_tok, post_ident_trivia)?;
                let (elements, trailing) =
                    self.parse_tuple_elements_from_first(leading, first_expr)?;

                if self.peek_kind() != TokenKind::RParen {
                    return Err(Self::error(open_paren.span, Error::ExpectedStructLikeEnd));
                }
                let close_paren = self.next_token();

                Ok(Some(StructBody::Tuple(TupleBody {
                    open_paren: open_paren.span,
                    leading: Trivia::empty(),
                    elements,
                    trailing,
                    close_paren: close_paren.span,
                })))
            }
        } else {
            // Not starting with identifier, must be tuple elements
            let elements = self.parse_tuple_elements(leading)?;
            let trailing = self.collect_leading_trivia();

            if self.peek_kind() != TokenKind::RParen {
                return Err(Self::error(open_paren.span, Error::ExpectedStructLikeEnd));
            }
            let close_paren = self.next_token();

            Ok(Some(StructBody::Tuple(TupleBody {
                open_paren: open_paren.span,
                leading: Trivia::empty(),
                elements,
                trailing,
                close_paren: close_paren.span,
            })))
        }
    }

    /// Convert an identifier token to an expression.
    /// Handles booleans, None, Some, inf/NaN, and named structs.
    fn ident_token_to_expr(
        &mut self,
        tok: Token<'a>,
        pre_body: Trivia<'a>,
    ) -> SpannedResult<Expr<'a>> {
        match tok.text {
            "true" => Ok(Expr::Bool(BoolExpr {
                span: tok.span,
                value: true,
            })),
            "false" => Ok(Expr::Bool(BoolExpr {
                span: tok.span,
                value: false,
            })),
            "None" => Ok(Expr::Option(Box::new(OptionExpr {
                span: tok.span,
                value: None,
            }))),
            "Some" => self.parse_some(tok),
            "inf" | "NaN" => Ok(Expr::Number(NumberExpr {
                span: tok.span,
                raw: Cow::Borrowed(tok.text),
                kind: NumberKind::SpecialFloat,
            })),
            _ => {
                // Named struct/enum - check for body
                let name = Ident {
                    span: tok.span.clone(),
                    name: Cow::Borrowed(tok.text),
                };

                let body = match self.peek_kind() {
                    TokenKind::LParen => {
                        let open_paren = self.next_token();
                        let leading = self.collect_leading_trivia();

                        if self.peek_kind() == TokenKind::RParen {
                            let close_paren = self.next_token();
                            Some(StructBody::Tuple(TupleBody {
                                open_paren: open_paren.span,
                                leading,
                                elements: Vec::new(),
                                trailing: Trivia::empty(),
                                close_paren: close_paren.span,
                            }))
                        } else {
                            self.parse_struct_body_contents(open_paren, leading)?
                        }
                    }
                    _ => None,
                };

                let span = if let Some(ref b) = body {
                    let end_span = match b {
                        StructBody::Tuple(t) => &t.close_paren,
                        StructBody::Fields(f) => &f.close_brace,
                    };
                    Span {
                        start: tok.span.start,
                        end: end_span.end,
                        start_offset: tok.span.start_offset,
                        end_offset: end_span.end_offset,
                    }
                } else {
                    tok.span.clone()
                };

                Ok(Expr::Struct(StructExpr {
                    span,
                    name,
                    pre_body,
                    body,
                }))
            }
        }
    }

    /// Parse struct fields starting with the first field name already consumed.
    fn parse_struct_fields_from_first(
        &mut self,
        leading: Trivia<'a>,
        first_name: &Token<'a>,
        pre_colon: Trivia<'a>,
    ) -> SpannedResult<(Vec<StructField<'a>>, Trivia<'a>)> {
        let mut fields = Vec::new();

        // Parse the first field (name already consumed)
        let colon_tok = self.next_token();
        debug_assert_eq!(colon_tok.kind, TokenKind::Colon);

        let post_colon = self.collect_leading_trivia();
        let value = self.parse_expr()?;
        let trailing = self.collect_leading_trivia();

        let comma = if self.peek_kind() == TokenKind::Comma {
            let comma_tok = self.next_token();
            Some(comma_tok.span)
        } else {
            None
        };
        let has_comma = comma.is_some();

        fields.push(StructField {
            leading,
            name: Ident {
                span: first_name.span.clone(),
                name: Cow::Borrowed(first_name.text),
            },
            pre_colon,
            colon: colon_tok.span,
            post_colon,
            value,
            trailing,
            comma,
        });

        if !has_comma {
            return Ok((fields, self.collect_leading_trivia()));
        }

        // Parse remaining fields
        let mut leading = self.collect_leading_trivia();

        loop {
            if self.peek_kind() == TokenKind::RParen {
                break;
            }

            // Expect field name
            if self.peek_kind() != TokenKind::Ident {
                let tok = self.next_token();
                return Err(Self::error(tok.span, Error::ExpectedIdentifier));
            }
            let name_tok = self.next_token();

            let pre_colon = self.collect_leading_trivia();

            if self.peek_kind() != TokenKind::Colon {
                return Err(Self::error(name_tok.span, Error::ExpectedMapColon { context: Some("struct field") }));
            }
            let colon_tok = self.next_token();

            let post_colon = self.collect_leading_trivia();
            let value = self.parse_expr()?;
            let trailing = self.collect_leading_trivia();

            let comma = if self.peek_kind() == TokenKind::Comma {
                let comma_tok = self.next_token();
                Some(comma_tok.span)
            } else {
                None
            };
            let has_comma = comma.is_some();

            fields.push(StructField {
                leading,
                name: Ident {
                    span: name_tok.span.clone(),
                    name: Cow::Borrowed(name_tok.text),
                },
                pre_colon,
                colon: colon_tok.span,
                post_colon,
                value,
                trailing,
                comma,
            });

            leading = self.collect_leading_trivia();

            if !has_comma {
                break;
            }
        }

        Ok((fields, leading))
    }

    /// Parse tuple elements starting with the first expression already parsed.
    fn parse_tuple_elements_from_first(
        &mut self,
        leading: Trivia<'a>,
        first_expr: Expr<'a>,
    ) -> SpannedResult<(Vec<TupleElement<'a>>, Trivia<'a>)> {
        let mut elements = Vec::new();

        // Handle the first element
        let trailing = self.collect_leading_trivia();

        let comma = if self.peek_kind() == TokenKind::Comma {
            let comma_tok = self.next_token();
            Some(comma_tok.span)
        } else {
            None
        };
        let has_comma = comma.is_some();

        elements.push(TupleElement {
            leading,
            expr: first_expr,
            trailing,
            comma,
        });

        if !has_comma {
            return Ok((elements, self.collect_leading_trivia()));
        }

        // Parse remaining elements
        let mut leading = self.collect_leading_trivia();

        loop {
            if self.peek_kind() == TokenKind::RParen {
                break;
            }

            let expr = self.parse_expr()?;
            let trailing = self.collect_leading_trivia();

            let comma = if self.peek_kind() == TokenKind::Comma {
                let comma_tok = self.next_token();
                Some(comma_tok.span)
            } else {
                None
            };
            let has_comma = comma.is_some();

            elements.push(TupleElement {
                leading,
                expr,
                trailing,
                comma,
            });

            leading = self.collect_leading_trivia();

            if !has_comma {
                break;
            }
        }

        Ok((elements, leading))
    }

    /// Parse an integer literal.
    fn parse_integer(&mut self) -> Expr<'a> {
        let tok = self.next_token();
        debug_assert_eq!(tok.kind, TokenKind::Integer);

        let kind = if tok.text.starts_with('-') {
            NumberKind::NegativeInteger
        } else {
            NumberKind::Integer
        };

        Expr::Number(NumberExpr {
            span: tok.span,
            raw: Cow::Borrowed(tok.text),
            kind,
        })
    }

    /// Parse a float literal.
    fn parse_float(&mut self) -> Expr<'a> {
        let tok = self.next_token();
        debug_assert_eq!(tok.kind, TokenKind::Float);

        Expr::Number(NumberExpr {
            span: tok.span,
            raw: Cow::Borrowed(tok.text),
            kind: NumberKind::Float,
        })
    }

    /// Parse a string literal.
    fn parse_string(&mut self) -> SpannedResult<Expr<'a>> {
        let tok = self.next_token();
        debug_assert_eq!(tok.kind, TokenKind::String);

        let (value, kind) =
            Self::decode_string(tok.text).map_err(|e| Self::error(tok.span.clone(), e))?;

        Ok(Expr::String(StringExpr {
            span: tok.span,
            raw: Cow::Borrowed(tok.text),
            value,
            kind,
        }))
    }

    /// Decode a string literal.
    fn decode_string(raw: &str) -> Result<(String, StringKind)> {
        if raw.starts_with('r') {
            // Raw string
            let hash_count = raw.chars().skip(1).take_while(|&c| c == '#').count();
            let delim_len = 1 + hash_count + 1; // r + hashes + quote
            let content = &raw[delim_len..=raw.len() - delim_len];
            // Hash count is bounded by string length, which fits in u8 for practical use
            #[allow(clippy::cast_possible_truncation)]
            let hash_count_u8 = hash_count as u8;
            Ok((
                String::from(content),
                StringKind::Raw {
                    hash_count: hash_count_u8,
                },
            ))
        } else {
            // Regular string - need to process escapes
            let content = &raw[1..raw.len() - 1]; // Strip quotes
            let value = Self::unescape_string(content)?;
            Ok((value, StringKind::Regular))
        }
    }

    /// Process escape sequences in a string.
    fn unescape_string(s: &str) -> Result<String> {
        let mut result = String::new();
        let mut chars = s.chars().peekable();

        while let Some(c) = chars.next() {
            if c == '\\' {
                match chars.next() {
                    Some('n') => result.push('\n'),
                    Some('r') => result.push('\r'),
                    Some('t') => result.push('\t'),
                    Some('\\') => result.push('\\'),
                    Some('0') => result.push('\0'),
                    Some('"') => result.push('"'),
                    Some('\'') => result.push('\''),
                    Some('x') => {
                        let hex: String = chars.by_ref().take(2).collect();
                        let val = u8::from_str_radix(&hex, 16)
                            .map_err(|_| Error::InvalidEscape("invalid hex escape"))?;
                        result.push(val as char);
                    }
                    Some('u') => {
                        if chars.next() != Some('{') {
                            return Err(Error::InvalidEscape("expected { after \\u"));
                        }
                        let hex: String = chars.by_ref().take_while(|&c| c != '}').collect();
                        let val = u32::from_str_radix(&hex, 16)
                            .map_err(|_| Error::InvalidEscape("invalid unicode escape"))?;
                        let c = char::from_u32(val)
                            .ok_or(Error::InvalidEscape("invalid unicode codepoint"))?;
                        result.push(c);
                    }
                    Some(_) => return Err(Error::InvalidEscape("unknown escape sequence")),
                    None => return Err(Error::InvalidEscape("unexpected end of string")),
                }
            } else {
                result.push(c);
            }
        }

        Ok(result)
    }

    /// Parse a byte string literal.
    fn parse_bytes(&mut self) -> SpannedResult<Expr<'a>> {
        let tok = self.next_token();
        debug_assert_eq!(tok.kind, TokenKind::ByteString);

        let (value, kind) =
            Self::decode_byte_string(tok.text).map_err(|e| Self::error(tok.span.clone(), e))?;

        Ok(Expr::Bytes(BytesExpr {
            span: tok.span,
            raw: Cow::Borrowed(tok.text),
            value,
            kind,
        }))
    }

    /// Decode a byte string literal.
    fn decode_byte_string(raw: &str) -> Result<(Vec<u8>, BytesKind)> {
        if raw.starts_with("br") {
            // Raw byte string: br"..." or br#"..."#
            let hash_count = raw.chars().skip(2).take_while(|&c| c == '#').count();
            let start = 2 + hash_count + 1; // br + hashes + opening quote
            let end = raw.len() - 1 - hash_count; // closing quote + hashes
            let content = &raw[start..end];
            // Hash count is bounded by string length, which fits in u8 for practical use
            #[allow(clippy::cast_possible_truncation)]
            let hash_count_u8 = hash_count as u8;
            Ok((
                content.as_bytes().to_vec(),
                BytesKind::Raw {
                    hash_count: hash_count_u8,
                },
            ))
        } else {
            // Regular byte string b"..."
            let content = &raw[2..raw.len() - 1]; // Strip b" and "
            let value = Self::unescape_bytes(content)?;
            Ok((value, BytesKind::Regular))
        }
    }

    /// Process escape sequences in a byte string.
    fn unescape_bytes(s: &str) -> Result<Vec<u8>> {
        let mut result = Vec::new();
        let mut chars = s.chars().peekable();

        while let Some(c) = chars.next() {
            if c == '\\' {
                match chars.next() {
                    Some('n') => result.push(b'\n'),
                    Some('r') => result.push(b'\r'),
                    Some('t') => result.push(b'\t'),
                    Some('\\') => result.push(b'\\'),
                    Some('0') => result.push(0),
                    Some('"') => result.push(b'"'),
                    Some('\'') => result.push(b'\''),
                    Some('x') => {
                        let hex: String = chars.by_ref().take(2).collect();
                        let val = u8::from_str_radix(&hex, 16)
                            .map_err(|_| Error::InvalidEscape("invalid hex escape"))?;
                        result.push(val);
                    }
                    Some(_) => return Err(Error::InvalidEscape("unknown escape sequence")),
                    None => return Err(Error::InvalidEscape("unexpected end of string")),
                }
            } else if c.is_ascii() {
                result.push(c as u8);
            } else {
                return Err(Error::InvalidEscape("non-ASCII character in byte string"));
            }
        }

        Ok(result)
    }

    /// Parse a character literal.
    fn parse_char(&mut self) -> SpannedResult<Expr<'a>> {
        let tok = self.next_token();
        debug_assert_eq!(tok.kind, TokenKind::Char);

        // Check if it's a byte literal b'x'
        if tok.text.starts_with("b'") {
            let content = &tok.text[2..tok.text.len() - 1];
            let value =
                Self::unescape_byte_char(content).map_err(|e| Self::error(tok.span.clone(), e))?;
            return Ok(Expr::Byte(ByteExpr {
                span: tok.span,
                raw: Cow::Borrowed(tok.text),
                value,
            }));
        }

        // Regular char literal 'x'
        let content = &tok.text[1..tok.text.len() - 1];
        let value = Self::unescape_char(content).map_err(|e| Self::error(tok.span.clone(), e))?;

        Ok(Expr::Char(CharExpr {
            span: tok.span,
            raw: Cow::Borrowed(tok.text),
            value,
        }))
    }

    /// Unescape a single character.
    fn unescape_char(s: &str) -> Result<char> {
        let mut chars = s.chars();
        let c = chars.next().ok_or(Error::ExpectedChar)?;

        if c == '\\' {
            match chars.next() {
                Some('n') => Ok('\n'),
                Some('r') => Ok('\r'),
                Some('t') => Ok('\t'),
                Some('\\') => Ok('\\'),
                Some('0') => Ok('\0'),
                Some('\'') => Ok('\''),
                Some('x') => {
                    let hex: String = chars.take(2).collect();
                    let val = u8::from_str_radix(&hex, 16)
                        .map_err(|_| Error::InvalidEscape("invalid hex escape"))?;
                    Ok(val as char)
                }
                Some('u') => {
                    if chars.next() != Some('{') {
                        return Err(Error::InvalidEscape("expected { after \\u"));
                    }
                    let hex: String = chars.take_while(|&c| c != '}').collect();
                    let val = u32::from_str_radix(&hex, 16)
                        .map_err(|_| Error::InvalidEscape("invalid unicode escape"))?;
                    char::from_u32(val).ok_or(Error::InvalidEscape("invalid unicode codepoint"))
                }
                _ => Err(Error::InvalidEscape("unknown escape sequence")),
            }
        } else {
            Ok(c)
        }
    }

    /// Unescape a byte character.
    fn unescape_byte_char(s: &str) -> Result<u8> {
        let mut chars = s.chars();
        let c = chars.next().ok_or(Error::ExpectedByteLiteral)?;

        if c == '\\' {
            match chars.next() {
                Some('n') => Ok(b'\n'),
                Some('r') => Ok(b'\r'),
                Some('t') => Ok(b'\t'),
                Some('\\') => Ok(b'\\'),
                Some('0') => Ok(0),
                Some('\'') => Ok(b'\''),
                Some('x') => {
                    let hex: String = chars.take(2).collect();
                    u8::from_str_radix(&hex, 16)
                        .map_err(|_| Error::InvalidEscape("invalid hex escape"))
                }
                _ => Err(Error::InvalidEscape("unknown escape sequence")),
            }
        } else if c.is_ascii() {
            Ok(c as u8)
        } else {
            Err(Error::InvalidEscape("non-ASCII character in byte literal"))
        }
    }
}

#[cfg(test)]
#[allow(clippy::panic)]
mod tests {
    use super::*;

    #[test]
    fn parse_empty_document() {
        let doc = parse_document("").unwrap();
        assert!(doc.value.is_none());
        assert!(doc.attributes.is_empty());
    }

    #[test]
    fn parse_comment_only_document() {
        let doc = parse_document("// just a comment\n").unwrap();
        assert!(doc.value.is_none());
        assert!(!doc.leading.comments.is_empty());
    }

    #[test]
    fn parse_unit() {
        let doc = parse_document("()").unwrap();
        assert!(matches!(doc.value, Some(Expr::Unit(_))));
    }

    #[test]
    fn parse_bool_true() {
        let doc = parse_document("true").unwrap();
        match doc.value {
            Some(Expr::Bool(b)) => assert!(b.value),
            _ => panic!("expected bool"),
        }
    }

    #[test]
    fn parse_bool_false() {
        let doc = parse_document("false").unwrap();
        match doc.value {
            Some(Expr::Bool(b)) => assert!(!b.value),
            _ => panic!("expected bool"),
        }
    }

    #[test]
    fn parse_integer() {
        let doc = parse_document("42").unwrap();
        match doc.value {
            Some(Expr::Number(n)) => {
                assert_eq!(n.raw, "42");
                assert_eq!(n.kind, NumberKind::Integer);
            }
            _ => panic!("expected number"),
        }
    }

    #[test]
    fn parse_negative_integer() {
        let doc = parse_document("-42").unwrap();
        match doc.value {
            Some(Expr::Number(n)) => {
                assert_eq!(n.raw, "-42");
                assert_eq!(n.kind, NumberKind::NegativeInteger);
            }
            _ => panic!("expected number"),
        }
    }

    #[test]
    fn parse_float() {
        let doc = parse_document("3.14").unwrap();
        match doc.value {
            Some(Expr::Number(n)) => {
                assert_eq!(n.raw, "3.14");
                assert_eq!(n.kind, NumberKind::Float);
            }
            _ => panic!("expected number"),
        }
    }

    #[test]
    fn parse_string() {
        let doc = parse_document(r#""hello""#).unwrap();
        match doc.value {
            Some(Expr::String(s)) => {
                assert_eq!(s.value, "hello");
                assert_eq!(s.kind, StringKind::Regular);
            }
            _ => panic!("expected string"),
        }
    }

    #[test]
    fn parse_string_with_escapes() {
        let doc = parse_document(r#""hello\nworld""#).unwrap();
        match doc.value {
            Some(Expr::String(s)) => {
                assert_eq!(s.value, "hello\nworld");
            }
            _ => panic!("expected string"),
        }
    }

    #[test]
    fn parse_raw_string() {
        let doc = parse_document(r##"r#"raw string"#"##).unwrap();
        match doc.value {
            Some(Expr::String(s)) => {
                assert_eq!(s.value, "raw string");
                assert!(matches!(s.kind, StringKind::Raw { hash_count: 1 }));
            }
            _ => panic!("expected string"),
        }
    }

    #[test]
    fn parse_char() {
        let doc = parse_document("'a'").unwrap();
        match doc.value {
            Some(Expr::Char(c)) => {
                assert_eq!(c.value, 'a');
            }
            _ => panic!("expected char"),
        }
    }

    #[test]
    fn parse_none() {
        let doc = parse_document("None").unwrap();
        match doc.value {
            Some(Expr::Option(opt)) => assert!(opt.value.is_none()),
            _ => panic!("expected option"),
        }
    }

    #[test]
    fn parse_some() {
        let doc = parse_document("Some(42)").unwrap();
        match doc.value {
            Some(Expr::Option(opt)) => {
                assert!(opt.value.is_some());
            }
            _ => panic!("expected option"),
        }
    }

    #[test]
    fn parse_seq() {
        let doc = parse_document("[1, 2, 3]").unwrap();
        match doc.value {
            Some(Expr::Seq(seq)) => {
                assert_eq!(seq.items.len(), 3);
            }
            _ => panic!("expected seq"),
        }
    }

    #[test]
    fn parse_empty_seq() {
        let doc = parse_document("[]").unwrap();
        match doc.value {
            Some(Expr::Seq(seq)) => {
                assert!(seq.items.is_empty());
            }
            _ => panic!("expected seq"),
        }
    }

    #[test]
    fn parse_map() {
        let doc = parse_document(r#"{"a": 1, "b": 2}"#).unwrap();
        match doc.value {
            Some(Expr::Map(map)) => {
                assert_eq!(map.entries.len(), 2);
            }
            _ => panic!("expected map"),
        }
    }

    #[test]
    fn parse_tuple() {
        let doc = parse_document("(1, 2, 3)").unwrap();
        match doc.value {
            Some(Expr::Tuple(tuple)) => {
                assert_eq!(tuple.elements.len(), 3);
            }
            _ => panic!("expected tuple"),
        }
    }

    #[test]
    fn parse_struct() {
        let doc = parse_document("Point(1, 2)").unwrap();
        match doc.value {
            Some(Expr::Struct(s)) => {
                assert_eq!(s.name.name, "Point");
                assert!(matches!(s.body, Some(StructBody::Tuple(_))));
            }
            _ => panic!("expected struct"),
        }
    }

    #[test]
    fn parse_brace_after_ident_is_separate_map() {
        // Braces after an identifier are NOT struct syntax - they're a separate map
        // "Point { x: 1 }" should fail because it parses as "Point" followed by a map,
        // but maps require string keys, not identifiers
        let result = parse_document("Point { x: 1 }");
        assert!(result.is_err(), "Brace syntax after ident should not be valid");
    }

    #[test]
    fn parse_struct_with_fields_parens() {
        // RON standard syntax for named fields uses parens: Point(x: 1, y: 2)
        let doc = parse_document("Point(x: 1, y: 2)").unwrap();
        match doc.value {
            Some(Expr::Struct(s)) => {
                assert_eq!(s.name.name, "Point");
                match s.body {
                    Some(StructBody::Fields(f)) => {
                        assert_eq!(f.fields.len(), 2);
                        assert_eq!(f.fields[0].name.name, "x");
                        assert_eq!(f.fields[1].name.name, "y");
                    }
                    _ => panic!("expected fields body, got {:?}", s.body),
                }
            }
            _ => panic!("expected struct"),
        }
    }

    #[test]
    fn parse_preserves_trivia() {
        let source = "// header comment\n42 // inline comment";
        let doc = parse_document(source).unwrap();
        assert!(!doc.leading.comments.is_empty());
    }

    #[test]
    fn parse_attribute() {
        let doc = parse_document(r"#![enable(unwrap_newtypes)] 42").unwrap();
        assert_eq!(doc.attributes.len(), 1);
        assert_eq!(doc.attributes[0].name, "enable");
        match &doc.attributes[0].content {
            AttributeContent::Args(args) => {
                assert_eq!(args, &["unwrap_newtypes"]);
            }
            _ => panic!("expected args"),
        }
    }

    // =========================================================================
    // Anonymous struct parsing tests
    // =========================================================================

    #[test]
    fn parse_anon_struct_simple() {
        // Basic anonymous struct with named fields
        let doc = parse_document(r#"(name: "test", value: 42)"#).unwrap();
        match doc.value {
            Some(Expr::AnonStruct(s)) => {
                assert_eq!(s.fields.len(), 2);
                assert_eq!(s.fields[0].name.name, "name");
                assert_eq!(s.fields[1].name.name, "value");
            }
            _ => panic!("expected anonymous struct, got {:?}", doc.value),
        }
    }

    #[test]
    fn parse_anon_struct_single_field() {
        // Anonymous struct with a single field
        let doc = parse_document("(x: 1)").unwrap();
        match doc.value {
            Some(Expr::AnonStruct(s)) => {
                assert_eq!(s.fields.len(), 1);
                assert_eq!(s.fields[0].name.name, "x");
            }
            _ => panic!("expected anonymous struct, got {:?}", doc.value),
        }
    }

    #[test]
    fn parse_anon_struct_trailing_comma() {
        // Anonymous struct with trailing comma
        let doc = parse_document("(x: 1, y: 2,)").unwrap();
        match doc.value {
            Some(Expr::AnonStruct(s)) => {
                assert_eq!(s.fields.len(), 2);
                assert!(s.fields[1].comma.is_some());
            }
            _ => panic!("expected anonymous struct"),
        }
    }

    #[test]
    fn parse_anon_struct_nested_values() {
        // Anonymous struct with nested complex values
        let doc = parse_document(r"(items: [1, 2, 3], config: (enabled: true))").unwrap();
        match doc.value {
            Some(Expr::AnonStruct(s)) => {
                assert_eq!(s.fields.len(), 2);
                assert_eq!(s.fields[0].name.name, "items");
                assert_eq!(s.fields[1].name.name, "config");
                // Check that the nested value is also an anonymous struct
                match &s.fields[1].value {
                    Expr::AnonStruct(inner) => {
                        assert_eq!(inner.fields.len(), 1);
                        assert_eq!(inner.fields[0].name.name, "enabled");
                    }
                    _ => panic!("expected nested anonymous struct"),
                }
            }
            _ => panic!("expected anonymous struct"),
        }
    }

    #[test]
    fn parse_empty_parens_is_unit() {
        // Empty parentheses should produce Unit, not AnonStruct
        let doc = parse_document("()").unwrap();
        assert!(matches!(doc.value, Some(Expr::Unit(_))));
    }

    #[test]
    fn parse_tuple_not_anon_struct() {
        // Tuple values (no colons) should produce Tuple, not AnonStruct
        let doc = parse_document("(1, 2, 3)").unwrap();
        match doc.value {
            Some(Expr::Tuple(t)) => {
                assert_eq!(t.elements.len(), 3);
            }
            _ => panic!("expected tuple, got {:?}", doc.value),
        }
    }

    #[test]
    fn parse_single_element_tuple() {
        // Single value in parens without colon should be a tuple
        let doc = parse_document("(x)").unwrap();
        match doc.value {
            Some(Expr::Tuple(t)) => {
                assert_eq!(t.elements.len(), 1);
                // The element should be a struct (identifier `x`)
                match &t.elements[0].expr {
                    Expr::Struct(s) => {
                        assert_eq!(s.name.name, "x");
                        assert!(s.body.is_none());
                    }
                    _ => panic!("expected struct/identifier"),
                }
            }
            _ => panic!("expected tuple, got {:?}", doc.value),
        }
    }

    #[test]
    fn parse_anon_struct_vs_single_element_tuple() {
        // `(x: 1)` should be anonymous struct (has colon)
        let anon = parse_document("(x: 1)").unwrap();
        assert!(matches!(anon.value, Some(Expr::AnonStruct(_))));

        // `(x)` should be tuple (no colon)
        let tuple = parse_document("(x)").unwrap();
        assert!(matches!(tuple.value, Some(Expr::Tuple(_))));
    }

    #[test]
    fn parse_anon_struct_with_whitespace() {
        // Anonymous struct with various whitespace
        let doc = parse_document("( x : 1 , y : 2 )").unwrap();
        match doc.value {
            Some(Expr::AnonStruct(s)) => {
                assert_eq!(s.fields.len(), 2);
                assert_eq!(s.fields[0].name.name, "x");
                assert_eq!(s.fields[1].name.name, "y");
            }
            _ => panic!("expected anonymous struct"),
        }
    }

    #[test]
    fn parse_anon_struct_multiline() {
        // Anonymous struct with newlines
        let doc = parse_document("(\n  x: 1,\n  y: 2\n)").unwrap();
        match doc.value {
            Some(Expr::AnonStruct(s)) => {
                assert_eq!(s.fields.len(), 2);
            }
            _ => panic!("expected anonymous struct"),
        }
    }

    #[test]
    fn parse_anon_struct_with_comments() {
        // Anonymous struct with comments
        let doc = parse_document("(\n  // comment\n  x: 1\n)").unwrap();
        match doc.value {
            Some(Expr::AnonStruct(s)) => {
                assert_eq!(s.fields.len(), 1);
                assert_eq!(s.fields[0].name.name, "x");
            }
            _ => panic!("expected anonymous struct"),
        }
    }

    #[test]
    fn parse_anon_struct_field_values() {
        // Verify various field value types are parsed correctly
        let doc = parse_document(
            r#"(
            bool_field: true,
            int_field: 42,
            str_field: "hello",
            option_field: Some(1),
            seq_field: [1, 2],
            map_field: {"a": 1}
        )"#,
        )
        .unwrap();
        match doc.value {
            Some(Expr::AnonStruct(s)) => {
                assert_eq!(s.fields.len(), 6);
                assert!(matches!(s.fields[0].value, Expr::Bool(_)));
                assert!(matches!(s.fields[1].value, Expr::Number(_)));
                assert!(matches!(s.fields[2].value, Expr::String(_)));
                assert!(matches!(s.fields[3].value, Expr::Option(_)));
                assert!(matches!(s.fields[4].value, Expr::Seq(_)));
                assert!(matches!(s.fields[5].value, Expr::Map(_)));
            }
            _ => panic!("expected anonymous struct"),
        }
    }
}
