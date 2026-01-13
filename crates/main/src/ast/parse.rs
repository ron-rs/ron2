//! AST parser for RON documents.
//!
//! This module provides parsing of RON source into a full AST that preserves
//! all trivia (whitespace and comments) for round-trip editing.

use alloc::{borrow::Cow, boxed::Box, vec::Vec};
use core::iter::Peekable;

use super::unescape;
use crate::{
    ast::{
        AnonStructExpr, Attribute, AttributeContent, BoolExpr, ByteExpr, BytesExpr, CharExpr,
        Comment, CommentKind, Document, ErrorExpr, Expr, FieldsBody, Ident, MapEntry, MapExpr,
        NumberExpr, NumberKind, OptionExpr, OptionValue, SeqExpr, SeqItem, StringExpr, StructBody,
        StructExpr, StructField, Trivia, TupleBody, TupleElement, TupleExpr, UnitExpr,
    },
    error::{Error, ErrorKind, Position, Result, Span},
    lexer::Lexer,
    token::{Token, TokenKind},
};

/// Parse RON source into an AST document.
///
/// This is the main entry point for AST parsing. It returns a fully-formed
/// AST with all trivia preserved.
///
/// # Errors
///
/// Returns an error if the source contains invalid RON syntax.
pub fn parse_document(source: &str) -> Result<Document<'_>> {
    let lexer = Lexer::new(source).with_trivia(true);
    let mut parser = AstParser::new(source, lexer);
    parser.parse_document()
}

/// Parse RON source into an AST document with error recovery.
///
/// Returns the parsed document plus any errors encountered.
#[must_use]
pub fn parse_document_lossy(source: &str) -> (Document<'_>, Vec<Error>) {
    let lexer = Lexer::new(source).with_trivia(true);
    let mut parser = AstParser::new(source, lexer);
    parser.parse_document_lossy()
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

    /// Peek at the span of the next non-trivia token without consuming.
    fn peek_span(&mut self) -> Span {
        if let Some(tok) = self.lookahead.last() {
            return tok.span.clone();
        }

        while let Some(tok) = self.tokens.peek() {
            if tok.kind.is_trivia() {
                if let Some(tok) = self.tokens.next() {
                    self.trivia_buffer.push(tok);
                }
            } else {
                return tok.span.clone();
            }
        }

        self.eof_span()
    }

    /// Create a zero-width span at the end of the given span.
    fn span_at_end(span: &Span) -> Span {
        Span {
            start: span.end,
            end: span.end,
            start_offset: span.end_offset,
            end_offset: span.end_offset,
        }
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

    /// Consume a comma if present. Returns `(span, has_comma)`.
    fn consume_comma(&mut self) -> (Option<Span>, bool) {
        if self.peek_kind() == TokenKind::Comma {
            let comma_tok = self.next_token();
            (Some(comma_tok.span), true)
        } else {
            (None, false)
        }
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
    fn error(span: Span, kind: ErrorKind) -> Error {
        Error::with_span(kind, span)
    }

    /// Parse a complete document.
    fn parse_document(&mut self) -> Result<Document<'a>> {
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
            return Err(Self::error(tok.span, ErrorKind::TrailingCharacters));
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

    /// Parse a complete document with error recovery.
    fn parse_document_lossy(&mut self) -> (Document<'a>, Vec<Error>) {
        let mut errors = Vec::new();

        let leading = self.collect_leading_trivia();
        let attributes = self.parse_attributes_lossy(&mut errors);
        let pre_value = self.collect_leading_trivia();

        let value = match self.peek_kind() {
            TokenKind::Eof => None,
            _ => Some(self.parse_expr_lossy(&mut errors)),
        };

        let trailing = self.collect_leading_trivia();

        if self.peek_kind() != TokenKind::Eof {
            let tok = self.next_token();
            errors.push(Self::error(tok.span, ErrorKind::TrailingCharacters));
            self.recover_until(&[TokenKind::Eof]);
        }

        (
            Document {
                source: Cow::Borrowed(self.source),
                leading,
                attributes,
                pre_value,
                value,
                trailing,
            },
            errors,
        )
    }

    /// Collect leading trivia without consuming a non-trivia token.
    fn collect_leading_trivia(&mut self) -> Trivia<'a> {
        // Peek to force trivia collection
        let _ = self.peek_kind();
        self.drain_trivia()
    }

    /// Parse inner attributes (`#![...]`).
    fn parse_attributes(&mut self) -> Result<Vec<Attribute<'a>>> {
        let mut attributes = Vec::new();

        while self.peek_kind() == TokenKind::Hash {
            let leading = self.drain_trivia();
            let attr = self.parse_attribute(leading)?;
            attributes.push(attr);
        }

        Ok(attributes)
    }

    /// Parse inner attributes (`#![...]`) with error recovery.
    fn parse_attributes_lossy(&mut self, errors: &mut Vec<Error>) -> Vec<Attribute<'a>> {
        let mut attributes = Vec::new();

        while self.peek_kind() == TokenKind::Hash {
            let leading = self.drain_trivia();
            match self.parse_attribute(leading) {
                Ok(attr) => attributes.push(attr),
                Err(err) => {
                    errors.push(err);
                    self.recover_until(&[TokenKind::RBracket, TokenKind::Eof]);
                    if self.peek_kind() == TokenKind::RBracket {
                        let _ = self.next_token();
                    }
                }
            }
        }

        attributes
    }

    /// Parse a single attribute.
    fn parse_attribute(&mut self, leading: Trivia<'a>) -> Result<Attribute<'a>> {
        let hash = self.next_token();
        debug_assert_eq!(hash.kind, TokenKind::Hash);

        // Expect `!`
        if self.peek_kind() != TokenKind::Bang {
            return Err(Self::error(hash.span, Self::expected("attribute", None)));
        }
        let _bang = self.next_token();

        // Expect `[`
        if self.peek_kind() != TokenKind::LBracket {
            return Err(Self::error(hash.span, Self::expected("attribute", None)));
        }
        let _lbracket = self.next_token();

        // Expect identifier
        if self.peek_kind() != TokenKind::Ident {
            return Err(Self::error(hash.span, Self::expected("identifier", None)));
        }
        let name_tok = self.next_token();
        let name = name_tok.text;

        // Parse content based on what follows
        let content = match self.peek_kind() {
            TokenKind::Eq => {
                let _eq = self.next_token();
                // Expect string value
                if self.peek_kind() != TokenKind::String {
                    return Err(Self::error(name_tok.span, Self::expected("string", None)));
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
                            return Err(Self::error(tok.span, Self::expected("identifier", None)));
                        }
                    }
                }

                // Expect `)`
                if self.peek_kind() != TokenKind::RParen {
                    return Err(Self::error(
                        name_tok.span,
                        Self::expected("closing `]`", Some("attribute")),
                    ));
                }
                let _rparen = self.next_token();

                AttributeContent::Args(args)
            }
            _ => AttributeContent::None,
        };

        // Expect `]`
        if self.peek_kind() != TokenKind::RBracket {
            return Err(Self::error(
                hash.span,
                Self::expected("closing `]`", Some("attribute")),
            ));
        }
        let rbracket = self.next_token();

        Ok(Attribute {
            span: Span::between(&hash.span, &rbracket.span),
            leading,
            name: Cow::Borrowed(name),
            content,
        })
    }

    /// Parse an expression.
    fn parse_expr(&mut self) -> Result<Expr<'a>> {
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
            TokenKind::Eof => Err(Self::error(self.eof_span(), ErrorKind::Eof)),
            TokenKind::Error => {
                let tok = self.next_token();
                Err(Self::error_for_error_token(&tok))
            }
            _ => {
                let tok = self.next_token();
                Err(Self::error(
                    tok.span,
                    ErrorKind::UnexpectedChar(tok.text.chars().next().unwrap_or('?')),
                ))
            }
        }
    }

    /// Parse an expression with error recovery.
    fn parse_expr_lossy(&mut self, errors: &mut Vec<Error>) -> Expr<'a> {
        match self.peek_kind() {
            TokenKind::LParen => self.parse_tuple_or_unit_lossy(errors),
            TokenKind::LBracket => self.parse_seq_lossy(errors),
            TokenKind::LBrace => self.parse_map_lossy(errors),
            TokenKind::Ident => self.parse_ident_expr_lossy(errors),
            TokenKind::Integer => self.parse_integer(),
            TokenKind::Float => self.parse_float(),
            TokenKind::String => match self.parse_string() {
                Ok(expr) => expr,
                Err(err) => self.error_expr_from(err, errors),
            },
            TokenKind::ByteString => match self.parse_bytes() {
                Ok(expr) => expr,
                Err(err) => self.error_expr_from(err, errors),
            },
            TokenKind::Char => match self.parse_char() {
                Ok(expr) => expr,
                Err(err) => self.error_expr_from(err, errors),
            },
            TokenKind::Eof => {
                let err = Self::error(self.eof_span(), ErrorKind::Eof);
                self.error_expr_from(err, errors)
            }
            TokenKind::Error => {
                let tok = self.next_token();
                let err = Self::error_for_error_token(&tok);
                self.error_expr_from(err, errors)
            }
            _ => {
                let tok = self.next_token();
                let err = Self::error(
                    tok.span,
                    ErrorKind::UnexpectedChar(tok.text.chars().next().unwrap_or('?')),
                );
                self.error_expr_from(err, errors)
            }
        }
    }

    /// Create an `ErrorKind::Expected` with optional context.
    fn expected(msg: &'static str, ctx: Option<&'static str>) -> ErrorKind {
        ErrorKind::Expected {
            expected: Cow::Borrowed(msg),
            context: ctx,
        }
    }

    /// Classify an error token and create the appropriate error.
    ///
    /// This handles detection of specific error types based on token content:
    /// - Unclosed block comments
    /// - Unclosed strings
    /// - Invalid numbers with base prefixes
    /// - Other unexpected characters
    fn error_for_error_token(tok: &Token<'_>) -> Error {
        debug_assert_eq!(tok.kind, TokenKind::Error);

        if tok.text.starts_with("/*") {
            Self::error(tok.span.clone(), ErrorKind::UnclosedBlockComment)
        } else if tok.text.starts_with('"') || tok.text.starts_with("r#") {
            Self::error(tok.span.clone(), ErrorKind::ExpectedStringEnd)
        } else if tok.text.starts_with("0x")
            || tok.text.starts_with("0X")
            || tok.text.starts_with("0b")
            || tok.text.starts_with("0B")
            || tok.text.starts_with("0o")
            || tok.text.starts_with("0O")
        {
            // Invalid number with base prefix - point to position after prefix
            let prefix_len = 2;
            let error_span = Span {
                start: Position {
                    line: tok.span.start.line,
                    col: tok.span.start.col + prefix_len,
                },
                end: tok.span.end,
                start_offset: tok.span.start_offset + prefix_len,
                end_offset: tok.span.end_offset,
            };
            let invalid_char = tok.text[prefix_len..].chars().next().unwrap_or('?');
            Self::error(error_span, ErrorKind::UnexpectedChar(invalid_char))
        } else {
            Self::error(
                tok.span.clone(),
                ErrorKind::UnexpectedChar(tok.text.chars().next().unwrap_or('?')),
            )
        }
    }

    fn error_expr_from(&mut self, err: Error, errors: &mut Vec<Error>) -> Expr<'a> {
        let span = err.span().clone();
        let error = err.clone();
        errors.push(err);
        self.recover_until(&[
            TokenKind::Comma,
            TokenKind::RParen,
            TokenKind::RBracket,
            TokenKind::RBrace,
        ]);
        Expr::Error(ErrorExpr { span, error })
    }

    /// Advance the parser until a synchronization token or EOF is found.
    fn recover_until(&mut self, sync: &[TokenKind]) {
        self.trivia_buffer.clear();
        loop {
            let kind = self.peek_kind();
            if kind == TokenKind::Eof || sync.contains(&kind) {
                break;
            }
            let _ = self.next_token();
        }
    }

    /// Consume a closing delimiter or recover to it, returning a synthetic span if missing.
    fn consume_closing(
        &mut self,
        expected: TokenKind,
        error_kind: ErrorKind,
        errors: &mut Vec<Error>,
    ) -> Span {
        if self.peek_kind() == expected {
            return self.next_token().span;
        }

        errors.push(Self::error(self.peek_span(), error_kind));
        self.recover_until(&[expected, TokenKind::Eof]);
        if self.peek_kind() == expected {
            return self.next_token().span;
        }

        self.eof_span()
    }

    /// Consume a closing delimiter in strict mode, returning an error if missing.
    fn consume_closing_strict(
        &mut self,
        expected: TokenKind,
        error_kind: ErrorKind,
    ) -> Result<Span> {
        if self.peek_kind() == expected {
            Ok(self.next_token().span)
        } else {
            Err(Self::error(self.eof_span(), error_kind))
        }
    }

    /// Parse a tuple `(a, b, c)` or unit `()`.
    fn parse_tuple_or_unit(&mut self) -> Result<Expr<'a>> {
        let open_paren = self.next_token();
        debug_assert_eq!(open_paren.kind, TokenKind::LParen);

        let leading = self.collect_leading_trivia();

        // Check for empty tuple (unit)
        if self.peek_kind() == TokenKind::RParen {
            let close_paren = self.next_token();
            return Ok(Expr::Unit(UnitExpr {
                span: Span::between(&open_paren.span, &close_paren.span),
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

    /// Parse a tuple `(a, b, c)` or unit `()` with error recovery.
    fn parse_tuple_or_unit_lossy(&mut self, errors: &mut Vec<Error>) -> Expr<'a> {
        let open_paren = self.next_token();
        debug_assert_eq!(open_paren.kind, TokenKind::LParen);

        let leading = self.collect_leading_trivia();

        if self.peek_kind() == TokenKind::RParen {
            let close_paren = self.next_token();
            return Expr::Unit(UnitExpr {
                span: Span::between(&open_paren.span, &close_paren.span),
            });
        }

        let is_named_fields = self.peek_is_named_field();

        if is_named_fields {
            self.parse_fields_body_inner_lossy(open_paren, leading, errors)
        } else {
            self.parse_tuple_inner_lossy(open_paren, leading, errors)
        }
    }

    /// Check if the next tokens look like `ident:` (a named field).
    fn peek_is_named_field(&mut self) -> bool {
        // Check if we have `ident:` pattern using two-token lookahead
        let (first, second) = self.peek_two_kinds();
        first == TokenKind::Ident && second == TokenKind::Colon
    }

    /// Try to parse an empty tuple body `()`.
    ///
    /// If the next token is `)`, consumes it and returns `Ok` with an empty `TupleBody`.
    /// Otherwise returns `Err` with the trivia passed back without consuming any tokens.
    fn try_parse_empty_tuple_body(
        &mut self,
        open_paren: &Span,
        leading: Trivia<'a>,
    ) -> Result<TupleBody<'a>, Trivia<'a>> {
        if self.peek_kind() == TokenKind::RParen {
            let close_paren = self.next_token();
            Ok(TupleBody {
                open_paren: open_paren.clone(),
                leading,
                elements: Vec::new(),
                trailing: Trivia::empty(),
                close_paren: close_paren.span,
            })
        } else {
            Err(leading)
        }
    }

    /// Parse a tuple expression starting after the opening paren.
    fn parse_tuple_inner(
        &mut self,
        open_paren: Token<'a>,
        mut leading: Trivia<'a>,
    ) -> Result<Expr<'a>> {
        let mut elements = Vec::new();

        loop {
            if self.peek_kind() == TokenKind::RParen {
                break;
            }

            let expr = self.parse_expr()?;
            let trailing = self.collect_leading_trivia();

            let (comma, has_comma) = self.consume_comma();

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

        let close_paren = self.consume_closing_strict(
            TokenKind::RParen,
            Self::expected("closing `)` or `}`", Some("struct")),
        )?;

        Ok(Expr::Tuple(TupleExpr {
            span: Span::between(&open_paren.span, &close_paren),
            open_paren: open_paren.span,
            leading: Trivia::empty(),
            elements,
            trailing,
            close_paren,
        }))
    }

    /// Parse a tuple expression with error recovery.
    fn parse_tuple_inner_lossy(
        &mut self,
        open_paren: Token<'a>,
        mut leading: Trivia<'a>,
        errors: &mut Vec<Error>,
    ) -> Expr<'a> {
        let mut elements = Vec::new();

        loop {
            match self.peek_kind() {
                TokenKind::RParen | TokenKind::Eof => break,
                _ => {}
            }

            let expr = self.parse_expr_lossy(errors);
            let trailing = self.collect_leading_trivia();

            let (comma, has_comma) = self.consume_comma();

            elements.push(TupleElement {
                leading,
                expr,
                trailing,
                comma,
            });

            leading = self.collect_leading_trivia();

            if !has_comma {
                if matches!(self.peek_kind(), TokenKind::RParen | TokenKind::Eof) {
                    break;
                }
                errors.push(Self::error(
                    self.peek_span(),
                    Self::expected("comma", Some("tuple")),
                ));
            }
        }

        let trailing = if elements.is_empty() {
            leading
        } else {
            self.collect_leading_trivia()
        };

        let close_paren = self.consume_closing(
            TokenKind::RParen,
            Self::expected("closing `)` or `}`", Some("struct")),
            errors,
        );

        Expr::Tuple(TupleExpr {
            span: Span::between(&open_paren.span, &close_paren),
            open_paren: open_paren.span,
            leading: Trivia::empty(),
            elements,
            trailing,
            close_paren,
        })
    }

    /// Parse a fields body starting after opening paren.
    fn parse_fields_body_inner(
        &mut self,
        open_paren: Token<'a>,
        mut leading: Trivia<'a>,
    ) -> Result<Expr<'a>> {
        let mut fields = Vec::new();

        loop {
            if self.peek_kind() == TokenKind::RParen {
                break;
            }

            // Expect field name
            if self.peek_kind() != TokenKind::Ident {
                let tok = self.next_token();
                return Err(Self::error(
                    tok.span,
                    Self::expected("identifier", None),
                ));
            }
            let name_tok = self.next_token();

            let pre_colon = self.collect_leading_trivia();

            // Expect colon
            if self.peek_kind() != TokenKind::Colon {
                return Err(Self::error(
                    name_tok.span,
                    Self::expected("`:` after map key", Some("struct field")),
                ));
            }
            let colon_tok = self.next_token();

            let post_colon = self.collect_leading_trivia();

            // Parse value
            let value = self.parse_expr()?;
            let trailing = self.collect_leading_trivia();

            let (comma, has_comma) = self.consume_comma();

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

        let close_paren = self.consume_closing_strict(
            TokenKind::RParen,
            Self::expected("closing `)` or `}`", Some("struct")),
        )?;

        // Anonymous struct with fields
        Ok(Expr::AnonStruct(AnonStructExpr {
            span: Span::between(&open_paren.span, &close_paren),
            open_paren: open_paren.span,
            leading: Trivia::empty(),
            fields,
            trailing,
            close_paren,
        }))
    }

    /// Parse a fields body starting after opening paren with error recovery.
    #[allow(clippy::too_many_lines)]
    fn parse_fields_body_inner_lossy(
        &mut self,
        open_paren: Token<'a>,
        mut leading: Trivia<'a>,
        errors: &mut Vec<Error>,
    ) -> Expr<'a> {
        let mut fields = Vec::new();

        loop {
            match self.peek_kind() {
                TokenKind::RParen | TokenKind::Eof => break,
                TokenKind::Ident => {}
                _ => {
                    let error_span = self.peek_span();
                    errors.push(Self::error(
                        error_span.clone(),
                        Self::expected("identifier", None),
                    ));
                    self.recover_until(&[TokenKind::Comma, TokenKind::RParen]);
                    let trailing = self.collect_leading_trivia();
                    let (comma, has_comma) = self.consume_comma();
                    // Create a placeholder field to preserve structure
                    fields.push(StructField {
                        leading,
                        name: Ident {
                            span: error_span.clone(),
                            name: Cow::Borrowed(""),
                        },
                        pre_colon: Trivia::empty(),
                        colon: Self::span_at_end(&error_span),
                        post_colon: Trivia::empty(),
                        value: Expr::Error(ErrorExpr {
                            span: error_span.clone(),
                            error: Error::with_span(
                                Self::expected("identifier", None),
                                error_span,
                            ),
                        }),
                        trailing,
                        comma,
                    });
                    leading = self.collect_leading_trivia();
                    if !has_comma {
                        break;
                    }
                    continue;
                }
            }

            let name_tok = self.next_token();
            let pre_colon = self.collect_leading_trivia();

            let colon = if self.peek_kind() == TokenKind::Colon {
                self.next_token().span
            } else {
                errors.push(Self::error(
                    name_tok.span.clone(),
                    Self::expected("`:` after map key", Some("struct field")),
                ));
                Self::span_at_end(&name_tok.span)
            };

            let post_colon = self.collect_leading_trivia();
            let value = match self.peek_kind() {
                TokenKind::Comma | TokenKind::RParen => {
                    let error_span = Self::span_at_end(&colon);
                    let error_kind = Self::expected("value", Some("struct field"));
                    errors.push(Self::error(error_span.clone(), error_kind.clone()));
                    Expr::Error(ErrorExpr {
                        span: error_span.clone(),
                        error: Error::with_span(error_kind, error_span),
                    })
                }
                _ => self.parse_expr_lossy(errors),
            };
            let trailing = self.collect_leading_trivia();

            let (comma, has_comma) = self.consume_comma();

            fields.push(StructField {
                leading,
                name: Ident {
                    span: name_tok.span.clone(),
                    name: Cow::Borrowed(name_tok.text),
                },
                pre_colon,
                colon,
                post_colon,
                value,
                trailing,
                comma,
            });

            leading = self.collect_leading_trivia();

            if !has_comma {
                if matches!(self.peek_kind(), TokenKind::Ident) {
                    errors.push(Self::error(
                        self.peek_span(),
                        Self::expected("comma", Some("struct")),
                    ));
                } else {
                    break;
                }
            }
        }

        let trailing = if fields.is_empty() {
            leading
        } else {
            self.collect_leading_trivia()
        };

        let close_paren = self.consume_closing(
            TokenKind::RParen,
            Self::expected("closing `)` or `}`", Some("struct")),
            errors,
        );

        Expr::AnonStruct(AnonStructExpr {
            span: Span::between(&open_paren.span, &close_paren),
            open_paren: open_paren.span,
            leading: Trivia::empty(),
            fields,
            trailing,
            close_paren,
        })
    }

    /// Parse a sequence `[a, b, c]`.
    fn parse_seq(&mut self) -> Result<Expr<'a>> {
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

            let (comma, has_comma) = self.consume_comma();

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

        let close_bracket = self.consume_closing_strict(
            TokenKind::RBracket,
            Self::expected("closing `]`", Some("array")),
        )?;

        Ok(Expr::Seq(SeqExpr {
            span: Span::between(&open_bracket.span, &close_bracket),
            open_bracket: open_bracket.span,
            leading: Trivia::empty(),
            items,
            trailing,
            close_bracket,
        }))
    }

    /// Parse a sequence `[a, b, c]` with error recovery.
    fn parse_seq_lossy(&mut self, errors: &mut Vec<Error>) -> Expr<'a> {
        let open_bracket = self.next_token();
        debug_assert_eq!(open_bracket.kind, TokenKind::LBracket);

        let mut leading = self.collect_leading_trivia();
        let mut items = Vec::new();

        loop {
            match self.peek_kind() {
                TokenKind::RBracket | TokenKind::Eof => break,
                _ => {}
            }

            let expr = self.parse_expr_lossy(errors);
            let trailing = self.collect_leading_trivia();

            let (comma, has_comma) = self.consume_comma();

            items.push(SeqItem {
                leading,
                expr,
                trailing,
                comma,
            });

            leading = self.collect_leading_trivia();

            if !has_comma {
                if matches!(self.peek_kind(), TokenKind::RBracket | TokenKind::Eof) {
                    break;
                }
                errors.push(Self::error(
                    self.peek_span(),
                    Self::expected("comma", Some("array")),
                ));
            }
        }

        let trailing = if items.is_empty() {
            leading
        } else {
            self.collect_leading_trivia()
        };

        let close_bracket = self.consume_closing(
            TokenKind::RBracket,
            Self::expected("closing `]`", Some("array")),
            errors,
        );

        Expr::Seq(SeqExpr {
            span: Span::between(&open_bracket.span, &close_bracket),
            open_bracket: open_bracket.span,
            leading: Trivia::empty(),
            items,
            trailing,
            close_bracket,
        })
    }

    /// Parse a map `{key: value, ...}`.
    fn parse_map(&mut self) -> Result<Expr<'a>> {
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
                // Point error to the unexpected token, not the key
                let unexpected = self.next_token();
                return Err(Self::error(
                    unexpected.span,
                    Self::expected("`:` after map key", Some("map entry")),
                ));
            }
            let colon_tok = self.next_token();

            let post_colon = self.collect_leading_trivia();
            let value = self.parse_expr()?;
            let trailing = self.collect_leading_trivia();

            let (comma, has_comma) = self.consume_comma();

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

        let close_brace = self.consume_closing_strict(
            TokenKind::RBrace,
            Self::expected("closing `}`", Some("map")),
        )?;

        Ok(Expr::Map(MapExpr {
            span: Span::between(&open_brace.span, &close_brace),
            open_brace: open_brace.span,
            leading: Trivia::empty(),
            entries,
            trailing,
            close_brace,
        }))
    }

    /// Create a map entry with missing colon during error recovery.
    fn recover_map_entry_missing_colon(
        &mut self,
        leading: Trivia<'a>,
        key: Expr<'a>,
        pre_colon: Trivia<'a>,
        errors: &mut Vec<Error>,
    ) -> MapEntry<'a> {
        errors.push(Self::error(
            self.peek_span(),
            Self::expected("`:` after map key", Some("map entry")),
        ));
        self.recover_until(&[TokenKind::Comma, TokenKind::RBrace]);
        let trailing = self.collect_leading_trivia();
        let (comma, _) = self.consume_comma();
        let colon_span = pre_colon.span.clone().unwrap_or_else(|| self.eof_span());
        let key_span = key.span().clone();
        let error_span = Self::span_at_end(&key_span);
        MapEntry {
            leading,
            key,
            pre_colon,
            colon: Self::span_at_end(&colon_span),
            post_colon: Trivia::empty(),
            value: Expr::Error(ErrorExpr {
                span: error_span.clone(),
                error: Error::with_span(
                    Self::expected("value", Some("map entry")),
                    error_span,
                ),
            }),
            trailing,
            comma,
        }
    }

    /// Parse a map `{key: value, ...}` with error recovery.
    fn parse_map_lossy(&mut self, errors: &mut Vec<Error>) -> Expr<'a> {
        let open_brace = self.next_token();
        debug_assert_eq!(open_brace.kind, TokenKind::LBrace);

        let mut leading = self.collect_leading_trivia();
        let mut entries = Vec::new();

        loop {
            match self.peek_kind() {
                TokenKind::RBrace | TokenKind::Eof => break,
                _ => {}
            }

            let key = self.parse_expr_lossy(errors);
            let pre_colon = self.collect_leading_trivia();

            if self.peek_kind() != TokenKind::Colon {
                let entry = self.recover_map_entry_missing_colon(leading, key, pre_colon, errors);
                leading = self.collect_leading_trivia();
                entries.push(entry);
                continue;
            }
            let colon_tok = self.next_token();

            let post_colon = self.collect_leading_trivia();
            let value = self.parse_expr_lossy(errors);
            let trailing = self.collect_leading_trivia();

            let (comma, has_comma) = self.consume_comma();

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
                if matches!(self.peek_kind(), TokenKind::RBrace | TokenKind::Eof) {
                    break;
                }
                errors.push(Self::error(
                    self.peek_span(),
                    Self::expected("comma", Some("map")),
                ));
            }
        }

        let trailing = if entries.is_empty() {
            leading
        } else {
            self.collect_leading_trivia()
        };

        let close_brace = self.consume_closing(
            TokenKind::RBrace,
            Self::expected("closing `}`", Some("map")),
            errors,
        );

        Expr::Map(MapExpr {
            span: Span::between(&open_brace.span, &close_brace),
            open_brace: open_brace.span,
            leading: Trivia::empty(),
            entries,
            trailing,
            close_brace,
        })
    }

    /// Parse an identifier-starting expression.
    /// Could be: bool, option, named struct, enum variant.
    fn parse_ident_expr(&mut self) -> Result<Expr<'a>> {
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

    /// Parse an identifier-starting expression with error recovery.
    fn parse_ident_expr_lossy(&mut self, errors: &mut Vec<Error>) -> Expr<'a> {
        let ident_tok = self.next_token();
        debug_assert_eq!(ident_tok.kind, TokenKind::Ident);

        match ident_tok.text {
            "true" => Expr::Bool(BoolExpr {
                span: ident_tok.span,
                value: true,
            }),
            "false" => Expr::Bool(BoolExpr {
                span: ident_tok.span,
                value: false,
            }),
            "None" => Expr::Option(Box::new(OptionExpr {
                span: ident_tok.span,
                value: None,
            })),
            "Some" => self.parse_some_lossy(ident_tok, errors),
            "inf" | "NaN" => Expr::Number(NumberExpr {
                span: ident_tok.span,
                raw: Cow::Borrowed(ident_tok.text),
                kind: NumberKind::SpecialFloat,
            }),
            _ => self.parse_struct_or_variant_lossy(&ident_tok, errors),
        }
    }

    /// Parse `Some(value)`.
    fn parse_some(&mut self, some_tok: Token<'a>) -> Result<Expr<'a>> {
        if self.peek_kind() != TokenKind::LParen {
            return Err(Self::error(
                some_tok.span,
                Self::expected("`Some` or `None`", None),
            ));
        }
        let open_paren = self.next_token();

        let leading = self.collect_leading_trivia();
        let expr = self.parse_expr()?;
        let trailing = self.collect_leading_trivia();

        let close_paren = self.consume_closing_strict(
            TokenKind::RParen,
            Self::expected("closing `)`", Some("option")),
        )?;

        Ok(Expr::Option(Box::new(OptionExpr {
            span: Span::between(&some_tok.span, &close_paren),
            value: Some(OptionValue {
                open_paren: open_paren.span,
                leading,
                expr,
                trailing,
                close_paren,
            }),
        })))
    }

    /// Parse `Some(value)` with error recovery.
    fn parse_some_lossy(&mut self, some_tok: Token<'a>, errors: &mut Vec<Error>) -> Expr<'a> {
        if self.peek_kind() != TokenKind::LParen {
            let err = Self::error(
                some_tok.span,
                Self::expected("`Some` or `None`", None),
            );
            return self.error_expr_from(err, errors);
        }
        let open_paren = self.next_token();

        let leading = self.collect_leading_trivia();
        let expr = self.parse_expr_lossy(errors);
        let trailing = self.collect_leading_trivia();

        let close_paren = self.consume_closing(
            TokenKind::RParen,
            Self::expected("closing `)`", Some("option")),
            errors,
        );

        Expr::Option(Box::new(OptionExpr {
            span: Span::between(&some_tok.span, &close_paren),
            value: Some(OptionValue {
                open_paren: open_paren.span,
                leading,
                expr,
                trailing,
                close_paren,
            }),
        }))
    }

    /// Parse a struct or enum variant: `Name(...)` or `Name`.
    ///
    /// In RON, named struct fields use paren syntax: `Point(x: 1, y: 2)`.
    /// Tuple structs also use parens: `Point(1, 2)`.
    /// The parser looks ahead to detect if the first element is `ident: value` (struct)
    /// or just a value (tuple).
    fn parse_struct_or_variant(&mut self, name_tok: &Token<'a>) -> Result<Expr<'a>> {
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

                match self.try_parse_empty_tuple_body(&open_paren.span, leading) {
                    Ok(tuple_body) => Some(StructBody::Tuple(tuple_body)),
                    Err(leading) => {
                        // Determine if this is named fields or tuple elements
                        // by looking at the first token and what follows
                        self.parse_struct_body_contents(open_paren, leading)?
                    }
                }
            }
            _ => None,
        };

        let span = if let Some(ref b) = body {
            let end_span = match b {
                StructBody::Tuple(t) => &t.close_paren,
                StructBody::Fields(f) => &f.close_brace,
            };
            Span::between(&name_tok.span, end_span)
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

    /// Parse a struct or enum variant with error recovery.
    fn parse_struct_or_variant_lossy(
        &mut self,
        name_tok: &Token<'a>,
        errors: &mut Vec<Error>,
    ) -> Expr<'a> {
        let name = Ident {
            span: name_tok.span.clone(),
            name: Cow::Borrowed(name_tok.text),
        };

        let pre_body = self.collect_leading_trivia();

        let body = match self.peek_kind() {
            TokenKind::LParen => {
                let open_paren = self.next_token();
                let leading = self.collect_leading_trivia();

                match self.try_parse_empty_tuple_body(&open_paren.span, leading) {
                    Ok(tuple_body) => Some(StructBody::Tuple(tuple_body)),
                    Err(leading) => {
                        Some(self.parse_struct_body_contents_lossy(open_paren, leading, errors))
                    }
                }
            }
            _ => None,
        };

        let span = if let Some(ref b) = body {
            let end_span = match b {
                StructBody::Tuple(t) => &t.close_paren,
                StructBody::Fields(f) => &f.close_brace,
            };
            Span::between(&name_tok.span, end_span)
        } else {
            name_tok.span.clone()
        };

        Expr::Struct(StructExpr {
            span,
            name,
            pre_body,
            body,
        })
    }

    /// Parse tuple elements until closing paren.
    fn parse_tuple_elements(&mut self, mut leading: Trivia<'a>) -> Result<Vec<TupleElement<'a>>> {
        let mut elements = Vec::new();

        loop {
            if self.peek_kind() == TokenKind::RParen {
                break;
            }

            let expr = self.parse_expr()?;
            let trailing = self.collect_leading_trivia();

            let (comma, has_comma) = self.consume_comma();

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

    /// Parse tuple elements until closing paren with error recovery.
    fn parse_tuple_elements_lossy(
        &mut self,
        mut leading: Trivia<'a>,
        errors: &mut Vec<Error>,
    ) -> Vec<TupleElement<'a>> {
        let mut elements = Vec::new();

        loop {
            match self.peek_kind() {
                TokenKind::RParen | TokenKind::Eof => break,
                _ => {}
            }

            let expr = self.parse_expr_lossy(errors);
            let trailing = self.collect_leading_trivia();

            let (comma, has_comma) = self.consume_comma();

            elements.push(TupleElement {
                leading,
                expr,
                trailing,
                comma,
            });

            leading = self.collect_leading_trivia();

            if !has_comma {
                if matches!(self.peek_kind(), TokenKind::RParen | TokenKind::Eof) {
                    break;
                }
                errors.push(Self::error(
                    self.peek_span(),
                    Self::expected("comma", Some("tuple")),
                ));
            }
        }

        elements
    }

    /// Parse the body contents of a named struct: either named fields or tuple elements.
    /// Determines the type by looking at the first token: if it's `ident:`, it's named fields.
    fn parse_struct_body_contents(
        &mut self,
        open_paren: Token<'a>,
        leading: Trivia<'a>,
    ) -> Result<Option<StructBody<'a>>> {
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
                    return Err(Self::error(
                        open_paren.span,
                        Self::expected("closing `)` or `}`", Some("struct")),
                    ));
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
                    return Err(Self::error(
                        open_paren.span,
                        Self::expected("closing `)` or `}`", Some("struct")),
                    ));
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
                return Err(Self::error(
                    open_paren.span,
                    Self::expected("closing `)` or `}`", Some("struct")),
                ));
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

    /// Parse the body contents of a named struct with error recovery.
    fn parse_struct_body_contents_lossy(
        &mut self,
        open_paren: Token<'a>,
        leading: Trivia<'a>,
        errors: &mut Vec<Error>,
    ) -> StructBody<'a> {
        if self.peek_kind() == TokenKind::Ident {
            let first_tok = self.next_token();
            let post_ident_trivia = self.collect_leading_trivia();

            if self.peek_kind() == TokenKind::Colon {
                let (fields, trailing) = self.parse_struct_fields_from_first_lossy(
                    leading,
                    &first_tok,
                    post_ident_trivia,
                    errors,
                );

                let close_paren = self.consume_closing(
                    TokenKind::RParen,
                    Self::expected("closing `)` or `}`", Some("struct")),
                    errors,
                );

                StructBody::Fields(FieldsBody {
                    open_brace: open_paren.span,
                    leading: Trivia::empty(),
                    fields,
                    trailing,
                    close_brace: close_paren,
                })
            } else {
                let first_expr =
                    self.ident_token_to_expr_lossy(first_tok, post_ident_trivia, errors);
                let (elements, trailing) =
                    self.parse_tuple_elements_from_first_lossy(leading, first_expr, errors);

                let close_paren = self.consume_closing(
                    TokenKind::RParen,
                    Self::expected("closing `)` or `}`", Some("struct")),
                    errors,
                );

                StructBody::Tuple(TupleBody {
                    open_paren: open_paren.span,
                    leading: Trivia::empty(),
                    elements,
                    trailing,
                    close_paren,
                })
            }
        } else {
            let elements = self.parse_tuple_elements_lossy(leading, errors);
            let trailing = self.collect_leading_trivia();

            let close_paren = self.consume_closing(
                TokenKind::RParen,
                Self::expected("closing `)` or `}`", Some("struct")),
                errors,
            );

            StructBody::Tuple(TupleBody {
                open_paren: open_paren.span,
                leading: Trivia::empty(),
                elements,
                trailing,
                close_paren,
            })
        }
    }

    /// Convert an identifier token to an expression.
    /// Handles booleans, None, Some, inf/NaN, and named structs.
    fn ident_token_to_expr(&mut self, tok: Token<'a>, pre_body: Trivia<'a>) -> Result<Expr<'a>> {
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

                        match self.try_parse_empty_tuple_body(&open_paren.span, leading) {
                            Ok(tuple_body) => Some(StructBody::Tuple(tuple_body)),
                            Err(leading) => self.parse_struct_body_contents(open_paren, leading)?,
                        }
                    }
                    _ => None,
                };

                let span = if let Some(ref b) = body {
                    let end_span = match b {
                        StructBody::Tuple(t) => &t.close_paren,
                        StructBody::Fields(f) => &f.close_brace,
                    };
                    Span::between(&tok.span, end_span)
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

    /// Convert an identifier token to an expression with error recovery.
    fn ident_token_to_expr_lossy(
        &mut self,
        tok: Token<'a>,
        pre_body: Trivia<'a>,
        errors: &mut Vec<Error>,
    ) -> Expr<'a> {
        match tok.text {
            "true" => Expr::Bool(BoolExpr {
                span: tok.span,
                value: true,
            }),
            "false" => Expr::Bool(BoolExpr {
                span: tok.span,
                value: false,
            }),
            "None" => Expr::Option(Box::new(OptionExpr {
                span: tok.span,
                value: None,
            })),
            "Some" => self.parse_some_lossy(tok, errors),
            "inf" | "NaN" => Expr::Number(NumberExpr {
                span: tok.span,
                raw: Cow::Borrowed(tok.text),
                kind: NumberKind::SpecialFloat,
            }),
            _ => {
                let name = Ident {
                    span: tok.span.clone(),
                    name: Cow::Borrowed(tok.text),
                };

                let body = match self.peek_kind() {
                    TokenKind::LParen => {
                        let open_paren = self.next_token();
                        let leading = self.collect_leading_trivia();

                        match self.try_parse_empty_tuple_body(&open_paren.span, leading) {
                            Ok(tuple_body) => Some(StructBody::Tuple(tuple_body)),
                            Err(leading) => Some(
                                self.parse_struct_body_contents_lossy(open_paren, leading, errors),
                            ),
                        }
                    }
                    _ => None,
                };

                let span = if let Some(ref b) = body {
                    let end_span = match b {
                        StructBody::Tuple(t) => &t.close_paren,
                        StructBody::Fields(f) => &f.close_brace,
                    };
                    Span::between(&tok.span, end_span)
                } else {
                    tok.span.clone()
                };

                Expr::Struct(StructExpr {
                    span,
                    name,
                    pre_body,
                    body,
                })
            }
        }
    }

    /// Parse struct fields starting with the first field name already consumed.
    fn parse_struct_fields_from_first(
        &mut self,
        leading: Trivia<'a>,
        first_name: &Token<'a>,
        pre_colon: Trivia<'a>,
    ) -> Result<(Vec<StructField<'a>>, Trivia<'a>)> {
        let mut fields = Vec::new();

        // Parse the first field (name already consumed)
        let colon_tok = self.next_token();
        debug_assert_eq!(colon_tok.kind, TokenKind::Colon);

        let post_colon = self.collect_leading_trivia();
        let value = self.parse_expr()?;
        let trailing = self.collect_leading_trivia();

        let (comma, has_comma) = self.consume_comma();

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
                return Err(Self::error(
                    tok.span,
                    Self::expected("identifier", None),
                ));
            }
            let name_tok = self.next_token();

            let pre_colon = self.collect_leading_trivia();

            if self.peek_kind() != TokenKind::Colon {
                return Err(Self::error(
                    name_tok.span,
                    Self::expected("`:` after map key", Some("struct field")),
                ));
            }
            let colon_tok = self.next_token();

            let post_colon = self.collect_leading_trivia();
            let value = self.parse_expr()?;
            let trailing = self.collect_leading_trivia();

            let (comma, has_comma) = self.consume_comma();

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

    /// Parse struct fields starting with the first field name already consumed, with recovery.
    #[allow(clippy::too_many_lines)]
    fn parse_struct_fields_from_first_lossy(
        &mut self,
        leading: Trivia<'a>,
        first_name: &Token<'a>,
        pre_colon: Trivia<'a>,
        errors: &mut Vec<Error>,
    ) -> (Vec<StructField<'a>>, Trivia<'a>) {
        let mut fields = Vec::new();

        let colon_tok = self.next_token();
        debug_assert_eq!(colon_tok.kind, TokenKind::Colon);

        let post_colon = self.collect_leading_trivia();
        let value = self.parse_expr_lossy(errors);
        let trailing = self.collect_leading_trivia();

        let (comma, has_comma) = self.consume_comma();

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
            return (fields, self.collect_leading_trivia());
        }

        let mut leading = self.collect_leading_trivia();

        loop {
            match self.peek_kind() {
                TokenKind::RParen | TokenKind::Eof => break,
                TokenKind::Ident => {}
                _ => {
                    let error_span = self.peek_span();
                    errors.push(Self::error(
                        error_span.clone(),
                        Self::expected("identifier", None),
                    ));
                    self.recover_until(&[TokenKind::Comma, TokenKind::RParen]);
                    let trailing = self.collect_leading_trivia();
                    let (comma, has_comma) = self.consume_comma();
                    // Create a placeholder field to preserve structure
                    fields.push(StructField {
                        leading,
                        name: Ident {
                            span: error_span.clone(),
                            name: Cow::Borrowed(""),
                        },
                        pre_colon: Trivia::empty(),
                        colon: Self::span_at_end(&error_span),
                        post_colon: Trivia::empty(),
                        value: Expr::Error(ErrorExpr {
                            span: error_span.clone(),
                            error: Error::with_span(
                                Self::expected("identifier", None),
                                error_span,
                            ),
                        }),
                        trailing,
                        comma,
                    });
                    leading = self.collect_leading_trivia();
                    if !has_comma {
                        break;
                    }
                    continue;
                }
            }

            let name_tok = self.next_token();
            let pre_colon = self.collect_leading_trivia();

            let colon = if self.peek_kind() == TokenKind::Colon {
                self.next_token().span
            } else {
                errors.push(Self::error(
                    name_tok.span.clone(),
                    Self::expected("`:` after map key", Some("struct field")),
                ));
                Self::span_at_end(&name_tok.span)
            };

            let post_colon = self.collect_leading_trivia();
            let value = match self.peek_kind() {
                TokenKind::Comma | TokenKind::RParen => {
                    let error_span = Self::span_at_end(&colon);
                    let error_kind = Self::expected("value", Some("struct field"));
                    errors.push(Self::error(error_span.clone(), error_kind.clone()));
                    Expr::Error(ErrorExpr {
                        span: error_span.clone(),
                        error: Error::with_span(error_kind, error_span),
                    })
                }
                _ => self.parse_expr_lossy(errors),
            };
            let trailing = self.collect_leading_trivia();

            let (comma, has_comma) = self.consume_comma();

            fields.push(StructField {
                leading,
                name: Ident {
                    span: name_tok.span.clone(),
                    name: Cow::Borrowed(name_tok.text),
                },
                pre_colon,
                colon,
                post_colon,
                value,
                trailing,
                comma,
            });

            leading = self.collect_leading_trivia();

            if !has_comma {
                if matches!(self.peek_kind(), TokenKind::Ident) {
                    errors.push(Self::error(
                        self.peek_span(),
                        Self::expected("comma", Some("struct")),
                    ));
                } else {
                    break;
                }
            }
        }

        (fields, leading)
    }

    /// Parse tuple elements starting with the first expression already parsed.
    fn parse_tuple_elements_from_first(
        &mut self,
        leading: Trivia<'a>,
        first_expr: Expr<'a>,
    ) -> Result<(Vec<TupleElement<'a>>, Trivia<'a>)> {
        let mut elements = Vec::new();

        // Handle the first element
        let trailing = self.collect_leading_trivia();

        let (comma, has_comma) = self.consume_comma();

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

            let (comma, has_comma) = self.consume_comma();

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

    /// Parse tuple elements starting with the first expression, with recovery.
    fn parse_tuple_elements_from_first_lossy(
        &mut self,
        leading: Trivia<'a>,
        first_expr: Expr<'a>,
        errors: &mut Vec<Error>,
    ) -> (Vec<TupleElement<'a>>, Trivia<'a>) {
        let mut elements = Vec::new();

        let trailing = self.collect_leading_trivia();
        let (comma, has_comma) = self.consume_comma();

        elements.push(TupleElement {
            leading,
            expr: first_expr,
            trailing,
            comma,
        });

        if !has_comma {
            return (elements, self.collect_leading_trivia());
        }

        let mut leading = self.collect_leading_trivia();

        loop {
            match self.peek_kind() {
                TokenKind::RParen | TokenKind::Eof => break,
                _ => {}
            }

            let expr = self.parse_expr_lossy(errors);
            let trailing = self.collect_leading_trivia();

            let (comma, has_comma) = self.consume_comma();

            elements.push(TupleElement {
                leading,
                expr,
                trailing,
                comma,
            });

            leading = self.collect_leading_trivia();

            if !has_comma {
                if matches!(self.peek_kind(), TokenKind::RParen | TokenKind::Eof) {
                    break;
                }
                errors.push(Self::error(
                    self.peek_span(),
                    Self::expected("comma", Some("tuple")),
                ));
            }
        }

        (elements, leading)
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
    fn parse_string(&mut self) -> Result<Expr<'a>> {
        let tok = self.next_token();
        debug_assert_eq!(tok.kind, TokenKind::String);

        let (value, kind) = unescape::decode_string(tok.text).map_err(|(e, byte_offset)| {
            // Compute the error position within the source
            // byte_offset is relative to the token text start
            let error_start_offset = tok.span.start_offset + byte_offset;
            let error_end_offset = error_start_offset + 2; // Point to the escape sequence (\x)

            // Compute line/column by counting from token start
            // For now, assume same line (escapes rarely span lines)
            let col_offset = tok.text[..byte_offset].chars().count();
            let error_col = tok.span.start.col + col_offset;

            Self::error(
                Span {
                    start: Position {
                        line: tok.span.start.line,
                        col: error_col,
                    },
                    end: Position {
                        line: tok.span.start.line,
                        col: error_col + 2,
                    },
                    start_offset: error_start_offset,
                    end_offset: error_end_offset.min(tok.span.end_offset),
                },
                e.kind().clone(),
            )
        })?;

        Ok(Expr::String(StringExpr {
            span: tok.span,
            raw: Cow::Borrowed(tok.text),
            value,
            kind,
        }))
    }

    /// Parse a byte string literal.
    fn parse_bytes(&mut self) -> Result<Expr<'a>> {
        let tok = self.next_token();
        debug_assert_eq!(tok.kind, TokenKind::ByteString);

        let (value, kind) = unescape::decode_byte_string(tok.text)
            .map_err(|e| Self::error(tok.span.clone(), e.kind().clone()))?;

        Ok(Expr::Bytes(BytesExpr {
            span: tok.span,
            raw: Cow::Borrowed(tok.text),
            value,
            kind,
        }))
    }

    /// Parse a character literal.
    fn parse_char(&mut self) -> Result<Expr<'a>> {
        let tok = self.next_token();
        debug_assert_eq!(tok.kind, TokenKind::Char);

        // Check if it's a byte literal b'x'
        if tok.text.starts_with("b'") {
            let content = &tok.text[2..tok.text.len() - 1];
            let value = unescape::unescape_byte_char(content)
                .map_err(|e| Self::error(tok.span.clone(), e.kind().clone()))?;
            return Ok(Expr::Byte(ByteExpr {
                span: tok.span,
                raw: Cow::Borrowed(tok.text),
                value,
            }));
        }

        // Regular char literal 'x'
        let content = &tok.text[1..tok.text.len() - 1];
        let value = unescape::unescape_char(content)
            .map_err(|e| Self::error(tok.span.clone(), e.kind().clone()))?;

        Ok(Expr::Char(CharExpr {
            span: tok.span,
            raw: Cow::Borrowed(tok.text),
            value,
        }))
    }
}

#[cfg(test)]
#[allow(clippy::panic)]
mod tests {
    use super::*;
    use crate::ast::StringKind;

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
        assert!(
            result.is_err(),
            "Brace syntax after ident should not be valid"
        );
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

    // =========================================================================
    // Lossy parsing tests (error recovery)
    // =========================================================================

    #[test]
    fn lossy_missing_closing_paren() {
        let (doc, errors) = parse_document_lossy("(x: 1, y: 2");
        assert_eq!(errors.len(), 1);
        match &doc.value {
            Some(Expr::AnonStruct(s)) => {
                assert_eq!(s.fields.len(), 2);
                assert_eq!(s.fields[0].name.name, "x");
                assert_eq!(s.fields[1].name.name, "y");
            }
            _ => panic!("expected anonymous struct"),
        }
    }

    #[test]
    fn lossy_missing_closing_bracket() {
        let (doc, errors) = parse_document_lossy("[1, 2, 3");
        assert_eq!(errors.len(), 1);
        match &doc.value {
            Some(Expr::Seq(seq)) => {
                assert_eq!(seq.items.len(), 3);
            }
            _ => panic!("expected sequence"),
        }
    }

    #[test]
    fn lossy_missing_closing_brace() {
        let (doc, errors) = parse_document_lossy(r#"{"a": 1"#);
        assert_eq!(errors.len(), 1);
        match &doc.value {
            Some(Expr::Map(map)) => {
                assert_eq!(map.entries.len(), 1);
            }
            _ => panic!("expected map"),
        }
    }

    #[test]
    fn lossy_missing_comma_in_struct() {
        let (doc, errors) = parse_document_lossy("(x: 1 y: 2)");
        assert_eq!(errors.len(), 1);
        match &doc.value {
            Some(Expr::AnonStruct(s)) => {
                assert_eq!(s.fields.len(), 2);
            }
            _ => panic!("expected anonymous struct"),
        }
    }

    #[test]
    fn lossy_invalid_token_in_struct() {
        let (doc, errors) = parse_document_lossy("(x: @, y: 2)");
        assert_eq!(errors.len(), 1);
        match &doc.value {
            Some(Expr::AnonStruct(s)) => {
                assert_eq!(s.fields.len(), 2);
                assert!(matches!(s.fields[0].value, Expr::Error(_)));
                assert!(matches!(s.fields[1].value, Expr::Number(_)));
            }
            _ => panic!("expected anonymous struct"),
        }
    }

    #[test]
    fn lossy_invalid_field_name() {
        // Input starts with a number, so parser sees this as a tuple first.
        // "y: 2" in tuple context will cause a colon error.
        let (doc, errors) = parse_document_lossy("(123, y: 2)");
        // Errors occur because "y: 2" looks like a map entry in tuple context
        assert!(!errors.is_empty());
        match &doc.value {
            Some(Expr::Tuple(t)) => {
                // First element is the number, second involves recovery
                assert!(!t.elements.is_empty());
            }
            _ => panic!("expected tuple, got {:?}", doc.value),
        }
    }

    #[test]
    fn lossy_invalid_token_as_field_name() {
        // @ is not an identifier, so parser can't know it's meant to be a field.
        // It parses as a tuple with error elements.
        let (doc, errors) = parse_document_lossy("(@: 1, y: 2)");
        assert!(!errors.is_empty());
        match &doc.value {
            Some(Expr::Tuple(t)) => {
                // Contains error expressions for invalid tokens
                assert!(!t.elements.is_empty());
            }
            _ => panic!("expected tuple, got {:?}", doc.value),
        }
    }

    #[test]
    fn lossy_invalid_token_in_existing_struct() {
        // Start with a valid field so parser knows it's a struct,
        // then encounter an invalid token where a field name is expected.
        let (doc, errors) = parse_document_lossy("(x: 1, @: 2, y: 3)");
        assert!(!errors.is_empty());
        match &doc.value {
            Some(Expr::AnonStruct(s)) => {
                // Should have 3 fields: x, placeholder for @, and y
                assert_eq!(s.fields.len(), 3);
                assert_eq!(s.fields[0].name.name, "x");
                assert_eq!(s.fields[1].name.name, ""); // placeholder
                assert_eq!(s.fields[2].name.name, "y");
            }
            _ => panic!("expected anonymous struct, got {:?}", doc.value),
        }
    }

    #[test]
    fn lossy_multiple_errors() {
        let (doc, errors) = parse_document_lossy("(x: @, y: #)");
        assert_eq!(errors.len(), 2);
        match &doc.value {
            Some(Expr::AnonStruct(s)) => {
                assert_eq!(s.fields.len(), 2);
                assert!(matches!(s.fields[0].value, Expr::Error(_)));
                assert!(matches!(s.fields[1].value, Expr::Error(_)));
            }
            _ => panic!("expected anonymous struct"),
        }
    }

    #[test]
    fn lossy_nested_recovery() {
        let (doc, errors) = parse_document_lossy("(x: [1, @, 3], y: 2)");
        assert_eq!(errors.len(), 1);
        match &doc.value {
            Some(Expr::AnonStruct(s)) => {
                assert_eq!(s.fields.len(), 2);
                match &s.fields[0].value {
                    Expr::Seq(seq) => {
                        assert_eq!(seq.items.len(), 3);
                        assert!(matches!(seq.items[1].expr, Expr::Error(_)));
                    }
                    _ => panic!("expected sequence"),
                }
            }
            _ => panic!("expected anonymous struct"),
        }
    }

    #[test]
    fn lossy_missing_value_after_colon() {
        let (doc, errors) = parse_document_lossy("(x:, y: 2)");
        assert_eq!(errors.len(), 1);
        match &doc.value {
            Some(Expr::AnonStruct(s)) => {
                assert_eq!(s.fields.len(), 2);
                assert!(matches!(s.fields[0].value, Expr::Error(_)));
                assert!(matches!(s.fields[1].value, Expr::Number(_)));
            }
            _ => panic!("expected anonymous struct"),
        }
    }

    #[test]
    fn lossy_valid_document_no_errors() {
        let (doc, errors) = parse_document_lossy("(x: 1, y: 2)");
        assert!(errors.is_empty());
        match &doc.value {
            Some(Expr::AnonStruct(s)) => {
                assert_eq!(s.fields.len(), 2);
            }
            _ => panic!("expected anonymous struct"),
        }
    }

    #[test]
    fn lossy_preserves_attributes() {
        let (doc, errors) = parse_document_lossy(r#"#![type = "foo::Bar"] (x: 1"#);
        assert_eq!(errors.len(), 1); // missing closing paren
        assert_eq!(doc.attributes.len(), 1);
        assert_eq!(doc.attributes[0].name, "type");
    }
}
