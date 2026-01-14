//! Core parsing utilities.
//!
//! This module provides the `ParserCore` trait with utility methods used by all
//! parsing submodules, including token manipulation, trivia handling, error creation,
//! and error recovery.

use alloc::{borrow::Cow, vec::Vec};

use super::AstParser;
use crate::{
    ast::{Comment, CommentKind, ErrorExpr, Expr, Trivia},
    error::{Error, ErrorKind, Position, Span},
    token::{Token, TokenKind},
};

/// Internal trait providing core parsing utilities.
///
/// This trait is implemented for `AstParser` and provides utility methods
/// used by all parsing submodules.
pub(super) trait ParserCore<'a> {
    // Token manipulation
    fn peek_kind(&mut self) -> TokenKind;
    fn peek_span(&mut self) -> Span;
    fn peek_two_kinds(&mut self) -> (TokenKind, TokenKind);
    fn next_token(&mut self) -> Token<'a>;
    fn consume_comma(&mut self) -> (Option<Span>, bool);

    // Trivia handling
    fn drain_trivia(&mut self) -> Trivia<'a>;
    fn collect_leading_trivia(&mut self) -> Trivia<'a>;

    // Error handling
    fn error(span: Span, kind: ErrorKind) -> Error;
    fn error_expr_from(&mut self, err: Error, errors: &mut Vec<Error>) -> Expr<'a>;
    fn expected(msg: &'static str, ctx: Option<&'static str>) -> ErrorKind;
    fn error_for_error_token(tok: &Token<'_>) -> Error;

    // Error recovery
    fn recover_until(&mut self, sync: &[TokenKind]);
    fn consume_closing(
        &mut self,
        expected: TokenKind,
        error_kind: ErrorKind,
        errors: &mut Vec<Error>,
    ) -> Span;
    fn at_closing_or_eof(&mut self, closing: TokenKind) -> bool;
    fn report_missing_comma(&mut self, context: &'static str, errors: &mut Vec<Error>);
    fn handle_missing_comma(
        &mut self,
        context: &'static str,
        closing: TokenKind,
        errors: &mut Vec<Error>,
    ) -> bool;

    // Helpers
    fn eof_span(&self) -> Span;
    fn span_at_end(span: &Span) -> Span;
}

impl<'a> ParserCore<'a> for AstParser<'a> {
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
            return tok.span;
        }

        while let Some(tok) = self.tokens.peek() {
            if tok.kind.is_trivia() {
                if let Some(tok) = self.tokens.next() {
                    self.trivia_buffer.push(tok);
                }
            } else {
                return tok.span;
            }
        }

        self.eof_span()
    }

    /// Peek at the next two non-trivia token kinds without consuming.
    /// Returns (`first_kind`, `second_kind`).
    fn peek_two_kinds(&mut self) -> (TokenKind, TokenKind) {
        // Ensure we have at least one token in lookahead
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

        // Skip trivia between first and second token to find the second non-trivia token
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

        let tokens = ::core::mem::take(&mut self.trivia_buffer);
        let start = tokens.first().map(|t| t.span);
        let end = tokens.last().map(|t| t.span);

        let span = match (start, end) {
            (Some(s), Some(e)) => Some(Span {
                start: s.start,
                end: e.end,
                start_offset: s.start_offset,
                end_offset: e.end_offset,
            }),
            _ => None,
        };

        // Build whitespace string from first whitespace token and collect comments
        let mut first_whitespace_range: Option<(usize, usize)> = None;
        let mut comments = Vec::new();

        for tok in tokens {
            match tok.kind {
                TokenKind::Whitespace => {
                    if first_whitespace_range.is_none() {
                        first_whitespace_range =
                            Some((tok.span.start_offset, tok.span.end_offset));
                    }
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

        let whitespace = if let Some((start, end)) = first_whitespace_range {
            Cow::Borrowed(&self.source[start..end])
        } else {
            Cow::Borrowed("")
        };

        Trivia {
            span,
            whitespace,
            comments,
        }
    }

    /// Collect leading trivia without consuming a non-trivia token.
    fn collect_leading_trivia(&mut self) -> Trivia<'a> {
        // Peek to force trivia collection
        let _ = self.peek_kind();
        self.drain_trivia()
    }

    /// Create a spanned error.
    fn error(span: Span, kind: ErrorKind) -> Error {
        Error::with_span(kind, span)
    }

    /// Create an error expression and recover.
    fn error_expr_from(&mut self, err: Error, errors: &mut Vec<Error>) -> Expr<'a> {
        let span = *err.span();
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
            Self::error(tok.span, ErrorKind::UnclosedBlockComment)
        } else if tok.text.starts_with('"') || tok.text.starts_with("r#") {
            Self::error(tok.span, ErrorKind::ExpectedStringEnd)
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
                tok.span,
                ErrorKind::UnexpectedChar(tok.text.chars().next().unwrap_or('?')),
            )
        }
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

    /// Check if we're at a closing delimiter or EOF.
    fn at_closing_or_eof(&mut self, closing: TokenKind) -> bool {
        let kind = self.peek_kind();
        kind == closing || kind == TokenKind::Eof
    }

    /// Report a missing comma error.
    fn report_missing_comma(&mut self, context: &'static str, errors: &mut Vec<Error>) {
        errors.push(Self::error(
            self.peek_span(),
            Self::expected("comma", Some(context)),
        ));
    }

    /// Handle missing comma in unified parsing.
    /// Returns true if parsing should continue, false if loop should break.
    fn handle_missing_comma(
        &mut self,
        context: &'static str,
        closing: TokenKind,
        errors: &mut Vec<Error>,
    ) -> bool {
        if self.at_closing_or_eof(closing) {
            false
        } else {
            self.report_missing_comma(context, errors);
            true
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

    /// Create a zero-width span at the end of the given span.
    #[inline]
    fn span_at_end(span: &Span) -> Span {
        Span {
            start: span.end,
            end: span.end,
            start_offset: span.end_offset,
            end_offset: span.end_offset,
        }
    }
}
