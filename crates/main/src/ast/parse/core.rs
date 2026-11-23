//! Token access, trivia collection, error reporting and recovery shared by
//! all parsing submodules, plus the comma-separated list loop used by every
//! container.

use alloc::{borrow::Cow, vec::Vec};
use core::{mem, ops::Range};

use super::AstParser;
use crate::{
    ast::{
        Comment, CommentKind, ErrorExpr, Expr, MapEntry, SeqItem, StructField, Trivia, TupleElement,
    },
    error::{Error, ErrorKind, Span},
    token::{LexError, Token, TokenKind},
};

/// Tokens at which error recovery inside a container stops.
const SYNC: [TokenKind; 4] = [
    TokenKind::Comma,
    TokenKind::RParen,
    TokenKind::RBracket,
    TokenKind::RBrace,
];

/// A container delimiter pair.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum Delim {
    Paren = 0,
    Bracket = 1,
    Brace = 2,
}

impl Delim {
    fn of_closer(kind: TokenKind) -> Option<Self> {
        match kind {
            TokenKind::RParen => Some(Self::Paren),
            TokenKind::RBracket => Some(Self::Bracket),
            TokenKind::RBrace => Some(Self::Brace),
            _ => None,
        }
    }

    fn closer(self) -> TokenKind {
        match self {
            Self::Paren => TokenKind::RParen,
            Self::Bracket => TokenKind::RBracket,
            Self::Brace => TokenKind::RBrace,
        }
    }

    fn expected_closer(self) -> &'static str {
        match self {
            Self::Paren => "closing `)`",
            Self::Bracket => "closing `]`",
            Self::Brace => "closing `}`",
        }
    }
}

/// An item of a comma-separated list, whose trailing trivia and comma are
/// filled in by [`AstParser::parse_list`].
pub(super) trait ListItem<'a> {
    fn set_tail(&mut self, trailing: Trivia<'a>, comma: Option<Span>);
}

macro_rules! impl_list_item {
    ($($ty:ident),*) => {$(
        impl<'a> ListItem<'a> for $ty<'a> {
            fn set_tail(&mut self, trailing: Trivia<'a>, comma: Option<Span>) {
                self.trailing = trailing;
                self.comma = comma;
            }
        }
    )*};
}

impl_list_item!(SeqItem, MapEntry, TupleElement, StructField);

/// The parsed contents of a delimited list.
pub(super) struct List<'a, T> {
    pub items: Vec<T>,
    /// Trivia after the last item (or after the opener of an empty list).
    pub trailing: Trivia<'a>,
    /// The closing delimiter, or a zero-width span where it is missing.
    pub close: Span,
}

pub(super) fn expected(expected: &'static str, context: Option<&'static str>) -> ErrorKind {
    ErrorKind::Expected {
        expected: Cow::Borrowed(expected),
        context,
    }
}

/// A zero-width span at the end of `span`.
pub(super) fn span_at_end(span: &Span) -> Span {
    Span {
        start: span.end,
        end: span.end,
        start_offset: span.end_offset,
        end_offset: span.end_offset,
    }
}

/// A zero-width span at the start of `span`.
fn span_at_start(span: &Span) -> Span {
    Span {
        start: span.start,
        end: span.start,
        start_offset: span.start_offset,
        end_offset: span.start_offset,
    }
}

impl<'a> AstParser<'a> {
    // ------------------------------------------------------------------
    // Token access
    // ------------------------------------------------------------------

    /// Read the next raw token (trivia included).
    fn read_raw(&mut self) -> Token<'a> {
        self.ahead
            .pop_front()
            .unwrap_or_else(|| self.lexer.next_token())
    }

    /// Read the next non-trivia token, buffering the trivia before it.
    pub(super) fn read_significant(&mut self) -> Token<'a> {
        loop {
            let tok = self.read_raw();
            if !tok.kind.is_trivia() {
                return tok;
            }
            self.trivia.push(tok);
        }
    }

    #[inline]
    pub(super) fn peek_kind(&self) -> TokenKind {
        self.peeked.kind
    }

    #[inline]
    pub(super) fn peek_span(&self) -> Span {
        self.peeked.span
    }

    /// The kind of the non-trivia token after the peeked one.
    pub(super) fn peek_second_kind(&mut self) -> TokenKind {
        let mut i = 0;
        loop {
            if i == self.ahead.len() {
                let tok = self.lexer.next_token();
                self.ahead.push_back(tok);
            }
            let kind = self.ahead[i].kind;
            if !kind.is_trivia() {
                return kind;
            }
            i += 1;
        }
    }

    /// Consume the peeked token.
    ///
    /// Trivia before it that was not taken with [`take_trivia`](Self::take_trivia)
    /// lies inside a construct the AST does not model (e.g. `Some /* c */ (1)`)
    /// and is dropped, so it cannot end up in a later trivia span.
    #[inline]
    pub(super) fn next_token(&mut self) -> Token<'a> {
        self.trivia.clear();
        let next = self.read_significant();
        mem::replace(&mut self.peeked, next)
    }

    /// Consume the peeked token if it is of `kind`.
    pub(super) fn eat(&mut self, kind: TokenKind) -> Option<Span> {
        (self.peek_kind() == kind).then(|| self.next_token().span)
    }

    /// Take the trivia before the peeked token.
    pub(super) fn take_trivia(&mut self) -> Trivia<'a> {
        let (Some(first), Some(last)) = (self.trivia.first(), self.trivia.last()) else {
            return Trivia::empty();
        };
        let span = Span::between(&first.span, &last.span);

        let mut whitespace = None;
        let mut comments = Vec::new();
        for tok in self.trivia.drain(..) {
            let kind = match tok.kind {
                TokenKind::LineComment => CommentKind::Line,
                TokenKind::BlockComment => CommentKind::Block,
                _ => {
                    whitespace.get_or_insert(tok.text);
                    continue;
                }
            };
            comments.push(Comment {
                span: tok.span,
                text: Cow::Borrowed(tok.text),
                kind,
            });
        }

        Trivia {
            span: Some(span),
            whitespace: Cow::Borrowed(whitespace.unwrap_or_default()),
            comments,
        }
    }

    // ------------------------------------------------------------------
    // Errors
    // ------------------------------------------------------------------

    /// The span of a byte range of the source.
    pub(super) fn span_of(&self, range: Range<usize>) -> Span {
        let index = self.lexer.line_index();
        Span {
            start: index.position(range.start),
            end: index.position(range.end),
            start_offset: range.start,
            end_offset: range.end,
        }
    }

    /// Error for an unexpected token: lexer error tokens explain themselves,
    /// any other token is reported with `kind`.
    pub(super) fn unexpected(&self, tok: &Token<'_>, kind: ErrorKind) -> Error {
        match tok.kind {
            TokenKind::Error(error) => self.lex_error(tok, error),
            _ => Error::with_span(kind, tok.span),
        }
    }

    fn lex_error(&self, tok: &Token<'_>, error: LexError) -> Error {
        let kind = match error {
            LexError::UnexpectedChar => {
                ErrorKind::UnexpectedChar(tok.text.chars().next().unwrap_or_default())
            }
            LexError::UnclosedBlockComment => ErrorKind::UnclosedBlockComment,
            LexError::UnterminatedString => ErrorKind::ExpectedStringEnd,
            LexError::TooManyHashes => expected("at most 255 `#`", Some("raw string")),
            LexError::UnterminatedChar => expected("closing `'`", Some("character literal")),
            LexError::EmptyChar => expected("a character", Some("character literal")),
            LexError::MissingDigits { base } => expected(
                match base {
                    16 => "hexadecimal digits",
                    8 => "octal digits",
                    _ => "binary digits",
                },
                Some("integer"),
            ),
            LexError::MissingExponent => expected("exponent digits", Some("float")),
            LexError::InvalidRawLiteral => ErrorKind::InvalidIdentifier(tok.text.into()),
            LexError::InvalidDigit { base, at } => {
                let at = usize::from(at);
                let digit = tok
                    .text
                    .get(at..)
                    .and_then(|rest| rest.chars().next())
                    .unwrap_or_default();
                let start = tok.span.start_offset + at;
                return Error::with_span(
                    ErrorKind::InvalidIntegerDigit { digit, base },
                    self.span_of(start..start + digit.len_utf8()),
                );
            }
        };
        Error::with_span(kind, tok.span)
    }

    /// Record `err` and recover to the next comma or closing delimiter.
    #[inline(never)]
    pub(super) fn error_expr(&mut self, err: Error, errors: &mut Vec<Error>) -> Expr<'a> {
        let span = *err.span();
        errors.push(err.clone());
        self.recover();
        Expr::Error(ErrorExpr { span, error: err })
    }

    /// An error expression for a missing value, without consuming anything.
    pub(super) fn missing_value(&self, context: &'static str, errors: &mut Vec<Error>) -> Expr<'a> {
        let span = span_at_start(&self.peek_span());
        let error = Error::with_span(expected("value", Some(context)), span);
        errors.push(error.clone());
        Expr::Error(ErrorExpr { span, error })
    }

    // ------------------------------------------------------------------
    // Recovery
    // ------------------------------------------------------------------

    /// Skip tokens until EOF or a token in `sync`.
    pub(super) fn skip_until(&mut self, sync: &[TokenKind]) {
        while self.peek_kind() != TokenKind::Eof && !sync.contains(&self.peek_kind()) {
            self.next_token();
        }
    }

    /// Skip to the next comma or closing delimiter.
    pub(super) fn recover(&mut self) {
        self.skip_until(&SYNC);
    }

    /// Whether `kind` closes a container that is currently open.
    fn expects_closer(&self, kind: TokenKind) -> bool {
        Delim::of_closer(kind).is_some_and(|delim| self.open[delim as usize] > 0)
    }

    /// Consume closing delimiters that no open container expects.
    fn skip_stray_closers(&mut self, errors: &mut Vec<Error>) {
        while Delim::of_closer(self.peek_kind()).is_some() && !self.expects_closer(self.peek_kind())
        {
            let tok = self.next_token();
            let c = tok.text.chars().next().unwrap_or_default();
            errors.push(Error::with_span(ErrorKind::UnexpectedChar(c), tok.span));
        }
    }

    /// Whether a list ends here: at EOF or any closer (after
    /// [`skip_stray_closers`](Self::skip_stray_closers), every closer is
    /// expected by this list or an enclosing container).
    fn at_list_end(&self) -> bool {
        self.peek_kind() == TokenKind::Eof || Delim::of_closer(self.peek_kind()).is_some()
    }

    /// Parse a comma-separated list after its opening delimiter, through the closer.
    ///
    /// Every iteration consumes at least one token or ends the loop: items
    /// start only at a token that is neither a closer nor EOF, and every item
    /// parser consumes that token unless it is a comma, which the loop eats.
    /// A closer of an enclosing container ends the list without being consumed,
    /// so `[(1, 2]` reports the missing `)` and still closes the `[`.
    pub(super) fn parse_list<T: ListItem<'a>>(
        &mut self,
        delim: Delim,
        context: &'static str,
        errors: &mut Vec<Error>,
        mut parse_item: impl FnMut(&mut Self, Trivia<'a>, &mut Vec<Error>) -> T,
    ) -> List<'a, T> {
        self.open[delim as usize] += 1;
        let mut items = Vec::new();
        let mut leading = self.take_trivia();
        let mut after_comma = false;

        loop {
            self.skip_stray_closers(errors);
            if self.at_list_end() {
                if after_comma && self.peek_kind() == TokenKind::Eof {
                    // Input cut off after `,`: report the end before the missing closer.
                    errors.push(Error::with_span(ErrorKind::Eof, self.peek_span()));
                }
                break;
            }

            let mut item = parse_item(self, leading, errors);
            let trailing = self.take_trivia();
            let comma = self.eat(TokenKind::Comma);
            after_comma = comma.is_some();
            item.set_tail(trailing, comma);
            items.push(item);
            leading = self.take_trivia();

            if comma.is_none() {
                self.skip_stray_closers(errors);
                if !self.at_list_end() {
                    errors.push(Error::with_span(
                        expected("comma", Some(context)),
                        self.peek_span(),
                    ));
                }
            }
        }

        let close = self.expect_closer(delim, context, errors);
        self.open[delim as usize] -= 1;
        List {
            items,
            trailing: leading,
            close,
        }
    }

    /// Consume the closer of `delim`, reporting and skipping anything before it.
    ///
    /// Stops without consuming at EOF or at a closer an enclosing container
    /// expects, returning a zero-width span there.
    pub(super) fn expect_closer(
        &mut self,
        delim: Delim,
        context: &'static str,
        errors: &mut Vec<Error>,
    ) -> Span {
        let closer = delim.closer();
        if let Some(span) = self.eat(closer) {
            return span;
        }
        errors.push(Error::with_span(
            expected(delim.expected_closer(), Some(context)),
            self.peek_span(),
        ));
        loop {
            let kind = self.peek_kind();
            if kind == closer {
                return self.next_token().span;
            }
            if kind == TokenKind::Eof || self.expects_closer(kind) {
                return span_at_start(&self.peek_span());
            }
            self.next_token();
        }
    }

    /// Run `f` with `delim` counted as open, for containers that are not
    /// lists, such as `Some(...)`.
    pub(super) fn with_open<R>(&mut self, delim: Delim, f: impl FnOnce(&mut Self) -> R) -> R {
        self.open[delim as usize] += 1;
        let result = f(self);
        self.open[delim as usize] -= 1;
        result
    }
}
