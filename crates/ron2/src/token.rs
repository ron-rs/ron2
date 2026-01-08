//! Token types for the RON lexer.

use crate::error::Span;

/// The kind of a token produced by the lexer.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    // Literals
    /// An identifier (e.g., `foo`, `Some`, `r#type`)
    Ident,
    /// An integer literal (e.g., `42`, `0x1F`, `0b1010`)
    Integer,
    /// A floating-point literal (e.g., `3.14`, `1e10`)
    Float,
    /// A string literal (e.g., `"hello"`, `r#"raw"#`)
    String,
    /// A byte string literal (e.g., `b"bytes"`)
    ByteString,
    /// A character literal (e.g., `'a'`, `'\n'`)
    Char,

    // Punctuation
    /// `(`
    LParen,
    /// `)`
    RParen,
    /// `{`
    LBrace,
    /// `}`
    RBrace,
    /// `[`
    LBracket,
    /// `]`
    RBracket,
    /// `:`
    Colon,
    /// `,`
    Comma,
    /// `#`
    Hash,
    /// `!`
    Bang,
    /// `=`
    Eq,

    // Trivia
    /// Whitespace (spaces, tabs, newlines)
    Whitespace,
    /// A line comment (e.g., `// comment`)
    LineComment,
    /// A block comment (e.g., `/* comment */`)
    BlockComment,

    // Special
    /// End of file
    Eof,
    /// Lexer error - invalid token
    Error,
}

impl TokenKind {
    /// Returns `true` if this token kind is trivia (whitespace or comments).
    ///
    /// Trivia tokens are typically skipped during parsing but preserved
    /// for formatting or syntax highlighting purposes.
    #[must_use]
    pub const fn is_trivia(self) -> bool {
        matches!(
            self,
            TokenKind::Whitespace | TokenKind::LineComment | TokenKind::BlockComment
        )
    }
}

/// A token produced by the lexer.
///
/// Contains the token kind, the raw source text, and the span indicating
/// where in the source this token was found.
#[derive(Debug, Clone)]
pub struct Token<'a> {
    /// The kind of token.
    pub kind: TokenKind,
    /// The raw source text of this token.
    pub text: &'a str,
    /// The span of this token in the source.
    pub span: Span,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error::Position;

    #[test]
    fn token_kind_is_trivia() {
        assert!(TokenKind::Whitespace.is_trivia());
        assert!(TokenKind::LineComment.is_trivia());
        assert!(TokenKind::BlockComment.is_trivia());

        assert!(!TokenKind::Ident.is_trivia());
        assert!(!TokenKind::Integer.is_trivia());
        assert!(!TokenKind::LParen.is_trivia());
        assert!(!TokenKind::Eof.is_trivia());
        assert!(!TokenKind::Error.is_trivia());
    }

    #[test]
    fn token_kind_copy() {
        let kind = TokenKind::Ident;
        let kind2 = kind; // Copy
        assert_eq!(kind, kind2);
    }

    #[test]
    fn token_clone() {
        let token = Token {
            kind: TokenKind::Ident,
            text: "foo",
            span: Span {
                start: Position { line: 1, col: 1 },
                end: Position { line: 1, col: 4 },
                start_offset: 0,
                end_offset: 3,
            },
        };
        let token2 = token.clone();
        assert_eq!(token.kind, token2.kind);
        assert_eq!(token.text, token2.text);
        assert_eq!(token.span, token2.span);
    }
}
