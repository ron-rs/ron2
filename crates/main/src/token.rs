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
    /// A byte literal (e.g., `b'a'`, `b'\x7f'`)
    Byte,

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
    /// Invalid input; the token text covers everything consumed for it.
    Error(LexError),
}

/// Why the lexer produced an [`TokenKind::Error`] token.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LexError {
    /// A character that cannot start any token.
    UnexpectedChar,
    /// `/*` without matching `*/`.
    UnclosedBlockComment,
    /// String or byte string without closing quote.
    UnterminatedString,
    /// Raw string with more than 255 `#`.
    TooManyHashes,
    /// Character or byte literal without closing `'`.
    UnterminatedChar,
    /// `''`.
    EmptyChar,
    /// Base prefix (`0x`, `0o`, `0b`) without digits.
    MissingDigits {
        /// The number base of the prefix.
        base: u8,
    },
    /// Base prefix followed by a digit that is invalid for that base.
    InvalidDigit {
        /// The number base of the prefix.
        base: u8,
        /// Byte offset of the invalid digit within the token text.
        at: u8,
    },
    /// Exponent marker (`e`/`E`) without digits.
    MissingExponent,
    /// `r#` not followed by a raw identifier or raw string (e.g. `r#`, `r##foo`).
    InvalidRawLiteral,
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
#[derive(Debug, Clone, Copy)]
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

    #[test]
    fn token_kind_is_trivia() {
        assert!(TokenKind::Whitespace.is_trivia());
        assert!(TokenKind::LineComment.is_trivia());
        assert!(TokenKind::BlockComment.is_trivia());

        assert!(!TokenKind::Ident.is_trivia());
        assert!(!TokenKind::Integer.is_trivia());
        assert!(!TokenKind::LParen.is_trivia());
        assert!(!TokenKind::Eof.is_trivia());
        assert!(!TokenKind::Error(LexError::UnexpectedChar).is_trivia());
    }
}
