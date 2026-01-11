//! Lexer for RON (Rusty Object Notation)
//!
//! This module provides a tokenizer that converts RON source text into a stream of tokens.
//! The lexer supports two modes:
//! - **Skip trivia mode** (default): Whitespace is skipped, suitable for value parsing
//! - **Emit trivia mode**: All tokens including whitespace are emitted, suitable for AST parsing

use crate::chars::{is_ident_first_char, is_ident_raw_char, is_whitespace_char};
use crate::error::{Position, Span};
use crate::token::{Token, TokenKind};

extern crate alloc;

/// A lexer that tokenizes RON source text.
///
/// The lexer implements `Iterator` and produces `Token` items for each
/// recognized construct in the source.
///
/// # Example
///
/// ```
/// use ron2::lexer::Lexer;
/// use ron2::token::TokenKind;
///
/// let mut lexer = Lexer::new("42");
/// let token = lexer.next().unwrap();
/// assert_eq!(token.kind, TokenKind::Integer);
/// ```
pub struct Lexer<'a> {
    /// The source text being tokenized
    source: &'a str,
    /// Current byte offset in the source
    cursor: usize,
    /// Current line number (1-indexed)
    line: usize,
    /// Current column number (1-indexed)
    col: usize,
    /// Whether to emit trivia tokens (whitespace, comments)
    emit_trivia: bool,
    /// Whether we've emitted the EOF token
    eof_emitted: bool,
}

impl<'a> Lexer<'a> {
    /// Create a new lexer for the given source text.
    ///
    /// By default, trivia (whitespace and comments) is skipped.
    /// Use [`with_trivia`](Self::with_trivia) to enable trivia emission.
    #[must_use]
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            cursor: 0,
            line: 1,
            col: 1,
            emit_trivia: false,
            eof_emitted: false,
        }
    }

    /// Enable or disable trivia emission.
    ///
    /// When enabled, whitespace and comments are returned as tokens.
    /// This is required for AST parsing that needs to preserve formatting.
    #[must_use]
    pub fn with_trivia(mut self, emit: bool) -> Self {
        self.emit_trivia = emit;
        self
    }

    /// Returns whether trivia emission is enabled.
    #[must_use]
    pub fn emits_trivia(&self) -> bool {
        self.emit_trivia
    }

    /// Returns the current position in the source.
    #[must_use]
    pub fn current_position(&self) -> Position {
        Position {
            line: self.line,
            col: self.col,
        }
    }

    /// Returns the current byte offset in the source.
    #[must_use]
    pub fn offset(&self) -> usize {
        self.cursor
    }

    /// Returns the remaining source text starting from the current cursor position.
    #[must_use]
    fn remaining(&self) -> &'a str {
        &self.source[self.cursor..]
    }

    /// Returns the next character without consuming it, if any.
    #[must_use]
    fn peek_char(&self) -> Option<char> {
        self.remaining().chars().next()
    }

    /// Returns the second character without consuming anything, if any.
    #[must_use]
    fn peek_char_second(&self) -> Option<char> {
        let mut chars = self.remaining().chars();
        chars.next();
        chars.next()
    }

    /// Consumes and returns the next character, updating position tracking.
    fn next_char(&mut self) -> Option<char> {
        let c = self.peek_char()?;
        self.cursor += c.len_utf8();

        // Update line/column tracking
        if c == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }

        Some(c)
    }

    /// Advances the cursor by the given number of bytes, updating position tracking.
    fn advance(&mut self, bytes: usize) {
        let text = &self.source[self.cursor..self.cursor + bytes];
        for c in text.chars() {
            if c == '\n' {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
        }
        self.cursor += bytes;
    }

    /// Checks if the remaining source starts with the given string.
    #[must_use]
    fn check_str(&self, s: &str) -> bool {
        self.remaining().starts_with(s)
    }

    /// Consumes the given string if it matches the start of the remaining source.
    /// Returns true if consumed.
    fn consume_str(&mut self, s: &str) -> bool {
        if self.check_str(s) {
            self.advance(s.len());
            true
        } else {
            false
        }
    }

    /// Tokenize the next token from the source.
    fn next_token(&mut self) -> Option<Token<'a>> {
        if self.cursor >= self.source.len() {
            if self.eof_emitted {
                return None;
            }
            self.eof_emitted = true;
            let pos = self.current_position();
            return Some(Token {
                kind: TokenKind::Eof,
                text: "",
                span: Span {
                    start: pos,
                    end: pos,
                    start_offset: self.cursor,
                    end_offset: self.cursor,
                },
            });
        }

        let start = self.cursor;
        let start_pos = self.current_position();

        // Check for whitespace first
        if let Some(c) = self.peek_char()
            && is_whitespace_char(c)
        {
            let kind = self.whitespace();
            if self.emit_trivia {
                let text = &self.source[start..self.cursor];
                return Some(Token {
                    kind,
                    text,
                    span: Span {
                        start: start_pos,
                        end: self.current_position(),
                        start_offset: start,
                        end_offset: self.cursor,
                    },
                });
            }
            // Skip trivia and try again
            return self.next_token();
        }

        // Check for comments
        if (self.check_str("//") || self.check_str("/*"))
            && let Some(kind) = self.try_comment()
        {
            if self.emit_trivia {
                let text = &self.source[start..self.cursor];
                return Some(Token {
                    kind,
                    text,
                    span: Span {
                        start: start_pos,
                        end: self.current_position(),
                        start_offset: start,
                        end_offset: self.cursor,
                    },
                });
            }
            // Skip trivia and try again
            return self.next_token();
        }

        // Try to match non-trivia token types
        let kind = self.match_token();

        let text = &self.source[start..self.cursor];
        Some(Token {
            kind,
            text,
            span: Span {
                start: start_pos,
                end: self.current_position(),
                start_offset: start,
                end_offset: self.cursor,
            },
        })
    }

    /// Match whitespace sequence.
    fn whitespace(&mut self) -> TokenKind {
        while let Some(c) = self.peek_char() {
            if is_whitespace_char(c) {
                self.next_char();
            } else {
                break;
            }
        }
        TokenKind::Whitespace
    }

    /// Match and consume the next token, returning its kind.
    fn match_token(&mut self) -> TokenKind {
        let Some(c) = self.peek_char() else {
            return TokenKind::Eof;
        };

        // Single-character punctuation
        match c {
            '(' => {
                self.next_char();
                return TokenKind::LParen;
            }
            ')' => {
                self.next_char();
                return TokenKind::RParen;
            }
            '{' => {
                self.next_char();
                return TokenKind::LBrace;
            }
            '}' => {
                self.next_char();
                return TokenKind::RBrace;
            }
            '[' => {
                self.next_char();
                return TokenKind::LBracket;
            }
            ']' => {
                self.next_char();
                return TokenKind::RBracket;
            }
            ':' => {
                self.next_char();
                return TokenKind::Colon;
            }
            ',' => {
                self.next_char();
                return TokenKind::Comma;
            }
            '#' => {
                self.next_char();
                return TokenKind::Hash;
            }
            '!' => {
                self.next_char();
                return TokenKind::Bang;
            }
            '=' => {
                self.next_char();
                return TokenKind::Eq;
            }
            _ => {}
        }

        // Strings (including raw strings)
        if c == '"' {
            return self.string();
        }
        if c == 'r'
            && let Some(kind) = self.try_raw_string()
        {
            return kind;
        }

        // Byte strings
        if c == 'b'
            && let Some(kind) = self.try_byte_string()
        {
            return kind;
        }

        // Characters
        if c == '\'' {
            return self.char_literal();
        }

        // Numbers (including those starting with + or -)
        if c.is_ascii_digit() || ((c == '+' || c == '-') && self.is_number_ahead()) {
            return self.number();
        }

        // Identifiers (including raw identifiers r#...)
        if is_ident_first_char(c) || c == 'r' {
            return self.identifier();
        }

        // Unknown character - consume it and return error token
        self.next_char();
        TokenKind::Error
    }

    /// Check if there's a number ahead (after + or -).
    fn is_number_ahead(&self) -> bool {
        let remaining = self.remaining();
        if remaining.len() < 2 {
            return false;
        }
        // Check for digit after sign
        if remaining.chars().nth(1).is_some_and(|c| c.is_ascii_digit()) {
            return true;
        }
        // Check for inf/NaN after sign
        let after_sign = &remaining[1..];
        after_sign.starts_with("inf") || after_sign.starts_with("NaN")
    }

    /// Try to match a comment (line or block).
    fn try_comment(&mut self) -> Option<TokenKind> {
        if !self.check_str("/") {
            return None;
        }

        let second = self.peek_char_second()?;

        match second {
            '/' => {
                // Line comment
                self.advance(2); // consume //
                while let Some(c) = self.peek_char() {
                    if c == '\n' {
                        break;
                    }
                    self.next_char();
                }
                Some(TokenKind::LineComment)
            }
            '*' => {
                // Block comment (supports nesting)
                self.advance(2); // consume /*
                let mut depth = 1;

                while depth > 0 {
                    if self.remaining().is_empty() {
                        // Unclosed block comment
                        return Some(TokenKind::Error);
                    }

                    if self.consume_str("/*") {
                        depth += 1;
                    } else if self.consume_str("*/") {
                        depth -= 1;
                    } else {
                        self.next_char();
                    }
                }

                Some(TokenKind::BlockComment)
            }
            _ => None,
        }
    }

    /// Parse a regular string literal ("...").
    fn string(&mut self) -> TokenKind {
        self.next_char(); // consume opening "

        loop {
            match self.peek_char() {
                None => {
                    // Unterminated string
                    return TokenKind::Error;
                }
                Some('"') => {
                    self.next_char();
                    return TokenKind::String;
                }
                Some('\\') => {
                    // Escape sequence - consume backslash and next char
                    self.next_char();
                    if self.peek_char().is_some() {
                        self.next_char();
                    }
                }
                Some(_) => {
                    self.next_char();
                }
            }
        }
    }

    /// Try to match a raw string (r"..." or r#"..."# with any number of #).
    fn try_raw_string(&mut self) -> Option<TokenKind> {
        if !self.check_str("r") {
            return None;
        }

        let remaining = self.remaining();
        let mut chars = remaining.chars();
        chars.next(); // skip 'r'

        // Count leading hashes
        let mut hash_count = 0;
        for c in chars.by_ref() {
            if c == '#' {
                hash_count += 1;
            } else {
                break;
            }
        }

        // Check for opening quote
        let prefix_len = 1 + hash_count; // 'r' + hashes
        if !remaining[prefix_len..].starts_with('"') {
            // Not a raw string - might be a raw identifier
            return None;
        }

        self.advance(prefix_len + 1); // consume r###..."

        // Find the closing sequence: " followed by same number of #
        let closing: alloc::string::String = core::iter::once('"')
            .chain(core::iter::repeat_n('#', hash_count))
            .collect();

        loop {
            if self.remaining().is_empty() {
                // Unterminated raw string
                return Some(TokenKind::Error);
            }

            if self.check_str(&closing) {
                self.advance(closing.len());
                return Some(TokenKind::String);
            }

            self.next_char();
        }
    }

    /// Try to match a byte string (b"..." or br"..." or br#"..."#).
    fn try_byte_string(&mut self) -> Option<TokenKind> {
        if !self.check_str("b") {
            return None;
        }

        let remaining = self.remaining();

        // b"..." - regular byte string
        if remaining.starts_with("b\"") {
            self.advance(2); // consume b"
            return Some(self.byte_string_contents());
        }

        // br"..." or br#"..."# - raw byte string
        if let Some(after_br) = remaining.strip_prefix("br") {
            let mut chars = after_br.chars();

            // Count leading hashes
            let mut hash_count = 0;
            for c in chars.by_ref() {
                if c == '#' {
                    hash_count += 1;
                } else {
                    break;
                }
            }

            // Check for opening quote
            let prefix_len = 2 + hash_count; // "br" + hashes
            if remaining.len() > prefix_len && remaining[prefix_len..].starts_with('"') {
                self.advance(prefix_len + 1); // consume br###..."

                // Find the closing sequence
                let closing: alloc::string::String = core::iter::once('"')
                    .chain(core::iter::repeat_n('#', hash_count))
                    .collect();

                loop {
                    if self.remaining().is_empty() {
                        return Some(TokenKind::Error);
                    }

                    if self.check_str(&closing) {
                        self.advance(closing.len());
                        return Some(TokenKind::ByteString);
                    }

                    self.next_char();
                }
            }
        }

        None
    }

    /// Parse the contents of a byte string (after b").
    fn byte_string_contents(&mut self) -> TokenKind {
        loop {
            match self.peek_char() {
                None => {
                    return TokenKind::Error;
                }
                Some('"') => {
                    self.next_char();
                    return TokenKind::ByteString;
                }
                Some('\\') => {
                    self.next_char();
                    if self.peek_char().is_some() {
                        self.next_char();
                    }
                }
                Some(_) => {
                    self.next_char();
                }
            }
        }
    }

    /// Parse a character literal ('x').
    fn char_literal(&mut self) -> TokenKind {
        self.next_char(); // consume opening '

        match self.peek_char() {
            None => TokenKind::Error,
            Some('\\') => {
                // Escape sequence
                self.next_char();
                if let Some(c) = self.peek_char() {
                    self.next_char();
                    // For \x, \u escapes, consume additional characters
                    if c == 'x' {
                        // \xNN
                        for _ in 0..2 {
                            if self.peek_char().is_some() {
                                self.next_char();
                            }
                        }
                    } else if c == 'u' {
                        // \u{NNNN}
                        if self.consume_str("{") {
                            while let Some(ch) = self.peek_char() {
                                if ch == '}' {
                                    self.next_char();
                                    break;
                                }
                                self.next_char();
                            }
                        }
                    }
                }
                // Expect closing '
                if self.peek_char() == Some('\'') {
                    self.next_char();
                    TokenKind::Char
                } else {
                    TokenKind::Error
                }
            }
            Some('\'') => {
                // Empty char literal
                self.next_char();
                TokenKind::Error
            }
            Some(_) => {
                self.next_char();
                if self.peek_char() == Some('\'') {
                    self.next_char();
                    TokenKind::Char
                } else {
                    TokenKind::Error
                }
            }
        }
    }

    /// Parse a number (integer or float).
    fn number(&mut self) -> TokenKind {
        // Handle optional sign
        if self.peek_char() == Some('+') || self.peek_char() == Some('-') {
            self.next_char();
        }

        // Check for special float literals: inf, NaN
        if self.check_str("inf") {
            self.advance(3);
            return self.consume_float_suffix();
        }
        if self.check_str("NaN") {
            self.advance(3);
            return self.consume_float_suffix();
        }

        // Check for base prefixes
        let base = if self.consume_str("0x") || self.consume_str("0X") {
            16
        } else if self.consume_str("0o") || self.consume_str("0O") {
            8
        } else if self.consume_str("0b") || self.consume_str("0B") {
            2
        } else {
            10
        };

        // Consume digits according to base
        let mut has_digits = false;
        while let Some(c) = self.peek_char() {
            if c == '_' || is_digit_for_base(c, base) {
                self.next_char();
                has_digits = true;
            } else {
                break;
            }
        }

        if !has_digits {
            // No valid digits found after prefix - consume any alphanumeric chars
            // that might have been intended as digits for better error reporting
            while let Some(c) = self.peek_char() {
                if c.is_ascii_alphanumeric() || c == '_' {
                    self.next_char();
                } else {
                    break;
                }
            }
            return TokenKind::Error;
        }

        // For decimal numbers, check for float components
        if base == 10 {
            let mut is_float = false;

            // Decimal point
            if self.peek_char() == Some('.') {
                // Look ahead to see if this is a float or something else
                if self.remaining().len() > 1 {
                    let next = self.remaining().chars().nth(1);
                    if next.is_some_and(|c| c.is_ascii_digit()) {
                        self.next_char(); // consume '.'
                        is_float = true;
                        // Consume fractional digits
                        while let Some(c) = self.peek_char() {
                            if c == '_' || c.is_ascii_digit() {
                                self.next_char();
                            } else {
                                break;
                            }
                        }
                    }
                }
            }

            // Exponent
            if self.peek_char() == Some('e') || self.peek_char() == Some('E') {
                self.next_char();
                is_float = true;
                // Optional sign
                if self.peek_char() == Some('+') || self.peek_char() == Some('-') {
                    self.next_char();
                }
                // Exponent digits
                while let Some(c) = self.peek_char() {
                    if c == '_' || c.is_ascii_digit() {
                        self.next_char();
                    } else {
                        break;
                    }
                }
            }

            // Float suffix
            if self.check_str("f32") || self.check_str("f64") {
                let suffix_end = self.cursor + 3;
                if suffix_end >= self.source.len()
                    || !is_ident_continue_char(
                        self.source[suffix_end..].chars().next().unwrap_or(' '),
                    )
                {
                    self.advance(3);
                    return TokenKind::Float;
                }
            }

            if is_float {
                return TokenKind::Float;
            }
        }

        // Integer suffix
        self.consume_integer_suffix();

        TokenKind::Integer
    }

    /// Consume an optional integer type suffix (i8, u32, etc.).
    fn consume_integer_suffix(&mut self) {
        let suffixes = [
            "i128", "i64", "i32", "i16", "i8", // longer first to avoid partial matches
            "u128", "u64", "u32", "u16", "u8",
        ];

        for suffix in &suffixes {
            if self.check_str(suffix) {
                // Make sure suffix is not followed by identifier char
                let suffix_end = self.cursor + suffix.len();
                if suffix_end >= self.source.len()
                    || !is_ident_continue_char(
                        self.source[suffix_end..].chars().next().unwrap_or(' '),
                    )
                {
                    self.advance(suffix.len());
                    return;
                }
            }
        }
    }

    /// Consume an optional float type suffix and return the token kind.
    fn consume_float_suffix(&mut self) -> TokenKind {
        if self.check_str("f32") || self.check_str("f64") {
            let suffix_end = self.cursor + 3;
            if suffix_end >= self.source.len()
                || !is_ident_continue_char(self.source[suffix_end..].chars().next().unwrap_or(' '))
            {
                self.advance(3);
            }
        }
        TokenKind::Float
    }

    /// Parse an identifier (standard or raw r#...).
    fn identifier(&mut self) -> TokenKind {
        // Check for raw identifier (r#ident)
        if self.check_str("r#") {
            let remaining = &self.remaining()[2..];
            // Check if it's a raw string (r#") or raw identifier
            if remaining.starts_with('"') {
                // It's a raw string, not an identifier
                return TokenKind::Error;
            }

            // It's a raw identifier
            self.advance(2); // consume r#
            while let Some(c) = self.peek_char() {
                if is_ident_raw_char(c) {
                    self.next_char();
                } else {
                    break;
                }
            }
            return TokenKind::Ident;
        }

        // Check for special float literals: inf, NaN (without sign)
        if self.check_ident("inf") || self.check_ident("NaN") {
            self.advance(3);
            // Check for optional float suffix
            if self.check_str("f32") || self.check_str("f64") {
                let suffix_end = self.cursor + 3;
                if suffix_end >= self.source.len()
                    || !is_ident_continue_char(
                        self.source[suffix_end..].chars().next().unwrap_or(' '),
                    )
                {
                    self.advance(3);
                }
            }
            return TokenKind::Float;
        }

        // Standard identifier
        let first = self.peek_char();
        if !first.is_some_and(is_ident_first_char) {
            return TokenKind::Error;
        }

        self.next_char(); // consume first char

        while let Some(c) = self.peek_char() {
            if is_ident_continue_char(c) {
                self.next_char();
            } else {
                break;
            }
        }

        TokenKind::Ident
    }

    /// Check if remaining text starts with the given identifier
    /// (not followed by identifier continuation chars).
    fn check_ident(&self, ident: &str) -> bool {
        if !self.check_str(ident) {
            return false;
        }
        let after = self.cursor + ident.len();
        if after >= self.source.len() {
            return true;
        }
        !self.source[after..]
            .chars()
            .next()
            .is_some_and(is_ident_continue_char)
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

// Character classification functions using unicode_ident

/// Check if a character can continue an identifier.
#[must_use]
pub fn is_ident_continue_char(c: char) -> bool {
    unicode_ident::is_xid_continue(c)
}

/// Check if a character is a valid digit for the given base.
const fn is_digit_for_base(c: char, base: u8) -> bool {
    match base {
        2 => matches!(c, '0' | '1'),
        8 => matches!(c, '0'..='7'),
        10 => c.is_ascii_digit(),
        16 => c.is_ascii_hexdigit(),
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tokenize(source: &str) -> alloc::vec::Vec<TokenKind> {
        Lexer::new(source).map(|t| t.kind).collect()
    }

    fn tokenize_with_trivia(source: &str) -> alloc::vec::Vec<TokenKind> {
        Lexer::new(source)
            .with_trivia(true)
            .map(|t| t.kind)
            .collect()
    }

    #[test]
    fn test_punctuation() {
        assert_eq!(
            tokenize("(){}[]:,#!="),
            alloc::vec![
                TokenKind::LParen,
                TokenKind::RParen,
                TokenKind::LBrace,
                TokenKind::RBrace,
                TokenKind::LBracket,
                TokenKind::RBracket,
                TokenKind::Colon,
                TokenKind::Comma,
                TokenKind::Hash,
                TokenKind::Bang,
                TokenKind::Eq,
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_line_comment() {
        // Without trivia - comments skipped
        assert_eq!(
            tokenize("// comment\n42"),
            alloc::vec![TokenKind::Integer, TokenKind::Eof]
        );

        // With trivia - comments preserved
        assert_eq!(
            tokenize_with_trivia("// comment\n42"),
            alloc::vec![
                TokenKind::LineComment,
                TokenKind::Whitespace,
                TokenKind::Integer,
                TokenKind::Eof
            ]
        );
    }

    #[test]
    fn test_block_comment() {
        assert_eq!(
            tokenize("/* comment */42"),
            alloc::vec![TokenKind::Integer, TokenKind::Eof]
        );

        assert_eq!(
            tokenize_with_trivia("/* comment */42"),
            alloc::vec![TokenKind::BlockComment, TokenKind::Integer, TokenKind::Eof]
        );
    }

    #[test]
    fn test_nested_block_comment() {
        assert_eq!(
            tokenize("/* outer /* inner */ */42"),
            alloc::vec![TokenKind::Integer, TokenKind::Eof]
        );
    }

    #[test]
    fn test_string() {
        assert_eq!(
            tokenize("\"hello\""),
            alloc::vec![TokenKind::String, TokenKind::Eof]
        );
        assert_eq!(
            tokenize("\"hello\\nworld\""),
            alloc::vec![TokenKind::String, TokenKind::Eof]
        );
    }

    #[test]
    fn test_raw_string() {
        assert_eq!(
            tokenize("r\"hello\""),
            alloc::vec![TokenKind::String, TokenKind::Eof]
        );
        assert_eq!(
            tokenize("r#\"hello\"#"),
            alloc::vec![TokenKind::String, TokenKind::Eof]
        );
        assert_eq!(
            tokenize("r##\"hello\"##"),
            alloc::vec![TokenKind::String, TokenKind::Eof]
        );
    }

    #[test]
    fn test_byte_string() {
        assert_eq!(
            tokenize("b\"hello\""),
            alloc::vec![TokenKind::ByteString, TokenKind::Eof]
        );
    }

    #[test]
    fn test_raw_byte_string() {
        assert_eq!(
            tokenize("br\"hello\""),
            alloc::vec![TokenKind::ByteString, TokenKind::Eof]
        );
        assert_eq!(
            tokenize("br#\"hello\"#"),
            alloc::vec![TokenKind::ByteString, TokenKind::Eof]
        );
    }

    #[test]
    fn test_char() {
        assert_eq!(
            tokenize("'a'"),
            alloc::vec![TokenKind::Char, TokenKind::Eof]
        );
        assert_eq!(
            tokenize("'\\n'"),
            alloc::vec![TokenKind::Char, TokenKind::Eof]
        );
        assert_eq!(
            tokenize("'\\x41'"),
            alloc::vec![TokenKind::Char, TokenKind::Eof]
        );
    }

    #[test]
    fn test_integer() {
        assert_eq!(
            tokenize("42"),
            alloc::vec![TokenKind::Integer, TokenKind::Eof]
        );
        assert_eq!(
            tokenize("0x2A"),
            alloc::vec![TokenKind::Integer, TokenKind::Eof]
        );
        assert_eq!(
            tokenize("0o52"),
            alloc::vec![TokenKind::Integer, TokenKind::Eof]
        );
        assert_eq!(
            tokenize("0b101010"),
            alloc::vec![TokenKind::Integer, TokenKind::Eof]
        );
        assert_eq!(
            tokenize("42i32"),
            alloc::vec![TokenKind::Integer, TokenKind::Eof]
        );
        assert_eq!(
            tokenize("-42"),
            alloc::vec![TokenKind::Integer, TokenKind::Eof]
        );
        assert_eq!(
            tokenize("+42"),
            alloc::vec![TokenKind::Integer, TokenKind::Eof]
        );
    }

    #[test]
    fn test_float() {
        assert_eq!(
            tokenize("3.14"),
            alloc::vec![TokenKind::Float, TokenKind::Eof]
        );
        assert_eq!(
            tokenize("3.14f32"),
            alloc::vec![TokenKind::Float, TokenKind::Eof]
        );
        assert_eq!(
            tokenize("3e10"),
            alloc::vec![TokenKind::Float, TokenKind::Eof]
        );
        assert_eq!(
            tokenize("3.14e-10"),
            alloc::vec![TokenKind::Float, TokenKind::Eof]
        );
        assert_eq!(
            tokenize("inf"),
            alloc::vec![TokenKind::Float, TokenKind::Eof]
        );
        assert_eq!(
            tokenize("NaN"),
            alloc::vec![TokenKind::Float, TokenKind::Eof]
        );
        assert_eq!(
            tokenize("-inf"),
            alloc::vec![TokenKind::Float, TokenKind::Eof]
        );
    }

    #[test]
    fn test_identifier() {
        assert_eq!(
            tokenize("foo"),
            alloc::vec![TokenKind::Ident, TokenKind::Eof]
        );
        assert_eq!(
            tokenize("_bar"),
            alloc::vec![TokenKind::Ident, TokenKind::Eof]
        );
        assert_eq!(
            tokenize("foo123"),
            alloc::vec![TokenKind::Ident, TokenKind::Eof]
        );
    }

    #[test]
    fn test_raw_identifier() {
        assert_eq!(
            tokenize("r#foo"),
            alloc::vec![TokenKind::Ident, TokenKind::Eof]
        );
        assert_eq!(
            tokenize("r#foo.bar"),
            alloc::vec![TokenKind::Ident, TokenKind::Eof]
        );
    }

    #[test]
    fn test_whitespace_handling() {
        // Without trivia - whitespace skipped
        assert_eq!(
            tokenize("  42  "),
            alloc::vec![TokenKind::Integer, TokenKind::Eof]
        );
        assert_eq!(
            tokenize("\t\n42"),
            alloc::vec![TokenKind::Integer, TokenKind::Eof]
        );

        // With trivia - whitespace preserved
        assert_eq!(
            tokenize_with_trivia("  42  "),
            alloc::vec![
                TokenKind::Whitespace,
                TokenKind::Integer,
                TokenKind::Whitespace,
                TokenKind::Eof
            ]
        );
    }

    #[test]
    fn test_complete_ron() {
        let tokens = tokenize("Config(name: \"test\", value: 42)");
        assert_eq!(
            tokens,
            alloc::vec![
                TokenKind::Ident,   // Config
                TokenKind::LParen,  // (
                TokenKind::Ident,   // name
                TokenKind::Colon,   // :
                TokenKind::String,  // "test"
                TokenKind::Comma,   // ,
                TokenKind::Ident,   // value
                TokenKind::Colon,   // :
                TokenKind::Integer, // 42
                TokenKind::RParen,  // )
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_token_spans() {
        let mut lexer = Lexer::new("foo 123");

        let token1 = lexer.next().unwrap();
        assert_eq!(token1.kind, TokenKind::Ident);
        assert_eq!(token1.text, "foo");
        assert_eq!(token1.span.start.line, 1);
        assert_eq!(token1.span.start.col, 1);
        assert_eq!(token1.span.end.line, 1);
        assert_eq!(token1.span.end.col, 4);

        let token2 = lexer.next().unwrap();
        assert_eq!(token2.kind, TokenKind::Integer);
        assert_eq!(token2.text, "123");
        assert_eq!(token2.span.start.line, 1);
        assert_eq!(token2.span.start.col, 5);
        assert_eq!(token2.span.end.line, 1);
        assert_eq!(token2.span.end.col, 8);
    }

    #[test]
    fn test_multiline_spans() {
        let mut lexer = Lexer::new("foo\nbar");

        let token1 = lexer.next().unwrap();
        assert_eq!(token1.kind, TokenKind::Ident);
        assert_eq!(token1.text, "foo");
        assert_eq!(token1.span.start.line, 1);
        assert_eq!(token1.span.end.line, 1);

        let token2 = lexer.next().unwrap();
        assert_eq!(token2.kind, TokenKind::Ident);
        assert_eq!(token2.text, "bar");
        assert_eq!(token2.span.start.line, 2);
        assert_eq!(token2.span.start.col, 1);
    }

    #[test]
    fn test_eof_token() {
        let mut lexer = Lexer::new("");
        let token = lexer.next().unwrap();
        assert_eq!(token.kind, TokenKind::Eof);
        assert!(lexer.next().is_none());
    }

    #[test]
    fn test_trivia_round_trip() {
        let source = "// comment\nfoo /* block */ bar";
        let tokens: alloc::vec::Vec<_> = Lexer::new(source).with_trivia(true).collect();

        // Reconstruct source from tokens
        let reconstructed: alloc::string::String = tokens.iter().map(|t| t.text).collect();
        assert_eq!(source, reconstructed);
    }
}
