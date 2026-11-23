//! Lexer for RON (Rusty Object Notation).
//!
//! Converts RON source text into a stream of tokens. Whitespace and comments
//! are emitted as trivia tokens so the AST parser can preserve them.
//!
//! The lexer never fails: invalid input becomes a [`TokenKind::Error`] token
//! whose [`LexError`] says what is wrong. Every call consumes at least one byte
//! until the end of input, after which it keeps returning [`TokenKind::Eof`].

use crate::{
    chars::{is_ident_continue_char, is_ident_first_char, is_ident_raw_char, is_whitespace_char},
    error::{LineIndex, LineIndexCursor, Span},
    token::{LexError, Token, TokenKind},
};

/// Raw strings may use at most this many `#`, like in Rust.
const MAX_RAW_HASHES: usize = 255;

/// A lexer that tokenizes RON source text, including trivia.
pub struct Lexer<'a> {
    source: &'a str,
    /// Current byte offset; always on a UTF-8 char boundary.
    cursor: usize,
    line_index: LineIndex,
    line_cursor: LineIndexCursor,
    /// Whether the iterator has yielded `Eof`.
    done: bool,
}

impl<'a> Lexer<'a> {
    /// Create a new lexer for the given source text.
    #[must_use]
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            cursor: 0,
            line_index: LineIndex::new(source),
            line_cursor: LineIndexCursor::new(),
            done: false,
        }
    }

    /// The line index of the source, for converting byte offsets to positions.
    #[must_use]
    pub fn line_index(&self) -> &LineIndex {
        &self.line_index
    }

    /// Lex the next token. Returns `Eof` tokens forever once the input is exhausted.
    pub fn next_token(&mut self) -> Token<'a> {
        let start = self.cursor;
        let start_pos = self.line_cursor.position(&self.line_index, start);
        let kind = self.lex();
        let end_pos = self.line_cursor.position(&self.line_index, self.cursor);
        Token {
            kind,
            text: self.source.get(start..self.cursor).unwrap_or_default(),
            span: Span {
                start: start_pos,
                end: end_pos,
                start_offset: start,
                end_offset: self.cursor,
            },
        }
    }

    fn remaining(&self) -> &'a str {
        self.source.get(self.cursor..).unwrap_or_default()
    }

    #[inline]
    fn peek_byte(&self) -> Option<u8> {
        self.source.as_bytes().get(self.cursor).copied()
    }

    /// The byte `n` bytes after the cursor.
    #[inline]
    fn peek_byte_at(&self, n: usize) -> Option<u8> {
        self.source.as_bytes().get(self.cursor + n).copied()
    }

    #[inline]
    fn peek_char(&self) -> Option<char> {
        match self.peek_byte()? {
            b if b.is_ascii() => Some(b as char),
            _ => self.remaining().chars().next(),
        }
    }

    #[inline]
    fn next_char(&mut self) -> Option<char> {
        let c = self.peek_char()?;
        self.cursor += c.len_utf8();
        Some(c)
    }

    /// Advance by `bytes`, which must end on a char boundary.
    #[inline]
    fn advance(&mut self, bytes: usize) {
        self.cursor += bytes;
    }

    fn eat_while(&mut self, pred: impl Fn(char) -> bool) {
        while self.peek_char().is_some_and(&pred) {
            self.next_char();
        }
    }

    /// Consume `suffix` if it is not followed by an identifier char
    /// (e.g. `f32` in `1.5f32`, but not in `1.5f32x`).
    fn eat_suffix(&mut self, suffix: &str) -> bool {
        let found = self
            .remaining()
            .strip_prefix(suffix)
            .is_some_and(|rest| !rest.chars().next().is_some_and(is_ident_continue_char));
        if found {
            self.advance(suffix.len());
        }
        found
    }

    /// Consume the next token and return its kind.
    fn lex(&mut self) -> TokenKind {
        let Some(c) = self.peek_char() else {
            self.done = true;
            return TokenKind::Eof;
        };

        let single = match c {
            '(' => Some(TokenKind::LParen),
            ')' => Some(TokenKind::RParen),
            '{' => Some(TokenKind::LBrace),
            '}' => Some(TokenKind::RBrace),
            '[' => Some(TokenKind::LBracket),
            ']' => Some(TokenKind::RBracket),
            ':' => Some(TokenKind::Colon),
            ',' => Some(TokenKind::Comma),
            '#' => Some(TokenKind::Hash),
            '!' => Some(TokenKind::Bang),
            '=' => Some(TokenKind::Eq),
            _ => None,
        };
        if let Some(kind) = single {
            self.advance(1);
            return kind;
        }

        match c {
            _ if is_whitespace_char(c) => {
                self.eat_while(is_whitespace_char);
                TokenKind::Whitespace
            }
            '/' if self.peek_byte_at(1) == Some(b'/') => self.line_comment(),
            '/' if self.peek_byte_at(1) == Some(b'*') => self.block_comment(),
            '"' => self.quoted(0, TokenKind::String),
            '\'' => self.char_literal(0, TokenKind::Char),
            '0'..='9' => self.number(),
            '+' | '-' | '.' if self.is_number_ahead() => self.number(),
            'r' => self.raw_string(1, TokenKind::String).unwrap_or_else(|| {
                if self.peek_byte_at(1) == Some(b'#') {
                    self.raw_identifier()
                } else {
                    self.identifier()
                }
            }),
            'b' => match self.peek_byte_at(1) {
                Some(b'"') => self.quoted(1, TokenKind::ByteString),
                Some(b'\'') => self.char_literal(1, TokenKind::Byte),
                Some(b'r') => self
                    .raw_string(2, TokenKind::ByteString)
                    .unwrap_or_else(|| self.identifier()),
                _ => self.identifier(),
            },
            _ if is_ident_first_char(c) => self.identifier(),
            _ => {
                self.next_char();
                TokenKind::Error(LexError::UnexpectedChar)
            }
        }
    }

    fn line_comment(&mut self) -> TokenKind {
        let len = self
            .remaining()
            .bytes()
            .position(|b| b == b'\n')
            .unwrap_or(self.remaining().len());
        self.advance(len);
        TokenKind::LineComment
    }

    /// Lex a (possibly nested) block comment.
    fn block_comment(&mut self) -> TokenKind {
        self.advance(2);
        let mut depth = 1usize;
        let bytes = self.source.as_bytes();
        while let Some(&b) = bytes.get(self.cursor) {
            match (b, bytes.get(self.cursor + 1)) {
                (b'/', Some(b'*')) => {
                    depth += 1;
                    self.advance(2);
                }
                (b'*', Some(b'/')) => {
                    self.advance(2);
                    depth -= 1;
                    if depth == 0 {
                        return TokenKind::BlockComment;
                    }
                }
                _ => {
                    self.next_char();
                }
            }
        }
        TokenKind::Error(LexError::UnclosedBlockComment)
    }

    /// Lex `"..."` after a prefix of `prefix_len` bytes (`b` for byte strings).
    ///
    /// Escapes are only skipped here; the parser validates and decodes them.
    fn quoted(&mut self, prefix_len: usize, kind: TokenKind) -> TokenKind {
        self.advance(prefix_len + 1);
        loop {
            let Some(len) = self
                .remaining()
                .bytes()
                .position(|b| b == b'"' || b == b'\\')
            else {
                self.cursor = self.source.len();
                return TokenKind::Error(LexError::UnterminatedString);
            };
            self.advance(len);
            if self.next_char() == Some('"') {
                return kind;
            }
            // Skip the escaped char so an escaped quote does not end the string.
            self.next_char();
        }
    }

    /// Lex `r#*"..."#*` after a prefix of `prefix_len` bytes (`r` or `br`).
    ///
    /// Returns `None` without consuming anything if the prefix and hashes are
    /// not followed by `"`.
    fn raw_string(&mut self, prefix_len: usize, kind: TokenKind) -> Option<TokenKind> {
        let after_prefix = self.remaining().get(prefix_len..)?;
        let hashes = after_prefix.bytes().take_while(|&b| b == b'#').count();
        if after_prefix.as_bytes().get(hashes) != Some(&b'"') {
            return None;
        }
        self.advance(prefix_len + hashes + 1);

        let body = self.remaining().as_bytes();
        let mut search = 0;
        while let Some(quote) = body[search..].iter().position(|&b| b == b'"') {
            let quote = search + quote;
            let closing = body
                .get(quote + 1..quote + 1 + hashes)
                .is_some_and(|tail| tail.iter().all(|&b| b == b'#'));
            if closing {
                self.advance(quote + 1 + hashes);
                return Some(if hashes > MAX_RAW_HASHES {
                    TokenKind::Error(LexError::TooManyHashes)
                } else {
                    kind
                });
            }
            search = quote + 1;
        }
        self.cursor = self.source.len();
        Some(TokenKind::Error(LexError::UnterminatedString))
    }

    /// Lex `'...'` after a prefix of `prefix_len` bytes (`b` for byte literals).
    ///
    /// Only the extent is determined here; the parser decodes the content.
    fn char_literal(&mut self, prefix_len: usize, kind: TokenKind) -> TokenKind {
        self.advance(prefix_len + 1);
        match self.next_char() {
            None | Some('\n') => return TokenKind::Error(LexError::UnterminatedChar),
            // `'''` is the quote char, as in upstream RON; `''` is empty.
            Some('\'') if self.peek_byte() != Some(b'\'') => {
                return TokenKind::Error(LexError::EmptyChar);
            }
            Some('\\') => {
                // Skip the escaped char, then the rest of the escape (`\x41`, `\u{41}`).
                if self.next_char().is_some() {
                    self.eat_while(|c| c != '\'' && c != '\n');
                }
            }
            Some(_) => {}
        }
        if self.peek_byte() == Some(b'\'') {
            self.advance(1);
            kind
        } else {
            TokenKind::Error(LexError::UnterminatedChar)
        }
    }

    /// Whether a number starts at a `+`, `-` or `.` under the cursor.
    fn is_number_ahead(&self) -> bool {
        let sign = usize::from(matches!(self.peek_byte(), Some(b'+' | b'-')));
        if sign == 1 && self.special_float_len(1).is_some() {
            return true;
        }
        match self.peek_byte_at(sign) {
            Some(b'0'..=b'9') => true,
            Some(b'.') => self
                .peek_byte_at(sign + 1)
                .is_some_and(|b| b.is_ascii_digit()),
            _ => false,
        }
    }

    /// Length of `inf`/`NaN` (with optional `f32`/`f64` suffix) starting
    /// `offset` bytes after the cursor, if it forms a complete word.
    fn special_float_len(&self, offset: usize) -> Option<usize> {
        let rest = self.remaining().get(offset..)?;
        let word = ["inf", "NaN"].into_iter().find(|w| rest.starts_with(w))?;
        let suffix = ["f32", "f64"]
            .into_iter()
            .find(|s| rest[word.len()..].starts_with(s))
            .map_or(0, str::len);
        let len = word.len() + suffix;
        let complete = !rest[len..]
            .chars()
            .next()
            .is_some_and(is_ident_continue_char);
        complete.then_some(len)
    }

    /// Consume digits (and `_` separators) valid for `base`, returning how many
    /// digits were consumed.
    fn eat_digits(&mut self, base: u8) -> usize {
        let mut digits = 0;
        while let Some(b) = self.peek_byte() {
            match b {
                b'_' => {}
                _ if char::from(b).is_digit(u32::from(base)) => digits += 1,
                _ => break,
            }
            self.advance(1);
        }
        digits
    }

    /// Lex an integer or float, optionally signed.
    fn number(&mut self) -> TokenKind {
        let start = self.cursor;
        if matches!(self.peek_byte(), Some(b'+' | b'-')) {
            self.advance(1);
        }
        if let Some(len) = self.special_float_len(0) {
            self.advance(len);
            return TokenKind::Float;
        }

        let base = match (self.peek_byte(), self.peek_byte_at(1)) {
            (Some(b'0'), Some(b'x' | b'X')) => 16,
            (Some(b'0'), Some(b'o' | b'O')) => 8,
            (Some(b'0'), Some(b'b' | b'B')) => 2,
            _ => 10,
        };
        if base != 10 {
            self.advance(2);
            let digits = self.eat_digits(base);
            // An invalid digit is any char after the prefix, or a decimal digit
            // after valid ones (`0b102`); letters there are suffixes or trailing input.
            let invalid_digit = if digits == 0 {
                self.peek_char().is_some_and(is_ident_continue_char)
            } else {
                self.peek_byte().is_some_and(|b| b.is_ascii_digit())
            };
            if invalid_digit {
                let at = u8::try_from(self.cursor - start).unwrap_or(u8::MAX);
                self.eat_while(is_ident_continue_char);
                return TokenKind::Error(LexError::InvalidDigit { base, at });
            }
            if digits == 0 {
                return TokenKind::Error(LexError::MissingDigits { base });
            }
            self.eat_integer_suffix();
            return TokenKind::Integer;
        }

        // Upstream RON accepts `1.`, `.5` and `1.e3`, so the integer and
        // fraction digits are both optional (`is_number_ahead` ensured one exists).
        self.eat_digits(10);
        let mut is_float = false;
        if self.peek_byte() == Some(b'.') {
            self.advance(1);
            if self.peek_byte() == Some(b'_') {
                // Rejected like upstream RON (`1._5`); `_` elsewhere is a separator.
                self.eat_digits(10);
                return TokenKind::Error(LexError::FloatUnderscore);
            }
            self.eat_digits(10);
            is_float = true;
        }
        if matches!(self.peek_byte(), Some(b'e' | b'E')) {
            self.advance(1);
            if matches!(self.peek_byte(), Some(b'+' | b'-')) {
                self.advance(1);
            }
            if self.eat_digits(10) == 0 {
                return TokenKind::Error(LexError::MissingExponent);
            }
            is_float = true;
        }
        if self.eat_suffix("f32") || self.eat_suffix("f64") || is_float {
            return TokenKind::Float;
        }
        self.eat_integer_suffix();
        TokenKind::Integer
    }

    fn eat_integer_suffix(&mut self) {
        const SUFFIXES: [&str; 10] = [
            "i8", "i16", "i32", "i64", "i128", "u8", "u16", "u32", "u64", "u128",
        ];
        for suffix in SUFFIXES {
            if self.eat_suffix(suffix) {
                return;
            }
        }
    }

    /// Lex an identifier, or the special floats `inf` and `NaN`.
    fn identifier(&mut self) -> TokenKind {
        if let Some(len) = self.special_float_len(0) {
            self.advance(len);
            return TokenKind::Float;
        }
        self.next_char();
        self.eat_while(is_ident_continue_char);
        TokenKind::Ident
    }

    /// Lex `r#ident` (the cursor is on `r`, followed by `#`).
    fn raw_identifier(&mut self) -> TokenKind {
        self.advance(2);
        if self.peek_char().is_some_and(is_ident_raw_char) {
            self.eat_while(is_ident_raw_char);
            return TokenKind::Ident;
        }
        // `r#` alone or `r##foo`: consume the whole malformed literal.
        self.eat_while(|c| c == '#');
        self.eat_while(is_ident_raw_char);
        TokenKind::Error(LexError::InvalidRawLiteral)
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    /// Yields all tokens up to and including the first `Eof`.
    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }
        Some(self.next_token())
    }
}

#[cfg(test)]
mod tests {
    use alloc::{string::String, vec::Vec};

    use super::*;
    use crate::error::Position;

    /// Non-trivia token kinds, including the final `Eof`.
    fn tokenize(source: &str) -> Vec<TokenKind> {
        Lexer::new(source)
            .map(|t| t.kind)
            .filter(|k| !k.is_trivia())
            .collect()
    }

    fn tokenize_with_trivia(source: &str) -> Vec<TokenKind> {
        Lexer::new(source).map(|t| t.kind).collect()
    }

    /// Kind and text of every non-trivia token before `Eof`.
    fn lex_texts(source: &str) -> Vec<(TokenKind, &str)> {
        Lexer::new(source)
            .filter(|t| !t.kind.is_trivia() && t.kind != TokenKind::Eof)
            .map(|t| (t.kind, t.text))
            .collect()
    }

    /// Asserts that `source` lexes as exactly one token and returns its kind.
    fn single(source: &str) -> TokenKind {
        let tokens = lex_texts(source);
        assert_eq!(tokens.len(), 1, "{source:?} lexed as {tokens:?}");
        assert_eq!(tokens[0].1, source, "{source:?} lexed as {tokens:?}");
        tokens[0].0
    }

    use TokenKind::{ByteString, Eof, Float, Ident, Integer};

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
                Eof,
            ]
        );
    }

    #[test]
    fn test_comments() {
        assert_eq!(
            tokenize_with_trivia("// comment\n42"),
            alloc::vec![TokenKind::LineComment, TokenKind::Whitespace, Integer, Eof]
        );
        assert_eq!(
            tokenize_with_trivia("/* comment */42"),
            alloc::vec![TokenKind::BlockComment, Integer, Eof]
        );
        assert_eq!(
            tokenize_with_trivia("/* outer /* inner */ */42"),
            alloc::vec![TokenKind::BlockComment, Integer, Eof]
        );
    }

    #[test]
    fn unclosed_block_comment_is_error() {
        assert_eq!(
            single("/* a /* b */"),
            TokenKind::Error(LexError::UnclosedBlockComment)
        );
    }

    #[test]
    fn many_comments_do_not_overflow_the_stack() {
        let source = "/**/".repeat(2_000_000);
        assert_eq!(Lexer::new(&source).count(), 2_000_001);
    }

    #[test]
    fn test_strings() {
        assert_eq!(single("\"hello\""), TokenKind::String);
        assert_eq!(single("\"hello\\nworld\""), TokenKind::String);
        assert_eq!(single("\"say \\\"hi\\\"\""), TokenKind::String);
        assert_eq!(single("\"\\\\\""), TokenKind::String);
        assert_eq!(single("r\"hello\""), TokenKind::String);
        assert_eq!(single("r#\"hello\"#"), TokenKind::String);
        assert_eq!(single("r##\"a \"# b\"##"), TokenKind::String);
        assert_eq!(single("b\"hello\""), ByteString);
        assert_eq!(single("br\"hello\""), ByteString);
        assert_eq!(single("br#\"hello\"#"), ByteString);
    }

    #[test]
    fn raw_strings_allow_up_to_255_hashes() {
        let hashes = "#".repeat(255);
        let source = alloc::format!("r{hashes}\"x\"{hashes}");
        assert_eq!(single(&source), TokenKind::String);
        let source = alloc::format!("br{hashes}\"x\"{hashes}");
        assert_eq!(single(&source), ByteString);

        let hashes = "#".repeat(256);
        let source = alloc::format!("r{hashes}\"x\"{hashes}");
        assert_eq!(single(&source), TokenKind::Error(LexError::TooManyHashes));
    }

    #[test]
    fn unterminated_strings_are_errors() {
        let unterminated = TokenKind::Error(LexError::UnterminatedString);
        assert_eq!(single("\"abc"), unterminated);
        assert_eq!(single("\"abc\\"), unterminated);
        assert_eq!(single("r\"abc"), unterminated);
        assert_eq!(single("r#\"abc\""), unterminated);
        assert_eq!(single("b\"abc"), unterminated);
        assert_eq!(single("br\"abc"), unterminated);
    }

    #[test]
    fn test_chars() {
        assert_eq!(single("'a'"), TokenKind::Char);
        assert_eq!(single("'\\n'"), TokenKind::Char);
        assert_eq!(single("'\\''"), TokenKind::Char);
        assert_eq!(single("'\\x41'"), TokenKind::Char);
        assert_eq!(single("'\\u{1F600}'"), TokenKind::Char);
        assert_eq!(single("'é'"), TokenKind::Char);
        assert_eq!(single("''"), TokenKind::Error(LexError::EmptyChar));
        assert_eq!(single("'a"), TokenKind::Error(LexError::UnterminatedChar));
        assert_eq!(single("'"), TokenKind::Error(LexError::UnterminatedChar));
    }

    #[test]
    fn byte_literals_lex_as_byte_tokens() {
        assert_eq!(single("b'a'"), TokenKind::Byte);
        assert_eq!(single("b'\\x7f'"), TokenKind::Byte);
        assert_eq!(single("b'\\''"), TokenKind::Byte);
        assert_eq!(single("b'a"), TokenKind::Error(LexError::UnterminatedChar));
        // Identifiers starting with `b` or `r` are unaffected.
        assert_eq!(single("bar"), Ident);
        assert_eq!(single("break"), Ident);
        assert_eq!(single("raw"), Ident);
    }

    #[test]
    fn test_integers() {
        for source in [
            "42", "0x2A", "0o52", "0b101010", "42i32", "-42", "+42", "1_000", "0xFFu8",
        ] {
            assert_eq!(single(source), Integer, "{source}");
        }
    }

    #[test]
    fn test_floats() {
        for source in [
            "3.14", "3.14f32", "3e10", "3.14e-10", "1f64", "inf", "NaN", "-inf", "+inf", "-NaN",
            "inff32", "NaNf64", "-inff32",
        ] {
            assert_eq!(single(source), Float, "{source}");
        }
    }

    #[test]
    fn floats_without_leading_or_trailing_digits() {
        // Matches upstream `ron` 0.12, which accepts all of these.
        for source in ["1.", ".5", "1.e3", "-.5", "+.5", "1.f32", "0."] {
            assert_eq!(single(source), Float, "{source}");
        }
        assert_eq!(
            lex_texts("[1.,2.]"),
            alloc::vec![
                (TokenKind::LBracket, "["),
                (Float, "1."),
                (TokenKind::Comma, ","),
                (Float, "2."),
                (TokenKind::RBracket, "]"),
            ]
        );
    }

    #[test]
    fn exponent_requires_digits() {
        assert_eq!(single("1e"), TokenKind::Error(LexError::MissingExponent));
        assert_eq!(single("1.5e+"), TokenKind::Error(LexError::MissingExponent));
    }

    #[test]
    fn special_float_words_need_word_boundary() {
        assert_eq!(single("infinity"), Ident);
        assert_eq!(single("inf_"), Ident);
        assert_eq!(single("NaNa"), Ident);
        // `-infinity` used to lex as Float `-inf` followed by Ident `inity`.
        assert_eq!(
            lex_texts("-infinity"),
            alloc::vec![
                (TokenKind::Error(LexError::UnexpectedChar), "-"),
                (Ident, "infinity"),
            ]
        );
    }

    #[test]
    fn base_prefix_errors() {
        assert_eq!(
            single("0x"),
            TokenKind::Error(LexError::MissingDigits { base: 16 })
        );
        assert_eq!(
            single("0b"),
            TokenKind::Error(LexError::MissingDigits { base: 2 })
        );
        assert_eq!(
            single("0xg"),
            TokenKind::Error(LexError::InvalidDigit { base: 16, at: 2 })
        );
        assert_eq!(
            single("-0xg"),
            TokenKind::Error(LexError::InvalidDigit { base: 16, at: 3 })
        );
        assert_eq!(
            single("0b2"),
            TokenKind::Error(LexError::InvalidDigit { base: 2, at: 2 })
        );
    }

    #[test]
    fn test_identifiers() {
        for source in [
            "foo",
            "_bar",
            "foo123",
            "r#foo",
            "r#foo.bar",
            "r#a-b+c",
            "rust",
        ] {
            assert_eq!(single(source), Ident, "{source}");
        }
    }

    #[test]
    fn raw_identifiers_for_literal_words() {
        for source in [
            "r#true", "r#None", "r#Some", "r#inf", "r#NaN", "r#inff32", "r#a.b-c",
        ] {
            assert_eq!(single(source), Ident, "{source}");
        }
    }

    #[test]
    fn number_digit_and_underscore_rules() {
        assert_eq!(single("1._5"), TokenKind::Error(LexError::FloatUnderscore));
        assert_eq!(single("1._"), TokenKind::Error(LexError::FloatUnderscore));
        for source in ["1_.5", "1.5_", "1_e3", "1e_3"] {
            assert_eq!(single(source), Float, "{source}");
        }
        assert_eq!(
            single("0b102"),
            TokenKind::Error(LexError::InvalidDigit { base: 2, at: 4 })
        );
        assert_eq!(single("0b1u8"), Integer);
        assert_eq!(single("0xFFu8"), Integer);
        assert_eq!(
            lex_texts("0x1g"),
            alloc::vec![(Integer, "0x1"), (Ident, "g")]
        );
    }

    #[test]
    fn quote_char_literal() {
        assert_eq!(single("'''"), TokenKind::Char);
        assert_eq!(single("b'''"), TokenKind::Byte);
        assert_eq!(single("''"), TokenKind::Error(LexError::EmptyChar));
    }

    #[test]
    fn malformed_raw_literals_are_errors() {
        let invalid = TokenKind::Error(LexError::InvalidRawLiteral);
        assert_eq!(single("r#"), invalid);
        assert_eq!(single("r##foo"), invalid);
        assert_eq!(
            lex_texts("r# x"),
            alloc::vec![(invalid, "r#"), (Ident, "x")]
        );
    }

    #[test]
    fn unexpected_char_is_error() {
        assert_eq!(single("@"), TokenKind::Error(LexError::UnexpectedChar));
        assert_eq!(single("é"), Ident);
        assert_eq!(single("§"), TokenKind::Error(LexError::UnexpectedChar));
    }

    #[test]
    fn test_whitespace() {
        assert_eq!(tokenize("\t\n42"), alloc::vec![Integer, Eof]);
        assert_eq!(
            tokenize_with_trivia("  42  "),
            alloc::vec![TokenKind::Whitespace, Integer, TokenKind::Whitespace, Eof]
        );
    }

    #[test]
    fn test_complete_ron() {
        assert_eq!(
            tokenize("Config(name: \"test\", value: 42)"),
            alloc::vec![
                Ident,
                TokenKind::LParen,
                Ident,
                TokenKind::Colon,
                TokenKind::String,
                TokenKind::Comma,
                Ident,
                TokenKind::Colon,
                Integer,
                TokenKind::RParen,
                Eof,
            ]
        );
    }

    #[test]
    fn test_token_spans() {
        let pos = |line, col| Position { line, col };
        let spans: Vec<_> = Lexer::new("foo 123\nbé x")
            .filter(|t| !t.kind.is_trivia() && t.kind != Eof)
            .map(|t| (t.text, t.span.start, t.span.end))
            .collect();
        assert_eq!(
            spans,
            alloc::vec![
                ("foo", pos(1, 1), pos(1, 4)),
                ("123", pos(1, 5), pos(1, 8)),
                // Columns are byte-based.
                ("bé", pos(2, 1), pos(2, 4)),
                ("x", pos(2, 5), pos(2, 6)),
            ]
        );
    }

    #[test]
    fn test_eof_token() {
        let mut lexer = Lexer::new("");
        assert_eq!(lexer.next().map(|t| t.kind), Some(Eof));
        assert!(lexer.next().is_none());
        // `next_token` keeps returning `Eof`.
        assert_eq!(lexer.next_token().kind, Eof);
    }

    #[test]
    fn tokens_cover_source_exactly() {
        let source =
            "// c\n#![type = \"x\"]\n(a: [1., .5, -inf], b: r#\"q\"#, c: b'x', d: '\\'', e: @ /* x";
        let text: String = Lexer::new(source).map(|t| t.text).collect();
        assert_eq!(text, source);
    }
}
