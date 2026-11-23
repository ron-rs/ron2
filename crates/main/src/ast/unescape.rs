//! Decoding of string, byte string, character and byte literals.
//!
//! One escape decoder serves all four literal kinds. The accepted escapes
//! follow upstream `ron` 0.12 (and Rust where `ron` does not differ):
//!
//! - `\n`, `\r`, `\t`, `\\`, `\0`, `\'` and `\"` everywhere.
//! - `\xNN` takes exactly two hex digits. In strings it is a *byte* escape: a
//!   byte `>= 0x80` must be followed by further `\x` escapes that together form
//!   one UTF-8 character, so `"\xC3\xA9"` is `"é"` and `"\xE9"` is an error.
//!   Character literals only accept ASCII `\x` escapes; byte literals and byte
//!   strings accept any byte.
//! - `\u{N}` takes 1 to 6 hex digits. It is not allowed in byte literals
//!   (`b'a'`); in byte strings it stands for the UTF-8 encoding of the char.
//! - Byte strings, raw or not, may contain non-ASCII text, which stands for
//!   its UTF-8 bytes (`b"é"` is `[0xC3, 0xA9]`). Rust rejects both of these;
//!   upstream `ron` accepts them, and RON files written for it must keep working.
//! - Byte literals must be ASCII.

use alloc::{borrow::ToOwned, string::String, vec::Vec};
use core::ops::Range;

use super::{BytesKind, StringKind};
use crate::error::{Error, ErrorKind};

/// An invalid literal, with the byte range of the problem within the literal's raw text.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct LiteralError {
    pub kind: ErrorKind,
    pub range: Range<usize>,
}

impl LiteralError {
    fn invalid(message: &'static str, range: Range<usize>) -> Self {
        Self {
            kind: ErrorKind::InvalidEscape(message.into()),
            range,
        }
    }

    fn shifted(mut self, offset: usize) -> Self {
        self.range = self.range.start + offset..self.range.end + offset;
        self
    }
}

/// Decode a string literal (`"..."` or `r#"..."#`), e.g. as found in a RON source.
///
/// Returns an error for invalid escapes and for text that is not a complete
/// string literal.
pub fn decode_string(raw: &str) -> Result<(String, StringKind), Error> {
    decode_str(raw).map_err(|e| Error::new(e.kind))
}

/// Decode the raw text of a string token.
pub(crate) fn decode_str(raw: &str) -> Result<(String, StringKind), LiteralError> {
    if let Some(rest) = raw.strip_prefix('r') {
        let (content, hash_count) = raw_content(raw, rest)?;
        return Ok((content.to_owned(), StringKind::Raw { hash_count }));
    }
    let content = delimited(raw, "\"", '"')?;
    if !content.contains('\\') {
        return Ok((content.to_owned(), StringKind::Regular));
    }
    let mut value = String::with_capacity(content.len());
    decode(content, 1, Literal::Str, |piece| match piece {
        Piece::Text(text) => value.push_str(text),
        Piece::Char(c) => value.push(c),
        Piece::Byte(b) => value.push(char::from(b)),
    })?;
    Ok((value, StringKind::Regular))
}

/// Decode the raw text of a byte string token (`b"..."` or `br#"..."#`).
pub(crate) fn decode_bytes(raw: &str) -> Result<(Vec<u8>, BytesKind), LiteralError> {
    if let Some(rest) = raw.strip_prefix("br") {
        let (content, hash_count) = raw_content(raw, rest)?;
        return Ok((content.as_bytes().to_vec(), BytesKind::Raw { hash_count }));
    }
    let content = delimited(raw, "b\"", '"')?;
    let mut value = Vec::with_capacity(content.len());
    decode(content, 2, Literal::Bytes, |piece| match piece {
        Piece::Text(text) => value.extend_from_slice(text.as_bytes()),
        Piece::Char(c) => value.extend_from_slice(c.encode_utf8(&mut [0; 4]).as_bytes()),
        Piece::Byte(b) => value.push(b),
    })?;
    Ok((value, BytesKind::Regular))
}

/// Decode the raw text of a character token (`'a'`).
pub(crate) fn decode_char(raw: &str) -> Result<char, LiteralError> {
    let content = delimited(raw, "'", '\'')?;
    let mut one = One::default();
    decode(content, 1, Literal::Char, |piece| match piece {
        Piece::Text(text) => text.chars().for_each(|c| one.push(c)),
        Piece::Char(c) => one.push(c),
        Piece::Byte(b) => one.push(char::from(b)),
    })?;
    one.get(raw, "exactly one character", "character literal")
}

/// Decode the raw text of a byte token (`b'a'`).
pub(crate) fn decode_byte(raw: &str) -> Result<u8, LiteralError> {
    let content = delimited(raw, "b'", '\'')?;
    let mut one = One::default();
    decode(content, 2, Literal::Byte, |piece| match piece {
        Piece::Text(text) => text.bytes().for_each(|b| one.push(b)),
        Piece::Char(c) => c.encode_utf8(&mut [0; 4]).bytes().for_each(|b| one.push(b)),
        Piece::Byte(b) => one.push(b),
    })?;
    one.get(raw, "exactly one byte", "byte literal")
}

/// Which literal is decoded; decides which escapes are allowed.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Literal {
    Str,
    Char,
    Bytes,
    Byte,
}

impl Literal {
    fn is_bytes(self) -> bool {
        matches!(self, Self::Bytes | Self::Byte)
    }
}

/// A decoded part of a literal's content.
enum Piece<'s> {
    /// Text without escapes (ASCII-only for byte literals).
    Text(&'s str),
    Char(char),
    /// A byte; always ASCII for string and character literals.
    Byte(u8),
}

/// Collects the single value of a character or byte literal.
struct One<T> {
    value: Option<T>,
    count: usize,
}

impl<T> Default for One<T> {
    fn default() -> Self {
        Self {
            value: None,
            count: 0,
        }
    }
}

impl<T: Copy> One<T> {
    fn push(&mut self, value: T) {
        self.value.get_or_insert(value);
        self.count += 1;
    }

    fn get(
        self,
        raw: &str,
        expected: &'static str,
        context: &'static str,
    ) -> Result<T, LiteralError> {
        match self.value {
            Some(value) if self.count == 1 => Ok(value),
            _ => Err(LiteralError {
                kind: ErrorKind::Expected {
                    expected: expected.into(),
                    context: Some(context),
                },
                range: 0..raw.len(),
            }),
        }
    }
}

fn unterminated(raw: &str) -> LiteralError {
    LiteralError {
        kind: ErrorKind::ExpectedStringEnd,
        range: 0..raw.len(),
    }
}

/// The content between `open` and a final `close`.
fn delimited<'s>(raw: &'s str, open: &str, close: char) -> Result<&'s str, LiteralError> {
    raw.strip_prefix(open)
        .and_then(|rest| rest.strip_suffix(close))
        .ok_or_else(|| unterminated(raw))
}

/// Content and hash count of a raw literal; `rest` is `raw` after the `r`/`br` prefix.
fn raw_content<'s>(raw: &str, rest: &'s str) -> Result<(&'s str, u8), LiteralError> {
    let hashes = rest.bytes().take_while(|&b| b == b'#').count();
    let (hash_str, after_hashes) = rest.split_at(hashes);
    after_hashes
        .strip_prefix('"')
        .and_then(|inner| inner.strip_suffix(hash_str))
        .and_then(|inner| inner.strip_suffix('"'))
        .zip(u8::try_from(hashes).ok())
        .ok_or_else(|| unterminated(raw))
}

/// Error unless `text` (found at `offset` in the raw literal) is ASCII.
fn check_ascii(text: &str, offset: usize) -> Result<(), LiteralError> {
    match text.char_indices().find(|(_, c)| !c.is_ascii()) {
        None => Ok(()),
        Some((i, c)) => Err(LiteralError::invalid(
            "non-ASCII character in byte literal",
            offset + i..offset + i + c.len_utf8(),
        )),
    }
}

/// Decode `content` (found at `offset` in the raw literal), passing each piece to `out`.
fn decode(
    content: &str,
    offset: usize,
    literal: Literal,
    mut out: impl FnMut(Piece<'_>),
) -> Result<(), LiteralError> {
    let mut text_start = 0;
    while let Some(i) = content[text_start..].find('\\') {
        let at = text_start + i;
        text(
            &content[text_start..at],
            offset + text_start,
            literal,
            &mut out,
        )?;
        let (piece, end) = escape(content, at, literal).map_err(|e| e.shifted(offset))?;
        out(piece);
        text_start = end;
    }
    text(
        &content[text_start..],
        offset + text_start,
        literal,
        &mut out,
    )
}

fn text(
    text: &str,
    offset: usize,
    literal: Literal,
    out: &mut impl FnMut(Piece<'_>),
) -> Result<(), LiteralError> {
    if literal == Literal::Byte {
        check_ascii(text, offset)?;
    }
    if !text.is_empty() {
        out(Piece::Text(text));
    }
    Ok(())
}

fn hex_value(b: u8) -> Option<u8> {
    match b {
        b'0'..=b'9' => Some(b - b'0'),
        b'a'..=b'f' => Some(b - b'a' + 10),
        b'A'..=b'F' => Some(b - b'A' + 10),
        _ => None,
    }
}

/// Decode the escape starting with the `\` at `s[at]`, returning the piece and
/// the offset just after the escape.
fn escape(s: &str, at: usize, literal: Literal) -> Result<(Piece<'static>, usize), LiteralError> {
    let bytes = s.as_bytes();
    let Some(&kind) = bytes.get(at + 1) else {
        return Err(LiteralError::invalid(
            "incomplete escape sequence",
            at..at + 1,
        ));
    };
    let simple = match kind {
        b'n' => b'\n',
        b'r' => b'\r',
        b't' => b'\t',
        b'\\' => b'\\',
        b'0' => b'\0',
        b'\'' => b'\'',
        b'"' => b'"',
        b'x' => return hex_escape(bytes, at, literal),
        b'u' if literal == Literal::Byte => {
            return Err(LiteralError::invalid(
                "unicode escape in byte literal",
                at..at + 2,
            ));
        }
        b'u' => return unicode_escape(bytes, at),
        _ => {
            let len = s[at + 1..].chars().next().map_or(1, char::len_utf8);
            return Err(LiteralError::invalid(
                "unknown escape sequence",
                at..at + 1 + len,
            ));
        }
    };
    Ok((Piece::Byte(simple), at + 2))
}

/// The byte of the `\xNN` escape starting at `bytes[at]`.
fn hex_byte(bytes: &[u8], at: usize) -> Result<u8, LiteralError> {
    match bytes.get(at + 2..at + 4) {
        Some(&[hi, lo]) => hex_value(hi).zip(hex_value(lo)),
        _ => None,
    }
    .map(|(hi, lo)| (hi << 4) | lo)
    .ok_or_else(|| LiteralError::invalid("expected two hex digits after `\\x`", at..at + 2))
}

/// Decode `\xNN`, plus the continuation escapes of a multi-byte character in strings.
fn hex_escape(
    bytes: &[u8],
    at: usize,
    literal: Literal,
) -> Result<(Piece<'static>, usize), LiteralError> {
    let first = hex_byte(bytes, at)?;
    let mut end = at + 4;
    if literal.is_bytes() || first.is_ascii() {
        return Ok((Piece::Byte(first), end));
    }
    if literal == Literal::Str {
        // A non-ASCII byte must start a UTF-8 sequence completed by more `\x` escapes.
        let mut buf = [first, 0, 0, 0];
        for i in 1..buf.len() {
            if bytes.get(end..end + 2) != Some(b"\\x") {
                break;
            }
            buf[i] = hex_byte(bytes, end)?;
            end += 4;
            if let Some(c) = core::str::from_utf8(&buf[..=i])
                .ok()
                .and_then(|s| s.chars().next())
            {
                return Ok((Piece::Char(c), end));
            }
        }
    }
    Err(LiteralError::invalid(
        "byte escapes do not form a valid UTF-8 character",
        at..end,
    ))
}

/// Decode `\u{N}` with 1 to 6 hex digits.
fn unicode_escape(bytes: &[u8], at: usize) -> Result<(Piece<'static>, usize), LiteralError> {
    if bytes.get(at + 2) != Some(&b'{') {
        return Err(LiteralError::invalid(
            "expected `{` after `\\u`",
            at..at + 2,
        ));
    }
    let digits_start = at + 3;
    let mut pos = digits_start;
    let mut value = 0u32;
    while pos - digits_start < 6 {
        let Some(digit) = bytes.get(pos).copied().and_then(hex_value) else {
            break;
        };
        value = (value << 4) | u32::from(digit);
        pos += 1;
    }
    if pos == digits_start {
        return Err(LiteralError::invalid(
            "expected 1 to 6 hex digits in unicode escape",
            at..pos,
        ));
    }
    if bytes.get(pos) != Some(&b'}') {
        return Err(LiteralError::invalid(
            "expected `}` to close unicode escape",
            at..pos,
        ));
    }
    let end = pos + 1;
    char::from_u32(value)
        .map(|c| (Piece::Char(c), end))
        .ok_or_else(|| LiteralError::invalid("invalid unicode code point", at..end))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn string(raw: &str) -> Result<String, LiteralError> {
        decode_str(raw).map(|(s, _)| s)
    }

    fn invalid(message: &'static str, range: Range<usize>) -> LiteralError {
        LiteralError::invalid(message, range)
    }

    #[test]
    fn simple_escapes() {
        assert_eq!(string(r#""a\n\r\t\\\0\'\"b""#).unwrap(), "a\n\r\t\\\0'\"b");
        assert_eq!(
            decode_bytes(r#"b"a\n\r\t\\\0\'\"b""#).unwrap().0,
            b"a\n\r\t\\\0'\"b"
        );
        assert_eq!(decode_char(r"'\n'").unwrap(), '\n');
        assert_eq!(decode_byte(r"b'\''").unwrap(), b'\'');
    }

    #[test]
    fn strings_without_escapes() {
        assert_eq!(string(r#""héllo""#).unwrap(), "héllo");
        assert_eq!(string("\"\"").unwrap(), "");
        assert_eq!(
            decode_str(r###"r##"a "# b"##"###).unwrap(),
            ("a \"# b".into(), StringKind::Raw { hash_count: 2 })
        );
    }

    #[test]
    fn hex_escapes_need_exactly_two_hex_digits() {
        assert_eq!(string(r#""\x41""#).unwrap(), "A");
        assert_eq!(string(r#""\x412""#).unwrap(), "A2");
        let err = invalid("expected two hex digits after `\\x`", 1..3);
        assert_eq!(string(r#""\x4""#), Err(err.clone()));
        assert_eq!(string(r#""\x+1""#), Err(err.clone()));
        assert_eq!(string(r#""\x""#), Err(err));
        assert!(decode_char(r"'\x4'").is_err());
        assert!(decode_byte(r"b'\x+f'").is_err());
        assert!(decode_bytes(r#"b"\x-1""#).is_err());
    }

    #[test]
    fn hex_escapes_in_strings_are_utf8_bytes() {
        // Matches upstream `ron` 0.12: consecutive `\x` escapes form one UTF-8 char.
        assert_eq!(string(r#""\xC3\xA9""#).unwrap(), "é");
        assert_eq!(string(r#""\xF0\x9F\x98\x80!""#).unwrap(), "😀!");
        let err = |end| invalid("byte escapes do not form a valid UTF-8 character", 1..end);
        assert_eq!(string(r#""\xE9""#), Err(err(5)));
        assert_eq!(string(r#""\xC3A""#), Err(err(5)));
        assert_eq!(string(r#""\xC3\x41""#), Err(err(9)));
        // Character literals take a single ASCII byte escape.
        assert_eq!(decode_char(r"'\x41'").unwrap(), 'A');
        assert!(decode_char(r"'\xE9'").is_err());
        assert!(decode_char(r"'\xC3\xA9'").is_err());
        // Byte literals take any byte.
        assert_eq!(decode_byte(r"b'\xff'").unwrap(), 0xff);
        assert_eq!(decode_bytes(r#"b"\xff\x00""#).unwrap().0, [0xff, 0]);
    }

    #[test]
    fn unicode_escapes() {
        assert_eq!(string(r#""\u{41}\u{1F600}""#).unwrap(), "A😀");
        assert_eq!(decode_char(r"'\u{e9}'").unwrap(), 'é');
        assert_eq!(
            string(r#""\u{41""#),
            Err(invalid("expected `}` to close unicode escape", 1..6))
        );
        assert_eq!(
            string(r#""\u{+41}""#),
            Err(invalid(
                "expected 1 to 6 hex digits in unicode escape",
                1..4
            ))
        );
        assert_eq!(
            string(r#""\u{}""#),
            Err(invalid(
                "expected 1 to 6 hex digits in unicode escape",
                1..4
            ))
        );
        assert_eq!(
            string(r#""\u{0000041}""#),
            Err(invalid("expected `}` to close unicode escape", 1..10))
        );
        assert_eq!(
            string(r#""\u41""#),
            Err(invalid("expected `{` after `\\u`", 1..3))
        );
        assert_eq!(
            string(r#""\u{D800}""#),
            Err(invalid("invalid unicode code point", 1..9))
        );
        // Like upstream `ron`, byte strings take `\u` as UTF-8 bytes; byte literals don't.
        assert_eq!(decode_bytes(r#"b"\u{41}\u{e9}""#).unwrap().0, b"A\xC3\xA9");
        assert!(decode_byte(r"b'\u{41}'").is_err());
    }

    #[test]
    fn unknown_escape_reports_its_range() {
        assert_eq!(
            string("\"aé\\q\""),
            Err(invalid("unknown escape sequence", 4..6))
        );
        assert_eq!(
            string("\"\\é\""),
            Err(invalid("unknown escape sequence", 1..4))
        );
    }

    #[test]
    fn byte_strings_take_utf8_and_byte_literals_must_be_ascii() {
        // Matches upstream `ron` 0.12.
        assert_eq!(decode_bytes("b\"aé\"").unwrap().0, "aé".as_bytes());
        assert_eq!(decode_bytes("br\"aé\"").unwrap().0, "aé".as_bytes());
        assert_eq!(decode_bytes("br#\"é\"#").unwrap().0, "é".as_bytes());
        assert_eq!(
            decode_byte("b'é'"),
            Err(invalid("non-ASCII character in byte literal", 2..4))
        );
        assert_eq!(
            decode_bytes(r##"br#"a"b"#"##).unwrap(),
            (b"a\"b".to_vec(), BytesKind::Raw { hash_count: 1 })
        );
    }

    #[test]
    fn char_literals_hold_exactly_one_char() {
        assert_eq!(decode_char("'é'").unwrap(), 'é');
        assert!(decode_char(r"'\nab'").is_err());
        assert!(decode_char("'ab'").is_err());
        assert!(decode_char("''").is_err());
        assert!(decode_byte("b'ab'").is_err());
    }

    #[test]
    fn malformed_literals_do_not_panic() {
        for raw in [
            "", "\"", "r", "r#", "r#\"", "r#\"a\"", "b", "b\"", "br#\"a\"", "'", "b'",
        ] {
            assert!(decode_str(raw).is_err(), "{raw:?}");
            assert!(decode_bytes(raw).is_err(), "{raw:?}");
            assert!(decode_char(raw).is_err(), "{raw:?}");
            assert!(decode_byte(raw).is_err(), "{raw:?}");
        }
        assert!(decode_string("\"\\").is_err());
    }
}
