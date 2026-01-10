//! String, byte string, and character unescape utilities.
//!
//! These functions handle escape sequence processing for RON literals.

use alloc::{string::String, vec::Vec};

use crate::error::{Error, Result};

use super::{BytesKind, StringKind};

/// Decode a string literal.
/// Returns `(value, kind)` on success, or `(Error, byte_offset_in_raw)` on failure.
pub fn decode_string(raw: &str) -> core::result::Result<(String, StringKind), (Error, usize)> {
    if raw.starts_with('r') {
        // Raw string - no escapes to process
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
        let value = unescape_string(content).map_err(|(e, offset)| (e, offset + 1))?; // +1 for opening quote
        Ok((value, StringKind::Regular))
    }
}

/// Process escape sequences in a string.
/// Returns the unescaped string, or `(Error, byte_offset)` on failure.
pub fn unescape_string(s: &str) -> core::result::Result<String, (Error, usize)> {
    let mut result = String::new();
    let mut byte_offset = 0usize;
    let mut chars = s.chars().peekable();

    while let Some(c) = chars.next() {
        let char_start = byte_offset;
        byte_offset += c.len_utf8();

        if c == '\\' {
            match chars.next() {
                Some('n') => {
                    result.push('\n');
                    byte_offset += 1;
                }
                Some('r') => {
                    result.push('\r');
                    byte_offset += 1;
                }
                Some('t') => {
                    result.push('\t');
                    byte_offset += 1;
                }
                Some('\\') => {
                    result.push('\\');
                    byte_offset += 1;
                }
                Some('0') => {
                    result.push('\0');
                    byte_offset += 1;
                }
                Some('"') => {
                    result.push('"');
                    byte_offset += 1;
                }
                Some('\'') => {
                    result.push('\'');
                    byte_offset += 1;
                }
                Some('x') => {
                    byte_offset += 1;
                    let hex: String = chars.by_ref().take(2).collect();
                    byte_offset += hex.len();
                    let val = u8::from_str_radix(&hex, 16)
                        .map_err(|_| (Error::InvalidEscape("invalid hex escape"), char_start))?;
                    result.push(val as char);
                }
                Some('u') => {
                    byte_offset += 1;
                    if chars.next() != Some('{') {
                        return Err((Error::InvalidEscape("expected { after \\u"), char_start));
                    }
                    byte_offset += 1;
                    let hex: String = chars.by_ref().take_while(|&c| c != '}').collect();
                    byte_offset += hex.len() + 1; // +1 for closing }
                    let val = u32::from_str_radix(&hex, 16).map_err(|_| {
                        (Error::InvalidEscape("invalid unicode escape"), char_start)
                    })?;
                    let c = char::from_u32(val).ok_or((
                        Error::InvalidEscape("invalid unicode codepoint"),
                        char_start,
                    ))?;
                    result.push(c);
                }
                Some(_) => {
                    return Err((Error::InvalidEscape("unknown escape sequence"), char_start));
                }
                None => {
                    return Err((Error::InvalidEscape("unexpected end of string"), char_start));
                }
            }
        } else {
            result.push(c);
        }
    }

    Ok(result)
}

/// Decode a byte string literal.
pub fn decode_byte_string(raw: &str) -> Result<(Vec<u8>, BytesKind)> {
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
        let value = unescape_bytes(content)?;
        Ok((value, BytesKind::Regular))
    }
}

/// Process escape sequences in a byte string.
pub fn unescape_bytes(s: &str) -> Result<Vec<u8>> {
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

/// Unescape a single character.
pub fn unescape_char(s: &str) -> Result<char> {
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
pub fn unescape_byte_char(s: &str) -> Result<u8> {
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
                u8::from_str_radix(&hex, 16).map_err(|_| Error::InvalidEscape("invalid hex escape"))
            }
            _ => Err(Error::InvalidEscape("unknown escape sequence")),
        }
    } else if c.is_ascii() {
        Ok(c as u8)
    } else {
        Err(Error::InvalidEscape("non-ASCII character in byte literal"))
    }
}
