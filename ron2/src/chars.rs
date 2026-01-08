//! Character classification functions for RON parsing.
//!
//! This module provides functions for classifying characters during RON
//! tokenization and parsing. All functions are `no_std` compatible.

use unicode_ident::{is_xid_continue, is_xid_start};

/// Returns `true` if the character is valid as the first character of an identifier.
///
/// An identifier can start with an underscore or any Unicode `XID_Start` character.
#[must_use]
pub fn is_ident_first_char(c: char) -> bool {
    c == '_' || is_xid_start(c)
}

/// Returns `true` if the character can continue an identifier.
///
/// Valid continuation characters are any Unicode `XID_Continue` character.
#[must_use]
pub fn is_ident_continue_char(c: char) -> bool {
    is_xid_continue(c)
}

/// Returns `true` if the character is valid in a raw identifier.
///
/// Raw identifiers (prefixed with `r#`) allow additional characters beyond
/// standard identifiers, including `.`, `+`, and `-`, as well as any
/// Unicode `XID_Continue` character.
#[must_use]
pub fn is_ident_raw_char(c: char) -> bool {
    matches!(c, '.' | '+' | '-') | is_xid_continue(c)
}

/// Returns `true` if the character is considered whitespace in RON.
///
/// This includes:
/// - Space (` `)
/// - Horizontal tab (`\t`)
/// - Line feed (`\n`)
/// - Carriage return (`\r`)
/// - Vertical tab (`\x0B`)
/// - Form feed (`\x0C`)
/// - Next line (`\u{85}`)
/// - Left-to-right mark (`\u{200E}`)
/// - Right-to-left mark (`\u{200F}`)
/// - Line separator (`\u{2028}`)
/// - Paragraph separator (`\u{2029}`)
#[must_use]
pub const fn is_whitespace_char(c: char) -> bool {
    matches!(
        c,
        ' ' | '\t'
            | '\n'
            | '\r'
            | '\x0B'
            | '\x0C'
            | '\u{85}'
            | '\u{200E}'
            | '\u{200F}'
            | '\u{2028}'
            | '\u{2029}'
    )
}

/// Returns `true` if the character is valid in an integer literal.
///
/// Valid characters are hexadecimal digits (0-9, a-f, A-F) and underscores
/// (used as visual separators).
#[must_use]
pub const fn is_int_char(c: char) -> bool {
    c.is_ascii_hexdigit() || c == '_'
}

/// Returns `true` if the character is valid in a floating-point literal.
///
/// Valid characters are:
/// - Decimal digits (0-9)
/// - Exponent markers (`e`, `E`)
/// - Decimal point (`.`)
/// - Sign characters (`+`, `-`)
/// - Underscore (`_`, used as visual separator)
#[must_use]
pub const fn is_float_char(c: char) -> bool {
    c.is_ascii_digit() || matches!(c, 'e' | 'E' | '.' | '+' | '-' | '_')
}
