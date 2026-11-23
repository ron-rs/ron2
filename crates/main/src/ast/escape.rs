//! RON source text for literal values: escaping of chars, strings and bytes,
//! and the canonical spelling of numbers.

use alloc::string::String;
use core::fmt::{Debug, Write};

use crate::{ast::NumberKind, value::Number};

/// Append `c` as a quoted RON char literal.
pub(crate) fn push_char(out: &mut String, c: char) {
    out.push('\'');
    match c {
        '\'' => out.push_str("\\'"),
        _ => push_escaped(out, c),
    }
    out.push('\'');
}

/// Append `s` as a quoted RON string literal.
pub(crate) fn push_string(out: &mut String, s: &str) {
    out.reserve(s.len() + 2);
    out.push('"');
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            _ => push_escaped(out, c),
        }
    }
    out.push('"');
}

/// Append `bytes` as a quoted RON byte string literal.
pub(crate) fn push_bytes(out: &mut String, bytes: &[u8]) {
    out.reserve(bytes.len() + 3);
    out.push_str("b\"");
    for &b in bytes {
        match b {
            b'"' => out.push_str("\\\""),
            b if b.is_ascii_graphic() || b == b' ' => push_escaped(out, char::from(b)),
            b'\n' | b'\r' | b'\t' | b'\0' => push_escaped(out, char::from(b)),
            b => {
                let _ = write!(out, "\\x{b:02x}");
            }
        }
    }
    out.push('"');
}

/// Escapes shared by all quoted literals; quotes are handled by the caller.
fn push_escaped(out: &mut String, c: char) {
    match c {
        '\\' => out.push_str("\\\\"),
        '\n' => out.push_str("\\n"),
        '\r' => out.push_str("\\r"),
        '\t' => out.push_str("\\t"),
        '\0' => out.push_str("\\0"),
        c if c.is_ascii_control() => {
            let _ = write!(out, "\\x{:02x}", u32::from(c));
        }
        c => out.push(c),
    }
}

/// Append the canonical RON spelling of `n` and return its literal kind.
///
/// Reading the text back gives a number equal to `n`: an `f32` whose
/// shortest spelling would read back as a different `f64` gets an `f32`
/// suffix (`0.1f32`), so `F32(0.1)` and `F64(0.1)` stay distinct.
pub(crate) fn push_number(out: &mut String, n: Number) -> NumberKind {
    match n {
        Number::I8(v) => push_int(out, v),
        Number::I16(v) => push_int(out, v),
        Number::I32(v) => push_int(out, v),
        Number::I64(v) => push_int(out, v),
        #[cfg(feature = "integer128")]
        Number::I128(v) => push_int(out, v),
        Number::U8(v) => push_int(out, v),
        Number::U16(v) => push_int(out, v),
        Number::U32(v) => push_int(out, v),
        Number::U64(v) => push_int(out, v),
        #[cfg(feature = "integer128")]
        Number::U128(v) => push_int(out, v),
        Number::F32(v) => {
            let start = out.len();
            let kind = push_f32(out, v.0);
            if v.0.is_finite() && out[start..].parse::<f64>().ok() != Some(f64::from(v.0)) {
                out.push_str("f32");
            }
            kind
        }
        Number::F64(v) => push_f64(out, v.0),
        #[cfg(not(doc))]
        Number::__NonExhaustive(never) => never.never(),
    }
}

pub(crate) fn push_int<T: itoa::Integer + PartialOrd + Default>(
    out: &mut String,
    v: T,
) -> NumberKind {
    out.push_str(itoa::Buffer::new().format(v));
    if v < T::default() {
        NumberKind::NegativeInteger
    } else {
        NumberKind::Integer
    }
}

/// Append the shortest spelling that reads back as exactly `v` at `f32` precision.
pub(crate) fn push_f32(out: &mut String, v: f32) -> NumberKind {
    push_float(out, v, v.is_nan(), v.is_sign_negative())
}

/// Append the shortest spelling that reads back as exactly `v`.
pub(crate) fn push_f64(out: &mut String, v: f64) -> NumberKind {
    push_float(out, v, v.is_nan(), v.is_sign_negative())
}

/// `Debug` is the shortest round-trip spelling; it switches to an exponent
/// for very large or small magnitudes and always has a `.` or exponent
/// for finite values, so the text reads back as a float.
fn push_float(out: &mut String, v: impl Debug, nan: bool, negative: bool) -> NumberKind {
    if nan {
        // Keep the sign: `-NaN` and `NaN` are different values under `total_cmp`.
        out.push_str(if negative { "-NaN" } else { "NaN" });
    } else {
        let _ = write!(out, "{v:?}");
    }
    NumberKind::Float
}

#[cfg(test)]
mod tests {
    use alloc::{vec, vec::Vec};

    use super::*;
    use crate::{FromRon, Value};

    fn with(f: impl FnOnce(&mut String)) -> String {
        let mut out = String::new();
        f(&mut out);
        out
    }

    /// Escaped literals parse back to the original value.
    #[test]
    fn escaped_literals_round_trip() {
        let mut chars: Vec<char> = (0u8..=0x7f).map(char::from).collect();
        chars.extend(['é', '😀', '\u{80}', '\u{2028}']);
        let string: String = chars.iter().collect();
        let bytes: Vec<u8> = (0u8..=255).collect();

        for &c in &chars {
            let text = with(|o| push_char(o, c));
            assert_eq!(Value::from_ron(&text).unwrap(), Value::Char(c), "{text}");
        }
        let text = with(|o| push_string(o, &string));
        assert_eq!(Value::from_ron(&text).unwrap(), Value::String(string));
        let text = with(|o| push_bytes(o, &bytes));
        assert_eq!(Value::from_ron(&text).unwrap(), Value::Bytes(bytes));
    }

    #[test]
    fn escapes() {
        assert_eq!(with(|o| push_char(o, '\'')), r"'\''");
        assert_eq!(with(|o| push_char(o, '"')), "'\"'");
        assert_eq!(
            with(|o| push_string(o, "a\"b\\\n\u{1}")),
            r#""a\"b\\\n\x01""#
        );
        assert_eq!(with(|o| push_bytes(o, b"a\xff'")), r#"b"a\xff'""#);
    }

    #[test]
    fn numbers() {
        let cases = vec![
            (Number::from(-5i8), "-5", NumberKind::NegativeInteger),
            (
                Number::from(u64::MAX),
                "18446744073709551615",
                NumberKind::Integer,
            ),
            (Number::from(1.0f64), "1.0", NumberKind::Float),
            (Number::from(-0.0f64), "-0.0", NumberKind::Float),
            (Number::from(0.1f32), "0.1f32", NumberKind::Float),
            (Number::from(0.5f32), "0.5", NumberKind::Float),
            (Number::from(f32::INFINITY), "inf", NumberKind::Float),
            (Number::from(1.5e10f64), "15000000000.0", NumberKind::Float),
            (Number::from(1e308f64), "1e308", NumberKind::Float),
            (Number::from(1.5e-300f64), "1.5e-300", NumberKind::Float),
            (Number::from(1e-45f32), "1e-45f32", NumberKind::Float),
            (Number::from(f32::NAN), "NaN", NumberKind::Float),
            (Number::from(-f64::NAN), "-NaN", NumberKind::Float),
            (Number::from(f64::NEG_INFINITY), "-inf", NumberKind::Float),
        ];
        for (n, text, kind) in cases {
            let mut out = String::new();
            assert_eq!(push_number(&mut out, n), kind);
            assert_eq!(out, text);
        }
    }
}
