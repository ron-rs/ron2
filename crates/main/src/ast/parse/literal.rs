//! Parsing for literal values (numbers, strings, byte strings, chars, bytes).

use alloc::{borrow::Cow, vec::Vec};

use super::AstParser;
use crate::{
    ast::{
        ByteExpr, BytesExpr, CharExpr, ErrorExpr, Expr, NumberExpr, NumberKind, StringExpr,
        unescape,
    },
    error::Error,
    token::TokenKind,
};

impl<'a> AstParser<'a> {
    /// Parse an integer or float literal.
    pub(super) fn parse_number(&mut self) -> Expr<'a> {
        let tok = self.next_token();
        let kind = if tok.kind == TokenKind::Float {
            NumberKind::Float
        } else if tok.text.starts_with('-') {
            NumberKind::NegativeInteger
        } else {
            NumberKind::Integer
        };
        Expr::Number(NumberExpr {
            span: tok.span,
            raw: Cow::Borrowed(tok.text),
            kind,
        })
    }

    /// Parse a string, byte string, char or byte literal.
    ///
    /// An invalid escape yields an [`Expr::Error`] whose error points at the escape.
    pub(super) fn parse_text_literal(&mut self, errors: &mut Vec<Error>) -> Expr<'a> {
        let tok = self.next_token();
        let span = tok.span;
        let raw = Cow::Borrowed(tok.text);
        let result = match tok.kind {
            TokenKind::String => unescape::decode_str(tok.text).map(|(value, kind)| {
                Expr::String(StringExpr {
                    span,
                    raw,
                    value,
                    kind,
                })
            }),
            TokenKind::ByteString => unescape::decode_bytes(tok.text).map(|(value, kind)| {
                Expr::Bytes(BytesExpr {
                    span,
                    raw,
                    value,
                    kind,
                })
            }),
            TokenKind::Char => unescape::decode_char(tok.text)
                .map(|value| Expr::Char(CharExpr { span, raw, value })),
            _ => unescape::decode_byte(tok.text)
                .map(|value| Expr::Byte(ByteExpr { span, raw, value })),
        };
        result.unwrap_or_else(|err| {
            let start = span.start_offset;
            let error = Error::with_span(
                err.kind,
                self.span_of(start + err.range.start..start + err.range.end),
            );
            errors.push(error.clone());
            Expr::Error(ErrorExpr { span, error })
        })
    }
}

#[cfg(test)]
#[allow(clippy::panic)]
mod tests {
    use super::super::{parse_document, parse_document_lossy};
    use crate::{
        ast::{Expr, NumberKind, StringKind},
        error::{ErrorKind, Position},
    };

    #[test]
    fn parse_integer() {
        let doc = parse_document("42").unwrap();
        match doc.value {
            Some(Expr::Number(n)) => {
                assert_eq!(n.raw, "42");
                assert_eq!(n.kind, NumberKind::Integer);
            }
            _ => panic!("expected number"),
        }
    }

    #[test]
    fn parse_negative_integer() {
        let doc = parse_document("-42").unwrap();
        match doc.value {
            Some(Expr::Number(n)) => {
                assert_eq!(n.raw, "-42");
                assert_eq!(n.kind, NumberKind::NegativeInteger);
            }
            _ => panic!("expected number"),
        }
    }

    #[test]
    fn parse_float() {
        for source in ["3.14", "1.", ".5", "inf", "-NaN"] {
            match parse_document(source).unwrap().value {
                Some(Expr::Number(n)) => {
                    assert_eq!(n.raw, source);
                    assert_eq!(n.kind, NumberKind::Float);
                }
                other => panic!("expected number for {source}, got {other:?}"),
            }
        }
    }

    #[test]
    fn parse_string() {
        let doc = parse_document(r#""hello""#).unwrap();
        match doc.value {
            Some(Expr::String(s)) => {
                assert_eq!(s.value, "hello");
                assert_eq!(s.kind, StringKind::Regular);
            }
            _ => panic!("expected string"),
        }
    }

    #[test]
    fn parse_string_with_escapes() {
        let doc = parse_document(r#""hello\nworld""#).unwrap();
        match doc.value {
            Some(Expr::String(s)) => {
                assert_eq!(s.value, "hello\nworld");
            }
            _ => panic!("expected string"),
        }
    }

    #[test]
    fn parse_raw_string() {
        let doc = parse_document(r##"r#"raw string"#"##).unwrap();
        match doc.value {
            Some(Expr::String(s)) => {
                assert_eq!(s.value, "raw string");
                assert!(matches!(s.kind, StringKind::Raw { hash_count: 1 }));
            }
            _ => panic!("expected string"),
        }
    }

    #[test]
    fn parse_char() {
        let doc = parse_document("'a'").unwrap();
        match doc.value {
            Some(Expr::Char(c)) => {
                assert_eq!(c.value, 'a');
            }
            _ => panic!("expected char"),
        }
    }

    #[test]
    fn parse_byte_literal() {
        for (source, expected) in [("b'a'", b'a'), ("b'\\n'", b'\n'), ("b'\\xff'", 0xff)] {
            match parse_document(source).unwrap().value {
                Some(Expr::Byte(b)) => {
                    assert_eq!(b.value, expected);
                    assert_eq!(b.raw, source);
                }
                other => panic!("expected byte for {source}, got {other:?}"),
            }
        }
        assert!(parse_document("b'é'").is_err());
    }

    #[test]
    fn invalid_literal_becomes_error_expr() {
        let (doc, errors) = parse_document_lossy(r#"["\q", 1]"#);
        assert_eq!(errors.len(), 1);
        match doc.value {
            Some(Expr::Seq(seq)) => {
                assert!(matches!(seq.items[0].expr, Expr::Error(_)));
                assert!(matches!(seq.items[1].expr, Expr::Number(_)));
            }
            other => panic!("expected seq, got {other:?}"),
        }
    }

    /// Start position, end position and kind of the first error for `source`.
    fn first_error(source: &str) -> (Position, Position, ErrorKind) {
        let err = parse_document(source).unwrap_err();
        (err.span().start, err.span().end, err.kind().clone())
    }

    fn pos(line: usize, col: usize) -> Position {
        Position { line, col }
    }

    #[test]
    fn escape_error_points_at_escape() {
        let (start, end, kind) = first_error("\"ab\\q\"");
        assert_eq!((start, end), (pos(1, 4), pos(1, 6)));
        assert!(matches!(kind, ErrorKind::InvalidEscape(_)));
    }

    #[test]
    fn escape_error_on_later_line() {
        // Used to report 1:5, assuming the escape is on the string's first line.
        let (start, end, _) = first_error("\"a\nb\\q\"");
        assert_eq!((start, end), (pos(2, 2), pos(2, 4)));
    }

    #[test]
    fn escape_error_after_multibyte_char() {
        // Columns are byte-based; `é` is two bytes.
        let (start, end, _) = first_error("\"é\\q\"");
        assert_eq!((start, end), (pos(1, 4), pos(1, 6)));
    }

    #[test]
    fn escape_error_in_byte_string_points_at_escape() {
        let (start, end, _) = first_error("[\n  b\"ab\\u{41}\"]");
        assert_eq!((start, end), (pos(2, 7), pos(2, 9)));
    }

    #[test]
    fn lexer_errors_have_specific_messages() {
        let expected_string_end = |s: &str| {
            let (_, _, kind) = first_error(s);
            assert_eq!(kind, ErrorKind::ExpectedStringEnd, "{s}");
        };
        expected_string_end("\"abc");
        expected_string_end("r\"abc");
        expected_string_end("r#\"abc\"");
        expected_string_end("b\"abc");
        expected_string_end("br\"abc");

        let (_, _, kind) = first_error("'a");
        assert_eq!(
            kind.to_string(),
            "Expected closing `'` in character literal"
        );
        let (_, _, kind) = first_error("''");
        assert_eq!(
            kind.to_string(),
            "Expected a character in character literal"
        );
        let (_, _, kind) = first_error("0x");
        assert_eq!(kind.to_string(), "Expected hexadecimal digits in integer");
        let (_, _, kind) = first_error("1e");
        assert_eq!(kind.to_string(), "Expected exponent digits in float");
        let (_, _, kind) = first_error("r#");
        assert_eq!(kind, ErrorKind::InvalidIdentifier("r#".into()));
        let (_, _, kind) = first_error("/* open");
        assert_eq!(kind, ErrorKind::UnclosedBlockComment);
        let (_, _, kind) = first_error("1 /* open");
        assert_eq!(kind, ErrorKind::UnclosedBlockComment);
    }

    #[test]
    fn invalid_digit_points_at_digit() {
        for (source, col) in [("0xg", 3), ("-0xg", 4), ("[0b2]", 4)] {
            let (start, end, kind) = first_error(source);
            assert_eq!((start, end), (pos(1, col), pos(1, col + 1)), "{source}");
            assert!(
                matches!(kind, ErrorKind::InvalidIntegerDigit { .. }),
                "{source}: {kind:?}"
            );
        }
    }
}
