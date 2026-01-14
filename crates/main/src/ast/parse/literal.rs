//! Parsing for literal values (integers, floats, strings, chars, bytes).

use alloc::borrow::Cow;

use super::{AstParser, core::ParserCore};
use crate::{
    ast::{ByteExpr, BytesExpr, CharExpr, Expr, NumberExpr, NumberKind, StringExpr, unescape},
    error::{Position, Result, Span},
    token::TokenKind,
};

/// Internal trait for parsing literal values.
pub(super) trait LiteralParser<'a>: ParserCore<'a> {
    fn parse_integer(&mut self) -> Expr<'a>;
    fn parse_float(&mut self) -> Expr<'a>;
    fn parse_string(&mut self) -> Result<Expr<'a>>;
    fn parse_bytes(&mut self) -> Result<Expr<'a>>;
    fn parse_char(&mut self) -> Result<Expr<'a>>;
}

impl<'a> LiteralParser<'a> for AstParser<'a> {
    fn parse_integer(&mut self) -> Expr<'a> {
        let tok = self.next_token();
        debug_assert_eq!(tok.kind, TokenKind::Integer);

        let kind = if tok.text.starts_with('-') {
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

    /// Parse a float literal.
    fn parse_float(&mut self) -> Expr<'a> {
        let tok = self.next_token();
        debug_assert_eq!(tok.kind, TokenKind::Float);

        Expr::Number(NumberExpr {
            span: tok.span,
            raw: Cow::Borrowed(tok.text),
            kind: NumberKind::Float,
        })
    }

    /// Parse a string literal.
    fn parse_string(&mut self) -> Result<Expr<'a>> {
        let tok = self.next_token();
        debug_assert_eq!(tok.kind, TokenKind::String);

        let (value, kind) = unescape::decode_string(tok.text).map_err(|(e, byte_offset)| {
            // Compute the error position within the source
            // byte_offset is relative to the token text start
            let error_start_offset = tok.span.start_offset + byte_offset;
            let error_end_offset = error_start_offset + 2; // Point to the escape sequence (\x)

            // Compute line/column by counting from token start
            // For now, assume same line (escapes rarely span lines)
            let col_offset = tok.text[..byte_offset].chars().count();
            let error_col = tok.span.start.col + col_offset;

            Self::error(
                Span {
                    start: Position {
                        line: tok.span.start.line,
                        col: error_col,
                    },
                    end: Position {
                        line: tok.span.start.line,
                        col: error_col + 2,
                    },
                    start_offset: error_start_offset,
                    end_offset: error_end_offset.min(tok.span.end_offset),
                },
                e.kind().clone(),
            )
        })?;

        Ok(Expr::String(StringExpr {
            span: tok.span,
            raw: Cow::Borrowed(tok.text),
            value,
            kind,
        }))
    }

    /// Parse a byte string literal.
    fn parse_bytes(&mut self) -> Result<Expr<'a>> {
        let tok = self.next_token();
        debug_assert_eq!(tok.kind, TokenKind::ByteString);

        let (value, kind) = unescape::decode_byte_string(tok.text)
            .map_err(|e| Self::error(tok.span.clone(), e.kind().clone()))?;

        Ok(Expr::Bytes(BytesExpr {
            span: tok.span,
            raw: Cow::Borrowed(tok.text),
            value,
            kind,
        }))
    }

    /// Parse a character literal.
    fn parse_char(&mut self) -> Result<Expr<'a>> {
        let tok = self.next_token();
        debug_assert_eq!(tok.kind, TokenKind::Char);

        // Check if it's a byte literal b'x'
        if tok.text.starts_with("b'") {
            let content = &tok.text[2..tok.text.len() - 1];
            let value = unescape::unescape_byte_char(content)
                .map_err(|e| Self::error(tok.span.clone(), e.kind().clone()))?;
            return Ok(Expr::Byte(ByteExpr {
                span: tok.span,
                raw: Cow::Borrowed(tok.text),
                value,
            }));
        }

        // Regular char literal 'x'
        let content = &tok.text[1..tok.text.len() - 1];
        let value = unescape::unescape_char(content)
            .map_err(|e| Self::error(tok.span.clone(), e.kind().clone()))?;

        Ok(Expr::Char(CharExpr {
            span: tok.span,
            raw: Cow::Borrowed(tok.text),
            value,
        }))
    }
}

#[cfg(test)]
#[allow(clippy::panic)]
mod tests {
    use super::super::parse_document;
    use crate::ast::{Expr, NumberKind, StringKind};

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
        let doc = parse_document("3.14").unwrap();
        match doc.value {
            Some(Expr::Number(n)) => {
                assert_eq!(n.raw, "3.14");
                assert_eq!(n.kind, NumberKind::Float);
            }
            _ => panic!("expected number"),
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
}
