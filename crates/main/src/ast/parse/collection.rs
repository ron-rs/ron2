//! Parsing for collection types (sequences and maps).

use alloc::vec::Vec;

use super::{AstParser, core::ParserCore};
use crate::{
    ast::{ErrorExpr, Expr, MapEntry, MapExpr, SeqExpr, SeqItem, Trivia},
    error::{Error, Span},
    token::TokenKind,
};

/// Internal trait for parsing collection types.
pub(super) trait CollectionParser<'a>: ParserCore<'a> {
    fn parse_seq(&mut self, errors: &mut Vec<Error>) -> Expr<'a>;
    fn parse_map(&mut self, errors: &mut Vec<Error>) -> Expr<'a>;
}

impl<'a> CollectionParser<'a> for AstParser<'a> {
    /// Parse a sequence `[a, b, c]` with error recovery.
    fn parse_seq(&mut self, errors: &mut Vec<Error>) -> Expr<'a> {
        let open_bracket = self.next_token();
        debug_assert_eq!(open_bracket.kind, TokenKind::LBracket);

        let mut leading = self.collect_leading_trivia();
        let mut items = Vec::new();

        loop {
            let kind = self.peek_kind();

            // Always check for closing bracket (handles trailing commas)
            if kind == TokenKind::RBracket {
                break;
            }
            // Only check for EOF if we're not after a comma (allow error in strict mode)
            if (items.is_empty()
                || items
                    .last()
                    .is_some_and(|item: &SeqItem| item.comma.is_none()))
                && kind == TokenKind::Eof
            {
                break;
            }

            let expr = self.parse_expr_lossy(errors);
            let trailing = self.collect_leading_trivia();

            let (comma, has_comma) = self.consume_comma();

            items.push(SeqItem {
                leading,
                expr,
                trailing,
                comma,
            });

            leading = self.collect_leading_trivia();

            if !has_comma && !self.handle_missing_comma("array", TokenKind::RBracket, errors) {
                break;
            }
        }

        let trailing = if items.is_empty() {
            leading
        } else {
            self.collect_leading_trivia()
        };

        let close_bracket = self.consume_closing(
            TokenKind::RBracket,
            Self::expected("closing `]`", Some("array")),
            errors,
        );

        Expr::Seq(SeqExpr {
            span: Span::between(&open_bracket.span, &close_bracket),
            open_bracket: open_bracket.span,
            leading: Trivia::empty(),
            items,
            trailing,
            close_bracket,
        })
    }

    /// Parse a map `{key: value, ...}` with error recovery.
    fn parse_map(&mut self, errors: &mut Vec<Error>) -> Expr<'a> {
        let open_brace = self.next_token();
        debug_assert_eq!(open_brace.kind, TokenKind::LBrace);

        let mut leading = self.collect_leading_trivia();
        let mut entries = Vec::new();

        loop {
            let kind = self.peek_kind();

            // Always check for closing brace (handles trailing commas)
            if kind == TokenKind::RBrace {
                break;
            }
            // Only check for EOF if not after a comma
            if (entries.is_empty() || entries.last().is_some_and(|e: &MapEntry| e.comma.is_none()))
                && kind == TokenKind::Eof
            {
                break;
            }

            let key = self.parse_expr_lossy(errors);
            let pre_colon = self.collect_leading_trivia();

            if self.peek_kind() != TokenKind::Colon {
                // Missing colon - create error entry and recover
                errors.push(Self::error(
                    self.peek_span(),
                    Self::expected("`:` after map key", Some("map entry")),
                ));
                self.recover_until(&[TokenKind::Comma, TokenKind::RBrace]);
                let trailing = self.collect_leading_trivia();
                let (comma, _) = self.consume_comma();
                let colon_span = pre_colon.span.unwrap_or_else(|| self.eof_span());
                let key_span = key.span();
                let error_span = Self::span_at_end(key_span);
                entries.push(MapEntry {
                    leading,
                    key,
                    pre_colon,
                    colon: Self::span_at_end(&colon_span),
                    post_colon: Trivia::empty(),
                    value: Expr::Error(ErrorExpr {
                        span: error_span,
                        error: Error::with_span(
                            Self::expected("value", Some("map entry")),
                            error_span,
                        ),
                    }),
                    trailing,
                    comma,
                });
                leading = self.collect_leading_trivia();
                continue;
            }
            let colon_tok = self.next_token();

            let post_colon = self.collect_leading_trivia();
            let value = self.parse_expr_lossy(errors);
            let trailing = self.collect_leading_trivia();

            let (comma, has_comma) = self.consume_comma();

            entries.push(MapEntry {
                leading,
                key,
                pre_colon,
                colon: colon_tok.span,
                post_colon,
                value,
                trailing,
                comma,
            });

            leading = self.collect_leading_trivia();

            if !has_comma && !self.handle_missing_comma("map", TokenKind::RBrace, errors) {
                break;
            }
        }

        let trailing = if entries.is_empty() {
            leading
        } else {
            self.collect_leading_trivia()
        };

        let close_brace = self.consume_closing(
            TokenKind::RBrace,
            Self::expected("closing `}`", Some("map")),
            errors,
        );

        Expr::Map(MapExpr {
            span: Span::between(&open_brace.span, &close_brace),
            open_brace: open_brace.span,
            leading: Trivia::empty(),
            entries,
            trailing,
            close_brace,
        })
    }
}

#[cfg(test)]
#[allow(clippy::panic)]
mod tests {
    use super::super::parse_document;
    use crate::ast::Expr;

    #[test]
    fn parse_seq() {
        let doc = parse_document("[1, 2, 3]").unwrap();
        match doc.value {
            Some(Expr::Seq(seq)) => {
                assert_eq!(seq.items.len(), 3);
            }
            _ => panic!("expected seq"),
        }
    }

    #[test]
    fn parse_empty_seq() {
        let doc = parse_document("[]").unwrap();
        match doc.value {
            Some(Expr::Seq(seq)) => {
                assert!(seq.items.is_empty());
            }
            _ => panic!("expected seq"),
        }
    }

    #[test]
    fn parse_map() {
        let doc = parse_document(r#"{"a": 1, "b": 2}"#).unwrap();
        match doc.value {
            Some(Expr::Map(map)) => {
                assert_eq!(map.entries.len(), 2);
            }
            _ => panic!("expected map"),
        }
    }
}
