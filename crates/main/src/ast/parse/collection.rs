//! Parsing for collection types (sequences and maps).

use alloc::vec::Vec;

use super::{
    AstParser,
    core::{Delim, expected, span_at_end},
};
use crate::{
    ast::{ErrorExpr, Expr, MapEntry, MapExpr, SeqExpr, SeqItem, Trivia},
    error::{Error, Span},
    token::TokenKind,
};

impl<'a> AstParser<'a> {
    /// Parse a sequence `[a, b, c]`.
    #[inline(never)]
    pub(super) fn parse_seq(&mut self, errors: &mut Vec<Error>) -> Expr<'a> {
        let open = self.next_token();
        let list = self.parse_list(Delim::Bracket, "array", errors, |p, leading, errors| {
            SeqItem {
                leading,
                expr: p.parse_expr(errors),
                trailing: Trivia::empty(),
                comma: None,
            }
        });
        Expr::Seq(SeqExpr {
            span: Span::between(&open.span, &list.close),
            open_bracket: open.span,
            leading: Trivia::empty(),
            items: list.items,
            trailing: list.trailing,
            close_bracket: list.close,
        })
    }

    /// Parse a map `{key: value, ...}`.
    #[inline(never)]
    pub(super) fn parse_map(&mut self, errors: &mut Vec<Error>) -> Expr<'a> {
        let open = self.next_token();
        let list = self.parse_list(Delim::Brace, "map", errors, Self::parse_map_entry);
        Expr::Map(MapExpr {
            span: Span::between(&open.span, &list.close),
            open_brace: open.span,
            leading: Trivia::empty(),
            entries: list.items,
            trailing: list.trailing,
            close_brace: list.close,
        })
    }

    fn parse_map_entry(&mut self, leading: Trivia<'a>, errors: &mut Vec<Error>) -> MapEntry<'a> {
        let key = self.parse_expr(errors);
        let pre_colon = self.take_trivia();

        let (colon, post_colon, value) = if let Some(colon) = self.eat(TokenKind::Colon) {
            let post_colon = self.take_trivia();
            (colon, post_colon, self.parse_value("map entry", errors))
        } else {
            let colon = span_at_end(key.span());
            let error = Error::with_span(
                expected("`:` after map key", Some("map entry")),
                self.peek_span(),
            );
            // An invalid key was already reported (and recovered from).
            if !matches!(key, Expr::Error(_)) {
                errors.push(error.clone());
                self.recover();
            }
            let value = Expr::Error(ErrorExpr { span: colon, error });
            (colon, Trivia::empty(), value)
        };

        MapEntry {
            leading,
            key,
            pre_colon,
            colon,
            post_colon,
            value,
            trailing: Trivia::empty(),
            comma: None,
        }
    }
}

#[cfg(test)]
#[allow(clippy::panic)]
mod tests {
    use super::super::{parse_document, parse_document_lossy};
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
                assert!(seq.items.is_empty(), "unexpected items: {:?}", seq.items);
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

    #[test]
    fn map_missing_value_keeps_closer() {
        // The `}` used to be consumed as the missing value.
        let (doc, errors) = parse_document_lossy(r#"[{"a": }, 2]"#);
        assert_eq!(errors.len(), 1, "{errors:?}");
        match doc.value {
            Some(Expr::Seq(seq)) => {
                assert_eq!(seq.items.len(), 2);
                assert!(matches!(seq.items[1].expr, Expr::Number(_)));
            }
            other => panic!("expected seq, got {other:?}"),
        }
    }

    #[test]
    fn map_missing_colon_stops_at_outer_closer() {
        let (doc, errors) = parse_document_lossy(r#"[{"a" ]"#);
        assert_eq!(errors.len(), 2, "{errors:?}"); // missing `:` and missing `}`
        match doc.value {
            Some(Expr::Seq(seq)) => assert_eq!(seq.items.len(), 1),
            other => panic!("expected seq, got {other:?}"),
        }
    }
}
