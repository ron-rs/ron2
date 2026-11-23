//! Parsing for parenthesized values: unit, tuples and anonymous structs.

use alloc::vec::Vec;

use super::{AstParser, core::Delim};
use crate::{
    ast::{AnonStructExpr, Expr, Trivia, TupleElement, TupleExpr, UnitExpr},
    error::{Error, Span},
    token::TokenKind,
};

impl<'a> AstParser<'a> {
    /// Parse `()` (unit), `(x: 1, ...)` (anonymous struct) or `(1, ...)` (tuple).
    #[inline(never)]
    pub(super) fn parse_paren(&mut self, errors: &mut Vec<Error>) -> Expr<'a> {
        let open = self.next_token();

        if self.peek_kind() == TokenKind::RParen {
            let inner = self.take_trivia();
            let close = self.next_token();
            return Expr::Unit(UnitExpr {
                span: Span::between(&open.span, &close.span),
                inner,
            });
        }

        if self.peek_is_named_field() {
            let list = self.parse_list(Delim::Paren, "struct", errors, Self::parse_field);
            Expr::AnonStruct(AnonStructExpr {
                span: Span::between(&open.span, &list.close),
                open_paren: open.span,
                leading: Trivia::empty(),
                fields: list.items,
                trailing: list.trailing,
                close_paren: list.close,
            })
        } else {
            let list = self.parse_list(Delim::Paren, "tuple", errors, Self::parse_element);
            Expr::Tuple(TupleExpr {
                span: Span::between(&open.span, &list.close),
                open_paren: open.span,
                leading: Trivia::empty(),
                elements: list.items,
                trailing: list.trailing,
                close_paren: list.close,
            })
        }
    }

    /// Whether the next tokens are `ident:`, i.e. the start of a named field.
    pub(super) fn peek_is_named_field(&mut self) -> bool {
        self.peek_kind() == TokenKind::Ident && self.peek_second_kind() == TokenKind::Colon
    }

    pub(super) fn parse_element(
        &mut self,
        leading: Trivia<'a>,
        errors: &mut Vec<Error>,
    ) -> TupleElement<'a> {
        TupleElement {
            leading,
            expr: self.parse_expr(errors),
            trailing: Trivia::empty(),
            comma: None,
        }
    }
}

#[cfg(test)]
#[allow(clippy::panic)]
mod tests {
    use super::super::parse_document;
    use crate::ast::Expr;

    #[test]
    fn parse_unit() {
        let doc = parse_document("()").unwrap();
        assert!(matches!(doc.value, Some(Expr::Unit(_))));
        let doc = parse_document("( /* c */ )").unwrap();
        assert!(matches!(doc.value, Some(Expr::Unit(_))));
    }

    #[test]
    fn parse_tuple() {
        let doc = parse_document("(1, 2, 3)").unwrap();
        match doc.value {
            Some(Expr::Tuple(tuple)) => {
                assert_eq!(tuple.elements.len(), 3);
            }
            _ => panic!("expected tuple"),
        }
    }

    #[test]
    fn parse_single_element_tuple() {
        // Single value in parens without colon should be a tuple
        let doc = parse_document("(x)").unwrap();
        match doc.value {
            Some(Expr::Tuple(t)) => {
                assert_eq!(t.elements.len(), 1);
                // The element should be a struct (identifier `x`)
                match &t.elements[0].expr {
                    Expr::Struct(s) => {
                        assert_eq!(s.name.name, "x");
                        assert!(s.body.is_none());
                    }
                    _ => panic!("expected struct/identifier"),
                }
            }
            _ => panic!("expected tuple, got {:?}", doc.value),
        }
    }

    #[test]
    fn named_field_detection_skips_trivia() {
        let doc = parse_document("( x /* c */ : 1 )").unwrap();
        assert!(matches!(doc.value, Some(Expr::AnonStruct(_))));
        let doc = parse_document("( x /* c */ , 1 )").unwrap();
        assert!(matches!(doc.value, Some(Expr::Tuple(_))));
    }
}
