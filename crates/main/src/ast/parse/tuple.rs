//! Parsing for tuple types and unit values.

use alloc::vec::Vec;

use super::{AstParser, core::ParserCore, struct_field::StructFieldParser};
use crate::{
    ast::{Expr, Trivia, TupleBody, TupleElement, TupleExpr, UnitExpr},
    error::{Error, Result, Span},
    token::{Token, TokenKind},
};

/// Internal trait for parsing tuple types.
pub(super) trait TupleParser<'a>: ParserCore<'a> {
    fn parse_tuple_or_unit_prefix(&mut self) -> (Token<'a>, Trivia<'a>, Option<Span>, bool);
    fn parse_tuple_or_unit(&mut self) -> Result<Expr<'a>>;
    fn parse_tuple_or_unit_inner(&mut self, errors: &mut Vec<Error>) -> Expr<'a>;
    fn parse_tuple_or_unit_lossy(&mut self, errors: &mut Vec<Error>) -> Expr<'a>;
    fn peek_is_named_field(&mut self) -> bool;
    fn try_parse_empty_tuple_body(
        &mut self,
        open_paren: &Span,
        leading: Trivia<'a>,
    ) -> Result<TupleBody<'a>, Trivia<'a>>;
    fn parse_tuple_inner_impl(
        &mut self,
        open_paren: Token<'a>,
        leading: Trivia<'a>,
        errors: &mut Vec<Error>,
    ) -> Expr<'a>;
    fn parse_tuple_inner(&mut self, open_paren: Token<'a>, leading: Trivia<'a>)
    -> Result<Expr<'a>>;
    fn parse_tuple_inner_lossy(
        &mut self,
        open_paren: Token<'a>,
        leading: Trivia<'a>,
        errors: &mut Vec<Error>,
    ) -> Expr<'a>;
    fn parse_tuple_elements_inner(
        &mut self,
        leading: Trivia<'a>,
        errors: &mut Vec<Error>,
    ) -> Vec<TupleElement<'a>>;
    fn parse_tuple_elements(&mut self, leading: Trivia<'a>) -> Result<Vec<TupleElement<'a>>>;
    fn parse_tuple_elements_lossy(
        &mut self,
        leading: Trivia<'a>,
        errors: &mut Vec<Error>,
    ) -> Vec<TupleElement<'a>>;
    fn parse_tuple_elements_from_first_inner(
        &mut self,
        leading: Trivia<'a>,
        first_expr: Expr<'a>,
        errors: &mut Vec<Error>,
    ) -> (Vec<TupleElement<'a>>, Trivia<'a>);
    fn parse_tuple_elements_from_first(
        &mut self,
        leading: Trivia<'a>,
        first_expr: Expr<'a>,
    ) -> Result<(Vec<TupleElement<'a>>, Trivia<'a>)>;
    fn parse_tuple_elements_from_first_lossy(
        &mut self,
        leading: Trivia<'a>,
        first_expr: Expr<'a>,
        errors: &mut Vec<Error>,
    ) -> (Vec<TupleElement<'a>>, Trivia<'a>);
}

impl<'a> TupleParser<'a> for AstParser<'a> {
    fn parse_tuple_or_unit_prefix(&mut self) -> (Token<'a>, Trivia<'a>, Option<Span>, bool) {
        let open_paren = self.next_token();
        debug_assert_eq!(open_paren.kind, TokenKind::LParen);

        let leading = self.collect_leading_trivia();

        // Check for empty tuple (unit)
        let close_if_unit = if self.peek_kind() == TokenKind::RParen {
            Some(self.next_token().span)
        } else {
            None
        };

        // Check for named field syntax: `x: value`
        let is_named_fields = self.peek_is_named_field();

        (open_paren, leading, close_if_unit, is_named_fields)
    }

    /// Parse a tuple `(a, b, c)` or unit `()`.
    fn parse_tuple_or_unit(&mut self) -> Result<Expr<'a>> {
        let mut errors = Vec::new();
        let expr = self.parse_tuple_or_unit_inner(&mut errors);
        if errors.is_empty() {
            Ok(expr)
        } else {
            Err(errors.remove(0))
        }
    }

    /// Parse a tuple `(a, b, c)` or unit `()` (internal unified implementation).
    fn parse_tuple_or_unit_inner(&mut self, errors: &mut Vec<Error>) -> Expr<'a> {
        let (open_paren, leading, close_if_unit, is_named_fields) =
            self.parse_tuple_or_unit_prefix();

        if let Some(close_span) = close_if_unit {
            return Expr::Unit(UnitExpr {
                span: Span::between(&open_paren.span, &close_span),
            });
        }

        if is_named_fields {
            self.parse_fields_body_inner_impl(open_paren, leading, errors)
        } else {
            self.parse_tuple_inner_impl(open_paren, leading, errors)
        }
    }

    /// Parse a tuple `(a, b, c)` or unit `()` with error recovery.
    fn parse_tuple_or_unit_lossy(&mut self, errors: &mut Vec<Error>) -> Expr<'a> {
        self.parse_tuple_or_unit_inner(errors)
    }

    /// Check if the next tokens look like `ident:` (a named field).
    fn peek_is_named_field(&mut self) -> bool {
        // Check if we have `ident:` pattern using two-token lookahead
        let (first, second) = self.peek_two_kinds();
        first == TokenKind::Ident && second == TokenKind::Colon
    }

    /// Try to parse an empty tuple body `()`.
    ///
    /// If the next token is `)`, consumes it and returns `Ok` with an empty `TupleBody`.
    /// Otherwise returns `Err` with the trivia passed back without consuming any tokens.
    fn try_parse_empty_tuple_body(
        &mut self,
        open_paren: &Span,
        leading: Trivia<'a>,
    ) -> Result<TupleBody<'a>, Trivia<'a>> {
        if self.peek_kind() == TokenKind::RParen {
            let close_paren = self.next_token();
            Ok(TupleBody {
                open_paren: open_paren.clone(),
                leading,
                elements: Vec::new(),
                trailing: Trivia::empty(),
                close_paren: close_paren.span,
            })
        } else {
            Err(leading)
        }
    }

    /// Parse a tuple expression starting after the opening paren.
    /// Parse a tuple expression (internal unified implementation).
    fn parse_tuple_inner_impl(
        &mut self,
        open_paren: Token<'a>,
        mut leading: Trivia<'a>,
        errors: &mut Vec<Error>,
    ) -> Expr<'a> {
        let mut elements = Vec::new();

        loop {
            // Always check for closing paren (handles trailing commas)
            if self.peek_kind() == TokenKind::RParen {
                break;
            }
            // Only check for EOF if not after a comma
            if (elements.is_empty()
                || elements
                    .last()
                    .is_some_and(|e: &TupleElement| e.comma.is_none()))
                && self.peek_kind() == TokenKind::Eof
            {
                break;
            }

            let expr = self.parse_expr_lossy(errors);
            let trailing = self.collect_leading_trivia();

            let (comma, has_comma) = self.consume_comma();

            elements.push(TupleElement {
                leading,
                expr,
                trailing,
                comma,
            });

            leading = self.collect_leading_trivia();

            if !has_comma && !self.handle_missing_comma("tuple", TokenKind::RParen, errors) {
                break;
            }
        }

        let trailing = if elements.is_empty() {
            leading
        } else {
            self.collect_leading_trivia()
        };

        let close_paren = self.consume_closing(
            TokenKind::RParen,
            Self::expected("closing `)` or `}`", Some("struct")),
            errors,
        );

        Expr::Tuple(TupleExpr {
            span: Span::between(&open_paren.span, &close_paren),
            open_paren: open_paren.span,
            leading: Trivia::empty(),
            elements,
            trailing,
            close_paren,
        })
    }

    fn parse_tuple_inner(
        &mut self,
        open_paren: Token<'a>,
        leading: Trivia<'a>,
    ) -> Result<Expr<'a>> {
        let mut errors = Vec::new();
        let expr = self.parse_tuple_inner_impl(open_paren, leading, &mut errors);
        if errors.is_empty() {
            Ok(expr)
        } else {
            Err(errors.remove(0))
        }
    }

    /// Parse a tuple expression with error recovery.
    fn parse_tuple_inner_lossy(
        &mut self,
        open_paren: Token<'a>,
        leading: Trivia<'a>,
        errors: &mut Vec<Error>,
    ) -> Expr<'a> {
        self.parse_tuple_inner_impl(open_paren, leading, errors)
    }

    fn parse_tuple_elements_inner(
        &mut self,
        mut leading: Trivia<'a>,
        errors: &mut Vec<Error>,
    ) -> Vec<TupleElement<'a>> {
        let mut elements = Vec::new();

        loop {
            match self.peek_kind() {
                TokenKind::RParen | TokenKind::Eof => break,
                _ => {}
            }

            let expr = self.parse_expr_inner(errors);
            let trailing = self.collect_leading_trivia();

            let (comma, has_comma) = self.consume_comma();

            elements.push(TupleElement {
                leading,
                expr,
                trailing,
                comma,
            });

            leading = self.collect_leading_trivia();

            if !has_comma {
                if matches!(self.peek_kind(), TokenKind::RParen | TokenKind::Eof) {
                    break;
                }
                errors.push(Self::error(
                    self.peek_span(),
                    Self::expected("comma", Some("tuple")),
                ));
            }
        }

        elements
    }

    fn parse_tuple_elements(&mut self, leading: Trivia<'a>) -> Result<Vec<TupleElement<'a>>> {
        let mut errors = Vec::new();
        let elements = self.parse_tuple_elements_inner(leading, &mut errors);
        if errors.is_empty() {
            Ok(elements)
        } else {
            Err(errors.remove(0))
        }
    }

    /// Parse tuple elements until closing paren with error recovery.
    fn parse_tuple_elements_lossy(
        &mut self,
        leading: Trivia<'a>,
        errors: &mut Vec<Error>,
    ) -> Vec<TupleElement<'a>> {
        self.parse_tuple_elements_inner(leading, errors)
    }

    fn parse_tuple_elements_from_first_inner(
        &mut self,
        leading: Trivia<'a>,
        first_expr: Expr<'a>,
        errors: &mut Vec<Error>,
    ) -> (Vec<TupleElement<'a>>, Trivia<'a>) {
        let mut elements = Vec::new();

        let trailing = self.collect_leading_trivia();
        let (comma, has_comma) = self.consume_comma();

        elements.push(TupleElement {
            leading,
            expr: first_expr,
            trailing,
            comma,
        });

        if !has_comma {
            return (elements, self.collect_leading_trivia());
        }

        let mut leading = self.collect_leading_trivia();

        loop {
            match self.peek_kind() {
                TokenKind::RParen | TokenKind::Eof => break,
                _ => {}
            }

            let expr = self.parse_expr_inner(errors);
            let trailing = self.collect_leading_trivia();

            let (comma, has_comma) = self.consume_comma();

            elements.push(TupleElement {
                leading,
                expr,
                trailing,
                comma,
            });

            leading = self.collect_leading_trivia();

            if !has_comma {
                if matches!(self.peek_kind(), TokenKind::RParen | TokenKind::Eof) {
                    break;
                }
                errors.push(Self::error(
                    self.peek_span(),
                    Self::expected("comma", Some("tuple")),
                ));
            }
        }

        (elements, leading)
    }

    /// Parse tuple elements starting with the first expression already parsed.
    fn parse_tuple_elements_from_first(
        &mut self,
        leading: Trivia<'a>,
        first_expr: Expr<'a>,
    ) -> Result<(Vec<TupleElement<'a>>, Trivia<'a>)> {
        let mut errors = Vec::new();
        let result = self.parse_tuple_elements_from_first_inner(leading, first_expr, &mut errors);
        if errors.is_empty() {
            Ok(result)
        } else {
            Err(errors.remove(0))
        }
    }

    /// Parse tuple elements starting with the first expression, with recovery.
    fn parse_tuple_elements_from_first_lossy(
        &mut self,
        leading: Trivia<'a>,
        first_expr: Expr<'a>,
        errors: &mut Vec<Error>,
    ) -> (Vec<TupleElement<'a>>, Trivia<'a>) {
        self.parse_tuple_elements_from_first_inner(leading, first_expr, errors)
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
    fn parse_empty_parens_is_unit() {
        // Empty parentheses should produce Unit, not AnonStruct
        let doc = parse_document("()").unwrap();
        assert!(matches!(doc.value, Some(Expr::Unit(_))));
    }

    #[test]
    fn parse_tuple_not_anon_struct() {
        // Tuple values (no colons) should produce Tuple, not AnonStruct
        let doc = parse_document("(1, 2, 3)").unwrap();
        match doc.value {
            Some(Expr::Tuple(t)) => {
                assert_eq!(t.elements.len(), 3);
            }
            _ => panic!("expected tuple, got {:?}", doc.value),
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
}
