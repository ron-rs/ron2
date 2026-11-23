//! Parsing for identifier-led values (`true`, `None`, `Some(..)`, structs and
//! enum variants) and named struct fields.

use alloc::{borrow::Cow, boxed::Box, vec::Vec};

use super::{
    AstParser,
    core::{Delim, expected, span_at_end},
};
use crate::{
    ast::{
        BoolExpr, ErrorExpr, Expr, FieldsBody, Ident, OptionExpr, OptionValue, StructBody,
        StructExpr, StructField, Trivia, TupleBody,
    },
    error::{Error, Span},
    token::{Token, TokenKind},
};

impl<'a> AstParser<'a> {
    /// Parse an expression starting with an identifier.
    pub(super) fn parse_ident_expr(&mut self, errors: &mut Vec<Error>) -> Expr<'a> {
        let tok = self.next_token();
        match tok.text {
            "true" | "false" => Expr::Bool(BoolExpr {
                span: tok.span,
                value: tok.text == "true",
            }),
            "None" => Expr::Option(Box::new(OptionExpr {
                span: tok.span,
                value: None,
            })),
            "Some" => self.parse_some(&tok, errors),
            _ => self.parse_struct(&tok, errors),
        }
    }

    /// Parse `Some(value)` after the `Some` token.
    fn parse_some(&mut self, some: &Token<'a>, errors: &mut Vec<Error>) -> Expr<'a> {
        let Some(open_paren) = self.eat(TokenKind::LParen) else {
            let err = Error::with_span(expected("`(` after `Some`", None), self.peek_span());
            return self.error_expr(err, errors);
        };
        let (leading, expr, trailing, close_paren) = self.with_open(Delim::Paren, |p| {
            let leading = p.take_trivia();
            let expr = p.parse_value("option", errors);
            let trailing = p.take_trivia();
            let close = p.expect_closer(Delim::Paren, "option", errors);
            (leading, expr, trailing, close)
        });
        Expr::Option(Box::new(OptionExpr {
            span: Span::between(&some.span, &close_paren),
            value: Some(OptionValue {
                open_paren,
                leading,
                expr,
                trailing,
                close_paren,
            }),
        }))
    }

    /// Parse a struct or enum variant: `Name`, `Name(a, b)` or `Name(x: a)`.
    fn parse_struct(&mut self, name: &Token<'a>, errors: &mut Vec<Error>) -> Expr<'a> {
        let (pre_body, body) = if self.peek_kind() == TokenKind::LParen {
            (self.take_trivia(), Some(self.parse_struct_body(errors)))
        } else {
            // Trivia after a bodiless name belongs to whatever follows the value.
            (Trivia::empty(), None)
        };

        let span = match &body {
            Some(StructBody::Tuple(t)) => Span::between(&name.span, &t.close_paren),
            Some(StructBody::Fields(f)) => Span::between(&name.span, &f.close_brace),
            None => name.span,
        };
        Expr::Struct(StructExpr {
            span,
            name: Ident {
                span: name.span,
                name: Cow::Borrowed(name.text),
            },
            pre_body,
            body,
        })
    }

    fn parse_struct_body(&mut self, errors: &mut Vec<Error>) -> StructBody<'a> {
        let open = self.next_token();
        if self.peek_is_named_field() {
            let list = self.parse_list(Delim::Paren, "struct", errors, Self::parse_field);
            StructBody::Fields(FieldsBody {
                open_brace: open.span,
                leading: Trivia::empty(),
                fields: list.items,
                trailing: list.trailing,
                close_brace: list.close,
            })
        } else {
            let list = self.parse_list(Delim::Paren, "tuple", errors, Self::parse_element);
            StructBody::Tuple(TupleBody {
                open_paren: open.span,
                leading: Trivia::empty(),
                elements: list.items,
                trailing: list.trailing,
                close_paren: list.close,
            })
        }
    }

    /// Parse a named field `name: value`.
    ///
    /// Anything other than an identifier becomes a placeholder field with an
    /// empty name and an error value.
    pub(super) fn parse_field(
        &mut self,
        leading: Trivia<'a>,
        errors: &mut Vec<Error>,
    ) -> StructField<'a> {
        if self.peek_kind() != TokenKind::Ident {
            let span = self.peek_span();
            let error = self.unexpected(&self.peeked, expected("identifier", None));
            errors.push(error.clone());
            self.recover();
            return StructField {
                leading,
                name: Ident {
                    span,
                    name: Cow::Borrowed(""),
                },
                pre_colon: Trivia::empty(),
                colon: span_at_end(&span),
                post_colon: Trivia::empty(),
                value: Expr::Error(ErrorExpr { span, error }),
                trailing: Trivia::empty(),
                comma: None,
            };
        }

        let name = self.next_token();
        let pre_colon = self.take_trivia();
        let colon = self.eat(TokenKind::Colon).unwrap_or_else(|| {
            errors.push(Error::with_span(
                expected("`:` after field name", Some("struct field")),
                name.span,
            ));
            span_at_end(&name.span)
        });
        let post_colon = self.take_trivia();
        let value = self.parse_value("struct field", errors);

        StructField {
            leading,
            name: Ident {
                span: name.span,
                name: Cow::Borrowed(name.text),
            },
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
    use crate::ast::{Expr, StructBody};

    #[test]
    fn parse_named_struct_bodies() {
        let doc = parse_document("Foo(x: 1, y: Bar(2, 3))").unwrap();
        let Some(Expr::Struct(s)) = doc.value else {
            panic!("expected struct");
        };
        let Some(StructBody::Fields(fields)) = s.body else {
            panic!("expected fields");
        };
        assert_eq!(fields.fields.len(), 2);
        let Expr::Struct(bar) = &fields.fields[1].value else {
            panic!("expected struct");
        };
        assert!(matches!(&bar.body, Some(StructBody::Tuple(t)) if t.elements.len() == 2));
    }

    #[test]
    fn parse_empty_named_body() {
        let doc = parse_document("Foo()").unwrap();
        let Some(Expr::Struct(s)) = doc.value else {
            panic!("expected struct");
        };
        assert!(matches!(&s.body, Some(StructBody::Tuple(t)) if t.elements.is_empty()));
    }

    #[test]
    fn named_body_tuple_starting_with_ident() {
        // The first element is an identifier but not a field name.
        let doc = parse_document("Foo(Bar, true, None, Some(1))").unwrap();
        let Some(Expr::Struct(s)) = doc.value else {
            panic!("expected struct");
        };
        let Some(StructBody::Tuple(t)) = s.body else {
            panic!("expected tuple body");
        };
        assert!(matches!(t.elements[0].expr, Expr::Struct(_)));
        assert!(matches!(t.elements[1].expr, Expr::Bool(_)));
        assert!(matches!(t.elements[2].expr, Expr::Option(_)));
        assert!(matches!(t.elements[3].expr, Expr::Option(_)));
    }

    #[test]
    fn bodiless_name_does_not_take_following_trivia() {
        let doc = parse_document("[A /* a */, B]").unwrap();
        let Some(Expr::Seq(seq)) = doc.value else {
            panic!("expected seq");
        };
        let Expr::Struct(a) = &seq.items[0].expr else {
            panic!("expected struct");
        };
        assert_eq!(a.pre_body.comments.len(), 0);
        assert_eq!(seq.items[0].trailing.comments.len(), 1);
    }

    #[test]
    fn some_without_value_keeps_closer() {
        let (doc, errors) = parse_document_lossy("[Some(), 1]");
        assert_eq!(errors.len(), 1, "{errors:?}");
        let Some(Expr::Seq(seq)) = doc.value else {
            panic!("expected seq");
        };
        assert_eq!(seq.items.len(), 2);
    }

    #[test]
    fn some_with_extra_tokens() {
        let (doc, errors) = parse_document_lossy("Some(1 2)");
        assert_eq!(errors.len(), 1, "{errors:?}");
        assert!(matches!(doc.value, Some(Expr::Option(_))));
    }

    #[test]
    fn missing_colon_after_field_name() {
        let (doc, errors) = parse_document_lossy("(x: 1, y 2)");
        assert_eq!(errors.len(), 1, "{errors:?}");
        assert_eq!(
            errors[0].kind().to_string(),
            "Expected `:` after field name in struct field"
        );
        let Some(Expr::AnonStruct(s)) = doc.value else {
            panic!("expected anon struct");
        };
        assert_eq!(s.fields.len(), 2);
        assert!(matches!(s.fields[1].value, Expr::Number(_)));
    }
}
