//! Parsing for struct and field types.

use alloc::{borrow::Cow, boxed::Box, vec::Vec};

use super::{AstParser, core::ParserCore, tuple::TupleParser};
use crate::{
    ast::{
        AnonStructExpr, BoolExpr, ErrorExpr, Expr, FieldsBody, Ident, NumberExpr, NumberKind,
        OptionExpr, OptionValue, StructBody, StructExpr, StructField, Trivia, TupleBody,
    },
    error::{Error, Result, Span},
    token::{Token, TokenKind},
};

/// Internal trait for parsing struct and field types.
pub(super) trait StructFieldParser<'a>: ParserCore<'a> {
    fn parse_fields_body_inner_impl(
        &mut self,
        open_paren: Token<'a>,
        leading: Trivia<'a>,
        errors: &mut Vec<Error>,
    ) -> Expr<'a>;
    fn parse_fields_body_inner(
        &mut self,
        open_paren: Token<'a>,
        leading: Trivia<'a>,
    ) -> Result<Expr<'a>>;
    fn parse_fields_body_inner_lossy(
        &mut self,
        open_paren: Token<'a>,
        leading: Trivia<'a>,
        errors: &mut Vec<Error>,
    ) -> Expr<'a>;
    fn parse_ident_expr_inner(&mut self, errors: &mut Vec<Error>) -> Expr<'a>;
    fn parse_ident_expr(&mut self) -> Result<Expr<'a>>;
    fn parse_ident_expr_lossy(&mut self, errors: &mut Vec<Error>) -> Expr<'a>;
    fn parse_some_inner(&mut self, some_tok: Token<'a>, errors: &mut Vec<Error>) -> Expr<'a>;
    fn parse_some(&mut self, some_tok: Token<'a>) -> Result<Expr<'a>>;
    fn parse_some_lossy(&mut self, some_tok: Token<'a>, errors: &mut Vec<Error>) -> Expr<'a>;
    fn parse_struct_or_variant_inner(
        &mut self,
        name_tok: &Token<'a>,
        errors: &mut Vec<Error>,
    ) -> Expr<'a>;
    fn parse_struct_or_variant(&mut self, name_tok: &Token<'a>) -> Result<Expr<'a>>;
    fn parse_struct_or_variant_lossy(
        &mut self,
        name_tok: &Token<'a>,
        errors: &mut Vec<Error>,
    ) -> Expr<'a>;
    fn parse_struct_body_contents_inner(
        &mut self,
        open_paren: Token<'a>,
        leading: Trivia<'a>,
        errors: &mut Vec<Error>,
    ) -> StructBody<'a>;
    fn parse_struct_body_contents(
        &mut self,
        open_paren: Token<'a>,
        leading: Trivia<'a>,
    ) -> Result<Option<StructBody<'a>>>;
    fn parse_struct_body_contents_lossy(
        &mut self,
        open_paren: Token<'a>,
        leading: Trivia<'a>,
        errors: &mut Vec<Error>,
    ) -> StructBody<'a>;
    fn ident_token_to_expr_inner(
        &mut self,
        tok: Token<'a>,
        pre_body: Trivia<'a>,
        errors: &mut Vec<Error>,
    ) -> Expr<'a>;
    fn ident_token_to_expr(&mut self, tok: Token<'a>, pre_body: Trivia<'a>) -> Result<Expr<'a>>;
    fn ident_token_to_expr_lossy(
        &mut self,
        tok: Token<'a>,
        pre_body: Trivia<'a>,
        errors: &mut Vec<Error>,
    ) -> Expr<'a>;
    fn parse_struct_fields_from_first_inner(
        &mut self,
        leading: Trivia<'a>,
        first_name: &Token<'a>,
        pre_colon: Trivia<'a>,
        errors: &mut Vec<Error>,
    ) -> (Vec<StructField<'a>>, Trivia<'a>);
    fn parse_struct_fields_from_first(
        &mut self,
        leading: Trivia<'a>,
        first_name: &Token<'a>,
        pre_colon: Trivia<'a>,
    ) -> Result<(Vec<StructField<'a>>, Trivia<'a>)>;
    fn parse_struct_fields_from_first_lossy(
        &mut self,
        leading: Trivia<'a>,
        first_name: &Token<'a>,
        pre_colon: Trivia<'a>,
        errors: &mut Vec<Error>,
    ) -> (Vec<StructField<'a>>, Trivia<'a>);
}

impl<'a> StructFieldParser<'a> for AstParser<'a> {
    /// Parse a fields body starting after opening paren (internal unified implementation).
    #[allow(clippy::too_many_lines)]
    fn parse_fields_body_inner_impl(
        &mut self,
        open_paren: Token<'a>,
        mut leading: Trivia<'a>,
        errors: &mut Vec<Error>,
    ) -> Expr<'a> {
        let mut fields = Vec::new();

        loop {
            match self.peek_kind() {
                TokenKind::RParen | TokenKind::Eof => break,
                TokenKind::Ident => {}
                _ => {
                    let error_span = self.peek_span();
                    errors.push(Self::error(
                        error_span.clone(),
                        Self::expected("identifier", None),
                    ));
                    self.recover_until(&[TokenKind::Comma, TokenKind::RParen]);
                    let trailing = self.collect_leading_trivia();
                    let (comma, has_comma) = self.consume_comma();
                    // Create a placeholder field to preserve structure
                    fields.push(StructField {
                        leading,
                        name: Ident {
                            span: error_span.clone(),
                            name: Cow::Borrowed(""),
                        },
                        pre_colon: Trivia::empty(),
                        colon: Self::span_at_end(&error_span),
                        post_colon: Trivia::empty(),
                        value: Expr::Error(ErrorExpr {
                            span: error_span.clone(),
                            error: Error::with_span(Self::expected("identifier", None), error_span),
                        }),
                        trailing,
                        comma,
                    });
                    leading = self.collect_leading_trivia();
                    if !has_comma {
                        break;
                    }
                    continue;
                }
            }

            let name_tok = self.next_token();
            let pre_colon = self.collect_leading_trivia();

            let colon = if self.peek_kind() == TokenKind::Colon {
                self.next_token().span
            } else {
                errors.push(Self::error(
                    name_tok.span.clone(),
                    Self::expected("`:` after map key", Some("struct field")),
                ));
                Self::span_at_end(&name_tok.span)
            };

            let post_colon = self.collect_leading_trivia();
            let value = match self.peek_kind() {
                TokenKind::Comma | TokenKind::RParen => {
                    let error_span = Self::span_at_end(&colon);
                    let error_kind = Self::expected("value", Some("struct field"));
                    errors.push(Self::error(error_span.clone(), error_kind.clone()));
                    Expr::Error(ErrorExpr {
                        span: error_span.clone(),
                        error: Error::with_span(error_kind, error_span),
                    })
                }
                _ => self.parse_expr_lossy(errors),
            };
            let trailing = self.collect_leading_trivia();

            let (comma, has_comma) = self.consume_comma();

            fields.push(StructField {
                leading,
                name: Ident {
                    span: name_tok.span.clone(),
                    name: Cow::Borrowed(name_tok.text),
                },
                pre_colon,
                colon,
                post_colon,
                value,
                trailing,
                comma,
            });

            leading = self.collect_leading_trivia();

            if !has_comma {
                if matches!(self.peek_kind(), TokenKind::Ident) {
                    errors.push(Self::error(
                        self.peek_span(),
                        Self::expected("comma", Some("struct")),
                    ));
                } else {
                    break;
                }
            }
        }

        let trailing = if fields.is_empty() {
            leading
        } else {
            self.collect_leading_trivia()
        };

        let close_paren = self.consume_closing(
            TokenKind::RParen,
            Self::expected("closing `)` or `}`", Some("struct")),
            errors,
        );

        Expr::AnonStruct(AnonStructExpr {
            span: Span::between(&open_paren.span, &close_paren),
            open_paren: open_paren.span,
            leading: Trivia::empty(),
            fields,
            trailing,
            close_paren,
        })
    }

    fn parse_fields_body_inner(
        &mut self,
        open_paren: Token<'a>,
        leading: Trivia<'a>,
    ) -> Result<Expr<'a>> {
        let mut errors = Vec::new();
        let expr = self.parse_fields_body_inner_impl(open_paren, leading, &mut errors);
        if errors.is_empty() {
            Ok(expr)
        } else {
            Err(errors.remove(0))
        }
    }

    /// Parse a fields body starting after opening paren with error recovery.
    fn parse_fields_body_inner_lossy(
        &mut self,
        open_paren: Token<'a>,
        leading: Trivia<'a>,
        errors: &mut Vec<Error>,
    ) -> Expr<'a> {
        self.parse_fields_body_inner_impl(open_paren, leading, errors)
    }

    /// Parse an identifier-starting expression (internal unified implementation).
    fn parse_ident_expr_inner(&mut self, errors: &mut Vec<Error>) -> Expr<'a> {
        let ident_tok = self.next_token();
        debug_assert_eq!(ident_tok.kind, TokenKind::Ident);

        match ident_tok.text {
            "true" => Expr::Bool(BoolExpr {
                span: ident_tok.span,
                value: true,
            }),
            "false" => Expr::Bool(BoolExpr {
                span: ident_tok.span,
                value: false,
            }),
            "None" => Expr::Option(Box::new(OptionExpr {
                span: ident_tok.span,
                value: None,
            })),
            "Some" => self.parse_some_inner(ident_tok, errors),
            "inf" | "NaN" => Expr::Number(NumberExpr {
                span: ident_tok.span,
                raw: Cow::Borrowed(ident_tok.text),
                kind: NumberKind::SpecialFloat,
            }),
            _ => self.parse_struct_or_variant_lossy(&ident_tok, errors),
        }
    }

    fn parse_ident_expr(&mut self) -> Result<Expr<'a>> {
        let mut errors = Vec::new();
        let expr = self.parse_ident_expr_inner(&mut errors);
        if errors.is_empty() {
            Ok(expr)
        } else {
            Err(errors.remove(0))
        }
    }

    /// Parse an identifier-starting expression with error recovery.
    fn parse_ident_expr_lossy(&mut self, errors: &mut Vec<Error>) -> Expr<'a> {
        self.parse_ident_expr_inner(errors)
    }

    /// Parse `Some(value)` (internal unified implementation).
    fn parse_some_inner(&mut self, some_tok: Token<'a>, errors: &mut Vec<Error>) -> Expr<'a> {
        if self.peek_kind() != TokenKind::LParen {
            let err = Self::error(some_tok.span, Self::expected("`Some` or `None`", None));
            return self.error_expr_from(err, errors);
        }
        let open_paren = self.next_token();

        let leading = self.collect_leading_trivia();
        let expr = self.parse_expr_lossy(errors);
        let trailing = self.collect_leading_trivia();

        let close_paren = self.consume_closing(
            TokenKind::RParen,
            Self::expected("closing `)`", Some("option")),
            errors,
        );

        Expr::Option(Box::new(OptionExpr {
            span: Span::between(&some_tok.span, &close_paren),
            value: Some(OptionValue {
                open_paren: open_paren.span,
                leading,
                expr,
                trailing,
                close_paren,
            }),
        }))
    }

    fn parse_some(&mut self, some_tok: Token<'a>) -> Result<Expr<'a>> {
        let mut errors = Vec::new();
        let expr = self.parse_some_inner(some_tok, &mut errors);
        if errors.is_empty() {
            Ok(expr)
        } else {
            Err(errors.remove(0))
        }
    }

    /// Parse `Some(value)` with error recovery.
    fn parse_some_lossy(&mut self, some_tok: Token<'a>, errors: &mut Vec<Error>) -> Expr<'a> {
        self.parse_some_inner(some_tok, errors)
    }

    /// Parse a struct or enum variant (internal unified implementation).
    fn parse_struct_or_variant_inner(
        &mut self,
        name_tok: &Token<'a>,
        errors: &mut Vec<Error>,
    ) -> Expr<'a> {
        let name = Ident {
            span: name_tok.span.clone(),
            name: Cow::Borrowed(name_tok.text),
        };

        let pre_body = self.collect_leading_trivia();

        let body = match self.peek_kind() {
            TokenKind::LParen => {
                let open_paren = self.next_token();
                let leading = self.collect_leading_trivia();

                match self.try_parse_empty_tuple_body(&open_paren.span, leading) {
                    Ok(tuple_body) => Some(StructBody::Tuple(tuple_body)),
                    Err(leading) => {
                        Some(self.parse_struct_body_contents_lossy(open_paren, leading, errors))
                    }
                }
            }
            _ => None,
        };

        let span = if let Some(ref b) = body {
            let end_span = match b {
                StructBody::Tuple(t) => &t.close_paren,
                StructBody::Fields(f) => &f.close_brace,
            };
            Span::between(&name_tok.span, end_span)
        } else {
            name_tok.span.clone()
        };

        Expr::Struct(StructExpr {
            span,
            name,
            pre_body,
            body,
        })
    }

    fn parse_struct_or_variant(&mut self, name_tok: &Token<'a>) -> Result<Expr<'a>> {
        let mut errors = Vec::new();
        let expr = self.parse_struct_or_variant_inner(name_tok, &mut errors);
        if errors.is_empty() {
            Ok(expr)
        } else {
            Err(errors.remove(0))
        }
    }

    /// Parse a struct or enum variant with error recovery.
    fn parse_struct_or_variant_lossy(
        &mut self,
        name_tok: &Token<'a>,
        errors: &mut Vec<Error>,
    ) -> Expr<'a> {
        self.parse_struct_or_variant_inner(name_tok, errors)
    }

    /// Parse the body contents of a named struct (internal unified implementation).
    fn parse_struct_body_contents_inner(
        &mut self,
        open_paren: Token<'a>,
        leading: Trivia<'a>,
        errors: &mut Vec<Error>,
    ) -> StructBody<'a> {
        if self.peek_kind() == TokenKind::Ident {
            let first_tok = self.next_token();
            let post_ident_trivia = self.collect_leading_trivia();

            if self.peek_kind() == TokenKind::Colon {
                let (fields, trailing) = self.parse_struct_fields_from_first_lossy(
                    leading,
                    &first_tok,
                    post_ident_trivia,
                    errors,
                );

                let close_paren = self.consume_closing(
                    TokenKind::RParen,
                    Self::expected("closing `)` or `}`", Some("struct")),
                    errors,
                );

                StructBody::Fields(FieldsBody {
                    open_brace: open_paren.span,
                    leading: Trivia::empty(),
                    fields,
                    trailing,
                    close_brace: close_paren,
                })
            } else {
                let first_expr =
                    self.ident_token_to_expr_lossy(first_tok, post_ident_trivia, errors);
                let (elements, trailing) =
                    self.parse_tuple_elements_from_first_lossy(leading, first_expr, errors);

                let close_paren = self.consume_closing(
                    TokenKind::RParen,
                    Self::expected("closing `)` or `}`", Some("struct")),
                    errors,
                );

                StructBody::Tuple(TupleBody {
                    open_paren: open_paren.span,
                    leading: Trivia::empty(),
                    elements,
                    trailing,
                    close_paren,
                })
            }
        } else {
            let elements = self.parse_tuple_elements_inner(leading, errors);
            let trailing = self.collect_leading_trivia();

            let close_paren = self.consume_closing(
                TokenKind::RParen,
                Self::expected("closing `)` or `}`", Some("struct")),
                errors,
            );

            StructBody::Tuple(TupleBody {
                open_paren: open_paren.span,
                leading: Trivia::empty(),
                elements,
                trailing,
                close_paren,
            })
        }
    }

    fn parse_struct_body_contents(
        &mut self,
        open_paren: Token<'a>,
        leading: Trivia<'a>,
    ) -> Result<Option<StructBody<'a>>> {
        let mut errors = Vec::new();
        let body = self.parse_struct_body_contents_inner(open_paren, leading, &mut errors);
        if errors.is_empty() {
            Ok(Some(body))
        } else {
            Err(errors.remove(0))
        }
    }

    /// Parse the body contents of a named struct with error recovery.
    fn parse_struct_body_contents_lossy(
        &mut self,
        open_paren: Token<'a>,
        leading: Trivia<'a>,
        errors: &mut Vec<Error>,
    ) -> StructBody<'a> {
        self.parse_struct_body_contents_inner(open_paren, leading, errors)
    }

    /// Convert an identifier token to an expression (internal unified implementation).
    fn ident_token_to_expr_inner(
        &mut self,
        tok: Token<'a>,
        pre_body: Trivia<'a>,
        errors: &mut Vec<Error>,
    ) -> Expr<'a> {
        match tok.text {
            "true" => Expr::Bool(BoolExpr {
                span: tok.span,
                value: true,
            }),
            "false" => Expr::Bool(BoolExpr {
                span: tok.span,
                value: false,
            }),
            "None" => Expr::Option(Box::new(OptionExpr {
                span: tok.span,
                value: None,
            })),
            "Some" => self.parse_some_inner(tok, errors),
            "inf" | "NaN" => Expr::Number(NumberExpr {
                span: tok.span,
                raw: Cow::Borrowed(tok.text),
                kind: NumberKind::SpecialFloat,
            }),
            _ => {
                let name = Ident {
                    span: tok.span.clone(),
                    name: Cow::Borrowed(tok.text),
                };

                let body = match self.peek_kind() {
                    TokenKind::LParen => {
                        let open_paren = self.next_token();
                        let leading = self.collect_leading_trivia();

                        match self.try_parse_empty_tuple_body(&open_paren.span, leading) {
                            Ok(tuple_body) => Some(StructBody::Tuple(tuple_body)),
                            Err(leading) => Some(
                                self.parse_struct_body_contents_inner(open_paren, leading, errors),
                            ),
                        }
                    }
                    _ => None,
                };

                let span = if let Some(ref b) = body {
                    let end_span = match b {
                        StructBody::Tuple(t) => &t.close_paren,
                        StructBody::Fields(f) => &f.close_brace,
                    };
                    Span::between(&tok.span, end_span)
                } else {
                    tok.span.clone()
                };

                Expr::Struct(StructExpr {
                    span,
                    name,
                    pre_body,
                    body,
                })
            }
        }
    }

    fn ident_token_to_expr(&mut self, tok: Token<'a>, pre_body: Trivia<'a>) -> Result<Expr<'a>> {
        let mut errors = Vec::new();
        let expr = self.ident_token_to_expr_inner(tok, pre_body, &mut errors);
        if errors.is_empty() {
            Ok(expr)
        } else {
            Err(errors.remove(0))
        }
    }

    /// Convert an identifier token to an expression with error recovery.
    fn ident_token_to_expr_lossy(
        &mut self,
        tok: Token<'a>,
        pre_body: Trivia<'a>,
        errors: &mut Vec<Error>,
    ) -> Expr<'a> {
        self.ident_token_to_expr_inner(tok, pre_body, errors)
    }

    /// Parse struct fields starting with the first field name already consumed (internal).
    #[allow(clippy::too_many_lines)]
    fn parse_struct_fields_from_first_inner(
        &mut self,
        leading: Trivia<'a>,
        first_name: &Token<'a>,
        pre_colon: Trivia<'a>,
        errors: &mut Vec<Error>,
    ) -> (Vec<StructField<'a>>, Trivia<'a>) {
        let mut fields = Vec::new();

        let colon_tok = self.next_token();
        debug_assert_eq!(colon_tok.kind, TokenKind::Colon);

        let post_colon = self.collect_leading_trivia();
        let value = self.parse_expr_inner(errors);
        let trailing = self.collect_leading_trivia();

        let (comma, has_comma) = self.consume_comma();

        fields.push(StructField {
            leading,
            name: Ident {
                span: first_name.span.clone(),
                name: Cow::Borrowed(first_name.text),
            },
            pre_colon,
            colon: colon_tok.span,
            post_colon,
            value,
            trailing,
            comma,
        });

        if !has_comma {
            return (fields, self.collect_leading_trivia());
        }

        let mut leading = self.collect_leading_trivia();

        loop {
            match self.peek_kind() {
                TokenKind::RParen | TokenKind::Eof => break,
                TokenKind::Ident => {}
                _ => {
                    let error_span = self.peek_span();
                    errors.push(Self::error(
                        error_span.clone(),
                        Self::expected("identifier", None),
                    ));
                    self.recover_until(&[TokenKind::Comma, TokenKind::RParen]);
                    let trailing = self.collect_leading_trivia();
                    let (comma, has_comma) = self.consume_comma();
                    // Create a placeholder field to preserve structure
                    fields.push(StructField {
                        leading,
                        name: Ident {
                            span: error_span.clone(),
                            name: Cow::Borrowed(""),
                        },
                        pre_colon: Trivia::empty(),
                        colon: Self::span_at_end(&error_span),
                        post_colon: Trivia::empty(),
                        value: Expr::Error(ErrorExpr {
                            span: error_span.clone(),
                            error: Error::with_span(Self::expected("identifier", None), error_span),
                        }),
                        trailing,
                        comma,
                    });
                    leading = self.collect_leading_trivia();
                    if !has_comma {
                        break;
                    }
                    continue;
                }
            }

            let name_tok = self.next_token();
            let pre_colon = self.collect_leading_trivia();

            let colon = if self.peek_kind() == TokenKind::Colon {
                self.next_token().span
            } else {
                errors.push(Self::error(
                    name_tok.span.clone(),
                    Self::expected("`:` after map key", Some("struct field")),
                ));
                Self::span_at_end(&name_tok.span)
            };

            let post_colon = self.collect_leading_trivia();
            let value = match self.peek_kind() {
                TokenKind::Comma | TokenKind::RParen => {
                    let error_span = Self::span_at_end(&colon);
                    let error_kind = Self::expected("value", Some("struct field"));
                    errors.push(Self::error(error_span.clone(), error_kind.clone()));
                    Expr::Error(ErrorExpr {
                        span: error_span.clone(),
                        error: Error::with_span(error_kind, error_span),
                    })
                }
                _ => self.parse_expr_inner(errors),
            };
            let trailing = self.collect_leading_trivia();

            let (comma, has_comma) = self.consume_comma();

            fields.push(StructField {
                leading,
                name: Ident {
                    span: name_tok.span.clone(),
                    name: Cow::Borrowed(name_tok.text),
                },
                pre_colon,
                colon,
                post_colon,
                value,
                trailing,
                comma,
            });

            leading = self.collect_leading_trivia();

            if !has_comma {
                if matches!(self.peek_kind(), TokenKind::Ident) {
                    errors.push(Self::error(
                        self.peek_span(),
                        Self::expected("comma", Some("struct")),
                    ));
                } else {
                    break;
                }
            }
        }

        (fields, leading)
    }

    fn parse_struct_fields_from_first(
        &mut self,
        leading: Trivia<'a>,
        first_name: &Token<'a>,
        pre_colon: Trivia<'a>,
    ) -> Result<(Vec<StructField<'a>>, Trivia<'a>)> {
        let mut errors = Vec::new();
        let result =
            self.parse_struct_fields_from_first_inner(leading, first_name, pre_colon, &mut errors);
        if errors.is_empty() {
            Ok(result)
        } else {
            Err(errors.remove(0))
        }
    }

    /// Parse struct fields starting with the first field name already consumed, with recovery.
    fn parse_struct_fields_from_first_lossy(
        &mut self,
        leading: Trivia<'a>,
        first_name: &Token<'a>,
        pre_colon: Trivia<'a>,
        errors: &mut Vec<Error>,
    ) -> (Vec<StructField<'a>>, Trivia<'a>) {
        self.parse_struct_fields_from_first_inner(leading, first_name, pre_colon, errors)
    }
}
