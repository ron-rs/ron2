//! Parsing for inner attributes (#![...]).

use alloc::{borrow::Cow, vec::Vec};

use super::{AstParser, core::ParserCore};
use crate::{
    ast::{Attribute, AttributeContent, Trivia},
    error::{Error, Result, Span},
    token::TokenKind,
};

/// Internal trait for parsing attributes.
pub(super) trait AttributeParser<'a>: ParserCore<'a> {
    fn parse_attributes_inner(&mut self, errors: &mut Vec<Error>) -> Vec<Attribute<'a>>;
    fn parse_attributes(&mut self) -> Result<Vec<Attribute<'a>>>;
    fn parse_attributes_lossy(&mut self, errors: &mut Vec<Error>) -> Vec<Attribute<'a>>;
    fn parse_attribute(&mut self, leading: Trivia<'a>) -> Result<Attribute<'a>>;
}

impl<'a> AttributeParser<'a> for AstParser<'a> {
    /// Parse inner attributes (`#![...]`) (internal unified implementation).
    fn parse_attributes_inner(&mut self, errors: &mut Vec<Error>) -> Vec<Attribute<'a>> {
        let mut attributes = Vec::new();

        while self.peek_kind() == TokenKind::Hash {
            let leading = self.drain_trivia();
            match self.parse_attribute(leading) {
                Ok(attr) => attributes.push(attr),
                Err(err) => {
                    errors.push(err);
                    self.recover_until(&[TokenKind::RBracket, TokenKind::Eof]);
                    if self.peek_kind() == TokenKind::RBracket {
                        let _ = self.next_token();
                    }
                }
            }
        }

        attributes
    }

    fn parse_attributes(&mut self) -> Result<Vec<Attribute<'a>>> {
        let mut errors = Vec::new();
        let attrs = self.parse_attributes_inner(&mut errors);
        if errors.is_empty() {
            Ok(attrs)
        } else {
            Err(errors.remove(0))
        }
    }

    /// Parse inner attributes (`#![...]`) with error recovery.
    fn parse_attributes_lossy(&mut self, errors: &mut Vec<Error>) -> Vec<Attribute<'a>> {
        self.parse_attributes_inner(errors)
    }

    /// Parse a single attribute.
    fn parse_attribute(&mut self, leading: Trivia<'a>) -> Result<Attribute<'a>> {
        let hash = self.next_token();
        debug_assert_eq!(hash.kind, TokenKind::Hash);

        // Expect `!`
        if self.peek_kind() != TokenKind::Bang {
            return Err(Self::error(hash.span, Self::expected("attribute", None)));
        }
        let _bang = self.next_token();

        // Expect `[`
        if self.peek_kind() != TokenKind::LBracket {
            return Err(Self::error(hash.span, Self::expected("attribute", None)));
        }
        let _lbracket = self.next_token();

        // Expect identifier
        if self.peek_kind() != TokenKind::Ident {
            return Err(Self::error(hash.span, Self::expected("identifier", None)));
        }
        let name_tok = self.next_token();
        let name = name_tok.text;

        // Parse content based on what follows
        let content = match self.peek_kind() {
            TokenKind::Eq => {
                let _eq = self.next_token();
                // Expect string value
                if self.peek_kind() != TokenKind::String {
                    return Err(Self::error(name_tok.span, Self::expected("string", None)));
                }
                let value_tok = self.next_token();
                AttributeContent::Value(Cow::Borrowed(value_tok.text))
            }
            TokenKind::LParen => {
                let _lparen = self.next_token();
                let mut args = Vec::new();

                // Parse comma-separated identifiers
                loop {
                    match self.peek_kind() {
                        TokenKind::RParen => break,
                        TokenKind::Ident => {
                            let arg_tok = self.next_token();
                            args.push(Cow::Borrowed(arg_tok.text));

                            // Check for comma
                            if self.peek_kind() == TokenKind::Comma {
                                let _comma = self.next_token();
                            }
                        }
                        _ => {
                            let tok = self.next_token();
                            return Err(Self::error(tok.span, Self::expected("identifier", None)));
                        }
                    }
                }

                // Expect `)`
                if self.peek_kind() != TokenKind::RParen {
                    return Err(Self::error(
                        name_tok.span,
                        Self::expected("closing `]`", Some("attribute")),
                    ));
                }
                let _rparen = self.next_token();

                AttributeContent::Args(args)
            }
            _ => AttributeContent::None,
        };

        // Expect `]`
        if self.peek_kind() != TokenKind::RBracket {
            return Err(Self::error(
                hash.span,
                Self::expected("closing `]`", Some("attribute")),
            ));
        }
        let rbracket = self.next_token();

        Ok(Attribute {
            span: Span::between(&hash.span, &rbracket.span),
            leading,
            name: Cow::Borrowed(name),
            content,
        })
    }
}

#[cfg(test)]
#[allow(clippy::panic)]
mod tests {
    use super::super::parse_document;
    use crate::ast::AttributeContent;

    #[test]
    fn parse_attribute() {
        let doc = parse_document(r"#![enable(unwrap_newtypes)] 42").unwrap();
        assert_eq!(doc.attributes.len(), 1);
        assert_eq!(doc.attributes[0].name, "enable");
        match &doc.attributes[0].content {
            AttributeContent::Args(args) => {
                assert_eq!(args, &["unwrap_newtypes"]);
            }
            _ => panic!("expected args"),
        }
    }
}
