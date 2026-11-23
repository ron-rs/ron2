//! Parsing for inner attributes (`#![...]`).

use alloc::{borrow::Cow, vec::Vec};

use super::{AstParser, core::expected};
use crate::{
    ast::{Attribute, AttributeContent, Trivia},
    error::{Error, Result, Span},
    token::TokenKind,
};

impl<'a> AstParser<'a> {
    /// Parse inner attributes (`#![...]`), skipping to the next `]` after an error.
    pub(super) fn parse_attributes(&mut self, errors: &mut Vec<Error>) -> Vec<Attribute<'a>> {
        let mut attributes = Vec::new();

        while self.peek_kind() == TokenKind::Hash {
            let leading = self.take_trivia();
            match self.parse_attribute(leading) {
                Ok(attr) => attributes.push(attr),
                Err(err) => {
                    errors.push(err);
                    self.skip_until(&[TokenKind::RBracket]);
                    self.eat(TokenKind::RBracket);
                }
            }
        }

        attributes
    }

    /// Parse a single attribute. Its text is reproduced from its span, so
    /// trivia inside it is not kept separately.
    fn parse_attribute(&mut self, leading: Trivia<'a>) -> Result<Attribute<'a>> {
        let hash = self.next_token();

        if self.eat(TokenKind::Bang).is_none() || self.eat(TokenKind::LBracket).is_none() {
            return Err(Error::with_span(expected("attribute", None), hash.span));
        }

        if self.peek_kind() != TokenKind::Ident {
            return Err(Error::with_span(expected("identifier", None), hash.span));
        }
        let name = self.next_token();

        let content = if self.eat(TokenKind::Eq).is_some() {
            if self.peek_kind() != TokenKind::String {
                return Err(Error::with_span(expected("string", None), name.span));
            }
            AttributeContent::Value(Cow::Borrowed(self.next_token().text))
        } else if self.eat(TokenKind::LParen).is_some() {
            let mut args = Vec::new();
            while self.eat(TokenKind::RParen).is_none() {
                if self.peek_kind() != TokenKind::Ident {
                    return Err(Error::with_span(
                        expected("identifier", None),
                        self.peek_span(),
                    ));
                }
                args.push(Cow::Borrowed(self.next_token().text));
                self.eat(TokenKind::Comma);
            }
            AttributeContent::Args(args)
        } else {
            AttributeContent::None
        };

        let Some(close) = self.eat(TokenKind::RBracket) else {
            return Err(Error::with_span(
                expected("closing `]`", Some("attribute")),
                hash.span,
            ));
        };

        Ok(Attribute {
            span: Span::between(&hash.span, &close),
            leading,
            name: Cow::Borrowed(name.text),
            content,
        })
    }
}

#[cfg(test)]
#[allow(clippy::panic)]
mod tests {
    use super::super::{parse_document, parse_document_lossy};
    use crate::ast::{AttributeContent, serialize_document};

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

    #[test]
    fn attribute_with_inner_whitespace_round_trips() {
        // Trivia inside the attribute used to leak into the following trivia span,
        // duplicating part of the attribute on serialization.
        let source = "#![type = \"a::B\"]\n#![enable( x , y )]\n\n42\n";
        let doc = parse_document(source).unwrap();
        assert_eq!(serialize_document(&doc).unwrap(), source);
        let attr_end = doc.attributes[1].span.end_offset;
        let pre_value = doc.pre_value.span.unwrap();
        assert_eq!(pre_value.start_offset, attr_end);
        assert_eq!(pre_value.slice(source), "\n\n");

        let source = "#![ type = \"x\" ]\n42";
        let doc = parse_document(source).unwrap();
        assert_eq!(serialize_document(&doc).unwrap(), source);
        assert_eq!(doc.pre_value.span.unwrap().slice(source), "\n");
    }

    #[test]
    fn invalid_attribute_recovers() {
        let (doc, errors) = parse_document_lossy("#![enable(1)]\n#![type = \"x\"]\n42");
        assert_eq!(errors.len(), 1, "{errors:?}");
        assert_eq!(doc.attributes.len(), 1);
        assert!(doc.value.is_some());
    }
}
