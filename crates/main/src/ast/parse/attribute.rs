//! Parsing for inner attributes (`#![...]`).

use alloc::{borrow::Cow, vec::Vec};

use super::{AstParser, core::expected};
use crate::{
    ast::{Attribute, AttributeArg, AttributeContent, Trivia},
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

    /// Parse a single attribute, keeping the trivia between its tokens.
    fn parse_attribute(&mut self, leading: Trivia<'a>) -> Result<Attribute<'a>> {
        let hash = self.next_token();
        let expected_attribute = || Error::with_span(expected("attribute", None), hash.span);

        let post_hash = self.take_trivia();
        self.eat(TokenKind::Bang).ok_or_else(expected_attribute)?;
        let post_bang = self.take_trivia();
        self.eat(TokenKind::LBracket)
            .ok_or_else(expected_attribute)?;
        let pre_name = self.take_trivia();

        if self.peek_kind() != TokenKind::Ident {
            return Err(Error::with_span(expected("identifier", None), hash.span));
        }
        let name = self.next_token();
        let post_name = self.take_trivia();

        let content = if self.eat(TokenKind::Eq).is_some() {
            let post_eq = self.take_trivia();
            if self.peek_kind() != TokenKind::String {
                return Err(Error::with_span(expected("string", None), name.span));
            }
            AttributeContent::Value {
                post_eq,
                value: Cow::Borrowed(self.next_token().text),
            }
        } else if self.eat(TokenKind::LParen).is_some() {
            self.parse_attribute_args()?
        } else {
            AttributeContent::None
        };
        let trailing = match content {
            AttributeContent::None => Trivia::empty(),
            _ => self.take_trivia(),
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
            post_hash,
            post_bang,
            pre_name,
            name: Cow::Borrowed(name.text),
            post_name,
            content,
            trailing,
        })
    }

    /// Parse `a, b)` after the `(` of an attribute: comma-separated identifiers.
    fn parse_attribute_args(&mut self) -> Result<AttributeContent<'a>> {
        let mut args = Vec::new();
        let mut leading = self.take_trivia();
        while self.eat(TokenKind::RParen).is_none() {
            if self.peek_kind() != TokenKind::Ident {
                return Err(Error::with_span(
                    expected("identifier", None),
                    self.peek_span(),
                ));
            }
            let name = Cow::Borrowed(self.next_token().text);
            let trailing = self.take_trivia();
            let comma = self.eat(TokenKind::Comma);
            args.push(AttributeArg {
                leading,
                name,
                trailing,
                comma,
            });
            leading = self.take_trivia();
            if comma.is_none() && self.peek_kind() != TokenKind::RParen {
                return Err(Error::with_span(
                    expected("comma", Some("attribute")),
                    self.peek_span(),
                ));
            }
        }
        Ok(AttributeContent::Args {
            args,
            trailing: leading,
        })
    }
}

#[cfg(test)]
#[allow(clippy::panic)]
mod tests {
    use alloc::{
        string::{String, ToString},
        vec,
        vec::Vec,
    };

    use super::super::{parse_document, parse_document_lossy};
    use crate::ast::{AttributeContent, serialize_document};

    #[test]
    fn parse_attribute() {
        let doc = parse_document(r"#![enable(unwrap_newtypes)] 42").unwrap();
        assert_eq!(doc.attributes.len(), 1);
        assert_eq!(doc.attributes[0].name, "enable");
        match &doc.attributes[0].content {
            AttributeContent::Args { args, .. } => {
                let names: Vec<_> = args.iter().map(|arg| &*arg.name).collect();
                assert_eq!(names, ["unwrap_newtypes"]);
            }
            _ => panic!("expected args"),
        }
    }

    /// The comments of each gap between an attribute's tokens.
    fn gaps(source: &str) -> Vec<Vec<String>> {
        let doc = parse_document(source).unwrap();
        doc.attributes[0]
            .inner_trivia()
            .map(|t| t.comments.iter().map(|c| c.text.to_string()).collect())
            .collect()
    }

    #[test]
    fn trivia_between_attribute_tokens_is_kept() {
        let c = |s: &str| vec![s.to_string()];
        assert_eq!(
            gaps("#/*a*/!/*b*/[/*c*/type/*d*/=/*e*/\"Foo\"/*f*/]\n42"),
            [
                c("/*a*/"),
                c("/*b*/"),
                c("/*c*/"),
                c("/*d*/"),
                c("/*e*/"),
                c("/*f*/")
            ]
        );
        // Per argument: leading and trailing; then the trivia after the last
        // comma, and after `)`.
        assert_eq!(
            gaps("#![enable/*n*/(/*a*/x/*b*/,/*c*/y,/*d*/)/*e*/]\n42"),
            [
                vec![],
                vec![],
                vec![],
                c("/*n*/"),
                c("/*a*/"),
                c("/*b*/"),
                c("/*c*/"),
                vec![],
                c("/*d*/"),
                c("/*e*/"),
            ]
        );
        // Without content, everything after the name is `post_name`.
        let doc = parse_document("#![name // c\n]\n42").unwrap();
        let attr = &doc.attributes[0];
        assert_eq!(attr.post_name.comments[0].text, "// c");
        assert!(attr.trailing.is_empty());
    }

    #[test]
    fn attribute_arguments_need_commas() {
        let (doc, errors) = parse_document_lossy("#![enable(a b)]\n42");
        assert_eq!(errors.len(), 1, "{errors:?}");
        assert_eq!(
            errors[0].to_string(),
            "1:13-1:14: Expected comma in attribute"
        );
        assert_eq!(doc.attributes, []);
        assert!(parse_document("#![enable(a, b,)]\n42").is_ok());
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
