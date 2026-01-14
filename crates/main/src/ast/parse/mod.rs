//! AST parser for RON documents.
//!
//! This module provides parsing of RON source into a full AST that preserves
//! all trivia (whitespace and comments) for round-trip editing.

mod attribute;
mod collection;
mod core;
mod literal;
mod struct_field;
mod tuple;

use alloc::{borrow::Cow, vec::Vec};

use ::core::iter::Peekable;

use self::{
    attribute::AttributeParser, collection::CollectionParser, core::ParserCore,
    literal::LiteralParser, struct_field::StructFieldParser, tuple::TupleParser,
};
#[cfg(test)]
use crate::ast::StructBody;
use crate::{
    ast::{Document, Expr},
    error::{Error, ErrorKind, Result},
    lexer::Lexer,
    token::{Token, TokenKind},
};

/// Maximum recursion depth for parsing nested structures.
const MAX_RECURSION_DEPTH: usize = 128;

/// Parse RON source into an AST document.
///
/// This is the main entry point for AST parsing. It returns a fully-formed
/// AST with all trivia preserved.
///
/// # Errors
///
/// Returns an error if the source contains invalid RON syntax.
pub fn parse_document(source: &str) -> Result<Document<'_>> {
    let lexer = Lexer::new(source).with_trivia(true);
    let mut parser = AstParser::new(source, lexer);
    parser.parse_document()
}

/// Parse RON source into an AST document with error recovery.
///
/// Returns the parsed document plus any errors encountered.
#[must_use]
pub fn parse_document_lossy(source: &str) -> (Document<'_>, Vec<Error>) {
    let lexer = Lexer::new(source).with_trivia(true);
    let mut parser = AstParser::new(source, lexer);
    parser.parse_document_lossy()
}

/// AST parser that consumes tokens and builds AST nodes.
struct AstParser<'a> {
    pub(super) source: &'a str,
    pub(super) tokens: Peekable<Lexer<'a>>,
    /// Buffer for collecting trivia before a token.
    pub(super) trivia_buffer: Vec<Token<'a>>,
    /// Lookahead buffer for tokens that were peeked but need to be returned.
    pub(super) lookahead: Vec<Token<'a>>,
    /// Current recursion depth for nested structures.
    depth: usize,
}

impl<'a> AstParser<'a> {
    fn new(source: &'a str, lexer: Lexer<'a>) -> Self {
        Self {
            source,
            tokens: lexer.peekable(),
            trivia_buffer: Vec::new(),
            lookahead: Vec::new(),
            depth: 0,
        }
    }

    /// Parse a complete document.
    /// Parse a complete document (internal unified implementation).
    fn parse_document_inner(&mut self, errors: &mut Vec<Error>) -> Document<'a> {
        let leading = self.collect_leading_trivia();
        let attributes = self.parse_attributes(errors);
        let pre_value = self.collect_leading_trivia();

        let value = match self.peek_kind() {
            TokenKind::Eof => None,
            _ => Some(self.parse_expr_inner(errors)),
        };

        let trailing = self.collect_leading_trivia();

        if self.peek_kind() != TokenKind::Eof {
            let tok = self.next_token();
            errors.push(Self::error(tok.span, ErrorKind::TrailingCharacters));
            self.recover_until(&[TokenKind::Eof]);
        }

        Document {
            source: Cow::Borrowed(self.source),
            leading,
            attributes,
            pre_value,
            value,
            trailing,
        }
    }

    fn parse_document(&mut self) -> Result<Document<'a>> {
        let mut errors = Vec::new();
        let doc = self.parse_document_inner(&mut errors);
        if errors.is_empty() {
            Ok(doc)
        } else {
            Err(errors.remove(0))
        }
    }

    /// Parse a complete document with error recovery.
    fn parse_document_lossy(&mut self) -> (Document<'a>, Vec<Error>) {
        let mut errors = Vec::new();
        let doc = self.parse_document_inner(&mut errors);
        (doc, errors)
    }

    /// Parse an expression.
    /// Parse an expression (internal unified implementation).
    fn parse_expr_inner(&mut self, errors: &mut Vec<Error>) -> Expr<'a> {
        self.depth += 1;

        // Check recursion limit
        if self.depth > MAX_RECURSION_DEPTH {
            let span = self.peek_span();
            let err = Self::error(span, ErrorKind::ExceededRecursionLimit);
            self.depth -= 1;
            return self.error_expr_from(err, errors);
        }

        let result = match self.peek_kind() {
            TokenKind::LParen => self.parse_tuple_or_unit(errors),
            TokenKind::LBracket => self.parse_seq(errors),
            TokenKind::LBrace => self.parse_map(errors),
            TokenKind::Ident => self.parse_ident_expr(errors),
            TokenKind::Integer => self.parse_integer(),
            TokenKind::Float => self.parse_float(),
            TokenKind::String => match self.parse_string() {
                Ok(expr) => expr,
                Err(err) => self.error_expr_from(err, errors),
            },
            TokenKind::ByteString => match self.parse_bytes() {
                Ok(expr) => expr,
                Err(err) => self.error_expr_from(err, errors),
            },
            TokenKind::Char => match self.parse_char() {
                Ok(expr) => expr,
                Err(err) => self.error_expr_from(err, errors),
            },
            TokenKind::Eof => {
                let err = Self::error(self.eof_span(), ErrorKind::Eof);
                self.error_expr_from(err, errors)
            }
            TokenKind::Error => {
                let tok = self.next_token();
                let err = Self::error_for_error_token(&tok);
                self.error_expr_from(err, errors)
            }
            _ => {
                let tok = self.next_token();
                let err = Self::error(
                    tok.span,
                    ErrorKind::UnexpectedChar(tok.text.chars().next().unwrap_or('?')),
                );
                self.error_expr_from(err, errors)
            }
        };

        self.depth -= 1;
        result
    }

    /// Parse an expression with error recovery.
    fn parse_expr_lossy(&mut self, errors: &mut Vec<Error>) -> Expr<'a> {
        self.parse_expr_inner(errors)
    }
}

#[cfg(test)]
#[allow(clippy::panic)]
mod tests {
    use super::*;

    #[test]
    fn parse_empty_document() {
        let doc = parse_document("").unwrap();
        assert!(doc.value.is_none());
        assert!(doc.attributes.is_empty());
    }

    #[test]
    fn parse_comment_only_document() {
        let doc = parse_document("// just a comment\n").unwrap();
        assert!(doc.value.is_none());
        assert!(!doc.leading.comments.is_empty());
    }

    #[test]
    fn parse_bool_true() {
        let doc = parse_document("true").unwrap();
        match doc.value {
            Some(Expr::Bool(b)) => assert!(b.value),
            _ => panic!("expected bool"),
        }
    }

    #[test]
    fn parse_bool_false() {
        let doc = parse_document("false").unwrap();
        match doc.value {
            Some(Expr::Bool(b)) => assert!(!b.value),
            _ => panic!("expected bool"),
        }
    }

    #[test]
    fn parse_none() {
        let doc = parse_document("None").unwrap();
        match doc.value {
            Some(Expr::Option(opt)) => assert!(opt.value.is_none()),
            _ => panic!("expected option"),
        }
    }

    #[test]
    fn parse_some() {
        let doc = parse_document("Some(42)").unwrap();
        match doc.value {
            Some(Expr::Option(opt)) => {
                assert!(opt.value.is_some());
            }
            _ => panic!("expected option"),
        }
    }

    #[test]
    fn parse_struct() {
        let doc = parse_document("Point(1, 2)").unwrap();
        match doc.value {
            Some(Expr::Struct(s)) => {
                assert_eq!(s.name.name, "Point");
                assert!(matches!(s.body, Some(StructBody::Tuple(_))));
            }
            _ => panic!("expected struct"),
        }
    }

    #[test]
    fn parse_brace_after_ident_is_separate_map() {
        // Braces after an identifier are NOT struct syntax - they're a separate map
        // "Point { x: 1 }" should fail because it parses as "Point" followed by a map,
        // but maps require string keys, not identifiers
        let result = parse_document("Point { x: 1 }");
        assert!(
            result.is_err(),
            "Brace syntax after ident should not be valid"
        );
    }

    #[test]
    fn parse_struct_with_fields_parens() {
        // RON standard syntax for named fields uses parens: Point(x: 1, y: 2)
        let doc = parse_document("Point(x: 1, y: 2)").unwrap();
        match doc.value {
            Some(Expr::Struct(s)) => {
                assert_eq!(s.name.name, "Point");
                match s.body {
                    Some(StructBody::Fields(f)) => {
                        assert_eq!(f.fields.len(), 2);
                        assert_eq!(f.fields[0].name.name, "x");
                        assert_eq!(f.fields[1].name.name, "y");
                    }
                    _ => panic!("expected fields body, got {:?}", s.body),
                }
            }
            _ => panic!("expected struct"),
        }
    }

    #[test]
    fn parse_preserves_trivia() {
        let source = "// header comment\n42 // inline comment";
        let doc = parse_document(source).unwrap();
        assert!(!doc.leading.comments.is_empty());
    }

    // =========================================================================
    // Anonymous struct parsing tests
    // =========================================================================

    #[test]
    fn parse_anon_struct_simple() {
        // Basic anonymous struct with named fields
        let doc = parse_document(r#"(name: "test", value: 42)"#).unwrap();
        match doc.value {
            Some(Expr::AnonStruct(s)) => {
                assert_eq!(s.fields.len(), 2);
                assert_eq!(s.fields[0].name.name, "name");
                assert_eq!(s.fields[1].name.name, "value");
            }
            _ => panic!("expected anonymous struct, got {:?}", doc.value),
        }
    }

    #[test]
    fn parse_anon_struct_single_field() {
        // Anonymous struct with a single field
        let doc = parse_document("(x: 1)").unwrap();
        match doc.value {
            Some(Expr::AnonStruct(s)) => {
                assert_eq!(s.fields.len(), 1);
                assert_eq!(s.fields[0].name.name, "x");
            }
            _ => panic!("expected anonymous struct, got {:?}", doc.value),
        }
    }

    #[test]
    fn parse_anon_struct_trailing_comma() {
        // Anonymous struct with trailing comma
        let doc = parse_document("(x: 1, y: 2,)").unwrap();
        match doc.value {
            Some(Expr::AnonStruct(s)) => {
                assert_eq!(s.fields.len(), 2);
                assert!(s.fields[1].comma.is_some());
            }
            _ => panic!("expected anonymous struct"),
        }
    }

    #[test]
    fn parse_anon_struct_nested_values() {
        // Anonymous struct with nested complex values
        let doc = parse_document(r"(items: [1, 2, 3], config: (enabled: true))").unwrap();
        match doc.value {
            Some(Expr::AnonStruct(s)) => {
                assert_eq!(s.fields.len(), 2);
                assert_eq!(s.fields[0].name.name, "items");
                assert_eq!(s.fields[1].name.name, "config");
                // Check that the nested value is also an anonymous struct
                match &s.fields[1].value {
                    Expr::AnonStruct(inner) => {
                        assert_eq!(inner.fields.len(), 1);
                        assert_eq!(inner.fields[0].name.name, "enabled");
                    }
                    _ => panic!("expected nested anonymous struct"),
                }
            }
            _ => panic!("expected anonymous struct"),
        }
    }

    #[test]
    fn parse_anon_struct_vs_single_element_tuple() {
        // `(x: 1)` should be anonymous struct (has colon)
        let anon = parse_document("(x: 1)").unwrap();
        assert!(matches!(anon.value, Some(Expr::AnonStruct(_))));

        // `(x)` should be tuple (no colon)
        let tuple = parse_document("(x)").unwrap();
        assert!(matches!(tuple.value, Some(Expr::Tuple(_))));
    }

    #[test]
    fn parse_anon_struct_with_whitespace() {
        // Anonymous struct with various whitespace
        let doc = parse_document("( x : 1 , y : 2 )").unwrap();
        match doc.value {
            Some(Expr::AnonStruct(s)) => {
                assert_eq!(s.fields.len(), 2);
                assert_eq!(s.fields[0].name.name, "x");
                assert_eq!(s.fields[1].name.name, "y");
            }
            _ => panic!("expected anonymous struct"),
        }
    }

    #[test]
    fn parse_anon_struct_multiline() {
        // Anonymous struct with newlines
        let doc = parse_document("(\n  x: 1,\n  y: 2\n)").unwrap();
        match doc.value {
            Some(Expr::AnonStruct(s)) => {
                assert_eq!(s.fields.len(), 2);
            }
            _ => panic!("expected anonymous struct"),
        }
    }

    #[test]
    fn parse_anon_struct_with_comments() {
        // Anonymous struct with comments
        let doc = parse_document("(\n  // comment\n  x: 1\n)").unwrap();
        match doc.value {
            Some(Expr::AnonStruct(s)) => {
                assert_eq!(s.fields.len(), 1);
                assert_eq!(s.fields[0].name.name, "x");
            }
            _ => panic!("expected anonymous struct"),
        }
    }

    #[test]
    fn parse_anon_struct_field_values() {
        // Verify various field value types are parsed correctly
        let doc = parse_document(
            r#"(
            bool_field: true,
            int_field: 42,
            str_field: "hello",
            option_field: Some(1),
            seq_field: [1, 2],
            map_field: {"a": 1}
        )"#,
        )
        .unwrap();
        match doc.value {
            Some(Expr::AnonStruct(s)) => {
                assert_eq!(s.fields.len(), 6);
                assert!(matches!(s.fields[0].value, Expr::Bool(_)));
                assert!(matches!(s.fields[1].value, Expr::Number(_)));
                assert!(matches!(s.fields[2].value, Expr::String(_)));
                assert!(matches!(s.fields[3].value, Expr::Option(_)));
                assert!(matches!(s.fields[4].value, Expr::Seq(_)));
                assert!(matches!(s.fields[5].value, Expr::Map(_)));
            }
            _ => panic!("expected anonymous struct"),
        }
    }

    // =========================================================================
    // Lossy parsing tests (error recovery)
    // =========================================================================

    #[test]
    fn lossy_missing_closing_paren() {
        let (doc, errors) = parse_document_lossy("(x: 1, y: 2");
        assert_eq!(errors.len(), 1);
        match &doc.value {
            Some(Expr::AnonStruct(s)) => {
                assert_eq!(s.fields.len(), 2);
                assert_eq!(s.fields[0].name.name, "x");
                assert_eq!(s.fields[1].name.name, "y");
            }
            _ => panic!("expected anonymous struct"),
        }
    }

    #[test]
    fn lossy_missing_closing_bracket() {
        let (doc, errors) = parse_document_lossy("[1, 2, 3");
        assert_eq!(errors.len(), 1);
        match &doc.value {
            Some(Expr::Seq(seq)) => {
                assert_eq!(seq.items.len(), 3);
            }
            _ => panic!("expected sequence"),
        }
    }

    #[test]
    fn lossy_missing_closing_brace() {
        let (doc, errors) = parse_document_lossy(r#"{"a": 1"#);
        assert_eq!(errors.len(), 1);
        match &doc.value {
            Some(Expr::Map(map)) => {
                assert_eq!(map.entries.len(), 1);
            }
            _ => panic!("expected map"),
        }
    }

    #[test]
    fn lossy_missing_comma_in_struct() {
        let (doc, errors) = parse_document_lossy("(x: 1 y: 2)");
        assert_eq!(errors.len(), 1);
        match &doc.value {
            Some(Expr::AnonStruct(s)) => {
                assert_eq!(s.fields.len(), 2);
            }
            _ => panic!("expected anonymous struct"),
        }
    }

    #[test]
    fn lossy_invalid_token_in_struct() {
        let (doc, errors) = parse_document_lossy("(x: @, y: 2)");
        assert_eq!(errors.len(), 1);
        match &doc.value {
            Some(Expr::AnonStruct(s)) => {
                assert_eq!(s.fields.len(), 2);
                assert!(matches!(s.fields[0].value, Expr::Error(_)));
                assert!(matches!(s.fields[1].value, Expr::Number(_)));
            }
            _ => panic!("expected anonymous struct"),
        }
    }

    #[test]
    fn lossy_invalid_field_name() {
        // Input starts with a number, so parser sees this as a tuple first.
        // "y: 2" in tuple context will cause a colon error.
        let (doc, errors) = parse_document_lossy("(123, y: 2)");
        // Errors occur because "y: 2" looks like a map entry in tuple context
        assert!(!errors.is_empty());
        match &doc.value {
            Some(Expr::Tuple(t)) => {
                // First element is the number, second involves recovery
                assert!(!t.elements.is_empty());
            }
            _ => panic!("expected tuple, got {:?}", doc.value),
        }
    }

    #[test]
    fn lossy_invalid_token_as_field_name() {
        // @ is not an identifier, so parser can't know it's meant to be a field.
        // It parses as a tuple with error elements.
        let (doc, errors) = parse_document_lossy("(@: 1, y: 2)");
        assert!(!errors.is_empty());
        match &doc.value {
            Some(Expr::Tuple(t)) => {
                // Contains error expressions for invalid tokens
                assert!(!t.elements.is_empty());
            }
            _ => panic!("expected tuple, got {:?}", doc.value),
        }
    }

    #[test]
    fn lossy_invalid_token_in_existing_struct() {
        // Start with a valid field so parser knows it's a struct,
        // then encounter an invalid token where a field name is expected.
        let (doc, errors) = parse_document_lossy("(x: 1, @: 2, y: 3)");
        assert!(!errors.is_empty());
        match &doc.value {
            Some(Expr::AnonStruct(s)) => {
                // Should have 3 fields: x, placeholder for @, and y
                assert_eq!(s.fields.len(), 3);
                assert_eq!(s.fields[0].name.name, "x");
                assert_eq!(s.fields[1].name.name, ""); // placeholder
                assert_eq!(s.fields[2].name.name, "y");
            }
            _ => panic!("expected anonymous struct, got {:?}", doc.value),
        }
    }

    #[test]
    fn lossy_multiple_errors() {
        let (doc, errors) = parse_document_lossy("(x: @, y: #)");
        assert_eq!(errors.len(), 2);
        match &doc.value {
            Some(Expr::AnonStruct(s)) => {
                assert_eq!(s.fields.len(), 2);
                assert!(matches!(s.fields[0].value, Expr::Error(_)));
                assert!(matches!(s.fields[1].value, Expr::Error(_)));
            }
            _ => panic!("expected anonymous struct"),
        }
    }

    #[test]
    fn lossy_nested_recovery() {
        let (doc, errors) = parse_document_lossy("(x: [1, @, 3], y: 2)");
        assert_eq!(errors.len(), 1);
        match &doc.value {
            Some(Expr::AnonStruct(s)) => {
                assert_eq!(s.fields.len(), 2);
                match &s.fields[0].value {
                    Expr::Seq(seq) => {
                        assert_eq!(seq.items.len(), 3);
                        assert!(matches!(seq.items[1].expr, Expr::Error(_)));
                    }
                    _ => panic!("expected sequence"),
                }
            }
            _ => panic!("expected anonymous struct"),
        }
    }

    #[test]
    fn lossy_missing_value_after_colon() {
        let (doc, errors) = parse_document_lossy("(x:, y: 2)");
        assert_eq!(errors.len(), 1);
        match &doc.value {
            Some(Expr::AnonStruct(s)) => {
                assert_eq!(s.fields.len(), 2);
                assert!(matches!(s.fields[0].value, Expr::Error(_)));
                assert!(matches!(s.fields[1].value, Expr::Number(_)));
            }
            _ => panic!("expected anonymous struct"),
        }
    }

    #[test]
    fn lossy_valid_document_no_errors() {
        let (doc, errors) = parse_document_lossy("(x: 1, y: 2)");
        assert!(errors.is_empty());
        match &doc.value {
            Some(Expr::AnonStruct(s)) => {
                assert_eq!(s.fields.len(), 2);
            }
            _ => panic!("expected anonymous struct"),
        }
    }

    #[test]
    fn lossy_preserves_attributes() {
        let (doc, errors) = parse_document_lossy(r#"#![type = "foo::Bar"] (x: 1"#);
        assert_eq!(errors.len(), 1); // missing closing paren
        assert_eq!(doc.attributes.len(), 1);
        assert_eq!(doc.attributes[0].name, "type");
    }

    #[test]
    fn test_recursion_limit() {
        // Create deeply nested structure that exceeds the recursion limit
        let deep = "[".repeat(200) + &"]".repeat(200);
        let result = parse_document(&deep);
        assert!(result.is_err());
        assert!(matches!(
            result.unwrap_err().kind(),
            ErrorKind::ExceededRecursionLimit
        ));
    }

    #[test]
    fn test_recursion_limit_exact() {
        // 128 levels should work (at the limit)
        let at_limit = "[".repeat(128) + &"]".repeat(128);
        let result = parse_document(&at_limit);
        assert!(result.is_ok());

        // 129 levels should fail (exceeds limit)
        let over_limit = "[".repeat(129) + &"]".repeat(129);
        let result = parse_document(&over_limit);
        assert!(result.is_err());
        assert!(matches!(
            result.unwrap_err().kind(),
            ErrorKind::ExceededRecursionLimit
        ));
    }
}
