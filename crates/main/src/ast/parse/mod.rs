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

use alloc::{borrow::Cow, collections::VecDeque, vec::Vec};

use crate::{
    ast::{Document, Expr},
    error::{Error, ErrorKind, Result, Span},
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
/// Returns the first error if the source contains invalid RON syntax.
pub fn parse_document(source: &str) -> Result<Document<'_>> {
    let mut errors = Vec::new();
    let doc = AstParser::new(source).parse_document(&mut errors);
    match errors.into_iter().next() {
        None => Ok(doc),
        Some(err) => Err(err),
    }
}

/// Parse RON source into an AST document with error recovery.
///
/// Returns the parsed document plus all errors encountered. Invalid parts of
/// the document are represented by [`Expr::Error`] nodes.
#[must_use]
pub fn parse_document_lossy(source: &str) -> (Document<'_>, Vec<Error>) {
    let mut errors = Vec::new();
    let doc = AstParser::new(source).parse_document(&mut errors);
    (doc, errors)
}

/// AST parser that consumes tokens and builds AST nodes.
///
/// It looks at one significant (non-trivia) token, `peeked`. Trivia before it
/// is buffered in `trivia` until a node takes it; `peek_second_kind` reads
/// further raw tokens into `ahead` for the one place that needs two tokens of
/// lookahead (`ident:` field detection).
struct AstParser<'a> {
    source: &'a str,
    lexer: Lexer<'a>,
    /// The next non-trivia token.
    peeked: Token<'a>,
    /// Trivia tokens before `peeked`.
    trivia: Vec<Token<'a>>,
    /// Raw tokens after `peeked` that were read ahead.
    ahead: VecDeque<Token<'a>>,
    /// Current recursion depth for nested expressions.
    depth: usize,
    /// Number of open containers per [`core::Delim`] awaiting their closer.
    open: [usize; 3],
}

impl<'a> AstParser<'a> {
    fn new(source: &'a str) -> Self {
        let mut parser = Self {
            source,
            lexer: Lexer::new(source),
            peeked: Token {
                kind: TokenKind::Eof,
                text: "",
                span: Span::default(),
            },
            trivia: Vec::new(),
            ahead: VecDeque::new(),
            depth: 0,
            open: [0; 3],
        };
        parser.peeked = parser.read_significant();
        parser
    }

    fn parse_document(&mut self, errors: &mut Vec<Error>) -> Document<'a> {
        let leading = self.take_trivia();
        let attributes = self.parse_attributes(errors);
        let pre_value = self.take_trivia();
        let value = (self.peek_kind() != TokenKind::Eof).then(|| self.parse_expr(errors));
        let trailing = self.take_trivia();

        if self.peek_kind() != TokenKind::Eof {
            errors.push(self.unexpected(&self.peeked, ErrorKind::TrailingCharacters));
            self.skip_until(&[]);
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

    /// Parse an expression; on errors, returns [`Expr::Error`] and recovers.
    fn parse_expr(&mut self, errors: &mut Vec<Error>) -> Expr<'a> {
        if self.depth >= MAX_RECURSION_DEPTH {
            let err = Error::with_span(ErrorKind::ExceededRecursionLimit, self.peek_span());
            return self.error_expr(err, errors);
        }
        self.depth += 1;

        let expr = match self.peek_kind() {
            TokenKind::LParen => self.parse_paren(errors),
            TokenKind::LBracket => self.parse_seq(errors),
            TokenKind::LBrace => self.parse_map(errors),
            TokenKind::Ident => self.parse_ident_expr(errors),
            TokenKind::Integer | TokenKind::Float => self.parse_number(),
            TokenKind::String | TokenKind::ByteString | TokenKind::Char | TokenKind::Byte => {
                self.parse_text_literal(errors)
            }
            TokenKind::Eof => {
                let err = Error::with_span(ErrorKind::Eof, self.peek_span());
                self.error_expr(err, errors)
            }
            _ => {
                let tok = self.next_token();
                let c = tok.text.chars().next().unwrap_or_default();
                let err = self.unexpected(&tok, ErrorKind::UnexpectedChar(c));
                self.error_expr(err, errors)
            }
        };

        self.depth -= 1;
        expr
    }

    /// Parse the value after a `:` or inside `Some(...)`, reporting a missing
    /// value without consuming the comma or closer that follows.
    fn parse_value(&mut self, context: &'static str, errors: &mut Vec<Error>) -> Expr<'a> {
        match self.peek_kind() {
            TokenKind::Comma | TokenKind::RParen | TokenKind::RBracket | TokenKind::RBrace => {
                self.missing_value(context, errors)
            }
            _ => self.parse_expr(errors),
        }
    }
}

#[cfg(test)]
#[allow(clippy::panic)]
mod tests {
    use super::*;
    use crate::ast::StructBody;

    #[test]
    fn parse_empty_document() {
        let doc = parse_document("").unwrap();
        assert!(doc.value.is_none());
        assert!(
            doc.attributes.is_empty(),
            "unexpected attributes: {:?}",
            doc.attributes
        );
    }

    #[test]
    fn parse_comment_only_document() {
        let doc = parse_document("// just a comment\n").unwrap();
        assert!(doc.value.is_none());
        let comments: Vec<_> = doc.leading.comments.iter().map(|c| &*c.text).collect();
        assert_eq!(comments, ["// just a comment"]);
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
                let inner = opt.value.expect("Some value");
                assert!(matches!(&inner.expr, Expr::Number(n) if n.raw == "42"));
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
        let leading: Vec<_> = doc.leading.comments.iter().map(|c| &*c.text).collect();
        assert_eq!(leading, ["// header comment"]);
        let trailing: Vec<_> = doc.trailing.comments.iter().map(|c| &*c.text).collect();
        assert_eq!(trailing, ["// inline comment"]);
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
        let kinds: Vec<_> = errors.iter().map(Error::kind).collect();
        assert_eq!(
            kinds,
            [
                &ErrorKind::Expected {
                    expected: "comma".into(),
                    context: Some("tuple")
                },
                &ErrorKind::UnexpectedChar(':'),
            ]
        );
        match &doc.value {
            Some(Expr::Tuple(t)) => {
                // First element is the number, second involves recovery
                assert_eq!(t.elements.len(), 3);
                assert!(matches!(&t.elements[0].expr, Expr::Number(n) if n.raw == "123"));
                assert!(matches!(t.elements[2].expr, Expr::Error(_)));
            }
            _ => panic!("expected tuple, got {:?}", doc.value),
        }
    }

    #[test]
    fn lossy_invalid_token_as_field_name() {
        // @ is not an identifier, so parser can't know it's meant to be a field.
        // It parses as a tuple with error elements.
        let (doc, errors) = parse_document_lossy("(@: 1, y: 2)");
        assert_eq!(errors.len(), 3, "{errors:?}");
        assert_eq!(errors[0].kind(), &ErrorKind::UnexpectedChar('@'));
        match &doc.value {
            Some(Expr::Tuple(t)) => {
                // Contains error expressions for invalid tokens
                assert_eq!(t.elements.len(), 3);
                assert!(matches!(t.elements[0].expr, Expr::Error(_)));
                assert!(matches!(t.elements[2].expr, Expr::Error(_)));
            }
            _ => panic!("expected tuple, got {:?}", doc.value),
        }
    }

    #[test]
    fn lossy_invalid_token_in_existing_struct() {
        // Start with a valid field so parser knows it's a struct,
        // then encounter an invalid token where a field name is expected.
        let (doc, errors) = parse_document_lossy("(x: 1, @: 2, y: 3)");
        assert_eq!(errors.len(), 1, "{errors:?}");
        assert_eq!(errors[0].kind(), &ErrorKind::UnexpectedChar('@'));
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
        assert!(errors.is_empty(), "unexpected errors: {errors:?}");
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

    // =========================================================================
    // Trivia round-trips (serialize_document must reproduce the source)
    // =========================================================================

    fn assert_round_trip(source: &str) {
        let doc = parse_document(source).unwrap();
        let output = crate::ast::serialize_document(&doc).unwrap();
        assert_eq!(output, source);
    }

    #[test]
    fn trivia_after_trailing_comma_round_trips() {
        // The comment after the last comma used to be dropped.
        for source in [
            "[\n 1,\n 2, // two\n]",
            "{\n \"a\": 1, // one\n}",
            "(\n x: 1, // one\n)",
            "(\n 1,\n 2, // two\n)",
            "Foo(\n 1, // one\n)",
            "Foo(\n x: 1, // one\n)",
            "[1, /* a */ 2 /* b */, /* c */]",
        ] {
            assert_round_trip(source);
        }
    }

    #[test]
    fn trivia_in_empty_containers_round_trips() {
        for source in ["[ /* c */ ]", "{ // c\n}", "Foo( /* c */ )", "( )"] {
            assert_round_trip(source);
        }
    }

    #[test]
    fn unit_stores_inner_trivia() {
        let doc = parse_document("( /* c */ )").unwrap();
        match doc.value {
            Some(Expr::Unit(u)) => {
                assert_eq!(u.inner.comments.len(), 1);
                assert_eq!(u.inner.comments[0].text, "/* c */");
                assert_eq!(u.inner.whitespace, "  ");
            }
            _ => panic!("expected unit"),
        }
    }

    #[test]
    fn trivia_whitespace_covers_all_whitespace() {
        // `whitespace` used to hold only the first whitespace run.
        let doc = parse_document(" \n/* a */\t// b\n  1").unwrap();
        assert_eq!(doc.leading.whitespace, " \n\t\n  ");
        assert_eq!(doc.leading.comments.len(), 2);
        let doc = parse_document("  1").unwrap();
        assert!(matches!(doc.leading.whitespace, Cow::Borrowed("  ")));
    }

    #[test]
    fn trailing_comment_is_in_container_trailing() {
        let doc = parse_document("[\n 1,\n 2, // two\n]").unwrap();
        match doc.value {
            Some(Expr::Seq(seq)) => {
                assert_eq!(seq.trailing.comments.len(), 1);
                assert_eq!(seq.trailing.comments[0].text, "// two");
            }
            _ => panic!("expected seq"),
        }
    }

    // =========================================================================
    // Error recovery always terminates
    // =========================================================================

    #[test]
    fn mismatched_closer_at_recursion_limit() {
        // Used to loop forever: past the depth limit nothing was consumed and the
        // container kept reporting a missing comma before the foreign closer.
        let source = "[".repeat(128) + ")";
        let (_, errors) = parse_document_lossy(&source);
        // The stray `)`, then one missing `]` per array.
        assert_eq!(errors.len(), 129);
        assert_eq!(errors[0].kind(), &ErrorKind::UnexpectedChar(')'));
        assert!(parse_document(&source).is_err());
    }

    #[test]
    fn mismatched_closer_skipped_when_unexpected() {
        let (doc, errors) = parse_document_lossy("[1 ) 2]");
        assert_eq!(errors.len(), 2, "{errors:?}"); // stray `)` and missing comma
        match doc.value {
            Some(Expr::Seq(seq)) => assert_eq!(seq.items.len(), 2),
            _ => panic!("expected seq"),
        }
    }

    #[test]
    fn mismatched_closer_closes_outer_container() {
        let (doc, errors) = parse_document_lossy("[(1, 2], 3");
        // Missing `)`, then the `]` closes the array and `, 3` is trailing input.
        assert_eq!(errors.len(), 2, "{errors:?}");
        match doc.value {
            Some(Expr::Seq(seq)) => {
                assert_eq!(seq.items.len(), 1);
                assert!(matches!(&seq.items[0].expr, Expr::Tuple(t) if t.elements.len() == 2));
            }
            _ => panic!("expected seq"),
        }
    }
}
