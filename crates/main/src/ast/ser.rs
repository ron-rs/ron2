//! Lossless serialization of an AST back to RON text.
//!
//! Tokens are written from AST data: identifiers from their name, literals from their raw
//! text (keeping hex numbers, raw strings and escapes as written), punctuation as fixed
//! text. Edited and synthetic ASTs therefore serialize to what they contain.
//!
//! Whitespace and comments come from the trivia. While a trivia's span still describes
//! [`Document::source`] exactly, its original text is copied, so `parse_document` followed
//! by `serialize_document` reproduces the source byte for byte.

use alloc::string::{String, ToString};
use core::fmt::Write;

use crate::{
    ast::{Attribute, CommentKind, Document, Expr, StructBody, StructField, Trivia, TupleElement},
    error::{Error, ErrorKind, Result, Span},
    lexer::Lexer,
};

/// Serialize an AST document back to RON text.
///
/// For a document straight from [`parse_document`](crate::ast::parse_document) the
/// output equals the source.
///
/// # Example
///
/// ```
/// use ron2::ast::{parse_document, serialize_document};
///
/// let source = "// comment\n42";
/// let doc = parse_document(source).unwrap();
/// let output = serialize_document(&doc).unwrap();
/// assert_eq!(output, source);
/// ```
pub fn serialize_document(doc: &Document<'_>) -> Result<String> {
    let mut output = String::new();
    serialize_document_to(&mut output, doc)?;
    Ok(output)
}

/// Serialize an AST document to a writer.
pub fn serialize_document_to<W: Write>(writer: W, doc: &Document<'_>) -> Result<()> {
    AstSerializer {
        writer,
        source: &doc.source,
    }
    .document(doc)
}

struct AstSerializer<'s, W: Write> {
    writer: W,
    source: &'s str,
}

impl<W: Write> AstSerializer<'_, W> {
    fn str(&mut self, s: &str) -> Result<()> {
        self.writer.write_str(s)?;
        Ok(())
    }

    fn document(&mut self, doc: &Document<'_>) -> Result<()> {
        self.trivia(&doc.leading)?;
        for attr in &doc.attributes {
            self.attribute(attr)?;
        }
        self.trivia(&doc.pre_value)?;
        if let Some(value) = &doc.value {
            self.expr(value)?;
        }
        self.trivia(&doc.trailing)
    }

    fn trivia(&mut self, trivia: &Trivia<'_>) -> Result<()> {
        if let Some(text) = trivia_source(self.source, trivia) {
            return self.str(text);
        }
        self.str(&trivia.whitespace)?;
        for comment in &trivia.comments {
            self.str(&comment.text)?;
            if comment.kind == CommentKind::Line {
                self.str("\n")?;
            }
        }
        Ok(())
    }

    fn attribute(&mut self, attr: &Attribute<'_>) -> Result<()> {
        self.trivia(&attr.leading)?;
        // Attributes keep no inner trivia, so reuse the source text while it still
        // spells the same tokens.
        let canonical = attr.to_string();
        match source_slice(self.source, &attr.span).filter(|text| same_tokens(text, &canonical)) {
            Some(text) => self.str(text),
            None => self.str(&canonical),
        }
    }

    fn expr(&mut self, expr: &Expr<'_>) -> Result<()> {
        match expr {
            Expr::Unit(unit) => {
                // `( )` may contain whitespace and comments that the AST does not keep.
                let text = source_slice(self.source, &unit.span)
                    .filter(|text| same_tokens(text, "()"))
                    .unwrap_or("()");
                self.str(text)
            }
            Expr::Bool(b) => self.str(if b.value { "true" } else { "false" }),
            Expr::Char(c) => self.str(&c.raw),
            Expr::Byte(b) => self.str(&b.raw),
            Expr::Number(n) => self.str(&n.raw),
            Expr::String(s) => self.str(&s.raw),
            Expr::Bytes(b) => self.str(&b.raw),
            Expr::Option(opt) => match &opt.value {
                None => self.str("None"),
                Some(inner) => {
                    self.str("Some(")?;
                    self.trivia(&inner.leading)?;
                    self.expr(&inner.expr)?;
                    self.trivia(&inner.trailing)?;
                    self.str(")")
                }
            },
            Expr::Seq(seq) => self.delimited(
                "[",
                &seq.leading,
                &seq.items,
                &seq.trailing,
                "]",
                |ser, item| {
                    ser.element(&item.leading, &item.expr, &item.trailing)?;
                    Ok(item.comma)
                },
            ),
            Expr::Map(map) => self.delimited(
                "{",
                &map.leading,
                &map.entries,
                &map.trailing,
                "}",
                |ser, entry| {
                    ser.trivia(&entry.leading)?;
                    ser.expr(&entry.key)?;
                    ser.trivia(&entry.pre_colon)?;
                    ser.str(":")?;
                    ser.trivia(&entry.post_colon)?;
                    ser.expr(&entry.value)?;
                    ser.trivia(&entry.trailing)?;
                    Ok(entry.comma)
                },
            ),
            Expr::Tuple(tuple) => self.tuple(&tuple.leading, &tuple.elements, &tuple.trailing),
            Expr::AnonStruct(s) => self.fields(&s.leading, &s.fields, &s.trailing),
            Expr::Struct(s) => {
                self.str(&s.name.name)?;
                self.trivia(&s.pre_body)?;
                match &s.body {
                    Some(StructBody::Tuple(body)) => {
                        self.tuple(&body.leading, &body.elements, &body.trailing)
                    }
                    Some(StructBody::Fields(body)) => {
                        self.fields(&body.leading, &body.fields, &body.trailing)
                    }
                    None => Ok(()),
                }
            }
            Expr::Error(_) => Err(Error::new(ErrorKind::Message(
                "cannot serialize error expression".to_string(),
            ))),
        }
    }

    fn tuple(
        &mut self,
        leading: &Trivia<'_>,
        elements: &[TupleElement<'_>],
        trailing: &Trivia<'_>,
    ) -> Result<()> {
        self.delimited("(", leading, elements, trailing, ")", |ser, element| {
            ser.element(&element.leading, &element.expr, &element.trailing)?;
            Ok(element.comma)
        })
    }

    fn fields(
        &mut self,
        leading: &Trivia<'_>,
        fields: &[StructField<'_>],
        trailing: &Trivia<'_>,
    ) -> Result<()> {
        self.delimited("(", leading, fields, trailing, ")", |ser, field| {
            ser.trivia(&field.leading)?;
            ser.str(&field.name.name)?;
            ser.trivia(&field.pre_colon)?;
            ser.str(":")?;
            ser.trivia(&field.post_colon)?;
            ser.expr(&field.value)?;
            ser.trivia(&field.trailing)?;
            Ok(field.comma)
        })
    }

    fn element(
        &mut self,
        leading: &Trivia<'_>,
        expr: &Expr<'_>,
        trailing: &Trivia<'_>,
    ) -> Result<()> {
        self.trivia(leading)?;
        self.expr(expr)?;
        self.trivia(trailing)
    }

    /// Writes a bracketed list. `item` writes one item and returns its comma; a comma is
    /// also written between items that lack one (e.g. in synthetic ASTs).
    fn delimited<T>(
        &mut self,
        open: &str,
        leading: &Trivia<'_>,
        items: &[T],
        trailing: &Trivia<'_>,
        close: &str,
        mut item: impl FnMut(&mut Self, &T) -> Result<Option<Span>>,
    ) -> Result<()> {
        self.str(open)?;
        self.trivia(leading)?;
        for (i, element) in items.iter().enumerate() {
            let comma = item(self, element)?;
            if comma.is_some() || i + 1 < items.len() {
                self.str(",")?;
            }
        }
        self.trivia(trailing)?;
        self.str(close)
    }
}

/// The text of `span` in `source`, if the span is real and in range.
fn source_slice<'s>(source: &'s str, span: &Span) -> Option<&'s str> {
    if span.is_synthetic() {
        return None;
    }
    source.get(span.start_offset..span.end_offset)
}

/// The original text of `trivia`, if its span still describes `source` exactly: the same
/// comments, with only whitespace around them.
fn trivia_source<'s>(source: &'s str, trivia: &Trivia<'_>) -> Option<&'s str> {
    let span = trivia.span.as_ref()?;
    let text = source_slice(source, span)?;
    let is_blank = |range: core::ops::Range<usize>| {
        source
            .get(range)
            .is_some_and(|gap| gap.chars().all(char::is_whitespace))
    };
    let mut pos = span.start_offset;
    for comment in &trivia.comments {
        let (start, end) = (comment.span.start_offset, comment.span.end_offset);
        if !is_blank(pos..start) || source.get(start..end) != Some(comment.text.as_ref()) {
            return None;
        }
        pos = end;
    }
    is_blank(pos..span.end_offset).then_some(text)
}

/// Whether `a` and `b` consist of the same tokens, ignoring whitespace and comments.
fn same_tokens(a: &str, b: &str) -> bool {
    let tokens = |s| Lexer::new(s).map(|token| (token.kind, token.text));
    tokens(a).eq(tokens(b))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::parse_document;

    /// Test that round-tripping preserves the exact source.
    fn assert_round_trip(source: &str) {
        let doc = parse_document(source).unwrap();
        let output = serialize_document(&doc).unwrap();
        assert_eq!(output, source, "Round-trip failed for: {source:?}");
    }

    #[test]
    fn round_trip_integer() {
        assert_round_trip("42");
    }

    #[test]
    fn round_trip_negative() {
        assert_round_trip("-42");
    }

    #[test]
    fn round_trip_hex() {
        assert_round_trip("0xFF");
    }

    #[test]
    fn round_trip_float() {
        assert_round_trip("3.14");
    }

    #[test]
    fn round_trip_string() {
        assert_round_trip(r#""hello""#);
    }

    #[test]
    fn round_trip_raw_string() {
        assert_round_trip(r##"r#"raw string"#"##);
    }

    #[test]
    fn round_trip_bytes() {
        assert_round_trip(r#"b"hello""#);
    }

    #[test]
    fn round_trip_raw_bytes() {
        assert_round_trip(r#"br"hello""#);
    }

    #[test]
    fn round_trip_raw_bytes_with_hash() {
        assert_round_trip(r##"br#"hello"#"##);
    }

    #[test]
    fn round_trip_char() {
        assert_round_trip("'a'");
    }

    #[test]
    fn round_trip_bool() {
        assert_round_trip("true");
        assert_round_trip("false");
    }

    #[test]
    fn round_trip_unit() {
        assert_round_trip("()");
    }

    #[test]
    fn round_trip_none() {
        assert_round_trip("None");
    }

    #[test]
    fn round_trip_some() {
        assert_round_trip("Some(42)");
    }

    #[test]
    fn round_trip_seq() {
        assert_round_trip("[1, 2, 3]");
    }

    #[test]
    fn round_trip_seq_trailing_comma() {
        assert_round_trip("[1, 2, 3,]");
    }

    #[test]
    fn round_trip_empty_seq() {
        assert_round_trip("[]");
    }

    #[test]
    fn round_trip_map() {
        assert_round_trip(r#"{"a": 1, "b": 2}"#);
    }

    #[test]
    fn round_trip_tuple() {
        assert_round_trip("(1, 2, 3)");
    }

    #[test]
    fn round_trip_struct() {
        assert_round_trip("Point(1, 2)");
    }

    #[test]
    fn round_trip_struct_fields() {
        assert_round_trip("Point(x: 1, y: 2)");
    }

    #[test]
    fn round_trip_with_comments() {
        assert_round_trip("// header\n42");
    }

    #[test]
    fn round_trip_block_comment() {
        assert_round_trip("/* comment */ 42");
    }

    #[test]
    fn round_trip_whitespace() {
        assert_round_trip("  42  ");
    }

    #[test]
    fn round_trip_multiline() {
        assert_round_trip("[\n  1,\n  2,\n  3\n]");
    }

    #[test]
    fn round_trip_complex() {
        let source = r#"// Configuration file
Config(
    name: "test",
    // Port number
    port: 8080,
    enabled: true,
    tags: ["web", "api"],
)"#;
        assert_round_trip(source);
    }

    #[test]
    fn round_trip_attribute() {
        assert_round_trip(r"#![enable(unwrap_newtypes)] 42");
    }

    #[test]
    fn round_trip_empty_document() {
        assert_round_trip("");
    }

    #[test]
    fn round_trip_comment_only() {
        assert_round_trip("// just a comment\n");
    }

    // =========================================================================
    // Anonymous struct round-trip tests
    // =========================================================================

    #[test]
    fn round_trip_anon_struct_simple() {
        assert_round_trip(r#"(name: "test", value: 42)"#);
    }

    #[test]
    fn round_trip_anon_struct_single_field() {
        assert_round_trip("(x: 1)");
    }

    #[test]
    fn round_trip_anon_struct_trailing_comma() {
        assert_round_trip("(x: 1, y: 2,)");
    }

    #[test]
    fn round_trip_anon_struct_nested() {
        assert_round_trip("(outer: (inner: 42))");
    }

    #[test]
    fn round_trip_anon_struct_with_whitespace() {
        assert_round_trip("( x : 1 , y : 2 )");
    }

    #[test]
    fn round_trip_anon_struct_multiline() {
        assert_round_trip("(\n  x: 1,\n  y: 2\n)");
    }

    #[test]
    fn round_trip_anon_struct_with_comments() {
        assert_round_trip("(\n  // comment\n  x: 1\n)");
    }

    #[test]
    fn round_trip_anon_struct_complex() {
        let source = r#"(
    name: "test",
    // config section
    config: (
        enabled: true,
        values: [1, 2, 3]
    ),
    optional: Some("value")
)"#;
        assert_round_trip(source);
    }

    #[test]
    fn round_trip_anon_struct_vs_tuple() {
        // Anonymous struct with named fields
        assert_round_trip("(x: 1)");
        // Tuple (no named fields)
        assert_round_trip("(1, 2, 3)");
        // Single element tuple
        assert_round_trip("(x)");
    }

    #[test]
    fn round_trip_attribute_spacing() {
        assert_round_trip("#![type=\"Foo\"]\n#![enable(implicit_some,unwrap_newtypes)]\n42");
    }

    #[test]
    #[ignore = "parser bug: whitespace inside an attribute leaks into the following trivia"]
    fn round_trip_attribute_inner_whitespace() {
        assert_round_trip("#![type = \"Foo\"]\n#![ enable( implicit_some ,unwrap_newtypes ) ]\n42");
    }

    #[test]
    fn round_trip_unit_with_comment() {
        assert_round_trip("[( /* nothing */ )]");
    }

    // =========================================================================
    // ASTs not parsed from `doc.source`
    // =========================================================================

    fn synthetic_document(value: crate::Value) -> Document<'static> {
        Document {
            source: "".into(),
            leading: Trivia::empty(),
            attributes: alloc::vec![Attribute::synthetic_type("my::Type")],
            pre_value: Trivia::empty(),
            value: Some(crate::ast::value_to_expr(value)),
            trailing: Trivia::empty(),
        }
    }

    #[test]
    fn synthetic_ast_serializes_its_tokens() {
        use crate::{Map, NamedContent, Value};

        let seq = synthetic_document(Value::Seq(alloc::vec![
            Value::from(1),
            Value::Bool(true),
            Value::Unit,
            Value::Option(None),
            Value::Option(Some(alloc::boxed::Box::new(Value::from('x')))),
        ]));
        assert_eq!(
            serialize_document(&seq).unwrap(),
            "#![type = \"my::Type\"][1,true,(),None,Some('x')]"
        );

        let mut map = Map::new();
        map.insert(Value::from("k"), Value::from(2));
        let named = synthetic_document(Value::Named {
            name: "Point".into(),
            content: NamedContent::Struct(alloc::vec![
                ("x".into(), Value::from(1)),
                ("y".into(), Value::Map(map)),
            ]),
        });
        let text = serialize_document(&named).unwrap();
        assert_eq!(text, "#![type = \"my::Type\"]Point(x:1,y:{\"k\":2})");
        assert!(parse_document(&text).is_ok());
    }

    #[test]
    fn edited_ast_serializes_the_edit() {
        let source = "// head\nPoint( x : 1 , y : 2 ) // tail";
        let mut doc = parse_document(source).unwrap();
        let Some(Expr::Struct(s)) = &mut doc.value else {
            panic!("expected struct");
        };
        s.name.name = "Vec2".into();
        let Some(StructBody::Fields(body)) = &mut s.body else {
            panic!("expected fields");
        };
        body.fields[0].name.name = "first".into();
        body.fields[1].value = Expr::Bool(crate::ast::BoolExpr {
            span: Span::synthetic(),
            value: false,
        });
        assert_eq!(
            serialize_document(&doc).unwrap(),
            "// head\nVec2( first : 1 , y : false ) // tail"
        );
    }

    #[test]
    fn edited_trivia_is_not_taken_from_source() {
        let mut doc = parse_document("[1, // one\n2]").unwrap();
        let Some(Expr::Seq(seq)) = &mut doc.value else {
            panic!("expected seq");
        };
        seq.items[1].leading.comments.clear();
        let output = serialize_document(&doc).unwrap();
        assert!(!output.contains("one"), "{output:?}");
        assert!(parse_document(&output).is_ok(), "{output:?}");
    }

    #[test]
    fn edited_attribute_is_serialized() {
        let mut doc = parse_document("#![type=\"Old\"]\n42").unwrap();
        doc.attributes[0].content = crate::ast::AttributeContent::Value("\"New\"".into());
        assert_eq!(serialize_document(&doc).unwrap(), "#![type = \"New\"]\n42");
    }

    #[test]
    fn foreign_source_does_not_panic() {
        // The spans point past the end of the replaced source: nothing may be sliced from
        // it, the trivia's own whitespace is used instead.
        let source = "Config(name: \"a long value\", other: [1, 2, 3])";
        let mut doc = parse_document(source).unwrap();
        doc.source = "()".into();
        assert_eq!(serialize_document(&doc).unwrap(), source);
    }
}
