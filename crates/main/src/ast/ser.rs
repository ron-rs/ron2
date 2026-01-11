//! AST serializer for RON documents.
//!
//! This module provides serialization of AST back to RON text with
//! format preservation. The serializer preserves:
//! - Original whitespace and formatting
//! - Comments (via trivia)
//! - Raw literal representations (hex, binary, raw strings)
//!
//! For perfect round-tripping, the serializer uses the span byte offsets
//! to slice the original source text.

use alloc::string::String;
use core::fmt::Write;

use crate::ast::{
    AnonStructExpr, Attribute, Comment, Document, Expr, FieldsBody, Ident, MapEntry, MapExpr,
    OptionExpr, SeqExpr, SeqItem, StructBody, StructExpr, StructField, Trivia, TupleBody,
    TupleElement, TupleExpr,
};
use crate::error::Result;

/// Serialize an AST document back to RON text.
///
/// This preserves the original formatting by using the source spans.
/// For perfect round-tripping, this should produce identical output
/// to the original source.
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
    let mut ser = AstSerializer::new(&mut output, &doc.source);
    ser.write_document(doc)?;
    Ok(output)
}

/// Serialize an AST document to a writer.
pub fn serialize_document_to<W: Write>(writer: W, doc: &Document<'_>) -> Result<()> {
    let mut ser = AstSerializer::new(writer, &doc.source);
    ser.write_document(doc)
}

/// AST serializer that writes to a `fmt::Write` target.
struct AstSerializer<'a, W: Write> {
    writer: W,
    source: &'a str,
}

impl<'a, W: Write> AstSerializer<'a, W> {
    fn new(writer: W, source: &'a str) -> Self {
        Self { writer, source }
    }

    fn write_document(&mut self, doc: &Document<'_>) -> Result<()> {
        // Write leading trivia
        self.write_trivia(&doc.leading)?;

        // Write attributes
        for attr in &doc.attributes {
            self.write_attribute(attr)?;
        }

        // Write trivia between attributes and value
        self.write_trivia(&doc.pre_value)?;

        // Write the main value if present
        if let Some(ref value) = doc.value {
            self.write_expr(value)?;
        }

        // Write trailing trivia
        self.write_trivia(&doc.trailing)?;

        Ok(())
    }

    fn write_trivia(&mut self, trivia: &Trivia<'_>) -> Result<()> {
        // If we have a span, use the original source text for perfect round-tripping
        if let Some(ref span) = trivia.span {
            let text = span.slice(self.source);
            self.writer.write_str(text)?;
        } else {
            // No span - write whitespace and comments individually
            self.writer.write_str(&trivia.whitespace)?;
            for comment in &trivia.comments {
                self.write_comment(comment)?;
            }
        }
        Ok(())
    }

    fn write_comment(&mut self, comment: &Comment<'_>) -> Result<()> {
        // Use the raw text which includes delimiters
        self.writer.write_str(&comment.text)?;
        Ok(())
    }

    fn write_attribute(&mut self, attr: &Attribute<'_>) -> Result<()> {
        // Write leading trivia
        self.write_trivia(&attr.leading)?;

        // Use the span if available for exact reproduction
        let text = attr.span.slice(self.source);
        self.writer.write_str(text)?;

        Ok(())
    }

    fn write_expr(&mut self, expr: &Expr<'_>) -> Result<()> {
        match expr {
            Expr::Unit(u) => {
                let text = u.span.slice(self.source);
                self.writer.write_str(text)?;
            }
            Expr::Bool(b) => {
                let text = b.span.slice(self.source);
                self.writer.write_str(text)?;
            }
            Expr::Char(c) => {
                // Use raw text to preserve original format
                self.writer.write_str(&c.raw)?;
            }
            Expr::Byte(b) => {
                self.writer.write_str(&b.raw)?;
            }
            Expr::Number(n) => {
                // Use raw text to preserve hex, binary, underscores
                self.writer.write_str(&n.raw)?;
            }
            Expr::String(s) => {
                // Use raw text to preserve raw strings, escapes
                self.writer.write_str(&s.raw)?;
            }
            Expr::Bytes(b) => {
                self.writer.write_str(&b.raw)?;
            }
            Expr::Option(opt) => self.write_option(opt)?,
            Expr::Seq(seq) => self.write_seq(seq)?,
            Expr::Map(map) => self.write_map(map)?,
            Expr::Tuple(tuple) => self.write_tuple(tuple)?,
            Expr::AnonStruct(s) => self.write_anon_struct(s)?,
            Expr::Struct(s) => self.write_struct(s)?,
        }
        Ok(())
    }

    fn write_option(&mut self, opt: &OptionExpr<'_>) -> Result<()> {
        if let Some(inner) = &opt.value {
            // Write "Some"
            self.writer.write_str("Some")?;

            // Write opening paren using span
            let open_text = inner.open_paren.slice(self.source);
            self.writer.write_str(open_text)?;

            // Write leading trivia
            self.write_trivia(&inner.leading)?;

            // Write the inner expression
            self.write_expr(&inner.expr)?;

            // Write trailing trivia
            self.write_trivia(&inner.trailing)?;

            // Write closing paren using span
            let close_text = inner.close_paren.slice(self.source);
            self.writer.write_str(close_text)?;
        } else {
            // Write "None" - use span for exact text
            let text = opt.span.slice(self.source);
            self.writer.write_str(text)?;
        }
        Ok(())
    }

    fn write_seq(&mut self, seq: &SeqExpr<'_>) -> Result<()> {
        // Write opening bracket
        let open_text = seq.open_bracket.slice(self.source);
        self.writer.write_str(open_text)?;

        // Write leading trivia (inside the bracket)
        self.write_trivia(&seq.leading)?;

        // Write items
        for item in &seq.items {
            self.write_seq_item(item)?;
        }

        // Write trailing trivia (before closing bracket)
        self.write_trivia(&seq.trailing)?;

        // Write closing bracket
        let close_text = seq.close_bracket.slice(self.source);
        self.writer.write_str(close_text)?;

        Ok(())
    }

    fn write_seq_item(&mut self, item: &SeqItem<'_>) -> Result<()> {
        // Write leading trivia
        self.write_trivia(&item.leading)?;

        // Write expression
        self.write_expr(&item.expr)?;

        // Write trailing trivia
        self.write_trivia(&item.trailing)?;

        // Write comma if present
        if let Some(ref comma) = item.comma {
            let text = comma.slice(self.source);
            self.writer.write_str(text)?;
        }

        Ok(())
    }

    fn write_map(&mut self, map: &MapExpr<'_>) -> Result<()> {
        // Write opening brace
        let open_text = map.open_brace.slice(self.source);
        self.writer.write_str(open_text)?;

        // Write leading trivia
        self.write_trivia(&map.leading)?;

        // Write entries
        for entry in &map.entries {
            self.write_map_entry(entry)?;
        }

        // Write trailing trivia
        self.write_trivia(&map.trailing)?;

        // Write closing brace
        let close_text = map.close_brace.slice(self.source);
        self.writer.write_str(close_text)?;

        Ok(())
    }

    fn write_map_entry(&mut self, entry: &MapEntry<'_>) -> Result<()> {
        // Write leading trivia
        self.write_trivia(&entry.leading)?;

        // Write key
        self.write_expr(&entry.key)?;

        // Write pre-colon trivia
        self.write_trivia(&entry.pre_colon)?;

        // Write colon
        let colon_text = entry.colon.slice(self.source);
        self.writer.write_str(colon_text)?;

        // Write post-colon trivia
        self.write_trivia(&entry.post_colon)?;

        // Write value
        self.write_expr(&entry.value)?;

        // Write trailing trivia
        self.write_trivia(&entry.trailing)?;

        // Write comma if present
        if let Some(ref comma) = entry.comma {
            let text = comma.slice(self.source);
            self.writer.write_str(text)?;
        }

        Ok(())
    }

    fn write_tuple(&mut self, tuple: &TupleExpr<'_>) -> Result<()> {
        // Write opening paren
        let open_text = tuple.open_paren.slice(self.source);
        self.writer.write_str(open_text)?;

        // Write leading trivia
        self.write_trivia(&tuple.leading)?;

        // Write elements
        for elem in &tuple.elements {
            self.write_tuple_element(elem)?;
        }

        // Write trailing trivia
        self.write_trivia(&tuple.trailing)?;

        // Write closing paren
        let close_text = tuple.close_paren.slice(self.source);
        self.writer.write_str(close_text)?;

        Ok(())
    }

    fn write_tuple_element(&mut self, elem: &TupleElement<'_>) -> Result<()> {
        // Write leading trivia
        self.write_trivia(&elem.leading)?;

        // Write expression
        self.write_expr(&elem.expr)?;

        // Write trailing trivia
        self.write_trivia(&elem.trailing)?;

        // Write comma if present
        if let Some(ref comma) = elem.comma {
            let text = comma.slice(self.source);
            self.writer.write_str(text)?;
        }

        Ok(())
    }

    fn write_anon_struct(&mut self, s: &AnonStructExpr<'_>) -> Result<()> {
        // Write opening paren
        let open_text = s.open_paren.slice(self.source);
        self.writer.write_str(open_text)?;

        // Write leading trivia
        self.write_trivia(&s.leading)?;

        // Write fields
        for field in &s.fields {
            self.write_struct_field(field)?;
        }

        // Write trailing trivia
        self.write_trivia(&s.trailing)?;

        // Write closing paren
        let close_text = s.close_paren.slice(self.source);
        self.writer.write_str(close_text)?;

        Ok(())
    }

    fn write_struct(&mut self, s: &StructExpr<'_>) -> Result<()> {
        // Write the name
        self.write_ident(&s.name)?;

        // Write trivia between name and body (e.g., space in `Point { ... }`)
        self.write_trivia(&s.pre_body)?;

        // Write the body if present
        if let Some(ref body) = s.body {
            match body {
                StructBody::Tuple(tuple) => self.write_tuple_body(tuple)?,
                StructBody::Fields(fields) => self.write_fields_body(fields)?,
            }
        }

        Ok(())
    }

    fn write_ident(&mut self, ident: &Ident<'_>) -> Result<()> {
        // Use the span for exact reproduction
        let text = ident.span.slice(self.source);
        self.writer.write_str(text)?;
        Ok(())
    }

    fn write_tuple_body(&mut self, tuple: &TupleBody<'_>) -> Result<()> {
        // Write opening paren
        let open_text = tuple.open_paren.slice(self.source);
        self.writer.write_str(open_text)?;

        // Write leading trivia
        self.write_trivia(&tuple.leading)?;

        // Write elements
        for elem in &tuple.elements {
            self.write_tuple_element(elem)?;
        }

        // Write trailing trivia
        self.write_trivia(&tuple.trailing)?;

        // Write closing paren
        let close_text = tuple.close_paren.slice(self.source);
        self.writer.write_str(close_text)?;

        Ok(())
    }

    fn write_fields_body(&mut self, fields: &FieldsBody<'_>) -> Result<()> {
        // Write opening brace
        let open_text = fields.open_brace.slice(self.source);
        self.writer.write_str(open_text)?;

        // Write leading trivia
        self.write_trivia(&fields.leading)?;

        // Write fields
        for field in &fields.fields {
            self.write_struct_field(field)?;
        }

        // Write trailing trivia
        self.write_trivia(&fields.trailing)?;

        // Write closing brace
        let close_text = fields.close_brace.slice(self.source);
        self.writer.write_str(close_text)?;

        Ok(())
    }

    fn write_struct_field(&mut self, field: &StructField<'_>) -> Result<()> {
        // Write leading trivia
        self.write_trivia(&field.leading)?;

        // Write field name
        self.write_ident(&field.name)?;

        // Write pre-colon trivia
        self.write_trivia(&field.pre_colon)?;

        // Write colon
        let colon_text = field.colon.slice(self.source);
        self.writer.write_str(colon_text)?;

        // Write post-colon trivia
        self.write_trivia(&field.post_colon)?;

        // Write value
        self.write_expr(&field.value)?;

        // Write trailing trivia
        self.write_trivia(&field.trailing)?;

        // Write comma if present
        if let Some(ref comma) = field.comma {
            let text = comma.slice(self.source);
            self.writer.write_str(text)?;
        }

        Ok(())
    }
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
}
