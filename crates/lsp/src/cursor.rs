//! Locating the cursor in the AST.
//!
//! Completion and hover both need to know where the cursor sits: the path from
//! the root value to the innermost container, and whether the cursor is on a
//! field name, on a value, or inside a string or comment. Everything here
//! works on the error-recovered AST, so string and comment boundaries come
//! from the lexer instead of ad-hoc text scanning.

use ron2::{
    Span,
    ast::{self, Expr, StructBody, StructField, Trivia, TupleElement},
};

/// One step from a value to a nested value, as written in the document.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PathStep {
    /// The value of the struct field with this name.
    Field(String),
    /// A named value `Name(...)`: selects an enum variant, or names a struct.
    Variant(String),
    /// The element at this index of a tuple or tuple variant.
    Element(usize),
    /// An item of a sequence.
    SeqItem,
    /// A key of a map.
    MapKey,
    /// A value of a map.
    MapValue,
    /// The inner value of `Some(...)`.
    OptionSome,
}

/// An identifier under the cursor with its source span.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident {
    pub name: String,
    pub span: Span,
}

/// Where the cursor is, relative to the document's value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CursorContext {
    /// A field name is expected in the struct body reached by `path`.
    FieldName {
        path: Vec<PathStep>,
        /// Fields already present in that struct body.
        used: Vec<String>,
        /// The field name under the cursor, if any.
        ident: Option<Ident>,
    },
    /// A value is expected; its type is found by following `path` from the root.
    Value {
        path: Vec<PathStep>,
        /// The identifier under the cursor (an enum variant or struct name), if any.
        ident: Option<Ident>,
    },
    /// Inside `(...)` whose contents do not tell a struct body from a tuple
    /// (for example `()` or `(name)`); the schema type at `path` decides.
    Parens { path: Vec<PathStep>, element: usize },
    /// Inside a string, character literal, comment or attribute, or after the value.
    None,
}

/// Determine the cursor context at a byte offset.
pub fn context_at_offset(doc: &ast::Document<'_>, offset: usize) -> CursorContext {
    if in_comment(doc, offset) || doc.attributes.iter().any(|a| contains(&a.span, offset)) {
        return CursorContext::None;
    }

    let Some(value) = &doc.value else {
        return value_at(Vec::new());
    };
    let span = value.span();
    if offset < span.start_offset {
        return value_at(Vec::new());
    }
    if let Some(context) = without_root_parens(doc, value, offset) {
        return context;
    }
    if offset > span.end_offset {
        return CursorContext::None;
    }
    let walker = Walker {
        source: &doc.source,
        offset,
    };
    walker.walk(value, Vec::new())
}

fn value_at(path: Vec<PathStep>) -> CursorContext {
    CursorContext::Value { path, ident: None }
}

/// Handle `field: value, ...` written without the surrounding parentheses.
///
/// The parser reads `shape: Empty, opt: x` as the value `shape` followed by
/// garbage, so the document is re-parsed with the fields wrapped in `(...)`.
fn without_root_parens(
    doc: &ast::Document<'_>,
    value: &Expr<'_>,
    offset: usize,
) -> Option<CursorContext> {
    let Expr::Struct(s) = value else {
        return None;
    };
    let after_name = doc.source.get(s.name.span.end_offset..)?;
    if s.body.is_some() || !after_name.trim_start().starts_with(':') {
        return None;
    }

    let start = s.span.start_offset;
    let wrapped = format!("{}({})", &doc.source[..start], &doc.source[start..]);
    let (wrapped_doc, _) = ast::parse_document_lossy(&wrapped);
    let mut context = context_at_offset(&wrapped_doc, offset + 1);

    // Map identifier spans back to the original text, without the inserted `(`.
    let ident = match &mut context {
        CursorContext::FieldName { ident, .. } | CursorContext::Value { ident, .. } => ident,
        _ => return Some(context),
    };
    if let Some(Ident { span, .. }) = ident {
        span.start_offset -= 1;
        span.end_offset -= 1;
        for position in [&mut span.start, &mut span.end] {
            if position.line == s.span.start.line {
                position.col -= 1;
            }
        }
    }
    Some(context)
}

/// `start <= offset <= end`: the cursor touches the span.
fn contains(span: &Span, offset: usize) -> bool {
    span.start_offset <= offset && offset <= span.end_offset
}

/// `start < offset < end`: the cursor is strictly inside the span.
fn strictly_inside(span: &Span, offset: usize) -> bool {
    span.start_offset < offset && offset < span.end_offset
}

/// A zero-width span inserted by error recovery.
fn is_synthetic(span: &Span) -> bool {
    span.start_offset == span.end_offset
}

fn ident(id: &ast::Ident<'_>) -> Ident {
    Ident {
        name: id.name.to_string(),
        span: id.span,
    }
}

struct Walker<'s> {
    source: &'s str,
    offset: usize,
}

impl Walker<'_> {
    /// Whether the cursor is between an opening and closing delimiter.
    ///
    /// A missing closing delimiter has a synthetic span at the end of the
    /// input, so everything after the opening delimiter counts as inside.
    fn inside(&self, open: &Span, close: &Span) -> bool {
        self.offset >= open.end_offset && (self.offset <= close.start_offset || is_synthetic(close))
    }

    /// Whether the source at `span` starts a string, byte string or char literal.
    fn starts_literal(&self, span: &Span) -> bool {
        let text = self.source.get(span.start_offset..).unwrap_or("");
        let text = text.strip_prefix('b').unwrap_or(text);
        let text = text.strip_prefix('r').unwrap_or(text);
        text.starts_with(['"', '\'', '#'])
    }

    /// Whether the cursor is right behind a closing delimiter, where a value is complete.
    fn after(&self, close: &Span) -> bool {
        !is_synthetic(close) && self.offset >= close.end_offset
    }

    fn walk(&self, expr: &Expr<'_>, mut path: Vec<PathStep>) -> CursorContext {
        let offset = self.offset;
        if closing_delimiter(expr).is_some_and(|close| self.after(&close)) {
            return CursorContext::None;
        }
        match expr {
            Expr::String(_) | Expr::Char(_) | Expr::Bytes(_) | Expr::Byte(_)
                if strictly_inside(expr.span(), offset) =>
            {
                CursorContext::None
            }
            // An unterminated string, byte string or char literal runs to the cursor.
            Expr::Error(e) if offset > e.span.start_offset && self.starts_literal(&e.span) => {
                CursorContext::None
            }
            Expr::AnonStruct(s) if self.inside(&s.open_paren, &s.close_paren) => {
                self.in_fields(&s.fields, &s.close_paren, path)
            }
            Expr::Struct(s) => {
                if offset <= s.name.span.end_offset {
                    return CursorContext::Value {
                        path,
                        ident: Some(ident(&s.name)),
                    };
                }
                match &s.body {
                    Some(StructBody::Fields(body))
                        if self.inside(&body.open_brace, &body.close_brace) =>
                    {
                        path.push(PathStep::Variant(s.name.name.to_string()));
                        self.in_fields(&body.fields, &body.close_brace, path)
                    }
                    Some(StructBody::Tuple(body))
                        if self.inside(&body.open_paren, &body.close_paren) =>
                    {
                        path.push(PathStep::Variant(s.name.name.to_string()));
                        self.in_elements(&body.elements, path)
                    }
                    _ => value_at(path),
                }
            }
            Expr::Tuple(t) if self.inside(&t.open_paren, &t.close_paren) => {
                self.in_elements(&t.elements, path)
            }
            Expr::Unit(u) if strictly_inside(&u.span, offset) => {
                if self.in_unit_comment(&u.span) {
                    CursorContext::None
                } else {
                    CursorContext::Parens { path, element: 0 }
                }
            }
            Expr::Seq(s) if self.inside(&s.open_bracket, &s.close_bracket) => {
                path.push(PathStep::SeqItem);
                match s.items.iter().find(|i| contains(i.expr.span(), offset)) {
                    Some(item) => self.walk(&item.expr, path),
                    None => value_at(path),
                }
            }
            Expr::Map(m) if self.inside(&m.open_brace, &m.close_brace) => {
                for entry in &m.entries {
                    if contains(entry.key.span(), offset) {
                        path.push(PathStep::MapKey);
                        return self.walk(&entry.key, path);
                    }
                    let value_span = entry.value.span();
                    if !is_synthetic(&entry.colon)
                        && offset >= entry.colon.end_offset
                        && offset <= value_span.end_offset
                    {
                        path.push(PathStep::MapValue);
                        return if offset < value_span.start_offset {
                            value_at(path)
                        } else {
                            self.walk(&entry.value, path)
                        };
                    }
                }
                path.push(PathStep::MapKey);
                value_at(path)
            }
            Expr::Option(o) => match &o.value {
                Some(inner) if self.inside(&inner.open_paren, &inner.close_paren) => {
                    path.push(PathStep::OptionSome);
                    if contains(inner.expr.span(), offset) {
                        self.walk(&inner.expr, path)
                    } else {
                        value_at(path)
                    }
                }
                _ => value_at(path),
            },
            _ => value_at(path),
        }
    }

    fn in_fields(
        &self,
        fields: &[StructField<'_>],
        close: &Span,
        mut path: Vec<PathStep>,
    ) -> CursorContext {
        let offset = self.offset;
        let names = |skip: Option<usize>| -> Vec<String> {
            fields
                .iter()
                .enumerate()
                .filter(|(i, _)| Some(*i) != skip)
                .map(|(_, f)| f.name.name.to_string())
                .collect()
        };

        for (i, field) in fields.iter().enumerate() {
            if contains(&field.name.span, offset) {
                return CursorContext::FieldName {
                    path,
                    used: names(Some(i)),
                    ident: Some(ident(&field.name)),
                };
            }
            if is_synthetic(&field.colon) || offset < field.colon.end_offset {
                continue;
            }

            let value_span = field.value.span();
            let missing_value = matches!(field.value, Expr::Error(_)) && is_synthetic(value_span);
            // A missing value leaves the whole gap up to the next separator open for typing.
            let region_end = if missing_value {
                field
                    .comma
                    .map(|c| c.start_offset)
                    .or_else(|| fields.get(i + 1).map(|f| f.name.span.start_offset))
                    .unwrap_or(close.start_offset.max(value_span.end_offset))
            } else {
                value_span.end_offset
            };
            if offset <= region_end {
                path.push(PathStep::Field(field.name.name.to_string()));
                return if missing_value || offset < value_span.start_offset {
                    value_at(path)
                } else {
                    self.walk(&field.value, path)
                };
            }
        }

        CursorContext::FieldName {
            path,
            used: names(None),
            ident: None,
        }
    }

    fn in_elements(&self, elements: &[TupleElement<'_>], mut path: Vec<PathStep>) -> CursorContext {
        let offset = self.offset;
        if let Some((i, element)) = elements
            .iter()
            .enumerate()
            .find(|(_, e)| contains(e.expr.span(), offset))
        {
            // A lone identifier may be a field name typed into what parsed as a tuple.
            if matches!(&element.expr, Expr::Struct(s) if s.body.is_none()) {
                return CursorContext::Parens { path, element: i };
            }
            path.push(PathStep::Element(i));
            return self.walk(&element.expr, path);
        }
        let element = elements
            .iter()
            .filter(|e| e.expr.span().end_offset < offset)
            .count();
        CursorContext::Parens { path, element }
    }

    /// Whether the cursor is in a comment inside `( ... )` parsed as unit.
    ///
    /// The parser keeps no trivia for `()`, so these comments are not in the
    /// AST. The text between `(` and the cursor is only whitespace and
    /// comments, so the lexer decides: a value placed at the cursor is
    /// swallowed exactly when the cursor is inside a comment.
    fn in_unit_comment(&self, unit: &Span) -> bool {
        let Some(trivia) = self.source.get(unit.start_offset + 1..self.offset) else {
            return false;
        };
        let probe_source = format!("{trivia}0");
        let (probe, _) = ast::parse_document_lossy(&probe_source);
        !matches!(probe.value, Some(Expr::Number(_)))
    }
}

/// The span of an expression's closing delimiter (the whole span for `()`).
fn closing_delimiter(expr: &Expr<'_>) -> Option<Span> {
    match expr {
        Expr::Unit(u) => Some(u.span),
        Expr::AnonStruct(s) => Some(s.close_paren),
        Expr::Tuple(t) => Some(t.close_paren),
        Expr::Seq(s) => Some(s.close_bracket),
        Expr::Map(m) => Some(m.close_brace),
        Expr::Option(o) => o.value.as_ref().map(|v| v.close_paren),
        Expr::Struct(s) => match &s.body {
            Some(StructBody::Fields(b)) => Some(b.close_brace),
            Some(StructBody::Tuple(b)) => Some(b.close_paren),
            None => None,
        },
        _ => None,
    }
}

/// Whether the offset is inside a comment anywhere in the document.
fn in_comment(doc: &ast::Document<'_>, offset: usize) -> bool {
    let mut found = false;
    let mut check = |trivia: &Trivia<'_>| {
        found |= trivia.comments.iter().any(|c| match c.kind {
            // A line comment runs to the end of the line, so its end is still inside.
            ast::CommentKind::Line => c.span.start_offset < offset && offset <= c.span.end_offset,
            ast::CommentKind::Block => strictly_inside(&c.span, offset),
        });
    };
    check(&doc.leading);
    for attr in &doc.attributes {
        check(&attr.leading);
    }
    check(&doc.pre_value);
    if let Some(value) = &doc.value {
        visit_trivia(value, &mut check);
    }
    check(&doc.trailing);
    found
}

fn visit_trivia<'a>(expr: &Expr<'a>, f: &mut dyn FnMut(&Trivia<'a>)) {
    match expr {
        Expr::Option(o) => {
            if let Some(v) = &o.value {
                f(&v.leading);
                visit_trivia(&v.expr, f);
                f(&v.trailing);
            }
        }
        Expr::Seq(s) => {
            f(&s.leading);
            for item in &s.items {
                f(&item.leading);
                visit_trivia(&item.expr, f);
                f(&item.trailing);
            }
            f(&s.trailing);
        }
        Expr::Map(m) => {
            f(&m.leading);
            for entry in &m.entries {
                f(&entry.leading);
                visit_trivia(&entry.key, f);
                f(&entry.pre_colon);
                f(&entry.post_colon);
                visit_trivia(&entry.value, f);
                f(&entry.trailing);
            }
            f(&m.trailing);
        }
        Expr::Tuple(t) => visit_elements(&t.leading, &t.elements, &t.trailing, f),
        Expr::AnonStruct(s) => visit_fields(&s.leading, &s.fields, &s.trailing, f),
        Expr::Struct(s) => {
            f(&s.pre_body);
            match &s.body {
                Some(StructBody::Tuple(b)) => {
                    visit_elements(&b.leading, &b.elements, &b.trailing, f)
                }
                Some(StructBody::Fields(b)) => visit_fields(&b.leading, &b.fields, &b.trailing, f),
                None => {}
            }
        }
        _ => {}
    }
}

fn visit_elements<'a>(
    leading: &Trivia<'a>,
    elements: &[TupleElement<'a>],
    trailing: &Trivia<'a>,
    f: &mut dyn FnMut(&Trivia<'a>),
) {
    f(leading);
    for element in elements {
        f(&element.leading);
        visit_trivia(&element.expr, f);
        f(&element.trailing);
    }
    f(trailing);
}

fn visit_fields<'a>(
    leading: &Trivia<'a>,
    fields: &[StructField<'a>],
    trailing: &Trivia<'a>,
    f: &mut dyn FnMut(&Trivia<'a>),
) {
    f(leading);
    for field in fields {
        f(&field.leading);
        f(&field.pre_colon);
        f(&field.post_colon);
        visit_trivia(&field.value, f);
        f(&field.trailing);
    }
    f(trailing);
}

#[cfg(test)]
mod tests {
    use ron2::ast::parse_document_lossy;

    use super::*;

    /// Context at the `|` marker.
    fn context(source: &str) -> CursorContext {
        let offset = source.find('|').expect("cursor marker");
        let source = source.replacen('|', "", 1);
        let (doc, _) = parse_document_lossy(&source);
        context_at_offset(&doc, offset)
    }

    fn field(name: &str) -> PathStep {
        PathStep::Field(name.to_string())
    }

    fn is_field_name(ctx: &CursorContext, expected_path: &[PathStep]) -> bool {
        matches!(ctx, CursorContext::FieldName { path, .. } if path == expected_path)
    }

    fn is_value(ctx: &CursorContext, expected_path: &[PathStep]) -> bool {
        matches!(ctx, CursorContext::Value { path, .. } if path == expected_path)
    }

    #[test]
    fn quote_in_line_comment_does_not_open_a_string() {
        let ctx = context("// say \"hi\n(a: 1, |)");
        assert!(is_field_name(&ctx, &[]), "{ctx:?}");
    }

    #[test]
    fn quote_char_literal_does_not_open_a_string() {
        let ctx = context("(c: '\"', |)");
        let CursorContext::FieldName { used, .. } = &ctx else {
            panic!("{ctx:?}");
        };
        assert_eq!(used, &["c"]);
        assert!(is_value(&context("(c: '\"', d: |)"), &[field("d")]));
    }

    #[test]
    fn strings_chars_and_comments_have_no_context() {
        for source in [
            "(a: \"x|y\")",
            "(a: \"unterminated|",
            "(a: 'x|')",
            "(a: '|)",
            "(a: 'x|",
            "(a: b\"x|",
            "(a: r#\"x|",
            "(a: 1, // comm|ent\n b: 2)",
            "(a: /* bl|ock */ 1)",
            "( // in unit|\n)",
            "( /* in unit| */ )",
            "( /* a /* nested */ still| */ )",
            "#![type = \"a|\"]\n()",
        ] {
            assert_eq!(context(source), CursorContext::None, "{source}");
        }
    }

    #[test]
    #[ignore = "ron2 parser drops comments after the last trailing comma"]
    fn comment_after_last_trailing_comma_has_no_context() {
        assert_eq!(context("(a: 1, // comm|ent\n)"), CursorContext::None);
        assert_eq!(context("[1, // comm|ent\n]"), CursorContext::None);
    }

    #[test]
    fn cursor_after_comment_in_unit_is_inside_parens() {
        assert_eq!(
            context("( /* c */ | )"),
            CursorContext::Parens {
                path: vec![],
                element: 0
            }
        );
    }

    #[test]
    fn nested_paths() {
        assert!(is_value(
            &context("(server: (tls: |))"),
            &[field("server"), field("tls")]
        ));
        assert!(is_field_name(
            &context("(server: (port: 1, |))"),
            &[field("server")]
        ));
        assert!(is_field_name(
            &context("(ship: Fighter(slots: 1, |))"),
            &[field("ship"), PathStep::Variant("Fighter".into())]
        ));
        assert!(is_value(
            &context("(items: [(a: 1), (a: |)])"),
            &[field("items"), PathStep::SeqItem, field("a")]
        ));
        assert!(is_value(
            &context("(m: {\"k\": Some(|)})"),
            &[field("m"), PathStep::MapValue, PathStep::OptionSome]
        ));
        assert_eq!(
            context("(t: (1, (|)))"),
            CursorContext::Parens {
                path: vec![field("t"), PathStep::Element(1)],
                element: 0
            }
        );
        assert_eq!(
            context("(t: (1, |))"),
            CursorContext::Parens {
                path: vec![field("t")],
                element: 1
            }
        );
    }

    #[test]
    fn empty_parens_and_bare_identifiers_defer_to_schema() {
        let parens = |path: Vec<PathStep>, element| CursorContext::Parens { path, element };
        assert_eq!(context("(server: ( | ))"), parens(vec![field("server")], 0));
        assert_eq!(context("(server: (po|))"), parens(vec![field("server")], 0));
    }

    #[test]
    fn identifier_value_reports_ident() {
        let ctx = context("(graphics: Hi|gh)");
        let CursorContext::Value {
            path,
            ident: Some(ident),
        } = ctx
        else {
            panic!("expected value with ident");
        };
        assert_eq!(path, [field("graphics")]);
        assert_eq!(ident.name, "High");
    }

    #[test]
    fn missing_parentheses_at_root() {
        assert!(is_value(&context("graphics: |\n"), &[field("graphics")]));
        assert!(is_value(&context("shape: Empty,\nopt: |"), &[field("opt")]));
        assert!(is_field_name(&context("shape: Empty,\n|"), &[]));
        let CursorContext::Value {
            ident: Some(ident), ..
        } = context("shape: Em|pty")
        else {
            panic!("expected identifier");
        };
        assert_eq!((ident.span.start_offset, ident.span.end_offset), (7, 12));
        assert_eq!(ident.span.start.col, 8);
    }

    #[test]
    fn nothing_after_a_complete_value() {
        assert_eq!(context("(a: 1)\n|"), CursorContext::None);
        assert_eq!(context("(name: \"a\")|"), CursorContext::None);
        assert_eq!(context("(a: (b: 1)|, c: 2)"), CursorContext::None);
        assert_eq!(context("(a: [1]|)"), CursorContext::None);
        assert_eq!(context("(a: X(b: 1)|)"), CursorContext::None);
        assert_eq!(context("(a: ()|)"), CursorContext::None);
    }
}
