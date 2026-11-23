//! Property tests for the formatter over a corpus of inputs and configurations.
//!
//! For every input and configuration the output must
//! - parse again,
//! - describe the same value,
//! - keep every comment (none in `CommentMode::Delete`),
//! - be a fixed point: formatting it again changes nothing,
//! - have no trailing whitespace.

use std::time::{Duration, Instant};

use ron2::{
    Span, Value,
    ast::{
        AnonStructExpr, Document, Expr, Ident, StringExpr, StringKind, StructBody, StructField,
        Trivia, TupleElement, parse_document,
    },
    fmt::{CommentMode, CompactTypes, FormatConfig, Spacing, format_document, format_expr},
};

const CORPUS: &[&str] = &[
    "",
    "// only a comment\n",
    "42",
    "42 // answer",
    "// header\n42",
    "/* block */ 42",
    "(a: 1) /* x */",
    "Unit",
    "Config(x:1,y:2)",
    "Point(1, 2, 3)",
    "[1, 2, 3]",
    "[[[[[[1]]]]]]",
    "{\"a\": 1, \"b\": [1, 2]}",
    "(map: {(1, 2): \"tuple key\", \"k\": [1]})",
    "(r: r#\"raw \"str\"\"#, b: b\"bytes\", c: 'c', h: 0xFF, f: 1.5e3, n: None, u: ())",
    "Some(/* c */ 1)",
    // Line comments followed by a comma, colon, value or closing delimiter.
    "[\n 1 // one\n , 2]",
    "(a // c\n : 1)",
    "(\n a: // c\n 1,\n)",
    "(a: Some(1 // c\n))",
    "(a: Some( // c\n 1))",
    "(a: [ // c\n ], b: 1)",
    "[1 // c\n, 2]",
    // Compaction inside compaction.
    "(a: (1, [1111111, 2222222, 3333333]))",
    "(a: (b: [\n // c\n 1,\n]))",
    // Line comments that cannot become block comments.
    "(a: [\n // see a*/b\n 1,\n], b: [ // open /* here\n 2])",
    // Comments in less common places.
    "Point /* c */ (x: 1)",
    "A(a: 1, /* c */)",
    "(a: [/* x */], b: {/* z */})",
    "[\n    /* multi\n       line */\n    1,\n]",
    "(\n    a: 1, /* a */ // b\n    b: 2,\n)",
    "(\n port: 8080, // the port\n host: \"h\",\n)",
    "#![type = \"Foo\"]\n// doc\nConfig(\n    // Server port\n    port: 8080,\n    tags: [\"web\", \"api\"], /* trailing block */\n    nested: Inner(a: Some([1, 2]), b: {1: (x: 1)}),\n)",
    "[\r\n 1, // c\r\n /* d\r\n */ 2\r\n]",
    // Width.
    "(a: \"ünïcödé strings that are fairly long to push the line over the limit\", b: [1, 2])",
    "E(Variant(1), Other(x: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21]))",
];

/// Inputs whose output puts a comment after the last item's comma, where the parser
/// currently drops it.
const NEEDS_TRAILING_TRIVIA_FIX: &[&str] = &[
    "[\n 1,\n 2, // two\n]",
    "[1, 2 // two\n]",
    "(\n port: 8080, // the port\n host: \"h\", // the host\n)",
    "Point(\n x: 1,\n // dangling\n)",
    "Some([\n 1,\n 2, // two\n])",
];

fn configs() -> Vec<(&'static str, FormatConfig)> {
    vec![
        ("default", FormatConfig::default()),
        ("no length rule", FormatConfig::default().char_limit(0)),
        ("width 20", FormatConfig::default().char_limit(20)),
        (
            "tabs width 30",
            FormatConfig::default().indent("\t").char_limit(30),
        ),
        (
            "preserve depth 1",
            FormatConfig::default()
                .comments(CommentMode::Preserve)
                .compact_from_depth(1),
        ),
        (
            "auto depth 1",
            FormatConfig::default().compact_from_depth(1),
        ),
        (
            "auto depth 0",
            FormatConfig::default().compact_from_depth(0),
        ),
        (
            "all types",
            FormatConfig::default()
                .char_limit(5)
                .compact_types(CompactTypes::all()),
        ),
        (
            "tuples",
            FormatConfig::default()
                .char_limit(10)
                .compact_types(CompactTypes {
                    tuples: true,
                    ..CompactTypes::default()
                }),
        ),
        (
            "no indent preserve",
            FormatConfig::default()
                .indent("")
                .comments(CommentMode::Preserve),
        ),
        ("no spacing", FormatConfig::default().spacing(Spacing::None)),
        (
            "delete",
            FormatConfig::default().comments(CommentMode::Delete),
        ),
        ("minimal", FormatConfig::minimal()),
    ]
}

fn check_corpus(corpus: &[&str]) {
    let mut failures = Vec::new();
    for source in corpus {
        let doc = parse_document(source).unwrap_or_else(|e| panic!("{source:?}: {e}"));
        let value = source.parse::<Value>().ok();
        let comments = document_comments(&doc);
        for (name, config) in configs() {
            if let Err(problem) = check(&doc, value.as_ref(), comments, &config) {
                failures.push(format!("[{name}] {source:?}\n{problem}"));
            }
        }
    }
    assert!(failures.is_empty(), "\n{}", failures.join("\n\n"));
}

fn check(
    doc: &Document<'_>,
    value: Option<&Value>,
    comments: usize,
    config: &FormatConfig,
) -> Result<(), String> {
    let once = format_document(doc, config);
    let reparsed =
        parse_document(&once).map_err(|e| format!("output does not parse: {e}\n{once}"))?;
    if once.parse::<Value>().ok().as_ref() != value {
        return Err(format!("value changed:\n{once}"));
    }
    let expected = if config.comments == CommentMode::Delete {
        0
    } else {
        comments
    };
    let found = document_comments(&reparsed);
    if found != expected {
        return Err(format!("{expected} comments became {found}:\n{once}"));
    }
    let twice = format_document(&reparsed, config);
    if twice != once {
        return Err(format!(
            "not idempotent:\n{once}\n--- second pass:\n{twice}"
        ));
    }
    if once.lines().any(|line| line.ends_with([' ', '\t'])) {
        return Err(format!("trailing whitespace:\n{once:?}"));
    }
    Ok(())
}

#[test]
fn corpus_is_stable_and_lossless() {
    check_corpus(CORPUS);
}

#[test]
#[ignore = "needs W1 parser trailing-trivia fix"]
fn corpus_with_comments_after_last_comma() {
    check_corpus(NEEDS_TRAILING_TRIVIA_FIX);
}

/// `(a: (a: … "xxx…" …))`, built directly: the parser stops at depth 128.
fn nested_struct(depth: usize) -> Expr<'static> {
    let text = "x".repeat(100);
    let mut expr = Expr::String(StringExpr {
        span: Span::synthetic(),
        raw: format!("\"{text}\"").into(),
        value: text,
        kind: StringKind::Regular,
    });
    for _ in 0..depth {
        expr = Expr::AnonStruct(AnonStructExpr {
            span: Span::synthetic(),
            open_paren: Span::synthetic(),
            leading: Trivia::empty(),
            fields: vec![StructField {
                leading: Trivia::empty(),
                name: Ident {
                    span: Span::synthetic(),
                    name: "a".into(),
                },
                pre_colon: Trivia::empty(),
                colon: Span::synthetic(),
                post_colon: Trivia::empty(),
                value: expr,
                trailing: Trivia::empty(),
                comma: None,
            }],
            trailing: Trivia::empty(),
            close_paren: Span::synthetic(),
        });
    }
    expr
}

#[test]
fn deep_nesting_formats_in_linear_time() {
    // Before the fix every level formatted its subtree twice: depth 26 took seconds.
    let expr = nested_struct(200);

    let mut nested_source = "(a: ".repeat(120);
    nested_source.push_str("[1, // c\n 2]");
    nested_source.push_str(&")".repeat(120));
    let doc = parse_document(&nested_source).unwrap();

    for (name, config) in configs() {
        let start = Instant::now();
        let from_value = format_expr(&expr, &config);
        let from_source = format_document(&doc, &config);
        let elapsed = start.elapsed();
        assert!(
            elapsed < Duration::from_secs(1),
            "[{name}] took {elapsed:?}"
        );
        assert!(from_value.contains(&"x".repeat(100)));
        assert!(from_source.contains('2'));
    }
}

// ============================================================================
// Comment counting
// ============================================================================

fn document_comments(doc: &Document<'_>) -> usize {
    trivia(&doc.leading)
        + doc
            .attributes
            .iter()
            .map(|a| trivia(&a.leading))
            .sum::<usize>()
        + trivia(&doc.pre_value)
        + doc.value.as_ref().map_or(0, expr)
        + trivia(&doc.trailing)
}

fn trivia(t: &Trivia<'_>) -> usize {
    t.comments.len()
}

fn expr(e: &Expr<'_>) -> usize {
    match e {
        Expr::Option(opt) => opt.value.as_ref().map_or(0, |v| {
            trivia(&v.leading) + expr(&v.expr) + trivia(&v.trailing)
        }),
        Expr::Seq(s) => {
            trivia(&s.leading)
                + s.items
                    .iter()
                    .map(|i| trivia(&i.leading) + expr(&i.expr) + trivia(&i.trailing))
                    .sum::<usize>()
                + trivia(&s.trailing)
        }
        Expr::Map(m) => {
            trivia(&m.leading)
                + m.entries
                    .iter()
                    .map(|e| {
                        trivia(&e.leading)
                            + expr(&e.key)
                            + trivia(&e.pre_colon)
                            + trivia(&e.post_colon)
                            + expr(&e.value)
                            + trivia(&e.trailing)
                    })
                    .sum::<usize>()
                + trivia(&m.trailing)
        }
        Expr::Tuple(t) => tuple(&t.leading, &t.elements, &t.trailing),
        Expr::AnonStruct(s) => fields(&s.leading, &s.fields, &s.trailing),
        Expr::Struct(s) => {
            trivia(&s.pre_body)
                + match &s.body {
                    Some(StructBody::Tuple(t)) => tuple(&t.leading, &t.elements, &t.trailing),
                    Some(StructBody::Fields(f)) => fields(&f.leading, &f.fields, &f.trailing),
                    None => 0,
                }
        }
        _ => 0,
    }
}

fn tuple(leading: &Trivia<'_>, elements: &[TupleElement<'_>], trailing: &Trivia<'_>) -> usize {
    trivia(leading)
        + elements
            .iter()
            .map(|e| trivia(&e.leading) + expr(&e.expr) + trivia(&e.trailing))
            .sum::<usize>()
        + trivia(trailing)
}

fn fields(leading: &Trivia<'_>, fields: &[StructField<'_>], trailing: &Trivia<'_>) -> usize {
    trivia(leading)
        + fields
            .iter()
            .map(|f| {
                trivia(&f.leading)
                    + trivia(&f.pre_colon)
                    + trivia(&f.post_colon)
                    + expr(&f.value)
                    + trivia(&f.trailing)
            })
            .sum::<usize>()
        + trivia(trailing)
}
