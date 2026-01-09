//! Pretty RON error reporting with miette.
//!
//! This example demonstrates how to display user-friendly error messages
//! when parsing RON files using the miette diagnostic library.
//!
//! Run with: `cargo run -p pretty-errors-miette`

use miette::{Diagnostic, NamedSource, Report, SourceSpan};
use ron2::ast::{parse_document, Document};
use ron2::error::{Error, SpannedError};
use std::fs;
use thiserror::Error;

/// A RON parsing error wrapped for miette display.
#[derive(Error, Debug, Diagnostic)]
#[error("{message}")]
pub struct RonDiagnostic {
    message: String,

    #[source_code]
    src: NamedSource<String>,

    #[label("{label}")]
    span: SourceSpan,

    label: String,

    #[help]
    help: Option<String>,
}

impl RonDiagnostic {
    /// Create a diagnostic from a ron2 SpannedError.
    pub fn from_spanned(err: SpannedError, src: String, filename: &str) -> Self {
        let (label, help) = error_label_and_help(&err.code);

        Self {
            message: err.code.to_string(),
            src: NamedSource::new(filename, src),
            span: (err.span.start_offset, err.span.end_offset - err.span.start_offset).into(),
            label,
            help,
        }
    }

    /// Create a diagnostic from a ron2 Error (without span info).
    /// Falls back to highlighting the entire source.
    pub fn from_error(err: Error, src: String, filename: &str) -> Self {
        let (label, help) = error_label_and_help(&err);

        Self {
            message: err.to_string(),
            src: NamedSource::new(filename, src.clone()),
            span: (0, src.len()).into(),
            label,
            help,
        }
    }
}

/// Generate a contextual label and optional help message for an error.
fn error_label_and_help(err: &Error) -> (String, Option<String>) {
    match err {
        Error::ExpectedBoolean => (
            "expected `true` or `false`".into(),
            Some("RON booleans are lowercase: true, false".into()),
        ),
        Error::ExpectedInteger => (
            "expected an integer".into(),
            Some("Examples: 42, -10, 0x1F, 0b1010, 0o755".into()),
        ),
        Error::ExpectedFloat => (
            "expected a float".into(),
            Some("Examples: 3.14, -0.5, 1e10, inf, NaN".into()),
        ),
        Error::ExpectedString => (
            "expected a string".into(),
            Some("Strings use double quotes: \"hello\"".into()),
        ),
        Error::ExpectedChar => (
            "expected a character".into(),
            Some("Characters use single quotes: 'a'".into()),
        ),
        Error::ExpectedArray => (
            "expected `[`".into(),
            Some("Arrays use square brackets: [1, 2, 3]".into()),
        ),
        Error::ExpectedArrayEnd => (
            "expected `]`".into(),
            Some("Did you forget to close the array?".into()),
        ),
        Error::ExpectedMap => (
            "expected `{`".into(),
            Some("Maps use curly braces: { \"key\": value }".into()),
        ),
        Error::ExpectedMapEnd => (
            "expected `}`".into(),
            Some("Did you forget to close the map?".into()),
        ),
        Error::ExpectedMapColon { context } => (
            "expected `:`".into(),
            context
                .map(|ctx| match ctx {
                    "struct field" => "Struct fields use colons: field: value".into(),
                    "map entry" => "Map entries use colons: key: value".into(),
                    _ => "Colons separate keys from values".into(),
                })
                .or(Some("Map entries use colons: key: value".into())),
        ),
        Error::ExpectedStructLike => (
            "expected `(`".into(),
            Some("Structs/tuples use parentheses: (field: value)".into()),
        ),
        Error::ExpectedStructLikeEnd => (
            "expected `)`".into(),
            Some("Did you forget to close the struct/tuple?".into()),
        ),
        Error::ExpectedComma { context } => (
            "expected `,`".into(),
            context
                .map(|ctx| match ctx {
                    "array" => "Separate array elements with commas".into(),
                    "map" => "Separate map entries with commas".into(),
                    "tuple" => "Separate tuple elements with commas".into(),
                    "struct" => "Separate struct fields with commas".into(),
                    _ => "Separate elements with commas. Trailing commas are allowed.".into(),
                })
                .or(Some("Separate elements with commas. Trailing commas are allowed.".into())),
        ),
        Error::ExpectedIdentifier => (
            "expected an identifier".into(),
            Some("Identifiers start with a letter or underscore".into()),
        ),
        Error::UnexpectedChar(c) => (
            format!("unexpected character `{c}`"),
            None,
        ),
        Error::Eof => (
            "unexpected end of input".into(),
            Some("The file ended unexpectedly. Check for unclosed brackets.".into()),
        ),
        Error::UnclosedBlockComment => (
            "unclosed block comment".into(),
            Some("Block comments must be closed with */".into()),
        ),
        Error::TrailingCharacters => (
            "unexpected content after value".into(),
            Some("RON files should contain exactly one value".into()),
        ),
        Error::InvalidEscape(msg) => (
            "invalid escape sequence".into(),
            Some((*msg).into()),
        ),
        Error::IntegerOutOfBounds { .. } => (
            "integer out of bounds".into(),
            Some("The number is too large or too small for the target type".into()),
        ),
        Error::FloatUnderscore => (
            "underscore in float".into(),
            Some("Underscores are not allowed in float literals".into()),
        ),
        Error::NoSuchEnumVariant { expected, found, .. } => (
            format!("unknown variant `{found}`"),
            Some(format!("Valid variants: {}", expected.join(", "))),
        ),
        Error::NoSuchStructField { expected, found, .. } => (
            format!("unknown field `{found}`"),
            Some(format!("Valid fields: {}", expected.join(", "))),
        ),
        Error::MissingStructField { field, .. } => (
            format!("missing required field `{field}`"),
            None,
        ),
        Error::DuplicateStructField { field, .. } => (
            format!("duplicate field `{field}`"),
            Some("Each field can only appear once".into()),
        ),
        Error::InvalidValueForType { expected, found } => (
            format!("expected {expected}, found {found}"),
            None,
        ),
        Error::SuggestRawIdentifier(ident) => (
            "invalid identifier".into(),
            Some(format!("Try using a raw identifier: r#{ident}")),
        ),
        _ => ("error occurred here".into(), None),
    }
}

/// Parse RON and return a miette Result for nice error display.
pub fn parse_ron<'a>(src: &'a str, filename: &str) -> Result<Document<'a>, Report> {
    parse_document(src).map_err(|err| RonDiagnostic::from_spanned(err, src.to_string(), filename).into())
}

/// Demonstrate various RON parsing errors with pretty output.
fn main() -> miette::Result<()> {
    // Install the fancy miette handler for beautiful output
    miette::set_hook(Box::new(|_| {
        Box::new(
            miette::MietteHandlerOpts::new()
                .terminal_links(true)
                .unicode(true)
                .context_lines(2)
                .tab_width(4)
                .build(),
        )
    }))
    .ok();

    println!("=== RON Pretty Errors with miette ===\n");

    // Example 1: Unclosed struct
    let example1 = r#"(
    name: "Test",
    value: 42
"#;
    println!("--- Example 1: Unclosed struct ---");
    if let Err(e) = parse_ron(example1, "config.ron") {
        println!("{:?}\n", e);
    }

    // Example 2: Invalid character
    let example2 = r#"(
    name: "Test",
    value: @invalid
)"#;
    println!("--- Example 2: Invalid character ---");
    if let Err(e) = parse_ron(example2, "config.ron") {
        println!("{:?}\n", e);
    }

    // Example 3: Missing colon
    let example3 = r#"(
    name "Test"
)"#;
    println!("--- Example 3: Missing colon ---");
    if let Err(e) = parse_ron(example3, "config.ron") {
        println!("{:?}\n", e);
    }

    // Example 4: Unclosed string
    let example4 = r#"(
    message: "Hello, world
)"#;
    println!("--- Example 4: Unclosed string ---");
    if let Err(e) = parse_ron(example4, "config.ron") {
        println!("{:?}\n", e);
    }

    // Example 5: Trailing content
    let example5 = r#"(value: 42) extra"#;
    println!("--- Example 5: Trailing content ---");
    if let Err(e) = parse_ron(example5, "config.ron") {
        println!("{:?}\n", e);
    }

    // Example 6: Unclosed block comment
    let example6 = r#"/* This comment
is never closed
(value: 42)"#;
    println!("--- Example 6: Unclosed block comment ---");
    if let Err(e) = parse_ron(example6, "config.ron") {
        println!("{:?}\n", e);
    }

    // Example 7: Valid RON (should succeed)
    let example7 = r#"(
    name: "Valid Config",
    enabled: true,
    count: 42,
)"#;
    println!("--- Example 7: Valid RON ---");
    match parse_ron(example7, "config.ron") {
        Ok(doc) => println!("Successfully parsed!\n{:#?}\n", doc.value),
        Err(e) => println!("{:?}\n", e),
    }

    // Interactive mode: parse from file if provided
    if let Some(path) = std::env::args().nth(1) {
        println!("--- Parsing file: {} ---", path);
        match fs::read_to_string(&path) {
            Ok(content) => {
                if let Err(e) = parse_ron(&content, &path) {
                    println!("{:?}", e);
                } else {
                    println!("File parsed successfully!");
                }
            }
            Err(e) => println!("Failed to read file: {}", e),
        }
    }

    Ok(())
}
