//! Pretty RON error reporting with ariadne.
//!
//! This example demonstrates how to display user-friendly error messages
//! when parsing RON files using the ariadne diagnostic library.
//!
//! Run with: `cargo run -p pretty-errors-ariadne`

use ariadne::{Color, Config, Label, Report, ReportKind, Source};
use ron2::ast::{parse_document, Document};
use ron2::error::{Error, SpannedError};
use std::fs;

/// Generate a contextual label and optional note for an error.
fn error_details(err: &Error) -> (&'static str, Option<&'static str>) {
    match err {
        Error::ExpectedBoolean => (
            "expected `true` or `false`",
            Some("RON booleans are lowercase: true, false"),
        ),
        Error::ExpectedInteger => (
            "expected an integer",
            Some("Examples: 42, -10, 0x1F, 0b1010, 0o755"),
        ),
        Error::ExpectedFloat => (
            "expected a float",
            Some("Examples: 3.14, -0.5, 1e10, inf, NaN"),
        ),
        Error::ExpectedString => (
            "expected a string",
            Some("Strings use double quotes: \"hello\""),
        ),
        Error::ExpectedChar => (
            "expected a character",
            Some("Characters use single quotes: 'a'"),
        ),
        Error::ExpectedArray => (
            "expected `[`",
            Some("Arrays use square brackets: [1, 2, 3]"),
        ),
        Error::ExpectedArrayEnd => ("expected `]`", Some("Did you forget to close the array?")),
        Error::ExpectedMap => (
            "expected `{`",
            Some("Maps use curly braces: { \"key\": value }"),
        ),
        Error::ExpectedMapEnd => ("expected `}`", Some("Did you forget to close the map?")),
        Error::ExpectedMapColon { context } => (
            "expected `:`",
            context
                .map(|ctx| match ctx {
                    "struct field" => "Struct fields use colons: field: value",
                    "map entry" => "Map entries use colons: key: value",
                    _ => "Colons separate keys from values",
                })
                .or(Some("Map entries use colons: key: value")),
        ),
        Error::ExpectedStructLike => (
            "expected `(`",
            Some("Structs/tuples use parentheses: (field: value)"),
        ),
        Error::ExpectedStructLikeEnd => (
            "expected `)`",
            Some("Did you forget to close the struct/tuple?"),
        ),
        Error::ExpectedComma { context } => (
            "expected `,`",
            context
                .map(|ctx| match ctx {
                    "array" => "Separate array elements with commas",
                    "map" => "Separate map entries with commas",
                    "tuple" => "Separate tuple elements with commas",
                    "struct" => "Separate struct fields with commas",
                    _ => "Separate elements with commas. Trailing commas are allowed.",
                })
                .or(Some(
                    "Separate elements with commas. Trailing commas are allowed.",
                )),
        ),
        Error::ExpectedIdentifier => (
            "expected an identifier",
            Some("Identifiers start with a letter or underscore"),
        ),
        Error::UnexpectedChar(_) => ("unexpected character", None),
        Error::Eof => (
            "unexpected end of input",
            Some("The file ended unexpectedly. Check for unclosed brackets."),
        ),
        Error::UnclosedBlockComment => (
            "unclosed block comment",
            Some("Block comments must be closed with */"),
        ),
        Error::TrailingCharacters => (
            "unexpected content after value",
            Some("RON files should contain exactly one value"),
        ),
        Error::InvalidEscape(_) => ("invalid escape sequence", None),
        Error::IntegerOutOfBounds { .. } => (
            "integer out of bounds",
            Some("The number is too large or too small for the target type"),
        ),
        Error::FloatUnderscore => (
            "underscore in float",
            Some("Underscores are not allowed in float literals"),
        ),
        Error::NoSuchEnumVariant { .. } => ("unknown enum variant", None),
        Error::NoSuchStructField { .. } => ("unknown struct field", None),
        Error::MissingStructField { .. } => ("missing required field", None),
        Error::DuplicateStructField { .. } => {
            ("duplicate field", Some("Each field can only appear once"))
        }
        Error::InvalidValueForType { .. } => ("type mismatch", None),
        Error::SuggestRawIdentifier(_) => (
            "invalid identifier",
            Some("Try using a raw identifier: r#name"),
        ),
        _ => ("error occurred here", None),
    }
}

/// Print a pretty error report for a RON parsing error.
pub fn print_error(err: &SpannedError, src: &str, filename: &str) {
    let (label_text, note) = error_details(&err.code);

    let mut report = Report::build(ReportKind::Error, filename, err.span.start_offset)
        .with_message(err.code.to_string())
        .with_label(
            Label::new((filename, err.span.start_offset..err.span.end_offset))
                .with_message(label_text)
                .with_color(Color::Red),
        )
        .with_config(Config::default().with_tab_width(4));

    // Add extra context for certain error types
    match &err.code {
        Error::NoSuchEnumVariant {
            expected, found, ..
        } => {
            report = report
                .with_note(format!("Unknown variant `{found}`"))
                .with_help(format!("Valid variants: {}", expected.join(", ")));
        }
        Error::NoSuchStructField {
            expected, found, ..
        } => {
            report = report
                .with_note(format!("Unknown field `{found}`"))
                .with_help(format!("Valid fields: {}", expected.join(", ")));
        }
        Error::MissingStructField { field, .. } => {
            report = report.with_help(format!("Add the missing field: {field}: <value>"));
        }
        Error::InvalidValueForType { expected, found } => {
            report = report.with_note(format!("Expected {expected}, but found {found}"));
        }
        Error::SuggestRawIdentifier(ident) => {
            report = report.with_help(format!("Try: r#{ident}"));
        }
        _ => {
            if let Some(note_text) = note {
                report = report.with_note(note_text);
            }
        }
    }

    report
        .finish()
        .print((filename, Source::from(src)))
        .expect("Failed to print error report");
}

/// Print a pretty error report with multiple labels (for complex errors).
#[allow(dead_code)]
pub fn print_error_with_context(
    err: &SpannedError,
    src: &str,
    filename: &str,
    additional_labels: Vec<(usize, usize, &str, Color)>,
) {
    let (label_text, note) = error_details(&err.code);

    let mut report = Report::build(ReportKind::Error, filename, err.span.start_offset)
        .with_message(err.code.to_string())
        .with_label(
            Label::new((filename, err.span.start_offset..err.span.end_offset))
                .with_message(label_text)
                .with_color(Color::Red),
        )
        .with_config(Config::default().with_tab_width(4));

    // Add additional context labels
    for (start, end, msg, color) in additional_labels {
        report = report.with_label(
            Label::new((filename, start..end))
                .with_message(msg)
                .with_color(color),
        );
    }

    if let Some(note_text) = note {
        report = report.with_note(note_text);
    }

    report
        .finish()
        .print((filename, Source::from(src)))
        .expect("Failed to print error report");
}

/// Parse RON and print pretty errors on failure.
pub fn parse_ron<'a>(src: &'a str, filename: &str) -> Option<Document<'a>> {
    match parse_document(src) {
        Ok(doc) => Some(doc),
        Err(err) => {
            print_error(&err, src, filename);
            None
        }
    }
}

/// Demonstrate various RON parsing errors with pretty output.
fn main() {
    println!("=== RON Pretty Errors with ariadne ===\n");

    // Example 1: Unclosed struct
    let example1 = r#"(
    name: "Test",
    value: 42
"#;
    println!("--- Example 1: Unclosed struct ---");
    parse_ron(example1, "config.ron");
    println!();

    // Example 2: Invalid character
    let example2 = r#"(
    name: "Test",
    value: @invalid
)"#;
    println!("--- Example 2: Invalid character ---");
    parse_ron(example2, "config.ron");
    println!();

    // Example 3: Missing colon
    let example3 = r#"(
    name "Test"
)"#;
    println!("--- Example 3: Missing colon ---");
    parse_ron(example3, "config.ron");
    println!();

    // Example 4: Unclosed string
    let example4 = r#"(
    message: "Hello, world
)"#;
    println!("--- Example 4: Unclosed string ---");
    parse_ron(example4, "config.ron");
    println!();

    // Example 5: Trailing content
    let example5 = r#"(value: 42) extra"#;
    println!("--- Example 5: Trailing content ---");
    parse_ron(example5, "config.ron");
    println!();

    // Example 6: Unclosed block comment
    let example6 = r#"/* This comment
is never closed
(value: 42)"#;
    println!("--- Example 6: Unclosed block comment ---");
    parse_ron(example6, "config.ron");
    println!();

    // Example 7: Valid RON (should succeed)
    let example7 = r#"(
    name: "Valid Config",
    enabled: true,
    count: 42,
)"#;
    println!("--- Example 7: Valid RON ---");
    if let Some(doc) = parse_ron(example7, "config.ron") {
        println!("Successfully parsed!\n{:#?}\n", doc.value);
    }

    // Example 8: Multi-label example using AST spans
    // This demonstrates how a schema validator would report type errors
    // by extracting actual spans from the parsed AST.
    println!("--- Example 8: Multi-label report from AST ---");
    let example8 = r#"GameConfig(
    player: Player(
        name: "Hero",
        health: 100,
        inventory: [
            "sword",
            "shield",
            42,
        ],
    ),
)"#;

    if let Ok(doc) = parse_document(example8) {
        // Navigate the AST to find the spans we need
        // In a real validator, this would come from schema checking
        use ron2::ast::{Expr, StructBody};

        // Helper to find a field by name
        fn find_field_value<'a>(body: &'a StructBody<'a>, name: &str) -> Option<&'a Expr<'a>> {
            match body {
                StructBody::Tuple(t) => {
                    // Tuple elements with named fields - look for AnonStruct pattern
                    t.elements.iter().find_map(|elem| {
                        if let Expr::AnonStruct(anon) = &elem.expr {
                            anon.fields
                                .iter()
                                .find(|f| f.name.name == name)
                                .map(|f| &f.value)
                        } else {
                            None
                        }
                    })
                }
                StructBody::Fields(f) => f
                    .fields
                    .iter()
                    .find(|field| field.name.name == name)
                    .map(|field| &field.value),
            }
        }

        if let Some(Expr::Struct(game_config)) = &doc.value {
            if let Some(body) = &game_config.body {
                // GameConfig has a `player` field
                if let Some(Expr::Struct(player)) = find_field_value(body, "player") {
                    let player_span = &player.span;

                    if let Some(player_body) = &player.body {
                        // Player has an `inventory` field
                        if let Some(Expr::Seq(inventory)) =
                            find_field_value(player_body, "inventory")
                        {
                            let inv_span = &inventory.span;

                            // Find the `42` (3rd item in array, index 2)
                            if let Some(bad_item) = inventory.items.get(2) {
                                let bad_span = bad_item.expr.span();

                                // Build a rich error report using actual AST spans
                                Report::build(ReportKind::Error, "game.ron", bad_span.start_offset)
                                    .with_message("Type mismatch in array")
                                    .with_label(
                                        Label::new((
                                            "game.ron",
                                            bad_span.start_offset..bad_span.end_offset,
                                        ))
                                        .with_message("expected String, found integer")
                                        .with_color(Color::Red),
                                    )
                                    .with_label(
                                        Label::new((
                                            "game.ron",
                                            inv_span.start_offset..inv_span.end_offset,
                                        ))
                                        .with_message("in this array")
                                        .with_color(Color::Blue),
                                    )
                                    .with_label(
                                        Label::new((
                                            "game.ron",
                                            player_span.start_offset..player_span.end_offset,
                                        ))
                                        .with_message("in field `player`")
                                        .with_color(Color::Cyan),
                                    )
                                    .with_note("Arrays should contain elements of the same type")
                                    .with_help(
                                        "Remove the integer or convert it to a string: \"42\"",
                                    )
                                    .with_config(Config::default().with_tab_width(4))
                                    .finish()
                                    .print(("game.ron", Source::from(example8)))
                                    .expect("Failed to print");
                                println!();
                            }
                        }
                    }
                }
            }
        }
    } else {
        println!("Failed to parse example8");
    }

    // Interactive mode: parse from file if provided
    if let Some(path) = std::env::args().nth(1) {
        println!("--- Parsing file: {} ---", path);
        match fs::read_to_string(&path) {
            Ok(content) => {
                if parse_ron(&content, &path).is_some() {
                    println!("File parsed successfully!");
                }
            }
            Err(e) => println!("Failed to read file: {}", e),
        }
    }
}
