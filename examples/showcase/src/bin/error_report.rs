//! Example demonstrating beautiful error reporting with ariadne.
//!
//! Shows how to print both:
//! - RON parsing errors (from ron2 library)
//! - Custom validation errors (user-defined logic)
//!
//! Run with: `cargo run --bin error_report`

use ariadne::{Color, Label, Report, ReportKind, Source};
use ron2::{Error as RonError, FromRon, Span, Spanned};
use ron2_derive::FromRon;

// Simple config struct with validation requirements
#[derive(Debug, FromRon)]
struct ServerConfig {
    host: Spanned<String>,
    port: Spanned<u16>,
    max_connections: Spanned<u32>,
    min_connections: Spanned<u32>,
}

// Custom validation error type
#[derive(Debug)]
struct ValidationError {
    message: String,
    span: Span,
    help: Option<String>,
}

impl ValidationError {
    fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span,
            help: None,
        }
    }

    fn with_help(mut self, help: impl Into<String>) -> Self {
        self.help = Some(help.into());
        self
    }
}

// Convert RON error to ariadne Report
fn ron_error_to_report<'a>(
    error: &RonError,
    source_name: &'a str,
) -> Report<'a, (&'a str, std::ops::Range<usize>)> {
    let span = error.span();
    let range = span.start_offset..span.end_offset;

    let mut report = Report::build(ReportKind::Error, (source_name, range.clone()))
        .with_message(error.kind().to_string());

    // Add main error label
    report = report.with_label(
        Label::new((source_name, range.clone()))
            .with_message(error.kind().to_string())
            .with_color(Color::Red),
    );

    // Add suggestion if available
    if let Some(suggestion) = error.suggestion() {
        report = report.with_help(format!("try: {}", suggestion));
    }

    // Add context from error path
    if !error.path().is_empty() {
        let context = error
            .path()
            .iter()
            .map(|seg| seg.to_string())
            .collect::<Vec<_>>()
            .join(" -> ");
        report = report.with_note(format!("in {}", context));
    }

    report.finish()
}

// Convert validation error to ariadne Report
fn validation_error_to_report<'a>(
    error: &ValidationError,
    source_name: &'a str,
) -> Report<'a, (&'a str, std::ops::Range<usize>)> {
    let range = error.span.start_offset..error.span.end_offset;

    let mut report = Report::build(ReportKind::Error, (source_name, range.clone()))
        .with_message(&error.message)
        .with_label(
            Label::new((source_name, range))
                .with_message(&error.message)
                .with_color(Color::Yellow),
        );

    if let Some(help) = &error.help {
        report = report.with_help(help);
    }

    report.finish()
}

// Validate config and return validation errors
fn validate_config(config: &ServerConfig) -> Vec<ValidationError> {
    let mut errors = Vec::new();

    // Check port range
    if config.port.value == 0 {
        errors.push(
            ValidationError::new("port cannot be 0", config.port.span.clone())
                .with_help("valid port range is 1-65535"),
        );
    }

    // Check min/max connection logic
    if config.max_connections.value < config.min_connections.value {
        errors.push(
            ValidationError::new(
                format!(
                    "max_connections ({}) must be >= min_connections ({})",
                    config.max_connections.value, config.min_connections.value
                ),
                config.max_connections.span.clone(),
            )
            .with_help("increase max_connections or decrease min_connections"),
        );
    }

    // Check reasonable connection limits
    if config.max_connections.value > 10000 {
        errors.push(
            ValidationError::new(
                "max_connections seems unusually high",
                config.max_connections.span.clone(),
            )
            .with_help("consider a more reasonable limit (< 10000)"),
        );
    }

    errors
}

fn main() {
    // Example 1: RON parsing error
    println!("=== Example 1: RON Parsing Error ===\n");

    let invalid_ron = r#"
(
    host: "localhost",
    port: 808x,  // Invalid: 'x' in number
    max_connections: 100,
    min_connections: 10,
)
"#;

    match ServerConfig::from_ron(invalid_ron) {
        Ok(_) => println!("Parsed successfully (unexpected)"),
        Err(error) => {
            let report = ron_error_to_report(&error, "config.ron");
            report
                .eprint(("config.ron", Source::from(invalid_ron)))
                .unwrap();
        }
    }

    println!("\n");

    // Example 2: Custom validation errors
    println!("=== Example 2: Custom Validation Errors ===\n");

    let valid_ron_invalid_config = r#"
(
    host: "localhost",
    port: 0,  // Invalid: port cannot be 0
    max_connections: 10,
    min_connections: 100,  // Invalid: min > max
)
"#;

    match ServerConfig::from_ron(valid_ron_invalid_config) {
        Ok(config) => {
            let validation_errors = validate_config(&config);

            if !validation_errors.is_empty() {
                for error in &validation_errors {
                    let report = validation_error_to_report(error, "config.ron");
                    report
                        .eprint(("config.ron", Source::from(valid_ron_invalid_config)))
                        .unwrap();
                    println!(); // Blank line between errors
                }
            } else {
                println!("✓ Config is valid!");
            }
        }
        Err(error) => {
            let report = ron_error_to_report(&error, "config.ron");
            report
                .eprint(("config.ron", Source::from(valid_ron_invalid_config)))
                .unwrap();
        }
    }
}
