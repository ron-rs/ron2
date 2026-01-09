//! ronfmt - A formatter for RON (Rusty Object Notation) files
//!
//! This tool formats RON files with consistent style while preserving comments.

use std::fs;
use std::io::{self, Read, Write};
use std::path::PathBuf;
use std::process::ExitCode;

use anyhow::{Context, Result};
use clap::Parser;
use ron2::ast::{format_document, parse_document, FormatConfig};

/// A formatter for RON (Rusty Object Notation) files
#[derive(Parser, Debug)]
#[command(name = "ronfmt")]
#[command(version, about, long_about = None)]
struct Args {
    /// Input file (reads from stdin if not provided)
    #[arg(value_name = "FILE")]
    input: Option<PathBuf>,

    /// Number of spaces per indentation level
    #[arg(short, long, default_value = "4", value_name = "N")]
    indent: usize,

    /// Character limit for compact formatting
    #[arg(short, long, default_value = "80", value_name = "N")]
    width: usize,

    /// Check if file is formatted correctly (exit 1 if not)
    #[arg(short, long)]
    check: bool,

    /// Write output to file instead of stdout
    #[arg(short, long, value_name = "FILE")]
    output: Option<PathBuf>,
}

fn main() -> ExitCode {
    match run() {
        Ok(formatted_correctly) => {
            if formatted_correctly {
                ExitCode::SUCCESS
            } else {
                ExitCode::from(1)
            }
        }
        Err(e) => {
            eprintln!("error: {e:#}");
            ExitCode::from(2)
        }
    }
}

fn run() -> Result<bool> {
    let args = Args::parse();

    // Read input
    let (source, source_name) = match &args.input {
        Some(path) => {
            let content = fs::read_to_string(path)
                .with_context(|| format!("failed to read {}", path.display()))?;
            (content, path.display().to_string())
        }
        None => {
            let mut content = String::new();
            io::stdin()
                .read_to_string(&mut content)
                .context("failed to read from stdin")?;
            (content, "<stdin>".to_string())
        }
    };

    // Parse the document
    let doc = parse_document(&source)
        .map_err(|e| anyhow::anyhow!("{}: parse error: {}", source_name, e))?;

    // Build format config
    let indent_str = " ".repeat(args.indent);
    let config = FormatConfig::new()
        .indent(indent_str)
        .char_limit(args.width);

    // Format
    let formatted = format_document(&doc, &config);

    // Check mode: compare and report
    if args.check {
        if formatted == source {
            return Ok(true);
        }
        eprintln!("{source_name}: not formatted correctly");
        return Ok(false);
    }

    // Write output
    match &args.output {
        Some(path) => {
            // Don't overwrite if content is the same
            if let Ok(existing) = fs::read_to_string(path) {
                if existing == formatted {
                    return Ok(true);
                }
            }
            fs::write(path, &formatted)
                .with_context(|| format!("failed to write to {}", path.display()))?;
        }
        None => {
            // If input file specified and no output, write back to input file
            if let Some(input_path) = &args.input {
                if source != formatted {
                    fs::write(input_path, &formatted)
                        .with_context(|| format!("failed to write to {}", input_path.display()))?;
                }
            } else {
                // stdin -> stdout
                io::stdout()
                    .write_all(formatted.as_bytes())
                    .context("failed to write to stdout")?;
            }
        }
    }

    Ok(true)
}
