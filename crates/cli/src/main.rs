//! ron2-cli - CLI tools for RON (Rusty Object Notation)
//!
//! This tool provides formatting and documentation generation for RON files.

use std::process::ExitCode;

use clap::{Parser, Subcommand};

mod doc;
mod fmt;

/// CLI tools for RON (Rusty Object Notation)
#[derive(Parser)]
#[command(name = "ron")]
#[command(version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Format RON files
    Fmt(fmt::FmtArgs),
    /// Generate documentation from RON schema files
    Doc(doc::DocArgs),
}

fn main() -> ExitCode {
    match run() {
        Ok(code) => code,
        Err(e) => {
            eprintln!("error: {e:#}");
            ExitCode::from(2)
        }
    }
}

fn run() -> anyhow::Result<ExitCode> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Fmt(args) => fmt::run(args),
        Commands::Doc(args) => {
            doc::run(args)?;
            Ok(ExitCode::SUCCESS)
        }
    }
}
