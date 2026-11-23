//! Generate Markdown documentation from RON schema files.

use std::{
    fs,
    path::{Path, PathBuf},
};

use anyhow::{Context, Result};
use clap::Args;
use ron2_doc::{DocConfig, DocGenerator, OutputFormat, OutputMode, discover_schemas};

/// Generate documentation from RON schema files
#[derive(Args, Debug)]
pub struct DocArgs {
    /// Input path (directory or single .schema.ron file)
    #[arg(value_name = "INPUT")]
    pub input: PathBuf,

    /// Output path (directory for multi-page, or .md file for single-page)
    #[arg(short, long, value_name = "PATH")]
    pub output: PathBuf,

    /// Base URL for cross-type links (e.g., "/docs/types")
    #[arg(long, value_name = "URL")]
    pub base_url: Option<String>,

    /// Output format
    #[arg(long, value_enum, default_value = "plain")]
    pub format: OutputFormat,

    /// Generate all documentation in a single markdown file
    #[arg(long)]
    pub single_page: bool,

    /// Maximum depth for TypeRef expansion in examples
    #[arg(long, default_value = "2", value_name = "N")]
    pub example_depth: usize,

    /// Skip generating index files (multi-page mode only)
    #[arg(long)]
    pub no_index: bool,
}

pub fn run(args: DocArgs) -> Result<()> {
    let schemas = discover_schemas(&args.input)
        .with_context(|| format!("failed to discover schemas in {}", args.input.display()))?;

    if schemas.is_empty() {
        anyhow::bail!("no schema files found in {}", args.input.display());
    }

    eprintln!("Found {} schema(s)", schemas.len());

    let output_mode = if args.single_page {
        OutputMode::SinglePage
    } else {
        OutputMode::MultiPage
    };

    let config = DocConfig {
        input: args.input,
        output: args.output.clone(),
        base_url: args.base_url,
        format: args.format,
        output_mode,
        example_depth: args.example_depth,
        generate_index: !args.no_index,
    };

    let generator = DocGenerator::new(&schemas, &config);

    if args.single_page {
        write(&args.output, &generator.single_page())?;
        eprintln!("  Generated: {}", args.output.display());
    } else {
        for schema in &schemas {
            let relative_path = generator.page_path(schema);
            write(
                &args.output.join(&relative_path),
                &generator.type_page(schema),
            )?;
            eprintln!("  Generated: {relative_path}");
        }

        if config.generate_index {
            write(&args.output.join("index.md"), &generator.index())?;
            eprintln!("  Generated: index.md");

            if matches!(config.format, OutputFormat::Starlight) {
                write(&args.output.join("sidebar.json"), &generator.sidebar())?;
                eprintln!("  Generated: sidebar.json");
            }
        }
    }

    eprintln!("\nDocumentation generated successfully!");

    Ok(())
}

/// Write a file, creating its parent directories.
fn write(path: &Path, content: &str) -> Result<()> {
    if let Some(parent) = path.parent()
        && !parent.as_os_str().is_empty()
    {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create directory: {}", parent.display()))?;
    }
    fs::write(path, content).with_context(|| format!("failed to write {}", path.display()))
}
