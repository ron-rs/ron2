//! Generate Markdown documentation from RON schema files.

use std::fs;
use std::path::PathBuf;

use anyhow::{Context, Result};
use clap::Args;
use ron2_doc::link::type_path_to_md_path;
use ron2_doc::{
    discover_schemas, generate_index, generate_markdown, generate_sidebar, generate_single_page,
    DocConfig, OutputFormat, OutputMode,
};

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
    // Discover schemas
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

    // Build configuration
    let config = DocConfig {
        input: args.input,
        output: args.output.clone(),
        base_url: args.base_url,
        format: args.format,
        output_mode,
        example_depth: args.example_depth,
        generate_index: !args.no_index,
    };

    if args.single_page {
        // Single-page mode: write all docs to one file
        let content = generate_single_page(&schemas, &config);

        // Ensure parent directory exists
        if let Some(parent) = args.output.parent() {
            if !parent.as_os_str().is_empty() {
                fs::create_dir_all(parent)
                    .with_context(|| format!("failed to create directory: {}", parent.display()))?;
            }
        }

        fs::write(&args.output, &content)
            .with_context(|| format!("failed to write {}", args.output.display()))?;

        eprintln!("  Generated: {}", args.output.display());
    } else {
        // Multi-page mode: one file per type
        fs::create_dir_all(&args.output).with_context(|| {
            format!(
                "failed to create output directory: {}",
                args.output.display()
            )
        })?;

        // Generate documentation for each schema
        for schema in &schemas {
            let content = generate_markdown(schema, &config, &schemas);
            let relative_path = type_path_to_md_path(&schema.type_path);
            let output_path = args.output.join(&relative_path);

            // Create parent directories
            if let Some(parent) = output_path.parent() {
                fs::create_dir_all(parent)
                    .with_context(|| format!("failed to create directory: {}", parent.display()))?;
            }

            fs::write(&output_path, &content)
                .with_context(|| format!("failed to write {}", output_path.display()))?;

            eprintln!("  Generated: {}", relative_path);
        }

        // Generate index files
        if config.generate_index {
            let index_content = generate_index(&schemas, config.format);
            let index_path = args.output.join("index.md");
            fs::write(&index_path, index_content)
                .with_context(|| format!("failed to write {}", index_path.display()))?;
            eprintln!("  Generated: index.md");

            if matches!(config.format, OutputFormat::Starlight) {
                let sidebar_content = generate_sidebar(&schemas);
                let sidebar_path = args.output.join("sidebar.json");
                fs::write(&sidebar_path, sidebar_content)
                    .with_context(|| format!("failed to write {}", sidebar_path.display()))?;
                eprintln!("  Generated: sidebar.json");
            }
        }
    }

    eprintln!("\nDocumentation generated successfully!");

    Ok(())
}
