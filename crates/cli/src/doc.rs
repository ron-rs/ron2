//! Generate Markdown documentation from RON schema files.

use std::{
    fs,
    path::{Path, PathBuf},
};

use anyhow::{Context, Result};
use clap::Args;
use ron2_doc::{
    DocConfig, DocGenerator, OutputFormat, OutputMode, discover_schemas, link::slugify,
};

/// Generate documentation from RON schema files
#[derive(Args, Debug)]
#[command(
    after_help = "With --format starlight, links are root-relative routes. They are derived \
from an --output inside src/content/docs (e.g. src/content/docs/reference -> /reference); \
otherwise pass --base-url with the route the output is served under, including any Astro base."
)]
pub struct DocArgs {
    /// Input path (directory or single .schema.ron file)
    #[arg(value_name = "INPUT")]
    pub input: PathBuf,

    /// Output path (directory for multi-page, or .md file for single-page)
    #[arg(short, long, value_name = "PATH")]
    pub output: PathBuf,

    /// Base URL for cross-type links (e.g., "/docs/types"); plain output uses relative links without it
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

    let base_url = match (&args.base_url, args.format) {
        (None, OutputFormat::Starlight) if !args.single_page => {
            Some(starlight_base(&args.output).with_context(|| {
                format!(
                    "--format starlight needs --base-url: {} is not inside src/content/docs, \
                     so the route it is served under is unknown",
                    args.output.display()
                )
            })?)
        }
        _ => args.base_url,
    };

    let config = DocConfig {
        input: args.input,
        output: args.output.clone(),
        base_url,
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

/// The Starlight route of an output directory inside `src/content/docs`.
fn starlight_base(output: &Path) -> Option<String> {
    let absolute = std::env::current_dir().ok()?.join(output);
    let segments: Vec<String> = absolute
        .components()
        .map(|c| c.as_os_str().to_string_lossy().into_owned())
        .collect();
    let docs = segments
        .windows(3)
        .rposition(|w| w == ["src", "content", "docs"])?;
    let route: Vec<String> = segments[docs + 3..]
        .iter()
        .filter(|s| *s != ".")
        .map(|s| slugify(s))
        .collect();
    Some(format!("/{}", route.join("/")))
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn starlight_base_from_content_docs() {
        assert_eq!(
            starlight_base(Path::new("site/src/content/docs/Reference/types")).as_deref(),
            Some("/reference/types")
        );
        assert_eq!(
            starlight_base(Path::new("/abs/src/content/docs")).as_deref(),
            Some("/")
        );
        assert_eq!(starlight_base(Path::new("docs/out")), None);
    }
}
