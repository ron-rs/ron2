//! Configuration types for documentation generation.

use std::path::PathBuf;

/// Output format for generated documentation.
#[derive(Debug, Clone, Copy, Default, clap::ValueEnum)]
pub enum OutputFormat {
    /// Standard markdown without frontmatter
    #[default]
    Plain,
    /// Astro Starlight format with YAML frontmatter
    Starlight,
}

/// Output mode for documentation generation.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum OutputMode {
    /// Generate one markdown file per type
    #[default]
    MultiPage,
    /// Generate all documentation in a single markdown file
    SinglePage,
}

/// Configuration for documentation generation.
#[derive(Debug, Clone)]
pub struct DocConfig {
    /// Input path (directory or single file)
    pub input: PathBuf,
    /// Output directory for markdown files (or single file path for SinglePage mode)
    pub output: PathBuf,
    /// Base URL for cross-type links (e.g., "/docs/types")
    pub base_url: Option<String>,
    /// Output format
    pub format: OutputFormat,
    /// Output mode (multi-page or single-page)
    pub output_mode: OutputMode,
    /// Maximum depth for TypeRef expansion in examples
    pub example_depth: usize,
    /// Whether to generate index files
    pub generate_index: bool,
}
