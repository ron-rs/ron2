//! ron2-doc - Generate Markdown documentation from RON schema files.
//!
//! This crate provides functionality to generate Markdown documentation
//! from RON schema files, with support for Astro Starlight frontmatter.

pub mod config;
pub mod discovery;
pub mod example;
pub mod generator;
pub mod index;
pub mod link;
pub mod single_page;

pub use config::{DocConfig, OutputFormat, OutputMode};
pub use discovery::{DiscoveredSchema, discover_schemas};
pub use generator::{LinkMode, generate_markdown};
pub use index::{generate_index, generate_sidebar};
pub use link::LinkResolver;
pub use single_page::generate_single_page;
