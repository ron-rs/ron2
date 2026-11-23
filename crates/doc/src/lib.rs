//! ron2-doc - Generate Markdown documentation from RON schema files.
//!
//! This crate provides functionality to generate Markdown documentation
//! from RON schema files, with support for Astro Starlight frontmatter.

pub mod config;
pub mod discovery;
pub mod example;
mod generator;
mod index;
pub mod link;
mod single_page;

pub use config::{DocConfig, OutputFormat, OutputMode};
pub use discovery::{DiscoveredSchema, discover_schemas};
pub use generator::DocGenerator;
pub use link::{LinkFrom, LinkResolver};
