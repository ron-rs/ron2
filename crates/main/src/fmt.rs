//! Formatting configuration for RON output.
//!
//! This module provides types for customizing how RON is formatted:
//!
//! - [`FormatConfig`] - Main configuration struct with builder methods
//! - [`Spacing`] - Controls whitespace around punctuation
//! - [`Compaction`] - Controls when collections are written on single lines
//! - [`CommentMode`] - Controls comment preservation
//! - [`CompactTypes`] - Flags for which collection types can be compacted
//!
//! # Example
//!
//! ```
//! use ron2::fmt::{FormatConfig, Spacing};
//!
//! // Create a minimal format (no extra whitespace)
//! let minimal = FormatConfig::minimal();
//!
//! // Create a custom format
//! let custom = FormatConfig::default()
//!     .spacing(Spacing::None)
//!     .indent("  ");  // 2 spaces
//! ```

pub use crate::ast::{
    CommentMode, CompactTypes, Compaction, FormatConfig, Spacing, format_document, format_expr,
};
