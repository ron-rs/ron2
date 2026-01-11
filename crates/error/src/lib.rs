//! Error types for RON (Rusty Object Notation).
//!
//! This crate provides shared error types used across the ron-extras workspace:
//! - [`Span`] and [`Position`] for source location tracking
//! - [`ValidationError`] and [`ValidationErrorKind`] for validation errors
//! - [`PathSegment`] for error context paths
//!
//! All error types rely on `alloc`/`std` and are always available in this crate.

#![deny(clippy::correctness)]
#![deny(clippy::suspicious)]
#![deny(clippy::complexity)]
#![deny(clippy::perf)]
#![deny(clippy::style)]
#![warn(clippy::pedantic)]
#![deny(unsafe_code)]

extern crate alloc;

mod span;

mod path;

mod error;

pub use span::{Position, Span};

pub use path::PathSegment;

pub use error::{ValidationError, ValidationErrorKind};
