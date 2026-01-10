//! Error types for RON (Rusty Object Notation).
//!
//! This crate provides shared error types used across the ron-extras workspace:
//! - [`Span`] and [`Position`] for source location tracking
//! - [`ValidationError`] and [`ValidationErrorKind`] for validation errors
//! - [`PathSegment`] for error context paths
//!
//! # Features
//!
//! - `alloc` (default) - Enables [`ValidationError`], [`ValidationErrorKind`], and [`PathSegment`]
//! - `std` - Enables `std::error::Error` implementations
//!
//! # `no_std` Support
//!
//! Without the `alloc` feature, only [`Span`] and [`Position`] are available.

#![cfg_attr(not(feature = "std"), no_std)]
#![deny(clippy::correctness)]
#![deny(clippy::suspicious)]
#![deny(clippy::complexity)]
#![deny(clippy::perf)]
#![deny(clippy::style)]
#![warn(clippy::pedantic)]
#![deny(unsafe_code)]

#[cfg(feature = "alloc")]
extern crate alloc;

mod span;

#[cfg(feature = "alloc")]
mod path;

#[cfg(feature = "alloc")]
mod error;

pub use span::{Position, Span};

#[cfg(feature = "alloc")]
pub use path::PathSegment;

#[cfg(feature = "alloc")]
pub use error::{ValidationError, ValidationErrorKind};
