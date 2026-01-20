//! Spanned values that capture source location information during deserialization.
//!
//! The [`Spanned`] wrapper type pairs a deserialized value with its source location
//! [`Span`], enabling precise error reporting, IDE integration, and configuration
//! validation.
//!
//! # Use Cases
//!
//! - **Configuration validation with precise errors**
//!   ```text
//!   Error: Port 99999 at line 5, column 10 exceeds valid range (1-65535)
//!   ```
//!
//! - **Cross-field validation**
//!   ```text
//!   Error: max_connections (10) at line 8 must be >= min_connections (20) at line 7
//!   ```
//!
//! - **IDE/LSP integration**
//!   - Jump to definition of configuration values
//!   - Hover to see where a setting is defined
//!   - Rename refactoring for config keys
//!
//! - **Configuration merge tracking**
//!   - Track which file each setting came from
//!   - Implement "config explain" commands
//!
//! # Quick Start
//!
//! ```
//! use ron2::{FromRon, Spanned};
//!
//! #[derive(FromRon, Debug)]
//! struct ServerConfig {
//!     host: Spanned<String>,
//!     port: Spanned<u16>,
//! }
//!
//! let ron = r#"
//! (
//!     host: "localhost",
//!     port: 8080,
//! )
//! "#;
//!
//! let config: ServerConfig = ServerConfig::from_ron(ron).unwrap();
//!
//! // Access the value
//! assert_eq!(*config.port, 8080);
//! assert_eq!(config.port.value, 8080);
//!
//! // Access the span
//! let span = config.port.span();
//! println!("Port defined at line {}, column {}",
//!          span.start.line, span.start.col);
//! ```
//!
//! # Option Patterns
//!
//! There are two patterns for optional spanned values with different semantics:
//!
//! ## `Option<Spanned<T>>` - Span only when present
//!
//! Use when you only care about the span of present values:
//!
//! ```
//! use ron2::{FromRon, Spanned};
//!
//! #[derive(FromRon)]
//! struct Config {
//!     name: Option<Spanned<String>>,  // None when field is absent
//! }
//! ```
//!
//! Given:
//! ```ron
//! (name: "Alice")  // Some(Spanned { value: "Alice", span: <line 1> })
//! ```
//!
//! Or:
//! ```ron
//! ()  // name is None (no span, value is absent)
//! ```
//!
//! ## `Spanned<Option<T>>` - Always capture span
//!
//! Use when you want to track the location of the `None` keyword itself:
//!
//! ```
//! use ron2::{FromRon, Spanned};
//!
//! #[derive(FromRon)]
//! struct Config {
//!     name: Spanned<Option<String>>,  // Span even for None
//! }
//! ```
//!
//! Given:
//! ```ron
//! (name: Some("Alice"))  // Spanned { value: Some("Alice"), span: <line 1> }
//! ```
//!
//! Or:
//! ```ron
//! (name: None)  // Spanned { value: None, span: <line 1> } - captures None's location!
//! ```
//!
//! # Collections
//!
//! `Spanned<T>` works seamlessly with collections:
//!
//! ```
//! use ron2::{FromRon, Spanned};
//! use std::collections::HashMap;
//!
//! #[derive(FromRon)]
//! struct Config {
//!     ports: Vec<Spanned<u16>>,                    // Each element has a span
//!     flags: HashMap<String, Spanned<bool>>,       // Each value has a span
//! }
//! ```
//!
//! # Error Reporting Example
//!
//! ```
//! use ron2::{FromRon, Spanned};
//!
//! #[derive(FromRon)]
//! struct ServerConfig {
//!     port: Spanned<u16>,
//!     max_connections: Spanned<u32>,
//!     min_connections: Spanned<u32>,
//! }
//!
//! fn validate_config(config: &ServerConfig) -> Result<(), String> {
//!     // Validate port range
//!     if config.port.value == 0 {
//!         return Err(format!(
//!             "Invalid port 0 at line {}, column {}",
//!             config.port.span.start.line,
//!             config.port.span.start.col
//!         ));
//!     }
//!
//!     // Cross-field validation with both locations
//!     if config.max_connections.value < config.min_connections.value {
//!         return Err(format!(
//!             "max_connections ({}) at line {} must be >= min_connections ({}) at line {}",
//!             config.max_connections.value,
//!             config.max_connections.span.start.line,
//!             config.min_connections.value,
//!             config.min_connections.span.start.line,
//!         ));
//!     }
//!
//!     Ok(())
//! }
//! ```
//!
//! # Performance
//!
//! [`Span`] is a small type (32 bytes on 64-bit platforms) and implements [`Copy`],
//! so cloning during deserialization is very cheap. The overhead of capturing spans
//! is minimal compared to the parsing cost itself.

use core::ops::{Deref, DerefMut};

use crate::{
    ast::Expr,
    convert::{FromRon, ToRon},
    error::{Result, Span},
};

/// A value paired with its source location span.
///
/// Captures both the deserialized value and the exact location (line, column,
/// byte offsets) where it appeared in the RON source.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Spanned<T> {
    /// The deserialized value.
    pub value: T,
    /// The source location span.
    pub span: Span,
}

impl<T> Spanned<T> {
    /// Create a new spanned value.
    ///
    /// # Example
    ///
    /// ```
    /// use ron2::{Spanned, error::{Span, Position}};
    ///
    /// let span = Span {
    ///     start: Position { line: 1, column: 1 },
    ///     end: Position { line: 1, column: 5 },
    ///     start_offset: 0,
    ///     end_offset: 4,
    /// };
    ///
    /// let spanned = Spanned::new(42, span);
    /// assert_eq!(spanned.value, 42);
    /// ```
    pub fn new(value: T, span: Span) -> Self {
        Self { value, span }
    }

    /// Get a reference to the inner value.
    ///
    /// Alternatively, you can use the [`Deref`] implementation.
    ///
    /// # Example
    ///
    /// ```
    /// # use ron2::{Spanned, error::{Span, Position}};
    /// # let span = Span {
    /// #     start: Position { line: 1, column: 1 },
    /// #     end: Position { line: 1, column: 5 },
    /// #     start_offset: 0,
    /// #     end_offset: 4,
    /// # };
    /// let spanned = Spanned::new(42, span);
    /// assert_eq!(*spanned.get(), 42);
    /// assert_eq!(*spanned, 42);  // via Deref
    /// ```
    pub fn get(&self) -> &T {
        &self.value
    }

    /// Get a mutable reference to the inner value.
    ///
    /// Alternatively, you can use the [`DerefMut`] implementation.
    ///
    /// # Example
    ///
    /// ```
    /// # use ron2::{Spanned, error::{Span, Position}};
    /// # let span = Span {
    /// #     start: Position { line: 1, column: 1 },
    /// #     end: Position { line: 1, column: 5 },
    /// #     start_offset: 0,
    /// #     end_offset: 4,
    /// # };
    /// let mut spanned = Spanned::new(42, span);
    /// *spanned.get_mut() = 100;
    /// assert_eq!(spanned.value, 100);
    /// ```
    pub fn get_mut(&mut self) -> &mut T {
        &mut self.value
    }

    /// Consume the wrapper and return the inner value, discarding the span.
    ///
    /// # Example
    ///
    /// ```
    /// # use ron2::{Spanned, error::{Span, Position}};
    /// # let span = Span {
    /// #     start: Position { line: 1, column: 1 },
    /// #     end: Position { line: 1, column: 5 },
    /// #     start_offset: 0,
    /// #     end_offset: 4,
    /// # };
    /// let spanned = Spanned::new(42, span);
    /// let value = spanned.into_inner();
    /// assert_eq!(value, 42);
    /// ```
    pub fn into_inner(self) -> T {
        self.value
    }

    /// Map the value while preserving the span.
    ///
    /// # Example
    ///
    /// ```
    /// # use ron2::{Spanned, error::{Span, Position}};
    /// # let span = Span {
    /// #     start: Position { line: 1, column: 1 },
    /// #     end: Position { line: 1, column: 5 },
    /// #     start_offset: 0,
    /// #     end_offset: 4,
    /// # };
    /// let spanned = Spanned::new(42, span);
    /// let doubled = spanned.map(|x| x * 2);
    /// assert_eq!(doubled.value, 84);
    /// assert_eq!(doubled.span, span);
    /// ```
    pub fn map<U, F>(self, f: F) -> Spanned<U>
    where
        F: FnOnce(T) -> U,
    {
        Spanned {
            value: f(self.value),
            span: self.span,
        }
    }

    /// Try to map the value, preserving span on success.
    ///
    /// # Example
    ///
    /// ```
    /// # use ron2::{Spanned, error::{Span, Position}};
    /// # let span = Span {
    /// #     start: Position { line: 1, column: 1 },
    /// #     end: Position { line: 1, column: 5 },
    /// #     start_offset: 0,
    /// #     end_offset: 4,
    /// # };
    /// let spanned = Spanned::new("42", span);
    /// let parsed: Result<Spanned<i32>, _> = spanned.try_map(|s| s.parse());
    /// assert_eq!(parsed.unwrap().value, 42);
    /// ```
    pub fn try_map<U, E, F>(self, f: F) -> Result<Spanned<U>, E>
    where
        F: FnOnce(T) -> Result<U, E>,
    {
        Ok(Spanned {
            value: f(self.value)?,
            span: self.span,
        })
    }

    /// Get a reference to the span.
    ///
    /// # Example
    ///
    /// ```
    /// # use ron2::{Spanned, error::{Span, Position}};
    /// # let span = Span {
    /// #     start: Position { line: 1, column: 1 },
    /// #     end: Position { line: 1, column: 5 },
    /// #     start_offset: 0,
    /// #     end_offset: 4,
    /// # };
    /// let spanned = Spanned::new(42, span);
    /// assert_eq!(spanned.span().start.line, 1);
    /// ```
    pub fn span(&self) -> &Span {
        &self.span
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

impl<T: FromRon> FromRon for Spanned<T> {
    fn from_ast(expr: &Expr<'_>) -> Result<Self> {
        Ok(Spanned {
            value: T::from_ast(expr)?,
            span: *expr.span(),
        })
    }
}

impl<T: ToRon> ToRon for Spanned<T> {
    fn to_ast(&self) -> Result<Expr<'static>> {
        // Serialize only the value, discard span
        self.value.to_ast()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error::Position;

    fn make_span(line: usize, col_val: usize) -> Span {
        Span {
            start: Position { line, col: col_val },
            end: Position {
                line,
                col: col_val + 1,
            },
            start_offset: 0,
            end_offset: 1,
        }
    }

    #[test]
    fn test_new_and_accessors() {
        let span = make_span(1, 1);
        let spanned = Spanned::new(42, span);

        assert_eq!(spanned.value, 42);
        assert_eq!(*spanned.get(), 42);
        assert_eq!(*spanned, 42); // Deref
        assert_eq!(*spanned.span(), span);
    }

    #[test]
    fn test_get_mut() {
        let span = make_span(1, 1);
        let mut spanned = Spanned::new(42, span);

        *spanned.get_mut() = 100;
        assert_eq!(spanned.value, 100);

        *spanned = 200; // DerefMut
        assert_eq!(spanned.value, 200);
    }

    #[test]
    fn test_into_inner() {
        let span = make_span(1, 1);
        let spanned = Spanned::new(42, span);
        let value = spanned.into_inner();
        assert_eq!(value, 42);
    }

    #[test]
    fn test_map() {
        let span = make_span(1, 1);
        let spanned = Spanned::new(42, span);
        let doubled = spanned.map(|x| x * 2);

        assert_eq!(doubled.value, 84);
        assert_eq!(doubled.span, span);
    }

    #[test]
    fn test_try_map_success() {
        let span = make_span(1, 1);
        let spanned = Spanned::new("42", span);
        let parsed: core::result::Result<Spanned<i32>, _> = spanned.try_map(str::parse);

        let parsed = parsed.unwrap();
        assert_eq!(parsed.value, 42);
        assert_eq!(parsed.span, span);
    }

    #[test]
    fn test_try_map_failure() {
        let span = make_span(1, 1);
        let spanned = Spanned::new("not a number", span);
        let parsed: core::result::Result<Spanned<i32>, _> = spanned.try_map(str::parse);

        assert!(parsed.is_err());
    }

    #[test]
    fn test_from_ron_primitive() {
        let ron = "42";
        let spanned: Spanned<i32> = Spanned::from_ron(ron).unwrap();

        assert_eq!(spanned.value, 42);
        assert_eq!(spanned.span.start.line, 1);
        assert_eq!(spanned.span.start.col, 1);
    }

    #[test]
    fn test_from_ron_string() {
        let ron = r#""hello""#;
        let spanned: Spanned<String> = Spanned::from_ron(ron).unwrap();

        assert_eq!(spanned.value, "hello");
        assert_eq!(spanned.span.start.line, 1);
    }

    #[test]
    fn test_from_ron_bool() {
        let ron = "true";
        let spanned: Spanned<bool> = Spanned::from_ron(ron).unwrap();

        assert!(spanned.value);
        assert_eq!(spanned.span.start.line, 1);
    }

    #[test]
    fn test_to_ron_discards_span() {
        let span = make_span(5, 10);
        let spanned = Spanned::new(42, span);

        let serialized = spanned.to_ron().unwrap();
        assert_eq!(serialized, "42");

        // Deserialize back and verify we get a new span at line 1
        let deserialized: Spanned<i32> = Spanned::from_ron(&serialized).unwrap();
        assert_eq!(deserialized.value, 42);
        assert_eq!(deserialized.span.start.line, 1); // New span, not line 5
    }

    #[test]
    fn test_roundtrip_value_preserved() {
        let spanned = Spanned::new("test string", make_span(1, 1));
        let serialized = spanned.to_ron().unwrap();
        let deserialized: Spanned<String> = Spanned::from_ron(&serialized).unwrap();

        assert_eq!(deserialized.value, "test string");
    }

    #[test]
    fn test_vec_of_spanned() {
        let ron = "[1, 2, 3]";
        let spanned_vec: Vec<Spanned<i32>> = Vec::from_ron(ron).unwrap();

        assert_eq!(spanned_vec.len(), 3);
        assert_eq!(spanned_vec[0].value, 1);
        assert_eq!(spanned_vec[1].value, 2);
        assert_eq!(spanned_vec[2].value, 3);

        // Each element should have its own span
        assert!(spanned_vec[0].span.start.col < spanned_vec[1].span.start.col);
        assert!(spanned_vec[1].span.start.col < spanned_vec[2].span.start.col);
    }

    #[test]
    fn test_option_spanned_some() {
        let ron = "Some(42)";
        let opt: Option<Spanned<i32>> = Option::from_ron(ron).unwrap();

        assert!(opt.is_some());
        let spanned = opt.unwrap();
        assert_eq!(spanned.value, 42);
        assert!(spanned.span.start.line > 0); // Has a real span
    }

    #[test]
    fn test_option_spanned_none() {
        let ron = "None";
        let opt: Option<Spanned<i32>> = Option::from_ron(ron).unwrap();

        assert!(opt.is_none()); // No span captured
    }

    #[test]
    fn test_spanned_option_some() {
        let ron = "Some(42)";
        let spanned: Spanned<Option<i32>> = Spanned::from_ron(ron).unwrap();

        assert_eq!(spanned.value, Some(42));
        assert!(spanned.span.start.line > 0); // Span for entire expression
    }

    #[test]
    fn test_spanned_option_none() {
        let ron = "None";
        let spanned: Spanned<Option<i32>> = Spanned::from_ron(ron).unwrap();

        assert_eq!(spanned.value, None);
        assert!(spanned.span.start.line > 0); // Span for None keyword!
    }

    #[test]
    fn test_implicit_some_with_option_spanned() {
        let ron = "42"; // Bare value
        let opt: Option<Spanned<i32>> = Option::from_ron(ron).unwrap();

        assert!(opt.is_some());
        let spanned = opt.unwrap();
        assert_eq!(spanned.value, 42);
    }

    #[test]
    fn test_implicit_some_with_spanned_option() {
        let ron = "42"; // Bare value
        let spanned: Spanned<Option<i32>> = Spanned::from_ron(ron).unwrap();

        assert_eq!(spanned.value, Some(42)); // Implicit Some
    }

    #[test]
    fn test_multiline_span() {
        let ron = r#"
"hello
world"
"#;
        let spanned: Spanned<String> = Spanned::from_ron(ron.trim()).unwrap();

        assert_eq!(spanned.value, "hello\nworld");
        // Span should cover multiple lines
        assert!(spanned.span.end.line > spanned.span.start.line);
    }

    #[test]
    fn test_clone() {
        let span = make_span(1, 1);
        let spanned = Spanned::new(42, span);
        let cloned = spanned.clone();

        assert_eq!(cloned.value, spanned.value);
        assert_eq!(cloned.span, spanned.span);
    }

    #[test]
    fn test_eq() {
        let span1 = make_span(1, 1);
        let span2 = make_span(2, 2);

        let spanned1 = Spanned::new(42, span1);
        let spanned2 = Spanned::new(42, span1);
        let spanned3 = Spanned::new(42, span2);
        let spanned4 = Spanned::new(100, span1);

        assert_eq!(spanned1, spanned2); // Same value and span
        assert_ne!(spanned1, spanned3); // Same value, different span
        assert_ne!(spanned1, spanned4); // Different value, same span
    }
}
