//! Span and position types for source location tracking.

use core::fmt;

/// A position in source text (line and column, 1-indexed).
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct Position {
    /// The line number (1-indexed).
    pub line: usize,
    /// The column number (1-indexed).
    pub col: usize,
}

impl Position {
    /// Create a position at line 1, column 1 (the start of a document).
    #[must_use]
    pub const fn start() -> Self {
        Self { line: 1, col: 1 }
    }

    /// Compute the position at the end of the given source text.
    #[must_use]
    pub fn from_src_end(src: &str) -> Self {
        let line = 1 + src.chars().filter(|&c| c == '\n').count();
        let col = 1 + src.chars().rev().take_while(|&c| c != '\n').count();

        Self { line, col }
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { line, col } = self;
        write!(f, "{line}:{col}")
    }
}

/// A span in source text, covering a range between two positions.
///
/// Spans are used to indicate the start and end positions of parsed elements.
/// They include both line/column positions and byte offsets for flexible use.
///
/// ## Byte Offset Semantics
///
/// Byte offsets use **exclusive end** semantics (like Rust ranges):
/// - `start_offset..end_offset` covers the half-open range `[start, end)`
/// - Use `source[start_offset..end_offset]` to slice the spanned text
///
/// ## Position Semantics
///
/// Line and column positions are **1-indexed** (the first line is line 1,
/// the first column is column 1). The end position points to the last
/// character in the span (inclusive for display purposes).
///
/// ## Synthetic Spans
///
/// Spans created by [`Span::synthetic()`] have `line: 0` to distinguish them
/// from real source positions. Use [`is_synthetic()`](Self::is_synthetic) to check.
#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct Span {
    /// The start position (line and column, 1-indexed).
    pub start: Position,
    /// The end position (line and column, 1-indexed).
    pub end: Position,
    /// The start byte offset in the source (0-indexed, inclusive).
    pub start_offset: usize,
    /// The end byte offset in the source (0-indexed, exclusive).
    pub end_offset: usize,
}

impl Span {
    /// Create a synthetic span for values without source positions.
    ///
    /// Uses line 0 to distinguish from real spans (which are 1-indexed).
    /// This is useful when converting `Value` to `Expr` for deserialization.
    /// Check with [`is_synthetic()`](Self::is_synthetic).
    #[must_use]
    pub const fn synthetic() -> Self {
        Self {
            start: Position { line: 0, col: 0 },
            end: Position { line: 0, col: 0 },
            start_offset: 0,
            end_offset: 0,
        }
    }

    /// Returns `true` if this is a synthetic span (no source position).
    ///
    /// Synthetic spans are created by [`Span::synthetic()`] for values
    /// converted from `Value` to AST without original source positions.
    /// They use line 0 to distinguish from real spans (which are 1-indexed).
    #[must_use]
    pub const fn is_synthetic(&self) -> bool {
        self.start.line == 0
    }

    /// Slice the given source text using this span's byte offsets.
    ///
    /// # Panics
    /// Panics if the byte offsets are out of bounds for the source.
    #[must_use]
    pub fn slice<'a>(&self, source: &'a str) -> &'a str {
        &source[self.start_offset..self.end_offset]
    }

    /// Create a span covering from the start of `start` to the end of `end`.
    ///
    /// This is useful for creating spans that cover a range of tokens,
    /// such as the span of a tuple from `(` to `)`.
    #[must_use]
    pub fn between(start: &Span, end: &Span) -> Self {
        Self {
            start: start.start,
            end: end.end,
            start_offset: start.start_offset,
            end_offset: end.end_offset,
        }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { start, end, .. } = self;
        if start == end {
            write!(f, "{start}")
        } else {
            write!(f, "{start}-{end}")
        }
    }
}

#[cfg(test)]
mod tests {
    extern crate alloc;
    use alloc::string::ToString;

    use super::*;

    #[test]
    fn test_position_display() {
        let pos = Position { line: 3, col: 15 };
        assert_eq!(pos.to_string(), "3:15");
    }

    #[test]
    fn test_span_display() {
        let span = Span {
            start: Position { line: 1, col: 1 },
            end: Position { line: 1, col: 1 },
            start_offset: 0,
            end_offset: 0,
        };
        assert_eq!(span.to_string(), "1:1");

        let span = Span {
            start: Position { line: 1, col: 1 },
            end: Position { line: 2, col: 5 },
            start_offset: 0,
            end_offset: 10,
        };
        assert_eq!(span.to_string(), "1:1-2:5");
    }

    #[test]
    fn test_synthetic_span() {
        let span = Span::synthetic();
        assert!(span.is_synthetic());

        let real_span = Span {
            start: Position { line: 1, col: 1 },
            end: Position { line: 1, col: 5 },
            start_offset: 0,
            end_offset: 4,
        };
        assert!(!real_span.is_synthetic());
    }

    #[test]
    fn test_span_slice() {
        let source = "hello world";
        let span = Span {
            start: Position { line: 1, col: 1 },
            end: Position { line: 1, col: 5 },
            start_offset: 0,
            end_offset: 5,
        };
        assert_eq!(span.slice(source), "hello");
    }

    #[test]
    fn test_position_from_src_end() {
        assert_eq!(
            Position::from_src_end("hello"),
            Position { line: 1, col: 6 }
        );
        assert_eq!(
            Position::from_src_end("hello\nworld"),
            Position { line: 2, col: 6 }
        );
        assert_eq!(
            Position::from_src_end("a\nb\nc"),
            Position { line: 3, col: 2 }
        );
    }
}
