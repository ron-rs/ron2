//! Span and position types for source location tracking.

extern crate alloc;

use alloc::vec::Vec;
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
    /// Uses single-pass byte iteration for efficiency.
    #[must_use]
    pub fn from_src_end(src: &str) -> Self {
        let mut line = 1;
        let mut col = 1;

        for &b in src.as_bytes() {
            if b == b'\n' {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }

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
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
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

/// Index of line start positions for efficient byte offset → line/column lookup.
///
/// Builds a table of line start offsets once and uses binary search to convert
/// byte offsets to positions.
///
/// # Complexity
///
/// - Build: O(n) single pass through the source
/// - Lookup: O(log L) where L = number of lines
#[derive(Clone, Debug)]
pub struct LineIndex {
    /// Byte offsets where each line starts. `line_starts[0]` = 0 (first line starts at offset 0).
    line_starts: Vec<usize>,
}

impl LineIndex {
    /// Build a line index from source text.
    ///
    /// Performs a single O(n) pass to find all newline positions.
    #[must_use]
    pub fn new(source: &str) -> Self {
        let mut line_starts = vec![0];
        for (i, b) in source.bytes().enumerate() {
            if b == b'\n' {
                line_starts.push(i + 1);
            }
        }
        Self { line_starts }
    }

    /// Convert a byte offset to a line/column position.
    ///
    /// Uses binary search for O(log L) lookup where L = number of lines.
    ///
    /// # Panics
    ///
    /// Panics if `offset` is greater than the source length.
    #[must_use]
    pub fn position(&self, offset: usize) -> Position {
        // partition_point returns the number of elements for which the predicate is true
        // We want the last line_start that is <= offset
        let line = self.line_starts.partition_point(|&start| start <= offset);
        // line is now 1-indexed (partition_point returns count, not index)
        let line_start = self.line_starts[line - 1];
        Position {
            line,
            col: offset - line_start + 1,
        }
    }

    /// Returns the line starts array for creating a cursor.
    #[must_use]
    pub fn line_starts(&self) -> &[usize] {
        &self.line_starts
    }
}

/// A cursor for efficient sequential position lookups.
///
/// For lexers that compute positions sequentially (cursor only moves forward),
/// this provides O(k) lookups where k = number of newlines crossed, rather
/// than O(log L) binary search per lookup.
#[derive(Clone, Debug)]
pub struct LineIndexCursor<'a> {
    line_starts: &'a [usize],
    /// Current line (1-indexed).
    line: usize,
    /// Byte offset where current line starts.
    line_start: usize,
}

impl<'a> LineIndexCursor<'a> {
    /// Create a cursor starting at position (1, 1).
    #[must_use]
    pub fn new(line_index: &'a LineIndex) -> Self {
        Self {
            line_starts: &line_index.line_starts,
            line: 1,
            line_start: 0,
        }
    }

    /// Convert a byte offset to position, scanning forward from the last lookup.
    ///
    /// The offset must be >= the offset from the previous call (monotonic).
    /// This is O(k) where k = number of newlines between the last and current offset.
    #[must_use]
    pub fn position(&mut self, offset: usize) -> Position {
        // Scan forward through line starts to find which line we're on
        while self.line < self.line_starts.len() {
            let next_line_start = self.line_starts[self.line];
            if offset < next_line_start {
                break;
            }
            self.line += 1;
            self.line_start = next_line_start;
        }
        Position {
            line: self.line,
            col: offset - self.line_start + 1,
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

    #[test]
    fn test_line_index_single_line() {
        let idx = LineIndex::new("hello");
        assert_eq!(idx.position(0), Position { line: 1, col: 1 });
        assert_eq!(idx.position(4), Position { line: 1, col: 5 });
        assert_eq!(idx.position(5), Position { line: 1, col: 6 });
    }

    #[test]
    fn test_line_index_multiline() {
        let idx = LineIndex::new("foo\nbar\nbaz");
        // Line 1: "foo\n" (offsets 0-3)
        assert_eq!(idx.position(0), Position { line: 1, col: 1 });
        assert_eq!(idx.position(2), Position { line: 1, col: 3 });
        assert_eq!(idx.position(3), Position { line: 1, col: 4 }); // newline char
        // Line 2: "bar\n" (offsets 4-7)
        assert_eq!(idx.position(4), Position { line: 2, col: 1 });
        assert_eq!(idx.position(6), Position { line: 2, col: 3 });
        // Line 3: "baz" (offsets 8-10)
        assert_eq!(idx.position(8), Position { line: 3, col: 1 });
        assert_eq!(idx.position(10), Position { line: 3, col: 3 });
    }

    #[test]
    fn test_line_index_empty() {
        let idx = LineIndex::new("");
        assert_eq!(idx.position(0), Position { line: 1, col: 1 });
    }

    #[test]
    fn test_line_index_trailing_newline() {
        let idx = LineIndex::new("foo\n");
        assert_eq!(idx.position(0), Position { line: 1, col: 1 });
        assert_eq!(idx.position(3), Position { line: 1, col: 4 }); // newline
        assert_eq!(idx.position(4), Position { line: 2, col: 1 }); // after newline
    }
}
