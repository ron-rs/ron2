//! Path segment types for error context tracking.

use alloc::string::String;
use core::fmt;

/// A segment in the error context path.
///
/// These segments describe the location within a data structure
/// where an error occurred. Paths are built from innermost to outermost
/// (push order) and reversed on display to show outermost first.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PathSegment {
    /// Error occurred in a struct field.
    Field(String),
    /// Error occurred at a sequence/tuple element index.
    Element(usize),
    /// Error occurred in a map key.
    MapKey,
    /// Error occurred in a map value (includes key for context).
    MapValue(String),
    /// Error occurred in an enum variant.
    Variant(String),
    /// Error occurred in a `TypeRef` resolution.
    TypeRef(String),
}

impl fmt::Display for PathSegment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PathSegment::Field(name) => write!(f, "field '{name}'"),
            PathSegment::Element(idx) => write!(f, "element {idx}"),
            PathSegment::MapKey => write!(f, "map key"),
            PathSegment::MapValue(key) => write!(f, "map value for '{key}'"),
            PathSegment::Variant(name) => write!(f, "variant '{name}'"),
            PathSegment::TypeRef(path) => write!(f, "type '{path}'"),
        }
    }
}

#[cfg(test)]
mod tests {
    use alloc::string::ToString;

    use super::*;

    #[test]
    fn test_path_segment_display() {
        assert_eq!(
            PathSegment::Field("name".into()).to_string(),
            "field 'name'"
        );
        assert_eq!(PathSegment::Element(5).to_string(), "element 5");
        assert_eq!(PathSegment::MapKey.to_string(), "map key");
        assert_eq!(
            PathSegment::MapValue("key1".into()).to_string(),
            "map value for 'key1'"
        );
        assert_eq!(
            PathSegment::Variant("Some".into()).to_string(),
            "variant 'Some'"
        );
        assert_eq!(
            PathSegment::TypeRef("my::Type".into()).to_string(),
            "type 'my::Type'"
        );
    }
}
