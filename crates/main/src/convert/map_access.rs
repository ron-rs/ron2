//! `AstMapAccess` helper for struct deserialization with span preservation.

use alloc::borrow::Cow;

use super::{FromRon, FromRonFields, expr_type_name};
use crate::{
    ast::{AnonStructExpr, Expr, FieldsBody, StructField},
    error::{Error, Span, SpannedError, SpannedResult},
};

/// Maximum number of fields supported in a struct.
///
/// This limit exists because we use a `u64` bitmask to track consumed fields.
pub const MAX_FIELDS: usize = 64;

/// Helper struct for deserializing RON structs to Rust structs from AST.
///
/// Unlike Value-based deserialization, this preserves span information for
/// precise error reporting at the field level.
///
/// # Example
///
/// ```
/// use ron2::{AstMapAccess, FromRon};
/// use ron2::ast::{Expr, AnonStructExpr};
/// use ron2::error::SpannedResult;
///
/// // Manually implementing FromRon for a struct
/// struct Point { x: i32, y: i32 }
///
/// impl FromRon for Point {
///     fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
///         match expr {
///             Expr::AnonStruct(s) => {
///                 let mut access = AstMapAccess::from_anon(s, Some("Point"))?;
///                 let x = access.required("x")?;
///                 let y = access.required("y")?;
///                 access.deny_unknown_fields(&["x", "y"])?;
///                 Ok(Point { x, y })
///             }
///             _ => Err(ron2::error::SpannedError {
///                 code: ron2::error::Error::ExpectedStructLike,
///                 span: expr.span().clone(),
///             })
///         }
///     }
/// }
/// ```
#[derive(Debug)]
pub struct AstMapAccess<'a> {
    /// Slice of struct fields from the AST
    fields: &'a [StructField<'a>],
    /// Bitmask tracking which fields have been consumed (bit i = field i)
    consumed: u64,
    /// The span of the entire struct (for missing field errors)
    struct_span: Span,
    /// Optional struct name for error messages
    struct_name: Option<&'a str>,
}

impl<'a> AstMapAccess<'a> {
    /// Create from an anonymous struct expression: `(field: value, ...)`
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - The struct has more than 64 fields
    /// - Duplicate field names are found
    pub fn from_anon(
        s: &'a AnonStructExpr<'a>,
        struct_name: Option<&'a str>,
    ) -> SpannedResult<Self> {
        if s.fields.len() > MAX_FIELDS {
            return Err(SpannedError {
                code: Error::TooManyFields {
                    count: s.fields.len(),
                    limit: MAX_FIELDS,
                },
                span: s.span.clone(),
            });
        }

        // Check for duplicate field names via linear scan
        for (i, field) in s.fields.iter().enumerate() {
            let name = field.name.name.as_ref();
            for earlier in &s.fields[..i] {
                if earlier.name.name.as_ref() == name {
                    return Err(SpannedError {
                        code: Error::DuplicateStructField {
                            field: Cow::Owned(name.to_string()),
                            outer: struct_name.map(|s| Cow::Owned(s.to_string())),
                        },
                        span: field.name.span.clone(),
                    });
                }
            }
        }

        Ok(Self {
            fields: &s.fields,
            consumed: 0,
            struct_span: s.span.clone(),
            struct_name,
        })
    }

    /// Create from named struct fields: `Name { field: value, ... }`
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - The struct has more than 64 fields
    /// - Duplicate field names are found
    pub fn from_fields(
        fields_body: &'a FieldsBody<'a>,
        struct_name: Option<&'a str>,
        struct_span: Span,
    ) -> SpannedResult<Self> {
        if fields_body.fields.len() > MAX_FIELDS {
            return Err(SpannedError {
                code: Error::TooManyFields {
                    count: fields_body.fields.len(),
                    limit: MAX_FIELDS,
                },
                span: struct_span,
            });
        }

        // Check for duplicate field names via linear scan
        for (i, field) in fields_body.fields.iter().enumerate() {
            let name = field.name.name.as_ref();
            for earlier in &fields_body.fields[..i] {
                if earlier.name.name.as_ref() == name {
                    return Err(SpannedError {
                        code: Error::DuplicateStructField {
                            field: Cow::Owned(name.to_string()),
                            outer: struct_name.map(|s| Cow::Owned(s.to_string())),
                        },
                        span: field.name.span.clone(),
                    });
                }
            }
        }

        Ok(Self {
            fields: &fields_body.fields,
            consumed: 0,
            struct_span,
            struct_name,
        })
    }

    /// Find a field by name, returning its index and reference.
    fn find_field(&self, name: &str) -> Option<(usize, &'a StructField<'a>)> {
        self.fields
            .iter()
            .enumerate()
            .find(|(_, f)| f.name.name.as_ref() == name)
    }

    /// Get a required field.
    ///
    /// Returns an error with the field's span if deserialization fails,
    /// or the struct's span if the field is missing.
    pub fn required<T: FromRon>(&mut self, name: &'static str) -> SpannedResult<T> {
        match self.find_field(name) {
            Some((idx, field)) => {
                self.consumed |= 1 << idx;
                T::from_ast(&field.value)
            }
            None => Err(SpannedError {
                code: Error::MissingStructField {
                    field: Cow::Borrowed(name),
                    outer: self.struct_name.map(|s| Cow::Owned(s.to_string())),
                },
                span: self.struct_span.clone(),
            }),
        }
    }

    /// Get an optional field.
    ///
    /// Returns `Ok(None)` if the field is missing.
    pub fn optional<T: FromRon>(&mut self, name: &'static str) -> SpannedResult<Option<T>> {
        match self.find_field(name) {
            Some((idx, field)) => {
                self.consumed |= 1 << idx;
                Ok(Some(T::from_ast(&field.value)?))
            }
            None => Ok(None),
        }
    }

    /// Get a field with a default value if missing.
    pub fn with_default<T: FromRon + Default>(&mut self, name: &'static str) -> SpannedResult<T> {
        match self.find_field(name) {
            Some((idx, field)) => {
                self.consumed |= 1 << idx;
                T::from_ast(&field.value)
            }
            None => Ok(T::default()),
        }
    }

    /// Get a field with a custom default function.
    pub fn with_default_fn<T: FromRon, F: FnOnce() -> T>(
        &mut self,
        name: &'static str,
        default_fn: F,
    ) -> SpannedResult<T> {
        match self.find_field(name) {
            Some((idx, field)) => {
                self.consumed |= 1 << idx;
                T::from_ast(&field.value)
            }
            None => Ok(default_fn()),
        }
    }

    /// Check for unknown fields.
    ///
    /// Pass the list of expected field names. Returns an error pointing to
    /// the first unknown field's span.
    pub fn deny_unknown_fields(&self, expected: &'static [&'static str]) -> SpannedResult<()> {
        for (i, field) in self.fields.iter().enumerate() {
            if self.consumed & (1 << i) == 0 {
                let name = field.name.name.as_ref();
                return Err(SpannedError {
                    code: Error::NoSuchStructField {
                        expected,
                        found: Cow::Owned(name.to_string()),
                        outer: self.struct_name.map(|s| Cow::Owned(s.to_string())),
                    },
                    span: field.name.span.clone(),
                });
            }
        }
        Ok(())
    }

    /// Get an iterator over remaining (unconsumed) field names.
    pub fn remaining_keys(&self) -> impl Iterator<Item = &str> {
        self.fields
            .iter()
            .enumerate()
            .filter(|(i, _)| self.consumed & (1 << *i) == 0)
            .map(|(_, f)| f.name.name.as_ref())
    }

    /// Get a required Option field that requires explicit `Some(...)` or `None` syntax.
    ///
    /// Unlike the default `Option<T>` handling which accepts bare values as `Some`,
    /// this method rejects bare values and requires explicit option syntax.
    ///
    /// Use this for fields marked with `#[ron(explicit)]`.
    pub fn required_explicit<T: FromRon>(
        &mut self,
        name: &'static str,
    ) -> SpannedResult<Option<T>> {
        match self.find_field(name) {
            Some((idx, field)) => {
                self.consumed |= 1 << idx;
                match &field.value {
                    Expr::Option(opt) => match &opt.value {
                        None => Ok(None),
                        Some(inner) => Ok(Some(T::from_ast(&inner.expr)?)),
                    },
                    other => Err(SpannedError {
                        code: Error::InvalidValueForType {
                            expected: "Some(...) or None".to_string(),
                            found: expr_type_name(other).to_string(),
                        },
                        span: other.span().clone(),
                    }),
                }
            }
            None => Err(SpannedError {
                code: Error::MissingStructField {
                    field: Cow::Borrowed(name),
                    outer: self.struct_name.map(|s| Cow::Owned(s.to_string())),
                },
                span: self.struct_span.clone(),
            }),
        }
    }

    /// Get an Option field with explicit syntax, using `None` as default if missing.
    ///
    /// Unlike the default `Option<T>` handling which accepts bare values as `Some`,
    /// this method rejects bare values and requires explicit option syntax.
    ///
    /// Use this for fields marked with `#[ron(explicit, default)]`.
    pub fn with_default_explicit<T: FromRon>(
        &mut self,
        name: &'static str,
    ) -> SpannedResult<Option<T>> {
        match self.find_field(name) {
            Some((idx, field)) => {
                self.consumed |= 1 << idx;
                match &field.value {
                    Expr::Option(opt) => match &opt.value {
                        None => Ok(None),
                        Some(inner) => Ok(Some(T::from_ast(&inner.expr)?)),
                    },
                    other => Err(SpannedError {
                        code: Error::InvalidValueForType {
                            expected: "Some(...) or None".to_string(),
                            found: expr_type_name(other).to_string(),
                        },
                        span: other.span().clone(),
                    }),
                }
            }
            None => Ok(None),
        }
    }

    /// Consume fields for a flattened struct.
    ///
    /// This is used for `#[ron(flatten)]` fields where the inner struct's
    /// fields appear directly in the parent struct rather than nested.
    ///
    /// The inner type must implement [`FromRonFields`] to consume fields
    /// from this access.
    pub fn flatten<T: FromRonFields>(&mut self) -> SpannedResult<T> {
        T::from_fields(self)
    }
}

#[cfg(test)]
mod tests {
    use core::fmt::Write;

    use super::*;
    use crate::ast::parse_document;

    #[test]
    fn test_basic_field_access() {
        let ron = "(a: 1, b: 2, c: 3)";
        let doc = parse_document(ron).unwrap();
        if let Some(Expr::AnonStruct(s)) = &doc.value {
            let mut access = AstMapAccess::from_anon(s, Some("Test")).unwrap();

            assert_eq!(access.required::<i32>("a").unwrap(), 1);
            assert_eq!(access.required::<i32>("b").unwrap(), 2);
            assert_eq!(access.required::<i32>("c").unwrap(), 3);
            assert!(access.deny_unknown_fields(&["a", "b", "c"]).is_ok());
        } else {
            panic!("Expected anonymous struct");
        }
    }

    #[test]
    fn test_consumed_bitmask_tracking() {
        let ron = "(a: 1, b: 2, c: 3)";
        let doc = parse_document(ron).unwrap();
        if let Some(Expr::AnonStruct(s)) = &doc.value {
            let mut access = AstMapAccess::from_anon(s, Some("Test")).unwrap();

            // Only consume 'a' and 'c', leaving 'b' unconsumed
            assert_eq!(access.required::<i32>("a").unwrap(), 1);
            assert_eq!(access.required::<i32>("c").unwrap(), 3);

            // deny_unknown_fields should fail because 'b' wasn't consumed
            let err = access.deny_unknown_fields(&["a", "c"]).unwrap_err();
            assert!(matches!(err.code, Error::NoSuchStructField { .. }));
        } else {
            panic!("Expected anonymous struct");
        }
    }

    #[test]
    fn test_remaining_keys() {
        let ron = "(a: 1, b: 2, c: 3, d: 4)";
        let doc = parse_document(ron).unwrap();
        if let Some(Expr::AnonStruct(s)) = &doc.value {
            let mut access = AstMapAccess::from_anon(s, None).unwrap();

            // Consume a and c
            let _ = access.required::<i32>("a").unwrap();
            let _ = access.required::<i32>("c").unwrap();

            // Remaining should be b and d
            let remaining: Vec<_> = access.remaining_keys().collect();
            assert_eq!(remaining.len(), 2);
            assert!(remaining.contains(&"b"));
            assert!(remaining.contains(&"d"));
        } else {
            panic!("Expected anonymous struct");
        }
    }

    #[test]
    fn test_duplicate_field_detection() {
        let ron = "(a: 1, b: 2, a: 3)";
        let doc = parse_document(ron).unwrap();
        if let Some(Expr::AnonStruct(s)) = &doc.value {
            let err = AstMapAccess::from_anon(s, Some("Test")).unwrap_err();
            match err.code {
                Error::DuplicateStructField { field, outer } => {
                    assert_eq!(field.as_ref(), "a");
                    assert_eq!(outer.as_deref(), Some("Test"));
                }
                _ => panic!("Expected DuplicateStructField error"),
            }
        } else {
            panic!("Expected anonymous struct");
        }
    }

    #[test]
    fn test_too_many_fields_error() {
        // Build a RON string with 65 fields
        let mut fields = String::new();
        for i in 0..65 {
            if i > 0 {
                fields.push_str(", ");
            }
            let _ = write!(fields, "field_{i}: {i}");
        }
        let ron = format!("({fields})");

        let doc = parse_document(&ron).unwrap();
        if let Some(Expr::AnonStruct(s)) = &doc.value {
            let err = AstMapAccess::from_anon(s, Some("TooLarge")).unwrap_err();
            match err.code {
                Error::TooManyFields { count, limit } => {
                    assert_eq!(count, 65);
                    assert_eq!(limit, MAX_FIELDS);
                }
                _ => panic!("Expected TooManyFields error, got {:?}", err.code),
            }
        } else {
            panic!("Expected anonymous struct");
        }
    }

    #[test]
    fn test_exactly_64_fields_ok() {
        // Build a RON string with exactly 64 fields (should succeed)
        let mut fields = String::new();
        for i in 0..64 {
            if i > 0 {
                fields.push_str(", ");
            }
            let _ = write!(fields, "f{i}: {i}");
        }
        let ron = format!("({fields})");

        let doc = parse_document(&ron).unwrap();
        if let Some(Expr::AnonStruct(s)) = &doc.value {
            let access = AstMapAccess::from_anon(s, None).unwrap();
            assert_eq!(access.fields.len(), 64);
        } else {
            panic!("Expected anonymous struct");
        }
    }

    #[test]
    fn test_missing_field_error() {
        let ron = "(a: 1)";
        let doc = parse_document(ron).unwrap();
        if let Some(Expr::AnonStruct(s)) = &doc.value {
            let mut access = AstMapAccess::from_anon(s, Some("Test")).unwrap();
            let err = access.required::<i32>("missing").unwrap_err();
            match err.code {
                Error::MissingStructField { field, outer } => {
                    assert_eq!(field.as_ref(), "missing");
                    assert_eq!(outer.as_deref(), Some("Test"));
                }
                _ => panic!("Expected MissingStructField error"),
            }
        } else {
            panic!("Expected anonymous struct");
        }
    }

    #[test]
    fn test_optional_field() {
        let ron = "(a: 1)";
        let doc = parse_document(ron).unwrap();
        if let Some(Expr::AnonStruct(s)) = &doc.value {
            let mut access = AstMapAccess::from_anon(s, None).unwrap();

            assert_eq!(access.optional::<i32>("a").unwrap(), Some(1));
            assert_eq!(access.optional::<i32>("missing").unwrap(), None);
        } else {
            panic!("Expected anonymous struct");
        }
    }

    #[test]
    fn test_with_default() {
        let ron = "(a: 1)";
        let doc = parse_document(ron).unwrap();
        if let Some(Expr::AnonStruct(s)) = &doc.value {
            let mut access = AstMapAccess::from_anon(s, None).unwrap();

            assert_eq!(access.with_default::<i32>("a").unwrap(), 1);
            assert_eq!(access.with_default::<i32>("missing").unwrap(), 0); // i32::default()
        } else {
            panic!("Expected anonymous struct");
        }
    }

    #[test]
    fn test_high_index_field_consumed() {
        // Test that field at index 63 (highest bit) is tracked correctly
        let mut fields = String::new();
        for i in 0..64 {
            if i > 0 {
                fields.push_str(", ");
            }
            let _ = write!(fields, "f{i}: {i}");
        }
        let ron = format!("({fields})");

        let doc = parse_document(&ron).unwrap();
        if let Some(Expr::AnonStruct(s)) = &doc.value {
            let mut access = AstMapAccess::from_anon(s, None).unwrap();

            // Consume the last field (index 63)
            assert_eq!(access.required::<i32>("f63").unwrap(), 63);

            // Verify it's marked as consumed (bit 63 should be set)
            assert_eq!(access.consumed & (1u64 << 63), 1u64 << 63);

            // Verify f62 is not consumed
            assert_eq!(access.consumed & (1u64 << 62), 0);
        } else {
            panic!("Expected anonymous struct");
        }
    }
}
