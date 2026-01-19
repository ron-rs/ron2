//! `AstMapAccess` helper for struct deserialization with span preservation.

use alloc::borrow::Cow;
use core::hash::{BuildHasher, Hash};
use std::collections::HashMap;

use ahash::HashMapExt;

use super::{FromRon, FromRonFields, expr_type_name};
use crate::{
    ast::{AnonStructExpr, Expr, FieldsBody, StringExpr, StringKind, StructField},
    error::{Error, ErrorKind, Result, Span},
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
/// use ron2::ast::Expr;
/// use ron2::error::Result;
///
/// // Manually implementing FromRon for a struct
/// struct Point { x: i32, y: i32 }
///
/// impl FromRon for Point {
///     fn from_ast(expr: &Expr<'_>) -> Result<Self> {
///         match expr {
///             Expr::AnonStruct(s) => {
///                 let mut access = AstMapAccess::from_anon(s, Some("Point"))?;
///                 let x = access.required("x")?;
///                 let y = access.required("y")?;
///                 access.deny_unknown_fields(&["x", "y"])?;
///                 Ok(Point { x, y })
///             }
///             _ => Err(ron2::Error::expected("struct", expr.span()))
///         }
///     }
/// }
/// ```
#[derive(Debug)]
pub struct AstMapAccess<'a> {
    /// Slice of struct fields from the AST
    fields: &'a [StructField<'a>],
    /// O(1) field name to index lookup
    field_indices: ahash::HashMap<&'a str, usize>,
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
    #[inline]
    pub fn from_anon(s: &'a AnonStructExpr<'a>, struct_name: Option<&'a str>) -> Result<Self> {
        if s.fields.len() > MAX_FIELDS {
            return Err(Error::with_span(
                ErrorKind::TooManyFields {
                    count: s.fields.len(),
                    limit: MAX_FIELDS,
                },
                s.span,
            ));
        }

        // Build field index HashMap while detecting duplicates in one pass
        let mut field_indices = ahash::HashMap::with_capacity(s.fields.len());
        for (i, field) in s.fields.iter().enumerate() {
            let name = field.name.name.as_ref();
            if field_indices.insert(name, i).is_some() {
                return Err(Error::with_span(
                    ErrorKind::DuplicateField {
                        field: Cow::Owned(name.to_string()),
                        outer: struct_name.map(|s| Cow::Owned(s.to_string())),
                    },
                    field.name.span,
                ));
            }
        }

        Ok(Self {
            fields: &s.fields,
            field_indices,
            consumed: 0,
            struct_span: s.span,
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
    #[inline]
    pub fn from_fields(
        fields_body: &'a FieldsBody<'a>,
        struct_name: Option<&'a str>,
        struct_span: Span,
    ) -> Result<Self> {
        if fields_body.fields.len() > MAX_FIELDS {
            return Err(Error::with_span(
                ErrorKind::TooManyFields {
                    count: fields_body.fields.len(),
                    limit: MAX_FIELDS,
                },
                struct_span,
            ));
        }

        // Build field index HashMap while detecting duplicates in one pass
        let mut field_indices = ahash::HashMap::with_capacity(fields_body.fields.len());
        for (i, field) in fields_body.fields.iter().enumerate() {
            let name = field.name.name.as_ref();
            if field_indices.insert(name, i).is_some() {
                return Err(Error::with_span(
                    ErrorKind::DuplicateField {
                        field: Cow::Owned(name.to_string()),
                        outer: struct_name.map(|s| Cow::Owned(s.to_string())),
                    },
                    field.name.span,
                ));
            }
        }

        Ok(Self {
            fields: &fields_body.fields,
            field_indices,
            consumed: 0,
            struct_span,
            struct_name,
        })
    }

    /// Find a field by name, returning its index and reference.
    #[inline]
    fn find_field(&self, name: &str) -> Option<(usize, &'a StructField<'a>)> {
        self.field_indices
            .get(name)
            .map(|&idx| (idx, &self.fields[idx]))
    }

    /// Get a required field.
    ///
    /// Returns an error with the field's span if deserialization fails,
    /// or the struct's span if the field is missing.
    #[inline]
    pub fn required<T: FromRon>(&mut self, name: &'static str) -> Result<T> {
        match self.find_field(name) {
            Some((idx, field)) => {
                self.consumed |= 1 << idx;
                T::from_ast(&field.value)
            }
            None => Err(Error::with_span(
                ErrorKind::MissingField {
                    field: Cow::Borrowed(name),
                    outer: self.struct_name.map(|s| Cow::Owned(s.to_string())),
                },
                self.struct_span,
            )),
        }
    }

    /// Get an optional field.
    ///
    /// Returns `Ok(None)` if the field is missing.
    #[inline]
    pub fn optional<T: FromRon>(&mut self, name: &'static str) -> Result<Option<T>> {
        match self.find_field(name) {
            Some((idx, field)) => {
                self.consumed |= 1 << idx;
                Ok(Some(T::from_ast(&field.value)?))
            }
            None => Ok(None),
        }
    }

    /// Get a field with a default value if missing.
    #[inline]
    pub fn with_default<T: FromRon + Default>(&mut self, name: &'static str) -> Result<T> {
        match self.find_field(name) {
            Some((idx, field)) => {
                self.consumed |= 1 << idx;
                T::from_ast(&field.value)
            }
            None => Ok(T::default()),
        }
    }

    /// Get a field with a custom default function.
    #[inline]
    pub fn with_default_fn<T: FromRon, F: FnOnce() -> T>(
        &mut self,
        name: &'static str,
        default_fn: F,
    ) -> Result<T> {
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
    pub fn deny_unknown_fields(&self, expected: &'static [&'static str]) -> Result<()> {
        for (i, field) in self.fields.iter().enumerate() {
            if self.consumed & (1 << i) == 0 {
                let name = field.name.name.as_ref();
                return Err(Error::with_span(
                    ErrorKind::UnknownField {
                        field: Cow::Owned(name.to_string()),
                        expected,
                        outer: self.struct_name.map(|s| Cow::Owned(s.to_string())),
                    },
                    field.name.span,
                ));
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
    #[inline]
    pub fn required_explicit<T: FromRon>(&mut self, name: &'static str) -> Result<Option<T>> {
        match self.find_field(name) {
            Some((idx, field)) => {
                self.consumed |= 1 << idx;
                match &field.value {
                    Expr::Option(opt) => match &opt.value {
                        None => Ok(None),
                        Some(inner) => Ok(Some(T::from_ast(&inner.expr)?)),
                    },
                    other => Err(Error::with_span(
                        ErrorKind::TypeMismatch {
                            expected: "Some(...) or None".to_string(),
                            found: expr_type_name(other).to_string(),
                        },
                        *other.span(),
                    )),
                }
            }
            None => Err(Error::with_span(
                ErrorKind::MissingField {
                    field: Cow::Borrowed(name),
                    outer: self.struct_name.map(|s| Cow::Owned(s.to_string())),
                },
                self.struct_span,
            )),
        }
    }

    /// Get an Option field with explicit syntax, using `None` as default if missing.
    ///
    /// Unlike the default `Option<T>` handling which accepts bare values as `Some`,
    /// this method rejects bare values and requires explicit option syntax.
    ///
    /// Use this for fields marked with `#[ron(explicit, default)]`.
    #[inline]
    pub fn with_default_explicit<T: FromRon>(&mut self, name: &'static str) -> Result<Option<T>> {
        match self.find_field(name) {
            Some((idx, field)) => {
                self.consumed |= 1 << idx;
                match &field.value {
                    Expr::Option(opt) => match &opt.value {
                        None => Ok(None),
                        Some(inner) => Ok(Some(T::from_ast(&inner.expr)?)),
                    },
                    other => Err(Error::with_span(
                        ErrorKind::TypeMismatch {
                            expected: "Some(...) or None".to_string(),
                            found: expr_type_name(other).to_string(),
                        },
                        *other.span(),
                    )),
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
    #[inline]
    pub fn flatten<T: FromRonFields>(&mut self) -> Result<T> {
        T::from_fields(self)
    }
}

impl<T: FromRonFields> FromRonFields for Option<T> {
    fn from_fields(access: &mut AstMapAccess<'_>) -> Result<Self> {
        let consumed_before = access.consumed;

        match T::from_fields(access) {
            Ok(value) => {
                if access.consumed == consumed_before {
                    Ok(None)
                } else {
                    Ok(Some(value))
                }
            }
            Err(err) => {
                if access.consumed == consumed_before
                    && matches!(err.kind(), ErrorKind::MissingField { .. })
                {
                    Ok(None)
                } else {
                    Err(err)
                }
            }
        }
    }
}

impl<K: FromRon + Eq + Hash, V: FromRon, S: BuildHasher + Default> FromRonFields
    for HashMap<K, V, S>
{
    fn from_fields(access: &mut AstMapAccess<'_>) -> Result<Self> {
        let remaining = access
            .fields
            .iter()
            .enumerate()
            .filter(|(i, _)| access.consumed & (1 << *i) == 0)
            .count();
        let mut map = HashMap::with_capacity_and_hasher(remaining, S::default());

        for (i, field) in access.fields.iter().enumerate() {
            if access.consumed & (1 << i) != 0 {
                continue;
            }

            access.consumed |= 1 << i;

            let key_expr = Expr::String(StringExpr {
                span: field.name.span,
                raw: Cow::Owned(format!("\"{}\"", field.name.name)),
                value: field.name.name.to_string(),
                kind: StringKind::Regular,
            });

            let key = K::from_ast(&key_expr)?;
            let value = V::from_ast(&field.value)?;
            map.insert(key, value);
        }

        Ok(map)
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
            assert!(matches!(err.kind(), ErrorKind::UnknownField { .. }));
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
            match err.kind() {
                ErrorKind::DuplicateField { field, outer } => {
                    assert_eq!(field.as_ref(), "a");
                    assert_eq!(outer.as_deref(), Some("Test"));
                }
                _ => panic!("Expected DuplicateField error"),
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
            match err.kind() {
                ErrorKind::TooManyFields { count, limit } => {
                    assert_eq!(*count, 65);
                    assert_eq!(*limit, MAX_FIELDS);
                }
                _ => panic!("Expected TooManyFields error, got {:?}", err.kind()),
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
            match err.kind() {
                ErrorKind::MissingField { field, outer } => {
                    assert_eq!(field.as_ref(), "missing");
                    assert_eq!(outer.as_deref(), Some("Test"));
                }
                _ => panic!("Expected MissingField error"),
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
