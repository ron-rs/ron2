//! `AstMapAccess` helper for struct deserialization with span preservation.

use alloc::borrow::Cow;
use std::collections::{HashMap, HashSet};

use crate::ast::{AnonStructExpr, Expr, FieldsBody, StructField};
use crate::error::{Error, Span, SpannedError, SpannedResult};

use super::{expr_type_name, FromRon, FromRonFields};

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
pub struct AstMapAccess<'a> {
    /// Map from field name to the field struct
    fields: HashMap<&'a str, &'a StructField<'a>>,
    /// Track which fields have been consumed
    consumed: HashSet<&'a str>,
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
    /// Returns an error if duplicate field names are found.
    pub fn from_anon(
        s: &'a AnonStructExpr<'a>,
        struct_name: Option<&'a str>,
    ) -> SpannedResult<Self> {
        let mut fields = HashMap::with_capacity(s.fields.len());
        for field in &s.fields {
            let name = field.name.name.as_ref();
            if fields.contains_key(name) {
                return Err(SpannedError {
                    code: Error::DuplicateStructField {
                        field: Cow::Owned(name.to_string()),
                        outer: struct_name.map(|s| Cow::Owned(s.to_string())),
                    },
                    span: field.name.span.clone(),
                });
            }
            fields.insert(name, field);
        }
        Ok(Self {
            fields,
            consumed: HashSet::new(),
            struct_span: s.span.clone(),
            struct_name,
        })
    }

    /// Create from named struct fields: `Name { field: value, ... }`
    ///
    /// # Errors
    ///
    /// Returns an error if duplicate field names are found.
    pub fn from_fields(
        fields_body: &'a FieldsBody<'a>,
        struct_name: Option<&'a str>,
        struct_span: Span,
    ) -> SpannedResult<Self> {
        let mut fields = HashMap::with_capacity(fields_body.fields.len());
        for field in &fields_body.fields {
            let name = field.name.name.as_ref();
            if fields.contains_key(name) {
                return Err(SpannedError {
                    code: Error::DuplicateStructField {
                        field: Cow::Owned(name.to_string()),
                        outer: struct_name.map(|s| Cow::Owned(s.to_string())),
                    },
                    span: field.name.span.clone(),
                });
            }
            fields.insert(name, field);
        }
        Ok(Self {
            fields,
            consumed: HashSet::new(),
            struct_span,
            struct_name,
        })
    }

    /// Get a required field.
    ///
    /// Returns an error with the field's span if deserialization fails,
    /// or the struct's span if the field is missing.
    pub fn required<T: FromRon>(&mut self, name: &'static str) -> SpannedResult<T> {
        match self.fields.get(name) {
            Some(field) => {
                self.consumed.insert(name);
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
        match self.fields.get(name) {
            Some(field) => {
                self.consumed.insert(name);
                Ok(Some(T::from_ast(&field.value)?))
            }
            None => Ok(None),
        }
    }

    /// Get a field with a default value if missing.
    pub fn with_default<T: FromRon + Default>(&mut self, name: &'static str) -> SpannedResult<T> {
        match self.fields.get(name) {
            Some(field) => {
                self.consumed.insert(name);
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
        match self.fields.get(name) {
            Some(field) => {
                self.consumed.insert(name);
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
        for (&name, &field) in &self.fields {
            if !self.consumed.contains(name) {
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
            .keys()
            .filter(|k| !self.consumed.contains(*k))
            .copied()
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
        match self.fields.get(name) {
            Some(field) => {
                self.consumed.insert(name);
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
        match self.fields.get(name) {
            Some(field) => {
                self.consumed.insert(name);
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
