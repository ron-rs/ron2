//! `AstMapAccess` helper for struct deserialization with span preservation.

use alloc::{borrow::Cow, collections::BTreeMap, string::String, vec, vec::Vec};
use core::hash::{BuildHasher, Hash};
use std::collections::HashMap;

use super::{FromRon, FromRonFields, expr_type_name};
use crate::{
    ast::{AnonStructExpr, Expr, FieldsBody, StructField, synthetic_string},
    error::{Error, ErrorKind, Result, Span},
};

/// Up to this many fields, a linear scan beats hashing: duplicates are found
/// by pairwise comparison and lookups scan. Larger structs get a name index.
const SMALL_STRUCT: usize = 16;

type FieldIndex<'a> = ahash::HashMap<&'a str, usize>;

/// Fail with [`ErrorKind::DuplicateField`] at the second occurrence of a
/// repeated field name; index the names of structs above [`SMALL_STRUCT`].
fn index_fields<'a>(
    fields: &'a [StructField<'a>],
    outer: Option<&str>,
) -> Result<Option<FieldIndex<'a>>> {
    let duplicate = |field: &StructField<'_>| {
        Error::with_span(
            ErrorKind::DuplicateField {
                field: Cow::Owned(String::from(&*field.name.name)),
                outer: outer.map(|s| Cow::Owned(String::from(s))),
            },
            field.name.span,
        )
    };

    if fields.len() <= SMALL_STRUCT {
        for (i, field) in fields.iter().enumerate() {
            if fields[..i].iter().any(|f| f.name.name == field.name.name) {
                return Err(duplicate(field));
            }
        }
        return Ok(None);
    }

    let mut index = FieldIndex::with_capacity_and_hasher(fields.len(), ahash::RandomState::new());
    for (i, field) in fields.iter().enumerate() {
        if index.insert(&*field.name.name, i).is_some() {
            return Err(duplicate(field));
        }
    }
    Ok(Some(index))
}

/// Fail with [`ErrorKind::DuplicateField`] if a field name repeats.
pub(crate) fn check_unique_fields(fields: &[StructField<'_>], outer: Option<&str>) -> Result<()> {
    index_fields(fields, outer).map(drop)
}

/// Set of consumed field indices; stays inline for structs of up to 64 fields.
#[derive(Debug)]
enum Consumed {
    Inline(u64),
    Spilled(Vec<u64>),
}

impl Consumed {
    fn new(len: usize) -> Self {
        if len <= 64 {
            Consumed::Inline(0)
        } else {
            Consumed::Spilled(vec![0; len.div_ceil(64)])
        }
    }

    fn contains(&self, index: usize) -> bool {
        let bit = 1 << (index % 64);
        match self {
            Consumed::Inline(word) => word & bit != 0,
            Consumed::Spilled(words) => words.get(index / 64).is_some_and(|w| w & bit != 0),
        }
    }

    /// Mark `index` as consumed; returns whether it was newly consumed.
    fn insert(&mut self, index: usize) -> bool {
        let bit = 1 << (index % 64);
        let word = match self {
            Consumed::Inline(word) => word,
            Consumed::Spilled(words) => match words.get_mut(index / 64) {
                Some(word) => word,
                None => return false,
            },
        };
        let fresh = *word & bit == 0;
        *word |= bit;
        fresh
    }
}

/// Helper struct for deserializing RON structs to Rust structs from AST.
///
/// Unlike Value-based deserialization, this preserves span information for
/// precise error reporting at the field level. Errors from a field's value
/// carry the field name in their [path](Error::path).
///
/// # Example
///
/// ```
/// use ron2::{convert::AstMapAccess, FromRon};
/// use ron2::ast::Expr;
/// use ron2::error::Result;
///
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
///             _ => Err(ron2::Error::expected("struct", *expr.span())),
///         }
///     }
/// }
///
/// let point = Point::from_ron("(x: 1, y: 2)").unwrap();
/// assert_eq!((point.x, point.y), (1, 2));
/// ```
#[derive(Debug)]
pub struct AstMapAccess<'a> {
    fields: &'a [StructField<'a>],
    /// Name index for structs above [`SMALL_STRUCT`] fields.
    index: Option<FieldIndex<'a>>,
    consumed: Consumed,
    consumed_count: usize,
    /// The field after the last one found: fields usually arrive in
    /// declaration order, so checking it first makes most lookups O(1).
    cursor: usize,
    /// The span of the entire struct (for missing field errors).
    struct_span: Span,
    struct_name: Option<&'a str>,
}

impl<'a> AstMapAccess<'a> {
    /// Create from an anonymous struct expression: `(field: value, ...)`
    ///
    /// # Errors
    ///
    /// Returns an error if a field name occurs twice.
    pub fn from_anon(s: &'a AnonStructExpr<'a>, struct_name: Option<&'a str>) -> Result<Self> {
        Self::new(&s.fields, struct_name, s.span)
    }

    /// Create from named struct fields: `Name(field: value, ...)`
    ///
    /// # Errors
    ///
    /// Returns an error if a field name occurs twice.
    pub fn from_fields(
        fields_body: &'a FieldsBody<'a>,
        struct_name: Option<&'a str>,
        struct_span: Span,
    ) -> Result<Self> {
        Self::new(&fields_body.fields, struct_name, struct_span)
    }

    fn new(
        fields: &'a [StructField<'a>],
        struct_name: Option<&'a str>,
        struct_span: Span,
    ) -> Result<Self> {
        Ok(Self {
            index: index_fields(fields, struct_name)?,
            fields,
            consumed: Consumed::new(fields.len()),
            consumed_count: 0,
            cursor: 0,
            struct_span,
            struct_name,
        })
    }

    /// Create an access with no fields, for `()` or `Name()` standing in for a
    /// struct whose fields all have defaults.
    #[must_use]
    pub fn empty(struct_name: Option<&'a str>, struct_span: Span) -> Self {
        Self {
            fields: &[],
            index: None,
            consumed: Consumed::new(0),
            consumed_count: 0,
            cursor: 0,
            struct_span,
            struct_name,
        }
    }

    /// The index of the field called `name`.
    fn find(&self, name: &str) -> Option<usize> {
        let fields = self.fields;
        if fields.get(self.cursor).is_some_and(|f| f.name.name == name) {
            Some(self.cursor)
        } else if let Some(index) = &self.index {
            index.get(name).copied()
        } else {
            fields.iter().position(|f| f.name.name == name)
        }
    }

    /// Find a field by name and mark it consumed.
    fn take(&mut self, name: &str) -> Option<&'a StructField<'a>> {
        let fields = self.fields;
        let index = self.find(name)?;
        self.cursor = index + 1;
        if self.consumed.insert(index) {
            self.consumed_count += 1;
        }
        Some(&fields[index])
    }

    /// Deserialize a field's value if present, adding the field to error paths.
    fn field<T: FromRon>(&mut self, name: &'static str) -> Option<Result<T>> {
        let field = self.take(name)?;
        Some(T::from_ast(&field.value).map_err(|e| e.in_field(name)))
    }

    /// Like [`field`](Self::field), but requires explicit `Some(...)` / `None`.
    fn explicit_field<T: FromRon>(&mut self, name: &'static str) -> Option<Result<Option<T>>> {
        let field = self.take(name)?;
        Some(match &field.value {
            Expr::Option(opt) => opt
                .value
                .as_ref()
                .map(|inner| T::from_ast(&inner.expr))
                .transpose()
                .map_err(|e| e.in_field(name)),
            other => Err(Error::with_span(
                ErrorKind::TypeMismatch {
                    expected: "Some(...) or None".into(),
                    found: expr_type_name(other).into(),
                },
                *other.span(),
            )
            .in_field(name)),
        })
    }

    fn missing(&self, name: &'static str) -> Error {
        Error::with_span(
            ErrorKind::MissingField {
                field: Cow::Borrowed(name),
                outer: self.struct_name.map(|s| Cow::Owned(String::from(s))),
            },
            self.struct_span,
        )
    }

    /// Get a required field.
    ///
    /// Returns an error with the field's span if deserialization fails,
    /// or the struct's span if the field is missing.
    pub fn required<T: FromRon>(&mut self, name: &'static str) -> Result<T> {
        self.field(name).unwrap_or_else(|| Err(self.missing(name)))
    }

    /// Get an optional field.
    ///
    /// Returns `Ok(None)` if the field is missing.
    pub fn optional<T: FromRon>(&mut self, name: &'static str) -> Result<Option<T>> {
        self.field(name).transpose()
    }

    /// Get a field with a default value if missing.
    pub fn with_default<T: FromRon + Default>(&mut self, name: &'static str) -> Result<T> {
        self.field(name).unwrap_or_else(|| Ok(T::default()))
    }

    /// Get a field with a custom default function.
    pub fn with_default_fn<T: FromRon, F: FnOnce() -> T>(
        &mut self,
        name: &'static str,
        default_fn: F,
    ) -> Result<T> {
        self.field(name).unwrap_or_else(|| Ok(default_fn()))
    }

    /// Get a required Option field that requires explicit `Some(...)` or `None` syntax.
    ///
    /// Unlike the default `Option<T>` handling which accepts bare values as `Some`,
    /// this method rejects bare values and requires explicit option syntax.
    ///
    /// Use this for fields marked with `#[ron(explicit)]`.
    pub fn required_explicit<T: FromRon>(&mut self, name: &'static str) -> Result<Option<T>> {
        self.explicit_field(name)
            .unwrap_or_else(|| Err(self.missing(name)))
    }

    /// Get an Option field with explicit syntax, using `None` as default if missing.
    ///
    /// Use this for fields marked with `#[ron(explicit, default)]`.
    pub fn with_default_explicit<T: FromRon>(&mut self, name: &'static str) -> Result<Option<T>> {
        self.explicit_field(name).unwrap_or(Ok(None))
    }

    /// Check for unknown fields.
    ///
    /// Pass the list of expected field names. Returns an error pointing to
    /// the first unknown field's span.
    pub fn deny_unknown_fields(&self, expected: &'static [&'static str]) -> Result<()> {
        self.deny_unknown_fields_with(|| Cow::Borrowed(expected))
    }

    /// Like [`deny_unknown_fields`](Self::deny_unknown_fields), but builds the
    /// list of expected names only when there is an unknown field.
    pub fn deny_unknown_fields_with(
        &self,
        expected: impl FnOnce() -> Cow<'static, [&'static str]>,
    ) -> Result<()> {
        match self.remaining().next() {
            None => Ok(()),
            Some(field) => Err(Error::with_span(
                ErrorKind::UnknownField {
                    field: Cow::Owned(String::from(field.name.name.as_ref())),
                    expected: expected(),
                    outer: self.struct_name.map(|s| Cow::Owned(String::from(s))),
                },
                field.name.span,
            )),
        }
    }

    fn remaining(&self) -> impl Iterator<Item = &'a StructField<'a>> + '_ {
        self.fields
            .iter()
            .enumerate()
            .filter(|(i, _)| !self.consumed.contains(*i))
            .map(|(_, field)| field)
    }

    /// Get an iterator over remaining (unconsumed) field names.
    pub fn remaining_keys(&self) -> impl Iterator<Item = &str> {
        self.remaining().map(|field| field.name.name.as_ref())
    }

    /// Consume fields for a flattened struct.
    ///
    /// This is used for `#[ron(flatten)]` fields where the inner struct's
    /// fields appear directly in the parent struct rather than nested.
    ///
    /// The inner type must implement [`FromRonFields`] to consume fields
    /// from this access.
    pub fn flatten<T: FromRonFields>(&mut self) -> Result<T> {
        T::from_fields(self)
    }
}

/// A flattened `Option<T>` is `None` when none of `T`'s fields are present
/// and `Some` otherwise, so a partially present `T` reports its missing
/// fields instead of being dropped. A catch-all `T` (a map) is `Some` when it
/// collected at least one field.
impl<T: FromRonFields> FromRonFields for Option<T> {
    const CATCH_ALL: bool = T::CATCH_ALL;

    fn from_fields(access: &mut AstMapAccess<'_>) -> Result<Self> {
        if T::CATCH_ALL {
            let consumed_before = access.consumed_count;
            let value = T::from_fields(access)?;
            return Ok((access.consumed_count != consumed_before).then_some(value));
        }

        let mut names = Vec::new();
        T::field_names(&mut names);
        if names.iter().any(|name| access.find(name).is_some()) {
            T::from_fields(access).map(Some)
        } else {
            Ok(None)
        }
    }

    fn field_names(names: &mut Vec<&'static str>) {
        T::field_names(names);
    }
}

impl AstMapAccess<'_> {
    /// Consume every remaining field as a `(key, value)` pair, for maps that
    /// collect the fields no one else claimed.
    fn take_remaining<K: FromRon, V: FromRon>(
        &mut self,
        mut insert: impl FnMut(K, V),
    ) -> Result<()> {
        for (index, field) in self.fields.iter().enumerate() {
            if !self.consumed.insert(index) {
                continue;
            }
            self.consumed_count += 1;

            let name = field.name.name.as_ref();
            let mut key_expr = synthetic_string(String::from(name));
            if let Expr::String(key) = &mut key_expr {
                key.span = field.name.span;
            }
            let key = K::from_ast(&key_expr).map_err(Error::in_map_key)?;
            let value = V::from_ast(&field.value).map_err(|e| e.in_field(name))?;
            insert(key, value);
        }
        Ok(())
    }

    fn remaining_count(&self) -> usize {
        self.fields.len() - self.consumed_count
    }
}

/// A catch-all map takes every field no other field consumed.
impl<K: FromRon + Eq + Hash, V: FromRon, S: BuildHasher + Default> FromRonFields
    for HashMap<K, V, S>
{
    const CATCH_ALL: bool = true;

    fn from_fields(access: &mut AstMapAccess<'_>) -> Result<Self> {
        let mut map = HashMap::with_capacity_and_hasher(access.remaining_count(), S::default());
        access.take_remaining(|k, v| {
            map.insert(k, v);
        })?;
        Ok(map)
    }
}

impl<K: FromRon + Ord, V: FromRon> FromRonFields for BTreeMap<K, V> {
    const CATCH_ALL: bool = true;

    fn from_fields(access: &mut AstMapAccess<'_>) -> Result<Self> {
        let mut map = BTreeMap::new();
        access.take_remaining(|k, v| {
            map.insert(k, v);
        })?;
        Ok(map)
    }
}

impl<K: FromRon + Eq + Hash, V: FromRon, S: BuildHasher + Default> FromRonFields
    for indexmap::IndexMap<K, V, S>
{
    const CATCH_ALL: bool = true;

    fn from_fields(access: &mut AstMapAccess<'_>) -> Result<Self> {
        let mut map =
            indexmap::IndexMap::with_capacity_and_hasher(access.remaining_count(), S::default());
        access.take_remaining(|k, v| {
            map.insert(k, v);
        })?;
        Ok(map)
    }
}

#[cfg(test)]
mod tests {
    use core::fmt::Write;

    use super::*;
    use crate::{PathSegment, ast::parse_document};

    fn with_access<R>(ron: &str, f: impl FnOnce(Result<AstMapAccess<'_>>) -> R) -> R {
        let doc = parse_document(ron).unwrap();
        let Some(Expr::AnonStruct(s)) = &doc.value else {
            panic!("expected anonymous struct");
        };
        f(AstMapAccess::from_anon(s, Some("Test")))
    }

    fn many_fields(count: usize) -> String {
        let mut ron = String::from("(");
        for i in 0..count {
            let _ = write!(ron, "f{i}: {i}, ");
        }
        ron.push(')');
        ron
    }

    #[test]
    fn test_basic_field_access() {
        with_access("(a: 1, b: 2, c: 3)", |access| {
            let mut access = access.unwrap();
            assert_eq!(access.required::<i32>("a").unwrap(), 1);
            assert_eq!(access.required::<i32>("b").unwrap(), 2);
            assert_eq!(access.required::<i32>("c").unwrap(), 3);
            assert!(access.deny_unknown_fields(&["a", "b", "c"]).is_ok());
        });
    }

    #[test]
    fn test_out_of_order_access() {
        with_access("(c: 3, a: 1, b: 2)", |access| {
            let mut access = access.unwrap();
            assert_eq!(access.required::<i32>("a").unwrap(), 1);
            assert_eq!(access.required::<i32>("b").unwrap(), 2);
            assert_eq!(access.required::<i32>("c").unwrap(), 3);
            assert_eq!(access.required::<i32>("a").unwrap(), 1);
            assert!(access.deny_unknown_fields(&["a", "b", "c"]).is_ok());
        });
    }

    #[test]
    fn test_unconsumed_field_is_unknown() {
        with_access("(a: 1, b: 2, c: 3)", |access| {
            let mut access = access.unwrap();
            assert_eq!(access.required::<i32>("a").unwrap(), 1);
            assert_eq!(access.required::<i32>("c").unwrap(), 3);

            let err = access.deny_unknown_fields(&["a", "c"]).unwrap_err();
            assert!(matches!(err.kind(), ErrorKind::UnknownField { field, .. } if field == "b"));
            assert_eq!(access.remaining_keys().collect::<Vec<_>>(), ["b"]);
        });
    }

    #[test]
    fn test_duplicate_field_detection() {
        for ron in [
            "(a: 1, b: 2, a: 3)".into(),
            many_fields(40).replace(')', "f3: 0)"),
        ] {
            with_access(&ron, |access| {
                let err = access.unwrap_err();
                assert!(
                    matches!(err.kind(), ErrorKind::DuplicateField { outer: Some(o), .. } if o == "Test"),
                    "{err:?}"
                );
            });
        }
    }

    /// Regression: a `u64` bitmask over the input once capped structs at 64
    /// input fields, even when the extra fields were unknown and ignored.
    #[test]
    fn test_more_than_64_input_fields() {
        with_access(&many_fields(200), |access| {
            let mut access = access.unwrap();
            assert_eq!(access.required::<i32>("f0").unwrap(), 0);
            assert_eq!(access.required::<i32>("f150").unwrap(), 150);
            assert_eq!(access.required::<i32>("f199").unwrap(), 199);
            assert_eq!(access.remaining_keys().count(), 197);

            let map: HashMap<String, i32> = access.flatten().unwrap();
            assert_eq!(map.len(), 197);
            assert_eq!(map["f64"], 64);
            assert_eq!(access.remaining_keys().count(), 0);
        });
    }

    /// Regression: lookups scanned from a cursor, which is quadratic for
    /// out-of-order input (5000 reversed fields took 236 ms). Large structs
    /// now use a name index.
    #[test]
    fn test_large_out_of_order_struct() {
        let mut ron = String::from("(");
        for i in (0..5000).rev() {
            let _ = write!(ron, "f{i}: {i}, ");
        }
        ron.push(')');
        with_access(&ron, |access| {
            let mut access = access.unwrap();
            assert!(access.index.is_some());
            for i in 0..5000 {
                let name: &'static str = String::leak(alloc::format!("f{i}"));
                assert_eq!(access.required::<i32>(name).unwrap(), i);
                assert_eq!(access.optional::<i32>("missing").unwrap(), None);
            }
            assert_eq!(access.remaining_keys().count(), 0);
        });
    }

    #[test]
    fn test_missing_field_error() {
        with_access("(a: 1)", |access| {
            let err = access.unwrap().required::<i32>("missing").unwrap_err();
            assert_eq!(
                err.kind(),
                &ErrorKind::MissingField {
                    field: "missing".into(),
                    outer: Some("Test".into())
                }
            );
        });
    }

    #[test]
    fn test_field_errors_carry_path() {
        with_access("(a: \"x\", b: 1)", |access| {
            let mut access = access.unwrap();
            let err = access.required::<i32>("a").unwrap_err();
            assert_eq!(err.path(), &[PathSegment::Field("a".into())]);
            assert_eq!(err.span().start.col, 5);

            let err = access.required_explicit::<i32>("b").unwrap_err();
            assert_eq!(err.path(), &[PathSegment::Field("b".into())]);
        });
    }

    #[test]
    fn test_optional_and_defaults() {
        with_access("(a: 1, e: Some(2), n: None)", |access| {
            let mut access = access.unwrap();
            assert_eq!(access.optional::<i32>("a").unwrap(), Some(1));
            assert_eq!(access.optional::<i32>("missing").unwrap(), None);
            assert_eq!(access.with_default::<i32>("missing").unwrap(), 0);
            assert_eq!(access.with_default_fn("missing", || 7).unwrap(), 7);
            assert_eq!(access.with_default_explicit::<i32>("e").unwrap(), Some(2));
            assert_eq!(access.with_default_explicit::<i32>("n").unwrap(), None);
            assert_eq!(
                access.with_default_explicit::<i32>("missing").unwrap(),
                None
            );
            let err = access.required_explicit::<i32>("missing").unwrap_err();
            assert_eq!(
                err.to_string(),
                "1:1-1:28: missing required field `missing` in `Test`"
            );
        });
    }
}
