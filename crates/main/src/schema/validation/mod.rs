//! Validate parsed RON against a [`Schema`].
//!
//! A value is valid when `FromRon` for the type the schema describes (derived
//! or built-in) would accept it. Validation walks the AST, so every issue
//! carries the span of the offending expression.
//!
//! Issues have a [`Severity`]. Errors are values `FromRon` would reject.
//! Warnings flag things that are accepted but likely wrong, or that could not
//! be checked:
//! - unknown fields in a type without `deny_unknown_fields` (`FromRon`
//!   ignores them, but they are usually typos),
//! - a `TypeRef` the [`SchemaResolver`] could not resolve
//!   ([`ErrorKind::UnresolvedTypeRef`](crate::error::ErrorKind::UnresolvedTypeRef)).

mod fields;

use alloc::{borrow::Cow, string::String, sync::Arc, vec::Vec};

use ahash::{HashMap, HashMapExt, HashSet, HashSetExt};

use super::{Schema, TypeKind, Variant, VariantKind};
use crate::{
    ast::{Expr, StructBody, StructExpr, expr_to_value},
    convert::{
        expr_type_name,
        number::{expr_to_float, expr_to_int},
    },
    error::{Error, ErrorKind, Span, ValidationError},
    fmt::{FormatConfig, format_expr},
};

/// Resolves `TypeRef` paths to schemas during validation.
///
/// Each type path is resolved at most once per validation run, so
/// implementations need no cache of their own for that.
///
/// # Example
///
/// ```rust
/// use std::{collections::HashMap, sync::Arc};
///
/// use ron2::schema::{Schema, SchemaResolver, TypeKind};
///
/// struct MapResolver(HashMap<String, Arc<Schema>>);
///
/// impl SchemaResolver for MapResolver {
///     fn resolve(&self, type_path: &str) -> Option<Arc<Schema>> {
///         self.0.get(type_path).cloned()
///     }
/// }
///
/// let mut schemas = HashMap::new();
/// schemas.insert("my::Id".to_string(), Arc::new(Schema::new(TypeKind::U64)));
/// assert!(MapResolver(schemas).resolve("my::Id").is_some());
/// ```
pub trait SchemaResolver {
    /// Resolve a type path to its schema, or `None` if it is unknown.
    fn resolve(&self, type_path: &str) -> Option<Arc<Schema>>;
}

impl<R: SchemaResolver + ?Sized> SchemaResolver for &R {
    fn resolve(&self, type_path: &str) -> Option<Arc<Schema>> {
        (**self).resolve(type_path)
    }
}

/// A resolver that knows no schemas: every `TypeRef` is reported as
/// unresolved.
#[derive(Debug, Clone, Copy, Default)]
pub struct NoResolver;

impl SchemaResolver for NoResolver {
    fn resolve(&self, _type_path: &str) -> Option<Arc<Schema>> {
        None
    }
}

/// How serious a [`ValidationIssue`] is.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Severity {
    /// `FromRon` would reject the value.
    Error,
    /// The value is accepted, but is likely a mistake or could not be checked.
    Warning,
}

/// A problem found by [`validate_all`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValidationIssue {
    /// Whether this is an error or a warning.
    pub severity: Severity,
    /// What is wrong, with the span of the offending expression.
    pub error: ValidationError,
}

/// Validate an expression against a schema, stopping at the first error.
///
/// Warnings (see the [module docs](self)) do not fail validation.
///
/// # Example
///
/// ```rust
/// use ron2::{
///     ast::parse_document,
///     schema::{Field, NoResolver, Schema, TypeKind, validate},
/// };
///
/// let schema = Schema::new(TypeKind::Struct {
///     fields: vec![
///         Field::new("port", TypeKind::U16),
///         Field::optional("host", TypeKind::String),
///     ],
/// });
///
/// let doc = parse_document("(port: 8080)").unwrap();
/// assert!(validate(doc.value.as_ref().unwrap(), &schema, &NoResolver).is_ok());
///
/// let doc = parse_document("(port: 70000)").unwrap();
/// assert!(validate(doc.value.as_ref().unwrap(), &schema, &NoResolver).is_err());
/// ```
pub fn validate<R: SchemaResolver + ?Sized>(
    expr: &Expr<'_>,
    schema: &Schema,
    resolver: &R,
) -> Result<(), ValidationError> {
    let mut walker = Walker::new(resolver, true);
    walker.check_schema(expr, schema);
    match walker
        .issues
        .into_iter()
        .find(|issue| issue.severity == Severity::Error)
    {
        Some(issue) => Err(issue.error),
        None => Ok(()),
    }
}

/// Validate an expression against a schema, collecting every issue.
///
/// `Expr::Error` nodes are skipped: the parser already reported them.
///
/// # Example
///
/// ```rust
/// use ron2::{
///     ast::parse_document,
///     schema::{Field, NoResolver, Schema, Severity, TypeKind, validate_all},
/// };
///
/// let schema = Schema::new(TypeKind::Struct {
///     fields: vec![Field::new("port", TypeKind::U16)],
/// });
/// let doc = parse_document(r#"(port: "x", prot: 1)"#).unwrap();
///
/// let issues = validate_all(doc.value.as_ref().unwrap(), &schema, &NoResolver);
/// let severities: Vec<_> = issues.iter().map(|i| i.severity).collect();
/// // Wrong type for `port`; `prot` is unknown (a warning, since the schema
/// // does not deny unknown fields).
/// assert_eq!(severities, [Severity::Error, Severity::Warning]);
/// ```
pub fn validate_all<R: SchemaResolver + ?Sized>(
    expr: &Expr<'_>,
    schema: &Schema,
    resolver: &R,
) -> Vec<ValidationIssue> {
    let mut walker = Walker::new(resolver, false);
    walker.check_schema(expr, schema);
    walker.issues
}

/// Type-level metadata, present when a kind is the root of a [`Schema`]
/// (the document's schema or a resolved `TypeRef`), i.e. a named Rust type.
#[derive(Clone, Copy)]
struct TypeMeta<'s> {
    name: Option<&'s str>,
    deny_unknown_fields: bool,
}

impl<'s> TypeMeta<'s> {
    fn of(schema: &'s Schema) -> Self {
        Self {
            name: schema.name.as_deref(),
            deny_unknown_fields: schema.deny_unknown_fields,
        }
    }
}

struct Walker<'r, R: ?Sized> {
    resolver: &'r R,
    /// Per-run resolution cache, including misses.
    resolved: HashMap<String, Option<Arc<Schema>>>,
    /// Type paths already reported as unresolved.
    unresolved_reported: HashSet<String>,
    /// `TypeRef`s being expanded, as (expression address, schema address).
    /// Re-entering one at the same expression means a `TypeRef` cycle that
    /// consumes no value structure; recursion into child values is fine.
    expanding: Vec<(usize, usize)>,
    stop_at_first_error: bool,
    stopped: bool,
    issues: Vec<ValidationIssue>,
}

impl<'r, R: SchemaResolver + ?Sized> Walker<'r, R> {
    fn new(resolver: &'r R, stop_at_first_error: bool) -> Self {
        Self {
            resolver,
            resolved: HashMap::new(),
            unresolved_reported: HashSet::new(),
            expanding: Vec::new(),
            stop_at_first_error,
            stopped: false,
            issues: Vec::new(),
        }
    }

    fn resolve(&mut self, type_path: &str) -> Option<Arc<Schema>> {
        if let Some(cached) = self.resolved.get(type_path) {
            return cached.clone();
        }
        let schema = self.resolver.resolve(type_path);
        self.resolved.insert(type_path.to_owned(), schema.clone());
        schema
    }

    fn report(&mut self, severity: Severity, error: ValidationError) {
        if severity == Severity::Error && self.stop_at_first_error {
            self.stopped = true;
        }
        self.issues.push(ValidationIssue { severity, error });
    }

    fn error(&mut self, kind: ErrorKind, span: Span) {
        self.report(Severity::Error, Error::with_span(kind, span));
    }

    fn mismatch(&mut self, expected: impl Into<String>, expr: &Expr<'_>) {
        self.error(
            ErrorKind::TypeMismatch {
                expected: expected.into(),
                found: expr_type_name(expr).into(),
            },
            *expr.span(),
        );
    }

    /// Add a path segment to every issue reported since `start`.
    fn wrap_since(&mut self, start: usize, wrap: impl Fn(ValidationError) -> ValidationError) {
        if self.issues.len() > start {
            let tail = self.issues.split_off(start);
            self.issues
                .extend(tail.into_iter().map(|issue| ValidationIssue {
                    error: wrap(issue.error),
                    ..issue
                }));
        }
    }

    fn check_schema(&mut self, expr: &Expr<'_>, schema: &Schema) {
        self.check(expr, &schema.kind, Some(TypeMeta::of(schema)));
    }

    fn check(&mut self, expr: &Expr<'_>, kind: &TypeKind, meta: Option<TypeMeta<'_>>) {
        if self.stopped || matches!(expr, Expr::Error(_)) {
            return;
        }
        match kind {
            TypeKind::Bool => self.expect(matches!(expr, Expr::Bool(_)), kind, expr),
            TypeKind::I8 => self.check_int::<i8>(expr, "i8"),
            TypeKind::I16 => self.check_int::<i16>(expr, "i16"),
            TypeKind::I32 => self.check_int::<i32>(expr, "i32"),
            TypeKind::I64 => self.check_int::<i64>(expr, "i64"),
            TypeKind::I128 => self.check_int::<i128>(expr, "i128"),
            TypeKind::U8 => self.check_int::<u8>(expr, "u8"),
            TypeKind::U16 => self.check_int::<u16>(expr, "u16"),
            TypeKind::U32 => self.check_int::<u32>(expr, "u32"),
            TypeKind::U64 => self.check_int::<u64>(expr, "u64"),
            TypeKind::U128 => self.check_int::<u128>(expr, "u128"),
            TypeKind::F32 => self.convert(expr_to_float::<f32>(expr)),
            TypeKind::F64 => self.convert(expr_to_float::<f64>(expr)),
            TypeKind::Char => self.expect(matches!(expr, Expr::Char(_)), kind, expr),
            TypeKind::String => self.expect(matches!(expr, Expr::String(_)), kind, expr),
            TypeKind::Unit => self.check_unit(expr, meta),
            TypeKind::Option(inner) => match expr {
                Expr::Option(opt) => {
                    if let Some(value) = &opt.value {
                        self.check(&value.expr, inner, None);
                    }
                }
                // `Option<T>` accepts a bare value as implicit `Some`.
                _ => self.check(expr, inner, None),
            },
            TypeKind::List(inner) => self.check_list(expr, kind, inner, None),
            TypeKind::Array { element, len } => self.check_list(expr, kind, element, Some(*len)),
            TypeKind::Map { key, value } => self.check_map(expr, kind, key, value),
            TypeKind::Tuple(types) => self.check_tuple(expr, kind, types, meta),
            TypeKind::Struct { fields } => self.check_struct(expr, fields, meta),
            TypeKind::Enum { variants } => self.check_enum(expr, variants, meta),
            TypeKind::TypeRef(type_path) => self.check_type_ref(expr, type_path),
        }
    }

    fn expect(&mut self, ok: bool, kind: &TypeKind, expr: &Expr<'_>) {
        if !ok {
            self.mismatch(kind.to_string(), expr);
        }
    }

    /// Number literals are checked by the same conversion `FromRon` uses, so
    /// ranges, suffixes and bases agree exactly.
    fn check_int<T>(&mut self, expr: &Expr<'_>, name: &'static str)
    where
        T: TryFrom<u128> + TryFrom<i128> + TryFrom<u8>,
    {
        self.convert(expr_to_int::<T>(expr, name));
    }

    fn convert<T>(&mut self, result: crate::error::Result<T>) {
        if let Err(e) = result {
            self.report(Severity::Error, e);
        }
    }

    /// Check `s`'s name against the schema's type name, if known.
    fn check_name(&mut self, s: &StructExpr<'_>, meta: Option<TypeMeta<'_>>) {
        if let Some(expected) = meta.and_then(|m| m.name)
            && s.name.name != expected
        {
            self.error(
                ErrorKind::TypeMismatch {
                    expected: alloc::format!("struct {expected}"),
                    found: alloc::format!("struct {}", s.name.name),
                },
                s.name.span,
            );
        }
    }

    fn check_unit(&mut self, expr: &Expr<'_>, meta: Option<TypeMeta<'_>>) {
        let name = meta.and_then(|m| m.name);
        match expr {
            Expr::Unit(_) => {}
            // A unit struct may also be written by its declared name:
            // `Marker`. Without a declared name (`()`, transparent wrappers,
            // old schema files) no name is accepted.
            Expr::Struct(s) if s.body.is_none() && name.is_some() => self.check_name(s, meta),
            _ => match name {
                Some(name) => self.mismatch(alloc::format!("unit struct {name}"), expr),
                None => self.mismatch("()", expr),
            },
        }
    }

    fn check_list(
        &mut self,
        expr: &Expr<'_>,
        kind: &TypeKind,
        inner: &TypeKind,
        len: Option<usize>,
    ) {
        let Some(items) = seq_items(expr) else {
            return self.mismatch(kind.to_string(), expr);
        };
        if let Some(len) = len
            && items.len() != len
        {
            return self.error(
                ErrorKind::LengthMismatch {
                    expected: alloc::format!("{len} elements"),
                    found: items.len(),
                    context: None,
                },
                *expr.span(),
            );
        }
        for (i, item) in items.into_iter().enumerate() {
            if self.stopped {
                return;
            }
            let start = self.issues.len();
            self.check(item, inner, None);
            self.wrap_since(start, |e| e.in_element(i));
        }
    }

    fn check_map(&mut self, expr: &Expr<'_>, kind: &TypeKind, key: &TypeKind, value: &TypeKind) {
        let Expr::Map(map) = expr else {
            return self.mismatch(kind.to_string(), expr);
        };
        let mut seen_keys = HashSet::with_capacity(map.entries.len());
        for entry in &map.entries {
            if self.stopped {
                return;
            }
            let start = self.issues.len();
            self.check(&entry.key, key, None);
            self.wrap_since(start, ValidationError::in_map_key);

            let start = self.issues.len();
            self.check(&entry.value, value, None);
            if self.issues.len() > start {
                let label = key_label(&entry.key);
                self.wrap_since(start, |e| e.in_map_value(label.clone()));
            }

            // FromRon rejects repeated keys.
            if let Ok(key_value) = expr_to_value(&entry.key)
                && !seen_keys.insert(key_value)
            {
                self.error(
                    ErrorKind::DuplicateKey {
                        key: key_label(&entry.key),
                    },
                    *entry.key.span(),
                );
            }
        }
    }

    fn check_tuple(
        &mut self,
        expr: &Expr<'_>,
        kind: &TypeKind,
        types: &[TypeKind],
        meta: Option<TypeMeta<'_>>,
    ) {
        let elements = match expr {
            // A tuple struct may be written by its declared name:
            // `Point(1, 2)`.
            Expr::Struct(s) if meta.is_some_and(|m| m.name.is_some()) => {
                self.check_name(s, meta);
                let Some(StructBody::Tuple(body)) = &s.body else {
                    return self.mismatch(kind.to_string(), expr);
                };
                body.elements.iter().map(|e| &e.expr).collect()
            }
            _ => match seq_items(expr) {
                Some(elements) => elements,
                None => return self.mismatch(kind.to_string(), expr),
            },
        };
        self.check_elements(&elements, types, *expr.span());
    }

    fn check_elements(&mut self, items: &[&Expr<'_>], types: &[TypeKind], span: Span) {
        if items.len() != types.len() {
            return self.error(
                ErrorKind::LengthMismatch {
                    expected: alloc::format!("{} elements", types.len()),
                    found: items.len(),
                    context: None,
                },
                span,
            );
        }
        for (i, (item, ty)) in items.iter().zip(types).enumerate() {
            if self.stopped {
                return;
            }
            let start = self.issues.len();
            self.check(item, ty, None);
            self.wrap_since(start, |e| e.in_element(i));
        }
    }

    fn check_struct(
        &mut self,
        expr: &Expr<'_>,
        fields: &[super::Field],
        meta: Option<TypeMeta<'_>>,
    ) {
        let deny_unknown = meta.is_some_and(|m| m.deny_unknown_fields);
        match expr {
            // `()` and `Name()` are accepted when every field has a default.
            Expr::Unit(unit) => self.check_defaults_only(fields, unit.span),
            Expr::AnonStruct(s) => self.check_fields(&s.fields, fields, s.span, deny_unknown),
            Expr::Struct(s) => {
                self.check_name(s, meta);
                match &s.body {
                    Some(StructBody::Fields(body)) => {
                        self.check_fields(&body.fields, fields, s.span, deny_unknown);
                    }
                    Some(StructBody::Tuple(body)) if body.elements.is_empty() => {
                        self.check_defaults_only(fields, s.span);
                    }
                    _ => self.mismatch(struct_label(meta), expr),
                }
            }
            _ => self.mismatch(struct_label(meta), expr),
        }
    }

    fn check_enum(&mut self, expr: &Expr<'_>, variants: &[Variant], meta: Option<TypeMeta<'_>>) {
        let deny_unknown = meta.is_some_and(|m| m.deny_unknown_fields);
        let s = match expr {
            Expr::Struct(s) => s,
            // `None` / `Some(x)` parse as options, but name enum variants
            // when the enum has them.
            Expr::Option(opt) => return self.check_option_variant(expr, opt, variants),
            _ => return self.mismatch("enum", expr),
        };

        let name = s.name.name.as_ref();
        let Some(variant) = variants.iter().find(|v| v.name == name) else {
            return self.error(
                ErrorKind::UnknownVariant {
                    variant: Cow::Owned(name.to_owned()),
                    expected: &[],
                    outer: None,
                },
                s.name.span,
            );
        };

        let start = self.issues.len();
        match (&variant.kind, &s.body) {
            (VariantKind::Unit, None) => {}
            (VariantKind::Unit, Some(StructBody::Tuple(body))) if body.elements.is_empty() => {}
            (VariantKind::Tuple(types), Some(StructBody::Tuple(body))) => {
                let items: Vec<_> = body.elements.iter().map(|e| &e.expr).collect();
                self.check_elements(&items, types, s.span);
            }
            (VariantKind::Struct(fields), Some(StructBody::Fields(body))) => {
                self.check_fields(&body.fields, fields, s.span, deny_unknown);
            }
            // `V()` and bare `V`, like `FromRon`, when every field has a default.
            (VariantKind::Struct(fields), Some(StructBody::Tuple(body)))
                if body.elements.is_empty() =>
            {
                self.check_defaults_only(fields, s.span);
            }
            (VariantKind::Struct(fields), None) => self.check_defaults_only(fields, s.span),
            (kind, _) => {
                let expected = match kind {
                    VariantKind::Unit => "unit variant",
                    VariantKind::Tuple(_) => "tuple variant",
                    VariantKind::Struct(_) => "struct variant",
                };
                self.error(
                    ErrorKind::TypeMismatch {
                        expected: alloc::format!("{expected} {name}"),
                        found: "variant with other content".into(),
                    },
                    s.span,
                );
            }
        }
        self.wrap_since(start, |e| e.in_variant(name));
    }

    fn check_option_variant(
        &mut self,
        expr: &Expr<'_>,
        opt: &crate::ast::OptionExpr<'_>,
        variants: &[Variant],
    ) {
        let (name, ok) = match &opt.value {
            None => (
                "None",
                variants
                    .iter()
                    .any(|v| v.name == "None" && matches!(v.kind, VariantKind::Unit)),
            ),
            Some(value) => {
                let some = variants.iter().find_map(|v| match &v.kind {
                    VariantKind::Tuple(types) if v.name == "Some" && types.len() == 1 => {
                        types.first()
                    }
                    _ => None,
                });
                if let Some(ty) = some {
                    let start = self.issues.len();
                    self.check(&value.expr, ty, None);
                    self.wrap_since(start, |e| e.in_element(0).in_variant("Some"));
                }
                ("Some", some.is_some())
            }
        };
        if !ok {
            self.mismatch(alloc::format!("enum without a {name} variant"), expr);
        }
    }

    /// Warn that `type_path` could not be resolved, once per run: a list of
    /// 100k values of a missing type yields one warning, not 100k.
    pub(super) fn report_unresolved(&mut self, type_path: &str, span: Span) {
        if self.unresolved_reported.insert(type_path.to_owned()) {
            self.report(
                Severity::Warning,
                Error::with_span(ErrorKind::UnresolvedTypeRef(type_path.to_owned()), span),
            );
        }
    }

    fn check_type_ref(&mut self, expr: &Expr<'_>, type_path: &str) {
        let Some(schema) = self.resolve(type_path) else {
            return self.report_unresolved(type_path, *expr.span());
        };

        let key = (
            core::ptr::from_ref(expr).addr(),
            Arc::as_ptr(&schema).addr(),
        );
        if self.expanding.contains(&key) {
            return;
        }
        self.expanding.push(key);
        let start = self.issues.len();
        self.check_schema(expr, &schema);
        self.wrap_since(start, |e| e.in_type_ref(type_path));
        self.expanding.pop();
    }
}

fn struct_label(meta: Option<TypeMeta<'_>>) -> String {
    match meta.and_then(|m| m.name) {
        Some(name) => alloc::format!("struct {name}"),
        None => "struct".into(),
    }
}

/// The elements of a sequence or tuple: what `FromRon` accepts for lists,
/// arrays and tuples.
fn seq_items<'e, 'a>(expr: &'e Expr<'a>) -> Option<Vec<&'e Expr<'a>>> {
    match expr {
        Expr::Seq(seq) => Some(seq.items.iter().map(|item| &item.expr).collect()),
        Expr::Tuple(tuple) => Some(tuple.elements.iter().map(|e| &e.expr).collect()),
        _ => None,
    }
}

/// A short label for a map key in error paths.
/// Rendered as RON, as `FromRon` does.
fn key_label(expr: &Expr<'_>) -> String {
    format_expr(expr, &FormatConfig::minimal())
}
