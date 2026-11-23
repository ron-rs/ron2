//! Struct field validation, including `#[ron(flatten)]`.

use alloc::{borrow::Cow, sync::Arc, vec::Vec};

use ahash::{HashSet, HashSetExt};

use super::{SchemaResolver, Severity, Walker};
use crate::{
    ast::StructField,
    error::{Error, ErrorKind, Span},
    schema::{Field, Schema, TypeKind},
};

/// Bound on `TypeRef` alias chains (`A = B`, `B = A`), guarding against
/// cyclic schemas.
const MAX_REF_CHAIN: usize = 16;

/// A kind borrowed from the schema being walked, or owned by a resolved schema.
enum Handle<'s> {
    Borrowed(&'s TypeKind),
    Shared(Arc<Schema>),
}

impl Handle<'_> {
    /// The kind with `Option` wrappers removed.
    fn kind(&self) -> &TypeKind {
        let mut kind = match self {
            Handle::Borrowed(kind) => kind,
            Handle::Shared(schema) => &schema.kind,
        };
        while let TypeKind::Option(inner) = kind {
            kind = inner;
        }
        kind
    }
}

/// A flattened struct whose fields appear directly in the parent.
struct FlatStruct<'s> {
    handle: Handle<'s>,
    /// The optional flattens (`Option<Inner>` or an optional field) this
    /// struct is nested in, as group ids. A group is absent when none of the
    /// fields of its structs are written; then its fields are not required.
    /// Once any is written, all its required fields are.
    groups: Vec<usize>,
}

impl FlatStruct<'_> {
    fn fields(&self) -> &[Field] {
        match self.handle.kind() {
            TypeKind::Struct { fields } => fields,
            _ => &[],
        }
    }
}

#[derive(Default)]
struct Flattened<'s> {
    structs: Vec<FlatStruct<'s>>,
    /// A flattened `Map<String, V>` takes every otherwise unknown field.
    map: Option<Handle<'s>>,
    /// A flattened type could not be resolved, so any field may belong to it.
    open: bool,
    group_count: usize,
}

impl Flattened<'_> {
    /// Whether each optional-flatten group has any field written.
    fn present_groups(&self, seen: &HashSet<&str>) -> Vec<bool> {
        let mut present = vec![false; self.group_count];
        for flat in &self.structs {
            if flat.fields().iter().any(|f| seen.contains(f.name.as_str())) {
                for &group in &flat.groups {
                    present[group] = true;
                }
            }
        }
        present
    }

    fn find_field(&self, name: &str) -> Option<&Field> {
        self.structs
            .iter()
            .find_map(|s| s.fields().iter().find(|f| !f.flattened && f.name == name))
    }

    fn map_value(&self) -> Option<&TypeKind> {
        match self.map.as_ref()?.kind() {
            TypeKind::Map { value, .. } => Some(value),
            _ => None,
        }
    }
}

impl<R: SchemaResolver + ?Sized> Walker<'_, R> {
    /// Check the fields of a struct (or struct variant) written with a body.
    pub(super) fn check_fields(
        &mut self,
        ast_fields: &[StructField<'_>],
        schema_fields: &[Field],
        span: Span,
        deny_unknown: bool,
    ) {
        let flattened = self.collect_flattened(schema_fields, span);
        let mut seen: HashSet<&str> = HashSet::with_capacity(ast_fields.len());

        for ast_field in ast_fields {
            if self.stopped {
                return;
            }
            let name = ast_field.name.name.as_ref();
            if !seen.insert(name) {
                self.error(
                    ErrorKind::DuplicateField {
                        field: Cow::Owned(name.to_owned()),
                        outer: None,
                    },
                    ast_field.name.span,
                );
                continue;
            }

            let start = self.issues.len();
            if let Some(field) = schema_fields
                .iter()
                .find(|f| !f.flattened && f.name == name)
                .or_else(|| flattened.find_field(name))
            {
                self.check(&ast_field.value, &field.ty, None);
            } else if let Some(value_kind) = flattened.map_value() {
                self.check(&ast_field.value, value_kind, None);
            } else if !flattened.open {
                let severity = if deny_unknown {
                    Severity::Error
                } else {
                    Severity::Warning
                };
                self.report(
                    severity,
                    Error::with_span(
                        ErrorKind::UnknownField {
                            field: Cow::Owned(name.to_owned()),
                            expected: &[],
                            outer: None,
                        },
                        ast_field.name.span,
                    ),
                );
            }
            self.wrap_since(start, |e| e.in_field(name));
        }

        self.report_missing(schema_fields, &seen, span);
        let present = flattened.present_groups(&seen);
        for flat in &flattened.structs {
            if flat.groups.iter().all(|&group| present[group]) {
                self.report_missing(flat.fields(), &seen, span);
            }
        }
    }

    /// Check a struct written without fields (`()` or `Name()`): valid when
    /// every field has a default. Flattened fields always do.
    pub(super) fn check_defaults_only(&mut self, schema_fields: &[Field], span: Span) {
        self.report_missing(schema_fields, &HashSet::new(), span);
    }

    fn report_missing(&mut self, schema_fields: &[Field], seen: &HashSet<&str>, span: Span) {
        for field in schema_fields {
            if !field.flattened && !field.optional && !seen.contains(field.name.as_str()) {
                self.error(
                    ErrorKind::MissingField {
                        field: Cow::Owned(field.name.clone()),
                        outer: None,
                    },
                    span,
                );
            }
        }
    }

    /// Resolve the targets of `fields`' flattened fields, recursing into
    /// flattened fields of flattened structs. Each struct is expanded at most
    /// once, so self-flattening schemas stay linear.
    fn collect_flattened<'s>(&mut self, fields: &'s [Field], span: Span) -> Flattened<'s> {
        let mut flattened = Flattened::default();
        let mut expanded: HashSet<usize> = HashSet::new();
        let mut pending: Vec<(Handle<'s>, bool, Vec<usize>)> = fields
            .iter()
            .filter(|f| f.flattened)
            .map(|f| (Handle::Borrowed(&f.ty), f.optional, Vec::new()))
            .collect();

        while let Some((handle, optional, mut groups)) = pending.pop() {
            let Some((handle, presence_based)) = self.resolve_flattened(handle, span) else {
                flattened.open = true;
                continue;
            };
            if presence_based || optional {
                groups.push(flattened.group_count);
                flattened.group_count += 1;
            }
            match handle.kind() {
                TypeKind::Struct { fields: inner }
                    if expanded.insert(core::ptr::from_ref(handle.kind()).addr()) =>
                {
                    // Nested field types live in `handle`; they are small
                    // (normally a `TypeRef`), so own them.
                    pending.extend(inner.iter().filter(|f| f.flattened).map(|f| {
                        let ty = Arc::new(Schema::new(f.ty.clone()));
                        (Handle::Shared(ty), f.optional, groups.clone())
                    }));
                    flattened.structs.push(FlatStruct { handle, groups });
                }
                TypeKind::Map { key, .. }
                    if flattened.map.is_none() && matches!(**key, TypeKind::String) =>
                {
                    flattened.map = Some(handle);
                }
                _ => {}
            }
        }
        flattened
    }

    /// Follow `Option`s and `TypeRef`s to the flattened type. Returns whether
    /// an `Option` was crossed, or `None` (after reporting) if a `TypeRef`
    /// could not be resolved.
    fn resolve_flattened<'s>(
        &mut self,
        mut handle: Handle<'s>,
        span: Span,
    ) -> Option<(Handle<'s>, bool)> {
        let mut presence_based = false;
        for _ in 0..MAX_REF_CHAIN {
            let mut kind = match &handle {
                Handle::Borrowed(kind) => *kind,
                Handle::Shared(schema) => &schema.kind,
            };
            while let TypeKind::Option(inner) = kind {
                presence_based = true;
                kind = inner;
            }
            let TypeKind::TypeRef(path) = kind else {
                return Some((handle, presence_based));
            };
            let Some(schema) = self.resolve(path) else {
                self.report_unresolved(path, span);
                return None;
            };
            handle = Handle::Shared(schema);
        }
        None
    }
}
