//! Following a cursor path through a schema.
//!
//! Turns the [`PathStep`]s produced by [`crate::cursor`] into the schema type
//! (or enum variant) at the cursor, resolving `TypeRef`s, implicit `Some`,
//! collections, enum variants and flattened fields on the way.

use ron2::schema::{Field, Schema, SchemaResolver, TypeKind, Variant, VariantKind};

use crate::cursor::PathStep;

/// The resolver type used by the LSP features (allows in-memory resolvers in tests).
pub type DynResolver = dyn SchemaResolver;

/// A point in the schema reached by following a path.
#[derive(Debug, Clone)]
pub enum Target {
    /// A type, with the documentation of the schema it came from (if any).
    Type { kind: TypeKind, doc: Option<String> },
    /// A selected enum variant.
    Variant(Variant),
}

impl Target {
    pub fn root(schema: &Schema) -> Self {
        Self::Type {
            kind: schema.kind.clone(),
            doc: schema.doc.clone(),
        }
    }

    fn ty(kind: TypeKind) -> Self {
        Self::Type { kind, doc: None }
    }
}

/// A concrete type: no `TypeRef` at the top level.
pub struct Resolved {
    pub kind: TypeKind,
    pub doc: Option<String>,
}

/// Follow `TypeRef`s (and, if `unwrap_option`, implicit `Some`) to a concrete type.
///
/// Returns `None` for unknown or cyclic `TypeRef`s.
pub fn resolve<R: SchemaResolver + ?Sized>(
    kind: &TypeKind,
    doc: Option<String>,
    unwrap_option: bool,
    resolver: &R,
) -> Option<Resolved> {
    resolve_tracked(kind, doc, unwrap_option, resolver, &mut Vec::new())
}

/// [`resolve`], recording every `TypeRef` followed in `visited` and failing
/// on one that is already there.
fn resolve_tracked<R: SchemaResolver + ?Sized>(
    kind: &TypeKind,
    doc: Option<String>,
    unwrap_option: bool,
    resolver: &R,
    visited: &mut Vec<String>,
) -> Option<Resolved> {
    let mut current = Resolved {
        kind: kind.clone(),
        doc,
    };
    loop {
        match current.kind {
            TypeKind::TypeRef(path) => {
                if visited.contains(&path) {
                    return None;
                }
                let schema = resolver.resolve(&path)?;
                visited.push(path);
                current = Resolved {
                    kind: schema.kind.clone(),
                    doc: schema.doc.clone(),
                };
            }
            TypeKind::Option(inner) if unwrap_option => {
                current.kind = *inner;
            }
            kind => {
                current.kind = kind;
                return Some(current);
            }
        }
    }
}

/// Follow a path from `root`.
pub fn follow<R: SchemaResolver + ?Sized>(
    root: Target,
    path: &[PathStep],
    resolver: &R,
) -> Option<Target> {
    path.iter()
        .try_fold(root, |target, step| step_into(target, step, resolver))
}

fn step_into<R: SchemaResolver + ?Sized>(
    target: Target,
    step: &PathStep,
    resolver: &R,
) -> Option<Target> {
    if let PathStep::Field(name) = step {
        return struct_fields(&target, resolver)?
            .into_iter()
            .find(|f| &f.name == name)
            .map(|f| Target::ty(f.ty));
    }

    let (kind, doc) = match target {
        Target::Variant(variant) => {
            return match (step, variant.kind) {
                (PathStep::Element(i), VariantKind::Tuple(types)) => {
                    types.into_iter().nth(*i).map(Target::ty)
                }
                _ => None,
            };
        }
        Target::Type { kind, doc } => (kind, doc),
    };
    let unwrap_option = !matches!(step, PathStep::OptionSome);
    let Resolved { kind, doc } = resolve(&kind, doc, unwrap_option, resolver)?;

    match (step, kind) {
        (PathStep::Variant(name), TypeKind::Enum { variants }) => variants
            .into_iter()
            .find(|v| &v.name == name)
            .map(Target::Variant),
        // A named struct `Name(...)` or tuple struct: the name does not change the type.
        (PathStep::Variant(_), kind) => Some(Target::Type { kind, doc }),
        (PathStep::Element(i), TypeKind::Tuple(types)) => types.into_iter().nth(*i).map(Target::ty),
        (PathStep::SeqItem, TypeKind::List(inner)) => Some(Target::ty(*inner)),
        (PathStep::MapKey, TypeKind::Map { key, .. }) => Some(Target::ty(*key)),
        (PathStep::MapValue, TypeKind::Map { value, .. }) => Some(Target::ty(*value)),
        (PathStep::OptionSome, TypeKind::Option(inner)) => Some(Target::ty(*inner)),
        // `Some(...)` around a type that is not an `Option`: accept the inner value as-is.
        (PathStep::OptionSome, kind) => Some(Target::Type { kind, doc }),
        _ => None,
    }
}

/// The fields of a struct or struct variant, with flattened fields inlined.
///
/// Returns `None` if the target is not struct-like.
pub fn struct_fields<R: SchemaResolver + ?Sized>(
    target: &Target,
    resolver: &R,
) -> Option<Vec<Field>> {
    // `TypeRef`s on the way to the fields, so flattening never re-enters them.
    let mut stack = Vec::new();
    let fields = match target {
        Target::Variant(Variant {
            kind: VariantKind::Struct(fields),
            ..
        }) => fields.clone(),
        Target::Variant(_) => return None,
        Target::Type { kind, .. } => {
            match resolve_tracked(kind, None, true, resolver, &mut stack)?.kind {
                TypeKind::Struct { fields } => fields,
                _ => return None,
            }
        }
    };
    let mut out = Vec::with_capacity(fields.len());
    inline_flattened(fields, resolver, &mut stack, &mut out);
    Some(out)
}

fn inline_flattened<R: SchemaResolver + ?Sized>(
    fields: Vec<Field>,
    resolver: &R,
    // `TypeRef`s being resolved or inlined, to stop on self-flattening schemas.
    stack: &mut Vec<String>,
    out: &mut Vec<Field>,
) {
    for field in fields {
        if !field.flattened {
            // A root schema loaded from a file is not on the stack, so a self-flattening
            // root is inlined once more; its repeated fields are dropped here.
            if !out.iter().any(|f| f.name == field.name) {
                out.push(field);
            }
            continue;
        }
        // A flattened map accepts arbitrary keys, so it contributes no named fields.
        // Resolving through the stack skips any `TypeRef` (also behind `Option`)
        // that is already being inlined.
        let depth = stack.len();
        if let Some(Resolved {
            kind: TypeKind::Struct { fields },
            ..
        }) = resolve_tracked(&field.ty, None, true, resolver, stack)
        {
            inline_flattened(fields, resolver, stack, out);
        }
        stack.truncate(depth);
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use std::sync::Arc;

    use ahash::HashMap;

    use super::*;

    /// An in-memory resolver for tests.
    pub(crate) struct MapResolver(pub HashMap<String, Arc<Schema>>);

    impl SchemaResolver for MapResolver {
        fn resolve(&self, type_path: &str) -> Option<Arc<Schema>> {
            self.0.get(type_path).cloned()
        }
    }

    pub(crate) fn resolver(schemas: &[(&str, TypeKind)]) -> MapResolver {
        MapResolver(
            schemas
                .iter()
                .map(|(path, kind)| ((*path).to_string(), Arc::new(Schema::new(kind.clone()))))
                .collect(),
        )
    }

    fn type_ref(path: &str) -> TypeKind {
        TypeKind::TypeRef(path.to_string())
    }

    #[test]
    fn resolve_stops_on_typeref_cycle() {
        let r = resolver(&[("a::A", type_ref("a::B")), ("a::B", type_ref("a::A"))]);
        assert!(resolve(&type_ref("a::A"), None, true, &r).is_none());
    }

    #[test]
    fn follow_through_option_list_and_variant() {
        let r = resolver(&[
            (
                "t::Root",
                TypeKind::Struct {
                    fields: vec![Field::new(
                        "items",
                        TypeKind::Option(Box::new(TypeKind::List(Box::new(type_ref("t::Item"))))),
                    )],
                },
            ),
            (
                "t::Item",
                TypeKind::Enum {
                    variants: vec![Variant::struct_variant(
                        "Pair",
                        vec![Field::new("left", TypeKind::Bool)],
                    )],
                },
            ),
        ]);
        let root = Target::ty(type_ref("t::Root"));
        let path = [
            PathStep::Field("items".into()),
            PathStep::SeqItem,
            PathStep::Variant("Pair".into()),
            PathStep::Field("left".into()),
        ];
        let target = follow(root, &path, &r).expect("path resolves");
        assert!(matches!(
            target,
            Target::Type {
                kind: TypeKind::Bool,
                ..
            }
        ));
    }

    #[test]
    fn flattened_struct_fields_are_inlined_and_cycles_terminate() {
        let r = resolver(&[
            (
                "t::Outer",
                TypeKind::Struct {
                    fields: vec![
                        Field::new("a", TypeKind::Bool),
                        Field::flattened("inner", type_ref("t::Inner")),
                    ],
                },
            ),
            (
                "t::Inner",
                TypeKind::Struct {
                    fields: vec![
                        Field::new("b", TypeKind::Bool),
                        Field::flattened("again", type_ref("t::Inner")),
                    ],
                },
            ),
        ]);
        let fields = struct_fields(&Target::ty(type_ref("t::Outer")), &r).expect("struct");
        let names: Vec<_> = fields.iter().map(|f| f.name.as_str()).collect();
        assert_eq!(names, ["a", "b"]);
    }

    #[test]
    fn flattened_option_cycles_terminate_without_duplicates() {
        let opt = |path: &str| TypeKind::Option(Box::new(type_ref(path)));
        let r = resolver(&[
            (
                "t::FlatOpt",
                TypeKind::Struct {
                    fields: vec![
                        Field::new("fz", TypeKind::Bool),
                        Field::flattened("again", opt("t::FlatOpt")),
                    ],
                },
            ),
            (
                "t::A",
                TypeKind::Struct {
                    fields: vec![
                        Field::new("a", TypeKind::Bool),
                        Field::flattened("b", opt("t::B")),
                    ],
                },
            ),
            (
                "t::B",
                TypeKind::Struct {
                    fields: vec![
                        Field::new("b", TypeKind::Bool),
                        Field::flattened("a", opt("t::A")),
                    ],
                },
            ),
        ]);
        let names = |root: &str| -> Vec<String> {
            struct_fields(&Target::ty(type_ref(root)), &r)
                .expect("struct")
                .into_iter()
                .map(|f| f.name)
                .collect()
        };
        assert_eq!(names("t::FlatOpt"), ["fz"]);
        assert_eq!(names("t::A"), ["a", "b"]);
        assert_eq!(names("t::B"), ["b", "a"]);
    }
}
