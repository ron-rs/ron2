//! Tests for schema collection and writing, including generic types.

#![cfg(feature = "derive")]

use std::{collections::HashMap, sync::Arc};

use ron2::{
    RonSchema,
    ast::parse_document,
    schema::{
        Field, RonSchema as RonSchemaTrait, TypeKind, collect::collect_schemas, read_schema,
        storage::type_path_to_file_path, validate, write_schemas,
    },
};

#[allow(dead_code)]
#[derive(RonSchema)]
struct GenericContainer<T> {
    item: T,
    child: Child,
}

#[allow(dead_code)]
#[derive(RonSchema)]
struct Child {
    value: u32,
}

/// Non-generic root that uses a generic type.
#[allow(dead_code)]
#[derive(RonSchema)]
struct Root {
    container: GenericContainer<u32>,
}

fn path<T: RonSchemaTrait>() -> &'static str {
    T::type_path().unwrap()
}

fn type_ref<T: RonSchemaTrait>() -> TypeKind {
    TypeKind::TypeRef(path::<T>().to_string())
}

/// Each instantiation of a generic type is a schema of its own, named like
/// `core::any::type_name` (and `#![type]` from `to_typed_ron`).
#[test]
fn generic_instantiations_have_their_own_path() {
    assert_eq!(
        path::<GenericContainer<u32>>(),
        "schema_generics::GenericContainer<u32>"
    );
    assert_ne!(
        path::<GenericContainer<u32>>(),
        path::<GenericContainer<String>>()
    );
    let catalog = collect_schemas::<GenericContainer<u32>>().expect("generic root");
    assert_eq!(catalog.schemas.len(), 2);
}

#[test]
fn collect_includes_generic_instantiations() {
    let catalog = collect_schemas::<Root>().expect("collect schemas");
    let keys: Vec<_> = catalog.schemas.keys().map(String::as_str).collect();
    assert_eq!(
        keys,
        vec![
            path::<Child>(),
            path::<GenericContainer<u32>>(),
            path::<Root>()
        ]
    );
    assert_eq!(
        catalog.schemas[path::<Root>()].kind,
        TypeKind::Struct {
            fields: vec![Field::new("container", type_ref::<GenericContainer<u32>>())],
        }
    );
    assert_eq!(
        catalog.schemas[path::<GenericContainer<u32>>()].kind,
        TypeKind::Struct {
            fields: vec![
                Field::new("item", TypeKind::U32),
                Field::new("child", type_ref::<Child>()),
            ],
        }
    );
}

/// The catalog resolves `TypeRef`s in memory, including generic ones.
#[test]
fn catalog_validates_through_generic_type_refs() {
    let catalog = collect_schemas::<Root>().expect("collect schemas");
    let schema = &catalog.schemas[path::<Root>()];
    let check = |input: &str| {
        let doc = parse_document(input).unwrap();
        validate(doc.value.as_ref().unwrap(), schema, &catalog)
    };
    check("(container: (item: 1, child: (value: 2)))").expect("valid");
    assert!(check("(container: (item: \"x\", child: (value: 2)))").is_err());
}

#[allow(dead_code)]
#[derive(RonSchema)]
enum Expr<T> {
    Lit(T),
    Add(Box<Expr<T>>, Box<Expr<T>>),
}

#[allow(dead_code)]
#[derive(RonSchema)]
struct ExprRoot {
    e: Expr<i32>,
}

#[allow(dead_code)]
#[derive(RonSchema)]
struct GTree<T> {
    v: T,
    kids: Vec<GTree<T>>,
    first: Option<T>,
}

/// Recursive generic types are referenced by their instantiation's path, so
/// their schemas are finite (this overflowed the stack when generic types
/// were inlined).
#[test]
fn recursive_generic_types_terminate() {
    assert_eq!(
        GTree::<u8>::type_kind(),
        TypeKind::Struct {
            fields: vec![
                Field::new("v", TypeKind::U8),
                Field::new("kids", TypeKind::List(Box::new(type_ref::<GTree<u8>>()))),
                Field::new("first", TypeKind::Option(Box::new(TypeKind::U8))),
            ],
        }
    );
    let catalog = collect_schemas::<ExprRoot>().expect("collect schemas");
    let keys: Vec<_> = catalog.schemas.keys().map(String::as_str).collect();
    assert_eq!(keys, vec![path::<Expr<i32>>(), path::<ExprRoot>()]);
}

/// Non-generic root, so the files have plain identifiers as names.
#[allow(dead_code)]
#[derive(RonSchema)]
struct Parent {
    child: Child,
}

#[test]
fn write_schemas_writes_referenced_types() {
    let temp_dir = tempfile::tempdir().expect("temp dir");
    let written = write_schemas::<Parent>(Some(temp_dir.path())).expect("write schemas");
    assert_eq!(written.len(), 2);

    let child_rel = type_path_to_file_path(path::<Child>()).unwrap();
    let child_schema = read_schema(&temp_dir.path().join(child_rel)).expect("read schema");
    assert_eq!(
        child_schema.kind,
        TypeKind::Struct {
            fields: vec![Field::new("value", TypeKind::U32)],
        }
    );
}

/// The trait's `write_schema` and `write_schemas` take the same directory type.
#[test]
fn trait_methods_write_to_the_same_directory() {
    let temp_dir = tempfile::tempdir().expect("temp dir");
    let dir: Option<&std::path::Path> = Some(temp_dir.path());

    let own = Parent::write_schema(dir).expect("write schema");
    let child = temp_dir
        .path()
        .join(type_path_to_file_path(path::<Child>()).unwrap());
    assert_eq!(
        own,
        temp_dir
            .path()
            .join(type_path_to_file_path(path::<Parent>()).unwrap())
    );
    assert!(!child.exists(), "write_schema writes only its own type");

    let mut all = Parent::write_schemas(dir).expect("write schemas");
    all.sort();
    let mut expected = vec![own, child];
    expected.sort();
    assert_eq!(all, expected);
}

/// Generic instantiations are written under the storage's encoding of their
/// path and read back by it.
#[test]
fn write_schemas_writes_generic_instantiations() {
    let temp_dir = tempfile::tempdir().expect("temp dir");
    let written = write_schemas::<Root>(Some(temp_dir.path())).expect("write schemas");
    assert_eq!(written.len(), 3);

    let rel = type_path_to_file_path(path::<GenericContainer<u32>>()).unwrap();
    let schema = read_schema(&temp_dir.path().join(rel)).expect("read schema");
    assert_eq!(schema, GenericContainer::<u32>::schema());
}

/// A recursive type is referenced by `TypeRef`, so its schema is finite and
/// collection terminates.
#[allow(dead_code)]
#[derive(RonSchema)]
struct Node {
    children: Vec<Node>,
    next: Option<Box<Node>>,
}

#[test]
fn recursive_types_terminate() {
    let node = TypeKind::TypeRef(path::<Node>().to_string());
    assert_eq!(
        Node::type_kind(),
        TypeKind::Struct {
            fields: vec![
                Field::new("children", TypeKind::List(Box::new(node.clone()))),
                Field::new("next", TypeKind::Option(Box::new(node))),
            ],
        }
    );
    let catalog = collect_schemas::<Node>().expect("collect schemas");
    assert_eq!(catalog.schemas.len(), 1);
}

mod config {
    #[allow(dead_code)]
    #[derive(ron2::RonSchema)]
    pub struct Inner {
        pub value: u8,
    }
}

type Port = u16;

/// Field types the derive cannot see through syntactically: aliases, `use`d
/// paths, wrappers, custom hashers. They resolve through their own impls.
#[allow(dead_code)]
#[derive(RonSchema)]
struct Resolved {
    size: usize,
    port: Port,
    name: Arc<String>,
    inner: config::Inner,
    by_name: HashMap<String, config::Inner, std::hash::RandomState>,
    path: std::path::PathBuf,
}

#[test]
fn field_kinds_come_from_the_field_types() {
    let inner = TypeKind::TypeRef(path::<config::Inner>().to_string());
    assert!(path::<config::Inner>().ends_with("::config::Inner"));
    assert_eq!(
        Resolved::type_kind(),
        TypeKind::Struct {
            fields: vec![
                Field::new("size", TypeKind::U64),
                Field::new("port", TypeKind::U16),
                Field::new("name", TypeKind::String),
                Field::new("inner", inner.clone()),
                Field::new(
                    "by_name",
                    TypeKind::Map {
                        key: Box::new(TypeKind::String),
                        value: Box::new(inner),
                    }
                ),
                Field::new("path", TypeKind::String),
            ],
        }
    );
    let catalog = collect_schemas::<Resolved>().expect("collect schemas");
    assert!(catalog.schemas.contains_key(path::<config::Inner>()));
}
