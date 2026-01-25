//! Tests for generic type handling in schema collection and writing.

#![cfg(feature = "derive")]

use ron2::{
    RonSchema,
    schema::{
        RonSchema as RonSchemaTrait, TypeKind, collect::collect_schemas, read_schema,
        storage::type_path_to_file_path, write_schemas,
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

#[test]
fn test_collect_schemas_for_generic_container() {
    let catalog = collect_schemas::<GenericContainer<u32>>().expect("collect schemas");
    let container_path = <GenericContainer<u32> as RonSchemaTrait>::type_path().unwrap();
    let child_path = Child::type_path().unwrap();

    assert_eq!(catalog.schemas.len(), 2);
    assert!(catalog.schemas.contains_key(container_path));
    assert!(catalog.schemas.contains_key(child_path));
}

#[test]
fn test_write_schemas_for_generic_container() {
    let temp_dir = tempfile::tempdir().expect("temp dir");
    let output_dir = temp_dir.path().to_str().expect("utf-8 temp dir");
    let written = write_schemas::<GenericContainer<u32>>(Some(output_dir)).expect("write schemas");

    let container_path = <GenericContainer<u32> as RonSchemaTrait>::type_path().unwrap();
    let child_path = Child::type_path().unwrap();
    let container_rel = type_path_to_file_path(container_path);
    let child_rel = type_path_to_file_path(child_path);

    assert_eq!(written.len(), 2);
    assert!(written.iter().any(|p| p.ends_with(&container_rel)));
    assert!(written.iter().any(|p| p.ends_with(&child_rel)));

    let container_schema = read_schema(&temp_dir.path().join(container_rel)).expect("read schema");
    match &container_schema.kind {
        TypeKind::Struct { fields } => {
            assert_eq!(fields.len(), 2);
            let item = fields.iter().find(|f| f.name == "item").unwrap();
            assert!(matches!(item.ty, TypeKind::TypeRef(_)));
            let child = fields.iter().find(|f| f.name == "child").unwrap();
            assert!(matches!(child.ty, TypeKind::TypeRef(_)));
        }
        _ => panic!("Expected struct schema"),
    }

    let child_schema = read_schema(&temp_dir.path().join(child_rel)).expect("read schema");
    match &child_schema.kind {
        TypeKind::Struct { fields } => {
            assert_eq!(fields.len(), 1);
            assert_eq!(fields[0].name, "value");
            assert_eq!(fields[0].ty, TypeKind::U32);
        }
        _ => panic!("Expected child struct schema"),
    }
}
