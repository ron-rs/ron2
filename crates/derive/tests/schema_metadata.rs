//! Schema metadata emitted by `#[derive(RonSchema)]`, checked against what
//! `FromRon` accepts.

#![allow(dead_code, non_camel_case_types)]

use std::rc::Rc;

use ron2::{
    FromRon, Ron, ToRon,
    ast::parse_document,
    fmt::FormatConfig,
    schema::{
        Field, NoResolver, RonSchema, Schema, SchemaResolver, Severity, TypeKind,
        collect::collect_schemas, validate, validate_all,
    },
};

/// Validate `input` against `T`'s schema, resolving `TypeRef`s through the
/// collected catalog, and check that `FromRon` agrees.
fn accepts<T: FromRon + RonSchema>(input: &str) -> bool {
    let catalog = collect_schemas::<T>().unwrap();
    let schema = catalog.resolve(T::type_path().unwrap()).unwrap();
    let doc = parse_document(input).unwrap();
    let valid = validate(doc.value.as_ref().unwrap(), &schema, &catalog).is_ok();
    assert_eq!(
        valid,
        T::from_ron(input).is_ok(),
        "validator and FromRon disagree on {input}"
    );
    valid
}

#[derive(Ron)]
struct Named {
    a: i32,
}

#[derive(Ron)]
#[ron(rename = "Other")]
struct Renamed(i32);

#[derive(Ron)]
struct Marker;

#[derive(Ron)]
#[ron(transparent)]
struct Transparent(i32);

#[derive(Ron)]
#[ron(deny_unknown_fields)]
enum Choice {
    V { a: i32 },
}

#[test]
fn schema_names_are_ron_names() {
    assert_eq!(Named::schema().name.as_deref(), Some("Named"));
    assert_eq!(Renamed::schema().name.as_deref(), Some("Other"));
    assert_eq!(Marker::schema().name.as_deref(), Some("Marker"));
    assert_eq!(Transparent::schema().name, None);
    assert_eq!(Choice::schema().name, None);

    assert!(accepts::<Renamed>("Other(1)"));
    assert!(!accepts::<Renamed>("Renamed(1)"));
    assert!(accepts::<Marker>("Marker"));
    assert!(!accepts::<Marker>("Wrong"));
}

#[derive(Ron)]
#[ron(deny_unknown_fields)]
struct Strict {
    a: i32,
}

#[test]
fn deny_unknown_fields_is_emitted() {
    assert!(Strict::schema().deny_unknown_fields);
    assert!(!Named::schema().deny_unknown_fields);
    assert!(Choice::schema().deny_unknown_fields);

    assert!(!accepts::<Strict>("(a: 1, b: 2)"));
    assert!(accepts::<Named>("(a: 1, b: 2)"));
    assert!(!accepts::<Choice>("V(a: 1, b: 2)"));
}

type MaybeInt = Option<i32>;

#[derive(Ron)]
struct Explicit {
    #[ron(explicit)]
    a: Option<i32>,
    #[ron(explicit, default)]
    b: MaybeInt,
    c: Option<i32>,
}

#[test]
fn explicit_fields_require_option_syntax() {
    let TypeKind::Struct { fields } = Explicit::type_kind() else {
        panic!("expected struct");
    };
    let explicit: Vec<_> = fields.iter().map(|f| f.explicit).collect();
    assert_eq!(explicit, [true, true, false]);

    assert!(accepts::<Explicit>("(a: Some(1), c: 1)"));
    assert!(accepts::<Explicit>("(a: None, b: Some(2), c: None)"));
    assert!(!accepts::<Explicit>("(a: 1, c: 1)"));
    assert!(!accepts::<Explicit>("(a: None, b: 2, c: 1)"));
}

#[test]
fn schema_files_without_explicit_still_load() {
    let field = Field::from_ron(r#"(name: "a", ty: Option(I32))"#).unwrap();
    assert!(!field.explicit);
    let written = Field::new("a", TypeKind::I32).to_ron().unwrap();
    assert!(!written.contains("explicit"), "{written}");
}

#[derive(Ron)]
struct Arrays {
    bytes: [u8; 3],
    nested: [[Named; 2]; 1],
}

#[test]
fn arrays_have_their_length() {
    let TypeKind::Struct { fields } = Arrays::type_kind() else {
        panic!("expected struct");
    };
    assert_eq!(
        fields[0].ty,
        TypeKind::Array {
            element: Box::new(TypeKind::U8),
            len: 3
        }
    );
    assert_eq!(
        fields[1].ty,
        TypeKind::Array {
            element: Box::new(TypeKind::Array {
                element: Box::new(TypeKind::TypeRef(Named::type_path().unwrap().into())),
                len: 2
            }),
            len: 1
        }
    );
    assert!(accepts::<Arrays>(
        "(bytes: [1, 2, 3], nested: [[(a: 1), (a: 2)]])"
    ));
    assert!(!accepts::<Arrays>(
        "(bytes: [1, 2], nested: [[(a: 1), (a: 2)]])"
    ));
}

#[derive(Ron)]
struct Gen<T> {
    t: T,
}

#[derive(Ron)]
struct Refs {
    shared: Rc<Named>,
    generic: Gen<u8>,
    nested: Gen<Gen<Named>>,
}

/// `Rc<T>` and generic instantiations resolve through the catalog.
#[test]
fn rc_and_generic_type_refs_resolve() {
    let catalog = collect_schemas::<Refs>().unwrap();
    for path in [
        Named::type_path(),
        Gen::<u8>::type_path(),
        Gen::<Gen<Named>>::type_path(),
        Gen::<Named>::type_path(),
    ] {
        assert!(catalog.resolve(path.unwrap()).is_some(), "{path:?}");
    }
    assert!(accepts::<Refs>(
        "(shared: (a: 1), generic: Gen(t: 2), nested: (t: (t: (a: 3))))"
    ));
    assert!(!accepts::<Refs>(
        "(shared: (a: 1), generic: (t: 300), nested: (t: (t: (a: 3))))"
    ));
    assert!(!accepts::<Refs>(
        "(shared: (a: \"x\"), generic: (t: 1), nested: (t: (t: (a: 3))))"
    ));
}

#[test]
fn unresolved_type_refs_are_reported_without_a_resolver() {
    let schema: Schema = Refs::schema();
    let doc =
        parse_document("(shared: (a: 1), generic: (t: 2), nested: (t: (t: (a: 3))))").unwrap();
    // Without a resolver the shape of TypeRef targets cannot be checked, but
    // validation must not fail on that alone: each unresolved path is a warning.
    let value = doc.value.as_ref().unwrap();
    validate(value, &schema, &NoResolver).unwrap();
    let issues: Vec<_> = validate_all(value, &schema, &NoResolver)
        .into_iter()
        .map(|issue| (issue.severity, issue.error.to_string()))
        .collect();
    assert_eq!(
        issues,
        [
            (
                Severity::Warning,
                "1:10-1:16: in field 'shared': No schema found for type \
                 `schema_metadata::Named`; value not checked"
                    .to_string()
            ),
            (
                Severity::Warning,
                "1:27-1:33: in field 'generic': No schema found for type \
                 `schema_metadata::Gen<u8>`; value not checked"
                    .to_string()
            ),
            (
                Severity::Warning,
                "1:43-1:59: in field 'nested': No schema found for type \
                 `schema_metadata::Gen<schema_metadata::Gen<schema_metadata::Named>>`; \
                 value not checked"
                    .to_string()
            ),
        ]
    );
}

/// Names the formatter must write as raw identifiers.
#[derive(Debug, Clone, Copy, Ron, PartialEq)]
enum Special {
    NaN,
    inf,
    None,
    r#true,
    Plain,
}

#[derive(Debug, Ron, PartialEq)]
struct Typed {
    r#type: u32,
    special: Special,
}

#[test]
fn raw_identifiers_round_trip() {
    for special in [
        Special::NaN,
        Special::inf,
        Special::None,
        Special::r#true,
        Special::Plain,
    ] {
        for config in [FormatConfig::default(), FormatConfig::minimal()] {
            let value = Typed { r#type: 1, special };
            let ron = value.to_ron_with(&config).unwrap();
            assert_eq!(Typed::from_ron(&ron).unwrap(), value, "{ron}");
        }
    }
    let ron = Special::r#true.to_ron().unwrap();
    assert_eq!(ron.trim(), "r#true");
    let ron = Typed {
        r#type: 1,
        special: Special::NaN,
    }
    .to_ron_with(&FormatConfig::minimal())
    .unwrap();
    assert_eq!(ron, "Typed(type:1,special:r#NaN)");
}
