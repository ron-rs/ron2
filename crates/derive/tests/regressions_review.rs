//! Regression tests for the second review of the derive rewrite.

#![allow(dead_code)]

use std::collections::{BTreeMap, HashMap};

use ron2::{
    ErrorKind, FromRon, ToRon,
    fmt::FormatConfig,
    schema::{Field, RonSchema, TypeKind},
};
use ron2_derive::Ron;

fn minimal<T: ToRon>(value: &T) -> String {
    value.to_ron_with(&FormatConfig::minimal()).unwrap()
}

fn roundtrip<T: ToRon + FromRon + PartialEq + std::fmt::Debug>(value: &T) -> String {
    let ron = minimal(value);
    let parsed = T::from_ron(&ron).unwrap_or_else(|e| panic!("{e} while parsing {ron}"));
    assert_eq!(&parsed, value, "roundtrip of {ron}");
    ron
}

fn struct_fields<T: RonSchema>() -> Vec<Field> {
    match T::type_kind() {
        TypeKind::Struct { fields } => fields,
        other => panic!("expected struct kind, got {other:?}"),
    }
}

fn five() -> u32 {
    5
}

/// Deeply nested generic instantiations register each schema once.
mod generic_registration {
    use super::*;

    #[derive(Ron)]
    struct G<T> {
        a: T,
        b: Vec<T>,
        c: Option<T>,
    }

    #[test]
    fn nested_generics_register_once_each() {
        type Deep = G<G<G<G<G<G<G<G<u8>>>>>>>>;
        let catalog = ron2::schema::collect::collect_schemas::<Deep>().unwrap();
        assert_eq!(catalog.schemas.len(), 8);
    }
}

/// A flattened field whose type uses a parameter needs the `*Fields`
/// traits on the whole field type, not `T: FromRon`.
mod generic_flatten {
    use super::*;

    #[derive(Debug, Ron, PartialEq)]
    struct Extra {
        e: u32,
    }

    #[derive(Debug, Ron, PartialEq)]
    struct Outer<T> {
        name: String,
        #[ron(flatten)]
        extra: Option<T>,
    }

    #[test]
    fn roundtrip_generic_flatten() {
        roundtrip(&Outer {
            name: "n".into(),
            extra: Some(Extra { e: 1 }),
        });
        roundtrip(&Outer::<Extra> {
            name: "n".into(),
            extra: None,
        });
    }
}

/// Associated types are bounded themselves, not their parameter.
mod associated_types {
    use super::*;

    pub trait Family {
        type Item;
    }

    /// Implements none of the ron traits: only its `Item` does.
    #[derive(Debug, PartialEq)]
    pub struct Strings;

    impl Family for Strings {
        type Item = String;
    }

    #[derive(Debug, Ron, PartialEq)]
    struct Assoc<T: Family> {
        item: T::Item,
        items: Vec<<T as Family>::Item>,
    }

    #[derive(Debug, Ron, PartialEq)]
    #[ron(bound(
        ser = "T: ron2::ToRon",
        de = "T: ron2::FromRon",
        schema = "T: ron2::schema::RonSchema"
    ))]
    struct PerDerive<T> {
        x: Box<T>,
    }

    #[test]
    fn assoc_roundtrip() {
        roundtrip(&Assoc::<Strings> {
            item: "a".into(),
            items: vec!["b".into()],
        });
        assert_eq!(struct_fields::<Assoc<Strings>>()[0].ty, TypeKind::String);
        roundtrip(&PerDerive { x: Box::new(1u8) });
    }
}

/// `#[ron(explicit)]` works through type aliases (it is checked by type).
mod explicit_alias {
    use super::*;

    type MaybeInt = Option<i32>;

    #[derive(Debug, Ron, PartialEq)]
    struct S {
        #[ron(explicit)]
        x: MaybeInt,
    }

    #[test]
    fn alias() {
        assert_eq!(S::from_ron("(x: Some(1))").unwrap(), S { x: Some(1) });
        assert!(S::from_ron("(x: 1)").is_err());
    }
}

/// `#[ron(skip)]` on tuple fields, e.g. a transparent wrapper with a marker.
mod tuple_skip {
    use std::marker::PhantomData;

    use super::*;

    #[derive(Debug, Ron, PartialEq)]
    #[ron(transparent)]
    struct Id<T, U>(T, #[ron(skip)] PhantomData<U>);

    #[derive(Debug, Ron, PartialEq)]
    struct Pair(u8, #[ron(skip, default = "five")] u32, String);

    #[derive(Debug, Ron, PartialEq)]
    enum E {
        V(#[ron(skip)] u8, u16),
    }

    #[test]
    fn skipped_tuple_fields() {
        let ron = roundtrip(&Id::<u8, String>(3, PhantomData));
        assert_eq!(ron, "3");

        assert_eq!(
            Pair::from_ron("(1, \"s\")").unwrap(),
            Pair(1, 5, "s".into())
        );
        assert_eq!(minimal(&Pair(1, 7, "s".into())), "Pair(1,\"s\")");
        assert_eq!(
            Pair::type_kind(),
            TypeKind::Tuple(vec![TypeKind::U8, TypeKind::String])
        );

        assert_eq!(E::from_ron("V(2)").unwrap(), E::V(0, 2));
        assert_eq!(minimal(&E::V(9, 2)), "V(2)");
    }
}

/// A struct variant whose fields all have defaults accepts `V`, `V()` and
/// `V(..)`, like a unit variant.
mod defaulted_variant {
    use super::*;

    #[derive(Debug, Ron, PartialEq)]
    enum E {
        V {
            #[ron(default)]
            x: u32,
        },
    }

    #[test]
    fn bare_name() {
        for input in ["V", "V()", "V(x: 0)"] {
            assert_eq!(E::from_ron(input).unwrap(), E::V { x: 0 }, "{input}");
        }
    }
}

/// Catch-all maps can be flattened in any position; named fields keep their
/// keys and are written first.
mod catch_all_maps {
    use super::*;

    #[derive(Debug, Ron, PartialEq)]
    struct MapFirst {
        #[ron(flatten)]
        rest: HashMap<String, i32>,
        name: String,
    }

    #[derive(Debug, Ron, PartialEq, Default)]
    struct Inner {
        a: i32,
    }

    #[derive(Debug, Ron, PartialEq)]
    struct MapBeforeStruct {
        #[ron(flatten)]
        rest: BTreeMap<String, i32>,
        #[ron(flatten)]
        inner: Inner,
    }

    #[derive(Debug, Ron, PartialEq)]
    struct Nested {
        #[ron(flatten)]
        tail: MapFirst,
        id: u8,
    }

    #[test]
    fn named_fields_win_regardless_of_order() {
        let value = MapFirst::from_ron("(x: 1, name: \"n\")").unwrap();
        assert_eq!(value.name, "n");
        assert_eq!(value.rest, HashMap::from_iter([("x".to_string(), 1)]));
        let ron = roundtrip(&value);
        assert!(
            ron.starts_with("MapFirst(name:"),
            "catch-all written last: {ron}"
        );

        let value = MapBeforeStruct::from_ron("(b: 2, a: 1)").unwrap();
        assert_eq!(value.inner, Inner { a: 1 });
        assert_eq!(value.rest, BTreeMap::from_iter([("b".to_string(), 2)]));
        roundtrip(&value);

        // A struct containing a catch-all is itself read after named fields.
        let value = Nested::from_ron("(name: \"n\", id: 1, x: 2)").unwrap();
        assert_eq!(value.id, 1);
        assert_eq!(value.tail.rest, HashMap::from_iter([("x".to_string(), 2)]));
    }

    // `IndexMap` has no `RonSchema` impl (yet), so no `Ron` derive.
    #[derive(Debug, ron2_derive::FromRon, ron2_derive::ToRon, PartialEq)]
    struct Indexed {
        #[ron(flatten)]
        rest: indexmap::IndexMap<String, i32>,
    }

    #[test]
    fn index_maps_flatten() {
        roundtrip(&Indexed {
            rest: indexmap::IndexMap::from_iter([("z".to_string(), 1), ("a".to_string(), 2)]),
        });
    }
}

/// Flattened `Option<Inner>`: `None` without any of Inner's fields, `Some`
/// when they are all there, an error when only some are.
mod flatten_option_presence {
    use super::*;

    #[derive(Debug, Ron, PartialEq)]
    struct Inner {
        a: i32,
        b: i32,
    }

    #[derive(Debug, Ron, PartialEq)]
    struct Outer {
        name: i32,
        #[ron(flatten)]
        inner: Option<Inner>,
    }

    #[test]
    fn presence_rule() {
        assert_eq!(Outer::from_ron("(name: 1)").unwrap().inner, None);
        assert_eq!(
            Outer::from_ron("(name: 1, a: 2, b: 3)").unwrap().inner,
            Some(Inner { a: 2, b: 3 })
        );
        let err = Outer::from_ron("(name: 1, b: 5)").unwrap_err();
        assert!(
            matches!(err.kind(), ErrorKind::MissingField { field, .. } if field == "a"),
            "{err}"
        );
    }
}

/// Raw identifiers appear without `r#` in runtime messages.
mod raw_names_in_messages {
    use super::*;

    #[derive(Debug, Ron, PartialEq)]
    #[allow(non_camel_case_types)]
    enum r#enum {
        A,
        #[ron(skip)]
        r#Hidden,
    }

    #[test]
    fn messages() {
        let err = r#enum::Hidden.to_ron().unwrap_err().to_string();
        assert!(err.contains("enum::Hidden") && !err.contains("r#"), "{err}");
        let err = r#enum::from_ron("1").unwrap_err().to_string();
        assert!(!err.contains("r#"), "{err}");
    }
}

/// Fields that `FromRon` never reads are not part of the schema.
mod skip_deserializing_schema {
    use super::*;

    #[derive(Debug, Ron, PartialEq)]
    #[ron(deny_unknown_fields)]
    struct SD {
        a: i32,
        #[ron(skip_deserializing)]
        b: i32,
    }

    #[test]
    fn not_required() {
        let names: Vec<_> = struct_fields::<SD>().into_iter().map(|f| f.name).collect();
        assert_eq!(names, ["a"]);
        let doc = ron2::ast::parse_document("(a: 1)").unwrap();
        let value = doc.value.as_ref().unwrap();
        ron2::schema::validate(value, &SD::schema(), &ron2::schema::NoResolver).unwrap();
        assert!(SD::from_ron("(a: 1)").is_ok());
        assert!(SD::from_ron("(a: 1, b: 2)").is_err());
    }
}
