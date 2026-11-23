//! Regression tests for derive bugs found in the audit.
//!
//! Compile-time rejections are covered by `tests/ui` (see `compile_fail.rs`).

#![allow(dead_code)]

use std::{collections::HashMap, sync::Arc};

use ron2::{
    Error, ErrorKind, FromRon, ToRon,
    fmt::FormatConfig,
    schema::{Field, RonSchema, TypeKind, Variant},
};
use ron2_derive::Ron;

fn roundtrip<T: ToRon + FromRon + PartialEq + std::fmt::Debug>(value: &T) -> String {
    let ron = value.to_ron_with(&FormatConfig::minimal()).unwrap();
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

fn enum_variants<T: RonSchema>() -> Vec<Variant> {
    match T::type_kind() {
        TypeKind::Enum { variants } => variants,
        other => panic!("expected enum kind, got {other:?}"),
    }
}

/// Field types the old derive guessed from syntax and then failed to compile
/// (`<usize>::__RON_SCHEMA_ENTRY` etc.).
mod field_types_compile {
    use super::*;

    type Port = u16;

    #[derive(Debug, Ron, PartialEq)]
    struct Fields {
        count: usize,
        offset: isize,
        port: Port,
        name: Arc<String>,
        by_name: HashMap<String, u32, std::hash::RandomState>,
    }

    #[test]
    fn roundtrip_and_schema() {
        let value = Fields {
            count: 3,
            offset: -1,
            port: 80,
            name: Arc::new("n".into()),
            by_name: HashMap::from_iter([("a".to_string(), 1)]),
        };
        roundtrip(&value);
        let kinds: Vec<_> = struct_fields::<Fields>()
            .into_iter()
            .map(|f| f.ty)
            .collect();
        assert_eq!(kinds[0], TypeKind::U64);
        assert_eq!(kinds[1], TypeKind::I64);
        assert_eq!(kinds[2], TypeKind::U16);
        assert_eq!(kinds[3], TypeKind::String);
    }
}

mod generics {
    use std::marker::PhantomData;

    use super::*;

    #[derive(Debug, Ron, PartialEq)]
    struct Wrapper<T> {
        x: T,
        list: Vec<T>,
    }

    #[derive(Debug, Ron, PartialEq)]
    enum Either<L, R> {
        Left(L),
        Right { value: R },
    }

    /// No bound on `T`: it only appears in `PhantomData`.
    struct NotRon;

    #[derive(Ron)]
    struct Tagged<T> {
        id: u32,
        #[ron(skip)]
        marker: PhantomData<T>,
    }

    #[derive(Debug, Ron, PartialEq)]
    #[ron(bound = "T: ron2::FromRon + ron2::ToRon + ron2::schema::RonSchema")]
    struct Explicit<T> {
        x: Box<T>,
    }

    #[test]
    fn generic_struct_roundtrip() {
        roundtrip(&Wrapper {
            x: 1u8,
            list: vec![2, 3],
        });
        roundtrip(&Wrapper {
            x: "a".to_string(),
            list: vec![],
        });
    }

    #[test]
    fn generic_enum_roundtrip() {
        roundtrip(&Either::<u8, String>::Left(1));
        roundtrip(&Either::<u8, String>::Right { value: "r".into() });
    }

    #[test]
    fn phantom_param_needs_no_bound() {
        let value = Tagged::<NotRon> {
            id: 7,
            marker: PhantomData,
        };
        let ron = value.to_ron().unwrap();
        assert!(ron.contains("id: 7") && !ron.contains("marker"), "{ron}");
        assert_eq!(Tagged::<NotRon>::from_ron("(id: 7)").unwrap().id, 7);
        assert_eq!(struct_fields::<Tagged<NotRon>>().len(), 1);
    }

    #[test]
    fn bound_attribute_replaces_inference() {
        roundtrip(&Explicit { x: Box::new(5u32) });
    }
}

/// `#[ron(flatten)]` inside a struct variant was dropped on serialization.
mod flatten_in_variant {
    use super::*;

    #[derive(Debug, Ron, PartialEq, Default)]
    struct Inner {
        a: u32,
    }

    #[derive(Debug, Ron, PartialEq)]
    enum E {
        V {
            name: String,
            #[ron(flatten)]
            inner: Inner,
        },
    }

    #[test]
    fn roundtrip_flattened_variant() {
        let ron = roundtrip(&E::V {
            name: "n".into(),
            inner: Inner { a: 1 },
        });
        assert!(ron.contains("a:1") && !ron.contains("inner"), "{ron}");
    }

    #[derive(Debug, Ron, PartialEq)]
    struct WithOptional {
        #[ron(flatten)]
        inner: Option<Inner>,
        #[ron(flatten)]
        extra: HashMap<String, u32>,
    }

    #[test]
    fn flatten_option_and_map() {
        let ron = roundtrip(&WithOptional {
            inner: Some(Inner { a: 2 }),
            extra: HashMap::from_iter([("b".to_string(), 3)]),
        });
        assert!(ron.contains("a:2") && ron.contains("b:3"), "{ron}");
        roundtrip(&WithOptional {
            inner: None,
            extra: HashMap::new(),
        });
    }

    #[derive(Debug, Ron, PartialEq)]
    struct Settings {
        name: String,
        #[ron(flatten)]
        inner: Inner,
        #[ron(flatten)]
        extension_settings: HashMap<String, String>,
    }

    /// A flattened map collects the remaining fields, so it is never a
    /// required field; a flattened struct without default is.
    #[test]
    fn flattened_map_is_optional_in_schema() {
        let fields = struct_fields::<Settings>();
        let flags: Vec<_> = fields
            .iter()
            .map(|f| (f.name.as_str(), f.optional, f.flattened))
            .collect();
        assert_eq!(
            flags,
            [
                ("name", false, false),
                ("inner", false, true),
                ("extension_settings", true, true)
            ]
        );
        assert!(
            struct_fields::<WithOptional>()
                .iter()
                .all(|f| f.optional && f.flattened)
        );

        let schema = Extensible::schema();
        for input in ["(name: \"n\", custom: \"x\")", "(name: \"n\")"] {
            let doc = ron2::ast::parse_document(input).unwrap();
            let value = doc.value.as_ref().unwrap();
            ron2::schema::validate(value, &schema, &ron2::schema::NoResolver).unwrap();
        }
    }

    #[derive(Debug, Ron, PartialEq)]
    struct Extensible {
        name: String,
        #[ron(flatten)]
        extension_settings: HashMap<String, String>,
    }
}

/// User items named like std items or like the generated locals must not
/// change what the generated code refers to.
mod hygiene {
    #![allow(unused_macros)]

    use ron2::{FromRon, ToRon};
    use ron2_derive::Ron;

    struct Box;
    struct Vec;
    struct String;
    struct Cow;
    enum Option {
        Some,
        None,
    }
    enum Result {
        Ok,
        Err,
    }
    macro_rules! vec {
        ($($t:tt)*) => {
            compile_error!("user vec! called")
        };
    }
    macro_rules! concat {
        ($($t:tt)*) => {
            compile_error!("user concat! called")
        };
    }

    #[derive(Debug, Ron, PartialEq)]
    struct Shadowed {
        access: u32,
        expr: u32,
        fields: u32,
        value: u32,
        elements: u32,
        names: u32,
        catalog: u32,
        #[ron(opt)]
        opt: u32,
        #[ron(explicit, default)]
        explicit: ::core::option::Option<u32>,
    }

    #[derive(Debug, Ron, PartialEq)]
    #[ron(deny_unknown_fields)]
    enum Choice {
        Unit,
        Tuple(u32, u32),
        Struct { access: u32, s: u32 },
        Some(u32),
        None,
    }

    #[derive(Debug, Ron, PartialEq)]
    struct Tuple(u32, ::std::string::String);

    #[test]
    fn shadowed_names_roundtrip() {
        let value = Shadowed {
            access: 1,
            expr: 2,
            fields: 3,
            value: 4,
            elements: 5,
            names: 6,
            catalog: 7,
            opt: 0,
            explicit: ::core::option::Option::Some(8),
        };
        assert_eq!(Shadowed::from_ron(&value.to_ron().unwrap()).unwrap(), value);
        for choice in [
            Choice::Unit,
            Choice::Tuple(1, 2),
            Choice::Struct { access: 3, s: 4 },
            Choice::Some(5),
            Choice::None,
        ] {
            assert_eq!(
                Choice::from_ron(
                    &choice
                        .to_ron_with(&ron2::fmt::FormatConfig::minimal())
                        .unwrap()
                )
                .unwrap(),
                choice
            );
        }
        let tuple = Tuple(1, "t".into());
        assert_eq!(Tuple::from_ron(&tuple.to_ron().unwrap()).unwrap(), tuple);
    }

    /// A user type named `Vec` is a type of its own, not a list.
    mod user_types {
        #[derive(ron2_derive::RonSchema)]
        pub struct Vec {
            pub len: u32,
        }

        #[derive(ron2_derive::RonSchema)]
        pub struct Holder {
            pub v: Vec,
        }
    }

    #[test]
    fn user_type_named_vec_is_a_type_ref() {
        use ron2::schema::{RonSchema, TypeKind};
        let TypeKind::Struct { fields } = user_types::Holder::type_kind() else {
            panic!("expected struct");
        };
        assert_eq!(
            fields[0].ty,
            TypeKind::TypeRef(user_types::Vec::type_path().unwrap_or_default().into())
        );
        assert!(
            user_types::Vec::type_path()
                .unwrap_or_default()
                .ends_with("user_types::Vec")
        );
    }
}

/// Raw identifiers use their plain name in RON.
mod raw_identifiers {
    use super::*;

    #[derive(Debug, Ron, PartialEq)]
    struct Raw {
        r#type: u32,
    }

    #[derive(Debug, Ron, PartialEq)]
    #[allow(non_camel_case_types)]
    enum RawEnum {
        r#match { r#in: u32 },
    }

    #[test]
    fn plain_names() {
        let ron = roundtrip(&Raw { r#type: 1 });
        assert!(ron.contains("type:1") && !ron.contains("r#"), "{ron}");
        assert_eq!(Raw::from_ron("(type: 1)").unwrap(), Raw { r#type: 1 });
        assert_eq!(struct_fields::<Raw>()[0].name, "type");

        let ron = roundtrip(&RawEnum::r#match { r#in: 2 });
        assert_eq!(ron, "match(in:2)");
    }
}

fn five() -> u32 {
    5
}

/// Values for missing or skipped fields come from `default = "path"`.
mod default_paths {
    use super::*;

    /// No `Default` impl: `V()` must not need one.
    #[derive(Debug, PartialEq)]
    struct NoDefault(u32);

    impl ToRon for NoDefault {
        fn to_ast(&self) -> ron2::Result<ron2::ast::Expr<'static>> {
            self.0.to_ast()
        }
    }

    impl RonSchema for NoDefault {
        fn type_kind() -> TypeKind {
            TypeKind::U32
        }
    }

    impl FromRon for NoDefault {
        fn from_ast(expr: &ron2::ast::Expr<'_>) -> ron2::Result<Self> {
            u32::from_ast(expr).map(Self)
        }
    }

    fn no_default() -> NoDefault {
        NoDefault(9)
    }

    #[derive(Debug, Ron, PartialEq)]
    enum E {
        V {
            #[ron(default = "five")]
            x: u32,
            #[ron(default = "no_default")]
            y: NoDefault,
        },
    }

    #[derive(Debug, Ron, PartialEq)]
    struct Skipped {
        name: String,
        #[ron(skip_deserializing, default = "five")]
        computed: u32,
        #[ron(skip, default = "five")]
        hidden: u32,
    }

    #[test]
    fn empty_variant_uses_default_paths() {
        assert_eq!(
            E::from_ron("V()").unwrap(),
            E::V {
                x: 5,
                y: NoDefault(9)
            }
        );
    }

    #[test]
    fn skipped_fields_use_default_paths() {
        let value = Skipped::from_ron("(name: \"n\")").unwrap();
        assert_eq!((value.computed, value.hidden), (5, 5));
    }
}

/// A flattened field is not a defaulted field: `()` must not skip its
/// required fields, and must not require `Inner: Default`.
mod flatten_is_not_default {
    use super::*;

    #[derive(Debug, Ron, PartialEq)]
    struct Inner {
        required: u32,
    }

    #[derive(Debug, Ron, PartialEq)]
    struct Outer {
        #[ron(default)]
        x: u32,
        #[ron(flatten)]
        inner: Inner,
    }

    #[derive(Debug, Ron, PartialEq, Default)]
    struct Defaulted {
        #[ron(default)]
        a: u32,
    }

    #[derive(Debug, Ron, PartialEq)]
    struct OuterDefault {
        #[ron(flatten, default)]
        inner: Defaulted,
    }

    #[test]
    fn unit_is_rejected_when_flattened_fields_are_required() {
        assert!(Outer::from_ron("()").is_err());
        assert!(Outer::from_ron("Outer()").is_err());
        assert_eq!(
            Outer::from_ron("(required: 1)").unwrap(),
            Outer {
                x: 0,
                inner: Inner { required: 1 }
            }
        );
    }

    #[test]
    fn flatten_with_default_accepts_unit() {
        assert_eq!(
            OuterDefault::from_ron("()").unwrap(),
            OuterDefault {
                inner: Defaulted { a: 0 }
            }
        );
    }
}

/// `#[ron(opt, explicit)]`: `None` is omitted on output and missing on input.
mod opt_explicit {
    use super::*;

    #[derive(Debug, Ron, PartialEq)]
    struct S {
        #[ron(opt, explicit)]
        x: Option<u32>,
    }

    #[test]
    fn roundtrip_none_and_some() {
        let ron = roundtrip(&S { x: None });
        assert!(!ron.contains('x'), "{ron}");
        let ron = roundtrip(&S { x: Some(1) });
        assert!(ron.contains("Some(1)"), "{ron}");
        assert!(S::from_ron("(x: 1)").is_err());
    }
}

/// Skipped variants are absent from schema, deserialization and the bounds;
/// serializing one is a runtime error (like serde).
mod skipped_variants {
    use super::*;

    struct NotRon;

    #[derive(Ron)]
    enum E {
        A,
        #[ron(skip)]
        Internal(NotRon),
    }

    #[test]
    fn skipped_variant() {
        let names: Vec<_> = enum_variants::<E>().into_iter().map(|v| v.name).collect();
        assert_eq!(names, ["A"]);

        let err = E::Internal(NotRon).to_ron().unwrap_err();
        assert!(err.to_string().contains("E::Internal"), "{err}");

        let err = E::from_ron("Internal").err().unwrap();
        assert!(
            matches!(err.kind(), ErrorKind::UnknownVariant { .. }),
            "{err}"
        );
        assert_eq!(E::A.to_ron().unwrap(), "A");
    }
}

/// `rename_all` on an enum renames variants; fields of a struct variant are
/// renamed only by a `rename_all` on that variant.
mod rename_all_scopes {
    use super::*;

    #[derive(Debug, Ron, PartialEq)]
    #[ron(rename_all = "snake_case")]
    enum Server {
        HTTPServer {
            max_conns: u32,
        },
        #[ron(rename_all = "camelCase")]
        PlainServer {
            max_conns: u32,
        },
    }

    #[test]
    fn scopes() {
        let ron = roundtrip(&Server::HTTPServer { max_conns: 1 });
        assert_eq!(ron, "http_server(max_conns:1)");
        let ron = roundtrip(&Server::PlainServer { max_conns: 2 });
        assert_eq!(ron, "plain_server(maxConns:2)");
    }
}

/// The unknown-field error lists flattened fields too.
mod deny_unknown_with_flatten {
    use super::*;

    #[derive(Debug, Ron, PartialEq)]
    struct Inner {
        inner_field: u32,
    }

    #[derive(Debug, Ron, PartialEq)]
    #[ron(deny_unknown_fields)]
    struct Outer {
        outer_field: u32,
        #[ron(flatten)]
        inner: Inner,
    }

    #[derive(Debug, Ron, PartialEq)]
    #[ron(deny_unknown_fields)]
    enum E {
        V {
            own: u32,
            #[ron(flatten)]
            inner: Inner,
        },
    }

    fn expected(err: &Error) -> Vec<&'static str> {
        match err.kind() {
            ErrorKind::UnknownField { expected, .. } => expected.to_vec(),
            _ => panic!("expected UnknownField, got {err}"),
        }
    }

    #[test]
    fn expected_list_includes_flattened_fields() {
        let err = Outer::from_ron("(outer_field: 1, inner_field: 2, bogus: 3)").unwrap_err();
        assert_eq!(expected(&err), ["outer_field", "inner_field"]);

        let err = E::from_ron("V(own: 1, inner_field: 2, bogus: 3)").unwrap_err();
        assert_eq!(expected(&err), ["own", "inner_field"]);

        assert_eq!(
            Outer::from_ron("(outer_field: 1, inner_field: 2)").unwrap(),
            Outer {
                outer_field: 1,
                inner: Inner { inner_field: 2 }
            }
        );
    }
}
