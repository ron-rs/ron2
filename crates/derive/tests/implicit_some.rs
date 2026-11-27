//! `#[ron(implicit_some)]`: `Some(x)` of an `Option` field is written as bare
//! `x`, except where `x` would read back as an option.

#![allow(dead_code)]

use std::fmt::Debug;

use ron2::{
    FromRon, NamedContent, ToRon, Value,
    ast::parse_document,
    fmt::FormatConfig,
    schema::{RonSchema, SchemaResolver, collect::collect_schemas, validate},
};
use ron2_derive::Ron;

/// Write `value` minimally, check the text and that it reads back equal.
#[track_caller]
fn round_trip<T: ToRon + FromRon + PartialEq + Debug>(value: &T, expected: &str) {
    let text = value.to_ron_with(&FormatConfig::minimal()).unwrap();
    assert_eq!(text, expected);
    assert_eq!(&T::from_ron(&text).unwrap(), value, "reading back {text}");
}

#[derive(Debug, Ron, PartialEq, Default)]
struct Plain {
    #[ron(implicit_some)]
    size: Option<u32>,
    #[ron(implicit_some)]
    name: Option<String>,
    #[ron(implicit_some)]
    flag: Option<bool>,
    #[ron(implicit_some)]
    list: Option<Vec<i8>>,
    // Without the attribute: still `Some(..)`.
    other: Option<u32>,
}

#[test]
fn plain_types_are_written_bare() {
    round_trip(
        &Plain {
            size: Some(12),
            name: Some("Arial".into()),
            flag: Some(false),
            list: Some(vec![1, -2]),
            other: Some(3),
        },
        r#"Plain(size:12,name:"Arial",flag:false,list:[1,-2],other:Some(3))"#,
    );
}

#[test]
fn none_is_still_written() {
    round_trip(
        &Plain::default(),
        "Plain(size:None,name:None,flag:None,list:None,other:None)",
    );
}

#[test]
fn pretty_output() {
    let value = Plain {
        size: Some(12),
        ..Plain::default()
    };
    assert_eq!(
        value.to_ron().unwrap(),
        "Plain(\n    size: 12,\n    name: None,\n    flag: None,\n    list: None,\n    other: None,\n)"
    );
}

#[test]
fn strings_that_look_like_options() {
    for s in ["None", "Some(1)", ""] {
        round_trip(
            &Plain {
                name: Some(s.into()),
                ..Plain::default()
            },
            &format!(
                "Plain(size:None,name:{:?},flag:None,list:None,other:None)",
                s
            ),
        );
    }
}

#[derive(Debug, Ron, PartialEq, Clone)]
enum Shape {
    Circle,
    Square(u32),
    Rect { w: u32, h: u32 },
}

#[derive(Debug, Ron, PartialEq)]
struct WithEnum {
    #[ron(implicit_some)]
    shape: Option<Shape>,
}

#[test]
fn enums_are_written_bare() {
    for (shape, text) in [
        (Shape::Circle, "WithEnum(shape:Circle)"),
        (Shape::Square(2), "WithEnum(shape:Square(2))"),
        (Shape::Rect { w: 1, h: 2 }, "WithEnum(shape:Rect(w:1,h:2))"),
    ] {
        round_trip(&WithEnum { shape: Some(shape) }, text);
    }
    round_trip(&WithEnum { shape: None }, "WithEnum(shape:None)");
}

#[derive(Debug, Ron, PartialEq)]
struct Nested {
    #[ron(implicit_some)]
    value: Option<Option<bool>>,
}

#[test]
fn nested_option_keeps_the_wrapper() {
    // The inner `Option` is an option expression, so the outer `Some` stays.
    round_trip(
        &Nested {
            value: Some(Some(true)),
        },
        "Nested(value:Some(Some(true)))",
    );
    round_trip(&Nested { value: Some(None) }, "Nested(value:Some(None))");
    round_trip(&Nested { value: None }, "Nested(value:None)");
}

#[derive(Debug, ron2_derive::FromRon, ron2_derive::ToRon, PartialEq)]
struct Dynamic {
    #[ron(implicit_some)]
    value: Option<Value>,
}

fn named(name: &str, content: NamedContent) -> Value {
    Value::Named {
        name: name.into(),
        content,
    }
}

#[test]
fn values_that_read_as_options_keep_the_wrapper() {
    let cases = [
        (Value::Option(None), "Dynamic(value:Some(None))"),
        (
            Value::Option(Some(Box::new(Value::from(1)))),
            "Dynamic(value:Some(Some(1)))",
        ),
        (
            named("None", NamedContent::Unit),
            "Dynamic(value:Some(r#None))",
        ),
        (
            named("Some", NamedContent::Tuple(vec![Value::from(1)])),
            "Dynamic(value:Some(r#Some(1)))",
        ),
        (
            named(
                "None",
                NamedContent::Struct(vec![("x".into(), Value::from(1))]),
            ),
            "Dynamic(value:Some(r#None(x:1)))",
        ),
    ];
    for (value, text) in cases {
        round_trip(&Dynamic { value: Some(value) }, text);
    }
}

#[test]
fn other_values_are_written_bare() {
    let cases = [
        (Value::from(1), "Dynamic(value:1)"),
        (Value::Unit, "Dynamic(value:())"),
        (named("Other", NamedContent::Unit), "Dynamic(value:Other)"),
        (
            named("Nonee", NamedContent::Tuple(vec![Value::from(1)])),
            "Dynamic(value:Nonee(1))",
        ),
    ];
    for (value, text) in cases {
        round_trip(&Dynamic { value: Some(value) }, text);
    }
}

/// An enum with variants named like the option constructors: written as
/// `r#None` / `r#Some(..)`, so they keep the wrapper.
#[derive(Debug, Ron, PartialEq)]
enum Maybe {
    None,
    Some(u8),
    Other,
}

#[derive(Debug, Ron, PartialEq)]
struct WithMaybe {
    #[ron(implicit_some)]
    value: Option<Maybe>,
}

#[test]
fn variants_named_none_or_some_keep_the_wrapper() {
    round_trip(
        &WithMaybe {
            value: Some(Maybe::None),
        },
        "WithMaybe(value:Some(r#None))",
    );
    round_trip(
        &WithMaybe {
            value: Some(Maybe::Some(1)),
        },
        "WithMaybe(value:Some(r#Some(1)))",
    );
    round_trip(
        &WithMaybe {
            value: Some(Maybe::Other),
        },
        "WithMaybe(value:Other)",
    );
    round_trip(&WithMaybe { value: None }, "WithMaybe(value:None)");
}

/// The typical migration: a field that was `u32` and became `Option<u32>`.
#[derive(Debug, Ron, PartialEq, Default)]
struct Font {
    family: String,
    #[ron(opt, implicit_some)]
    font_size: Option<u32>,
}

#[test]
fn with_opt() {
    round_trip(
        &Font {
            family: "Mono".into(),
            font_size: Some(12),
        },
        r#"Font(family:"Mono",font_size:12)"#,
    );
    round_trip(
        &Font {
            family: "Mono".into(),
            font_size: None,
        },
        r#"Font(family:"Mono")"#,
    );
    // Files written before the change, and with the wrapper, still read.
    for text in [
        r#"Font(family:"Mono",font_size:12)"#,
        r#"Font(family:"Mono",font_size:Some(12))"#,
    ] {
        assert_eq!(
            Font::from_ron(text).unwrap(),
            Font {
                family: "Mono".into(),
                font_size: Some(12),
            }
        );
    }
}

#[derive(Debug, Ron, PartialEq)]
struct NestedOpt {
    #[ron(opt, implicit_some)]
    value: Option<Option<u8>>,
}

#[test]
fn nested_with_opt() {
    round_trip(&NestedOpt { value: None }, "NestedOpt()");
    round_trip(
        &NestedOpt { value: Some(None) },
        "NestedOpt(value:Some(None))",
    );
    round_trip(
        &NestedOpt {
            value: Some(Some(1)),
        },
        "NestedOpt(value:Some(Some(1)))",
    );
}

fn is_zero(v: &Option<u32>) -> bool {
    *v == Some(0)
}

fn seven() -> Option<u32> {
    Some(7)
}

#[derive(Debug, Ron, PartialEq)]
struct Combined {
    #[ron(implicit_some, rename = "renamed")]
    a: Option<u32>,
    #[ron(implicit_some, default = "seven")]
    b: Option<u32>,
    #[ron(implicit_some, default, skip_serializing_if = "is_zero")]
    c: Option<u32>,
    #[ron(implicit_some, default)]
    d: Option<u32>,
}

#[test]
fn with_rename_default_and_skip_serializing_if() {
    round_trip(
        &Combined {
            a: Some(1),
            b: Some(2),
            c: Some(3),
            d: Some(4),
        },
        "Combined(renamed:1,b:2,c:3,d:4)",
    );
    let skipped = Combined {
        a: None,
        b: None,
        c: Some(0),
        d: None,
    };
    let text = skipped.to_ron_with(&FormatConfig::minimal()).unwrap();
    assert_eq!(text, "Combined(renamed:None,b:None,d:None)");
    assert_eq!(
        Combined::from_ron("Combined(renamed:1)").unwrap(),
        Combined {
            a: Some(1),
            b: Some(7),
            c: None,
            d: None,
        }
    );
}

#[derive(Debug, Ron, PartialEq)]
enum Event {
    Resize {
        #[ron(implicit_some)]
        width: Option<u32>,
        #[ron(opt, implicit_some, rename = "h")]
        height: Option<u32>,
    },
}

#[test]
fn struct_variants() {
    round_trip(
        &Event::Resize {
            width: Some(1),
            height: Some(2),
        },
        "Resize(width:1,h:2)",
    );
    round_trip(
        &Event::Resize {
            width: None,
            height: None,
        },
        "Resize(width:None)",
    );
}

#[derive(Debug, Ron, PartialEq)]
struct Generic<T> {
    #[ron(implicit_some)]
    value: Option<T>,
}

#[test]
fn generic_field() {
    round_trip(&Generic { value: Some(5u8) }, "Generic(value:5)");
    round_trip(
        &Generic {
            value: Some(Some(5u8)),
        },
        "Generic(value:Some(Some(5)))",
    );
}

/// Validation already accepts a bare value for an `Option` field, so the
/// schema needs no `implicit_some` flag.
#[test]
fn written_text_validates_against_the_schema() {
    fn check<T: ToRon + RonSchema>(value: &T) {
        let catalog = collect_schemas::<T>().unwrap();
        let schema = catalog.resolve(T::type_path().unwrap()).unwrap();
        let text = value.to_ron().unwrap();
        let doc = parse_document(&text).unwrap();
        validate(doc.value.as_ref().unwrap(), &schema, &catalog)
            .unwrap_or_else(|e| panic!("{text}: {e}"));
    }
    check(&Plain {
        size: Some(12),
        name: Some("x".into()),
        flag: Some(true),
        list: Some(vec![1]),
        other: Some(1),
    });
    check(&WithEnum {
        shape: Some(Shape::Rect { w: 1, h: 2 }),
    });
    check(&Nested { value: Some(None) });
    check(&Font {
        family: "Mono".into(),
        font_size: Some(12),
    });
    check(&WithMaybe {
        value: Some(Maybe::None),
    });
}

#[derive(Debug, Ron, PartialEq)]
struct PlainWithout {
    size: Option<u32>,
    name: Option<String>,
    flag: Option<bool>,
    list: Option<Vec<i8>>,
    other: Option<u32>,
}

#[test]
fn schema_is_unchanged() {
    assert_eq!(Plain::type_kind(), PlainWithout::type_kind());
}
