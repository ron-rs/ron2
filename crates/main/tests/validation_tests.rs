//! Tests for schema validation.
//!
//! Most tests validate RON source against hand-written schemas. The
//! differential tests at the end validate against schemas derived from real
//! types and assert that the validator accepts exactly what `FromRon` accepts.

use std::{cell::Cell, collections::HashMap, sync::Arc};

use ron2::{
    PathSegment,
    ast::parse_document,
    error::{Error, ErrorKind},
    schema::{
        Field, NoResolver, Schema, SchemaResolver, Severity, TypeKind, ValidationIssue, Variant,
        validate, validate_all,
    },
};

// =============================================================================
// Helpers
// =============================================================================

/// Resolves type paths from a map and counts calls.
#[derive(Default)]
struct Schemas {
    map: HashMap<String, Arc<Schema>>,
    calls: Cell<usize>,
}

impl Schemas {
    fn with(mut self, type_path: &str, schema: Schema) -> Self {
        self.map.insert(type_path.to_string(), Arc::new(schema));
        self
    }
}

impl SchemaResolver for Schemas {
    fn resolve(&self, type_path: &str) -> Option<Arc<Schema>> {
        self.calls.set(self.calls.get() + 1);
        self.map.get(type_path).cloned()
    }
}

fn issues_with(src: &str, schema: &Schema, resolver: &impl SchemaResolver) -> Vec<ValidationIssue> {
    let doc = parse_document(src).expect("valid RON");
    validate_all(doc.value.as_ref().expect("a value"), schema, resolver)
}

fn issues(src: &str, schema: &Schema) -> Vec<ValidationIssue> {
    issues_with(src, schema, &NoResolver)
}

fn errors_with(src: &str, schema: &Schema, resolver: &impl SchemaResolver) -> Vec<Error> {
    issues_with(src, schema, resolver)
        .into_iter()
        .filter(|i| i.severity == Severity::Error)
        .map(|i| i.error)
        .collect()
}

fn errors(src: &str, schema: &Schema) -> Vec<Error> {
    errors_with(src, schema, &NoResolver)
}

fn warnings(src: &str, schema: &Schema) -> Vec<Error> {
    issues(src, schema)
        .into_iter()
        .filter(|i| i.severity == Severity::Warning)
        .map(|i| i.error)
        .collect()
}

#[track_caller]
fn assert_valid(src: &str, kind: TypeKind) {
    let schema = Schema::new(kind);
    let found = issues(src, &schema);
    assert!(found.is_empty(), "{src}: unexpected issues {found:#?}");
}

#[track_caller]
fn assert_invalid(src: &str, kind: TypeKind) -> Error {
    let schema = Schema::new(kind);
    let mut found = errors(src, &schema);
    assert_eq!(found.len(), 1, "{src}: expected one error, got {found:#?}");
    found.remove(0)
}

fn st(fields: Vec<Field>) -> TypeKind {
    TypeKind::Struct { fields }
}

fn en(variants: Vec<Variant>) -> TypeKind {
    TypeKind::Enum { variants }
}

fn list(kind: TypeKind) -> TypeKind {
    TypeKind::List(Box::new(kind))
}

fn opt(kind: TypeKind) -> TypeKind {
    TypeKind::Option(Box::new(kind))
}

fn tref(path: &str) -> TypeKind {
    TypeKind::TypeRef(path.to_string())
}

/// The source text an error's span covers.
fn spanned<'a>(src: &'a str, err: &Error) -> &'a str {
    &src[err.span().start_offset..err.span().end_offset]
}

// =============================================================================
// Primitives
// =============================================================================

#[test]
fn primitives_accept_their_literals() {
    assert_valid("true", TypeKind::Bool);
    assert_valid("'a'", TypeKind::Char);
    assert_valid("\"s\"", TypeKind::String);
    assert_valid("r#\"raw\"#", TypeKind::String);
    assert_valid("()", TypeKind::Unit);
    assert_valid("1.5", TypeKind::F32);
    assert_valid("1", TypeKind::F64);
    // Integer literals in any base are valid floats.
    assert_valid("0x10", TypeKind::F64);
    assert_valid("-0b101", TypeKind::F32);
    assert_valid("inf", TypeKind::F64);
}

#[test]
fn primitives_reject_other_literals() {
    let err = assert_invalid("\"true\"", TypeKind::Bool);
    assert_eq!(
        err.kind(),
        &ErrorKind::TypeMismatch {
            expected: "bool".into(),
            found: "string".into()
        }
    );
    assert_invalid("1", TypeKind::String);
    assert_invalid("\"a\"", TypeKind::Char);
    assert_invalid("true", TypeKind::F32);
    // A `()` field cannot be written as a name (only unit structs can).
    let unit_field = Schema::new(st(vec![Field::new("u", TypeKind::Unit)]));
    assert_eq!(errors("(u: Marker)", &unit_field).len(), 1);
}

#[test]
fn integers_follow_from_ron_ranges() {
    for (src, kind) in [
        ("127", TypeKind::I8),
        ("-128", TypeKind::I8),
        ("255", TypeKind::U8),
        ("0xFF", TypeKind::U8),
        ("0b1111_1111", TypeKind::U8),
        ("-0x80", TypeKind::I8),
        ("1_000", TypeKind::I16),
        ("18446744073709551615", TypeKind::U64),
        ("200u8", TypeKind::U8),
        ("9223372036854775807", TypeKind::I128),
    ] {
        assert_valid(src, kind);
    }

    // Literals beyond 64 bits need the `integer128` feature, as in FromRon.
    let wide = [
        ("-170141183460469231731687303715884105728", TypeKind::I128),
        ("340282366920938463463374607431768211455", TypeKind::U128),
    ];
    for (src, kind) in wide {
        let found = errors(src, &Schema::new(kind));
        assert_eq!(found.is_empty(), cfg!(feature = "integer128"), "{src}");
    }

    for (src, kind) in [
        ("128", TypeKind::I8),
        ("256", TypeKind::U8),
        ("0x100", TypeKind::U8),
        ("-1", TypeKind::U8),
        ("18446744073709551616", TypeKind::U64),
        ("170141183460469231731687303715884105728", TypeKind::I128),
        ("340282366920938463463374607431768211456", TypeKind::U128),
        ("-1", TypeKind::U128),
        // A suffix must fit its own type.
        ("300u8", TypeKind::U16),
    ] {
        let err = assert_invalid(src, kind);
        assert!(
            matches!(err.kind(), ErrorKind::IntegerOutOfBounds { .. }),
            "{src}: {err}"
        );
    }

    let err = assert_invalid("1.5", TypeKind::I32);
    assert_eq!(
        err.kind(),
        &ErrorKind::TypeMismatch {
            expected: "i32".into(),
            found: "float".into()
        }
    );
    assert_invalid("NaN", TypeKind::U8);
    assert_invalid("\"1\"", TypeKind::I64);
}

#[test]
fn integer_error_spans_the_number() {
    let src = "(a: 1, b: 300)";
    let schema = Schema::new(st(vec![
        Field::new("a", TypeKind::U8),
        Field::new("b", TypeKind::U8),
    ]));
    let found = errors(src, &schema);
    assert_eq!(found.len(), 1);
    assert_eq!(spanned(src, &found[0]), "300");
    assert_eq!(found[0].path(), [PathSegment::Field("b".into())]);
}

// =============================================================================
// Option
// =============================================================================

#[test]
fn option_accepts_bare_value_as_implicit_some() {
    assert_valid("5", opt(TypeKind::I32));
    assert_valid("Some(5)", opt(TypeKind::I32));
    assert_valid("None", opt(TypeKind::I32));
    assert_valid("true", opt(opt(TypeKind::Bool)));
    assert_valid("Some(None)", opt(opt(TypeKind::Bool)));
    assert_valid("[1, None, Some(3)]", list(opt(TypeKind::I32)));
}

#[test]
fn option_checks_inner_value() {
    assert_invalid("\"x\"", opt(TypeKind::I32));
    let src = "Some(\"x\")";
    let err = assert_invalid(src, opt(TypeKind::I32));
    assert_eq!(spanned(src, &err), "\"x\"");
}

// =============================================================================
// Collections
// =============================================================================

#[test]
fn arrays_check_length() {
    let kind = <[u8; 3] as ron2::schema::RonSchema>::type_kind();
    assert_eq!(kind.to_string(), "[u8; 3]");
    assert_valid("[1, 2, 3]", kind.clone());
    assert_valid("(1, 2, 3)", kind.clone());
    let err = assert_invalid("[1, 2]", kind.clone());
    assert!(matches!(
        err.kind(),
        ErrorKind::LengthMismatch { found: 2, .. }
    ));
    let err = assert_invalid("[1, 2, 300]", kind);
    assert_eq!(err.path(), [PathSegment::Element(2)]);
}

#[test]
fn lists_accept_sequences_and_tuples() {
    assert_valid("[]", list(TypeKind::I32));
    assert_valid("[1, 2]", list(TypeKind::I32));
    assert_valid("(1, 2)", list(TypeKind::I32));
    assert_invalid("{}", list(TypeKind::I32));
    assert_invalid("Name(1, 2)", list(TypeKind::I32));
}

#[test]
fn list_reports_every_bad_element_with_index() {
    let src = "[1, \"a\", 3, \"b\"]";
    let found = errors(src, &Schema::new(list(TypeKind::I32)));
    let paths: Vec<_> = found.iter().map(|e| e.path().to_vec()).collect();
    assert_eq!(
        paths,
        [vec![PathSegment::Element(1)], vec![PathSegment::Element(3)]]
    );
}

#[test]
fn maps_check_keys_and_values() {
    let kind = TypeKind::Map {
        key: Box::new(TypeKind::String),
        value: Box::new(TypeKind::I32),
    };
    assert_valid("{}", kind.clone());
    assert_valid("{\"a\": 1}", kind.clone());

    let found = errors("{1: 1, \"b\": \"x\"}", &Schema::new(kind.clone()));
    assert_eq!(found.len(), 2);
    assert_eq!(found[0].path(), [PathSegment::MapKey]);
    assert_eq!(found[1].path(), [PathSegment::MapValue("\"b\"".into())]);
    assert_invalid("[]", kind.clone());

    // Repeated keys are rejected, compared by value as FromRon does.
    let src = "{\"a\": 1, \"b\": 2, \"a\": 3}";
    let err = assert_invalid(src, kind);
    assert_eq!(
        err.kind(),
        &ErrorKind::DuplicateKey {
            key: "\"a\"".into()
        }
    );
    assert_eq!(err.span().start_offset, src.rfind("\"a\"").unwrap());
    let int_keys = TypeKind::Map {
        key: Box::new(TypeKind::U8),
        value: Box::new(TypeKind::Bool),
    };
    assert_invalid("{1: true, 0x1: false}", int_keys);
}

#[test]
fn tuples_check_length_and_elements() {
    let kind = TypeKind::Tuple(vec![TypeKind::I32, TypeKind::String]);
    assert_valid("(1, \"a\")", kind.clone());
    assert_valid("[1, \"a\"]", kind.clone());
    let err = assert_invalid("(1, \"a\", 3)", kind.clone());
    assert!(matches!(
        err.kind(),
        ErrorKind::LengthMismatch { found: 3, .. }
    ));
    let err = assert_invalid("(\"a\", \"a\")", kind.clone());
    assert_eq!(err.path(), [PathSegment::Element(0)]);
    // A plain tuple has no name; only tuple structs may be written by name.
    let field = Schema::new(st(vec![Field::new("t", kind)]));
    assert_eq!(errors("(t: Pair(1, \"a\"))", &field).len(), 1);
}

// =============================================================================
// Structs
// =============================================================================

fn point() -> Schema {
    Schema::new(st(vec![
        Field::new("x", TypeKind::I32),
        Field::new("y", TypeKind::I32),
    ]))
    .with_name("Point")
}

#[test]
fn struct_accepts_anonymous_and_named_forms() {
    for src in ["(x: 1, y: 2)", "Point(x: 1, y: 2)", "(y: 2, x: 1)"] {
        assert!(issues(src, &point()).is_empty(), "{src}");
    }
}

#[test]
fn struct_rejects_wrong_name_when_known() {
    let src = "Bogus(x: 1, y: 2)";
    let found = errors(src, &point());
    assert_eq!(found.len(), 1);
    assert_eq!(
        found[0].kind(),
        &ErrorKind::TypeMismatch {
            expected: "struct Point".into(),
            found: "struct Bogus".into()
        }
    );
    assert_eq!(spanned(src, &found[0]), "Bogus");

    // Without a known name (old schema files), any name is accepted.
    let mut unnamed = point();
    unnamed.name = None;
    assert!(errors(src, &unnamed).is_empty());
}

#[test]
fn struct_reports_missing_fields_at_struct() {
    let src = "(x: 1)";
    let found = errors(src, &point());
    assert_eq!(found.len(), 1);
    assert_eq!(
        found[0].kind(),
        &ErrorKind::MissingField {
            field: "y".into(),
            outer: None
        }
    );
    assert_eq!(spanned(src, &found[0]), src);
}

#[test]
fn struct_reports_duplicate_fields() {
    let src = "(x: 1, y: 2, x: 3)";
    let found = errors(src, &point());
    assert_eq!(found.len(), 1);
    assert!(matches!(found[0].kind(), ErrorKind::DuplicateField { field, .. } if field == "x"));
    assert_eq!(src[found[0].span().start_offset..].find("x: 3"), Some(0));
}

#[test]
fn unknown_fields_are_warnings_unless_denied() {
    let src = "(x: 1, y: 2, z: 3)";
    assert!(errors(src, &point()).is_empty());
    let found = warnings(src, &point());
    assert_eq!(found.len(), 1);
    assert!(matches!(found[0].kind(), ErrorKind::UnknownField { field, .. } if field == "z"));
    assert_eq!(spanned(src, &found[0]), "z");

    let strict = point().with_deny_unknown_fields();
    assert_eq!(errors(src, &strict).len(), 1);
    assert!(
        validate(
            parse_document(src).unwrap().value.as_ref().unwrap(),
            &point(),
            &NoResolver
        )
        .is_ok()
    );
}

#[test]
fn struct_without_fields_is_valid_only_with_defaults() {
    let defaults = Schema::new(st(vec![
        Field::optional("a", TypeKind::I32),
        Field::optional("b", TypeKind::String),
    ]))
    .with_name("Defaults");
    for src in ["()", "Defaults()", "(a: 1)"] {
        assert!(issues(src, &defaults).is_empty(), "{src}");
    }
    assert_eq!(errors("()", &point()).len(), 2);
    assert_eq!(errors("Point()", &point()).len(), 2);
    // A named-field struct cannot be written as a bare name.
    assert_eq!(errors("Defaults", &defaults).len(), 1);
}

#[test]
fn struct_rejects_other_shapes() {
    for src in ["[1, 2]", "Point(1, 2)", "5", "{\"x\": 1}"] {
        assert_eq!(errors(src, &point()).len(), 1, "{src}");
    }
}

#[test]
fn unit_struct_accepts_its_name() {
    let marker = Schema::new(TypeKind::Unit).with_name("Marker");
    assert!(issues("Marker", &marker).is_empty());
    assert!(issues("()", &marker).is_empty());
    assert_eq!(errors("Other", &marker).len(), 1);
    assert_eq!(errors("Marker()", &marker).len(), 1);
}

#[test]
fn tuple_struct_accepts_its_name() {
    let pair =
        Schema::new(TypeKind::Tuple(vec![TypeKind::I32, TypeKind::String])).with_name("Pair");
    for src in ["Pair(1, \"a\")", "(1, \"a\")", "[1, \"a\"]"] {
        assert!(issues(src, &pair).is_empty(), "{src}");
    }
    assert_eq!(errors("Other(1, \"a\")", &pair).len(), 1);
    assert_eq!(errors("Pair(a: 1)", &pair).len(), 1);
}

#[test]
fn undeclared_names_are_rejected_for_tuples_and_units() {
    // Transparent wrappers over a tuple or `()` have no name of their own.
    let tuple = Schema::new(TypeKind::Tuple(vec![TypeKind::I32, TypeKind::I32]));
    assert!(issues("(1, 2)", &tuple).is_empty());
    assert_eq!(errors("Foo(1, 2)", &tuple).len(), 1);
    let unit = Schema::new(TypeKind::Unit);
    assert!(issues("()", &unit).is_empty());
    assert_eq!(errors("Bar", &unit).len(), 1);
}

#[test]
fn nested_struct_errors_have_paths() {
    let schema = Schema::new(st(vec![Field::new(
        "server",
        st(vec![Field::new("ports", list(TypeKind::U16))]),
    )]));
    let src = "(server: (ports: [80, -1]))";
    let found = errors(src, &schema);
    assert_eq!(found.len(), 1);
    assert_eq!(spanned(src, &found[0]), "-1");
    assert_eq!(
        found[0].to_string().split(": ").nth(1),
        Some("in field 'server' -> field 'ports' -> element 1")
    );
}

// =============================================================================
// Flatten
// =============================================================================

fn inner_schema() -> Schema {
    Schema::new(st(vec![
        Field::new("b", TypeKind::I32),
        Field::optional("c", TypeKind::I32),
    ]))
}

#[test]
fn flattened_struct_fields_are_merged() {
    let outer = Schema::new(st(vec![
        Field::new("a", TypeKind::I32),
        Field::flattened("inner", tref("t::Inner")),
    ]));
    let resolver = Schemas::default().with("t::Inner", inner_schema());

    assert!(issues_with("(a: 1, b: 2)", &outer, &resolver).is_empty());
    assert!(issues_with("(a: 1, b: 2, c: 3)", &outer, &resolver).is_empty());
    let found = errors_with("(a: 1)", &outer, &resolver);
    assert!(
        matches!(found[..], [ref e] if matches!(e.kind(), ErrorKind::MissingField { field, .. } if field == "b"))
    );
    let found = errors_with("(a: 1, b: \"x\")", &outer, &resolver);
    assert_eq!(found[0].path(), [PathSegment::Field("b".into())]);
}

#[test]
fn flatten_works_in_enum_struct_variants() {
    let schema = Schema::new(en(vec![Variant::struct_variant(
        "V",
        vec![
            Field::new("a", TypeKind::I32),
            Field::flattened("inner", tref("t::Inner")),
        ],
    )]));
    let resolver = Schemas::default().with("t::Inner", inner_schema());

    assert!(issues_with("V(a: 1, b: 2)", &schema, &resolver).is_empty());
    let found = errors_with("V(a: 1)", &schema, &resolver);
    assert_eq!(found.len(), 1);
    assert_eq!(
        found[0].path(),
        [PathSegment::Variant("V".into())],
        "{found:#?}"
    );
}

#[test]
fn optional_flatten_is_presence_based() {
    let outer = Schema::new(st(vec![
        Field::new("a", TypeKind::I32),
        Field::flattened("inner", opt(tref("t::Inner"))),
    ]));
    let resolver = Schemas::default().with("t::Inner", inner_schema());
    assert!(issues_with("(a: 1)", &outer, &resolver).is_empty());
    // Once one of its fields is present, the others become required.
    let found = errors_with("(a: 1, c: 1)", &outer, &resolver);
    assert!(
        matches!(&found[..], [e] if matches!(e.kind(), ErrorKind::MissingField { field, .. } if field == "b")),
        "{found:#?}"
    );
    assert!(issues_with("(a: 1, b: 2)", &outer, &resolver).is_empty());
}

#[test]
fn optional_flatten_presence_covers_nested_flattens() {
    // `Option<Middle { m, #[flatten] inner: Inner { b, c? } }>`: writing only
    // `m` makes the whole group present, so `b` is required too.
    let middle = Schema::new(st(vec![
        Field::new("m", TypeKind::I32),
        Field::flattened("inner", tref("t::Inner")),
    ]));
    let outer = Schema::new(st(vec![Field::flattened("mid", opt(tref("t::Middle")))]));
    let resolver = Schemas::default()
        .with("t::Inner", inner_schema())
        .with("t::Middle", middle);
    assert!(issues_with("()", &outer, &resolver).is_empty());
    assert_eq!(errors_with("(m: 1)", &outer, &resolver).len(), 1);
    assert_eq!(errors_with("(b: 1)", &outer, &resolver).len(), 1);
    assert!(issues_with("(m: 1, b: 2)", &outer, &resolver).is_empty());
}

#[test]
fn nested_flatten_is_followed() {
    let middle = Schema::new(st(vec![
        Field::new("m", TypeKind::I32),
        Field::flattened("inner", tref("t::Inner")),
    ]));
    let outer = Schema::new(st(vec![Field::flattened("middle", tref("t::Middle"))]));
    let resolver = Schemas::default()
        .with("t::Inner", inner_schema())
        .with("t::Middle", middle);
    assert!(issues_with("(m: 1, b: 2)", &outer, &resolver).is_empty());
    assert_eq!(errors_with("(m: 1)", &outer, &resolver).len(), 1);
}

#[test]
fn flattened_map_takes_remaining_fields() {
    let outer = Schema::new(st(vec![
        Field::new("a", TypeKind::I32),
        Field::flattened(
            "extra",
            TypeKind::Map {
                key: Box::new(TypeKind::String),
                value: Box::new(TypeKind::Bool),
            },
        ),
    ]))
    .with_deny_unknown_fields();
    assert!(issues("(a: 1, x: true, y: false)", &outer).is_empty());
    assert_eq!(errors("(a: 1, x: 1)", &outer).len(), 1);
}

#[test]
fn unresolved_flatten_suppresses_unknown_fields() {
    let outer = Schema::new(st(vec![
        Field::new("a", TypeKind::I32),
        Field::flattened("inner", tref("t::Missing")),
    ]));
    let found = issues("(a: 1, b: 2)", &outer);
    assert_eq!(found.len(), 1);
    assert_eq!(found[0].severity, Severity::Warning);
    assert_eq!(
        found[0].error.kind(),
        &ErrorKind::UnresolvedTypeRef("t::Missing".into())
    );
}

// =============================================================================
// Enums
// =============================================================================

fn color() -> Schema {
    Schema::new(en(vec![
        Variant::unit("Red"),
        Variant::tuple("Rgb", vec![TypeKind::U8, TypeKind::U8, TypeKind::U8]),
        Variant::struct_variant(
            "Named",
            vec![
                Field::new("name", TypeKind::String),
                Field::optional("alpha", TypeKind::F32),
            ],
        ),
    ]))
}

#[test]
fn enum_accepts_each_variant_form() {
    for src in [
        "Red",
        "Red()",
        "Rgb(1, 2, 3)",
        "Named(name: \"x\")",
        "Named(name: \"x\", alpha: 0.5)",
    ] {
        assert!(issues(src, &color()).is_empty(), "{src}");
    }
}

#[test]
fn enum_rejects_strings_and_mismatched_content() {
    for src in [
        "\"Red\"",
        "Red(1)",
        "Rgb(1, 2)",
        "Rgb",
        "Named",
        "Named()",
        "Rgb(r: 1)",
        "(1, 2, 3)",
    ] {
        assert_eq!(errors(src, &color()).len(), 1, "{src}");
    }
}

#[test]
fn enum_unknown_variant_spans_the_name() {
    let src = "Blue";
    let found = errors(src, &color());
    assert!(
        matches!(found[0].kind(), ErrorKind::UnknownVariant { variant, .. } if variant == "Blue")
    );
    assert_eq!(spanned(src, &found[0]), "Blue");
}

#[test]
fn enum_variant_errors_have_variant_path() {
    let src = "Rgb(1, 300, 3)";
    let found = errors(src, &color());
    assert_eq!(
        found[0].path(),
        [PathSegment::Element(1), PathSegment::Variant("Rgb".into())]
    );
    assert_eq!(spanned(src, &found[0]), "300");

    let found = errors("Named(name: 1)", &color());
    assert_eq!(
        found[0].path(),
        [
            PathSegment::Field("name".into()),
            PathSegment::Variant("Named".into())
        ]
    );
}

#[test]
fn enum_struct_variant_unknown_fields_follow_deny_flag() {
    let src = "Named(name: \"x\", nam: 1)";
    assert!(errors(src, &color()).is_empty());
    assert_eq!(warnings(src, &color()).len(), 1);
    assert_eq!(errors(src, &color().with_deny_unknown_fields()).len(), 1);
}

#[test]
fn option_syntax_selects_none_and_some_variants() {
    let schema = Schema::new(en(vec![
        Variant::unit("None"),
        Variant::tuple("Some", vec![TypeKind::I32]),
    ]));
    assert!(issues("None", &schema).is_empty());
    assert!(issues("Some(1)", &schema).is_empty());
    assert_eq!(errors("Some(\"x\")", &schema).len(), 1);
    assert_eq!(errors("None", &color()).len(), 1);
    assert_eq!(errors("Some(1)", &color()).len(), 1);
}

// =============================================================================
// TypeRef
// =============================================================================

#[test]
fn typeref_validates_against_resolved_schema() {
    let schema = Schema::new(st(vec![Field::new("p", tref("geo::Point"))]));
    let resolver = Schemas::default().with("geo::Point", point());

    assert!(issues_with("(p: Point(x: 1, y: 2))", &schema, &resolver).is_empty());
    let found = errors_with("(p: (x: 1, y: \"2\"))", &schema, &resolver);
    assert_eq!(found.len(), 1);
    assert_eq!(
        found[0].path(),
        [
            PathSegment::Field("y".into()),
            PathSegment::TypeRef("geo::Point".into()),
            PathSegment::Field("p".into()),
        ]
    );
    // The resolved schema's name applies.
    assert_eq!(
        errors_with("(p: Bogus(x: 1, y: 2))", &schema, &resolver).len(),
        1
    );
}

#[test]
fn unresolved_typeref_is_a_warning_at_the_value() {
    let schema = Schema::new(st(vec![Field::new("p", tref("geo::Missing"))]));
    let src = "(p: (x: 1))";
    let found = issues(src, &schema);
    assert_eq!(found.len(), 1);
    assert_eq!(found[0].severity, Severity::Warning);
    assert_eq!(
        found[0].error.kind(),
        &ErrorKind::UnresolvedTypeRef("geo::Missing".into())
    );
    assert_eq!(spanned(src, &found[0].error), "(x: 1)");
    // Warnings do not fail fail-fast validation.
    let doc = parse_document(src).unwrap();
    assert!(validate(doc.value.as_ref().unwrap(), &schema, &NoResolver).is_ok());
}

#[test]
fn recursive_types_validate_every_level() {
    let node = Schema::new(st(vec![
        Field::new("value", TypeKind::I32),
        Field::new("children", list(tref("t::Node"))),
    ]));
    let resolver = Schemas::default().with("t::Node", node.clone());

    let src = "(value: 1, children: [(value: \"bad\", children: [])])";
    let found = errors_with(src, &node, &resolver);
    assert_eq!(found.len(), 1, "{found:#?}");
    assert_eq!(spanned(src, &found[0]), "\"bad\"");

    let deep = "(value: 1, children: [(value: 2, children: [(value: 3, children: [(value: true, children: [])])])])";
    assert_eq!(errors_with(deep, &node, &resolver).len(), 1);
}

#[test]
fn mutually_recursive_types_validate_every_level() {
    let a = Schema::new(st(vec![Field::optional("b", opt(tref("t::B")))]));
    let b = Schema::new(st(vec![
        Field::new("n", TypeKind::I32),
        Field::optional("a", opt(tref("t::A"))),
    ]));
    let resolver = Schemas::default().with("t::A", a.clone()).with("t::B", b);
    assert!(issues_with("(b: (n: 1, a: (b: (n: 2))))", &a, &resolver).is_empty());
    assert_eq!(
        errors_with("(b: (n: 1, a: (b: (n: \"x\"))))", &a, &resolver).len(),
        1
    );
}

#[test]
fn typeref_cycles_without_structure_terminate() {
    // `A = B`, `B = A`: a cycle that consumes no value structure.
    let resolver = Schemas::default()
        .with("t::A", Schema::new(tref("t::B")))
        .with("t::B", Schema::new(tref("t::A")))
        // `C = Option<C>`: implicit `Some` also consumes no structure.
        .with("t::C", Schema::new(opt(tref("t::C"))));
    assert!(issues_with("1", &Schema::new(tref("t::A")), &resolver).is_empty());
    assert!(issues_with("1", &Schema::new(tref("t::C")), &resolver).is_empty());
}

#[test]
fn each_type_path_is_resolved_once_per_run() {
    let schema = Schema::new(list(tref("t::Item")));
    let resolver = Schemas::default()
        .with("t::Item", Schema::new(TypeKind::I32))
        .with("t::Unused", Schema::new(TypeKind::I32));
    let src = format!("[{}]", vec!["1"; 1000].join(", "));
    assert!(issues_with(&src, &schema, &resolver).is_empty());
    assert_eq!(resolver.calls.get(), 1);

    // Misses are cached too, and reported once, at the first occurrence.
    let missing = Schemas::default();
    let found = issues_with(&src, &schema, &missing);
    assert_eq!(found.len(), 1);
    assert_eq!(found[0].error.span().start_offset, 1);
    assert_eq!(missing.calls.get(), 1);
}

#[test]
fn unresolved_warnings_are_per_type_path() {
    let schema = Schema::new(st(vec![
        Field::new("a", list(tref("t::A"))),
        Field::new("b", tref("t::B")),
        Field::flattened("c", tref("t::A")),
    ]));
    let found = issues("(a: [1, 2], b: 3, x: 4)", &schema);
    let paths: Vec<_> = found
        .iter()
        .map(|i| match i.error.kind() {
            ErrorKind::UnresolvedTypeRef(path) => path.as_str(),
            other => panic!("{other:?}"),
        })
        .collect();
    assert_eq!(paths, ["t::A", "t::B"]);
}

#[test]
fn self_flattening_schema_stays_linear() {
    // `S { a, #[flatten] f0..f3: S }` used to expand 4^16 times.
    let mut fields = vec![Field::new("a", TypeKind::I32)];
    fields.extend((0..4).map(|i| Field::flattened(format!("f{i}"), tref("t::S"))));
    let s = Schema::new(st(fields));
    let resolver = Schemas::default().with("t::S", s.clone());
    let start = std::time::Instant::now();
    assert!(issues_with("(a: 1)", &s, &resolver).is_empty());
    assert_eq!(errors_with("()", &s, &resolver).len(), 1);
    assert!(start.elapsed() < std::time::Duration::from_secs(5));
}

#[test]
fn unknown_field_and_variant_messages_omit_empty_alternatives() {
    let found = warnings("(x: 1, y: 2, zz: 3)", &point());
    assert_eq!(found[0].kind().to_string(), "Unknown field `zz`");
    let found = errors("Blue", &color());
    assert_eq!(found[0].kind().to_string(), "Unknown enum variant `Blue`");
}

// =============================================================================
// Modes and parse errors
// =============================================================================

#[test]
fn validate_stops_at_first_error_and_validate_all_collects() {
    let src = "(x: \"a\", y: \"b\")";
    let doc = parse_document(src).unwrap();
    let expr = doc.value.as_ref().unwrap();
    let first = validate(expr, &point(), &NoResolver).unwrap_err();
    assert_eq!(first.path(), [PathSegment::Field("x".into())]);
    assert_eq!(validate_all(expr, &point(), &NoResolver).len(), 2);
}

#[test]
fn error_nodes_from_recovery_are_skipped() {
    let (doc, parse_errors) = ron2::ast::parse_document_lossy("(x: , y: 2)");
    assert!(!parse_errors.is_empty());
    let found = validate_all(doc.value.as_ref().unwrap(), &point(), &NoResolver);
    assert!(found.is_empty(), "{found:#?}");
}

// =============================================================================
// Differential tests against FromRon
// =============================================================================

mod differential {
    #![allow(dead_code)]

    use ron2::{
        FromRon, RonSchema,
        schema::collect::{SchemaCatalog, collect_schemas},
    };

    use super::*;

    #[derive(FromRon, RonSchema)]
    struct Point {
        x: i32,
        y: i32,
    }

    #[derive(FromRon, RonSchema)]
    #[ron(deny_unknown_fields)]
    struct Strict {
        a: i32,
    }

    #[derive(FromRon, RonSchema)]
    struct Marker;

    #[derive(FromRon, RonSchema)]
    struct Pair(i32, String);

    #[derive(FromRon, RonSchema)]
    enum Color {
        Red,
        Rgb(u8, u8, u8),
        Named { name: String },
    }

    #[derive(FromRon, RonSchema)]
    struct Opts {
        maybe: Option<i32>,
        nested: Option<Option<bool>>,
    }

    #[derive(FromRon, RonSchema)]
    struct Lists {
        items: Vec<i32>,
        pair: (i32, bool),
    }

    #[derive(FromRon, RonSchema)]
    struct Inner {
        b: i32,
    }

    #[derive(FromRon, RonSchema)]
    struct Outer {
        a: i32,
        #[ron(flatten)]
        inner: Inner,
    }

    #[derive(FromRon, RonSchema)]
    enum WithFlatten {
        V {
            a: i32,
            #[ron(flatten)]
            inner: Inner,
        },
    }

    #[derive(FromRon, RonSchema)]
    struct Node {
        value: i32,
        children: Vec<Node>,
    }

    #[derive(FromRon, RonSchema)]
    struct Defaults {
        #[ron(default)]
        a: i32,
        #[ron(default)]
        b: String,
    }

    #[derive(FromRon, RonSchema)]
    struct Ints {
        a: u8,
        b: i8,
        c: u64,
    }

    #[derive(FromRon, RonSchema)]
    enum MaybeLike {
        None,
        Some(i32),
    }

    /// Collect the schemas of all test types, adding the metadata
    /// `#[derive(RonSchema)]` does not emit yet (struct names, deny flags).
    fn catalog() -> SchemaCatalog {
        let mut catalog = SchemaCatalog::default();
        for part in [
            collect_schemas::<Point>(),
            collect_schemas::<Strict>(),
            collect_schemas::<Marker>(),
            collect_schemas::<Pair>(),
            collect_schemas::<Color>(),
            collect_schemas::<Opts>(),
            collect_schemas::<Lists>(),
            collect_schemas::<Outer>(),
            collect_schemas::<WithFlatten>(),
            collect_schemas::<Node>(),
            collect_schemas::<Defaults>(),
            collect_schemas::<Ints>(),
            collect_schemas::<MaybeLike>(),
        ] {
            catalog.schemas.extend(part.unwrap().schemas);
        }
        for (path, schema) in &mut catalog.schemas {
            let name = path.rsplit("::").next().unwrap();
            let schema = Arc::make_mut(schema);
            if matches!(
                schema.kind,
                TypeKind::Struct { .. } | TypeKind::Tuple(_) | TypeKind::Unit
            ) {
                schema.name = Some(name.to_string());
            }
            schema.deny_unknown_fields = name == "Strict";
        }
        catalog
    }

    /// Resolves derived `TypeRef`s by type name: they may lack the module
    /// path that `type_path()` has.
    struct BySuffix(SchemaCatalog);

    impl SchemaResolver for BySuffix {
        fn resolve(&self, type_path: &str) -> Option<Arc<Schema>> {
            let name = type_path.rsplit("::").next();
            self.0.resolve(type_path).or_else(|| {
                self.0
                    .schemas
                    .iter()
                    .find(|(path, _)| path.rsplit("::").next() == name)
                    .map(|(_, schema)| Arc::clone(schema))
            })
        }
    }

    /// `Some(description)` if the validator and `FromRon` disagree on `src`.
    fn disagreement<T: FromRon + ron2::schema::RonSchema>(
        resolver: &BySuffix,
        src: &str,
    ) -> Option<String> {
        let schema = resolver.resolve(T::type_path().unwrap()).unwrap();
        let from_ron = T::from_ron(src).map(|_| ());
        let doc = parse_document(src).unwrap();
        let validated = validate(doc.value.as_ref().unwrap(), &schema, resolver);
        (from_ron.is_ok() != validated.is_ok()).then(|| {
            format!(
                "{}: {src}\n  FromRon: {from_ron:?}\n  validate: {validated:?}",
                core::any::type_name::<T>()
            )
        })
    }

    macro_rules! cases {
        ($resolver:expr; $($ty:ty => [$($src:expr),* $(,)?]),* $(,)?) => {{
            let mut failures = Vec::new();
            $($(failures.extend(disagreement::<$ty>($resolver, $src));)*)*
            failures
        }};
    }

    #[test]
    fn validator_agrees_with_from_ron() {
        let resolver = BySuffix(catalog());
        let failures = cases![&resolver;
            Point => [
                "(x: 1, y: 2)", "Point(x: 1, y: 2)", "Bogus(x: 1, y: 2)", "(x: 1)",
                "(x: 1, y: 2, x: 3)", "(x: 1, y: 2, z: 3)", "Point", "Point()", "()",
                "(x: \"1\", y: 2)", "[1, 2]", "Point(1, 2)",
            ],
            Strict => ["(a: 1)", "(a: 1, b: 2)", "Strict(a: 1, b: 2)"],
            Marker => ["Marker", "()", "Other", "Marker()", "Marker(x: 1)"],
            Pair => [
                "(1, \"a\")", "[1, \"a\"]", "Pair(1, \"a\")", "Other(1, \"a\")",
                "Pair(1)", "Pair(a: 1)", "(1, 2)",
            ],
            Color => [
                "Red", "Red()", "\"Red\"", "Red(1)", "Rgb(1, 2, 3)", "Rgb(1, 2)",
                "Rgb(1, 2, 300)", "Named(name: \"x\")", "Named()", "Named", "Blue",
                "Named(name: \"x\", other: 1)", "None",
            ],
            Opts => [
                "(maybe: 5, nested: true)", "(maybe: None, nested: Some(None))",
                "(maybe: Some(5), nested: None)", "(maybe: \"x\", nested: None)",
                "(maybe: 5, nested: Some(Some(1)))", "(nested: None)",
            ],
            Lists => [
                "(items: [1, 2], pair: (1, true))", "(items: (1, 2), pair: [1, true])",
                "(items: [1, \"2\"], pair: (1, true))", "(items: [], pair: (1, true, 3))",
                "(items: Items(1), pair: (1, true))",
            ],
            Outer => [
                "(a: 1, b: 2)", "(a: 1)", "(a: 1, b: \"x\")", "Outer(a: 1, b: 2)", "()",
            ],
            WithFlatten => ["V(a: 1, b: 2)", "V(a: 1)", "V(a: 1, b: \"x\")", "V(b: 2)"],
            Node => [
                "(value: 1, children: [])",
                "(value: 1, children: [(value: 2, children: [])])",
                "(value: 1, children: [(value: \"bad\", children: [])])",
                "(value: 1, children: [(value: 2)])",
            ],
            Defaults => ["()", "Defaults()", "(a: 5)", "Defaults", "(a: \"x\")"],
            Ints => [
                "(a: 255, b: -128, c: 0xFF)", "(a: 256, b: 0, c: 0)", "(a: -1, b: 0, c: 0)",
                "(a: 1.0, b: 0, c: 0)", "(a: 1, b: 128, c: 0)",
                "(a: 1, b: 1, c: -1)",
            ],
            MaybeLike => ["None", "Some(5)", "Some(\"x\")", "5", "Other"],
        ];
        assert!(failures.is_empty(), "{}", failures.join("\n"));
    }
}
