//! Tests for RON extension attributes: transparent, explicit, implicit_some
//!
//! These tests verify the behavior of extension attributes in ron-derive:
//! - `#[ron(transparent)]` - Serialize/deserialize as the single inner field
//! - `#[ron(explicit)]` - Require explicit Some(...) or None for Option fields
//! - Implicit Some (default) - Option fields accept bare values

#![allow(dead_code)]

use ron2::{FromRon, ToRon};
use ron_derive::Ron;

// =============================================================================
// Transparent tests
// =============================================================================

mod transparent {
    use super::*;

    /// Newtype struct with transparent serialization.
    #[derive(Debug, Ron, PartialEq)]
    #[ron(transparent)]
    struct UserId(u64);

    #[test]
    fn newtype_deserialize() {
        let id: UserId = UserId::from_ron("42").unwrap();
        assert_eq!(id.0, 42);
    }

    #[test]
    fn newtype_serialize() {
        let id = UserId(123);
        assert_eq!(id.to_ron().unwrap(), "123");
    }

    #[test]
    fn newtype_roundtrip() {
        let original = UserId(999);
        let ron = original.to_ron().unwrap();
        let parsed: UserId = UserId::from_ron(&ron).unwrap();
        assert_eq!(original, parsed);
    }

    /// Named single-field struct with transparent serialization.
    #[derive(Debug, Ron, PartialEq)]
    #[ron(transparent)]
    struct Email {
        address: String,
    }

    #[test]
    fn named_struct_deserialize() {
        let email: Email = Email::from_ron(r#""user@example.com""#).unwrap();
        assert_eq!(email.address, "user@example.com");
    }

    #[test]
    fn named_struct_serialize() {
        let email = Email {
            address: "test@test.com".to_string(),
        };
        assert_eq!(email.to_ron().unwrap(), r#""test@test.com""#);
    }

    /// Struct with one active field and one skipped field.
    #[derive(Debug, Ron, PartialEq, Default)]
    #[ron(transparent)]
    struct WithSkipped {
        value: i32,
        #[ron(skip)]
        _internal: u8,
    }

    #[test]
    fn with_skipped_field_deserialize() {
        let item: WithSkipped = WithSkipped::from_ron("42").unwrap();
        assert_eq!(item.value, 42);
        assert_eq!(item._internal, 0); // default
    }

    #[test]
    fn with_skipped_field_serialize() {
        let item = WithSkipped {
            value: 100,
            _internal: 255,
        };
        // Only value is serialized, skipped field is ignored
        assert_eq!(item.to_ron().unwrap(), "100");
    }

    /// Transparent wrapper around Option.
    #[derive(Debug, Ron, PartialEq)]
    #[ron(transparent)]
    struct MaybeValue(Option<String>);

    #[test]
    fn transparent_option_some() {
        let val: MaybeValue = MaybeValue::from_ron(r#"Some("hello")"#).unwrap();
        assert_eq!(val.0, Some("hello".to_string()));
    }

    #[test]
    fn transparent_option_none() {
        let val: MaybeValue = MaybeValue::from_ron("None").unwrap();
        assert_eq!(val.0, None);
    }

    #[test]
    fn transparent_option_implicit_some() {
        // Implicit some works through transparent
        let val: MaybeValue = MaybeValue::from_ron(r#""world""#).unwrap();
        assert_eq!(val.0, Some("world".to_string()));
    }

    // Schema tests for transparent types
    use ron_schema::{RonSchemaType, TypeKind};

    #[test]
    fn transparent_newtype_schema_is_inner_type() {
        // UserId(u64) should have schema U64, not Tuple([U64])
        let type_kind = UserId::type_kind();
        assert!(
            matches!(type_kind, TypeKind::U64),
            "Expected U64, got {:?}",
            type_kind
        );
    }

    #[test]
    fn transparent_named_struct_schema_is_inner_type() {
        // Email { address: String } should have schema String, not Struct
        let type_kind = Email::type_kind();
        assert!(
            matches!(type_kind, TypeKind::String),
            "Expected String, got {:?}",
            type_kind
        );
    }

    #[test]
    fn transparent_option_schema_is_inner_type() {
        // MaybeValue(Option<String>) should have schema Option(String)
        let type_kind = MaybeValue::type_kind();
        match type_kind {
            TypeKind::Option(inner) => {
                assert!(
                    matches!(*inner, TypeKind::String),
                    "Expected Option(String), got Option({:?})",
                    inner
                );
            }
            _ => panic!("Expected Option, got {:?}", type_kind),
        }
    }
}

// =============================================================================
// Implicit Some tests (default behavior)
// =============================================================================

mod implicit_some {
    use super::*;

    #[derive(Debug, Ron, PartialEq)]
    struct Config {
        name: Option<String>,
        count: Option<i32>,
    }

    #[test]
    fn bare_value_becomes_some() {
        let cfg: Config = Config::from_ron(r#"(name: "Alice", count: 42)"#).unwrap();
        assert_eq!(cfg.name, Some("Alice".to_string()));
        assert_eq!(cfg.count, Some(42));
    }

    #[test]
    fn explicit_some_also_works() {
        let cfg: Config = Config::from_ron(r#"(name: Some("Bob"), count: Some(10))"#).unwrap();
        assert_eq!(cfg.name, Some("Bob".to_string()));
        assert_eq!(cfg.count, Some(10));
    }

    #[test]
    fn none_stays_none() {
        let cfg: Config = Config::from_ron(r#"(name: None, count: None)"#).unwrap();
        assert_eq!(cfg.name, None);
        assert_eq!(cfg.count, None);
    }

    #[test]
    fn mixed_implicit_and_explicit() {
        let cfg: Config = Config::from_ron(r#"(name: "Charlie", count: Some(5))"#).unwrap();
        assert_eq!(cfg.name, Some("Charlie".to_string()));
        assert_eq!(cfg.count, Some(5));
    }

    /// Nested Option<Option<T>> with implicit some.
    #[derive(Debug, Ron, PartialEq)]
    struct Nested {
        value: Option<Option<bool>>,
    }

    #[test]
    fn nested_option_bare_value() {
        // Bare value goes through both Option layers
        let n: Nested = Nested::from_ron("(value: true)").unwrap();
        assert_eq!(n.value, Some(Some(true)));
    }

    #[test]
    fn nested_option_explicit_outer() {
        let n: Nested = Nested::from_ron("(value: Some(Some(true)))").unwrap();
        assert_eq!(n.value, Some(Some(true)));
    }

    #[test]
    fn nested_option_some_none() {
        let n: Nested = Nested::from_ron("(value: Some(None))").unwrap();
        assert_eq!(n.value, Some(None));
    }

    #[test]
    fn nested_option_outer_none() {
        let n: Nested = Nested::from_ron("(value: None)").unwrap();
        assert_eq!(n.value, None);
    }
}

// =============================================================================
// Explicit tests
// =============================================================================

mod explicit {
    use super::*;

    #[derive(Debug, Ron, PartialEq)]
    struct StrictConfig {
        #[ron(explicit)]
        value: Option<i32>,
    }

    #[test]
    fn explicit_accepts_some() {
        let cfg: StrictConfig = StrictConfig::from_ron("(value: Some(42))").unwrap();
        assert_eq!(cfg.value, Some(42));
    }

    #[test]
    fn explicit_accepts_none() {
        let cfg: StrictConfig = StrictConfig::from_ron("(value: None)").unwrap();
        assert_eq!(cfg.value, None);
    }

    #[test]
    fn explicit_rejects_bare_value() {
        let result = StrictConfig::from_ron("(value: 42)");
        assert!(result.is_err());
        let err = result.unwrap_err();
        // Check error message mentions expected Some/None
        assert!(
            err.to_string().contains("Some") || err.to_string().contains("None"),
            "Error should mention Some/None: {}",
            err
        );
    }

    /// Option<Option<T>> with explicit outer, implicit inner.
    #[derive(Debug, Ron, PartialEq)]
    struct ExplicitOuter {
        #[ron(explicit)]
        value: Option<Option<String>>,
    }

    #[test]
    fn explicit_outer_some_bare_inner() {
        // Outer requires Some(...), inner can be bare
        let cfg: ExplicitOuter = ExplicitOuter::from_ron(r#"(value: Some("hello"))"#).unwrap();
        assert_eq!(cfg.value, Some(Some("hello".to_string())));
    }

    #[test]
    fn explicit_outer_some_some_inner() {
        let cfg: ExplicitOuter =
            ExplicitOuter::from_ron(r#"(value: Some(Some("world")))"#).unwrap();
        assert_eq!(cfg.value, Some(Some("world".to_string())));
    }

    #[test]
    fn explicit_outer_some_none_inner() {
        let cfg: ExplicitOuter = ExplicitOuter::from_ron("(value: Some(None))").unwrap();
        assert_eq!(cfg.value, Some(None));
    }

    #[test]
    fn explicit_outer_none() {
        let cfg: ExplicitOuter = ExplicitOuter::from_ron("(value: None)").unwrap();
        assert_eq!(cfg.value, None);
    }

    #[test]
    fn explicit_outer_rejects_bare() {
        // Bare string should fail because outer Option requires explicit
        let result = ExplicitOuter::from_ron(r#"(value: "fail")"#);
        assert!(result.is_err());
    }

    /// Explicit with default - missing field becomes None.
    #[derive(Debug, Ron, PartialEq)]
    struct ExplicitWithDefault {
        name: String,
        #[ron(explicit, default)]
        maybe: Option<i32>,
    }

    #[test]
    fn explicit_default_missing_is_none() {
        let cfg: ExplicitWithDefault = ExplicitWithDefault::from_ron(r#"(name: "test")"#).unwrap();
        assert_eq!(cfg.name, "test");
        assert_eq!(cfg.maybe, None);
    }

    #[test]
    fn explicit_default_present_some() {
        let cfg: ExplicitWithDefault =
            ExplicitWithDefault::from_ron(r#"(name: "test", maybe: Some(5))"#).unwrap();
        assert_eq!(cfg.maybe, Some(5));
    }

    #[test]
    fn explicit_default_present_none() {
        let cfg: ExplicitWithDefault =
            ExplicitWithDefault::from_ron(r#"(name: "test", maybe: None)"#).unwrap();
        assert_eq!(cfg.maybe, None);
    }

    #[test]
    fn explicit_default_rejects_bare() {
        let result = ExplicitWithDefault::from_ron(r#"(name: "test", maybe: 10)"#);
        assert!(result.is_err());
    }
}

// =============================================================================
// Combination tests
// =============================================================================

mod combinations {
    use super::*;

    /// Transparent struct containing Option field.
    #[derive(Debug, Ron, PartialEq)]
    #[ron(transparent)]
    struct TransparentWithOption {
        value: Option<String>,
    }

    #[test]
    fn transparent_with_option_bare() {
        let t: TransparentWithOption = TransparentWithOption::from_ron(r#""hello""#).unwrap();
        assert_eq!(t.value, Some("hello".to_string()));
    }

    #[test]
    fn transparent_with_option_some() {
        let t: TransparentWithOption = TransparentWithOption::from_ron(r#"Some("world")"#).unwrap();
        assert_eq!(t.value, Some("world".to_string()));
    }

    #[test]
    fn transparent_with_option_none() {
        let t: TransparentWithOption = TransparentWithOption::from_ron("None").unwrap();
        assert_eq!(t.value, None);
    }

    /// Transparent newtype used in a struct field.
    #[derive(Debug, Ron, PartialEq)]
    #[ron(transparent)]
    struct UserId(u64);

    #[derive(Debug, Ron, PartialEq)]
    struct User {
        id: UserId,
        name: String,
    }

    #[test]
    fn transparent_in_struct_field() {
        // UserId field should accept raw u64
        let user: User = User::from_ron(r#"(id: 42, name: "Alice")"#).unwrap();
        assert_eq!(user.id.0, 42);
        assert_eq!(user.name, "Alice");
    }

    #[test]
    fn transparent_in_struct_serialize() {
        let user = User {
            id: UserId(100),
            name: "Bob".to_string(),
        };
        let ron = user.to_ron().unwrap();
        // id should be serialized as just the number
        assert!(ron.contains("id:100"), "Expected id:100 in {}", ron);
    }

    /// Enum variant with Option field.
    #[derive(Debug, Ron, PartialEq)]
    enum Event {
        Login {
            user: String,
            session: Option<String>,
        },
        Logout,
    }

    #[test]
    fn enum_variant_implicit_some() {
        let e: Event = Event::from_ron(r#"Login(user: "alice", session: "abc123")"#).unwrap();
        match e {
            Event::Login { user, session } => {
                assert_eq!(user, "alice");
                assert_eq!(session, Some("abc123".to_string()));
            }
            _ => panic!("Expected Login"),
        }
    }

    #[test]
    fn enum_variant_none() {
        let e: Event = Event::from_ron(r#"Login(user: "bob", session: None)"#).unwrap();
        match e {
            Event::Login { user, session } => {
                assert_eq!(user, "bob");
                assert_eq!(session, None);
            }
            _ => panic!("Expected Login"),
        }
    }

    /// Enum variant with explicit Option field.
    #[derive(Debug, Ron, PartialEq)]
    enum StrictEvent {
        Action {
            name: String,
            #[ron(explicit)]
            metadata: Option<String>,
        },
    }

    #[test]
    fn enum_explicit_accepts_some() {
        let e: StrictEvent =
            StrictEvent::from_ron(r#"Action(name: "click", metadata: Some("data"))"#).unwrap();
        match e {
            StrictEvent::Action { name, metadata } => {
                assert_eq!(name, "click");
                assert_eq!(metadata, Some("data".to_string()));
            }
        }
    }

    #[test]
    fn enum_explicit_accepts_none() {
        let e: StrictEvent =
            StrictEvent::from_ron(r#"Action(name: "hover", metadata: None)"#).unwrap();
        match e {
            StrictEvent::Action { name, metadata } => {
                assert_eq!(name, "hover");
                assert_eq!(metadata, None);
            }
        }
    }

    #[test]
    fn enum_explicit_rejects_bare() {
        let result = StrictEvent::from_ron(r#"Action(name: "fail", metadata: "bare")"#);
        assert!(result.is_err());
    }
}

// =============================================================================
// Edge case tests
// =============================================================================

mod edge_cases {
    use super::*;

    /// Transparent newtype inside enum variant.
    #[derive(Debug, Ron, PartialEq)]
    #[ron(transparent)]
    struct Amount(f64);

    #[derive(Debug, Ron, PartialEq)]
    enum Transaction {
        Deposit(Amount),
        Withdraw(Amount),
    }

    #[test]
    fn transparent_newtype_in_enum_variant() {
        let t: Transaction = Transaction::from_ron("Deposit(100.50)").unwrap();
        match t {
            Transaction::Deposit(amt) => assert!((amt.0 - 100.50).abs() < 0.001),
            _ => panic!("Expected Deposit"),
        }
    }

    #[test]
    fn transparent_newtype_in_enum_serialize() {
        let t = Transaction::Withdraw(Amount(50.0));
        let ron = t.to_ron().unwrap();
        // Amount should be transparent
        assert!(ron.contains("50"), "Expected 50 in {}", ron);
    }

    /// Error span accuracy - verify errors point to correct location.
    #[derive(Debug, Ron, PartialEq)]
    struct SpanTest {
        first: String,
        #[ron(explicit)]
        second: Option<i32>,
        third: String,
    }

    #[test]
    fn error_span_for_explicit_field() {
        let ron = r#"(
            first: "ok",
            second: 42,
            third: "also ok"
        )"#;
        let result = SpanTest::from_ron(ron);
        assert!(result.is_err());
        let err = result.unwrap_err();
        // Error should mention the field that failed
        let err_str = format!("{:?}", err);
        // The span should point to line 3 where "second: 42" is
        assert!(
            err_str.contains("line") || err_str.contains("2:") || err_str.contains("3:"),
            "Error should include location info: {}",
            err_str
        );
    }

    /// Complex nested type with explicit.
    #[derive(Debug, Ron, PartialEq)]
    struct Complex {
        #[ron(explicit)]
        outer: Option<Vec<Option<i32>>>,
    }

    #[test]
    fn complex_explicit_some_vec() {
        // Outer Option is explicit, inner Options in Vec are implicit
        let c: Complex = Complex::from_ron("(outer: Some([1, 2, None, 4]))").unwrap();
        assert_eq!(c.outer, Some(vec![Some(1), Some(2), None, Some(4)]));
    }

    #[test]
    fn complex_explicit_none() {
        let c: Complex = Complex::from_ron("(outer: None)").unwrap();
        assert_eq!(c.outer, None);
    }

    #[test]
    fn complex_explicit_rejects_bare_vec() {
        let result = Complex::from_ron("(outer: [1, 2, 3])");
        assert!(result.is_err());
    }
}
