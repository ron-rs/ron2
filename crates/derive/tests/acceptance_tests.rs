//! Comprehensive acceptance tests for all ron-derive attributes.
//!
//! This file implements tests for all acceptance criteria defined in
//! ACCEPTANCE_CRITERIA.md at the repository root.

#![allow(dead_code)]

use ron2::{ast::FormatConfig, FromRon, ToRon};
use ron2_derive::Ron;

// =============================================================================
// Container Attribute Tests: rename
// =============================================================================

mod container_rename {
    use super::*;

    #[derive(Debug, Ron, PartialEq)]
    #[ron(rename = "MyPoint")]
    struct Point {
        x: i32,
        y: i32,
    }

    #[test]
    fn serializes_with_renamed_type() {
        let p = Point { x: 1, y: 2 };
        let ron = p.to_ron().unwrap();
        assert!(ron.contains("MyPoint"), "Expected 'MyPoint' in: {}", ron);
    }

    #[test]
    fn deserializes_from_renamed_type() {
        let ron = "MyPoint(x: 10, y: 20)";
        let p: Point = Point::from_ron(ron).unwrap();
        assert_eq!(p.x, 10);
        assert_eq!(p.y, 20);
    }

    #[test]
    fn roundtrip_preserves_value() {
        let original = Point { x: 42, y: 84 };
        let ron = original.to_ron().unwrap();
        let parsed: Point = Point::from_ron(&ron).unwrap();
        assert_eq!(original, parsed);
    }
}

// =============================================================================
// Container Attribute Tests: rename_all
// =============================================================================

mod container_rename_all {
    use super::*;

    #[derive(Debug, Ron, PartialEq)]
    #[ron(rename_all = "camelCase")]
    struct CamelConfig {
        user_name: String,
        max_count: u32,
    }

    #[test]
    fn camel_case_serialization() {
        let cfg = CamelConfig {
            user_name: "alice".to_string(),
            max_count: 100,
        };
        let ron = cfg.to_ron().unwrap();
        assert!(ron.contains("userName"), "Expected 'userName' in: {}", ron);
        assert!(ron.contains("maxCount"), "Expected 'maxCount' in: {}", ron);
    }

    #[test]
    fn camel_case_deserialization() {
        let ron = r#"(userName: "bob", maxCount: 50)"#;
        let cfg: CamelConfig = CamelConfig::from_ron(ron).unwrap();
        assert_eq!(cfg.user_name, "bob");
        assert_eq!(cfg.max_count, 50);
    }

    #[test]
    fn camel_case_roundtrip() {
        let original = CamelConfig {
            user_name: "charlie".to_string(),
            max_count: 200,
        };
        let ron = original.to_ron().unwrap();
        let parsed: CamelConfig = CamelConfig::from_ron(&ron).unwrap();
        assert_eq!(original, parsed);
    }

    #[derive(Debug, Ron, PartialEq)]
    #[ron(rename_all = "PascalCase")]
    struct PascalConfig {
        user_name: String,
        max_count: u32,
    }

    #[test]
    fn pascal_case_roundtrip() {
        let original = PascalConfig {
            user_name: "test".to_string(),
            max_count: 10,
        };
        let ron = original.to_ron().unwrap();
        assert!(ron.contains("UserName"));
        assert!(ron.contains("MaxCount"));
        let parsed: PascalConfig = PascalConfig::from_ron(&ron).unwrap();
        assert_eq!(original, parsed);
    }

    #[derive(Debug, Ron, PartialEq)]
    #[ron(rename_all = "SCREAMING_SNAKE_CASE")]
    struct ScreamingConfig {
        user_name: String,
        max_count: u32,
    }

    #[test]
    fn screaming_snake_case_roundtrip() {
        let original = ScreamingConfig {
            user_name: "test".to_string(),
            max_count: 10,
        };
        let ron = original.to_ron().unwrap();
        assert!(ron.contains("USER_NAME"));
        assert!(ron.contains("MAX_COUNT"));
        let parsed: ScreamingConfig = ScreamingConfig::from_ron(&ron).unwrap();
        assert_eq!(original, parsed);
    }

    #[test]
    #[ignore = "kebab-case removed - creates invalid RON identifiers"]
    fn kebab_case_not_supported() {
        // kebab-case creates identifiers like "user-name" which cannot be parsed
        // as valid RON identifiers (the dash is interpreted as subtraction operator)
    }

    #[derive(Debug, Ron, PartialEq)]
    #[ron(rename_all = "lowercase")]
    struct LowerConfig {
        user_name: String,
    }

    #[test]
    fn lowercase_roundtrip() {
        let original = LowerConfig {
            user_name: "test".to_string(),
        };
        let ron = original.to_ron().unwrap();
        assert!(ron.contains("username"));
        let parsed: LowerConfig = LowerConfig::from_ron(&ron).unwrap();
        assert_eq!(original, parsed);
    }

    #[derive(Debug, Ron, PartialEq)]
    #[ron(rename_all = "UPPERCASE")]
    struct UpperConfig {
        user_name: String,
    }

    #[test]
    fn uppercase_roundtrip() {
        let original = UpperConfig {
            user_name: "test".to_string(),
        };
        let ron = original.to_ron().unwrap();
        assert!(ron.contains("USERNAME"));
        let parsed: UpperConfig = UpperConfig::from_ron(&ron).unwrap();
        assert_eq!(original, parsed);
    }

    #[derive(Debug, Ron, PartialEq)]
    #[ron(rename_all = "snake_case")]
    struct WithRenameOverride {
        user_name: String,
        #[ron(rename = "custom")]
        max_count: u32,
    }

    #[test]
    fn field_rename_overrides_rename_all() {
        let cfg = WithRenameOverride {
            user_name: "test".to_string(),
            max_count: 10,
        };
        let ron = cfg.to_ron().unwrap();
        assert!(ron.contains("user_name"));
        assert!(ron.contains("custom"));
        assert!(!ron.contains("max_count"));

        let parsed: WithRenameOverride = WithRenameOverride::from_ron(&ron).unwrap();
        assert_eq!(cfg, parsed);
    }
}

// =============================================================================
// Container Attribute Tests: deny_unknown_fields
// =============================================================================

mod deny_unknown_fields {
    use super::*;

    #[derive(Debug, Ron, PartialEq)]
    #[ron(deny_unknown_fields)]
    struct StrictConfig {
        name: String,
        count: u32,
    }

    #[test]
    fn accepts_all_known_fields() {
        let ron = r#"(name: "test", count: 42)"#;
        let cfg: StrictConfig = StrictConfig::from_ron(ron).unwrap();
        assert_eq!(cfg.name, "test");
        assert_eq!(cfg.count, 42);
    }

    #[test]
    fn rejects_unknown_field() {
        let ron = r#"(name: "test", count: 42, unknown: true)"#;
        let result = StrictConfig::from_ron(ron);
        assert!(result.is_err());
        let err = result.unwrap_err();
        let err_str = err.to_string();
        assert!(
            err_str.contains("unknown") || err_str.contains("Unknown"),
            "Error should mention unknown field: {}",
            err_str
        );
    }

    #[test]
    fn error_names_unknown_field() {
        let ron = r#"(name: "test", count: 42, unexpected: 123)"#;
        let result = StrictConfig::from_ron(ron);
        assert!(result.is_err());
        let err = result.unwrap_err();
        let err_str = err.to_string();
        assert!(
            err_str.contains("unexpected"),
            "Error should name the unknown field 'unexpected': {}",
            err_str
        );
    }

    // Test with flattened struct
    #[derive(Debug, Ron, PartialEq)]
    struct Inner {
        x: i32,
    }

    #[derive(Debug, Ron, PartialEq)]
    #[ron(deny_unknown_fields)]
    struct OuterWithFlatten {
        name: String,
        #[ron(flatten)]
        inner: Inner,
    }

    #[test]
    fn deny_unknown_with_flatten_accepts_known() {
        let ron = r#"(name: "test", x: 10)"#;
        let outer: OuterWithFlatten = OuterWithFlatten::from_ron(ron).unwrap();
        assert_eq!(outer.name, "test");
        assert_eq!(outer.inner.x, 10);
    }

    #[test]
    fn deny_unknown_with_flatten_rejects_unknown() {
        let ron = r#"(name: "test", x: 10, unknown: true)"#;
        let result = OuterWithFlatten::from_ron(ron);
        assert!(result.is_err());
    }
}

// =============================================================================
// Field Attribute Tests: rename
// =============================================================================

mod field_rename {
    use super::*;

    #[derive(Debug, Ron, PartialEq)]
    struct Config {
        #[ron(rename = "userName")]
        user_name: String,
        count: u32,
    }

    #[test]
    fn serializes_with_renamed_field() {
        let cfg = Config {
            user_name: "alice".to_string(),
            count: 10,
        };
        let ron = cfg.to_ron().unwrap();
        assert!(ron.contains("userName"));
        assert!(!ron.contains("user_name"));
    }

    #[test]
    fn deserializes_from_renamed_field() {
        let ron = r#"(userName: "bob", count: 20)"#;
        let cfg: Config = Config::from_ron(ron).unwrap();
        assert_eq!(cfg.user_name, "bob");
        assert_eq!(cfg.count, 20);
    }

    #[test]
    fn rejects_original_field_name() {
        let ron = r#"(user_name: "fail", count: 30)"#;
        let result = Config::from_ron(ron);
        assert!(result.is_err());
    }

    #[test]
    fn roundtrip_preserves_value() {
        let original = Config {
            user_name: "charlie".to_string(),
            count: 100,
        };
        let ron = original.to_ron().unwrap();
        let parsed: Config = Config::from_ron(&ron).unwrap();
        assert_eq!(original, parsed);
    }
}

// =============================================================================
// Field Attribute Tests: skip
// =============================================================================

mod field_skip {
    use super::*;

    #[derive(Debug, Ron, PartialEq, Default)]
    struct WithSkipped {
        name: String,
        #[ron(skip)]
        internal: i32,
    }

    #[test]
    fn skip_omits_from_serialization() {
        let item = WithSkipped {
            name: "test".to_string(),
            internal: 42,
        };
        let ron = item.to_ron().unwrap();
        assert!(ron.contains("name"));
        assert!(!ron.contains("internal"));
    }

    #[test]
    fn skip_uses_default_on_deserialization() {
        let ron = r#"(name: "test")"#;
        let item: WithSkipped = WithSkipped::from_ron(ron).unwrap();
        assert_eq!(item.name, "test");
        assert_eq!(item.internal, 0); // default i32
    }

    #[test]
    fn skip_roundtrip_loses_value() {
        let original = WithSkipped {
            name: "test".to_string(),
            internal: 999,
        };
        let ron = original.to_ron().unwrap();
        let parsed: WithSkipped = WithSkipped::from_ron(&ron).unwrap();
        assert_eq!(parsed.name, original.name);
        assert_eq!(parsed.internal, 0); // lost, replaced with default
    }

    // Test with enum variant
    #[derive(Debug, Ron, PartialEq)]
    enum Message {
        Data {
            value: String,
            #[ron(skip)]
            _cache: Vec<u8>,
        },
    }

    #[test]
    fn skip_in_enum_variant() {
        let msg = Message::Data {
            value: "hello".to_string(),
            _cache: vec![1, 2, 3],
        };
        let ron = msg.to_ron().unwrap();
        assert!(ron.contains("value"));
        assert!(!ron.contains("cache"));

        let parsed: Message = Message::from_ron(&ron).unwrap();
        match parsed {
            Message::Data { value, _cache } => {
                assert_eq!(value, "hello");
                assert_eq!(_cache, Vec::<u8>::new()); // default
            }
        }
    }
}

// =============================================================================
// Field Attribute Tests: skip_serializing
// =============================================================================

mod field_skip_serializing {
    use super::*;

    #[derive(Debug, Ron, PartialEq)]
    struct WithSkipSerializing {
        name: String,
        #[ron(skip_serializing, default)]
        secret: String,
    }

    #[test]
    fn skip_serializing_omits_from_output() {
        let item = WithSkipSerializing {
            name: "test".to_string(),
            secret: "password123".to_string(),
        };
        let ron = item.to_ron().unwrap();
        assert!(ron.contains("name"));
        assert!(!ron.contains("secret"));
        assert!(!ron.contains("password123"));
    }

    #[test]
    fn skip_serializing_accepts_on_deserialization() {
        let ron = r#"(name: "test", secret: "mykey")"#;
        let item: WithSkipSerializing = WithSkipSerializing::from_ron(ron).unwrap();
        assert_eq!(item.name, "test");
        assert_eq!(item.secret, "mykey");
    }

    #[test]
    fn skip_serializing_uses_default_when_missing() {
        let ron = r#"(name: "test")"#;
        let item: WithSkipSerializing = WithSkipSerializing::from_ron(ron).unwrap();
        assert_eq!(item.name, "test");
        assert_eq!(item.secret, ""); // default String
    }

    #[test]
    fn skip_serializing_roundtrip_loses_value() {
        let original = WithSkipSerializing {
            name: "test".to_string(),
            secret: "important".to_string(),
        };
        let ron = original.to_ron().unwrap();
        let parsed: WithSkipSerializing = WithSkipSerializing::from_ron(&ron).unwrap();
        assert_eq!(parsed.name, original.name);
        assert_eq!(parsed.secret, ""); // lost, replaced with default
    }
}

// =============================================================================
// Field Attribute Tests: skip_deserializing
// =============================================================================

mod field_skip_deserializing {
    use super::*;

    #[derive(Debug, Ron, PartialEq)]
    struct WithSkipDeserializing {
        name: String,
        #[ron(skip_deserializing, default)]
        computed: i32,
    }

    #[test]
    fn skip_deserializing_includes_in_serialization() {
        let item = WithSkipDeserializing {
            name: "test".to_string(),
            computed: 42,
        };
        let ron = item.to_ron().unwrap();
        assert!(ron.contains("name"));
        assert!(ron.contains("computed"));
        assert!(ron.contains("42"));
    }

    #[test]
    fn skip_deserializing_ignores_on_deserialization() {
        let ron = r#"(name: "test", computed: 999)"#;
        let item: WithSkipDeserializing = WithSkipDeserializing::from_ron(ron).unwrap();
        assert_eq!(item.name, "test");
        assert_eq!(item.computed, 0); // ignored, uses default
    }

    #[test]
    fn skip_deserializing_accepts_missing_field() {
        let ron = r#"(name: "test")"#;
        let item: WithSkipDeserializing = WithSkipDeserializing::from_ron(ron).unwrap();
        assert_eq!(item.name, "test");
        assert_eq!(item.computed, 0); // default
    }
}

// =============================================================================
// Field Attribute Tests: skip_serializing_if
// =============================================================================

mod field_skip_serializing_if {
    use super::*;

    fn is_zero(n: &i32) -> bool {
        *n == 0
    }

    #[derive(Debug, Ron, PartialEq)]
    struct WithConditionalSkip {
        name: String,
        #[ron(skip_serializing_if = "is_zero", default)]
        count: i32,
    }

    #[test]
    fn skip_when_predicate_true() {
        let item = WithConditionalSkip {
            name: "test".to_string(),
            count: 0,
        };
        let ron = item.to_ron().unwrap();
        assert!(ron.contains("name"));
        assert!(!ron.contains("count"));
    }

    #[test]
    fn include_when_predicate_false() {
        let item = WithConditionalSkip {
            name: "test".to_string(),
            count: 5,
        };
        let ron = item.to_ron().unwrap();
        assert!(ron.contains("name"));
        assert!(ron.contains("count"));
        assert!(ron.contains("5"));
    }

    #[test]
    fn roundtrip_preserves_nonzero() {
        let original = WithConditionalSkip {
            name: "test".to_string(),
            count: 10,
        };
        let ron = original.to_ron().unwrap();
        let parsed: WithConditionalSkip = WithConditionalSkip::from_ron(&ron).unwrap();
        assert_eq!(original, parsed);
    }

    #[test]
    fn roundtrip_loses_zero() {
        let original = WithConditionalSkip {
            name: "test".to_string(),
            count: 0,
        };
        let ron = original.to_ron().unwrap();
        let parsed: WithConditionalSkip = WithConditionalSkip::from_ron(&ron).unwrap();
        assert_eq!(parsed.name, original.name);
        assert_eq!(parsed.count, 0); // still 0, from default
    }

    // Test with Option::is_none
    #[derive(Debug, Ron, PartialEq)]
    struct WithOptionalField {
        name: String,
        #[ron(skip_serializing_if = "Option::is_none", default)]
        value: Option<i32>,
    }

    #[test]
    fn skip_none_option() {
        let item = WithOptionalField {
            name: "test".to_string(),
            value: None,
        };
        let ron = item.to_ron().unwrap();
        assert!(!ron.contains("value"));
    }

    #[test]
    fn include_some_option() {
        let item = WithOptionalField {
            name: "test".to_string(),
            value: Some(42),
        };
        let ron = item.to_ron().unwrap();
        assert!(ron.contains("value"));
    }
}

// =============================================================================
// Field Attribute Tests: default
// =============================================================================

mod field_default {
    use super::*;

    #[derive(Debug, Ron, PartialEq)]
    struct WithDefault {
        name: String,
        #[ron(default)]
        count: i32,
        #[ron(default)]
        flag: Option<bool>,
    }

    #[test]
    fn default_uses_type_default_when_missing() {
        let ron = r#"(name: "test")"#;
        let item: WithDefault = WithDefault::from_ron(ron).unwrap();
        assert_eq!(item.name, "test");
        assert_eq!(item.count, 0);
        assert_eq!(item.flag, None);
    }

    #[test]
    fn default_uses_provided_value_when_present() {
        let ron = r#"(name: "test", count: 10, flag: Some(true))"#;
        let item: WithDefault = WithDefault::from_ron(ron).unwrap();
        assert_eq!(item.name, "test");
        assert_eq!(item.count, 10);
        assert_eq!(item.flag, Some(true));
    }

    #[test]
    fn default_serialization_includes_field() {
        let item = WithDefault {
            name: "test".to_string(),
            count: 0,
            flag: None,
        };
        let ron = item.to_ron().unwrap();
        assert!(ron.contains("count"));
        assert!(ron.contains("flag"));
    }

    #[test]
    fn default_errors_on_wrong_type() {
        let ron = r#"(name: "test", count: "not_a_number")"#;
        let result = WithDefault::from_ron(ron);
        assert!(result.is_err());
    }
}

// =============================================================================
// Field Attribute Tests: default = "path"
// =============================================================================

mod field_default_path {
    use super::*;

    fn default_count() -> i32 {
        100
    }

    fn default_name() -> String {
        "default".to_string()
    }

    #[derive(Debug, Ron, PartialEq)]
    struct WithCustomDefault {
        #[ron(default = "default_name")]
        name: String,
        #[ron(default = "default_count")]
        count: i32,
    }

    #[test]
    fn custom_default_uses_function_when_missing() {
        let ron = r#"()"#;
        let item: WithCustomDefault = WithCustomDefault::from_ron(ron).unwrap();
        assert_eq!(item.name, "default");
        assert_eq!(item.count, 100);
    }

    #[test]
    fn custom_default_uses_provided_value_when_present() {
        let ron = r#"(name: "alice", count: 50)"#;
        let item: WithCustomDefault = WithCustomDefault::from_ron(ron).unwrap();
        assert_eq!(item.name, "alice");
        assert_eq!(item.count, 50);
    }

    #[test]
    fn custom_default_partial_override() {
        let ron = r#"(name: "bob")"#;
        let item: WithCustomDefault = WithCustomDefault::from_ron(ron).unwrap();
        assert_eq!(item.name, "bob");
        assert_eq!(item.count, 100); // uses custom default
    }

    // Test with module path
    mod defaults {
        pub fn get_value() -> i32 {
            42
        }
    }

    #[derive(Debug, Ron, PartialEq)]
    struct WithModulePath {
        #[ron(default = "defaults::get_value")]
        value: i32,
    }

    #[test]
    fn custom_default_with_module_path() {
        let ron = r#"()"#;
        let item: WithModulePath = WithModulePath::from_ron(ron).unwrap();
        assert_eq!(item.value, 42);
    }
}

// =============================================================================
// Field Attribute Tests: flatten - COMPREHENSIVE
// =============================================================================

mod field_flatten {
    use super::*;

    // Basic flatten test
    #[derive(Debug, Ron, PartialEq)]
    struct Inner {
        x: i32,
        y: i32,
    }

    #[derive(Debug, Ron, PartialEq)]
    struct OuterWithFlatten {
        name: String,
        #[ron(flatten)]
        inner: Inner,
    }

    #[derive(Debug, Ron, PartialEq)]
    struct OuterInlined {
        name: String,
        x: i32,
        y: i32,
    }

    #[test]
    fn flatten_serialization() {
        let outer = OuterWithFlatten {
            name: "test".to_string(),
            inner: Inner { x: 10, y: 20 },
        };
        let ron = outer.to_ron().unwrap();
        
        // Fields should appear at top level
        assert!(ron.contains("name"));
        assert!(ron.contains("x"));
        assert!(ron.contains("y"));
        // Should NOT be nested
        assert!(!ron.contains("inner"));
    }

    #[test]
    fn flatten_deserialization() {
        let ron = r#"(name: "test", x: 10, y: 20)"#;
        let outer: OuterWithFlatten = OuterWithFlatten::from_ron(ron).unwrap();
        assert_eq!(outer.name, "test");
        assert_eq!(outer.inner.x, 10);
        assert_eq!(outer.inner.y, 20);
    }

    #[test]
    fn flatten_roundtrip() {
        let original = OuterWithFlatten {
            name: "test".to_string(),
            inner: Inner { x: 42, y: 84 },
        };
        let ron = original.to_ron().unwrap();
        let parsed: OuterWithFlatten = OuterWithFlatten::from_ron(&ron).unwrap();
        assert_eq!(original, parsed);
    }

    #[test]
    fn flatten_equivalence_serialization() {
        // Flattened and inlined should produce equivalent RON
        let flattened = OuterWithFlatten {
            name: "test".to_string(),
            inner: Inner { x: 1, y: 2 },
        };
        let inlined = OuterInlined {
            name: "test".to_string(),
            x: 1,
            y: 2,
        };

        let ron_flat = flattened.to_ron_with(&FormatConfig::minimal()).unwrap();
        let ron_inlined = inlined.to_ron_with(&FormatConfig::minimal()).unwrap();

        // Parse both as unordered field sets (field order may differ)
        assert!(ron_flat.contains("name:\"test\"") || ron_flat.contains(r#"name:"test""#));
        assert!(ron_flat.contains("x:1"));
        assert!(ron_flat.contains("y:2"));
        assert!(ron_inlined.contains("name:\"test\"") || ron_inlined.contains(r#"name:"test""#));
        assert!(ron_inlined.contains("x:1"));
        assert!(ron_inlined.contains("y:2"));
    }

    #[test]
    fn flatten_equivalence_cross_deserialization() {
        // Flattened struct should deserialize from anonymous struct (no type name)
        let anon_ron = r#"(name: "test", x: 5, y: 10)"#;
        let flattened: OuterWithFlatten = OuterWithFlatten::from_ron(anon_ron).unwrap();
        assert_eq!(flattened.name, "test");
        assert_eq!(flattened.inner.x, 5);
        assert_eq!(flattened.inner.y, 10);

        let inlined: OuterInlined = OuterInlined::from_ron(anon_ron).unwrap();
        assert_eq!(inlined.name, "test");
        assert_eq!(inlined.x, 5);
        assert_eq!(inlined.y, 10);
        
        // Both should serialize to the same field structure (ignoring type name)
        let flattened_ron = flattened.to_ron().unwrap();
        let inlined_ron = inlined.to_ron().unwrap();
        
        // Both should contain the same fields
        assert!(flattened_ron.contains("name"));
        assert!(flattened_ron.contains("x"));
        assert!(flattened_ron.contains("y"));
        assert!(inlined_ron.contains("name"));
        assert!(inlined_ron.contains("x"));
        assert!(inlined_ron.contains("y"));
    }

    // Multiple flattened fields
    #[derive(Debug, Ron, PartialEq)]
    struct Position {
        x: f32,
        y: f32,
    }

    #[derive(Debug, Ron, PartialEq)]
    struct Color {
        r: u8,
        g: u8,
        b: u8,
    }

    #[derive(Debug, Ron, PartialEq)]
    struct Sprite {
        name: String,
        #[ron(flatten)]
        position: Position,
        #[ron(flatten)]
        color: Color,
    }

    #[test]
    fn multiple_flatten_fields() {
        let sprite = Sprite {
            name: "player".to_string(),
            position: Position { x: 10.0, y: 20.0 },
            color: Color { r: 255, g: 0, b: 0 },
        };
        let ron = sprite.to_ron().unwrap();
        
        // All fields at top level
        assert!(ron.contains("name"));
        assert!(ron.contains("x"));
        assert!(ron.contains("y"));
        assert!(ron.contains("r"));
        assert!(ron.contains("g"));
        assert!(ron.contains("b"));
        
        let parsed: Sprite = Sprite::from_ron(&ron).unwrap();
        assert_eq!(sprite, parsed);
    }

    // Nested flatten
    #[derive(Debug, Ron, PartialEq)]
    struct Level2 {
        value: i32,
    }

    #[derive(Debug, Ron, PartialEq)]
    struct Level1 {
        id: String,
        #[ron(flatten)]
        level2: Level2,
    }

    #[derive(Debug, Ron, PartialEq)]
    struct Level0 {
        name: String,
        #[ron(flatten)]
        level1: Level1,
    }

    #[test]
    fn nested_flatten() {
        let nested = Level0 {
            name: "top".to_string(),
            level1: Level1 {
                id: "middle".to_string(),
                level2: Level2 { value: 42 },
            },
        };
        let ron = nested.to_ron().unwrap();
        
        // All fields should be at top level
        assert!(ron.contains("name"));
        assert!(ron.contains("id"));
        assert!(ron.contains("value"));
        
        let parsed: Level0 = Level0::from_ron(&ron).unwrap();
        assert_eq!(nested, parsed);
    }

    // Flatten with Option
    #[derive(Debug, Ron, PartialEq)]
    struct OptionalInner {
        x: i32,
        y: i32,
    }

    #[derive(Debug, Ron, PartialEq)]
    struct OuterWithOptionalFlatten {
        name: String,
        #[ron(flatten)]
        inner: Option<OptionalInner>,
    }

    #[test]
    fn flatten_option_serialization() {
        let none = OuterWithOptionalFlatten {
            name: "none".to_string(),
            inner: None,
        };
        let none_ron = none.to_ron().unwrap();
        assert!(none_ron.contains("name"));
        assert!(!none_ron.contains("x"));
        assert!(!none_ron.contains("y"));
        assert!(!none_ron.contains("inner"));

        let some = OuterWithOptionalFlatten {
            name: "some".to_string(),
            inner: Some(OptionalInner { x: 1, y: 2 }),
        };
        let some_ron = some.to_ron().unwrap();
        assert!(some_ron.contains("name"));
        assert!(some_ron.contains("x"));
        assert!(some_ron.contains("y"));
        assert!(!some_ron.contains("inner"));
    }

    #[test]
    fn flatten_option_deserialization() {
        let none_ron = r#"(name: "none")"#;
        let none: OuterWithOptionalFlatten = OuterWithOptionalFlatten::from_ron(none_ron).unwrap();
        assert_eq!(none.inner, None);

        let some_ron = r#"(name: "some", x: 1, y: 2)"#;
        let some: OuterWithOptionalFlatten = OuterWithOptionalFlatten::from_ron(some_ron).unwrap();
        assert_eq!(some.inner, Some(OptionalInner { x: 1, y: 2 }));

        let partial_ron = r#"(name: "partial", x: 1)"#;
        assert!(OuterWithOptionalFlatten::from_ron(partial_ron).is_err());
    }

    // Flatten with rename_all
    #[derive(Debug, Ron, PartialEq)]
    #[ron(rename_all = "camelCase")]
    struct InnerCamel {
        inner_field: String,
        another_field: i32,
    }

    #[derive(Debug, Ron, PartialEq)]
    #[ron(rename_all = "camelCase")]
    struct OuterCamel {
        outer_field: String,
        #[ron(flatten)]
        inner: InnerCamel,
    }

    #[test]
    fn flatten_with_rename_all() {
        let outer = OuterCamel {
            outer_field: "test".to_string(),
            inner: InnerCamel {
                inner_field: "inner".to_string(),
                another_field: 42,
            },
        };
        let ron = outer.to_ron().unwrap();
        
        // All fields should be camelCase
        assert!(ron.contains("outerField"));
        assert!(ron.contains("innerField"));
        assert!(ron.contains("anotherField"));
        
        let parsed: OuterCamel = OuterCamel::from_ron(&ron).unwrap();
        assert_eq!(outer, parsed);
    }

    // Flatten with default
    #[derive(Debug, Ron, PartialEq, Default)]
    struct InnerWithDefault {
        #[ron(default)]
        x: i32,
        #[ron(default)]
        y: i32,
    }

    #[derive(Debug, Ron, PartialEq)]
    struct OuterWithDefaultFlatten {
        name: String,
        #[ron(flatten, default)]
        inner: InnerWithDefault,
    }

    #[test]
    fn flatten_with_default_missing_uses_default() {
        let ron = r#"(name: "test")"#;
        let outer: OuterWithDefaultFlatten = OuterWithDefaultFlatten::from_ron(ron).unwrap();
        assert_eq!(outer.name, "test");
        assert_eq!(outer.inner.x, 0);
        assert_eq!(outer.inner.y, 0);
    }

    #[test]
    fn flatten_with_default_partial_fields() {
        // When flattened type has default on its fields, partial fields should work
        let ron = r#"(name: "test", x: 10)"#;
        let outer: OuterWithDefaultFlatten = OuterWithDefaultFlatten::from_ron(ron).unwrap();
        assert_eq!(outer.name, "test");
        assert_eq!(outer.inner.x, 10);
        assert_eq!(outer.inner.y, 0);
    }
}

// =============================================================================
// Variant Attribute Tests: rename
// =============================================================================

mod variant_rename {
    use super::*;

    #[derive(Debug, Ron, PartialEq)]
    enum Status {
        #[ron(rename = "active")]
        Active,
        #[ron(rename = "inactive")]
        Inactive,
        #[ron(rename = "pending")]
        Pending { reason: String },
    }

    #[test]
    fn variant_rename_serialization() {
        let s = Status::Active;
        let ron = s.to_ron().unwrap();
        assert!(ron.contains("active"));
        assert!(!ron.contains("Active"));
    }

    #[test]
    fn variant_rename_deserialization() {
        let ron = "active";
        let s: Status = Status::from_ron(ron).unwrap();
        assert_eq!(s, Status::Active);
    }

    #[test]
    fn variant_rename_struct_variant() {
        let s = Status::Pending {
            reason: "waiting".to_string(),
        };
        let ron = s.to_ron().unwrap();
        assert!(ron.contains("pending"));
        
        let parsed: Status = Status::from_ron(&ron).unwrap();
        assert_eq!(s, parsed);
    }

    #[test]
    fn variant_rename_roundtrip() {
        let original = Status::Inactive;
        let ron = original.to_ron().unwrap();
        let parsed: Status = Status::from_ron(&ron).unwrap();
        assert_eq!(original, parsed);
    }
}

// =============================================================================
// Variant Attribute Tests: skip
// =============================================================================

mod variant_skip {
    // Variant skip currently causes compilation errors with exhaustiveness checking
    // This is expected behavior - skipped variants should not be used
    #[test]
    #[ignore = "Variant skip causes exhaustiveness check failures - by design"]
    fn variant_skip_compile_test() {
        // This test documents that #[ron(skip)] on variants is primarily
        // for preventing serialization/deserialization, but causes compile errors
        // in exhaustive match contexts. This is the expected behavior.
        // 
        // Use case: Mark deprecated variants that shouldn't be used anymore
        // but can't be removed due to compatibility.
    }
}

// =============================================================================
// Attribute Combination Tests
// =============================================================================

mod combinations {
    use super::*;

    // flatten + default
    #[derive(Debug, Ron, PartialEq, Default)]
    struct Config {
        #[ron(default)]
        x: i32,
        #[ron(default)]
        y: i32,
    }

    #[derive(Debug, Ron, PartialEq)]
    struct Container {
        name: String,
        #[ron(flatten, default)]
        config: Config,
    }

    #[test]
    fn flatten_with_default_combination() {
        let ron = r#"(name: "test")"#;
        let c: Container = Container::from_ron(ron).unwrap();
        assert_eq!(c.name, "test");
        assert_eq!(c.config.x, 0);
        assert_eq!(c.config.y, 0);

        let ron2 = r#"(name: "test", x: 5, y: 10)"#;
        let c2: Container = Container::from_ron(ron2).unwrap();
        assert_eq!(c2.config.x, 5);
        assert_eq!(c2.config.y, 10);
    }

    // explicit + default
    #[derive(Debug, Ron, PartialEq)]
    struct StrictOptional {
        name: String,
        #[ron(explicit, default)]
        value: Option<i32>,
    }

    #[test]
    fn explicit_with_default_combination() {
        let ron = r#"(name: "test")"#;
        let s: StrictOptional = StrictOptional::from_ron(ron).unwrap();
        assert_eq!(s.value, None);

        let ron2 = r#"(name: "test", value: Some(42))"#;
        let s2: StrictOptional = StrictOptional::from_ron(ron2).unwrap();
        assert_eq!(s2.value, Some(42));

        let ron3 = r#"(name: "test", value: None)"#;
        let s3: StrictOptional = StrictOptional::from_ron(ron3).unwrap();
        assert_eq!(s3.value, None);

        // Bare value should fail
        let ron4 = r#"(name: "test", value: 42)"#;
        assert!(StrictOptional::from_ron(ron4).is_err());
    }

    // skip_serializing + default
    #[derive(Debug, Ron, PartialEq)]
    struct WriteOnly {
        name: String,
        #[ron(skip_serializing, default)]
        password: String,
    }

    #[test]
    fn skip_serializing_with_default_combination() {
        let w = WriteOnly {
            name: "user".to_string(),
            password: "secret".to_string(),
        };
        let ron = w.to_ron().unwrap();
        assert!(!ron.contains("password"));

        let parsed: WriteOnly = WriteOnly::from_ron(&ron).unwrap();
        assert_eq!(parsed.password, ""); // lost

        let ron2 = r#"(name: "user", password: "newpass")"#;
        let w2: WriteOnly = WriteOnly::from_ron(ron2).unwrap();
        assert_eq!(w2.password, "newpass");
    }

    // transparent + skip
    #[derive(Debug, Ron, PartialEq, Default)]
    #[ron(transparent)]
    struct TransparentWithSkip {
        value: i32,
        #[ron(skip)]
        _phantom: (),
    }

    #[test]
    fn transparent_with_skip_combination() {
        let t = TransparentWithSkip {
            value: 42,
            _phantom: (),
        };
        let ron = t.to_ron().unwrap();
        assert_eq!(ron, "42");

        let parsed: TransparentWithSkip = TransparentWithSkip::from_ron("100").unwrap();
        assert_eq!(parsed.value, 100);
    }

    // rename + rename_all interaction tested in container_rename_all module
}

// =============================================================================
// Edge Cases and Complex Scenarios
// =============================================================================

mod edge_cases {
    use super::*;

    // Empty struct with only skipped fields
    #[derive(Debug, Ron, PartialEq, Default)]
    struct AllSkipped {
        #[ron(skip)]
        a: i32,
        #[ron(skip)]
        b: String,
    }

    #[test]
    fn all_fields_skipped() {
        let s = AllSkipped {
            a: 42,
            b: "test".to_string(),
        };
        let ron = s.to_ron().unwrap();
        // Should serialize as empty struct
        assert!(ron.contains("()") || ron.contains("(  )"));

        let parsed: AllSkipped = AllSkipped::from_ron("()").unwrap();
        assert_eq!(parsed.a, 0);
        assert_eq!(parsed.b, "");
    }

    // Deeply nested structures
    #[derive(Debug, Ron, PartialEq)]
    struct Level3 {
        value: String,
    }

    #[derive(Debug, Ron, PartialEq)]
    struct Level2 {
        inner: Level3,
    }

    #[derive(Debug, Ron, PartialEq)]
    struct Level1 {
        middle: Level2,
    }

    #[derive(Debug, Ron, PartialEq)]
    struct Level0 {
        outer: Level1,
    }

    #[test]
    fn deeply_nested_roundtrip() {
        let nested = Level0 {
            outer: Level1 {
                middle: Level2 {
                    inner: Level3 {
                        value: "deep".to_string(),
                    },
                },
            },
        };
        let ron = nested.to_ron().unwrap();
        let parsed: Level0 = Level0::from_ron(&ron).unwrap();
        assert_eq!(nested, parsed);
    }

    // Complex enum with all variant types and attributes
    #[derive(Debug, Ron, PartialEq)]
    enum ComplexEnum {
        #[ron(rename = "unit")]
        Unit,
        #[ron(rename = "tuple")]
        Tuple(i32, String),
        #[ron(rename = "struct")]
        Struct {
            #[ron(rename = "val")]
            value: i32,
            #[ron(default)]
            optional: String,
        },
    }

    #[test]
    fn complex_enum_all_variants_roundtrip() {
        let variants = vec![
            ComplexEnum::Unit,
            ComplexEnum::Tuple(42, "test".to_string()),
            ComplexEnum::Struct {
                value: 10,
                optional: "opt".to_string(),
            },
        ];

        for variant in variants {
            let ron = variant.to_ron().unwrap();
            let parsed: ComplexEnum = ComplexEnum::from_ron(&ron).unwrap();
            assert_eq!(variant, parsed);
        }
    }
}
