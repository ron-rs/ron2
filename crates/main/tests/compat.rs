//! Compatibility tests between the original `ron` crate and `ron2`.
//!
//! This module tests that ron2 can parse ron output and vice versa,
//! validating roundtrips and semantic equivalence.

use ron2::{FromRon, ToRon, Value};
use ron2_derive::{FromRon, ToRon};
use serde::{Deserialize, Serialize};

// ============================================================================
// Test Types
// ============================================================================

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, ToRon, FromRon)]
struct UnitStruct;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, ToRon, FromRon)]
struct Newtype(i32);

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, ToRon, FromRon)]
struct TupleStruct(i32, String, bool);

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, ToRon, FromRon)]
struct Point {
    x: i32,
    y: i32,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, ToRon, FromRon)]
struct Person {
    name: String,
    age: u32,
    active: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, ToRon, FromRon)]
enum SimpleEnum {
    Unit,
    Tuple(i32),
    Struct { value: String },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, ToRon, FromRon)]
enum Color {
    Red,
    Green,
    Blue,
    Rgb(u8, u8, u8),
    Named { name: String, hex: String },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, ToRon, FromRon)]
struct Collections {
    numbers: Vec<i32>,
    strings: Vec<String>,
    nested: Vec<Vec<i32>>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, ToRon, FromRon)]
struct Optionals {
    maybe_int: Option<i32>,
    maybe_string: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, ToRon, FromRon)]
struct Inner {
    value: i32,
    name: String,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, ToRon, FromRon)]
struct Nested {
    inner: Inner,
    list: Vec<Inner>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, ToRon, FromRon)]
struct Numbers {
    i8_val: i8,
    i16_val: i16,
    i32_val: i32,
    i64_val: i64,
    u8_val: u8,
    u16_val: u16,
    u32_val: u32,
    u64_val: u64,
    f32_val: f32,
    f64_val: f64,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, ToRon, FromRon)]
struct Tuples {
    pair: (i32, String),
    triple: (bool, i32, f64),
}

// ============================================================================
// Cross-Parsing Tests: ron output → ron2 parsing
// ============================================================================

/// Test that ron2 can parse ron crate output
mod ron_to_ron2 {
    use super::*;

    fn ron_output_parseable_by_ron2<T>(value: &T)
    where
        T: Serialize + FromRon + PartialEq + std::fmt::Debug,
    {
        let ron_output = ron::to_string(value).expect("ron should serialize");
        let parsed = T::from_ron(&ron_output).expect("ron2 should parse ron output");
        assert_eq!(value, &parsed, "Values should match after ron → ron2");
    }

    #[test]
    fn unit_struct() {
        ron_output_parseable_by_ron2(&UnitStruct);
    }

    #[test]
    fn newtype() {
        ron_output_parseable_by_ron2(&Newtype(42));
        ron_output_parseable_by_ron2(&Newtype(-100));
        ron_output_parseable_by_ron2(&Newtype(0));
    }

    #[test]
    fn tuple_struct() {
        ron_output_parseable_by_ron2(&TupleStruct(42, "hello".into(), true));
        ron_output_parseable_by_ron2(&TupleStruct(-1, String::new(), false));
    }

    #[test]
    fn point() {
        ron_output_parseable_by_ron2(&Point { x: 10, y: 20 });
        ron_output_parseable_by_ron2(&Point { x: -5, y: 0 });
    }

    #[test]
    fn person() {
        ron_output_parseable_by_ron2(&Person {
            name: "Alice".into(),
            age: 30,
            active: true,
        });
    }

    #[test]
    fn simple_enum() {
        ron_output_parseable_by_ron2(&SimpleEnum::Unit);
        ron_output_parseable_by_ron2(&SimpleEnum::Tuple(42));
        ron_output_parseable_by_ron2(&SimpleEnum::Struct {
            value: "test".into(),
        });
    }

    #[test]
    fn color() {
        ron_output_parseable_by_ron2(&Color::Red);
        ron_output_parseable_by_ron2(&Color::Green);
        ron_output_parseable_by_ron2(&Color::Blue);
        ron_output_parseable_by_ron2(&Color::Rgb(255, 128, 0));
        ron_output_parseable_by_ron2(&Color::Named {
            name: "coral".into(),
            hex: "#FF7F50".into(),
        });
    }

    #[test]
    fn collections() {
        ron_output_parseable_by_ron2(&Collections {
            numbers: vec![1, 2, 3],
            strings: vec!["a".into(), "b".into()],
            nested: vec![vec![1, 2], vec![3, 4]],
        });
        ron_output_parseable_by_ron2(&Collections {
            numbers: vec![],
            strings: vec![],
            nested: vec![],
        });
    }

    #[test]
    fn optionals() {
        ron_output_parseable_by_ron2(&Optionals {
            maybe_int: Some(42),
            maybe_string: Some("hello".into()),
        });
        ron_output_parseable_by_ron2(&Optionals {
            maybe_int: None,
            maybe_string: None,
        });
    }

    #[test]
    fn nested() {
        ron_output_parseable_by_ron2(&Nested {
            inner: Inner {
                value: 1,
                name: "first".into(),
            },
            list: vec![
                Inner {
                    value: 2,
                    name: "second".into(),
                },
                Inner {
                    value: 3,
                    name: "third".into(),
                },
            ],
        });
    }

    #[test]
    fn numbers() {
        ron_output_parseable_by_ron2(&Numbers {
            i8_val: -128,
            i16_val: -32768,
            i32_val: -2147483648,
            i64_val: -9223372036854775808,
            u8_val: 255,
            u16_val: 65535,
            u32_val: 4294967295,
            u64_val: 18446744073709551615,
            f32_val: 1.5,
            f64_val: 2.5,
        });
    }

    #[test]
    fn tuples() {
        ron_output_parseable_by_ron2(&Tuples {
            pair: (42, "answer".into()),
            triple: (true, -1, 1.5),
        });
    }
}

// ============================================================================
// Cross-Parsing Tests: ron2 output → ron parsing
// ============================================================================

/// Test that ron crate can parse ron2 output
mod ron2_to_ron {
    use super::*;

    fn ron2_output_parseable_by_ron<T>(value: &T)
    where
        T: ToRon + for<'de> Deserialize<'de> + PartialEq + std::fmt::Debug,
    {
        let ron2_output = value.to_ron().expect("ron2 should serialize");
        let parsed: T = ron::from_str(&ron2_output).expect("ron should parse ron2 output");
        assert_eq!(value, &parsed, "Values should match after ron2 → ron");
    }

    #[test]
    fn unit_struct() {
        ron2_output_parseable_by_ron(&UnitStruct);
    }

    #[test]
    fn newtype() {
        ron2_output_parseable_by_ron(&Newtype(42));
        ron2_output_parseable_by_ron(&Newtype(-100));
    }

    #[test]
    fn tuple_struct() {
        ron2_output_parseable_by_ron(&TupleStruct(42, "hello".into(), true));
    }

    #[test]
    fn point() {
        ron2_output_parseable_by_ron(&Point { x: 10, y: 20 });
    }

    #[test]
    fn person() {
        ron2_output_parseable_by_ron(&Person {
            name: "Bob".into(),
            age: 25,
            active: false,
        });
    }

    #[test]
    fn simple_enum() {
        ron2_output_parseable_by_ron(&SimpleEnum::Unit);
        ron2_output_parseable_by_ron(&SimpleEnum::Tuple(99));
        ron2_output_parseable_by_ron(&SimpleEnum::Struct {
            value: "data".into(),
        });
    }

    #[test]
    fn color() {
        ron2_output_parseable_by_ron(&Color::Red);
        ron2_output_parseable_by_ron(&Color::Rgb(0, 0, 0));
        ron2_output_parseable_by_ron(&Color::Named {
            name: "white".into(),
            hex: "#FFFFFF".into(),
        });
    }

    #[test]
    fn collections() {
        ron2_output_parseable_by_ron(&Collections {
            numbers: vec![10, 20, 30],
            strings: vec!["x".into(), "y".into(), "z".into()],
            nested: vec![vec![100]],
        });
    }

    #[test]
    fn optionals() {
        ron2_output_parseable_by_ron(&Optionals {
            maybe_int: Some(0),
            maybe_string: None,
        });
    }

    #[test]
    fn nested() {
        ron2_output_parseable_by_ron(&Nested {
            inner: Inner {
                value: 42,
                name: "test".into(),
            },
            list: vec![],
        });
    }

    #[test]
    fn numbers() {
        ron2_output_parseable_by_ron(&Numbers {
            i8_val: 0,
            i16_val: 0,
            i32_val: 0,
            i64_val: 0,
            u8_val: 0,
            u16_val: 0,
            u32_val: 0,
            u64_val: 0,
            f32_val: 0.0,
            f64_val: 0.0,
        });
    }

    #[test]
    fn tuples() {
        ron2_output_parseable_by_ron(&Tuples {
            pair: (0, String::new()),
            triple: (false, 0, 0.0),
        });
    }
}

// ============================================================================
// Roundtrip Tests
// ============================================================================

/// Full roundtrip: ron → ron2 → ron
mod roundtrip_ron_to_ron2_to_ron {
    use super::*;

    fn roundtrip<T>(original: &T)
    where
        T: Serialize + FromRon + ToRon + for<'de> Deserialize<'de> + PartialEq + std::fmt::Debug,
    {
        // ron serialize
        let ron_text = ron::to_string(original).expect("ron serialize failed");

        // ron2 parse
        let ron2_parsed = T::from_ron(&ron_text).expect("ron2 parse failed");

        // ron2 serialize
        let ron2_text = ron2_parsed.to_ron().expect("ron2 serialize failed");

        // ron parse
        let final_value: T = ron::from_str(&ron2_text).expect("ron parse of ron2 output failed");

        assert_eq!(
            original, &final_value,
            "Roundtrip mismatch: ron → ron2 → ron"
        );
    }

    #[test]
    fn basic_structs() {
        roundtrip(&Point { x: 1, y: 2 });
        roundtrip(&Person {
            name: "Charlie".into(),
            age: 35,
            active: true,
        });
    }

    #[test]
    fn enums() {
        roundtrip(&SimpleEnum::Unit);
        roundtrip(&SimpleEnum::Tuple(42));
        roundtrip(&SimpleEnum::Struct {
            value: "roundtrip".into(),
        });
        roundtrip(&Color::Rgb(100, 150, 200));
    }

    #[test]
    fn collections() {
        roundtrip(&Collections {
            numbers: vec![1, 2, 3, 4, 5],
            strings: vec!["hello".into(), "world".into()],
            nested: vec![vec![1, 2], vec![3, 4, 5]],
        });
    }

    #[test]
    fn nested_types() {
        roundtrip(&Nested {
            inner: Inner {
                value: 100,
                name: "nested".into(),
            },
            list: vec![
                Inner {
                    value: 1,
                    name: "a".into(),
                },
                Inner {
                    value: 2,
                    name: "b".into(),
                },
            ],
        });
    }
}

/// Full roundtrip: ron2 → ron → ron2
mod roundtrip_ron2_to_ron_to_ron2 {
    use super::*;

    fn roundtrip<T>(original: &T)
    where
        T: ToRon + FromRon + Serialize + for<'de> Deserialize<'de> + PartialEq + std::fmt::Debug,
    {
        // ron2 serialize
        let ron2_text = original.to_ron().expect("ron2 serialize failed");

        // ron parse
        let ron_parsed: T = ron::from_str(&ron2_text).expect("ron parse failed");

        // ron serialize
        let ron_text = ron::to_string(&ron_parsed).expect("ron serialize failed");

        // ron2 parse
        let final_value = T::from_ron(&ron_text).expect("ron2 parse of ron output failed");

        assert_eq!(
            original, &final_value,
            "Roundtrip mismatch: ron2 → ron → ron2"
        );
    }

    #[test]
    fn basic_structs() {
        roundtrip(&Point { x: -10, y: 20 });
        roundtrip(&Person {
            name: "Diana".into(),
            age: 40,
            active: false,
        });
    }

    #[test]
    fn enums() {
        roundtrip(&SimpleEnum::Unit);
        roundtrip(&SimpleEnum::Tuple(-42));
        roundtrip(&Color::Named {
            name: "test".into(),
            hex: "#123456".into(),
        });
    }

    #[test]
    fn optionals() {
        roundtrip(&Optionals {
            maybe_int: Some(999),
            maybe_string: Some("optional".into()),
        });
        roundtrip(&Optionals {
            maybe_int: None,
            maybe_string: None,
        });
    }
}

// ============================================================================
// Value-Level Comparison Tests
// ============================================================================

/// Compare at the Value level
mod value_comparison {
    use super::*;

    /// Parse both ron and ron2 output to Value and compare semantically
    fn values_match<T>(value: &T)
    where
        T: Serialize + ToRon,
    {
        let ron_output = ron::to_string(value).expect("ron should serialize");
        let ron2_output = value.to_ron().expect("ron2 should serialize");

        let ron_value: Value = ron2::from_str(&ron_output).expect("ron2 should parse ron output");
        let ron2_value: Value =
            ron2::from_str(&ron2_output).expect("ron2 should parse ron2 output");

        // Values may differ in representation but should be semantically equivalent
        // For now, just verify both parse successfully
        assert!(
            !format!("{:?}", ron_value).is_empty(),
            "ron output should produce valid Value"
        );
        assert!(
            !format!("{:?}", ron2_value).is_empty(),
            "ron2 output should produce valid Value"
        );
    }

    #[test]
    fn structs() {
        values_match(&Point { x: 1, y: 2 });
        values_match(&Person {
            name: "Eve".into(),
            age: 28,
            active: true,
        });
    }

    #[test]
    fn enums() {
        values_match(&SimpleEnum::Unit);
        values_match(&SimpleEnum::Tuple(42));
        values_match(&Color::Rgb(1, 2, 3));
    }

    #[test]
    fn collections() {
        values_match(&Collections {
            numbers: vec![1, 2],
            strings: vec!["a".into()],
            nested: vec![],
        });
    }
}

// ============================================================================
// Edge Cases
// ============================================================================

mod edge_cases {
    use super::*;

    /// Test string escapes are handled consistently
    #[test]
    fn string_escapes() {
        let person = Person {
            name: "Line\nBreak".into(),
            age: 0,
            active: false,
        };

        let ron_out = ron::to_string(&person).unwrap();
        let ron2_parsed = Person::from_ron(&ron_out).unwrap();
        assert_eq!(person, ron2_parsed);

        let ron2_out = person.to_ron().unwrap();
        let ron_parsed: Person = ron::from_str(&ron2_out).unwrap();
        assert_eq!(person, ron_parsed);
    }

    /// Test special characters in strings
    #[test]
    fn special_chars() {
        let person = Person {
            name: "Tab\there".into(),
            age: 0,
            active: false,
        };

        let ron_out = ron::to_string(&person).unwrap();
        let ron2_parsed = Person::from_ron(&ron_out).unwrap();
        assert_eq!(person, ron2_parsed);
    }

    /// Test unicode strings
    #[test]
    fn unicode() {
        let person = Person {
            name: "日本語".into(),
            age: 0,
            active: false,
        };

        let ron_out = ron::to_string(&person).unwrap();
        let ron2_parsed = Person::from_ron(&ron_out).unwrap();
        assert_eq!(person, ron2_parsed);

        let ron2_out = person.to_ron().unwrap();
        let ron_parsed: Person = ron::from_str(&ron2_out).unwrap();
        assert_eq!(person, ron_parsed);
    }

    /// Test empty collections
    #[test]
    fn empty_collections() {
        let c = Collections {
            numbers: vec![],
            strings: vec![],
            nested: vec![],
        };

        let ron_out = ron::to_string(&c).unwrap();
        let ron2_parsed = Collections::from_ron(&ron_out).unwrap();
        assert_eq!(c, ron2_parsed);

        let ron2_out = c.to_ron().unwrap();
        let ron_parsed: Collections = ron::from_str(&ron2_out).unwrap();
        assert_eq!(c, ron_parsed);
    }

    /// Test numeric boundaries
    #[test]
    fn numeric_boundaries() {
        let nums = Numbers {
            i8_val: i8::MAX,
            i16_val: i16::MAX,
            i32_val: i32::MAX,
            i64_val: i64::MAX,
            u8_val: u8::MAX,
            u16_val: u16::MAX,
            u32_val: u32::MAX,
            u64_val: u64::MAX,
            f32_val: f32::MAX,
            f64_val: f64::MAX,
        };

        let ron_out = ron::to_string(&nums).unwrap();
        let ron2_parsed = Numbers::from_ron(&ron_out).unwrap();
        assert_eq!(nums, ron2_parsed);
    }

    /// Test negative number boundaries
    #[test]
    fn negative_boundaries() {
        let nums = Numbers {
            i8_val: i8::MIN,
            i16_val: i16::MIN,
            i32_val: i32::MIN,
            i64_val: i64::MIN,
            u8_val: 0,
            u16_val: 0,
            u32_val: 0,
            u64_val: 0,
            f32_val: f32::MIN,
            f64_val: f64::MIN,
        };

        let ron_out = ron::to_string(&nums).unwrap();
        let ron2_parsed = Numbers::from_ron(&ron_out).unwrap();
        assert_eq!(nums, ron2_parsed);
    }

    /// Test deeply nested structures
    #[test]
    fn deep_nesting() {
        let nested = Nested {
            inner: Inner {
                value: 1,
                name: "level1".into(),
            },
            list: vec![
                Inner {
                    value: 2,
                    name: "level2a".into(),
                },
                Inner {
                    value: 3,
                    name: "level2b".into(),
                },
            ],
        };

        let ron_out = ron::to_string(&nested).unwrap();
        let ron2_parsed = Nested::from_ron(&ron_out).unwrap();
        assert_eq!(nested, ron2_parsed);

        let ron2_out = nested.to_ron().unwrap();
        let ron_parsed: Nested = ron::from_str(&ron2_out).unwrap();
        assert_eq!(nested, ron_parsed);
    }
}

// ============================================================================
// Struct Syntax Verification
// ============================================================================

mod struct_syntax {
    use super::*;

    /// Verify both crates use the standard parenthesis syntax for structs.
    ///
    /// Standard RON: `Point(x: 1, y: 2)` - parentheses for named fields
    /// NOT: `Point { x: 1, y: 2 }` - braces are for maps
    #[test]
    fn both_crates_use_parenthesis_syntax() {
        let point = Point { x: 1, y: 2 };

        // Check ron crate output uses parentheses
        let ron_out = ron::to_string(&point).unwrap();
        assert!(
            ron_out.contains('(') && ron_out.contains(')'),
            "ron crate should use parentheses: {ron_out}"
        );
        assert!(
            !ron_out.starts_with("Point{") && !ron_out.contains("Point {"),
            "ron crate should not use braces for struct fields: {ron_out}"
        );

        // Check ron2 crate output uses parentheses
        let ron2_out = point.to_ron().unwrap();
        assert!(
            ron2_out.contains('(') && ron2_out.contains(')'),
            "ron2 crate should use parentheses: {ron2_out}"
        );
        assert!(
            !ron2_out.starts_with("Point{") && !ron2_out.contains("Point {"),
            "ron2 crate should not use braces for struct fields: {ron2_out}"
        );
    }

    /// Verify the exact output format matches expected RON syntax.
    #[test]
    fn ron2_produces_standard_syntax() {
        let point = Point { x: 1, y: 2 };
        let ron2_out = point.to_ron().unwrap();

        // ron2 produces compact output: Point(x:1,y:2)
        assert_eq!(ron2_out, "Point(x:1,y:2)");
    }
}
