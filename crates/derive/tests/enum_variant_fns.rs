//! Derived enum code has one `#[inline(never)]` function per variant, so the
//! stack a recursive enum needs per level does not grow with the number of
//! variants. The lints mirror the ron2 crate's to keep generated code clean.

#![deny(clippy::all)]
#![warn(clippy::pedantic)]

use std::{fmt::Debug, marker::PhantomData};

use ron2::{FromRon, ToRon, fmt::FormatConfig};

macro_rules! wide_enum {
    ($($variant:ident),*) => {
        /// Many struct variants, each holding children of the same type.
        #[derive(Debug, Clone, PartialEq, FromRon, ToRon)]
        enum Wide {
            Leaf,
            $($variant {
                name: String,
                count: u32,
                ratio: f64,
                tags: Vec<String>,
                flag: Option<bool>,
                extra: Option<i64>,
                children: Vec<Wide>,
            },)*
        }
    };
}

wide_enum!(
    V0, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16, V17, V18, V19, V20,
    V21, V22, V23
);

fn nested(depth: usize) -> Wide {
    let mut value = Wide::Leaf;
    for _ in 0..depth {
        value = Wide::V23 {
            name: "n".into(),
            count: 1,
            ratio: 0.5,
            tags: vec!["a".into()],
            flag: Some(true),
            extra: None,
            children: vec![value],
        };
    }
    value
}

/// Before the split, a debug build needed about 400 KB of stack per level
/// for `to_ast` and 36 KB for `from_ast` (aarch64), so this overflowed after
/// 4 levels; now it is about 20 KB per level, well within the margin.
#[test]
fn deep_recursive_enum_on_a_small_stack() {
    let handle = std::thread::Builder::new()
        .stack_size(2 << 20)
        .spawn(|| {
            let value = nested(16);
            let text = value.to_ron_with(&FormatConfig::minimal()).unwrap();
            assert_eq!(Wide::from_ron(&text).unwrap(), value);
        })
        .unwrap();
    handle.join().unwrap();
}

#[track_caller]
fn round_trip<T: ToRon + FromRon + PartialEq + Debug>(value: &T, expected: &str) {
    let text = value.to_ron_with(&FormatConfig::minimal()).unwrap();
    assert_eq!(text, expected);
    assert_eq!(&T::from_ron(&text).unwrap(), value);
}

#[derive(Debug, PartialEq, FromRon, ToRon)]
#[allow(non_camel_case_types)]
enum Shapes<'a, T: Clone, const N: usize>
where
    T: Debug,
{
    Unit,
    Empty(),
    EmptyStruct {},
    r#type(T, #[ron(skip)] u8),
    lower_case {
        values: [T; N],
        label: String,
        #[ron(skip)]
        marker: PhantomData<&'a ()>,
    },
    Copyable(u8, char, bool, f32, i64, u16, u32, i8, i16),
    Many {
        a: u8,
        b: u8,
        c: u8,
        d: u8,
        e: u8,
        f: u8,
        g: u8,
        h: Option<u8>,
        #[ron(skip)]
        skipped: u8,
    },
    #[ron(skip)]
    Skipped(T),
    Some(T),
    None,
}

/// User methods with names like the generated ones do not collide.
impl<T: Clone + Debug, const N: usize> Shapes<'_, T, N> {
    #[allow(clippy::unused_self)]
    fn to_ast_1(&self) -> u8 {
        1
    }
}

#[test]
fn generics_where_clauses_and_odd_names() {
    type S<'a> = Shapes<'a, u8, 2>;
    round_trip(&S::Unit, "Unit");
    round_trip(&S::Empty(), "Empty()");
    round_trip(&S::EmptyStruct {}, "EmptyStruct()");
    round_trip(&S::r#type(4, 0), "type(4)");
    round_trip(
        &S::lower_case {
            values: [1, 2],
            label: "x".into(),
            marker: PhantomData,
        },
        r#"lower_case(values:[1,2],label:"x")"#,
    );
    round_trip(
        &S::Copyable(1, 'c', true, 0.5, -1, 2, 3, -4, 5),
        "Copyable(1,'c',true,0.5,-1,2,3,-4,5)",
    );
    round_trip(
        &S::Many {
            a: 1,
            b: 2,
            c: 3,
            d: 4,
            e: 5,
            f: 6,
            g: 7,
            h: None,
            skipped: 0,
        },
        "Many(a:1,b:2,c:3,d:4,e:5,f:6,g:7,h:None)",
    );
    round_trip(&S::Some(3), "r#Some(3)");
    round_trip(&S::None, "r#None");
    assert!(S::Skipped(1).to_ron().is_err());
    assert_eq!(S::Unit.to_ast_1(), 1);
}
