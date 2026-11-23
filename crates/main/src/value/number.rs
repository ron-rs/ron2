use core::{
    cmp::{Eq, Ordering},
    hash::{Hash, Hasher},
};

/// A wrapper for any numeric primitive type in Rust.
///
/// Some variants of the `Number` enum are enabled by features:
/// - `Number::I128` and `Number::U128` by the `integer128` feature
///
/// To ensure that feature unification does not break `match`ing over `Number`,
/// the `Number` enum is non-exhaustive.
///
/// # Equality and ordering
///
/// Numbers compare by value, not by variant:
///
/// - Integers of any width are equal when their values are: `1u8 == 1i64`.
/// - Floats compare with [`f64::total_cmp`] after widening `f32` (exact), so
///   `0.5f32 == 0.5f64`, `NaN` equals itself and `-0.0 < 0.0`.
/// - An integer never equals a float (`1 != 1.0`), mirroring RON's distinct
///   integer and float literals. Ordering is still numeric across both
///   (`-1 < -0.5 < 0 < 0.5 < 1`); an integer sorts just before a float of the
///   same value.
/// - [`Hash`] agrees with [`Eq`], so numbers work as map keys across variants.
///
/// ```
/// use ron2::value::Number;
///
/// assert_eq!(Number::from(1u8), Number::from(1i64));
/// assert_ne!(Number::from(1u8), Number::from(1.0f64));
/// assert!(Number::from(-1000i16) < Number::from(-1i8));
/// assert!(Number::from(0.5f32) < Number::from(2u8));
/// ```
#[derive(Copy, Clone, Debug)]
#[cfg_attr(doc, non_exhaustive)]
pub enum Number {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    #[cfg(feature = "integer128")]
    I128(i128),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    #[cfg(feature = "integer128")]
    U128(u128),
    F32(F32),
    F64(F64),
    #[cfg(not(doc))]
    #[allow(private_interfaces)]
    __NonExhaustive(private::Never),
}

mod private {
    #[derive(Debug)]
    enum _Never {}

    #[derive(Copy, Clone, Debug)]
    pub struct Never {
        never: &'static _Never,
    }

    impl Never {
        pub fn never(self) -> ! {
            match *self.never {}
        }
    }
}

macro_rules! float_ty {
    ($ty:ident($float:ty)) => {
        #[doc = concat!(
                    "A wrapper for [`", stringify!($float), "`], which implements [`Eq`], ",
                    "[`Hash`] and [`Ord`] using [`", stringify!($float), "::total_cmp`] ",
                    "for a total order comparison",
                )]
        #[derive(Copy, Clone, Debug)] // GRCOV_EXCL_LINE
        pub struct $ty(pub $float);

        impl $ty {
            #[doc = concat!("Construct a new [`", stringify!($ty), "`].")]
            #[must_use]
            pub fn new(v: $float) -> Self {
                Self(v)
            }

            #[doc = concat!("Returns the wrapped [`", stringify!($float), "`].")]
            #[must_use]
            pub fn get(self) -> $float {
                self.0
            }
        }

        impl From<$float> for $ty {
            fn from(v: $float) -> Self {
                Self::new(v)
            }
        }

        /// Partial equality comparison
        ///
        #[doc = concat!(
                    "In order to be able to use [`", stringify!($ty), "`] as a mapping key, ",
                    "floating values use [`", stringify!($float), "::total_cmp`] for a total ",
                    "order comparison.",
                )]
        ///
        /// See the [`Ord`] implementation.
        impl PartialEq for $ty {
            fn eq(&self, other: &Self) -> bool {
                self.cmp(other).is_eq()
            }
        }

        /// Equality comparison
        ///
        #[doc = concat!(
                    "In order to be able to use [`", stringify!($ty), "`] as a mapping key, ",
                    "floating values use [`", stringify!($float), "::total_cmp`] for a total ",
                    "order comparison.",
                )]
        ///
        /// See the [`Ord`] implementation.
        impl Eq for $ty {}

        impl Hash for $ty {
            fn hash<H: Hasher>(&self, state: &mut H) {
                self.0.to_bits().hash(state);
            }
        }

        /// Partial ordering comparison
        ///
        #[doc = concat!(
                    "In order to be able to use [`", stringify!($ty), "`] as a mapping key, ",
                    "floating values use [`", stringify!($float), "::total_cmp`] for a total ",
                    "order comparison.",
                )]
        ///
        /// See the [`Ord`] implementation.
        impl PartialOrd for $ty {
            fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
                Some(self.cmp(other))
            }
        }

        /// Ordering comparison
        ///
        #[doc = concat!(
                    "In order to be able to use [`", stringify!($ty), "`] as a mapping key, ",
                    "floating values use [`", stringify!($float), "::total_cmp`] for a total ",
                    "order comparison.",
                )]
        impl Ord for $ty {
            fn cmp(&self, other: &Self) -> Ordering {
                self.0.total_cmp(&other.0)
            }
        }
    };
}

float_ty! { F32(f32) }
float_ty! { F64(f64) }

impl Number {
    /// Construct a new number.
    pub fn new(v: impl Into<Number>) -> Self {
        v.into()
    }

    /// Returns the [`f64`] representation of the [`Number`] regardless of
    /// whether the number is stored as a float or integer.
    ///
    /// # Example
    ///
    /// ```
    /// # use ron2::value::Number;
    /// let i = Number::new(5);
    /// let f = Number::new(2.0);
    /// assert_eq!(i.into_f64(), 5.0);
    /// assert_eq!(f.into_f64(), 2.0);
    /// ```
    #[must_use]
    pub fn into_f64(self) -> f64 {
        #[allow(clippy::cast_precision_loss)]
        match self {
            Self::I8(v) => f64::from(v),
            Self::I16(v) => f64::from(v),
            Self::I32(v) => f64::from(v),
            Self::I64(v) => v as f64,
            #[cfg(feature = "integer128")]
            Self::I128(v) => v as f64,
            Self::U8(v) => f64::from(v),
            Self::U16(v) => f64::from(v),
            Self::U32(v) => f64::from(v),
            Self::U64(v) => v as f64,
            #[cfg(feature = "integer128")]
            Self::U128(v) => v as f64,
            Self::F32(v) => f64::from(v.get()),
            Self::F64(v) => v.get(),
            #[cfg(not(doc))]
            Self::__NonExhaustive(never) => never.never(),
        }
    }
}

/// A number reduced to the form its equality and ordering are defined on.
#[derive(Clone, Copy)]
enum Canonical {
    Int { negative: bool, magnitude: u128 },
    Float(f64),
}

impl Canonical {
    fn signed(v: impl Into<i128>) -> Self {
        let v = v.into();
        Canonical::Int {
            negative: v < 0,
            magnitude: v.unsigned_abs(),
        }
    }

    fn unsigned(v: impl Into<u128>) -> Self {
        Canonical::Int {
            negative: false,
            magnitude: v.into(),
        }
    }
}

impl Number {
    fn canonical(self) -> Canonical {
        match self {
            Self::I8(v) => Canonical::signed(v),
            Self::I16(v) => Canonical::signed(v),
            Self::I32(v) => Canonical::signed(v),
            Self::I64(v) => Canonical::signed(v),
            #[cfg(feature = "integer128")]
            Self::I128(v) => Canonical::signed(v),
            Self::U8(v) => Canonical::unsigned(v),
            Self::U16(v) => Canonical::unsigned(v),
            Self::U32(v) => Canonical::unsigned(v),
            Self::U64(v) => Canonical::unsigned(v),
            #[cfg(feature = "integer128")]
            Self::U128(v) => Canonical::unsigned(v),
            Self::F32(v) => Canonical::Float(f64::from(v.0)),
            Self::F64(v) => Canonical::Float(v.0),
            #[cfg(not(doc))]
            Self::__NonExhaustive(never) => never.never(),
        }
    }
}

fn cmp_ints(a: (bool, u128), b: (bool, u128)) -> Ordering {
    match (a.0, b.0) {
        (false, false) => a.1.cmp(&b.1),
        (true, true) => b.1.cmp(&a.1),
        (true, false) => Ordering::Less,
        (false, true) => Ordering::Greater,
    }
}

/// Compare an integer with a float by value; on a tie the integer is less.
fn cmp_int_float(int: (bool, u128), f: f64) -> Ordering {
    // 2^128: every integer magnitude is below it.
    const INT_LIMIT: f64 = 340_282_366_920_938_463_463_374_607_431_768_211_456.0;

    if f.is_nan() || f.abs() >= INT_LIMIT {
        // Positive NaN sorts above +inf and negative NaN below -inf.
        return if f.is_sign_negative() {
            Ordering::Greater
        } else {
            Ordering::Less
        };
    }

    let whole = f.trunc();
    // Exact: `whole` is integral and its magnitude is below 2^128.
    #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
    let whole_int = (whole < 0.0, whole.abs() as u128);
    match cmp_ints(int, whole_int) {
        Ordering::Equal if f > whole => Ordering::Less,
        Ordering::Equal if f < whole => Ordering::Greater,
        Ordering::Equal => Ordering::Less,
        unequal => unequal,
    }
}

impl Ord for Number {
    fn cmp(&self, other: &Self) -> Ordering {
        use Canonical::{Float, Int};

        match (self.canonical(), other.canonical()) {
            (
                Int {
                    negative,
                    magnitude,
                },
                Int {
                    negative: other_negative,
                    magnitude: other_magnitude,
                },
            ) => cmp_ints((negative, magnitude), (other_negative, other_magnitude)),
            (Float(a), Float(b)) => a.total_cmp(&b),
            (
                Int {
                    negative,
                    magnitude,
                },
                Float(f),
            ) => cmp_int_float((negative, magnitude), f),
            (
                Float(f),
                Int {
                    negative,
                    magnitude,
                },
            ) => cmp_int_float((negative, magnitude), f).reverse(),
        }
    }
}

impl PartialOrd for Number {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other).is_eq()
    }
}

impl Eq for Number {}

impl Hash for Number {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self.canonical() {
            Canonical::Int {
                negative,
                magnitude,
            } => {
                state.write_u8(0);
                negative.hash(state);
                magnitude.hash(state);
            }
            Canonical::Float(f) => {
                state.write_u8(1);
                f.to_bits().hash(state);
            }
        }
    }
}

macro_rules! number_from_impl {
    (Number::$variant:ident($wrap:ident($ty:ty))) => {
        impl From<$ty> for Number {
            fn from(v: $ty) -> Number {
                Number::$variant($wrap(v))
            }
        }
    };
    (Number::$variant:ident($ty:ty)) => {
        impl From<$ty> for Number {
            fn from(v: $ty) -> Number {
                Number::$variant(v)
            }
        }
    };
}

number_from_impl! { Number::I8(i8) }
number_from_impl! { Number::I16(i16) }
number_from_impl! { Number::I32(i32) }
number_from_impl! { Number::I64(i64) }
#[cfg(feature = "integer128")]
number_from_impl! { Number::I128(i128) }
number_from_impl! { Number::U8(u8) }
number_from_impl! { Number::U16(u16) }
number_from_impl! { Number::U32(u32) }
number_from_impl! { Number::U64(u64) }
#[cfg(feature = "integer128")]
number_from_impl! { Number::U128(u128) }
number_from_impl! { Number::F32(F32(f32)) }
number_from_impl! { Number::F64(F64(f64)) }

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_nan() {
        assert_eq!(F32(f32::NAN), F32(f32::NAN));
        assert_eq!(F32(-f32::NAN), F32(-f32::NAN));
        assert_ne!(F32(f32::NAN), F32(-f32::NAN));
    }

    #[test]
    fn test_nan_hash() {
        use core::hash::{Hash, Hasher};
        use std::collections::hash_map::DefaultHasher;

        fn hash<T: Hash>(v: &T) -> u64 {
            let mut state = DefaultHasher::new();
            v.hash(&mut state);
            state.finish()
        }

        assert_eq!(hash(&F32(f32::NAN)), hash(&F32(f32::NAN)));
        assert_eq!(hash(&F32(-f32::NAN)), hash(&F32(-f32::NAN)));
        assert_ne!(hash(&F32(f32::NAN)), hash(&F32(-f32::NAN)));
    }

    #[test]
    fn test_partial_ord() {
        assert!(F32(f32::NAN) > F32(f32::INFINITY));
        assert!(F32(-f32::NAN) < F32(f32::NEG_INFINITY));
        assert_eq!(F32(f32::NAN), F32(f32::NAN));
    }

    fn hash(n: Number) -> u64 {
        use core::hash::BuildHasher;
        std::collections::hash_map::RandomState::new().hash_one(n)
    }

    fn hash_with(state: &std::collections::hash_map::RandomState, n: Number) -> u64 {
        use core::hash::BuildHasher;
        state.hash_one(n)
    }

    #[test]
    fn integers_compare_by_value_across_widths() {
        let state = std::collections::hash_map::RandomState::new();
        let ones = [
            Number::from(1u8),
            Number::from(1u16),
            Number::from(1u32),
            Number::from(1u64),
            Number::from(1i8),
            Number::from(1i16),
            Number::from(1i32),
            Number::from(1i64),
        ];
        for a in ones {
            for b in ones {
                assert_eq!(a, b);
                assert_eq!(hash_with(&state, a), hash_with(&state, b));
            }
        }
        assert_eq!(Number::from(u64::MAX), Number::from(u64::MAX));
        assert!(Number::from(i64::MIN) < Number::from(0u8));
        assert!(Number::from(-1i8) < Number::from(u64::MAX));
    }

    #[test]
    fn floats_compare_by_value_across_widths() {
        let state = std::collections::hash_map::RandomState::new();
        assert_eq!(Number::from(0.5f32), Number::from(0.5f64));
        assert_eq!(
            hash_with(&state, Number::from(0.5f32)),
            hash_with(&state, Number::from(0.5f64))
        );
        // 0.1f32 is not the same value as 0.1f64.
        assert_ne!(Number::from(0.1f32), Number::from(0.1f64));
        assert_eq!(Number::from(f32::NAN), Number::from(f64::NAN));
        assert!(Number::from(-0.0f64) < Number::from(0.0f32));
        assert_ne!(Number::from(-0.0f64), Number::from(0.0f64));
    }

    #[test]
    fn integers_and_floats_are_distinct_but_ordered_numerically() {
        assert_ne!(Number::from(1u8), Number::from(1.0f64));
        assert_ne!(Number::from(0u8), Number::from(0.0f64));
        assert_ne!(hash(Number::from(1u8)), hash(Number::from(1.0f64)));

        let mut numbers = alloc::vec![
            Number::from(300u16),
            Number::from(2u8),
            Number::from(0.5f64),
            Number::from(-1i8),
            Number::from(-1000i16),
            Number::from(f64::INFINITY),
            Number::from(f64::NEG_INFINITY),
            Number::from(f64::NAN),
            Number::from(-f64::NAN),
            Number::from(2.0f32),
            Number::from(-0.0f64),
            Number::from(0u8),
            Number::from(1e30f64),
            Number::from(-1.5f64),
        ];
        numbers.sort();
        assert_eq!(
            numbers,
            alloc::vec![
                Number::from(-f64::NAN),
                Number::from(f64::NEG_INFINITY),
                Number::from(-1000i16),
                Number::from(-1.5f64),
                Number::from(-1i8),
                Number::from(0u8),
                Number::from(-0.0f64),
                Number::from(0.5f64),
                Number::from(2u8),
                Number::from(2.0f32),
                Number::from(300u16),
                Number::from(1e30f64),
                Number::from(f64::INFINITY),
                Number::from(f64::NAN),
            ]
        );
        // Sorting must agree with pairwise comparison (total order).
        for (i, a) in numbers.iter().enumerate() {
            for (j, b) in numbers.iter().enumerate() {
                assert_eq!(a.cmp(b), i.cmp(&j), "{a:?} vs {b:?}");
            }
        }
    }

    #[cfg(feature = "integer128")]
    #[test]
    fn wide_integers_against_floats() {
        assert!(Number::from(u128::MAX) < Number::from(1e39f64));
        assert!(Number::from(i128::MIN) > Number::from(-1e39f64));
        assert!(Number::from(u128::MAX) > Number::from(1e38f64));
        assert_eq!(Number::from(u128::MAX), Number::from(u128::MAX));
    }
}
