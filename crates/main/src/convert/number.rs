//! Interpretation of number literals.
//!
//! Typed conversion (`FromRon` for integers and floats) and the [`Value`]
//! conversion share these rules, so both accept exactly the same literals:
//!
//! - Integer literals (decimal, `0x`, `0o`, `0b`, `_` separators, optional sign
//!   and type suffix such as `5u8`) are parsed exactly. A literal outside the
//!   widest supported integer type (`i64`/`u64`, or `i128`/`u128` with the
//!   `integer128` feature) is rejected, also when the target is a float.
//! - A type suffix must fit its type. [`Value`] uses it to pick the variant.
//! - Float literals are parsed directly in the target precision, so `f32` is
//!   rounded once. A finite literal that overflows the target is an error.
//! - Integer literals convert to floats with a single rounding.
//!
//! A synthetic number that carries its [`Number`] ([`NumberExpr::value`])
//! converts without parsing its text, to exactly the result the text gives.
//!
//! [`Value`]: crate::Value

use alloc::{borrow::Cow, string::String};
use core::{ops::Neg, str::FromStr};

use super::spanned_type_mismatch;
use crate::{
    ast::{Expr, NumberExpr, NumberKind},
    error::{Error, ErrorKind, Result},
    value::{F32, F64, Number},
};

/// An integer literal's exact value.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Int {
    /// Zero or positive.
    Pos(u128),
    /// Strictly negative.
    Neg(i128),
}

impl Int {
    /// The integer held by `n`; `None` for floats.
    fn from_number(n: Number) -> Option<Self> {
        let signed = |v: i128| u128::try_from(v).map_or(Int::Neg(v), Int::Pos);
        Some(match n {
            Number::I8(v) => signed(v.into()),
            Number::I16(v) => signed(v.into()),
            Number::I32(v) => signed(v.into()),
            Number::I64(v) => signed(v.into()),
            #[cfg(feature = "integer128")]
            Number::I128(v) => signed(v),
            Number::U8(v) => Int::Pos(v.into()),
            Number::U16(v) => Int::Pos(v.into()),
            Number::U32(v) => Int::Pos(v.into()),
            Number::U64(v) => Int::Pos(v.into()),
            #[cfg(feature = "integer128")]
            Number::U128(v) => Int::Pos(v),
            Number::F32(_) | Number::F64(_) => return None,
            #[cfg(not(doc))]
            Number::__NonExhaustive(never) => never.never(),
        })
    }

    fn convert<T: TryFrom<u128> + TryFrom<i128>>(self) -> Option<T> {
        match self {
            Int::Pos(v) => T::try_from(v).ok(),
            Int::Neg(v) => T::try_from(v).ok(),
        }
    }

    /// Whether the value fits the widest integer type `Value` can hold.
    fn is_supported(self) -> bool {
        cfg!(feature = "integer128")
            || match self {
                Int::Pos(v) => u64::try_from(v).is_ok(),
                Int::Neg(v) => i64::try_from(v).is_ok(),
            }
    }

    /// The smallest `Number` variant of matching signedness holding the value.
    fn smallest_number(self) -> Option<Number> {
        match self {
            Int::Pos(v) => {
                let n = u8::try_from(v).ok().map(Number::U8);
                let n = n.or_else(|| u16::try_from(v).ok().map(Number::U16));
                let n = n.or_else(|| u32::try_from(v).ok().map(Number::U32));
                let n = n.or_else(|| u64::try_from(v).ok().map(Number::U64));
                #[cfg(feature = "integer128")]
                let n = n.or(Some(Number::U128(v)));
                n
            }
            Int::Neg(v) => {
                let n = i8::try_from(v).ok().map(Number::I8);
                let n = n.or_else(|| i16::try_from(v).ok().map(Number::I16));
                let n = n.or_else(|| i32::try_from(v).ok().map(Number::I32));
                let n = n.or_else(|| i64::try_from(v).ok().map(Number::I64));
                #[cfg(feature = "integer128")]
                let n = n.or(Some(Number::I128(v)));
                n
            }
        }
    }

    /// The `Number` variant named by a type suffix, if the value fits it.
    fn suffixed_number(self, suffix: &str) -> Option<Number> {
        match suffix {
            "i8" => self.convert().map(Number::I8),
            "i16" => self.convert().map(Number::I16),
            "i32" => self.convert().map(Number::I32),
            "i64" => self.convert().map(Number::I64),
            #[cfg(feature = "integer128")]
            "i128" => self.convert().map(Number::I128),
            "u8" => self.convert().map(Number::U8),
            "u16" => self.convert().map(Number::U16),
            "u32" => self.convert().map(Number::U32),
            "u64" => self.convert().map(Number::U64),
            #[cfg(feature = "integer128")]
            "u128" => self.convert().map(Number::U128),
            // Without `integer128`, a value that passed `is_supported` fits
            // 64 bits; keep the suffix's signedness.
            #[cfg(not(feature = "integer128"))]
            "i128" => self
                .convert::<i128>()
                .and_then(|_| self.convert().map(Number::I64)),
            #[cfg(not(feature = "integer128"))]
            "u128" => self
                .convert::<u128>()
                .and_then(|_| self.convert().map(Number::U64)),
            _ => None,
        }
    }
}

const INT_SUFFIXES: [&str; 10] = [
    "i128", "i64", "i32", "i16", "i8", "u128", "u64", "u32", "u16", "u8",
];

/// Split a trailing integer type suffix (`5u8` → `5`, `u8`).
///
/// `i` and `u` are not digits in any base, so this never eats digits.
fn split_int_suffix(s: &str) -> (&str, Option<&'static str>) {
    INT_SUFFIXES
        .iter()
        .find_map(|suffix| s.strip_suffix(suffix).map(|rest| (rest, Some(*suffix))))
        .unwrap_or((s, None))
}

/// Name of the widest integer type `Value` can hold with the given sign.
fn widest_type(negative: bool) -> &'static str {
    match (negative, cfg!(feature = "integer128")) {
        (false, true) => "u128",
        (false, false) => "u64",
        (true, true) => "i128",
        (true, false) => "i64",
    }
}

fn split_sign(s: &str) -> (bool, &str) {
    if let Some(rest) = s.strip_prefix('-') {
        (true, rest)
    } else {
        (false, s.strip_prefix('+').unwrap_or(s))
    }
}

fn split_radix(s: &str) -> (u32, &str) {
    let radix = match s.as_bytes() {
        [b'0', b'x' | b'X', ..] => 16,
        [b'0', b'b' | b'B', ..] => 2,
        [b'0', b'o' | b'O', ..] => 8,
        _ => return (10, s),
    };
    (radix, s.get(2..).unwrap_or_default())
}

fn without_underscores(s: &str) -> Cow<'_, str> {
    if s.contains('_') {
        Cow::Owned(s.chars().filter(|&c| c != '_').collect::<String>())
    } else {
        Cow::Borrowed(s)
    }
}

fn malformed(raw: &str, what: &'static str) -> Error {
    Error::new(ErrorKind::Expected {
        expected: alloc::format!("{what}, found `{raw}`").into(),
        context: None,
    })
}

fn out_of_bounds(raw: &str, target_type: &'static str) -> Error {
    Error::integer_out_of_bounds(String::from(raw), target_type)
}

/// Parse an integer literal into its value and type suffix.
///
/// `target` names the type in out-of-range errors; `None` uses the widest
/// supported type.
fn parse_int(raw: &str, target: Option<&'static str>) -> Result<(Int, Option<&'static str>)> {
    let (negative, unsigned) = split_sign(raw);
    let (body, suffix) = split_int_suffix(unsigned);
    let (radix, digits) = split_radix(body);
    let digits = without_underscores(digits);
    // `from_str_radix` would accept a second sign.
    if digits.starts_with(['+', '-']) {
        return Err(malformed(raw, "an integer"));
    }

    let target = target.unwrap_or_else(|| widest_type(negative));
    let magnitude = u128::from_str_radix(&digits, radix).map_err(|e| match e.kind() {
        core::num::IntErrorKind::PosOverflow => out_of_bounds(raw, target),
        _ => malformed(raw, "an integer"),
    })?;

    let value = if negative && magnitude != 0 {
        Int::Neg(
            0i128
                .checked_sub_unsigned(magnitude)
                .ok_or_else(|| out_of_bounds(raw, target))?,
        )
    } else {
        Int::Pos(magnitude)
    };
    if !value.is_supported() {
        return Err(out_of_bounds(raw, target));
    }
    if let Some(suffix) = suffix
        && value.suffixed_number(suffix).is_none()
    {
        return Err(out_of_bounds(raw, suffix));
    }
    Ok((value, suffix))
}

/// Floating-point targets of number conversion.
pub(crate) trait Float: Copy + FromStr + From<u8> + Neg<Output = Self> {
    const NAME: &'static str;
    const INFINITY: Self;
    const NAN: Self;
    fn from_int(value: Int) -> Self;
    fn from_f32(value: f32) -> Self;
    fn from_f64(value: f64) -> Self;
    /// `value` at this precision if no rounding is needed (`f64` only).
    fn from_f64_exact(value: f64) -> Option<Self>;
    fn is_infinite(self) -> bool;
}

// Integer-to-float `as` casts round to nearest: the single rounding we want.
#[allow(clippy::cast_precision_loss)]
impl Float for f32 {
    const NAME: &'static str = "f32";
    const INFINITY: Self = f32::INFINITY;
    const NAN: Self = f32::NAN;
    fn from_int(value: Int) -> Self {
        match value {
            Int::Pos(v) => v as f32,
            Int::Neg(v) => v as f32,
        }
    }
    fn from_f32(value: f32) -> Self {
        value
    }
    #[allow(clippy::cast_possible_truncation)]
    fn from_f64(value: f64) -> Self {
        value as f32
    }
    fn from_f64_exact(_: f64) -> Option<Self> {
        None
    }
    fn is_infinite(self) -> bool {
        f32::is_infinite(self)
    }
}

#[allow(clippy::cast_precision_loss)]
impl Float for f64 {
    const NAME: &'static str = "f64";
    const INFINITY: Self = f64::INFINITY;
    const NAN: Self = f64::NAN;
    fn from_int(value: Int) -> Self {
        match value {
            Int::Pos(v) => v as f64,
            Int::Neg(v) => v as f64,
        }
    }
    fn from_f32(value: f32) -> Self {
        f64::from(value)
    }
    fn from_f64(value: f64) -> Self {
        value
    }
    fn from_f64_exact(value: f64) -> Option<Self> {
        Some(value)
    }
    fn is_infinite(self) -> bool {
        f64::is_infinite(self)
    }
}

/// Split a float literal into its numeric text and `f32`/`f64` suffix.
fn split_float_suffix(raw: &str) -> (&str, Option<&'static str>) {
    ["f32", "f64"]
        .iter()
        .find_map(|suffix| raw.strip_suffix(suffix).map(|rest| (rest, Some(*suffix))))
        .unwrap_or((raw, None))
}

fn is_float_kind(n: &NumberExpr<'_>) -> bool {
    n.kind == NumberKind::Float
}

fn float_out_of_bounds(raw: &str, target_type: &'static str) -> Error {
    Error::new(ErrorKind::FloatOutOfBounds {
        value: String::from(raw).into(),
        target_type,
    })
}

/// `NaN` with the given sign, as `NaN` and `-NaN` read.
fn signed_nan<G: Float>(negative: bool) -> G {
    if negative { -G::NAN } else { G::NAN }
}

/// An integer literal at float precision `F`; a value beyond `F`'s range is an error.
fn int_to_float<F: Float>(value: Int, raw: &str) -> Result<F> {
    let value = F::from_int(value);
    if value.is_infinite() {
        return Err(float_out_of_bounds(raw, F::NAME));
    }
    Ok(value)
}

/// A carried [`Number`] at float precision `F`, equal to what reading its
/// canonical text (`raw`) gives; `None` when only parsing the text can tell.
///
/// The text of an `F32` is its shortest spelling, read at `f32` precision or
/// spelled so that `f64` reads the widened value; an `F64` is read exactly
/// by `f64`, while reading it at `f32` precision rounds its shortest
/// spelling (not the value), so that case parses the text. `NaN` payloads
/// are not spelled, only the sign.
fn carried_float<F: Float>(n: Number, raw: &str) -> Option<Result<F>> {
    match n {
        Number::F32(v) if v.0.is_nan() => Some(Ok(signed_nan(v.0.is_sign_negative()))),
        Number::F32(v) => Some(Ok(F::from_f32(v.0))),
        Number::F64(v) if v.0.is_nan() => Some(Ok(signed_nan(v.0.is_sign_negative()))),
        Number::F64(v) => F::from_f64_exact(v.0).map(Ok),
        _ => Int::from_number(n).map(|value| int_to_float(value, raw)),
    }
}

/// Parse float text (no suffix or `_`) at precision `G`. A finite literal
/// that overflows `G` is an error; the sign of `NaN` is kept.
fn parse_float_text<G: Float>(text: &str, raw: &str) -> Result<G> {
    let (negative, body) = split_sign(text);
    let special = match body {
        "inf" => G::INFINITY,
        "NaN" => G::NAN,
        _ => {
            let value = text.parse::<G>().map_err(|_| malformed(raw, "a float"))?;
            if value.is_infinite() {
                return Err(float_out_of_bounds(raw, G::NAME));
            }
            return Ok(value);
        }
    };
    Ok(if negative { -special } else { special })
}

/// A float literal's suffix fixes its type: `16777217f32` is the `f32`
/// value 16777216 for every target, as it is in a `Value`.
fn parse_float<F: Float>(n: &NumberExpr<'_>) -> Result<F> {
    let raw = n.raw.as_ref();
    if let Some(value) = n.value.and_then(|number| carried_float(number, raw)) {
        return value;
    }
    if !is_float_kind(n) {
        return int_to_float(parse_int(raw, None)?.0, raw);
    }

    let (text, suffix) = split_float_suffix(raw);
    let text = without_underscores(text);
    match suffix {
        Some("f32") => parse_float_text::<f32>(&text, raw).map(F::from_f32),
        Some("f64") => {
            let wide = parse_float_text::<f64>(&text, raw)?;
            let value = F::from_f64(wide);
            if value.is_infinite() && !wide.is_infinite() {
                return Err(float_out_of_bounds(raw, F::NAME));
            }
            Ok(value)
        }
        _ => parse_float_text::<F>(&text, raw),
    }
}

fn parse_number(n: &NumberExpr<'_>) -> Result<Number> {
    if is_float_kind(n) {
        return Ok(match split_float_suffix(&n.raw).1 {
            Some("f32") => Number::F32(F32(parse_float(n)?)),
            _ => Number::F64(F64(parse_float(n)?)),
        });
    }

    let (value, suffix) = parse_int(&n.raw, None)?;
    match suffix {
        Some(suffix) => value.suffixed_number(suffix),
        None => value.smallest_number(),
    }
    .ok_or_else(|| out_of_bounds(&n.raw, widest_type(matches!(value, Int::Neg(_)))))
}

/// Convert a number literal to the `Number` stored in a `Value`.
///
/// A carried number is returned as is, keeping its variant (`5u32` stays
/// `U32`, where reading the text `5` gives `U8`; both are equal).
pub(crate) fn to_number(n: &NumberExpr<'_>) -> Result<Number> {
    match n.value {
        Some(number) => Ok(number),
        None => parse_number(n).map_err(|e| e.or_span(n.span)),
    }
}

/// Convert an expression to an integer type (`FromRon` for integers).
pub(crate) fn expr_to_int<T>(expr: &Expr<'_>, target: &'static str) -> Result<T>
where
    T: TryFrom<u128> + TryFrom<i128> + TryFrom<u8>,
{
    match expr {
        Expr::Number(n) if is_float_kind(n) => Err(Error::with_span(
            ErrorKind::TypeMismatch {
                expected: target.into(),
                found: "float".into(),
            },
            n.span,
        )),
        Expr::Number(n) => match n.value.and_then(Int::from_number) {
            Some(value) => Ok(value),
            None => parse_int(&n.raw, Some(target)).map(|(value, _)| value),
        }
        .and_then(|value| value.convert().ok_or_else(|| out_of_bounds(&n.raw, target)))
        .map_err(|e| e.or_span(n.span)),
        Expr::Byte(b) => {
            T::try_from(b.value).map_err(|_| out_of_bounds(&b.raw, target).or_span(b.span))
        }
        _ => Err(spanned_type_mismatch(target, expr)),
    }
}

/// Convert an expression to a float type (`FromRon` for floats).
pub(crate) fn expr_to_float<F: Float>(expr: &Expr<'_>) -> Result<F> {
    match expr {
        Expr::Number(n) => parse_float(n).map_err(|e| e.or_span(n.span)),
        Expr::Byte(b) => Ok(F::from(b.value)),
        _ => Err(spanned_type_mismatch(F::NAME, expr)),
    }
}
