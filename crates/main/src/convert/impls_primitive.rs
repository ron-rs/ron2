//! Primitive type implementations for `ToRon` and `FromRon`.

use alloc::string::String;

use crate::ast::{
    Expr, synthetic_bool, synthetic_char, synthetic_f32, synthetic_f64, synthetic_integer,
    synthetic_string, synthetic_unit,
};
use crate::error::{Error, Result, SpannedResult};

use super::number::{parse_float_from_raw, parse_integer_from_raw};
use super::{FromRon, ToRon, spanned_err, spanned_type_mismatch};

// =============================================================================
// ToRon implementations
// =============================================================================

impl ToRon for bool {
    fn to_ast(&self) -> Result<Expr<'static>> {
        Ok(synthetic_bool(*self))
    }
}

impl ToRon for char {
    fn to_ast(&self) -> Result<Expr<'static>> {
        Ok(synthetic_char(*self))
    }
}

impl ToRon for String {
    fn to_ast(&self) -> Result<Expr<'static>> {
        Ok(synthetic_string(self.clone()))
    }
}

impl ToRon for str {
    fn to_ast(&self) -> Result<Expr<'static>> {
        Ok(synthetic_string(self.into()))
    }
}

impl ToRon for () {
    fn to_ast(&self) -> Result<Expr<'static>> {
        Ok(synthetic_unit())
    }
}

// Integer types
macro_rules! impl_to_ron_int {
    ($($ty:ty),+ $(,)?) => {
        $(
            impl ToRon for $ty {
                fn to_ast(&self) -> Result<Expr<'static>> {
                    Ok(synthetic_integer(*self))
                }
            }
        )+
    };
}

impl_to_ron_int! {
    i8, i16, i32, i64,
    u8, u16, u32, u64,
}

#[cfg(feature = "integer128")]
impl_to_ron_int! {
    i128, u128,
}

// Float types
impl ToRon for f32 {
    fn to_ast(&self) -> Result<Expr<'static>> {
        Ok(synthetic_f32(*self))
    }
}

impl ToRon for f64 {
    fn to_ast(&self) -> Result<Expr<'static>> {
        Ok(synthetic_f64(*self))
    }
}

// =============================================================================
// FromRon implementations
// =============================================================================

impl FromRon for bool {
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        match expr {
            Expr::Bool(b) => Ok(b.value),
            _ => Err(spanned_type_mismatch("bool", expr)),
        }
    }
}

impl FromRon for char {
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        match expr {
            Expr::Char(c) => Ok(c.value),
            _ => Err(spanned_type_mismatch("char", expr)),
        }
    }
}

impl FromRon for String {
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        match expr {
            Expr::String(s) => Ok(s.value.clone()),
            _ => Err(spanned_type_mismatch("String", expr)),
        }
    }
}

impl FromRon for () {
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        match expr {
            Expr::Unit(_) => Ok(()),
            _ => Err(spanned_type_mismatch("()", expr)),
        }
    }
}

macro_rules! impl_from_ron_int {
    ($($ty:ty),+ $(,)?) => {
        $(
            impl FromRon for $ty {
                fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
                    match expr {
                        Expr::Number(n) => {
                            parse_integer_from_raw::<$ty>(&n.raw, &n.kind, stringify!($ty))
                                .map_err(|e| spanned_err(e, expr))
                        }
                        Expr::Byte(b) => {
                            // Byte literals can be converted to integers
                            <$ty>::try_from(b.value)
                                .map_err(|_| spanned_err(Error::IntegerOutOfBounds {
                                    value: alloc::format!("b'{}'", b.value as char).into(),
                                    target_type: stringify!($ty),
                                }, expr))
                        }
                        _ => Err(spanned_type_mismatch(stringify!($ty), expr)),
                    }
                }
            }
        )+
    };
}

impl_from_ron_int!(i8, i16, i32, i64, u8, u16, u32, u64);

#[cfg(feature = "integer128")]
impl_from_ron_int!(i128, u128);

impl FromRon for f32 {
    #[allow(clippy::cast_possible_truncation)]
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        match expr {
            Expr::Number(n) => {
                let val =
                    parse_float_from_raw(&n.raw, &n.kind).map_err(|e| spanned_err(e, expr))?;
                Ok(val as f32)
            }
            _ => Err(spanned_type_mismatch("f32", expr)),
        }
    }
}

impl FromRon for f64 {
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        match expr {
            Expr::Number(n) => {
                parse_float_from_raw(&n.raw, &n.kind).map_err(|e| spanned_err(e, expr))
            }
            _ => Err(spanned_type_mismatch("f64", expr)),
        }
    }
}
