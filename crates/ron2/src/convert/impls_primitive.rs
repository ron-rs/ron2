//! Primitive type implementations for `ToRon` and `FromRon`.

use alloc::string::String;

use crate::error::{Error, Result, SpannedResult};
use crate::ast::Expr;
use crate::value::{Number, Value};

use super::{ToRon, FromRon, spanned_type_mismatch, spanned_err};
use super::number::{parse_integer_from_raw, parse_float_from_raw};

// =============================================================================
// ToRon implementations
// =============================================================================

impl ToRon for bool {
    fn to_ron_value(&self) -> Result<Value> {
        Ok(Value::Bool(*self))
    }
}

impl ToRon for char {
    fn to_ron_value(&self) -> Result<Value> {
        Ok(Value::Char(*self))
    }
}

impl ToRon for String {
    fn to_ron_value(&self) -> Result<Value> {
        Ok(Value::String(self.clone()))
    }
}

impl ToRon for str {
    fn to_ron_value(&self) -> Result<Value> {
        Ok(Value::String(self.into()))
    }
}

impl ToRon for () {
    fn to_ron_value(&self) -> Result<Value> {
        Ok(Value::Unit)
    }
}

// Integer types
macro_rules! impl_to_ron_int {
    ($($ty:ty => $variant:ident),+ $(,)?) => {
        $(
            impl ToRon for $ty {
                fn to_ron_value(&self) -> Result<Value> {
                    Ok(Value::Number(Number::$variant(*self)))
                }
            }
        )+
    };
}

impl_to_ron_int! {
    i8 => I8,
    i16 => I16,
    i32 => I32,
    i64 => I64,
    u8 => U8,
    u16 => U16,
    u32 => U32,
    u64 => U64,
}

#[cfg(feature = "integer128")]
impl_to_ron_int! {
    i128 => I128,
    u128 => U128,
}

// Float types
impl ToRon for f32 {
    fn to_ron_value(&self) -> Result<Value> {
        Ok(Value::Number(Number::F32((*self).into())))
    }
}

impl ToRon for f64 {
    fn to_ron_value(&self) -> Result<Value> {
        Ok(Value::Number(Number::F64((*self).into())))
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
