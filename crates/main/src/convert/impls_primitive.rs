//! Primitive type implementations for `ToRon` and `FromRon`.

use alloc::string::String;
use std::{
    ffi::{OsStr, OsString},
    path::{Path, PathBuf},
};

use super::{
    FromRon, ToRon,
    number::{expr_to_float, expr_to_int},
    spanned_type_mismatch,
};
use crate::{
    ast::{
        Expr, synthetic_bool, synthetic_char, synthetic_f32, synthetic_f64, synthetic_integer,
        synthetic_string, synthetic_unit,
    },
    error::{Error, ErrorKind, Result},
};

impl ToRon for bool {
    fn to_ast(&self) -> Result<Expr<'static>> {
        Ok(synthetic_bool(*self))
    }
}

impl FromRon for bool {
    fn from_ast(expr: &Expr<'_>) -> Result<Self> {
        match expr {
            Expr::Bool(b) => Ok(b.value),
            _ => Err(spanned_type_mismatch("bool", expr)),
        }
    }
}

impl ToRon for char {
    fn to_ast(&self) -> Result<Expr<'static>> {
        Ok(synthetic_char(*self))
    }
}

impl FromRon for char {
    fn from_ast(expr: &Expr<'_>) -> Result<Self> {
        match expr {
            Expr::Char(c) => Ok(c.value),
            _ => Err(spanned_type_mismatch("char", expr)),
        }
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

impl FromRon for String {
    fn from_ast(expr: &Expr<'_>) -> Result<Self> {
        match expr {
            Expr::String(s) => Ok(s.value.clone()),
            _ => Err(spanned_type_mismatch("String", expr)),
        }
    }
}

/// Paths and OS strings are RON strings; non-UTF-8 ones cannot be written.
macro_rules! impl_os_string {
    ($($owned:ty, $borrowed:ty, $what:literal;)*) => {$(
        impl ToRon for $borrowed {
            fn to_ast(&self) -> Result<Expr<'static>> {
                match self.to_str() {
                    Some(s) => Ok(synthetic_string(s.into())),
                    None => Err(Error::new(ErrorKind::Message(alloc::format!(
                        "{} {:?} is not valid UTF-8",
                        $what, self
                    )))),
                }
            }
        }

        impl ToRon for $owned {
            fn to_ast(&self) -> Result<Expr<'static>> {
                (**self).to_ast()
            }
        }

        impl FromRon for $owned {
            fn from_ast(expr: &Expr<'_>) -> Result<Self> {
                String::from_ast(expr).map(Self::from)
            }
        }
    )*};
}

impl_os_string! {
    PathBuf, Path, "path";
    OsString, OsStr, "OS string";
}

impl ToRon for () {
    fn to_ast(&self) -> Result<Expr<'static>> {
        Ok(synthetic_unit())
    }
}

impl FromRon for () {
    fn from_ast(expr: &Expr<'_>) -> Result<Self> {
        match expr {
            Expr::Unit(_) => Ok(()),
            _ => Err(spanned_type_mismatch("()", expr)),
        }
    }
}

macro_rules! impl_int {
    ($($ty:ty),+ $(,)?) => {
        $(
            impl ToRon for $ty {
                fn to_ast(&self) -> Result<Expr<'static>> {
                    Ok(synthetic_integer(*self))
                }
            }

            impl FromRon for $ty {
                fn from_ast(expr: &Expr<'_>) -> Result<Self> {
                    expr_to_int(expr, stringify!($ty))
                }
            }
        )+
    };
}

impl_int!(i8, i16, i32, i64, isize, u8, u16, u32, u64, usize);

#[cfg(feature = "integer128")]
impl_int!(i128, u128);

macro_rules! impl_float {
    ($($ty:ty: $synthetic:ident),+) => {
        $(
            impl ToRon for $ty {
                fn to_ast(&self) -> Result<Expr<'static>> {
                    Ok($synthetic(*self))
                }
            }

            impl FromRon for $ty {
                fn from_ast(expr: &Expr<'_>) -> Result<Self> {
                    expr_to_float(expr)
                }
            }
        )+
    };
}

impl_float!(f32: synthetic_f32, f64: synthetic_f64);
