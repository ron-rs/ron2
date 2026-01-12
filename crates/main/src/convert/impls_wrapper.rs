//! Wrapper type and tuple implementations for `ToRon` and `FromRon`.

use alloc::{borrow::Cow, boxed::Box, rc::Rc, sync::Arc};
use core::cell::{Cell, RefCell};

use super::{FromRon, ToRon, spanned_err, spanned_type_mismatch};
use crate::{
    ast::{Expr, synthetic_option, synthetic_tuple},
    error::{Error, Result, SpannedResult},
};

// =============================================================================
// Option
// =============================================================================

impl<T: ToRon> ToRon for Option<T> {
    fn to_ast(&self) -> Result<Expr<'static>> {
        match self {
            Some(v) => Ok(synthetic_option(Some(v.to_ast()?))),
            None => Ok(synthetic_option(None)),
        }
    }
}

impl<T: FromRon> FromRon for Option<T> {
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        match expr {
            Expr::Option(opt) => match &opt.value {
                None => Ok(None),
                Some(inner) => Ok(Some(T::from_ast(&inner.expr)?)),
            },
            // Also accept raw values as implicit Some
            other => Ok(Some(T::from_ast(other)?)),
        }
    }
}

// =============================================================================
// Box
// =============================================================================

impl<T: ToRon + ?Sized> ToRon for Box<T> {
    fn to_ast(&self) -> Result<Expr<'static>> {
        (**self).to_ast()
    }
}

impl<T: FromRon> FromRon for Box<T> {
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        Ok(Box::new(T::from_ast(expr)?))
    }
}

// =============================================================================
// References
// =============================================================================

impl<T: ToRon + ?Sized> ToRon for &T {
    fn to_ast(&self) -> Result<Expr<'static>> {
        (*self).to_ast()
    }
}

impl<T: ToRon + ?Sized> ToRon for &mut T {
    fn to_ast(&self) -> Result<Expr<'static>> {
        (**self).to_ast()
    }
}

// =============================================================================
// Cow
// =============================================================================

impl<T: ToRon + Clone> ToRon for Cow<'_, T> {
    fn to_ast(&self) -> Result<Expr<'static>> {
        self.as_ref().to_ast()
    }
}

impl<T: FromRon + Clone> FromRon for Cow<'static, T> {
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        Ok(Cow::Owned(T::from_ast(expr)?))
    }
}

// =============================================================================
// Rc and Arc
// =============================================================================

impl<T: ToRon> ToRon for Rc<T> {
    fn to_ast(&self) -> Result<Expr<'static>> {
        (**self).to_ast()
    }
}

impl<T: FromRon> FromRon for Rc<T> {
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        Ok(Rc::new(T::from_ast(expr)?))
    }
}

impl<T: ToRon> ToRon for Arc<T> {
    fn to_ast(&self) -> Result<Expr<'static>> {
        (**self).to_ast()
    }
}

impl<T: FromRon> FromRon for Arc<T> {
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        Ok(Arc::new(T::from_ast(expr)?))
    }
}

// =============================================================================
// Cell and RefCell
// =============================================================================

impl<T: ToRon + Copy> ToRon for Cell<T> {
    fn to_ast(&self) -> Result<Expr<'static>> {
        self.get().to_ast()
    }
}

impl<T: FromRon> FromRon for Cell<T> {
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        Ok(Cell::new(T::from_ast(expr)?))
    }
}

impl<T: ToRon> ToRon for RefCell<T> {
    fn to_ast(&self) -> Result<Expr<'static>> {
        self.borrow().to_ast()
    }
}

impl<T: FromRon> FromRon for RefCell<T> {
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        Ok(RefCell::new(T::from_ast(expr)?))
    }
}

// =============================================================================
// Tuple implementations
// =============================================================================

/// Helper to extract elements from a sequence-like expression.
fn extract_seq_elements<'a>(expr: &'a Expr<'a>) -> Option<alloc::vec::Vec<&'a Expr<'a>>> {
    match expr {
        Expr::Seq(seq) => Some(seq.items.iter().map(|item| &item.expr).collect()),
        Expr::Tuple(tuple) => Some(tuple.elements.iter().map(|elem| &elem.expr).collect()),
        _ => None,
    }
}

macro_rules! impl_to_ron_tuple {
    () => {};
    ($first:ident $(, $rest:ident)*) => {
        impl<$first: ToRon $(, $rest: ToRon)*> ToRon for ($first, $($rest,)*) {
            fn to_ast(&self) -> Result<Expr<'static>> {
                #[allow(non_snake_case)]
                let ($first, $($rest,)*) = self;
                Ok(synthetic_tuple(alloc::vec![
                    $first.to_ast()?,
                    $($rest.to_ast()?,)*
                ]))
            }
        }
        impl_to_ron_tuple!($($rest),*);
    };
}

impl_to_ron_tuple!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12);

macro_rules! impl_from_ron_tuple {
    () => {};
    ($first:ident $(, $rest:ident)*) => {
        impl<$first: FromRon $(, $rest: FromRon)*> FromRon for ($first, $($rest,)*) {
            fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
                let Some(elements) = extract_seq_elements(expr) else {
                    return Err(spanned_type_mismatch("tuple", expr));
                };
                #[allow(unused_variables)]
                let expected = impl_from_ron_tuple!(@count $first $(, $rest)*);
                if elements.len() != expected {
                    return Err(spanned_err(
                        Error::invalid_value(alloc::format!(
                            "expected tuple of {expected} elements, got {}",
                            elements.len()
                        )),
                        expr,
                    ));
                }
                let mut iter = elements.into_iter();
                Ok((
                    $first::from_ast(iter.next().ok_or_else(|| spanned_err(Error::invalid_value("tuple too short"), expr))?)?,
                    $($rest::from_ast(iter.next().ok_or_else(|| spanned_err(Error::invalid_value("tuple too short"), expr))?)?,)*
                ))
            }
        }
        impl_from_ron_tuple!($($rest),*);
    };
    (@count $first:ident $(, $rest:ident)*) => {
        1 $(+ impl_from_ron_tuple!(@count $rest))*
    };
    (@count) => { 0 };
}

impl_from_ron_tuple!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12);
