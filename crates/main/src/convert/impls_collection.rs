//! Collection type implementations for `ToRon` and `FromRon`.

use alloc::{
    collections::{BTreeMap, BTreeSet, LinkedList, VecDeque},
    vec::Vec,
};
use core::hash::{BuildHasher, Hash};
use std::collections::{HashMap, HashSet};

use indexmap::{IndexMap, IndexSet};

use super::{
    FromRon, ToRon, check_len, collect_seq, element, expect_map, read_map_entries, seq_elements,
};
use crate::{
    ast::{Expr, synthetic_map, synthetic_seq},
    error::{Error, ErrorKind, Result},
};

fn seq_to_ast<'i, T: ToRon + 'i>(items: impl Iterator<Item = &'i T>) -> Result<Expr<'static>> {
    Ok(synthetic_seq(
        items.map(ToRon::to_ast).collect::<Result<_>>()?,
    ))
}

fn map_to_ast<'i, K: ToRon + 'i, V: ToRon + 'i>(
    entries: impl Iterator<Item = (&'i K, &'i V)>,
) -> Result<Expr<'static>> {
    let entries = entries
        .map(|(k, v)| Ok((k.to_ast()?, v.to_ast()?)))
        .collect::<Result<_>>()?;
    Ok(synthetic_map(entries))
}

/// `ToRon` and `FromRon` for sequence-like collections.
macro_rules! seq_impls {
    ($($seq:ident<T $(, $s:ident)?> $(where [$($bounds:tt)*])?;)*) => {$(
        impl<T: ToRon $(, $s)?> ToRon for $seq<T $(, $s)?> {
            fn to_ast(&self) -> Result<Expr<'static>> {
                seq_to_ast(self.iter())
            }
        }

        impl<T: FromRon $(, $s)?> FromRon for $seq<T $(, $s)?> $(where $($bounds)*)? {
            fn from_ast(expr: &Expr<'_>) -> Result<Self> {
                collect_seq(expr)
            }
        }
    )*};
}

seq_impls! {
    Vec<T>;
    VecDeque<T>;
    LinkedList<T>;
    BTreeSet<T> where [T: Ord];
    HashSet<T, S> where [T: Eq + Hash, S: BuildHasher + Default];
    IndexSet<T, S> where [T: Eq + Hash, S: BuildHasher + Default];
}

impl<T: ToRon> ToRon for [T] {
    fn to_ast(&self) -> Result<Expr<'static>> {
        seq_to_ast(self.iter())
    }
}

impl<T: ToRon, const N: usize> ToRon for [T; N] {
    fn to_ast(&self) -> Result<Expr<'static>> {
        seq_to_ast(self.iter())
    }
}

impl<T: FromRon, const N: usize> FromRon for [T; N] {
    fn from_ast(expr: &Expr<'_>) -> Result<Self> {
        let elements = seq_elements(expr, "array")?;
        check_len(&elements, N, "array", expr)?;
        let items = elements.map(element).collect::<Result<Vec<T>>>()?;
        items.try_into().map_err(|_| {
            Error::with_span(
                ErrorKind::Message(alloc::format!("expected array of length {N}")),
                *expr.span(),
            )
        })
    }
}

/// `ToRon` and `FromRon` for map collections; repeated keys are an error.
macro_rules! map_impls {
    ($($map:ident<K, V $(, $s:ident)?> where [$($bounds:tt)*] = $new:expr;)*) => {$(
        impl<K: ToRon, V: ToRon $(, $s)?> ToRon for $map<K, V $(, $s)?> {
            fn to_ast(&self) -> Result<Expr<'static>> {
                map_to_ast(self.iter())
            }
        }

        impl<K: FromRon, V: FromRon $(, $s)?> FromRon for $map<K, V $(, $s)?>
        where
            $($bounds)*
        {
            fn from_ast(expr: &Expr<'_>) -> Result<Self> {
                let map = expect_map(expr)?;
                let new: fn(usize) -> Self = $new;
                let mut out = new(map.entries.len());
                read_map_entries(map, |k, v| out.insert(k, v))?;
                Ok(out)
            }
        }
    )*};
}

map_impls! {
    BTreeMap<K, V> where [K: Ord] = |_| BTreeMap::new();
    HashMap<K, V, S> where [K: Eq + Hash, S: BuildHasher + Default] =
        |n| HashMap::with_capacity_and_hasher(n, S::default());
    IndexMap<K, V, S> where [K: Eq + Hash, S: BuildHasher + Default] =
        |n| IndexMap::with_capacity_and_hasher(n, S::default());
}
