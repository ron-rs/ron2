//! Collection type implementations for `ToRon` and `FromRon`.

use alloc::{
    collections::{BTreeMap, BTreeSet, LinkedList, VecDeque},
    format,
    vec::Vec,
};
use core::hash::{BuildHasher, Hash};
use std::collections::{HashMap, HashSet};

use super::{FromRon, ToRon, extract_seq_elements, invalid_value, spanned_err, spanned_type_mismatch};
use crate::{
    ast::{Expr, synthetic_map, synthetic_seq},
    error::Result,
};

// =============================================================================
// ToRon implementations
// =============================================================================

impl<T: ToRon> ToRon for Vec<T> {
    fn to_ast(&self) -> Result<Expr<'static>> {
        let items: Result<Vec<_>> = self.iter().map(ToRon::to_ast).collect();
        Ok(synthetic_seq(items?))
    }
}

impl<T: ToRon> ToRon for VecDeque<T> {
    fn to_ast(&self) -> Result<Expr<'static>> {
        let items: Result<Vec<_>> = self.iter().map(ToRon::to_ast).collect();
        Ok(synthetic_seq(items?))
    }
}

impl<T: ToRon> ToRon for LinkedList<T> {
    fn to_ast(&self) -> Result<Expr<'static>> {
        let items: Result<Vec<_>> = self.iter().map(ToRon::to_ast).collect();
        Ok(synthetic_seq(items?))
    }
}

impl<T: ToRon + Eq + Hash, S: BuildHasher> ToRon for HashSet<T, S> {
    fn to_ast(&self) -> Result<Expr<'static>> {
        let items: Result<Vec<_>> = self.iter().map(ToRon::to_ast).collect();
        Ok(synthetic_seq(items?))
    }
}

impl<T: ToRon + Ord> ToRon for BTreeSet<T> {
    fn to_ast(&self) -> Result<Expr<'static>> {
        let items: Result<Vec<_>> = self.iter().map(ToRon::to_ast).collect();
        Ok(synthetic_seq(items?))
    }
}

impl<T: ToRon, const N: usize> ToRon for [T; N] {
    fn to_ast(&self) -> Result<Expr<'static>> {
        let items: Result<Vec<_>> = self.iter().map(ToRon::to_ast).collect();
        Ok(synthetic_seq(items?))
    }
}

impl<T: ToRon> ToRon for [T] {
    fn to_ast(&self) -> Result<Expr<'static>> {
        let items: Result<Vec<_>> = self.iter().map(ToRon::to_ast).collect();
        Ok(synthetic_seq(items?))
    }
}

// Map types
impl<K: ToRon + Eq + Hash, V: ToRon, S: BuildHasher> ToRon for HashMap<K, V, S> {
    fn to_ast(&self) -> Result<Expr<'static>> {
        let entries: Result<Vec<_>> = self
            .iter()
            .map(|(k, v)| Ok((k.to_ast()?, v.to_ast()?)))
            .collect();
        Ok(synthetic_map(entries?))
    }
}

impl<K: ToRon + Ord, V: ToRon> ToRon for BTreeMap<K, V> {
    fn to_ast(&self) -> Result<Expr<'static>> {
        let entries: Result<Vec<_>> = self
            .iter()
            .map(|(k, v)| Ok((k.to_ast()?, v.to_ast()?)))
            .collect();
        Ok(synthetic_map(entries?))
    }
}

impl<K: ToRon + Eq + Hash, V: ToRon, S: core::hash::BuildHasher> ToRon
    for indexmap::IndexMap<K, V, S>
{
    fn to_ast(&self) -> Result<Expr<'static>> {
        let entries: Result<Vec<_>> = self
            .iter()
            .map(|(k, v)| Ok((k.to_ast()?, v.to_ast()?)))
            .collect();
        Ok(synthetic_map(entries?))
    }
}

impl<T: ToRon + Eq + Hash, S: core::hash::BuildHasher> ToRon for indexmap::IndexSet<T, S> {
    fn to_ast(&self) -> Result<Expr<'static>> {
        let items: Result<Vec<_>> = self.iter().map(ToRon::to_ast).collect();
        Ok(synthetic_seq(items?))
    }
}

// =============================================================================
// FromRon implementations
// =============================================================================

impl<T: FromRon> FromRon for Vec<T> {
    fn from_ast(expr: &Expr<'_>) -> Result<Self> {
        match extract_seq_elements(expr) {
            Some(elements) => elements.into_iter().map(T::from_ast).collect(),
            None => Err(spanned_type_mismatch("sequence", expr)),
        }
    }
}

impl<T: FromRon> FromRon for VecDeque<T> {
    fn from_ast(expr: &Expr<'_>) -> Result<Self> {
        match extract_seq_elements(expr) {
            Some(elements) => elements.into_iter().map(T::from_ast).collect(),
            None => Err(spanned_type_mismatch("sequence", expr)),
        }
    }
}

impl<T: FromRon> FromRon for LinkedList<T> {
    fn from_ast(expr: &Expr<'_>) -> Result<Self> {
        match extract_seq_elements(expr) {
            Some(elements) => elements.into_iter().map(T::from_ast).collect(),
            None => Err(spanned_type_mismatch("sequence", expr)),
        }
    }
}

impl<T: FromRon + Eq + Hash, S: BuildHasher + Default> FromRon for HashSet<T, S> {
    fn from_ast(expr: &Expr<'_>) -> Result<Self> {
        match extract_seq_elements(expr) {
            Some(elements) => elements
                .into_iter()
                .map(T::from_ast)
                .collect::<Result<HashSet<T, S>>>(),
            None => Err(spanned_type_mismatch("sequence", expr)),
        }
    }
}

impl<T: FromRon + Ord> FromRon for BTreeSet<T> {
    fn from_ast(expr: &Expr<'_>) -> Result<Self> {
        match extract_seq_elements(expr) {
            Some(elements) => elements.into_iter().map(T::from_ast).collect(),
            None => Err(spanned_type_mismatch("sequence", expr)),
        }
    }
}

impl<T: FromRon, const N: usize> FromRon for [T; N] {
    fn from_ast(expr: &Expr<'_>) -> Result<Self> {
        match extract_seq_elements(expr) {
            Some(elements) => {
                if elements.len() != N {
                    return Err(spanned_err(
                        invalid_value(format!(
                            "expected array of length {N}, got {}",
                            elements.len()
                        )),
                        expr,
                    ));
                }
                let vec: Vec<T> = elements
                    .into_iter()
                    .map(T::from_ast)
                    .collect::<Result<_>>()?;
                vec.try_into().map_err(|_| {
                    spanned_err(
                        invalid_value(format!("failed to convert to array of length {N}")),
                        expr,
                    )
                })
            }
            None => Err(spanned_type_mismatch("array", expr)),
        }
    }
}

// Map types
impl<K: FromRon + Eq + Hash, V: FromRon, S: BuildHasher + Default> FromRon for HashMap<K, V, S> {
    fn from_ast(expr: &Expr<'_>) -> Result<Self> {
        match expr {
            Expr::Map(map) => {
                let mut result =
                    HashMap::with_capacity_and_hasher(map.entries.len(), Default::default());
                for entry in &map.entries {
                    let k = K::from_ast(&entry.key)?;
                    let v = V::from_ast(&entry.value)?;
                    result.insert(k, v);
                }
                Ok(result)
            }
            _ => Err(spanned_type_mismatch("map", expr)),
        }
    }
}

impl<K: FromRon + Ord, V: FromRon> FromRon for BTreeMap<K, V> {
    fn from_ast(expr: &Expr<'_>) -> Result<Self> {
        match expr {
            Expr::Map(map) => {
                let mut result = BTreeMap::new();
                for entry in &map.entries {
                    let k = K::from_ast(&entry.key)?;
                    let v = V::from_ast(&entry.value)?;
                    result.insert(k, v);
                }
                Ok(result)
            }
            _ => Err(spanned_type_mismatch("map", expr)),
        }
    }
}

impl<K: FromRon + Eq + Hash, V: FromRon, S: core::hash::BuildHasher + Default> FromRon
    for indexmap::IndexMap<K, V, S>
{
    fn from_ast(expr: &Expr<'_>) -> Result<Self> {
        match expr {
            Expr::Map(map) => {
                let mut result =
                    indexmap::IndexMap::with_capacity_and_hasher(map.entries.len(), S::default());
                for entry in &map.entries {
                    let k = K::from_ast(&entry.key)?;
                    let v = V::from_ast(&entry.value)?;
                    result.insert(k, v);
                }
                Ok(result)
            }
            _ => Err(spanned_type_mismatch("map", expr)),
        }
    }
}

impl<T: FromRon + Eq + Hash, S: core::hash::BuildHasher + Default> FromRon
    for indexmap::IndexSet<T, S>
{
    fn from_ast(expr: &Expr<'_>) -> Result<Self> {
        match extract_seq_elements(expr) {
            Some(elements) => elements.into_iter().map(T::from_ast).collect::<Result<_>>(),
            None => Err(spanned_type_mismatch("sequence", expr)),
        }
    }
}
