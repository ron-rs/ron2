//! Collection type implementations for `ToRon` and `FromRon`.

use alloc::{
    collections::{BTreeMap, BTreeSet, LinkedList, VecDeque},
    format,
    vec::Vec,
};
use core::hash::Hash;

#[cfg(feature = "std")]
use core::hash::BuildHasher;

#[cfg(feature = "std")]
use std::collections::{HashMap, HashSet};

use crate::ast::Expr;
use crate::error::{Error, Result, SpannedResult};
use crate::value::{Map, Value};

use super::{FromRon, ToRon, spanned_err, spanned_type_mismatch};

// =============================================================================
// ToRon implementations
// =============================================================================

impl<T: ToRon> ToRon for Vec<T> {
    fn to_ron_value(&self) -> Result<Value> {
        let values: Result<Vec<_>> = self.iter().map(ToRon::to_ron_value).collect();
        Ok(Value::Seq(values?))
    }
}

impl<T: ToRon> ToRon for VecDeque<T> {
    fn to_ron_value(&self) -> Result<Value> {
        let values: Result<Vec<_>> = self.iter().map(ToRon::to_ron_value).collect();
        Ok(Value::Seq(values?))
    }
}

impl<T: ToRon> ToRon for LinkedList<T> {
    fn to_ron_value(&self) -> Result<Value> {
        let values: Result<Vec<_>> = self.iter().map(ToRon::to_ron_value).collect();
        Ok(Value::Seq(values?))
    }
}

#[cfg(feature = "std")]
impl<T: ToRon + Eq + Hash, S: BuildHasher> ToRon for HashSet<T, S> {
    fn to_ron_value(&self) -> Result<Value> {
        let values: Result<Vec<_>> = self.iter().map(ToRon::to_ron_value).collect();
        Ok(Value::Seq(values?))
    }
}

impl<T: ToRon + Ord> ToRon for BTreeSet<T> {
    fn to_ron_value(&self) -> Result<Value> {
        let values: Result<Vec<_>> = self.iter().map(ToRon::to_ron_value).collect();
        Ok(Value::Seq(values?))
    }
}

impl<T: ToRon, const N: usize> ToRon for [T; N] {
    fn to_ron_value(&self) -> Result<Value> {
        let values: Result<Vec<_>> = self.iter().map(ToRon::to_ron_value).collect();
        Ok(Value::Seq(values?))
    }
}

impl<T: ToRon> ToRon for [T] {
    fn to_ron_value(&self) -> Result<Value> {
        let values: Result<Vec<_>> = self.iter().map(ToRon::to_ron_value).collect();
        Ok(Value::Seq(values?))
    }
}

// Map types
#[cfg(feature = "std")]
impl<K: ToRon + Eq + Hash, V: ToRon, S: BuildHasher> ToRon for HashMap<K, V, S> {
    fn to_ron_value(&self) -> Result<Value> {
        let mut map = Map::new();
        for (k, v) in self {
            map.insert(k.to_ron_value()?, v.to_ron_value()?);
        }
        Ok(Value::Map(map))
    }
}

impl<K: ToRon + Ord, V: ToRon> ToRon for BTreeMap<K, V> {
    fn to_ron_value(&self) -> Result<Value> {
        let mut map = Map::new();
        for (k, v) in self {
            map.insert(k.to_ron_value()?, v.to_ron_value()?);
        }
        Ok(Value::Map(map))
    }
}

impl<K: ToRon + Eq + Hash, V: ToRon, S: core::hash::BuildHasher> ToRon
    for indexmap::IndexMap<K, V, S>
{
    fn to_ron_value(&self) -> Result<Value> {
        let mut map = Map::new();
        for (k, v) in self {
            map.insert(k.to_ron_value()?, v.to_ron_value()?);
        }
        Ok(Value::Map(map))
    }
}

impl<T: ToRon + Eq + Hash, S: core::hash::BuildHasher> ToRon for indexmap::IndexSet<T, S> {
    fn to_ron_value(&self) -> Result<Value> {
        let values: Result<Vec<_>> = self.iter().map(ToRon::to_ron_value).collect();
        Ok(Value::Seq(values?))
    }
}

// =============================================================================
// FromRon implementations
// =============================================================================

/// Helper to extract elements from a sequence-like expression.
fn extract_seq_elements<'a>(expr: &'a Expr<'a>) -> Option<Vec<&'a Expr<'a>>> {
    match expr {
        Expr::Seq(seq) => Some(seq.items.iter().map(|item| &item.expr).collect()),
        Expr::Tuple(tuple) => Some(tuple.elements.iter().map(|elem| &elem.expr).collect()),
        _ => None,
    }
}

impl<T: FromRon> FromRon for Vec<T> {
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        match extract_seq_elements(expr) {
            Some(elements) => elements.into_iter().map(T::from_ast).collect(),
            None => Err(spanned_type_mismatch("sequence", expr)),
        }
    }
}

impl<T: FromRon> FromRon for VecDeque<T> {
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        match extract_seq_elements(expr) {
            Some(elements) => elements.into_iter().map(T::from_ast).collect(),
            None => Err(spanned_type_mismatch("sequence", expr)),
        }
    }
}

impl<T: FromRon> FromRon for LinkedList<T> {
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        match extract_seq_elements(expr) {
            Some(elements) => elements.into_iter().map(T::from_ast).collect(),
            None => Err(spanned_type_mismatch("sequence", expr)),
        }
    }
}

#[cfg(feature = "std")]
impl<T: FromRon + Eq + Hash, S: BuildHasher + Default> FromRon for HashSet<T, S> {
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        match extract_seq_elements(expr) {
            Some(elements) => elements
                .into_iter()
                .map(T::from_ast)
                .collect::<SpannedResult<HashSet<T, S>>>(),
            None => Err(spanned_type_mismatch("sequence", expr)),
        }
    }
}

impl<T: FromRon + Ord> FromRon for BTreeSet<T> {
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        match extract_seq_elements(expr) {
            Some(elements) => elements.into_iter().map(T::from_ast).collect(),
            None => Err(spanned_type_mismatch("sequence", expr)),
        }
    }
}

impl<T: FromRon, const N: usize> FromRon for [T; N] {
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        match extract_seq_elements(expr) {
            Some(elements) => {
                if elements.len() != N {
                    return Err(spanned_err(
                        Error::invalid_value(format!(
                            "expected array of length {N}, got {}",
                            elements.len()
                        )),
                        expr,
                    ));
                }
                let vec: Vec<T> = elements
                    .into_iter()
                    .map(T::from_ast)
                    .collect::<SpannedResult<_>>()?;
                vec.try_into().map_err(|_| {
                    spanned_err(
                        Error::invalid_value(format!("failed to convert to array of length {N}")),
                        expr,
                    )
                })
            }
            None => Err(spanned_type_mismatch("array", expr)),
        }
    }
}

// Map types
#[cfg(feature = "std")]
impl<K: FromRon + Eq + Hash, V: FromRon, S: BuildHasher + Default> FromRon for HashMap<K, V, S> {
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
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
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
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
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
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
    fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
        match extract_seq_elements(expr) {
            Some(elements) => elements
                .into_iter()
                .map(T::from_ast)
                .collect::<SpannedResult<_>>(),
            None => Err(spanned_type_mismatch("sequence", expr)),
        }
    }
}
