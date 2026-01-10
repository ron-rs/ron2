//! Number parsing helpers for `FromRon` implementations.

use crate::ast::NumberKind;
use crate::error::{Error, Result};

/// Result of parsing an integer from its raw string representation.
#[derive(Debug, Clone, Copy)]
pub struct ParsedInt {
    /// The absolute value of the parsed integer.
    pub magnitude: u128,
    /// Whether the integer was negative.
    pub negative: bool,
}

/// Determine base and strip prefix from a numeric string.
/// Returns `(base, digits_str)`.
fn determine_base_and_digits(s: &str) -> (u32, &str) {
    if let Some(d) = s.strip_prefix("0x").or_else(|| s.strip_prefix("0X")) {
        (16, d)
    } else if let Some(d) = s.strip_prefix("0b").or_else(|| s.strip_prefix("0B")) {
        (2, d)
    } else if let Some(d) = s.strip_prefix("0o").or_else(|| s.strip_prefix("0O")) {
        (8, d)
    } else {
        (10, s)
    }
}

/// Parse an integer from its raw string representation.
///
/// Handles decimal, hex (0x), binary (0b), octal (0o), and underscores.
/// Returns the magnitude and sign separately for flexible fitting into target types.
pub fn parse_int_raw(raw: &str) -> Result<ParsedInt> {
    let raw = raw.trim();
    let (negative, unsigned_raw) = match raw.strip_prefix('-') {
        Some(r) => (true, r),
        None => (false, raw),
    };

    let magnitude = if unsigned_raw.contains('_') {
        // Slow path: filter underscores
        let cleaned: alloc::string::String = unsigned_raw.chars().filter(|&c| c != '_').collect();
        let (base, digits) = determine_base_and_digits(&cleaned);
        u128::from_str_radix(digits, base)
    } else {
        // Fast path: no underscores (common case)
        let (base, digits) = determine_base_and_digits(unsigned_raw);
        u128::from_str_radix(digits, base)
    }
    .map_err(|_| Error::IntegerOutOfBounds {
        value: raw.to_string().into(),
        target_type: "u128",
    })?;

    Ok(ParsedInt {
        magnitude,
        negative,
    })
}

/// Parse an integer from a raw string representation into a specific type.
pub(crate) fn parse_integer_from_raw<T>(
    raw: &str,
    kind: &NumberKind,
    target_type: &'static str,
) -> Result<T>
where
    T: TryFrom<i128> + TryFrom<u128>,
{
    let raw_trimmed = raw.trim();
    match kind {
        NumberKind::Integer => {
            let parsed = parse_int_raw(raw)?;
            T::try_from(parsed.magnitude).map_err(|_| Error::IntegerOutOfBounds {
                value: raw_trimmed.to_string().into(),
                target_type,
            })
        }
        NumberKind::NegativeInteger => {
            let parsed = parse_int_raw(raw)?;
            // Convert magnitude to signed and negate
            let val = i128::try_from(parsed.magnitude).map_err(|_| Error::IntegerOutOfBounds {
                value: raw_trimmed.to_string().into(),
                target_type,
            })?;
            let val = -val;
            T::try_from(val).map_err(|_| Error::IntegerOutOfBounds {
                value: raw_trimmed.to_string().into(),
                target_type,
            })
        }
        NumberKind::Float | NumberKind::SpecialFloat => Err(Error::ExpectedInteger),
    }
}

/// Parse a float from a raw string representation.
pub(crate) fn parse_float_from_raw(raw: &str, kind: &NumberKind) -> Result<f64> {
    let raw = raw.trim();
    match kind {
        NumberKind::SpecialFloat => match raw {
            "inf" => Ok(f64::INFINITY),
            "-inf" => Ok(f64::NEG_INFINITY),
            "NaN" => Ok(f64::NAN),
            _ => Err(Error::ExpectedFloat),
        },
        NumberKind::Float | NumberKind::Integer | NumberKind::NegativeInteger => {
            if raw.contains('_') {
                // Slow path: filter underscores
                let cleaned: alloc::string::String = raw.chars().filter(|&c| c != '_').collect();
                cleaned.parse().map_err(|_| Error::ExpectedFloat)
            } else {
                // Fast path: no underscores (common case)
                raw.parse().map_err(|_| Error::ExpectedFloat)
            }
        }
    }
}
