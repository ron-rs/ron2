//! Schema file locations and I/O.
//!
//! A schema for the type `my_crate::config::AppConfig` lives at
//! `my_crate/config/AppConfig.schema.ron` below a schema directory. Lookups
//! search, in order: directories given by the caller, `RON_SCHEMA_DIR`, and
//! the XDG data directory (`xdg` feature).

use std::{env, path::PathBuf};
#[cfg(feature = "derive")]
use std::{fs, path::Path};

#[cfg(feature = "derive")]
use crate::schema::Schema;
use crate::schema::error::{Result, SchemaError, StorageError};
#[cfg(feature = "derive")]
use crate::{FromRon, ToRon};

/// Environment variable for schema directory override.
pub const SCHEMA_DIR_ENV: &str = "RON_SCHEMA_DIR";

/// Default schema directory name under XDG data dir.
#[cfg(feature = "xdg")]
const DEFAULT_SCHEMA_DIR: &str = "ron-schemas";

/// Schema file extension.
const SCHEMA_EXTENSION: &str = "schema.ron";

/// The directory schemas are written to when no output directory is given:
/// `RON_SCHEMA_DIR` if set, else the XDG data directory (`xdg` feature).
pub fn resolve_schema_dir() -> Result<PathBuf> {
    default_schema_dirs()
        .into_iter()
        .next()
        .ok_or_else(SchemaError::no_schema_dir)
}

/// The directories searched after caller-supplied ones, in precedence order:
/// `RON_SCHEMA_DIR` (if set), then the XDG data directory (`xdg` feature).
#[must_use]
pub fn default_schema_dirs() -> Vec<PathBuf> {
    let mut dirs = Vec::with_capacity(2);
    if let Some(dir) = env::var_os(SCHEMA_DIR_ENV) {
        dirs.push(PathBuf::from(dir));
    }
    #[cfg(feature = "xdg")]
    if let Some(data_dir) = dirs::data_dir() {
        dirs.push(data_dir.join(DEFAULT_SCHEMA_DIR));
    }
    dirs
}

/// Convert a Rust type path to a schema file path relative to a schema
/// directory.
///
/// Examples:
/// - `my_crate::config::AppConfig` -> `my_crate/config/AppConfig.schema.ron`
/// - `my_crate::W<x::Y<u8>, i32>` -> `my_crate/W%3Cx%3A%3AY%3Cu8%3E%2C%20i32%3E.schema.ron`
///
/// The path is split on `::` outside angle brackets. Every segment before the
/// last is a directory and must be a Rust identifier. The last segment is an
/// identifier, optionally followed by one balanced `<...>` group of generic
/// arguments (as in `core::any::type_name`, which derived generic types use
/// as their type path).
///
/// File names:
/// - A plain name is used as is, except that names Windows treats as devices
///   (`CON`, `NUL`, `COM1`, ..., case-insensitive) get a `-` appended
///   (`a::Nul` -> `a/Nul-.schema.ron`) on every platform, so schema
///   directories stay portable.
/// - A name with generic arguments is percent-encoded: every byte outside
///   `[A-Za-z0-9_]` becomes `%XX`, so `/`, `.` and `\` cannot form a path.
///
/// The mapping is injective: identifiers contain neither `-` nor `%`, and an
/// encoded name always contains `%3C` (`<`) and never `-`.
///
/// # Errors
///
/// Returns [`StorageError::InvalidTypePath`] for any other shape. This keeps a
/// path taken from a RON file's `#![type = "..."]` from escaping the schema
/// directory (`..`, `/`, absolute paths).
pub fn type_path_to_file_path(type_path: &str) -> Result<PathBuf> {
    let invalid = || SchemaError::Storage(StorageError::InvalidTypePath(type_path.to_string()));

    let (dirs, last) = split_type_path(type_path).ok_or_else(invalid)?;
    let mut path = PathBuf::new();
    for dir in dirs {
        if !is_identifier(dir) {
            return Err(invalid());
        }
        path.push(format!("{dir}{}", device_escape(dir)));
    }

    let file_stem = match last.find('<') {
        None if is_identifier(last) => format!("{last}{}", device_escape(last)),
        Some(open) if is_identifier(&last[..open]) && is_generic_args(&last[open..]) => {
            percent_encode(last)
        }
        _ => return Err(invalid()),
    };
    path.push(format!("{file_stem}.{SCHEMA_EXTENSION}"));
    Ok(path)
}

/// Split `type_path` on `::` outside angle brackets into the leading
/// segments and the last one. `None` if the brackets are unbalanced.
fn split_type_path(type_path: &str) -> Option<(Vec<&str>, &str)> {
    let bytes = type_path.as_bytes();
    let mut dirs = Vec::new();
    let mut depth = 0usize;
    let mut start = 0;
    let mut i = 0;
    while let Some(&byte) = bytes.get(i) {
        match byte {
            b'<' => depth += 1,
            // `->` in `fn(..) -> T` is not a closing bracket.
            b'>' if i == 0 || bytes.get(i - 1) != Some(&b'-') => depth = depth.checked_sub(1)?,
            b':' if depth == 0 && bytes.get(i + 1) == Some(&b':') => {
                dirs.push(type_path.get(start..i)?);
                i += 2;
                start = i;
                continue;
            }
            _ => {}
        }
        i += 1;
    }
    (depth == 0).then_some((dirs, type_path.get(start..)?))
}

/// Whether `s` is one balanced `<...>` group spanning all of `s`, free of
/// path separators and control characters.
fn is_generic_args(s: &str) -> bool {
    if s.chars().any(|c| c == '/' || c == '\\' || c.is_control()) {
        return false;
    }
    let bytes = s.as_bytes();
    let mut depth = 0usize;
    for (i, &byte) in bytes.iter().enumerate() {
        match byte {
            b'<' => depth += 1,
            b'>' if i == 0 || bytes.get(i - 1) != Some(&b'-') => {
                depth = match depth.checked_sub(1) {
                    Some(d) => d,
                    None => return false,
                };
                // The group must close exactly at the end.
                if depth == 0 && i + 1 != bytes.len() {
                    return false;
                }
            }
            _ => {}
        }
        if depth == 0 && byte != b'>' {
            // Text before the group opens (or after it closed).
            return false;
        }
    }
    depth == 0 && s.len() > 2
}

/// Percent-encode every byte outside `[A-Za-z0-9_]`.
fn percent_encode(s: &str) -> String {
    const HEX: &[u8; 16] = b"0123456789ABCDEF";
    let mut encoded = String::with_capacity(s.len() * 2);
    for byte in s.bytes() {
        if byte.is_ascii_alphanumeric() || byte == b'_' {
            encoded.push(char::from(byte));
        } else {
            encoded.push('%');
            encoded.push(char::from(HEX[usize::from(byte >> 4)]));
            encoded.push(char::from(HEX[usize::from(byte & 0xF)]));
        }
    }
    encoded
}

fn device_escape(segment: &str) -> &'static str {
    if is_windows_device_name(segment) {
        "-"
    } else {
        ""
    }
}

/// Whether Windows treats a file named `s` (with any extension) as a device.
fn is_windows_device_name(s: &str) -> bool {
    let upper = s.to_ascii_uppercase();
    match upper.as_str() {
        "CON" | "PRN" | "AUX" | "NUL" => true,
        _ => upper
            .strip_prefix("COM")
            .or_else(|| upper.strip_prefix("LPT"))
            .is_some_and(|n| matches!(n.as_bytes(), [d] if d.is_ascii_digit())),
    }
}

/// Whether `s` is a (possibly raw) Rust identifier.
fn is_identifier(s: &str) -> bool {
    let s = s.strip_prefix("r#").unwrap_or(s);
    let mut chars = s.chars();
    chars
        .next()
        .is_some_and(|c| c == '_' || unicode_ident::is_xid_start(c))
        && chars.all(unicode_ident::is_xid_continue)
}

/// Find the schema file for `type_path`.
///
/// Searches `extra_dirs` in order, then [`default_schema_dirs`]; the first
/// existing file wins.
///
/// # Errors
///
/// Returns [`StorageError::InvalidTypePath`] for a malformed type path and
/// [`StorageError::SchemaNotFound`] if no directory contains the file.
pub fn find_schema_file(type_path: &str, extra_dirs: &[PathBuf]) -> Result<PathBuf> {
    let relative = type_path_to_file_path(type_path)?;
    extra_dirs
        .iter()
        .cloned()
        .chain(default_schema_dirs())
        .map(|dir| dir.join(&relative))
        .find(|path| path.is_file())
        .ok_or_else(|| SchemaError::schema_not_found(type_path))
}

/// Write a schema to the configured schema directory.
///
/// If `output_dir` is provided, it takes precedence over environment variable and XDG default.
#[cfg(feature = "derive")]
pub fn write_schema(
    type_path: &str,
    schema: &Schema,
    output_dir: Option<&Path>,
) -> Result<PathBuf> {
    let base_dir = match output_dir {
        Some(dir) => dir.to_path_buf(),
        None => resolve_schema_dir()?,
    };

    let file_path = base_dir.join(type_path_to_file_path(type_path)?);

    if let Some(parent) = file_path.parent() {
        fs::create_dir_all(parent)?;
    }

    let contents = schema
        .to_ron()
        .map_err(|e| StorageError::Io(format!("serializing schema for {type_path}: {e}")))?;
    fs::write(&file_path, contents)?;

    Ok(file_path)
}

/// Read a schema from a file path.
#[cfg(feature = "derive")]
pub fn read_schema(path: &Path) -> Result<Schema> {
    let contents = fs::read_to_string(path)?;
    Schema::from_ron(&contents)
        .map_err(|e| SchemaError::Storage(StorageError::Parse(format!("{}: {e}", path.display()))))
}

/// Find and read the schema for `type_path`, searching `extra_dirs` first and
/// then [`default_schema_dirs`].
#[cfg(feature = "derive")]
pub fn find_schema(type_path: &str, extra_dirs: &[PathBuf]) -> Result<Schema> {
    read_schema(&find_schema_file(type_path, extra_dirs)?)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn type_path_maps_to_nested_file() {
        assert_eq!(
            type_path_to_file_path("my_crate::config::AppConfig").unwrap(),
            PathBuf::from("my_crate/config/AppConfig.schema.ron")
        );
        assert_eq!(
            type_path_to_file_path("MyType").unwrap(),
            PathBuf::from("MyType.schema.ron")
        );
        assert_eq!(
            type_path_to_file_path("r#type::Über").unwrap(),
            PathBuf::from("r#type/Über.schema.ron")
        );
    }

    #[test]
    fn windows_device_names_are_escaped() {
        assert_eq!(
            type_path_to_file_path("a::NUL").unwrap(),
            PathBuf::from("a/NUL-.schema.ron")
        );
        assert_eq!(
            type_path_to_file_path("con::Lpt1::com9::Aux").unwrap(),
            PathBuf::from("con-/Lpt1-/com9-/Aux-.schema.ron")
        );
        for plain in ["Console", "Com", "COM10", "Lpt", "Nul_"] {
            assert_eq!(
                type_path_to_file_path(plain).unwrap(),
                PathBuf::from(format!("{plain}.schema.ron"))
            );
        }
    }

    #[test]
    fn generic_type_paths_are_percent_encoded() {
        assert_eq!(
            type_path_to_file_path("my_crate::m::GTree<u8>").unwrap(),
            PathBuf::from("my_crate/m/GTree%3Cu8%3E.schema.ron")
        );
        assert_eq!(
            type_path_to_file_path("a::b::W<x::Y<u8>, alloc::string::String>").unwrap(),
            PathBuf::from(
                "a/b/W%3Cx%3A%3AY%3Cu8%3E%2C%20alloc%3A%3Astring%3A%3AString%3E.schema.ron"
            )
        );
        // `type_name` output for references, arrays, tuples and fn pointers.
        for path in [
            "a::W<&'static str>",
            "a::W<[u8; 3]>",
            "a::W<(i32, bool)>",
            "a::W<fn(u8) -> u8>",
            "a::Nul<u8>",
        ] {
            let file = type_path_to_file_path(path).unwrap();
            let name = file.file_name().unwrap().to_str().unwrap();
            assert!(
                name.bytes()
                    .all(|b| b.is_ascii_alphanumeric() || b"_%.".contains(&b)),
                "{path} -> {name}"
            );
            assert_eq!(file.parent(), Some(std::path::Path::new("a")));
        }
    }

    #[test]
    fn distinct_type_paths_map_to_distinct_files() {
        let paths = [
            "a::Nul",
            "a::NUL",
            "a::W<u8>",
            "a::W<u16>",
            "a::W<a::u8>",
            "a::W<a ::u8>",
            "a::W<x::Y<u8>>",
            "a::W<x::Y<u8> >",
            "a::Nul<u8>",
            "a::r#Nul",
            "a::W3Cu8",
        ];
        let files: std::collections::HashSet<_> = paths
            .iter()
            .map(|p| type_path_to_file_path(p).unwrap())
            .collect();
        assert_eq!(files.len(), paths.len());
    }

    #[test]
    fn type_path_rejects_non_identifiers() {
        for bad in [
            "",
            "a::..::..::etc::x",
            "..",
            "a::b/c",
            "/etc/passwd",
            "a::::b",
            "C:\\x",
            "1abc",
            "a b",
            // Generic arguments: balanced, one group, at the end, no paths.
            "a<u8>::B",
            "a::W<u8",
            "a::W<u8>>",
            "a::W<u8>x",
            "a::W<u8><i8>",
            "a::W<>",
            "a::<u8>",
            "a::W<../../etc/passwd>",
            "a::W<..\\x>",
            "a::W<\0>",
            "a::W<u8>::X",
        ] {
            assert!(
                matches!(
                    type_path_to_file_path(bad),
                    Err(SchemaError::Storage(StorageError::InvalidTypePath(_)))
                ),
                "{bad:?} should be rejected"
            );
        }
    }
}
