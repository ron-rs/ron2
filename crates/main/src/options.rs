//! Roundtrip Options module.

use alloc::vec::Vec;
use std::io;

use crate::{
    ast::{parse_document, to_value},
    error::{Error, SpannedError, SpannedResult},
    extensions::Extensions,
    value::Value,
};

/// Roundtrip options for serialization and deserialization.
#[derive(Clone, Debug)]
#[non_exhaustive]
pub struct Options {
    /// Extensions that are enabled by default during serialization and
    /// deserialization.
    pub default_extensions: Extensions,
    /// Default recursion limit that is checked during serialization and
    /// deserialization.
    /// If set to `None`, infinite recursion is allowed and stack overflow
    /// errors can crash the serialization or deserialization process.
    /// Defaults to `Some(128)`, i.e. 128 recursive calls are allowed.
    pub recursion_limit: Option<usize>,
}

impl Default for Options {
    fn default() -> Self {
        Self {
            default_extensions: Extensions::empty(),
            recursion_limit: Some(128),
        }
    }
}

impl Options {
    #[must_use]
    /// Enable `default_extension` by default during serialization and deserialization.
    pub fn with_default_extension(mut self, default_extension: Extensions) -> Self {
        self.default_extensions |= default_extension;
        self
    }

    #[must_use]
    /// Do NOT enable `default_extension` by default during serialization and deserialization.
    pub fn without_default_extension(mut self, default_extension: Extensions) -> Self {
        self.default_extensions &= !default_extension;
        self
    }

    #[must_use]
    /// Set a maximum recursion limit during serialization and deserialization.
    pub fn with_recursion_limit(mut self, recursion_limit: usize) -> Self {
        self.recursion_limit = Some(recursion_limit);
        self
    }

    #[must_use]
    /// Disable the recursion limit during serialization and deserialization.
    pub fn without_recursion_limit(mut self) -> Self {
        self.recursion_limit = None;
        self
    }
}

impl Options {
    /// A convenience function for building a deserializer
    /// and deserializing a Value from a reader.
    ///
    /// # Panics
    ///
    /// This function contains an `expect()` call that cannot panic in practice:
    /// when UTF-8 validation fails, we slice up to `valid_up_to()` which is
    /// guaranteed to be valid UTF-8 by the `Utf8Error` contract.
    pub fn from_reader<R>(&self, mut rdr: R) -> SpannedResult<Value>
    where
        R: io::Read,
    {
        let mut bytes = Vec::new();

        if let Err(io_err) = rdr.read_to_end(&mut bytes) {
            // Try to compute a good error position for the I/O error
            #[allow(clippy::expect_used)]
            let valid_input = match core::str::from_utf8(&bytes) {
                Ok(valid_input) => valid_input,
                Err(err) => core::str::from_utf8(&bytes[..err.valid_up_to()])
                    .expect("source is valid up to error"),
            };
            return Err(SpannedError::wrap(io_err.into(), valid_input));
        }

        self.from_bytes(&bytes)
    }

    /// Parse a RON string into a Value using the AST parser.
    pub fn from_str(&self, s: &str) -> SpannedResult<Value> {
        let doc = parse_document(s)?;

        match to_value(&doc) {
            Some(Ok(value)) => Ok(value),
            Some(Err(e)) => {
                // Conversion error - e already contains span information
                Err(e)
            }
            None => {
                // Empty document - return EOF error (consistent with FromRon::from_ron)
                Err(SpannedError::at_start(Error::Eof))
            }
        }
    }

    /// Parse RON bytes into a Value using the AST parser.
    pub fn from_bytes(&self, s: &[u8]) -> SpannedResult<Value> {
        let s = core::str::from_utf8(s).map_err(|e| SpannedError::at_start(Error::Utf8Error(e)))?;
        self.from_str(s)
    }
}
