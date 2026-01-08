//! Deserialization module - parses RON text into Value.

use alloc::{boxed::Box, string::String, vec::Vec};

use crate::{
    error::{Error, Result, SpannedError, SpannedResult},
    extensions::Extensions,
    parse::{ParsedByteStr, ParsedStr, Parser},
    value::{Map, Value},
    Options,
};

/// Deserializer for parsing RON into Value.
pub struct Deserializer<'a> {
    parser: Parser<'a>,
    remaining_depth: Option<usize>,
}

impl<'a> Deserializer<'a> {
    /// Create a new Deserializer from a string.
    pub fn new(s: &'a str) -> SpannedResult<Self> {
        Self::from_str_with_options(s, &Options::default())
    }

    /// Create a new Deserializer from a string with options.
    pub fn from_str_with_options(s: &'a str, options: &Options) -> SpannedResult<Self> {
        let mut parser = Parser::new(s)?;
        parser.exts |= options.default_extensions;

        Ok(Self {
            parser,
            remaining_depth: options.recursion_limit,
        })
    }

    /// Create a new Deserializer from bytes.
    pub fn from_bytes(s: &'a [u8]) -> SpannedResult<Self> {
        Self::from_bytes_with_options(s, &Options::default())
    }

    /// Create a new Deserializer from bytes with options.
    pub fn from_bytes_with_options(s: &'a [u8], options: &Options) -> SpannedResult<Self> {
        let s = core::str::from_utf8(s).map_err(|e| SpannedError {
            code: Error::Utf8Error(e),
            span: crate::error::Span {
                start: crate::error::Position { line: 1, col: 1 },
                end: crate::error::Position { line: 1, col: 1 },
                start_offset: 0,
                end_offset: 0,
            },
        })?;
        Self::from_str_with_options(s, options)
    }

    /// Create a spanned error at the current position.
    #[must_use]
    pub fn span_error(&self, code: Error) -> SpannedError {
        self.parser.span_error(code)
    }

    /// Check that parsing is complete.
    pub fn end(&mut self) -> Result<()> {
        self.parser.skip_ws()?;
        if self.parser.src().is_empty() {
            Ok(())
        } else {
            Err(Error::TrailingCharacters)
        }
    }

    /// Returns the extensions that are active during parsing.
    #[must_use]
    pub fn extensions(&self) -> Extensions {
        self.parser.exts
    }

    fn enter_recursion(&mut self) -> Result<()> {
        if let Some(ref mut remaining) = self.remaining_depth {
            if *remaining == 0 {
                return Err(Error::ExceededRecursionLimit);
            }
            *remaining -= 1;
        }
        Ok(())
    }

    fn exit_recursion(&mut self) {
        if let Some(ref mut remaining) = self.remaining_depth {
            *remaining += 1;
        }
    }

    /// Parse a single Value from the input.
    pub fn parse_value(&mut self) -> Result<Value> {
        self.enter_recursion()?;
        let result = self.parse_value_inner();
        self.exit_recursion();
        result
    }

    fn parse_value_inner(&mut self) -> Result<Value> {
        self.parser.skip_ws()?;

        // Check for None
        if self.parser.consume_ident("None") {
            return Ok(Value::Option(None));
        }

        // Check for Some
        if self.parser.consume_ident("Some") {
            self.parser.skip_ws()?;
            self.parser.expect_char('(', Error::ExpectedOption)?;
            self.parser.skip_ws()?;
            let value = self.parse_value()?;
            self.parser.skip_ws()?;
            self.parser.expect_char(')', Error::ExpectedOptionEnd)?;
            return Ok(Value::Option(Some(Box::new(value))));
        }

        // Check for bool
        if self.parser.check_ident("true") || self.parser.check_ident("false") {
            return Ok(Value::Bool(self.parser.bool()?));
        }

        // Check for char
        if self.parser.check_char('\'') {
            return Ok(Value::Char(self.parser.char()?));
        }

        // Check for string
        if self.parser.check_char('"') || self.parser.check_char('r') {
            let s = self.parser.string()?;
            return Ok(Value::String(match s {
                ParsedStr::Allocated(s) => s,
                ParsedStr::Slice(s) => String::from(s),
            }));
        }

        // Check for byte string
        if self.parser.check_str("b\"") || self.parser.check_str("br") {
            let bytes = self.parser.byte_string_no_base64()?;
            return Ok(Value::Bytes(match bytes {
                ParsedByteStr::Allocated(b) => b,
                ParsedByteStr::Slice(b) => Vec::from(b),
            }));
        }

        // Check for sequence (array)
        if self.parser.consume_char('[') {
            return self.parse_seq();
        }

        // Check for map
        if self.parser.consume_char('{') {
            return self.parse_map();
        }

        // Check for tuple / struct / unit
        if self.parser.check_char('(') {
            self.parser.skip_next_char();
            return self.parse_tuple_or_struct();
        }

        // Check for named struct/enum (identifier followed by tuple/struct)
        if let Some(ident) = self.parser.skip_identifier() {
            self.parser.skip_ws()?;

            // Check if this is a unit variant
            if !self.parser.check_char('(') {
                // It's a unit identifier - we represent this as a string
                return Ok(Value::String(String::from(ident)));
            }

            // It's a named struct or enum variant with data
            self.parser.skip_next_char();
            self.parser.skip_ws()?;

            // Parse as map with struct name as a special "__type" entry
            // Or just parse the contents based on what's there
            let mut map = Map::new();
            map.insert(Value::String(String::from("__type")), Value::String(String::from(ident)));

            if self.parser.consume_char(')') {
                // Empty named struct
                return Ok(Value::Map(map));
            }

            // Check if it's a named struct (has colons) or tuple struct
            let has_colon = self.check_struct_has_named_fields();

            if has_colon {
                // Named struct fields
                self.parse_struct_fields_into(&mut map)?;
            } else {
                // Tuple struct - store values as numbered fields
                let mut index = 0;
                loop {
                    let value = self.parse_value()?;
                    map.insert(
                        Value::String(alloc::format!("{index}")),
                        value,
                    );
                    index += 1;

                    self.parser.skip_ws()?;
                    if self.parser.consume_char(')') {
                        break;
                    }
                    self.parser.expect_char(',', Error::ExpectedComma)?;
                    self.parser.skip_ws()?;

                    // Trailing comma check
                    if self.parser.consume_char(')') {
                        break;
                    }
                }
            }

            return Ok(Value::Map(map));
        }

        // Try to parse as number
        Ok(Value::Number(self.parser.any_number()?))
    }

    fn check_struct_has_named_fields(&mut self) -> bool {
        // Look ahead to see if there's an identifier followed by colon
        let src = self.parser.src();
        let mut chars = src.chars().peekable();

        // Skip whitespace
        while let Some(&c) = chars.peek() {
            if c.is_whitespace() {
                chars.next();
            } else {
                break;
            }
        }

        // Skip identifier
        if let Some(&c) = chars.peek()
            && crate::parse::is_ident_first_char(c)
        {
            chars.next();
            while let Some(&c) = chars.peek() {
                if unicode_ident::is_xid_continue(c) {
                    chars.next();
                } else {
                    break;
                }
            }

            // Skip whitespace again
            while let Some(&c) = chars.peek() {
                if c.is_whitespace() {
                    chars.next();
                } else {
                    break;
                }
            }

            // Check for colon
            if let Some(&':') = chars.peek() {
                return true;
            }
        }

        false
    }

    fn parse_struct_fields_into(&mut self, map: &mut Map) -> Result<()> {
        loop {
            self.parser.skip_ws()?;

            if self.parser.consume_char(')') {
                break;
            }

            let ident = self.parser.identifier()?;
            self.parser.skip_ws()?;
            self.parser.expect_char(':', Error::ExpectedMapColon)?;
            self.parser.skip_ws()?;

            let value = self.parse_value()?;
            map.insert(Value::String(String::from(ident)), value);

            self.parser.skip_ws()?;

            if self.parser.consume_char(')') {
                break;
            }
            self.parser.expect_char(',', Error::ExpectedComma)?;
        }

        Ok(())
    }

    fn parse_seq(&mut self) -> Result<Value> {
        let mut seq = Vec::new();

        loop {
            self.parser.skip_ws()?;

            if self.parser.consume_char(']') {
                break;
            }

            seq.push(self.parse_value()?);

            self.parser.skip_ws()?;

            if self.parser.consume_char(']') {
                break;
            }
            self.parser.expect_char(',', Error::ExpectedComma)?;
        }

        Ok(Value::Seq(seq))
    }

    fn parse_map(&mut self) -> Result<Value> {
        let mut map = Map::new();

        loop {
            self.parser.skip_ws()?;

            if self.parser.consume_char('}') {
                break;
            }

            let key = self.parse_value()?;
            self.parser.skip_ws()?;
            self.parser.expect_char(':', Error::ExpectedMapColon)?;
            self.parser.skip_ws()?;
            let value = self.parse_value()?;

            map.insert(key, value);

            self.parser.skip_ws()?;

            if self.parser.consume_char('}') {
                break;
            }
            self.parser.expect_char(',', Error::ExpectedComma)?;
        }

        Ok(Value::Map(map))
    }

    fn parse_tuple_or_struct(&mut self) -> Result<Value> {
        self.parser.skip_ws()?;

        // Empty tuple = unit
        if self.parser.consume_char(')') {
            return Ok(Value::Unit);
        }

        // Check if this is a struct (identifier followed by colon) or tuple
        let has_colon = self.check_struct_has_named_fields();

        if has_colon {
            // It's a struct
            let mut map = Map::new();
            self.parse_struct_fields_into(&mut map)?;
            Ok(Value::Map(map))
        } else {
            // It's a tuple - parse as sequence
            let mut seq = Vec::new();

            loop {
                seq.push(self.parse_value()?);

                self.parser.skip_ws()?;

                if self.parser.consume_char(')') {
                    break;
                }
                self.parser.expect_char(',', Error::ExpectedComma)?;
                self.parser.skip_ws()?;

                // Trailing comma check
                if self.parser.consume_char(')') {
                    break;
                }
            }

            // Single-element tuple could be unwrapped depending on extension
            if seq.len() == 1 && self.parser.exts.contains(Extensions::UNWRAP_NEWTYPES) {
                #[allow(clippy::unwrap_used)] // seq.len() == 1 checked above
                let first = seq.into_iter().next().unwrap();
                Ok(first)
            } else {
                Ok(Value::Seq(seq))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::{vec, vec::Vec};
    use crate::value::Number;

    fn from_str(s: &str) -> Result<Value> {
        let mut de = Deserializer::new(s).map_err(|e| e.code)?;
        let value = de.parse_value()?;
        de.end()?;
        Ok(value)
    }

    #[test]
    fn test_bool() {
        assert_eq!(from_str("true").unwrap(), Value::Bool(true));
        assert_eq!(from_str("false").unwrap(), Value::Bool(false));
    }

    #[test]
    fn test_char() {
        assert_eq!(from_str("'a'").unwrap(), Value::Char('a'));
        assert_eq!(from_str("'\\n'").unwrap(), Value::Char('\n'));
    }

    #[test]
    fn test_string() {
        assert_eq!(
            from_str("\"hello\"").unwrap(),
            Value::String(String::from("hello"))
        );
        assert_eq!(
            from_str("r#\"raw\"#").unwrap(),
            Value::String(String::from("raw"))
        );
    }

    #[test]
    fn test_bytes() {
        assert_eq!(
            from_str("b\"hello\"").unwrap(),
            Value::Bytes(Vec::from(*b"hello"))
        );
    }

    #[test]
    fn test_number() {
        assert_eq!(from_str("42").unwrap(), Value::Number(Number::U8(42)));
        assert_eq!(from_str("-42").unwrap(), Value::Number(Number::I8(-42)));
        assert_eq!(
            from_str("2.5").unwrap(),
            Value::Number(Number::F32(2.5.into()))
        );
    }

    #[test]
    fn test_option() {
        assert_eq!(from_str("None").unwrap(), Value::Option(None));
        assert_eq!(
            from_str("Some(42)").unwrap(),
            Value::Option(Some(Box::new(Value::Number(Number::U8(42)))))
        );
    }

    #[test]
    fn test_unit() {
        assert_eq!(from_str("()").unwrap(), Value::Unit);
    }

    #[test]
    fn test_seq() {
        assert_eq!(
            from_str("[1, 2, 3]").unwrap(),
            Value::Seq(vec![
                Value::Number(Number::U8(1)),
                Value::Number(Number::U8(2)),
                Value::Number(Number::U8(3)),
            ])
        );
    }

    #[test]
    fn test_tuple() {
        assert_eq!(
            from_str("(1, 2, 3)").unwrap(),
            Value::Seq(vec![
                Value::Number(Number::U8(1)),
                Value::Number(Number::U8(2)),
                Value::Number(Number::U8(3)),
            ])
        );
    }

    #[test]
    fn test_map() {
        let result = from_str("{\"a\": 1, \"b\": 2}").unwrap();
        assert!(matches!(result, Value::Map(_)));
        if let Value::Map(map) = result {
            assert_eq!(
                map.get(&Value::String(String::from("a"))),
                Some(&Value::Number(Number::U8(1)))
            );
            assert_eq!(
                map.get(&Value::String(String::from("b"))),
                Some(&Value::Number(Number::U8(2)))
            );
        }
    }

    #[test]
    fn test_struct() {
        let result = from_str("(x: 1, y: 2)").unwrap();
        assert!(matches!(result, Value::Map(_)));
        if let Value::Map(map) = result {
            assert_eq!(
                map.get(&Value::String(String::from("x"))),
                Some(&Value::Number(Number::U8(1)))
            );
            assert_eq!(
                map.get(&Value::String(String::from("y"))),
                Some(&Value::Number(Number::U8(2)))
            );
        }
    }

    #[test]
    fn test_named_struct() {
        let result = from_str("Point(x: 1, y: 2)").unwrap();
        assert!(matches!(result, Value::Map(_)));
        if let Value::Map(map) = result {
            assert_eq!(
                map.get(&Value::String(String::from("__type"))),
                Some(&Value::String(String::from("Point")))
            );
            assert_eq!(
                map.get(&Value::String(String::from("x"))),
                Some(&Value::Number(Number::U8(1)))
            );
        }
    }
}
