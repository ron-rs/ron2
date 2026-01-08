//! Serialization module - converts Value to RON text.

use alloc::string::String;
use core::fmt::{self, Write};

use crate::{
    error::{Error, Result},
    value::{Map, NamedContent, Number, StructFields, Value},
    Options,
};

/// Serializer for converting Value to RON text.
pub struct Serializer<W: Write> {
    writer: W,
    pretty: Option<PrettyConfig>,
    current_indent: usize,
    remaining_depth: Option<usize>,
}

/// Pretty printing configuration.
#[derive(Clone, Debug)]
pub struct PrettyConfig {
    /// Indentation string.
    pub indent: String,
    /// Add a newline at the end of the output.
    pub new_line: String,
    /// Separator between items in sequences and maps.
    pub separator: String,
    /// Whether to use struct names.
    pub struct_names: bool,
    /// Whether to include integer type suffixes (e.g., `42i32`).
    pub integer_type_suffix: bool,
    /// Whether to include float type suffixes (e.g., `3.14f64`).
    pub float_type_suffix: bool,
}

impl Default for PrettyConfig {
    fn default() -> Self {
        Self {
            indent: String::from("    "),
            new_line: String::from("\n"),
            separator: String::from(", "),
            struct_names: false,
            integer_type_suffix: false,
            float_type_suffix: false,
        }
    }
}

impl PrettyConfig {
    /// Create a new `PrettyConfig` with default settings.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the indentation string.
    #[must_use]
    pub fn indent(mut self, indent: impl Into<String>) -> Self {
        self.indent = indent.into();
        self
    }

    /// Set the newline string.
    #[must_use]
    pub fn new_line(mut self, new_line: impl Into<String>) -> Self {
        self.new_line = new_line.into();
        self
    }

    /// Set the separator string.
    #[must_use]
    pub fn separator(mut self, separator: impl Into<String>) -> Self {
        self.separator = separator.into();
        self
    }

    /// Enable or disable struct names.
    #[must_use]
    pub fn struct_names(mut self, struct_names: bool) -> Self {
        self.struct_names = struct_names;
        self
    }

    /// Enable or disable integer type suffixes.
    #[must_use]
    pub fn integer_type_suffix(mut self, integer_type_suffix: bool) -> Self {
        self.integer_type_suffix = integer_type_suffix;
        self
    }

    /// Enable or disable float type suffixes.
    #[must_use]
    pub fn float_type_suffix(mut self, float_type_suffix: bool) -> Self {
        self.float_type_suffix = float_type_suffix;
        self
    }
}

impl<W: Write> Serializer<W> {
    /// Create a new Serializer with default options.
    pub fn new(writer: W) -> Result<Self> {
        Self::with_options(writer, None, &Options::default())
    }

    /// Create a new Serializer with pretty printing.
    pub fn pretty(writer: W, config: PrettyConfig) -> Result<Self> {
        Self::with_options(writer, Some(config), &Options::default())
    }

    /// Create a new Serializer with options.
    pub fn with_options(writer: W, pretty: Option<PrettyConfig>, options: &Options) -> Result<Self> {
        Ok(Self {
            writer,
            pretty,
            current_indent: 0,
            remaining_depth: options.recursion_limit,
        })
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

    fn write_indent(&mut self) -> fmt::Result {
        if let Some(ref pretty) = self.pretty {
            for _ in 0..self.current_indent {
                self.writer.write_str(&pretty.indent)?;
            }
        }
        Ok(())
    }

    fn write_newline(&mut self) -> fmt::Result {
        if let Some(ref pretty) = self.pretty {
            self.writer.write_str(&pretty.new_line)?;
        }
        Ok(())
    }

    /// Serialize a Value to the writer.
    pub fn serialize(&mut self, value: &Value) -> Result<()> {
        self.enter_recursion()?;
        let result = self.serialize_inner(value);
        self.exit_recursion();
        result
    }

    fn serialize_inner(&mut self, value: &Value) -> Result<()> {
        match value {
            Value::Bool(b) => {
                if *b {
                    self.writer.write_str("true")?;
                } else {
                    self.writer.write_str("false")?;
                }
            }
            Value::Char(c) => {
                self.writer.write_char('\'')?;
                self.write_escaped_char(*c)?;
                self.writer.write_char('\'')?;
            }
            Value::Number(num) => {
                self.serialize_number(num)?;
            }
            Value::String(s) => {
                self.writer.write_char('"')?;
                self.write_escaped_str(s)?;
                self.writer.write_char('"')?;
            }
            Value::Bytes(bytes) => {
                self.writer.write_str("b\"")?;
                for &b in bytes {
                    self.write_escaped_byte(b)?;
                }
                self.writer.write_char('"')?;
            }
            Value::Unit => {
                self.writer.write_str("()")?;
            }
            Value::Option(opt) => {
                match opt {
                    None => self.writer.write_str("None")?,
                    Some(v) => {
                        self.writer.write_str("Some(")?;
                        self.serialize(v)?;
                        self.writer.write_char(')')?;
                    }
                }
            }
            Value::Seq(seq) => {
                self.serialize_seq(seq)?;
            }
            Value::Tuple(elements) => {
                self.serialize_tuple(elements)?;
            }
            Value::Map(map) => {
                self.serialize_map(map)?;
            }
            Value::Struct(fields) => {
                self.serialize_struct_fields(None, fields)?;
            }
            Value::Named { name, content } => {
                self.serialize_named(name, content)?;
            }
        }
        Ok(())
    }

    #[allow(clippy::too_many_lines)]
    fn serialize_number(&mut self, num: &Number) -> Result<()> {
        let type_suffix = self.pretty.as_ref().is_some_and(|p| p.integer_type_suffix);
        let float_suffix = self.pretty.as_ref().is_some_and(|p| p.float_type_suffix);

        match num {
            Number::I8(v) => {
                write!(self.writer, "{v}")?;
                if type_suffix {
                    self.writer.write_str("i8")?;
                }
            }
            Number::I16(v) => {
                write!(self.writer, "{v}")?;
                if type_suffix {
                    self.writer.write_str("i16")?;
                }
            }
            Number::I32(v) => {
                write!(self.writer, "{v}")?;
                if type_suffix {
                    self.writer.write_str("i32")?;
                }
            }
            Number::I64(v) => {
                write!(self.writer, "{v}")?;
                if type_suffix {
                    self.writer.write_str("i64")?;
                }
            }
            #[cfg(feature = "integer128")]
            Number::I128(v) => {
                write!(self.writer, "{v}")?;
                if type_suffix {
                    self.writer.write_str("i128")?;
                }
            }
            Number::U8(v) => {
                write!(self.writer, "{v}")?;
                if type_suffix {
                    self.writer.write_str("u8")?;
                }
            }
            Number::U16(v) => {
                write!(self.writer, "{v}")?;
                if type_suffix {
                    self.writer.write_str("u16")?;
                }
            }
            Number::U32(v) => {
                write!(self.writer, "{v}")?;
                if type_suffix {
                    self.writer.write_str("u32")?;
                }
            }
            Number::U64(v) => {
                write!(self.writer, "{v}")?;
                if type_suffix {
                    self.writer.write_str("u64")?;
                }
            }
            #[cfg(feature = "integer128")]
            Number::U128(v) => {
                write!(self.writer, "{v}")?;
                if type_suffix {
                    self.writer.write_str("u128")?;
                }
            }
            Number::F32(v) => {
                let f = v.get();
                if f.is_nan() {
                    if f.is_sign_negative() {
                        self.writer.write_str("-NaN")?;
                    } else {
                        self.writer.write_str("NaN")?;
                    }
                } else if f.is_infinite() {
                    if f.is_sign_negative() {
                        self.writer.write_str("-inf")?;
                    } else {
                        self.writer.write_str("inf")?;
                    }
                } else {
                    write!(self.writer, "{f}")?;
                    // Ensure we have a decimal point
                    if !Self::has_decimal_point(f) {
                        self.writer.write_str(".0")?;
                    }
                }
                if float_suffix {
                    self.writer.write_str("f32")?;
                }
            }
            Number::F64(v) => {
                let f = v.get();
                if f.is_nan() {
                    if f.is_sign_negative() {
                        self.writer.write_str("-NaN")?;
                    } else {
                        self.writer.write_str("NaN")?;
                    }
                } else if f.is_infinite() {
                    if f.is_sign_negative() {
                        self.writer.write_str("-inf")?;
                    } else {
                        self.writer.write_str("inf")?;
                    }
                } else {
                    write!(self.writer, "{f}")?;
                    // Ensure we have a decimal point
                    if !Self::has_decimal_point(f) {
                        self.writer.write_str(".0")?;
                    }
                }
                if float_suffix {
                    self.writer.write_str("f64")?;
                }
            }
            #[cfg(not(doc))]
            Number::__NonExhaustive(never) => never.never(),
        }
        Ok(())
    }

    fn has_decimal_point(f: impl Into<f64>) -> bool {
        let v = f.into();
        let s = alloc::format!("{v}");
        s.contains('.') || s.contains('e') || s.contains('E')
    }

    fn serialize_seq(&mut self, seq: &[Value]) -> Result<()> {
        self.writer.write_char('[')?;

        if seq.is_empty() {
            self.writer.write_char(']')?;
            return Ok(());
        }

        if self.pretty.is_some() {
            self.write_newline()?;
            self.current_indent += 1;
        }

        for (i, item) in seq.iter().enumerate() {
            if self.pretty.is_some() {
                self.write_indent()?;
            }

            self.serialize(item)?;

            if i < seq.len() - 1 {
                self.writer.write_char(',')?;
            }

            if self.pretty.is_some() {
                self.write_newline()?;
            }
        }

        if self.pretty.is_some() {
            self.current_indent -= 1;
            self.write_indent()?;
        }

        self.writer.write_char(']')?;
        Ok(())
    }

    /// Serialize a tuple: `(a, b, c)`
    fn serialize_tuple(&mut self, elements: &[Value]) -> Result<()> {
        self.writer.write_char('(')?;

        if elements.is_empty() {
            self.writer.write_char(')')?;
            return Ok(());
        }

        if self.pretty.is_some() {
            self.write_newline()?;
            self.current_indent += 1;
        }

        for (i, item) in elements.iter().enumerate() {
            if self.pretty.is_some() {
                self.write_indent()?;
            }

            self.serialize(item)?;

            if i < elements.len() - 1 {
                self.writer.write_char(',')?;
            }

            if self.pretty.is_some() {
                self.write_newline()?;
            }
        }

        if self.pretty.is_some() {
            self.current_indent -= 1;
            self.write_indent()?;
        }

        self.writer.write_char(')')?;
        Ok(())
    }

    /// Serialize a map: `{ key: value, ... }`
    fn serialize_map(&mut self, map: &Map) -> Result<()> {
        self.writer.write_char('{')?;

        if map.is_empty() {
            self.writer.write_char('}')?;
            return Ok(());
        }

        if self.pretty.is_some() {
            self.write_newline()?;
            self.current_indent += 1;
        }

        let entries: alloc::vec::Vec<_> = map.iter().collect();

        for (i, (key, value)) in entries.iter().enumerate() {
            if self.pretty.is_some() {
                self.write_indent()?;
            }

            self.serialize(key)?;
            self.writer.write_str(": ")?;
            self.serialize(value)?;

            if i < entries.len() - 1 {
                self.writer.write_char(',')?;
            }

            if self.pretty.is_some() {
                self.write_newline()?;
            }
        }

        if self.pretty.is_some() {
            self.current_indent -= 1;
            self.write_indent()?;
        }

        self.writer.write_char('}')?;
        Ok(())
    }

    /// Serialize struct fields: `(x: 1, y: 2)` with optional name prefix
    fn serialize_struct_fields(&mut self, name: Option<&str>, fields: &StructFields) -> Result<()> {
        if let Some(name) = name {
            self.writer.write_str(name)?;
        }

        self.writer.write_char('(')?;

        if fields.is_empty() {
            self.writer.write_char(')')?;
            return Ok(());
        }

        if self.pretty.is_some() {
            self.write_newline()?;
            self.current_indent += 1;
        }

        for (i, (field_name, value)) in fields.iter().enumerate() {
            if self.pretty.is_some() {
                self.write_indent()?;
            }

            self.writer.write_str(field_name)?;
            self.writer.write_str(": ")?;
            self.serialize(value)?;

            if i < fields.len() - 1 {
                self.writer.write_char(',')?;
            }

            if self.pretty.is_some() {
                self.write_newline()?;
            }
        }

        if self.pretty.is_some() {
            self.current_indent -= 1;
            self.write_indent()?;
        }

        self.writer.write_char(')')?;
        Ok(())
    }

    /// Serialize a named type: `Point`, `Point(1, 2)`, or `Point(x: 1, y: 2)`
    fn serialize_named(&mut self, name: &str, content: &NamedContent) -> Result<()> {
        match content {
            NamedContent::Unit => {
                self.writer.write_str(name)?;
            }
            NamedContent::Tuple(elements) => {
                self.writer.write_str(name)?;
                self.writer.write_char('(')?;

                if elements.is_empty() {
                    self.writer.write_char(')')?;
                    return Ok(());
                }

                if self.pretty.is_some() {
                    self.write_newline()?;
                    self.current_indent += 1;
                }

                for (i, item) in elements.iter().enumerate() {
                    if self.pretty.is_some() {
                        self.write_indent()?;
                    }

                    self.serialize(item)?;

                    if i < elements.len() - 1 {
                        self.writer.write_char(',')?;
                    }

                    if self.pretty.is_some() {
                        self.write_newline()?;
                    }
                }

                if self.pretty.is_some() {
                    self.current_indent -= 1;
                    self.write_indent()?;
                }

                self.writer.write_char(')')?;
            }
            NamedContent::Struct(fields) => {
                self.serialize_struct_fields(Some(name), fields)?;
            }
        }
        Ok(())
    }

    fn write_escaped_str(&mut self, s: &str) -> fmt::Result {
        for c in s.chars() {
            self.write_escaped_char(c)?;
        }
        Ok(())
    }

    fn write_escaped_char(&mut self, c: char) -> fmt::Result {
        match c {
            '\\' => self.writer.write_str("\\\\"),
            '"' => self.writer.write_str("\\\""),
            '\'' => self.writer.write_str("\\'"),
            '\n' => self.writer.write_str("\\n"),
            '\r' => self.writer.write_str("\\r"),
            '\t' => self.writer.write_str("\\t"),
            '\0' => self.writer.write_str("\\0"),
            c if c.is_ascii_control() => {
                write!(self.writer, "\\x{:02x}", c as u8)
            }
            c => self.writer.write_char(c),
        }
    }

    fn write_escaped_byte(&mut self, b: u8) -> fmt::Result {
        match b {
            b'\\' => self.writer.write_str("\\\\"),
            b'"' => self.writer.write_str("\\\""),
            b'\n' => self.writer.write_str("\\n"),
            b'\r' => self.writer.write_str("\\r"),
            b'\t' => self.writer.write_str("\\t"),
            b'\0' => self.writer.write_str("\\0"),
            b if b.is_ascii_graphic() || b == b' ' => self.writer.write_char(char::from(b)),
            b => write!(self.writer, "\\x{b:02x}"),
        }
    }
}

/// Serialize a Value to a String.
pub fn to_string(value: &Value) -> Result<String> {
    let mut output = String::new();
    let mut ser = Serializer::new(&mut output)?;
    ser.serialize(value)?;
    Ok(output)
}

/// Serialize a Value to a String with pretty printing.
pub fn to_string_pretty(value: &Value, config: PrettyConfig) -> Result<String> {
    let mut output = String::new();
    let mut ser = Serializer::pretty(&mut output, config)?;
    ser.serialize(value)?;
    Ok(output)
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec;

    #[test]
    fn test_bool() {
        assert_eq!(to_string(&Value::Bool(true)).unwrap(), "true");
        assert_eq!(to_string(&Value::Bool(false)).unwrap(), "false");
    }

    #[test]
    fn test_char() {
        assert_eq!(to_string(&Value::Char('a')).unwrap(), "'a'");
        assert_eq!(to_string(&Value::Char('\n')).unwrap(), "'\\n'");
    }

    #[test]
    fn test_string() {
        assert_eq!(
            to_string(&Value::String(String::from("hello"))).unwrap(),
            "\"hello\""
        );
        assert_eq!(
            to_string(&Value::String(String::from("line\nbreak"))).unwrap(),
            "\"line\\nbreak\""
        );
    }

    #[test]
    fn test_number() {
        assert_eq!(to_string(&Value::Number(Number::U8(42))).unwrap(), "42");
        assert_eq!(to_string(&Value::Number(Number::I8(-42))).unwrap(), "-42");
        assert_eq!(
            to_string(&Value::Number(Number::F64(2.5.into()))).unwrap(),
            "2.5"
        );
    }

    #[test]
    fn test_option() {
        assert_eq!(to_string(&Value::Option(None)).unwrap(), "None");
        assert_eq!(
            to_string(&Value::Option(Some(Box::new(Value::Number(Number::U8(42)))))).unwrap(),
            "Some(42)"
        );
    }

    #[test]
    fn test_unit() {
        assert_eq!(to_string(&Value::Unit).unwrap(), "()");
    }

    #[test]
    fn test_seq() {
        assert_eq!(
            to_string(&Value::Seq(vec![
                Value::Number(Number::U8(1)),
                Value::Number(Number::U8(2)),
                Value::Number(Number::U8(3)),
            ]))
            .unwrap(),
            "[1,2,3]"
        );
    }

    #[test]
    fn test_map() {
        let mut map = Map::new();
        map.insert(Value::String(String::from("x")), Value::Number(Number::U8(1)));
        map.insert(Value::String(String::from("y")), Value::Number(Number::U8(2)));

        let result = to_string(&Value::Map(map)).unwrap();
        // Maps use brace syntax with Value keys serialized
        assert!(result.contains("\"x\": 1"));
        assert!(result.contains("\"y\": 2"));
    }

    #[test]
    fn test_bytes() {
        assert_eq!(
            to_string(&Value::Bytes(alloc::vec![104, 101, 108, 108, 111])).unwrap(),
            "b\"hello\""
        );
    }
}
