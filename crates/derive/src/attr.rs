//! Unified attribute parsing for all ron-derive macros.
//!
//! This module provides shared attribute parsing for `#[derive(RonSchema)]`,
//! `#[derive(SerRon)]`, and `#[derive(DeRon)]`. All three derives use the
//! `#[ron(...)]` attribute namespace.

use syn::{Attribute, Expr, ExprPath, Lit, Meta};

/// Rename strategy for fields/variants.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(clippy::enum_variant_names)]
pub enum RenameRule {
    /// camelCase
    CamelCase,
    /// snake_case
    SnakeCase,
    /// PascalCase
    PascalCase,
    /// SCREAMING_SNAKE_CASE
    ScreamingSnakeCase,
    /// lowercase
    LowerCase,
    /// UPPERCASE
    UpperCase,
}

impl RenameRule {
    /// Parse a rename rule from a string.
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "camelCase" => Some(RenameRule::CamelCase),
            "snake_case" => Some(RenameRule::SnakeCase),
            "PascalCase" => Some(RenameRule::PascalCase),
            "SCREAMING_SNAKE_CASE" => Some(RenameRule::ScreamingSnakeCase),
            "lowercase" => Some(RenameRule::LowerCase),
            "UPPERCASE" => Some(RenameRule::UpperCase),
            _ => None,
        }
    }

    /// Apply this rename rule to an identifier.
    pub fn apply(&self, s: &str) -> String {
        match self {
            RenameRule::CamelCase => to_camel_case(s),
            RenameRule::SnakeCase => to_snake_case(s),
            RenameRule::PascalCase => to_pascal_case(s),
            RenameRule::ScreamingSnakeCase => to_screaming_snake_case(s),
            RenameRule::LowerCase => s.replace('_', "").to_lowercase(),
            RenameRule::UpperCase => s.replace('_', "").to_uppercase(),
        }
    }
}

/// Container-level attributes parsed from `#[ron(...)]`.
///
/// # Attributes
///
/// - `rename = "..."` - Use a different name in RON
/// - `rename_all = "..."` - Apply rename rule to all fields/variants
/// - `deny_unknown_fields` - Error on unknown fields during deserialization
/// - `transparent` - Serialize/deserialize as the single inner field
///
/// # Example
///
/// ```ignore
/// #[derive(FromRon, ToRon)]
/// #[ron(transparent)]
/// struct UserId(u64);
/// ```
#[derive(Debug, Default)]
pub struct ContainerAttrs {
    /// Rename the type in RON output.
    pub rename: Option<String>,
    /// Rename all fields using this rule.
    pub rename_all: Option<RenameRule>,
    /// Error on unknown fields during deserialization.
    pub deny_unknown_fields: bool,
    /// Serialize/deserialize as the single inner field (for newtypes).
    pub transparent: bool,
}

impl ContainerAttrs {
    /// Parse container attributes from a list of attributes.
    pub fn from_ast(attrs: &[Attribute]) -> syn::Result<Self> {
        parse_ron_attrs(attrs, |result: &mut Self, meta| {
            match meta {
                Meta::Path(path) => {
                    if path.is_ident("deny_unknown_fields") {
                        result.deny_unknown_fields = true;
                    } else if path.is_ident("transparent") {
                        result.transparent = true;
                    }
                }
                Meta::NameValue(nv) => {
                    if nv.path.is_ident("rename") {
                        result.rename = Some(get_lit_str(&nv.value)?);
                    } else if nv.path.is_ident("rename_all") {
                        let s = get_lit_str(&nv.value)?;
                        result.rename_all = RenameRule::from_str(&s);
                        if result.rename_all.is_none() {
                            return Err(syn::Error::new_spanned(
                                &nv.value,
                                format!("unknown rename rule: {}", s),
                            ));
                        }
                    }
                }
                _ => {}
            }
            Ok(())
        })
    }

    /// Get the effective field name, applying rename_all if set.
    pub fn rename_field(&self, name: &str) -> String {
        if let Some(rule) = self.rename_all {
            rule.apply(name)
        } else {
            name.to_string()
        }
    }
}

/// The default value for a field.
#[derive(Debug, Clone, Default)]
pub enum FieldDefault {
    /// No default value - field is required.
    #[default]
    None,
    /// Use `Default::default()`.
    Default,
    /// Use a custom function path.
    Path(ExprPath),
}

/// Field-level attributes parsed from `#[ron(...)]`.
///
/// # Attributes
///
/// - `rename = "..."` - Use a different name in RON
/// - `skip` - Skip this field entirely
/// - `skip_serializing` - Skip during serialization only
/// - `skip_deserializing` - Skip during deserialization only
/// - `skip_serializing_if = "path"` - Skip if predicate returns true
/// - `default` - Use `Default::default()` if missing
/// - `default = "path"` - Use custom function if missing
/// - `flatten` - Flatten nested struct fields into parent
/// - `explicit` - Require explicit `Some(...)` or `None` for Option fields
/// - `opt` - Shorthand for `default` + skip when value equals default
///
/// # Implicit Some (Default Behavior)
///
/// By default, `Option<T>` fields accept bare values:
/// ```ron
/// (name: "Alice")  // name: Option<String> becomes Some("Alice")
/// ```
///
/// Use `#[ron(explicit)]` to require explicit syntax:
/// ```ignore
/// #[ron(explicit)]
/// flags: Option<bool>,  // Must be Some(true), Some(false), or None
/// ```
#[derive(Debug, Clone, Default)]
pub struct FieldAttrs {
    /// Rename this field in RON.
    pub rename: Option<String>,
    /// Skip this field entirely (both ser and de).
    pub skip: bool,
    /// Skip this field during serialization.
    pub skip_serializing: bool,
    /// Skip this field during deserialization (use default).
    pub skip_deserializing: bool,
    /// Skip serializing if predicate returns true.
    pub skip_serializing_if: Option<ExprPath>,
    /// Default value for deserialization.
    pub default: FieldDefault,
    /// Flatten nested struct fields into parent.
    pub flatten: bool,
    /// Require explicit `Some(...)` or `None` syntax for Option fields.
    pub explicit: bool,
    /// Shorthand for `default` + skip when value equals default (requires Default + PartialEq).
    pub opt: bool,
}

impl FieldAttrs {
    /// Parse field attributes from a list of attributes.
    pub fn from_ast(attrs: &[Attribute]) -> syn::Result<Self> {
        parse_ron_attrs(attrs, |result: &mut Self, meta| {
            match meta {
                Meta::Path(path) => {
                    if path.is_ident("skip") {
                        result.skip = true;
                    } else if path.is_ident("skip_serializing") {
                        result.skip_serializing = true;
                    } else if path.is_ident("skip_deserializing") {
                        result.skip_deserializing = true;
                    } else if path.is_ident("default") {
                        result.default = FieldDefault::Default;
                    } else if path.is_ident("flatten") {
                        result.flatten = true;
                    } else if path.is_ident("explicit") {
                        result.explicit = true;
                    } else if path.is_ident("opt") {
                        result.opt = true;
                    }
                }
                Meta::NameValue(nv) => {
                    if nv.path.is_ident("rename") {
                        result.rename = Some(get_lit_str(&nv.value)?);
                    } else if nv.path.is_ident("default") {
                        // #[ron(default = "path::to::fn")]
                        let path = get_expr_path(&nv.value)?;
                        result.default = FieldDefault::Path(path);
                    } else if nv.path.is_ident("skip_serializing_if") {
                        result.skip_serializing_if = Some(get_expr_path(&nv.value)?);
                    }
                }
                _ => {}
            }
            Ok(())
        })
    }

    /// Get the effective field name, considering rename.
    pub fn effective_name(&self, original: &str, container: &ContainerAttrs) -> String {
        effective_name_impl(self.rename.as_ref(), original, container)
    }

    /// Check if this field should be skipped for serialization.
    pub fn should_skip_serializing(&self) -> bool {
        self.skip || self.skip_serializing
    }

    /// Check if this field should be skipped for deserialization.
    pub fn should_skip_deserializing(&self) -> bool {
        self.skip || self.skip_deserializing
    }

    /// Check if this field has any form of default.
    pub fn has_default(&self) -> bool {
        self.opt || !matches!(self.default, FieldDefault::None)
    }
}

/// Variant-level attributes parsed from `#[ron(...)]`.
#[derive(Debug, Default)]
pub struct VariantAttrs {
    /// Rename this variant in RON.
    pub rename: Option<String>,
    /// Skip this variant.
    pub skip: bool,
}

impl VariantAttrs {
    /// Parse variant attributes from a list of attributes.
    pub fn from_ast(attrs: &[Attribute]) -> syn::Result<Self> {
        parse_ron_attrs(attrs, |result: &mut Self, meta| {
            match meta {
                Meta::Path(path) => {
                    if path.is_ident("skip") {
                        result.skip = true;
                    }
                }
                Meta::NameValue(nv) => {
                    if nv.path.is_ident("rename") {
                        result.rename = Some(get_lit_str(&nv.value)?);
                    }
                }
                _ => {}
            }
            Ok(())
        })
    }

    /// Get the effective variant name.
    pub fn effective_name(&self, original: &str, container: &ContainerAttrs) -> String {
        effective_name_impl(self.rename.as_ref(), original, container)
    }
}

/// Extract the doc comment from attributes.
pub fn extract_doc_comment(attrs: &[Attribute]) -> Option<String> {
    let mut docs = Vec::new();

    for attr in attrs {
        if attr.path().is_ident("doc")
            && let syn::Meta::NameValue(meta) = &attr.meta
            && let Expr::Lit(syn::ExprLit {
                lit: Lit::Str(lit_str),
                ..
            }) = &meta.value
        {
            let value = lit_str.value();
            // Trim leading space that's typically added by rustdoc
            let trimmed = value.strip_prefix(' ').unwrap_or(&value);
            docs.push(trimmed.to_string());
        }
    }

    if docs.is_empty() {
        None
    } else {
        Some(docs.join("\n"))
    }
}

// =============================================================================
// Helper functions
// =============================================================================

/// Parse `#[ron(...)]` attributes using a callback handler.
///
/// This helper consolidates the common pattern of:
/// 1. Iterating through attributes
/// 2. Filtering for "ron" ident
/// 3. Parsing nested Meta items
/// 4. Processing each Meta with custom logic
fn parse_ron_attrs<T, F>(attrs: &[Attribute], mut handler: F) -> syn::Result<T>
where
    T: Default,
    F: FnMut(&mut T, &Meta) -> syn::Result<()>,
{
    let mut result = T::default();

    for attr in attrs {
        if !attr.path().is_ident("ron") {
            continue;
        }

        let nested = attr.parse_args_with(
            syn::punctuated::Punctuated::<Meta, syn::Token![,]>::parse_terminated,
        )?;

        for meta in nested {
            handler(&mut result, &meta)?;
        }
    }

    Ok(result)
}

/// Get the effective name for a field or variant.
///
/// Returns the explicit rename if provided, otherwise applies the container's
/// rename_all rule to the original name.
fn effective_name_impl(
    explicit_rename: Option<&String>,
    original: &str,
    container: &ContainerAttrs,
) -> String {
    explicit_rename
        .cloned()
        .unwrap_or_else(|| container.rename_field(original))
}

/// Extract a string literal from an expression.
fn get_lit_str(expr: &Expr) -> syn::Result<String> {
    if let Expr::Lit(syn::ExprLit {
        lit: Lit::Str(lit_str),
        ..
    }) = expr
    {
        Ok(lit_str.value())
    } else {
        Err(syn::Error::new_spanned(expr, "expected string literal"))
    }
}

/// Extract an expression path from an expression (for function paths).
fn get_expr_path(expr: &Expr) -> syn::Result<ExprPath> {
    // First try direct path
    if let Expr::Path(path) = expr {
        return Ok(path.clone());
    }

    // Try string literal and parse as path
    if let Expr::Lit(syn::ExprLit {
        lit: Lit::Str(lit_str),
        ..
    }) = expr
    {
        let path_str = lit_str.value();
        let path: ExprPath = syn::parse_str(&path_str)
            .map_err(|_| syn::Error::new_spanned(expr, format!("invalid path: {}", path_str)))?;
        return Ok(path);
    }

    Err(syn::Error::new_spanned(
        expr,
        "expected path or string literal",
    ))
}

// =============================================================================
// Case conversion utilities
// =============================================================================

fn to_snake_case(s: &str) -> String {
    let mut result = String::new();
    for (i, c) in s.chars().enumerate() {
        if c.is_uppercase() {
            if i > 0 {
                result.push('_');
            }
            result.push(c.to_lowercase().next().unwrap());
        } else {
            result.push(c);
        }
    }
    result
}

fn to_camel_case(s: &str) -> String {
    let mut result = String::new();
    let mut capitalize_next = false;
    for (i, c) in s.chars().enumerate() {
        if c == '_' {
            capitalize_next = true;
        } else if capitalize_next {
            result.push(c.to_uppercase().next().unwrap());
            capitalize_next = false;
        } else if i == 0 {
            result.push(c.to_lowercase().next().unwrap());
        } else {
            result.push(c);
        }
    }
    result
}

fn to_pascal_case(s: &str) -> String {
    let mut result = String::new();
    let mut capitalize_next = true;
    for c in s.chars() {
        if c == '_' {
            capitalize_next = true;
        } else if capitalize_next {
            result.push(c.to_uppercase().next().unwrap());
            capitalize_next = false;
        } else {
            result.push(c);
        }
    }
    result
}

fn to_screaming_snake_case(s: &str) -> String {
    to_snake_case(s).to_uppercase()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rename_rules() {
        assert_eq!(RenameRule::CamelCase.apply("my_field"), "myField");
        assert_eq!(RenameRule::SnakeCase.apply("MyField"), "my_field");
        assert_eq!(RenameRule::PascalCase.apply("my_field"), "MyField");
        assert_eq!(RenameRule::ScreamingSnakeCase.apply("MyField"), "MY_FIELD");
        assert_eq!(RenameRule::LowerCase.apply("MyField"), "myfield");
        assert_eq!(RenameRule::LowerCase.apply("my_field"), "myfield");
        assert_eq!(RenameRule::UpperCase.apply("MyField"), "MYFIELD");
        assert_eq!(RenameRule::UpperCase.apply("my_field"), "MYFIELD");
    }
}
