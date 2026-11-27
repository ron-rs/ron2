//! Parsing of `#[ron(...)]` attributes, shared by all derives.
//!
//! Unknown keys, repeated keys and malformed values are compile errors. Rules
//! that depend on more than one attribute live in `ast.rs`.

use proc_macro2::Span;
use syn::{
    Attribute, Expr, ExprPath, Lit, LitStr, Token, WherePredicate, meta::ParseNestedMeta,
    punctuated::Punctuated,
};

/// Rename strategy for `rename_all`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(clippy::enum_variant_names)]
pub enum RenameRule {
    CamelCase,
    SnakeCase,
    PascalCase,
    ScreamingSnakeCase,
    LowerCase,
    UpperCase,
}

impl RenameRule {
    // kebab-case is deliberately absent: `user-name` is not a RON identifier.
    const ALL: &[(&str, RenameRule)] = &[
        ("camelCase", RenameRule::CamelCase),
        ("snake_case", RenameRule::SnakeCase),
        ("PascalCase", RenameRule::PascalCase),
        ("SCREAMING_SNAKE_CASE", RenameRule::ScreamingSnakeCase),
        ("lowercase", RenameRule::LowerCase),
        ("UPPERCASE", RenameRule::UpperCase),
    ];

    fn parse(lit: &LitStr) -> syn::Result<Self> {
        let value = lit.value();
        Self::ALL
            .iter()
            .find(|(name, _)| *name == value)
            .map(|(_, rule)| *rule)
            .ok_or_else(|| {
                let names: Vec<_> = Self::ALL.iter().map(|(name, _)| *name).collect();
                syn::Error::new(
                    lit.span(),
                    format!(
                        "unknown rename rule `{value}`, expected one of: {}",
                        names.join(", ")
                    ),
                )
            })
    }

    /// Apply this rule to a Rust identifier (field or variant name).
    pub fn apply(self, ident: &str) -> String {
        let words = split_words(ident);
        match self {
            RenameRule::SnakeCase => join_mapped(&words, "_", str::to_lowercase),
            RenameRule::ScreamingSnakeCase => join_mapped(&words, "_", str::to_uppercase),
            RenameRule::CamelCase => {
                let mut out = String::new();
                for (i, word) in words.iter().enumerate() {
                    if i == 0 {
                        out.push_str(&word.to_lowercase());
                    } else {
                        out.push_str(&capitalize(word));
                    }
                }
                out
            }
            RenameRule::PascalCase => join_mapped(&words, "", capitalize),
            RenameRule::LowerCase => ident.to_lowercase(),
            RenameRule::UpperCase => ident.to_uppercase(),
        }
    }
}

/// Split an identifier into words at underscores, lower-to-upper transitions
/// and the end of acronyms (`HTTPServer` → `HTTP`, `Server`).
fn split_words(ident: &str) -> Vec<&str> {
    let chars: Vec<(usize, char)> = ident.char_indices().collect();
    let mut words = Vec::new();
    let mut start = None;
    for (i, &(pos, c)) in chars.iter().enumerate() {
        if c == '_' {
            if let Some(s) = start.take() {
                words.push(&ident[s..pos]);
            }
            continue;
        }
        if let Some(s) = start
            && c.is_uppercase()
        {
            let prev = chars[i - 1].1;
            let next_is_lower = chars.get(i + 1).is_some_and(|(_, n)| n.is_lowercase());
            if !prev.is_uppercase() || next_is_lower {
                words.push(&ident[s..pos]);
                start = Some(pos);
            }
        }
        start.get_or_insert(pos);
    }
    if let Some(s) = start {
        words.push(&ident[s..]);
    }
    words
}

fn join_mapped(words: &[&str], sep: &str, f: impl Fn(&str) -> String) -> String {
    words.iter().map(|w| f(w)).collect::<Vec<_>>().join(sep)
}

/// Uppercase the first character and keep the rest, so `HTTP` stays `HTTP`
/// and an already PascalCase name is unchanged.
fn capitalize(word: &str) -> String {
    let mut chars = word.chars();
    chars.next().map_or_else(String::new, |first| {
        first.to_uppercase().chain(chars).collect()
    })
}

/// A value together with the span of the attribute that set it.
#[derive(Clone)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

/// Where a missing field's value comes from.
#[derive(Clone)]
pub enum FieldDefault {
    /// `Default::default()`.
    Default,
    /// A user function, `default = "path"`.
    Path(ExprPath),
}

/// Attributes on a struct or enum.
#[derive(Default)]
pub struct ContainerAttrs {
    pub rename: Option<Spanned<String>>,
    pub rename_all: Option<Spanned<RenameRule>>,
    pub deny_unknown_fields: Option<Span>,
    pub transparent: Option<Span>,
    /// `bound = ".."` or `bound(ser = "..", de = "..", schema = "..")`:
    /// replaces the inferred where-clause predicates.
    pub bound: Bounds,
}

/// Where-clause overrides; a derive without one infers its bounds.
#[derive(Default)]
pub struct Bounds {
    pub ser: Option<Vec<WherePredicate>>,
    pub de: Option<Vec<WherePredicate>>,
    pub schema: Option<Vec<WherePredicate>>,
}

impl Bounds {
    fn parse(&mut self, meta: &ParseNestedMeta<'_>) -> syn::Result<()> {
        if meta.input.peek(Token![=]) {
            let predicates = predicates_value(meta)?;
            for slot in [&mut self.ser, &mut self.de, &mut self.schema] {
                if slot.is_some() {
                    return Err(duplicate(meta));
                }
                *slot = Some(predicates.clone());
            }
            return Ok(());
        }
        meta.parse_nested_meta(|inner| {
            let slot = match key_of(&inner).as_str() {
                "ser" => &mut self.ser,
                "de" => &mut self.de,
                "schema" => &mut self.schema,
                _ => return Err(unknown(&inner, &["ser", "de", "schema"])),
            };
            if slot.is_some() {
                return Err(duplicate(&inner));
            }
            *slot = Some(predicates_value(&inner)?);
            Ok(())
        })
    }
}

fn predicates_value(meta: &ParseNestedMeta<'_>) -> syn::Result<Vec<WherePredicate>> {
    let lit = meta.value()?.parse::<LitStr>()?;
    let predicates = lit.parse_with(Punctuated::<WherePredicate, Token![,]>::parse_terminated)?;
    Ok(predicates.into_iter().collect())
}

impl ContainerAttrs {
    pub fn from_ast(attrs: &[Attribute]) -> syn::Result<Self> {
        let mut out = Self::default();
        parse_ron_attrs(attrs, |meta| {
            let key = key_of(&meta);
            match key.as_str() {
                "rename" => set(&mut out.rename, string_value(&meta)?, &meta),
                "rename_all" => {
                    let lit = meta.value()?.parse::<LitStr>()?;
                    let rule = RenameRule::parse(&lit)?;
                    set(&mut out.rename_all, rule, &meta)
                }
                "deny_unknown_fields" => set_flag(&mut out.deny_unknown_fields, &meta),
                "transparent" => set_flag(&mut out.transparent, &meta),
                "bound" => out.bound.parse(&meta),
                _ => Err(unknown(
                    &meta,
                    &[
                        "rename",
                        "rename_all",
                        "deny_unknown_fields",
                        "transparent",
                        "bound",
                    ],
                )),
            }
        })?;
        Ok(out)
    }
}

/// Attributes on an enum variant.
#[derive(Default)]
pub struct VariantAttrs {
    pub rename: Option<Spanned<String>>,
    /// Renames the fields of a struct variant.
    pub rename_all: Option<Spanned<RenameRule>>,
    pub skip: Option<Span>,
}

impl VariantAttrs {
    pub fn from_ast(attrs: &[Attribute]) -> syn::Result<Self> {
        let mut out = Self::default();
        parse_ron_attrs(attrs, |meta| match key_of(&meta).as_str() {
            "rename" => set(&mut out.rename, string_value(&meta)?, &meta),
            "rename_all" => {
                let lit = meta.value()?.parse::<LitStr>()?;
                let rule = RenameRule::parse(&lit)?;
                set(&mut out.rename_all, rule, &meta)
            }
            "skip" => set_flag(&mut out.skip, &meta),
            _ => Err(unknown(&meta, &["rename", "rename_all", "skip"])),
        })?;
        Ok(out)
    }
}

/// Attributes on a field.
#[derive(Default)]
pub struct FieldAttrs {
    pub rename: Option<Spanned<String>>,
    pub skip: Option<Span>,
    pub skip_serializing: Option<Span>,
    pub skip_deserializing: Option<Span>,
    pub skip_serializing_if: Option<Spanned<ExprPath>>,
    pub default: Option<Spanned<FieldDefault>>,
    pub flatten: Option<Span>,
    /// Require `Some(..)`/`None` for an `Option` field.
    pub explicit: Option<Span>,
    /// Write `Some(x)` of an `Option` field as bare `x`.
    pub implicit_some: Option<Span>,
    /// `default` plus "skip serializing when equal to the default".
    pub opt: Option<Span>,
}

impl FieldAttrs {
    pub fn from_ast(attrs: &[Attribute]) -> syn::Result<Self> {
        let mut out = Self::default();
        parse_ron_attrs(attrs, |meta| match key_of(&meta).as_str() {
            "rename" => set(&mut out.rename, string_value(&meta)?, &meta),
            "skip" => set_flag(&mut out.skip, &meta),
            "skip_serializing" => set_flag(&mut out.skip_serializing, &meta),
            "skip_deserializing" => set_flag(&mut out.skip_deserializing, &meta),
            "skip_serializing_if" => set(&mut out.skip_serializing_if, path_value(&meta)?, &meta),
            "default" => {
                let default = if meta.input.peek(Token![=]) {
                    FieldDefault::Path(path_value(&meta)?)
                } else {
                    FieldDefault::Default
                };
                set(&mut out.default, default, &meta)
            }
            "flatten" => set_flag(&mut out.flatten, &meta),
            "explicit" => set_flag(&mut out.explicit, &meta),
            "implicit_some" => set_flag(&mut out.implicit_some, &meta),
            "opt" => set_flag(&mut out.opt, &meta),
            _ => Err(unknown(
                &meta,
                &[
                    "rename",
                    "skip",
                    "skip_serializing",
                    "skip_deserializing",
                    "skip_serializing_if",
                    "default",
                    "flatten",
                    "explicit",
                    "implicit_some",
                    "opt",
                ],
            )),
        })?;
        Ok(out)
    }

    /// Spans of every attribute that was set, for "not allowed here" errors.
    pub fn all_spans(&self) -> Vec<Span> {
        [
            self.rename.as_ref().map(|s| s.span),
            self.skip,
            self.skip_serializing,
            self.skip_deserializing,
            self.skip_serializing_if.as_ref().map(|s| s.span),
            self.default.as_ref().map(|s| s.span),
            self.flatten,
            self.explicit,
            self.implicit_some,
            self.opt,
        ]
        .into_iter()
        .flatten()
        .collect()
    }
}

/// Join the doc comments of an item.
pub fn doc_comment(attrs: &[Attribute]) -> Option<String> {
    let lines: Vec<String> = attrs
        .iter()
        .filter(|attr| attr.path().is_ident("doc"))
        .filter_map(|attr| match &attr.meta {
            syn::Meta::NameValue(nv) => match &nv.value {
                Expr::Lit(syn::ExprLit {
                    lit: Lit::Str(s), ..
                }) => {
                    let line = s.value();
                    Some(line.strip_prefix(' ').unwrap_or(&line).to_string())
                }
                _ => None,
            },
            _ => None,
        })
        .collect();
    (!lines.is_empty()).then(|| lines.join("\n"))
}

/// Run `handler` on each item of every `#[ron(...)]` attribute, combining
/// the errors of all attributes.
fn parse_ron_attrs(
    attrs: &[Attribute],
    mut handler: impl FnMut(ParseNestedMeta<'_>) -> syn::Result<()>,
) -> syn::Result<()> {
    let mut errors: Option<syn::Error> = None;
    for attr in attrs.iter().filter(|attr| attr.path().is_ident("ron")) {
        if let Err(err) = attr.parse_nested_meta(&mut handler) {
            match &mut errors {
                Some(errors) => errors.combine(err),
                None => errors = Some(err),
            }
        }
    }
    errors.map_or(Ok(()), Err)
}

fn key_of(meta: &ParseNestedMeta<'_>) -> String {
    meta.path
        .get_ident()
        .map_or_else(String::new, ToString::to_string)
}

fn span_of(meta: &ParseNestedMeta<'_>) -> Span {
    syn::spanned::Spanned::span(&meta.path)
}

fn unknown(meta: &ParseNestedMeta<'_>, expected: &[&str]) -> syn::Error {
    let path = &meta.path;
    let key = quote::quote!(#path).to_string().replace(' ', "");
    meta.error(format!(
        "unknown ron attribute `{key}`, expected one of: {}",
        expected.join(", ")
    ))
}

fn duplicate(meta: &ParseNestedMeta<'_>) -> syn::Error {
    meta.error(format!("duplicate ron attribute `{}`", key_of(meta)))
}

fn set<T>(slot: &mut Option<Spanned<T>>, value: T, meta: &ParseNestedMeta<'_>) -> syn::Result<()> {
    if slot.is_some() {
        return Err(duplicate(meta));
    }
    *slot = Some(Spanned {
        value,
        span: span_of(meta),
    });
    Ok(())
}

fn set_flag(slot: &mut Option<Span>, meta: &ParseNestedMeta<'_>) -> syn::Result<()> {
    if !meta.input.is_empty() && !meta.input.peek(Token![,]) {
        return Err(meta.error(format!(
            "ron attribute `{}` does not take a value",
            key_of(meta)
        )));
    }
    if slot.is_some() {
        return Err(duplicate(meta));
    }
    *slot = Some(span_of(meta));
    Ok(())
}

fn string_value(meta: &ParseNestedMeta<'_>) -> syn::Result<String> {
    Ok(meta.value()?.parse::<LitStr>()?.value())
}

/// A function path, written either as a string (`"path::to::fn"`) or bare.
fn path_value(meta: &ParseNestedMeta<'_>) -> syn::Result<ExprPath> {
    let input = meta.value()?;
    if input.peek(LitStr) {
        input.parse::<LitStr>()?.parse()
    } else {
        input.parse()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rename_rules() {
        let cases = [
            (RenameRule::CamelCase, "my_field", "myField"),
            (RenameRule::CamelCase, "HTTPServer", "httpServer"),
            (RenameRule::SnakeCase, "MyField", "my_field"),
            (RenameRule::SnakeCase, "HTTPServer", "http_server"),
            (RenameRule::SnakeCase, "my_field", "my_field"),
            (RenameRule::SnakeCase, "Vec3D", "vec3_d"),
            (RenameRule::PascalCase, "my_field", "MyField"),
            (RenameRule::PascalCase, "HTTPServer", "HTTPServer"),
            (RenameRule::ScreamingSnakeCase, "MyField", "MY_FIELD"),
            (RenameRule::ScreamingSnakeCase, "user_name", "USER_NAME"),
            (RenameRule::LowerCase, "MyField", "myfield"),
            (RenameRule::LowerCase, "my_field", "my_field"),
            (RenameRule::UpperCase, "MyField", "MYFIELD"),
            (RenameRule::UpperCase, "my_field", "MY_FIELD"),
        ];
        for (rule, input, expected) in cases {
            assert_eq!(rule.apply(input), expected, "{rule:?} of {input}");
        }
    }
}
