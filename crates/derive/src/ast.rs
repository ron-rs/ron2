//! The derive input, parsed once into a form all three derives share.
//!
//! Attributes are parsed and cross-checked here, and RON names are resolved
//! (`rename`, `rename_all`, raw identifiers), so code generation never has to
//! re-read attributes or decide on names.

use std::collections::HashMap;

use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, quote, quote_spanned};
use syn::{DeriveInput, Generics, Ident, Member, Type, ext::IdentExt, spanned::Spanned as _};

use crate::attr::{
    ContainerAttrs, FieldAttrs, FieldDefault, RenameRule, VariantAttrs, doc_comment,
};

pub struct Container<'a> {
    pub ident: &'a Ident,
    pub generics: &'a Generics,
    pub attrs: ContainerAttrs,
    pub doc: Option<String>,
    /// The struct name in RON (unused for enums, whose values are variants).
    pub name: String,
    pub data: Data<'a>,
}

pub enum Data<'a> {
    Struct(Style, Vec<Field<'a>>),
    Enum(Vec<Variant<'a>>),
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Style {
    Named,
    Tuple,
    Unit,
}

pub struct Variant<'a> {
    pub ident: &'a Ident,
    pub attrs: VariantAttrs,
    pub doc: Option<String>,
    pub name: String,
    pub style: Style,
    pub fields: Vec<Field<'a>>,
}

pub struct Field<'a> {
    pub member: Member,
    pub ty: &'a Type,
    pub attrs: FieldAttrs,
    pub doc: Option<String>,
    /// The field name in RON (empty for tuple fields).
    pub name: String,
}

impl Container<'_> {
    /// Whether the type has type or const parameters, so that its schema
    /// depends on how it is instantiated.
    pub fn is_generic(&self) -> bool {
        self.generics.type_params().next().is_some()
            || self.generics.const_params().next().is_some()
    }

    /// The field of a `#[ron(transparent)]` struct that is (de)serialized.
    pub fn transparent_field(&self) -> Option<&Field<'_>> {
        self.attrs.transparent?;
        match &self.data {
            Data::Struct(_, fields) => fields.iter().find(|f| f.attrs.skip.is_none()),
            Data::Enum(_) => None,
        }
    }

    /// The fields of the struct, or of every variant that is not skipped.
    pub fn all_fields(&self) -> Box<dyn Iterator<Item = &Field<'_>> + '_> {
        match &self.data {
            Data::Struct(_, fields) => Box::new(fields.iter()),
            Data::Enum(variants) => Box::new(
                variants
                    .iter()
                    .filter(|v| v.attrs.skip.is_none())
                    .flat_map(|v| v.fields.iter()),
            ),
        }
    }
}

/// The name of a Rust identifier without the `r#` of raw identifiers.
pub fn unraw(ident: &Ident) -> String {
    ident.unraw().to_string()
}

impl Field<'_> {
    pub fn skip_serializing(&self) -> bool {
        self.attrs.skip.is_some() || self.attrs.skip_serializing.is_some()
    }

    pub fn skip_deserializing(&self) -> bool {
        self.attrs.skip.is_some() || self.attrs.skip_deserializing.is_some()
    }

    /// Whether the field is part of the RON input format (and thus the schema).
    pub fn in_schema(&self) -> bool {
        !self.skip_deserializing()
    }

    pub fn is_flatten(&self) -> bool {
        self.attrs.flatten.is_some()
    }

    /// A callable producing the field's value when it is missing or skipped.
    /// `opt` implies `Default::default`.
    pub fn default_fn(&self) -> Option<TokenStream> {
        let ty = self.ty;
        let span = self.ty.span();
        match self.attrs.default.as_ref().map(|d| &d.value) {
            Some(FieldDefault::Path(path)) => Some(path.to_token_stream()),
            Some(FieldDefault::Default) => {
                Some(quote_spanned!(span=> <#ty as ::core::default::Default>::default))
            }
            None if self.attrs.opt.is_some() => {
                Some(quote_spanned!(span=> <#ty as ::core::default::Default>::default))
            }
            None => None,
        }
    }

    /// The value of a field that is not read from RON.
    pub fn default_value(&self) -> TokenStream {
        let ty = self.ty;
        let default = self.default_fn().unwrap_or_else(
            || quote_spanned!(ty.span()=> <#ty as ::core::default::Default>::default),
        );
        quote!(#default())
    }
}

/// Collects errors so that all of them are reported at once.
#[derive(Default)]
struct Errors(Option<syn::Error>);

impl Errors {
    fn push(&mut self, span: Span, msg: impl std::fmt::Display) {
        let err = syn::Error::new(span, msg);
        match &mut self.0 {
            Some(errors) => errors.combine(err),
            None => self.0 = Some(err),
        }
    }

    fn add(&mut self, err: syn::Error) {
        match &mut self.0 {
            Some(errors) => errors.combine(err),
            None => self.0 = Some(err),
        }
    }

    fn finish(self) -> syn::Result<()> {
        self.0.map_or(Ok(()), Err)
    }
}

impl<'a> Container<'a> {
    pub fn from_ast(input: &'a DeriveInput) -> syn::Result<Self> {
        let mut errors = Errors::default();
        let attrs = ContainerAttrs::from_ast(&input.attrs).unwrap_or_else(|err| {
            errors.add(err);
            ContainerAttrs::default()
        });
        let field_rule = attrs.rename_all.as_ref().map(|r| r.value);

        let data = match &input.data {
            syn::Data::Struct(data) => {
                let (style, fields) = parse_fields(&data.fields, field_rule, &mut errors);
                Data::Struct(style, fields)
            }
            syn::Data::Enum(data) => {
                let variants = data
                    .variants
                    .iter()
                    .map(|variant| parse_variant(variant, field_rule, &mut errors))
                    .collect();
                Data::Enum(variants)
            }
            syn::Data::Union(_) => {
                return Err(syn::Error::new_spanned(
                    input,
                    "ron derives do not support unions",
                ));
            }
        };

        let name = match &attrs.rename {
            Some(rename) => rename.value.clone(),
            None => input.ident.unraw().to_string(),
        };

        let cont = Container {
            ident: &input.ident,
            generics: &input.generics,
            doc: doc_comment(&input.attrs),
            attrs,
            name,
            data,
        };
        cont.validate(&mut errors);
        errors.finish()?;
        Ok(cont)
    }

    fn validate(&self, errors: &mut Errors) {
        let attrs = &self.attrs;
        if let Some(rename) = &attrs.rename {
            check_ron_ident(&rename.value, rename.span, errors);
        }
        match &self.data {
            Data::Enum(variants) => {
                if let Some(span) = attrs.transparent {
                    errors.push(span, "#[ron(transparent)] is only supported on structs");
                }
                if let Some(rename) = &attrs.rename {
                    errors.push(
                        rename.span,
                        "#[ron(rename)] has no effect on enums, rename the variants instead",
                    );
                }
                for variant in variants {
                    validate_variant(variant, errors);
                }
                check_unique_names(
                    variants
                        .iter()
                        .filter(|v| v.attrs.skip.is_none())
                        .map(|v| (v.name.as_str(), v.ident.span())),
                    "variant",
                    errors,
                );
            }
            Data::Struct(style, fields) => {
                if attrs.transparent.is_some() {
                    self.validate_transparent(fields, errors);
                } else if *style != Style::Named {
                    if let Some(rename_all) = &attrs.rename_all {
                        errors.push(rename_all.span, "#[ron(rename_all)] requires named fields");
                    }
                    if let Some(span) = attrs.deny_unknown_fields {
                        errors.push(span, "#[ron(deny_unknown_fields)] requires named fields");
                    }
                }
                validate_fields(fields, errors);
            }
        }
    }

    fn validate_transparent(&self, fields: &[Field<'_>], errors: &mut Errors) {
        let Some(span) = self.attrs.transparent else {
            return;
        };
        for (attr, set) in [
            ("rename", self.attrs.rename.as_ref().map(|r| r.span)),
            ("rename_all", self.attrs.rename_all.as_ref().map(|r| r.span)),
            ("deny_unknown_fields", self.attrs.deny_unknown_fields),
        ] {
            if let Some(attr_span) = set {
                errors.push(
                    attr_span,
                    format!("#[ron({attr})] has no effect on a transparent struct"),
                );
            }
        }
        let mut active = fields.iter().filter(|f| f.attrs.skip.is_none());
        match (active.next(), active.next()) {
            (Some(field), None) => {
                for attr_span in field.attrs.all_spans() {
                    errors.push(
                        attr_span,
                        "the field of a transparent struct cannot have ron attributes",
                    );
                }
            }
            _ => errors.push(
                span,
                "#[ron(transparent)] requires exactly one field that is not #[ron(skip)]",
            ),
        }
    }
}

fn parse_fields<'a>(
    fields: &'a syn::Fields,
    rule: Option<RenameRule>,
    errors: &mut Errors,
) -> (Style, Vec<Field<'a>>) {
    let style = match fields {
        syn::Fields::Named(_) => Style::Named,
        syn::Fields::Unnamed(_) => Style::Tuple,
        syn::Fields::Unit => Style::Unit,
    };
    let fields = fields
        .iter()
        .enumerate()
        .map(|(index, field)| {
            let attrs = FieldAttrs::from_ast(&field.attrs).unwrap_or_else(|err| {
                errors.add(err);
                FieldAttrs::default()
            });
            let (member, name) = match &field.ident {
                Some(ident) => {
                    let name = match (&attrs.rename, rule) {
                        (Some(rename), _) => rename.value.clone(),
                        (None, Some(rule)) => rule.apply(&ident.unraw().to_string()),
                        (None, None) => ident.unraw().to_string(),
                    };
                    (Member::Named(ident.clone()), name)
                }
                None => (Member::Unnamed(index.into()), String::new()),
            };
            Field {
                member,
                ty: &field.ty,
                doc: doc_comment(&field.attrs),
                attrs,
                name,
            }
        })
        .collect();
    (style, fields)
}

fn parse_variant<'a>(
    variant: &'a syn::Variant,
    rule: Option<RenameRule>,
    errors: &mut Errors,
) -> Variant<'a> {
    let attrs = VariantAttrs::from_ast(&variant.attrs).unwrap_or_else(|err| {
        errors.add(err);
        VariantAttrs::default()
    });
    let name = match (&attrs.rename, rule) {
        (Some(rename), _) => rename.value.clone(),
        (None, Some(rule)) => rule.apply(&variant.ident.unraw().to_string()),
        (None, None) => variant.ident.unraw().to_string(),
    };
    let field_rule = attrs.rename_all.as_ref().map(|r| r.value);
    let (style, fields) = parse_fields(&variant.fields, field_rule, errors);
    Variant {
        ident: &variant.ident,
        doc: doc_comment(&variant.attrs),
        attrs,
        name,
        style,
        fields,
    }
}

fn validate_variant(variant: &Variant<'_>, errors: &mut Errors) {
    let attrs = &variant.attrs;
    if let Some(rename) = &attrs.rename {
        check_ron_ident(&rename.value, rename.span, errors);
    }
    if let Some(rename_all) = &attrs.rename_all
        && variant.style != Style::Named
    {
        errors.push(rename_all.span, "#[ron(rename_all)] requires named fields");
    }
    if let Some(skip) = attrs.skip {
        if attrs.rename.is_some() || attrs.rename_all.is_some() {
            errors.push(skip, "a skipped variant cannot be renamed");
        }
        return;
    }
    validate_fields(&variant.fields, errors);
}

fn validate_fields(fields: &[Field<'_>], errors: &mut Errors) {
    for field in fields {
        if let Member::Unnamed(_) = field.member {
            validate_tuple_field(field, errors);
            continue;
        }
        validate_field(field, errors);
    }
    check_unique_names(
        fields
            .iter()
            .filter(|f| {
                matches!(f.member, Member::Named(_)) && f.attrs.skip.is_none() && !f.is_flatten()
            })
            .map(|f| (f.name.as_str(), f.member.span())),
        "field",
        errors,
    );
}

/// Tuple fields are positional: they can only be left out entirely
/// (`skip`, optionally with `default = ".."`).
fn validate_tuple_field(field: &Field<'_>, errors: &mut Errors) {
    let a = &field.attrs;
    let unsupported = [
        a.rename.as_ref().map(|r| r.span),
        a.skip_serializing,
        a.skip_deserializing,
        a.skip_serializing_if.as_ref().map(|s| s.span),
        a.flatten,
        a.explicit,
        a.opt,
    ];
    for span in unsupported.into_iter().flatten() {
        errors.push(
            span,
            "tuple fields only support #[ron(skip)] and #[ron(skip, default = \"..\")]",
        );
    }
    if let (None, Some(default)) = (a.skip, &a.default) {
        errors.push(
            default.span,
            "#[ron(default)] on a tuple field requires #[ron(skip)]",
        );
    }
}

fn validate_field(field: &Field<'_>, errors: &mut Errors) {
    let a = &field.attrs;
    let rename = a.rename.as_ref().map(|r| r.span);
    let skip_if = a.skip_serializing_if.as_ref().map(|s| s.span);
    let default = a.default.as_ref().map(|d| d.span);
    // (attribute, its span, conflicting attribute, its span)
    let conflicts = [
        ("skip", a.skip, "skip_serializing", a.skip_serializing),
        ("skip", a.skip, "skip_deserializing", a.skip_deserializing),
        ("skip", a.skip, "skip_serializing_if", skip_if),
        ("skip", a.skip, "rename", rename),
        ("skip", a.skip, "flatten", a.flatten),
        ("skip", a.skip, "explicit", a.explicit),
        ("skip", a.skip, "opt", a.opt),
        (
            "skip_deserializing",
            a.skip_deserializing,
            "explicit",
            a.explicit,
        ),
        (
            "skip_serializing",
            a.skip_serializing,
            "skip_serializing_if",
            skip_if,
        ),
        ("skip_serializing", a.skip_serializing, "opt", a.opt),
        ("opt", a.opt, "skip_serializing_if", skip_if),
        ("opt", a.opt, "default", default),
        ("flatten", a.flatten, "rename", rename),
        ("flatten", a.flatten, "explicit", a.explicit),
        ("flatten", a.flatten, "opt", a.opt),
        ("flatten", a.flatten, "skip_serializing_if", skip_if),
    ];
    for (first, first_span, second, second_span) in conflicts {
        if let (Some(_), Some(span)) = (first_span, second_span) {
            errors.push(
                span,
                format!("#[ron({second})] cannot be combined with #[ron({first})]"),
            );
        }
    }
    if let Some(rename) = &a.rename {
        check_ron_ident(&rename.value, rename.span, errors);
    }
}

fn check_ron_ident(name: &str, span: Span, errors: &mut Errors) {
    let mut chars = name.chars();
    let valid = chars.next().is_some_and(|c| c == '_' || c.is_alphabetic())
        && chars.all(|c| c == '_' || c.is_alphanumeric());
    if !valid {
        errors.push(span, format!("`{name}` is not a valid RON identifier"));
    }
}

fn check_unique_names<'n>(
    names: impl Iterator<Item = (&'n str, Span)>,
    what: &str,
    errors: &mut Errors,
) {
    let mut seen = HashMap::new();
    for (name, span) in names {
        if seen.insert(name, span).is_some() {
            errors.push(span, format!("duplicate {what} name `{name}` in RON"));
        }
    }
}
