//! Compile-time schema building functions.
//!
//! These functions build actual `ron_schema` values (not tokens) for use during
//! proc macro execution. This allows writing schema files at compile time.

use ron_schema::{Field, Schema, TypeKind, Variant, VariantKind};
use syn::{DeriveInput, Fields, Ident};

use crate::attr::{extract_doc_comment, ContainerAttrs, FieldAttrs, VariantAttrs};
use crate::type_mapper::rust_type_to_type_kind;
use crate::util::get_crate_name;

/// Build a Schema value at compile time.
pub fn build_schema(input: &DeriveInput, container_attrs: &ContainerAttrs) -> Option<Schema> {
    let doc = extract_doc_comment(&input.attrs);

    let type_kind = match &input.data {
        syn::Data::Struct(data_struct) => {
            build_struct_kind(&data_struct.fields, container_attrs).ok()?
        }
        syn::Data::Enum(data_enum) => build_enum_kind(data_enum, container_attrs).ok()?,
        syn::Data::Union(_) => return None,
    };

    Some(if let Some(doc_str) = doc {
        Schema::with_doc(doc_str, type_kind)
    } else {
        Schema::new(type_kind)
    })
}

/// Build TypeKind for a struct's fields.
fn build_struct_kind(fields: &Fields, container_attrs: &ContainerAttrs) -> syn::Result<TypeKind> {
    // Handle transparent structs - return the inner type's TypeKind
    if container_attrs.transparent {
        return build_transparent_struct_kind(fields);
    }

    match fields {
        Fields::Named(named) => {
            let mut field_values: Vec<Field> = Vec::new();

            for f in named.named.iter() {
                let attrs = FieldAttrs::from_ast(&f.attrs)?;
                if attrs.skip {
                    continue;
                }
                field_values.push(build_field(f, &attrs, container_attrs)?);
            }

            Ok(TypeKind::Struct {
                fields: field_values,
            })
        }
        Fields::Unnamed(unnamed) => {
            let type_kinds: Vec<TypeKind> = unnamed
                .unnamed
                .iter()
                .map(|f| rust_type_to_type_kind(&f.ty))
                .collect();

            Ok(TypeKind::Tuple(type_kinds))
        }
        Fields::Unit => Ok(TypeKind::Unit),
    }
}

/// Build TypeKind for a transparent struct.
///
/// Returns the inner type's TypeKind directly.
fn build_transparent_struct_kind(fields: &Fields) -> syn::Result<TypeKind> {
    match fields {
        Fields::Named(named) => {
            // Find the single non-skipped field
            let mut active_fields = Vec::new();
            for f in &named.named {
                let attrs = FieldAttrs::from_ast(&f.attrs)?;
                if !attrs.skip {
                    active_fields.push(f);
                }
            }

            if active_fields.len() != 1 {
                return Err(syn::Error::new_spanned(
                    &named.named,
                    "#[ron(transparent)] requires exactly one non-skipped field",
                ));
            }

            Ok(rust_type_to_type_kind(&active_fields[0].ty))
        }
        Fields::Unnamed(unnamed) => {
            if unnamed.unnamed.len() != 1 {
                return Err(syn::Error::new_spanned(
                    unnamed,
                    "#[ron(transparent)] requires exactly one field for tuple structs",
                ));
            }

            Ok(rust_type_to_type_kind(&unnamed.unnamed[0].ty))
        }
        Fields::Unit => Err(syn::Error::new(
            proc_macro2::Span::call_site(),
            "#[ron(transparent)] cannot be used on unit structs",
        )),
    }
}

/// Build TypeKind for an enum.
fn build_enum_kind(
    data_enum: &syn::DataEnum,
    container_attrs: &ContainerAttrs,
) -> syn::Result<TypeKind> {
    let variants: Vec<Variant> = data_enum
        .variants
        .iter()
        .map(|v| build_variant(v, container_attrs))
        .collect::<syn::Result<Vec<_>>>()?;

    Ok(TypeKind::Enum { variants })
}

/// Build a Field value for a struct field.
fn build_field(
    field: &syn::Field,
    attrs: &FieldAttrs,
    container_attrs: &ContainerAttrs,
) -> syn::Result<Field> {
    let original_name = field
        .ident
        .as_ref()
        .ok_or_else(|| syn::Error::new_spanned(field, "Expected named field"))?
        .to_string();

    let name = attrs.effective_name(&original_name, container_attrs);
    let ty = rust_type_to_type_kind(&field.ty);
    let doc = extract_doc_comment(&field.attrs);
    let optional = attrs.has_default();
    let flattened = attrs.flatten;

    Ok(Field {
        name,
        ty,
        doc,
        optional,
        flattened,
    })
}

/// Build a Variant value for an enum variant.
fn build_variant(variant: &syn::Variant, container_attrs: &ContainerAttrs) -> syn::Result<Variant> {
    let variant_attrs = VariantAttrs::from_ast(&variant.attrs)?;
    let original_name = variant.ident.to_string();
    let name = variant_attrs.effective_name(&original_name, container_attrs);
    let doc = extract_doc_comment(&variant.attrs);

    let kind = match &variant.fields {
        Fields::Unit => VariantKind::Unit,
        Fields::Unnamed(unnamed) => {
            let type_kinds: Vec<TypeKind> = unnamed
                .unnamed
                .iter()
                .map(|f| rust_type_to_type_kind(&f.ty))
                .collect();
            VariantKind::Tuple(type_kinds)
        }
        Fields::Named(named) => {
            let mut field_values: Vec<Field> = Vec::new();
            for f in named.named.iter() {
                let attrs = FieldAttrs::from_ast(&f.attrs)?;
                if attrs.skip {
                    continue;
                }
                field_values.push(build_field(f, &attrs, container_attrs)?);
            }
            VariantKind::Struct(field_values)
        }
    };

    Ok(Variant { name, doc, kind })
}

/// Write a schema file at compile time (during proc macro execution).
pub fn write_schema_at_compile_time(
    type_name: &Ident,
    schema: &Schema,
    env_schema_dir: Option<&str>,
) -> Result<std::path::PathBuf, Box<dyn std::error::Error>> {
    use ron2::ser::PrettyConfig;
    use ron_schema::ToRon;
    use std::path::PathBuf;

    // Resolve output directory (env var or XDG default)
    let output_dir: PathBuf = if let Some(dir) = env_schema_dir {
        PathBuf::from(dir)
    } else {
        // RON_SCHEMA_GLOBAL=1 but no dir specified - use XDG default
        dirs::data_dir()
            .ok_or("No data directory found")?
            .join("ron-schemas")
    };

    // Build type path: crate_name::TypeName (normalized, hyphens -> underscores)
    let crate_name = get_crate_name();
    let type_path = format!("{}::{}", crate_name, type_name);

    // Convert to file path
    let file_path = output_dir.join(ron_schema::type_path_to_file_path(&type_path));

    // Serialize
    let value = schema.to_ron_value()?;
    let config = PrettyConfig::default();
    let contents = ron2::ser::to_string_pretty(&value, config)?;

    // Write
    if let Some(parent) = file_path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    std::fs::write(&file_path, contents)?;

    Ok(file_path)
}
