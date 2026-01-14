//! Shared field validation utilities for derive macros.
//!
//! This module consolidates common field operations used across de.rs, ser.rs,
//! and schema_codegen.rs, particularly for transparent struct validation.

use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::Fields;

use crate::attr::FieldAttrs;

// =============================================================================
// Error Message Constants
// =============================================================================

/// Error message for transparent structs with named fields
pub const ERROR_TRANSPARENT_NAMED_FIELDS: &str =
    "#[ron(transparent)] requires exactly one non-skipped field";

/// Error message for transparent tuple structs
pub const ERROR_TRANSPARENT_TUPLE: &str =
    "#[ron(transparent)] requires exactly one field for tuple structs";

/// Error message for transparent unit structs
pub const ERROR_TRANSPARENT_UNIT: &str = "#[ron(transparent)] cannot be used on unit structs";

// =============================================================================
// Field Skip Mode
// =============================================================================

/// Determines which skip check to apply when filtering fields
#[derive(Debug, Clone, Copy)]
pub enum FieldSkipMode {
    /// Check should_skip_deserializing() for FromRon
    Deserializing,
    /// Check should_skip_serializing() for ToRon
    Serializing,
    /// Check attrs.skip for RonSchema
    Any,
}

impl FieldSkipMode {
    /// Returns true if the field should be skipped given the mode
    pub fn should_skip(&self, attrs: &FieldAttrs) -> bool {
        match self {
            FieldSkipMode::Deserializing => attrs.should_skip_deserializing(),
            FieldSkipMode::Serializing => attrs.should_skip_serializing(),
            FieldSkipMode::Any => attrs.skip,
        }
    }
}

// =============================================================================
// Transparent Field Result
// =============================================================================

/// Result of validating and extracting a transparent struct's inner field
pub enum TransparentField<'a> {
    /// Named struct with a single active field
    Named {
        #[allow(dead_code)]
        field: &'a syn::Field,
        ident: &'a syn::Ident,
        ty: &'a syn::Type,
        #[allow(dead_code)]
        attrs: Box<FieldAttrs>,
        skipped_fields: Vec<(&'a syn::Ident, &'a syn::Type)>,
    },
    /// Tuple struct with a single field
    Unnamed { ty: &'a syn::Type },
}

// =============================================================================
// Transparent Struct Validation
// =============================================================================

/// Validate transparent struct and return the single active field
///
/// This consolidates the validation logic used across de.rs, ser.rs, and schema_codegen.rs.
/// It ensures exactly one non-skipped field exists and returns information about that field.
///
/// # Arguments
///
/// * `name` - The name of the struct (for error messages)
/// * `fields` - The struct's fields (Named, Unnamed, or Unit)
/// * `skip_mode` - Which skip check to apply (deserialization, serialization, or any)
///
/// # Returns
///
/// A `TransparentField` enum indicating whether the struct has a named or unnamed field,
/// along with relevant type and attribute information.
///
/// # Errors
///
/// Returns an error if:
/// - The struct has no fields (Unit)
/// - The struct has multiple non-skipped fields
/// - The struct has zero non-skipped fields
pub fn validate_transparent_struct<'a>(
    name: &syn::Ident,
    fields: &'a Fields,
    skip_mode: FieldSkipMode,
) -> syn::Result<TransparentField<'a>> {
    match fields {
        Fields::Named(named) => {
            // Collect active (non-skipped) fields
            let mut active = Vec::new();
            for field in &named.named {
                let attrs = FieldAttrs::from_ast(&field.attrs)?;
                if !skip_mode.should_skip(&attrs) {
                    active.push((field, attrs));
                }
            }

            // Validate exactly one active field
            if active.len() != 1 {
                return Err(syn::Error::new_spanned(
                    name,
                    ERROR_TRANSPARENT_NAMED_FIELDS,
                ));
            }

            // Collect skipped fields (for deserialization - need to initialize with Default)
            let mut skipped = Vec::new();
            for f in &named.named {
                let attrs = FieldAttrs::from_ast(&f.attrs)?;
                if skip_mode.should_skip(&attrs) {
                    skipped.push((f.ident.as_ref().unwrap(), &f.ty));
                }
            }

            let (field, attrs) = &active[0];
            Ok(TransparentField::Named {
                field,
                ident: field.ident.as_ref().unwrap(),
                ty: &field.ty,
                attrs: Box::new(attrs.clone()),
                skipped_fields: skipped,
            })
        }
        Fields::Unnamed(unnamed) => {
            if unnamed.unnamed.len() != 1 {
                return Err(syn::Error::new_spanned(name, ERROR_TRANSPARENT_TUPLE));
            }

            Ok(TransparentField::Unnamed {
                ty: &unnamed.unnamed[0].ty,
            })
        }
        Fields::Unit => Err(syn::Error::new_spanned(name, ERROR_TRANSPARENT_UNIT)),
    }
}

// =============================================================================
// Flatten Field Helpers
// =============================================================================

/// Generate flatten field merge logic for ToRon serialization
///
/// This helper consolidates the duplicated pattern in ser.rs where flatten fields
/// need to handle both Option::Some(inner) and direct values.
pub fn generate_flatten_value_merge(field_ident: &syn::Ident) -> TokenStream2 {
    quote! {
        match ::ron2::ToRon::to_ron_value(&self.#field_ident)? {
            ::ron2::Value::Option(Some(inner)) => {
                flatten_value_into_fields(*inner, stringify!(#field_ident), &mut fields)?;
            }
            ::ron2::Value::Option(None) => {}
            value => {
                flatten_value_into_fields(value, stringify!(#field_ident), &mut fields)?;
            }
        }
    }
}

/// Generate the runtime helper function for flatten field processing
///
/// This emits a helper function that will be included in the generated ToRon impl.
/// It handles all the different value types that can be flattened into a struct.
pub fn emit_flatten_helper() -> TokenStream2 {
    quote! {
        #[allow(clippy::ptr_arg)]
        fn flatten_value_into_fields(
            value: ::ron2::Value,
            field_name: &str,
            fields: &mut Vec<(String, ::ron2::Value)>,
        ) -> ::ron2::error::Result<()> {
            match value {
                ::ron2::Value::Named {
                    content: ::ron2::NamedContent::Struct(nested_fields),
                    ..
                } => {
                    fields.extend(nested_fields);
                }
                ::ron2::Value::Struct(nested_fields) => {
                    fields.extend(nested_fields);
                }
                ::ron2::Value::Map(map) => {
                    for (key, value) in map {
                        match key {
                            ::ron2::Value::String(name) => {
                                fields.push((name, value));
                            }
                            _ => {
                                return Err(::ron2::Error::new(::ron2::error::ErrorKind::Message(
                                    format!("flatten field {} map keys must be strings", field_name),
                                )));
                            }
                        }
                    }
                }
                _ => {
                    return Err(::ron2::Error::new(::ron2::error::ErrorKind::Message(
                        format!(
                            "flatten field {} must serialize to a struct or map with string keys",
                            field_name
                        ),
                    )));
                }
            }
            Ok(())
        }
    }
}
