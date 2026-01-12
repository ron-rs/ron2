//! Schema code generation - generates TokenStream2 for runtime TypeKind construction.

use std::collections::{BTreeSet, HashSet};

use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{DeriveInput, Fields, GenericArgument, GenericParam, PathArguments, Type};

use crate::{
    attr::{extract_doc_comment, ContainerAttrs, FieldAttrs, VariantAttrs},
    type_mapper::{type_to_type_kind, PrimitiveKind},
};

/// Generate the RonSchemaType implementation for a type.
pub fn impl_ron_schema(
    input: &DeriveInput,
    container_attrs: &ContainerAttrs,
) -> syn::Result<TokenStream2> {
    let name = &input.ident;
    let type_name = name.to_string();

    // Extract doc comment
    let doc = extract_doc_comment(&input.attrs);

    // Generate the TypeKind based on the data type
    let type_kind_tokens = match &input.data {
        syn::Data::Struct(data_struct) => {
            generate_struct_kind(&data_struct.fields, container_attrs)?
        }
        syn::Data::Enum(data_enum) => generate_enum_kind(data_enum, container_attrs)?,
        syn::Data::Union(_) => {
            return Err(syn::Error::new_spanned(
                input,
                "RonSchema cannot be derived for unions",
            ));
        }
    };

    // Generate the schema construction code
    let schema_tokens = if let Some(doc_str) = doc {
        quote! {
            ::ron2::schema::Schema::with_doc(#doc_str, #type_kind_tokens)
        }
    } else {
        quote! {
            ::ron2::schema::Schema::new(#type_kind_tokens)
        }
    };

    let child_schemas_tokens = generate_child_schemas(input, container_attrs)?;

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let expanded = quote! {
        impl #impl_generics ::ron2::schema::RonSchemaType for #name #ty_generics #where_clause {
            fn type_kind() -> ::ron2::schema::TypeKind {
                #type_kind_tokens
            }

            fn schema() -> ::ron2::schema::Schema {
                #schema_tokens
            }

            fn type_path() -> Option<&'static str> {
                Some(concat!(module_path!(), "::", #type_name))
            }

            fn child_schemas() -> &'static [&'static ::ron2::schema::SchemaEntry] {
                #child_schemas_tokens
            }
        }

        impl #impl_generics #name #ty_generics #where_clause {
            #[doc(hidden)]
            pub const __RON_SCHEMA_ENTRY: ::ron2::schema::SchemaEntry = ::ron2::schema::SchemaEntry {
                type_path: concat!(module_path!(), "::", #type_name),
                schema: <Self as ::ron2::schema::RonSchemaType>::schema,
                children: <Self as ::ron2::schema::RonSchemaType>::child_schemas,
            };
        }
    };

    Ok(expanded)
}

/// Generate TypeKind tokens for a struct's fields.
pub fn generate_struct_kind(
    fields: &Fields,
    container_attrs: &ContainerAttrs,
) -> syn::Result<TokenStream2> {
    // Handle transparent structs - return the inner type's TypeKind
    if container_attrs.transparent {
        return generate_transparent_struct_kind(fields);
    }

    match fields {
        Fields::Named(named) => {
            let mut field_tokens: Vec<TokenStream2> = Vec::new();

            for f in named.named.iter() {
                let attrs = FieldAttrs::from_ast(&f.attrs)?;

                // Skip fields marked with #[ron(skip)]
                if attrs.skip {
                    continue;
                }

                field_tokens.push(generate_field(f, &attrs, container_attrs)?);
            }

            Ok(quote! {
                ::ron2::schema::TypeKind::Struct {
                    fields: vec![#(#field_tokens),*],
                }
            })
        }
        Fields::Unnamed(unnamed) => {
            // Tuple struct -> TypeKind::Tuple
            let type_tokens: Vec<TokenStream2> = unnamed
                .unnamed
                .iter()
                .map(|f| type_to_type_kind(&f.ty))
                .collect();

            Ok(quote! {
                ::ron2::schema::TypeKind::Tuple(vec![#(#type_tokens),*])
            })
        }
        Fields::Unit => {
            // Unit struct -> empty struct
            Ok(quote! {
                ::ron2::schema::TypeKind::Unit
            })
        }
    }
}

/// Generate TypeKind tokens for a transparent struct.
///
/// Returns the inner type's TypeKind directly.
fn generate_transparent_struct_kind(fields: &Fields) -> syn::Result<TokenStream2> {
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

            Ok(type_to_type_kind(&active_fields[0].ty))
        }
        Fields::Unnamed(unnamed) => {
            if unnamed.unnamed.len() != 1 {
                return Err(syn::Error::new_spanned(
                    unnamed,
                    "#[ron(transparent)] requires exactly one field for tuple structs",
                ));
            }

            Ok(type_to_type_kind(&unnamed.unnamed[0].ty))
        }
        Fields::Unit => Err(syn::Error::new(
            proc_macro2::Span::call_site(),
            "#[ron(transparent)] cannot be used on unit structs",
        )),
    }
}

/// Generate TypeKind tokens for an enum.
pub fn generate_enum_kind(
    data_enum: &syn::DataEnum,
    container_attrs: &ContainerAttrs,
) -> syn::Result<TokenStream2> {
    let variant_tokens: Vec<TokenStream2> = data_enum
        .variants
        .iter()
        .map(|v| generate_variant(v, container_attrs))
        .collect::<syn::Result<Vec<_>>>()?;

    Ok(quote! {
        ::ron2::schema::TypeKind::Enum {
            variants: vec![#(#variant_tokens),*],
        }
    })
}

/// Generate Field tokens for a struct field.
fn generate_field(
    field: &syn::Field,
    attrs: &FieldAttrs,
    container_attrs: &ContainerAttrs,
) -> syn::Result<TokenStream2> {
    let original_name = field
        .ident
        .as_ref()
        .ok_or_else(|| syn::Error::new_spanned(field, "Expected named field"))?
        .to_string();

    let name = attrs.effective_name(&original_name, container_attrs);
    let type_kind = type_to_type_kind(&field.ty);
    let doc = extract_doc_comment(&field.attrs);
    let optional = attrs.has_default();
    let flattened = attrs.flatten;

    let doc_tokens = match doc {
        Some(d) => quote! { Some(#d.to_string()) },
        None => quote! { None },
    };

    Ok(quote! {
        ::ron2::schema::Field {
            name: #name.to_string(),
            ty: #type_kind,
            doc: #doc_tokens,
            optional: #optional,
            flattened: #flattened,
        }
    })
}

/// Generate Variant tokens for an enum variant.
fn generate_variant(
    variant: &syn::Variant,
    container_attrs: &ContainerAttrs,
) -> syn::Result<TokenStream2> {
    let variant_attrs = VariantAttrs::from_ast(&variant.attrs)?;
    let original_name = variant.ident.to_string();
    let name = variant_attrs.effective_name(&original_name, container_attrs);
    let doc = extract_doc_comment(&variant.attrs);

    let doc_tokens = match doc {
        Some(d) => quote! { Some(#d.to_string()) },
        None => quote! { None },
    };

    let kind_tokens = match &variant.fields {
        Fields::Unit => quote! { ::ron2::schema::VariantKind::Unit },
        Fields::Unnamed(unnamed) => {
            let type_tokens: Vec<TokenStream2> = unnamed
                .unnamed
                .iter()
                .map(|f| type_to_type_kind(&f.ty))
                .collect();

            quote! {
                ::ron2::schema::VariantKind::Tuple(vec![#(#type_tokens),*])
            }
        }
        Fields::Named(named) => {
            let mut field_tokens: Vec<TokenStream2> = Vec::new();

            for f in named.named.iter() {
                let attrs = FieldAttrs::from_ast(&f.attrs)?;

                // Skip fields marked with #[ron(skip)]
                if attrs.skip {
                    continue;
                }

                field_tokens.push(generate_field(f, &attrs, container_attrs)?);
            }

            quote! {
                ::ron2::schema::VariantKind::Struct(vec![#(#field_tokens),*])
            }
        }
    };

    Ok(quote! {
        ::ron2::schema::Variant {
            name: #name.to_string(),
            doc: #doc_tokens,
            kind: #kind_tokens,
        }
    })
}

fn generate_child_schemas(
    input: &DeriveInput,
    container_attrs: &ContainerAttrs,
) -> syn::Result<TokenStream2> {
    let generics = collect_generic_idents(&input.generics);
    let mut child_paths = Vec::new();

    match &input.data {
        syn::Data::Struct(data_struct) => {
            collect_struct_children(
                &data_struct.fields,
                container_attrs,
                &generics,
                &mut child_paths,
            )?;
        }
        syn::Data::Enum(data_enum) => {
            for variant in &data_enum.variants {
                collect_variant_children(variant, &generics, &mut child_paths)?;
            }
        }
        syn::Data::Union(_) => {}
    }

    let mut seen = BTreeSet::new();
    let mut entries = Vec::new();
    for path in child_paths {
        let key = quote!(#path).to_string();
        if seen.insert(key) {
            entries.push(quote! { &<#path>::__RON_SCHEMA_ENTRY });
        }
    }

    if entries.is_empty() {
        Ok(quote! { &[] })
    } else {
        Ok(quote! { &[#(#entries),*] })
    }
}

fn collect_generic_idents(generics: &syn::Generics) -> HashSet<String> {
    generics
        .params
        .iter()
        .filter_map(|param| match param {
            GenericParam::Type(ty) => Some(ty.ident.to_string()),
            _ => None,
        })
        .collect()
}

fn collect_struct_children(
    fields: &Fields,
    container_attrs: &ContainerAttrs,
    generics: &HashSet<String>,
    out: &mut Vec<syn::Path>,
) -> syn::Result<()> {
    if container_attrs.transparent {
        return collect_transparent_struct_children(fields, generics, out);
    }

    match fields {
        Fields::Named(named) => {
            for f in named.named.iter() {
                let attrs = FieldAttrs::from_ast(&f.attrs)?;
                if attrs.skip {
                    continue;
                }
                collect_child_types_from_type(&f.ty, generics, out);
            }
        }
        Fields::Unnamed(unnamed) => {
            for f in &unnamed.unnamed {
                collect_child_types_from_type(&f.ty, generics, out);
            }
        }
        Fields::Unit => {}
    }

    Ok(())
}

fn collect_transparent_struct_children(
    fields: &Fields,
    generics: &HashSet<String>,
    out: &mut Vec<syn::Path>,
) -> syn::Result<()> {
    match fields {
        Fields::Named(named) => {
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

            collect_child_types_from_type(&active_fields[0].ty, generics, out);
        }
        Fields::Unnamed(unnamed) => {
            if unnamed.unnamed.len() != 1 {
                return Err(syn::Error::new_spanned(
                    unnamed,
                    "#[ron(transparent)] requires exactly one field for tuple structs",
                ));
            }

            collect_child_types_from_type(&unnamed.unnamed[0].ty, generics, out);
        }
        Fields::Unit => {
            return Err(syn::Error::new(
                proc_macro2::Span::call_site(),
                "#[ron(transparent)] cannot be used on unit structs",
            ));
        }
    }

    Ok(())
}

fn collect_variant_children(
    variant: &syn::Variant,
    generics: &HashSet<String>,
    out: &mut Vec<syn::Path>,
) -> syn::Result<()> {
    let _ = VariantAttrs::from_ast(&variant.attrs)?;
    match &variant.fields {
        Fields::Unit => {}
        Fields::Unnamed(unnamed) => {
            for f in &unnamed.unnamed {
                collect_child_types_from_type(&f.ty, generics, out);
            }
        }
        Fields::Named(named) => {
            for f in named.named.iter() {
                let attrs = FieldAttrs::from_ast(&f.attrs)?;
                if attrs.skip {
                    continue;
                }
                collect_child_types_from_type(&f.ty, generics, out);
            }
        }
    }

    Ok(())
}

fn collect_child_types_from_type(ty: &Type, generics: &HashSet<String>, out: &mut Vec<syn::Path>) {
    match ty {
        Type::Path(type_path) => {
            if type_path.qself.is_some() {
                return;
            }

            let path = &type_path.path;
            let segment = match path.segments.last() {
                Some(seg) => seg,
                None => return,
            };

            let ident_str = segment.ident.to_string();

            if path.segments.len() == 1 && generics.contains(&ident_str) {
                return;
            }

            if PrimitiveKind::from_ident(&ident_str).is_some() {
                return;
            }

            if let PathArguments::AngleBracketed(args) = &segment.arguments {
                let generic_args: Vec<_> = args
                    .args
                    .iter()
                    .filter_map(|arg| match arg {
                        GenericArgument::Type(t) => Some(t),
                        _ => None,
                    })
                    .collect();

                match ident_str.as_str() {
                    "Option" if generic_args.len() == 1 => {
                        collect_child_types_from_type(generic_args[0], generics, out);
                        return;
                    }
                    "Vec" | "VecDeque" | "HashSet" | "BTreeSet" | "LinkedList"
                        if generic_args.len() == 1 =>
                    {
                        collect_child_types_from_type(generic_args[0], generics, out);
                        return;
                    }
                    "HashMap" | "BTreeMap" if generic_args.len() == 2 => {
                        collect_child_types_from_type(generic_args[0], generics, out);
                        collect_child_types_from_type(generic_args[1], generics, out);
                        return;
                    }
                    "Box" if generic_args.len() == 1 => {
                        collect_child_types_from_type(generic_args[0], generics, out);
                        return;
                    }
                    _ => {}
                }
            }

            if is_std_like_path(path) {
                return;
            }

            out.push(path.clone());
        }
        Type::Tuple(tuple) => {
            for elem in &tuple.elems {
                collect_child_types_from_type(elem, generics, out);
            }
        }
        Type::Reference(reference) => {
            collect_child_types_from_type(&reference.elem, generics, out);
        }
        Type::Array(array) => {
            collect_child_types_from_type(&array.elem, generics, out);
        }
        Type::Slice(slice) => {
            collect_child_types_from_type(&slice.elem, generics, out);
        }
        Type::Paren(paren) => {
            collect_child_types_from_type(&paren.elem, generics, out);
        }
        Type::Group(group) => {
            collect_child_types_from_type(&group.elem, generics, out);
        }
        _ => {}
    }
}

fn is_std_like_path(path: &syn::Path) -> bool {
    let first = match path.segments.first() {
        Some(seg) => seg.ident.to_string(),
        None => return false,
    };

    matches!(first.as_str(), "std" | "core" | "alloc")
}
