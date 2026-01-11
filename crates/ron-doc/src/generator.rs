//! Core markdown documentation generation.

use std::collections::HashMap;

use ron_schema::{Field, Schema, TypeKind, Variant, VariantKind};

use crate::config::{DocConfig, OutputFormat, OutputMode};
use crate::discovery::DiscoveredSchema;
use crate::example::{build_schema_map, generate_example};
use crate::link::{type_path_short_name, LinkResolver};

/// Link mode for type formatting.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LinkMode {
    /// Generate links to separate markdown files
    File,
    /// Generate anchor links within the same page
    Anchor,
}

impl From<OutputMode> for LinkMode {
    fn from(mode: OutputMode) -> Self {
        match mode {
            OutputMode::MultiPage => LinkMode::File,
            OutputMode::SinglePage => LinkMode::Anchor,
        }
    }
}

/// Generate markdown documentation for a schema.
pub fn generate_markdown(
    schema: &DiscoveredSchema,
    config: &DocConfig,
    all_schemas: &[DiscoveredSchema],
) -> String {
    let mut output = String::new();
    let short_name = type_path_short_name(&schema.type_path);
    let resolver = LinkResolver::new(all_schemas, config.base_url.as_deref());
    let link_mode = LinkMode::from(config.output_mode);

    // Header (frontmatter for Starlight)
    write_header(&mut output, short_name, &schema.schema, config.format);

    // Type description
    if let Some(doc) = &schema.schema.doc {
        output.push_str(doc);
        output.push_str("\n\n");
    }

    // Main content based on TypeKind
    match &schema.schema.kind {
        TypeKind::Struct { fields } => {
            write_struct_docs(&mut output, fields, &resolver, link_mode);
        }
        TypeKind::Enum { variants } => {
            write_enum_docs(&mut output, variants, &resolver, link_mode);
        }
        _ => {
            // For primitives, options, etc., just show the type
            output.push_str("## Type\n\n");
            output.push_str(&format_type(&resolver, &schema.schema.kind, link_mode));
            output.push_str("\n\n");
        }
    }

    // RON example
    let schema_map = build_schema_map(all_schemas);
    write_example(&mut output, &schema.schema, config.example_depth, &schema_map);

    output
}

/// Generate type content for a schema (description + fields/variants).
/// Used by single-page generator to embed type docs.
pub fn generate_type_content(
    schema: &DiscoveredSchema,
    resolver: &LinkResolver,
    link_mode: LinkMode,
) -> String {
    let mut output = String::new();

    // Type description
    if let Some(doc) = &schema.schema.doc {
        output.push_str(doc);
        output.push_str("\n\n");
    }

    // Main content based on TypeKind
    match &schema.schema.kind {
        TypeKind::Struct { fields } => {
            write_struct_docs(&mut output, fields, resolver, link_mode);
        }
        TypeKind::Enum { variants } => {
            write_enum_docs(&mut output, variants, resolver, link_mode);
        }
        _ => {
            output.push_str("**Type:** ");
            output.push_str(&format_type(resolver, &schema.schema.kind, link_mode));
            output.push_str("\n\n");
        }
    }

    output
}

/// Generate a RON example for a schema.
pub fn generate_type_example(
    schema: &Schema,
    max_depth: usize,
    all_schemas: &[DiscoveredSchema],
) -> String {
    let schema_map = build_schema_map(all_schemas);
    let mut output = String::new();
    output.push_str("```ron\n");
    output.push_str(&generate_example(schema, max_depth, &schema_map));
    output.push_str("\n```\n");
    output
}

fn write_header(output: &mut String, name: &str, schema: &Schema, format: OutputFormat) {
    match format {
        OutputFormat::Starlight => {
            output.push_str("---\n");
            output.push_str(&format!("title: {}\n", name));
            if let Some(doc) = &schema.doc {
                let first_line = doc.lines().next().unwrap_or("");
                let escaped = first_line.replace('"', "\\\"");
                output.push_str(&format!("description: \"{}\"\n", escaped));
            }
            output.push_str("---\n\n");
        }
        OutputFormat::Plain => {
            output.push_str(&format!("# {}\n\n", name));
        }
    }
}

/// Format a type based on the link mode.
fn format_type(resolver: &LinkResolver, ty: &TypeKind, link_mode: LinkMode) -> String {
    match link_mode {
        LinkMode::File => resolver.type_to_markdown(ty),
        LinkMode::Anchor => resolver.type_to_markdown_anchor(ty),
    }
}

fn write_struct_docs(
    output: &mut String,
    fields: &[Field],
    resolver: &LinkResolver,
    link_mode: LinkMode,
) {
    if fields.is_empty() {
        output.push_str("This is a unit struct with no fields.\n\n");
        return;
    }

    output.push_str("## Fields\n\n");
    output.push_str("| Field | Type | Required | Description |\n");
    output.push_str("|-------|------|----------|-------------|\n");

    for field in fields {
        let type_str = format_type(resolver, &field.ty, link_mode);
        let required = if field.optional { "No" } else { "Yes" };
        let doc = field.doc.as_deref().unwrap_or("—");
        // Escape pipe characters in doc and replace newlines
        let doc_escaped = doc.replace('|', "\\|").replace('\n', " ");

        let flattened_note = if field.flattened { " *(flattened)*" } else { "" };

        output.push_str(&format!(
            "| `{}` | {} | {} | {}{} |\n",
            field.name, type_str, required, doc_escaped, flattened_note
        ));
    }

    output.push('\n');
}

fn write_enum_docs(
    output: &mut String,
    variants: &[Variant],
    resolver: &LinkResolver,
    link_mode: LinkMode,
) {
    if variants.is_empty() {
        output.push_str("This enum has no variants.\n\n");
        return;
    }

    output.push_str("## Variants\n\n");

    // Check if all variants are unit variants (simple enum)
    let all_unit = variants.iter().all(|v| matches!(v.kind, VariantKind::Unit));
    let has_complex = variants
        .iter()
        .any(|v| !matches!(v.kind, VariantKind::Unit));

    if all_unit {
        // Compact table for unit-only enums
        output.push_str("| Variant | Description |\n");
        output.push_str("|---------|-------------|\n");

        for variant in variants {
            let doc = variant.doc.as_deref().unwrap_or("—");
            let doc_escaped = doc.replace('|', "\\|").replace('\n', " ");
            output.push_str(&format!("| `{}` | {} |\n", variant.name, doc_escaped));
        }
        output.push('\n');
    } else {
        // Overview table with links to complex variant sections
        output.push_str("| Variant | Kind | Description |\n");
        output.push_str("|---------|------|-------------|\n");

        for variant in variants {
            let doc = variant
                .doc
                .as_ref()
                .map(|d| d.lines().next().unwrap_or(""))
                .unwrap_or("—");
            let doc_escaped = doc.replace('|', "\\|");

            let kind_label = match &variant.kind {
                VariantKind::Unit => "Unit",
                VariantKind::Tuple(_) => "Tuple",
                VariantKind::Struct(_) => "Struct",
            };

            // Link to section for complex variants
            let variant_cell = if matches!(variant.kind, VariantKind::Unit) {
                format!("`{}`", variant.name)
            } else {
                let anchor = variant.name.to_lowercase();
                format!("[`{}`](#{})", variant.name, anchor)
            };

            output.push_str(&format!(
                "| {} | {} | {} |\n",
                variant_cell, kind_label, doc_escaped
            ));
        }
        output.push('\n');

        // Detailed sections only for complex variants
        if has_complex {
            for variant in variants {
                match &variant.kind {
                    VariantKind::Unit => {
                        // Skip unit variants - already in table
                    }
                    VariantKind::Tuple(types) => {
                        output.push_str(&format!("### `{}`\n\n", variant.name));
                        if let Some(doc) = &variant.doc {
                            output.push_str(doc);
                            output.push_str("\n\n");
                        }
                        let type_strs: Vec<_> = types
                            .iter()
                            .map(|t| format_type(resolver, t, link_mode))
                            .collect();
                        output.push_str(&format!("**Type:** `({})`\n\n", type_strs.join(", ")));
                    }
                    VariantKind::Struct(fields) => {
                        output.push_str(&format!("### `{}`\n\n", variant.name));
                        if let Some(doc) = &variant.doc {
                            output.push_str(doc);
                            output.push_str("\n\n");
                        }
                        output.push_str("**Fields:**\n\n");
                        output.push_str("| Field | Type | Required | Description |\n");
                        output.push_str("|-------|------|----------|-------------|\n");

                        for field in fields {
                            let type_str = format_type(resolver, &field.ty, link_mode);
                            let required = if field.optional { "No" } else { "Yes" };
                            let doc = field.doc.as_deref().unwrap_or("—");
                            let doc_escaped = doc.replace('|', "\\|").replace('\n', " ");

                            output.push_str(&format!(
                                "| `{}` | {} | {} | {} |\n",
                                field.name, type_str, required, doc_escaped
                            ));
                        }
                        output.push('\n');
                    }
                }
            }
        }
    }
}

fn write_example(
    output: &mut String,
    schema: &Schema,
    max_depth: usize,
    schemas: &HashMap<&str, &Schema>,
) {
    output.push_str("## Example\n\n");
    output.push_str("```ron\n");
    output.push_str(&generate_example(schema, max_depth, schemas));
    output.push_str("\n```\n");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_write_header_plain() {
        let mut output = String::new();
        let schema = Schema::with_doc("Test description", TypeKind::Unit);
        write_header(&mut output, "TestType", &schema, OutputFormat::Plain);
        assert_eq!(output, "# TestType\n\n");
    }

    #[test]
    fn test_write_header_starlight() {
        let mut output = String::new();
        let schema = Schema::with_doc("Test description", TypeKind::Unit);
        write_header(&mut output, "TestType", &schema, OutputFormat::Starlight);
        assert!(output.contains("---"));
        assert!(output.contains("title: TestType"));
        assert!(output.contains("description: \"Test description\""));
    }

    #[test]
    fn test_write_struct_docs() {
        let mut output = String::new();
        let fields = vec![
            Field::new("name", TypeKind::String).with_doc("The name"),
            Field::optional("age", TypeKind::I32).with_doc("The age"),
        ];
        let schemas = vec![];
        let resolver = LinkResolver::new(&schemas, None);
        write_struct_docs(&mut output, &fields, &resolver, LinkMode::File);

        assert!(output.contains("## Fields"));
        assert!(output.contains("| `name` |"));
        assert!(output.contains("| Yes |"));
        assert!(output.contains("| `age` |"));
        assert!(output.contains("| No |"));
    }
}
