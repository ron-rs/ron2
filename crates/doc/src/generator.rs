//! Markdown documentation generation.

use ron2::schema::{Field, Schema, TypeKind, Variant, VariantKind};

use crate::{
    config::{DocConfig, OutputFormat},
    discovery::DiscoveredSchema,
    example::generate_example,
    link::{LinkFrom, LinkResolver, type_path_short_name, type_path_to_md_path, variant_anchor},
};

/// Generates documentation pages for a set of discovered schemas.
///
/// Build it once per run; it indexes the schemas for link resolution.
pub struct DocGenerator<'a> {
    pub(crate) schemas: &'a [DiscoveredSchema],
    pub(crate) config: &'a DocConfig,
    pub(crate) links: LinkResolver<'a>,
}

impl<'a> DocGenerator<'a> {
    pub fn new(schemas: &'a [DiscoveredSchema], config: &'a DocConfig) -> Self {
        Self {
            schemas,
            config,
            links: LinkResolver::new(schemas, config.base_url.as_deref(), config.format),
        }
    }

    /// The path of a type's page, relative to the output directory.
    pub fn page_path(&self, schema: &DiscoveredSchema) -> String {
        type_path_to_md_path(&schema.type_path)
    }

    /// The Markdown page for one type (multi-page mode).
    pub fn type_page(&self, schema: &DiscoveredSchema) -> String {
        let mut out = String::new();
        let name = type_path_short_name(&schema.type_path);
        write_header(
            &mut out,
            name,
            schema.schema.doc.as_deref(),
            self.config.format,
        );
        self.write_type_content(&mut out, schema, LinkFrom::Type(&schema.type_path), 2);
        out.push_str("## Example\n\n");
        out.push_str(&self.example(&schema.schema));
        out
    }

    /// Description and field/variant documentation of a type.
    ///
    /// `level` is the heading level of the sections (Fields, Variants, Type).
    pub(crate) fn write_type_content(
        &self,
        out: &mut String,
        schema: &DiscoveredSchema,
        from: LinkFrom<'_>,
        level: usize,
    ) {
        if let Some(doc) = &schema.schema.doc {
            out.push_str(doc);
            out.push_str("\n\n");
        }

        match &schema.schema.kind {
            TypeKind::Struct { fields } if fields.is_empty() => {
                out.push_str("This is a unit struct with no fields.\n\n");
            }
            TypeKind::Struct { fields } => {
                out.push_str(&heading(level, "Fields"));
                self.write_fields_table(out, fields, from);
            }
            TypeKind::Enum { variants } => {
                self.write_enum_docs(out, &schema.type_path, variants, from, level);
            }
            kind => {
                out.push_str(&heading(level, "Type"));
                out.push_str(&self.links.type_to_markdown(kind, from));
                out.push_str("\n\n");
            }
        }
    }

    /// A fenced RON example for a schema.
    pub(crate) fn example(&self, schema: &Schema) -> String {
        format!(
            "```ron\n{}\n```\n",
            generate_example(schema, self.config.example_depth, &self.links)
        )
    }

    fn write_fields_table(&self, out: &mut String, fields: &[Field], from: LinkFrom<'_>) {
        out.push_str("| Field | Type | Required | Description |\n");
        out.push_str("|-------|------|----------|-------------|\n");
        for field in fields {
            let required = if field.optional { "No" } else { "Yes" };
            let flattened = if field.flattened {
                " *(flattened)*"
            } else {
                ""
            };
            out.push_str(&format!(
                "| `{}` | {} | {} | {}{} |\n",
                field.name,
                self.links.type_to_markdown(&field.ty, from),
                required,
                table_cell(field.doc.as_deref().unwrap_or("—")),
                flattened
            ));
        }
        out.push('\n');
    }

    fn write_enum_docs(
        &self,
        out: &mut String,
        type_path: &str,
        variants: &[Variant],
        from: LinkFrom<'_>,
        level: usize,
    ) {
        if variants.is_empty() {
            out.push_str("This enum has no variants.\n\n");
            return;
        }
        out.push_str(&heading(level, "Variants"));

        let first_line = |v: &Variant| {
            table_cell(
                v.doc
                    .as_deref()
                    .and_then(|d| d.lines().next())
                    .unwrap_or("—"),
            )
        };

        if variants.iter().all(|v| matches!(v.kind, VariantKind::Unit)) {
            out.push_str("| Variant | Description |\n");
            out.push_str("|---------|-------------|\n");
            for variant in variants {
                out.push_str(&format!(
                    "| `{}` | {} |\n",
                    variant.name,
                    first_line(variant)
                ));
            }
            out.push('\n');
            return;
        }

        // Overview table linking to a section per tuple or struct variant.
        out.push_str("| Variant | Kind | Description |\n");
        out.push_str("|---------|------|-------------|\n");
        for variant in variants {
            let (cell, kind) = match variant.kind {
                VariantKind::Unit => (format!("`{}`", variant.name), "Unit"),
                VariantKind::Tuple(_) => (variant_link(type_path, variant), "Tuple"),
                VariantKind::Struct(_) => (variant_link(type_path, variant), "Struct"),
            };
            out.push_str(&format!(
                "| {} | {} | {} |\n",
                cell,
                kind,
                first_line(variant)
            ));
        }
        out.push('\n');

        for variant in variants {
            if matches!(variant.kind, VariantKind::Unit) {
                continue;
            }
            out.push_str(&heading(
                level + 1,
                &format!(
                    "<a id=\"{}\"></a>`{}`",
                    variant_anchor(type_path, &variant.name),
                    variant.name
                ),
            ));
            if let Some(doc) = &variant.doc {
                out.push_str(doc);
                out.push_str("\n\n");
            }
            match &variant.kind {
                VariantKind::Tuple(types) => {
                    let types: Vec<_> = types
                        .iter()
                        .map(|t| self.links.type_to_markdown(t, from))
                        .collect();
                    out.push_str(&format!("**Type:** ({})\n\n", types.join(", ")));
                }
                VariantKind::Struct(fields) if fields.is_empty() => {
                    out.push_str("This variant has no fields.\n\n");
                }
                VariantKind::Struct(fields) => self.write_fields_table(out, fields, from),
                VariantKind::Unit => {}
            }
        }
    }
}

fn variant_link(type_path: &str, variant: &Variant) -> String {
    format!(
        "[`{}`](#{})",
        variant.name,
        variant_anchor(type_path, &variant.name)
    )
}

/// A Markdown heading at the given level.
pub(crate) fn heading(level: usize, text: &str) -> String {
    format!("{} {}\n\n", "#".repeat(level), text)
}

/// Make text safe for a single Markdown table cell.
fn table_cell(text: &str) -> String {
    text.replace('|', "\\|").replace('\n', " ")
}

/// Write the page header: Starlight frontmatter or a Markdown title.
pub(crate) fn write_header(
    out: &mut String,
    title: &str,
    description: Option<&str>,
    format: OutputFormat,
) {
    match format {
        OutputFormat::Starlight => {
            // JSON strings are valid YAML double-quoted scalars, with all escapes YAML understands.
            out.push_str("---\n");
            out.push_str(&format!("title: {}\n", yaml_string(title)));
            if let Some(first_line) = description.and_then(|d| d.lines().next()) {
                out.push_str(&format!("description: {}\n", yaml_string(first_line)));
            }
            out.push_str("---\n\n");
        }
        OutputFormat::Plain => out.push_str(&heading(1, title)),
    }
}

fn yaml_string(text: &str) -> String {
    serde_json::Value::String(text.to_string()).to_string()
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::*;
    use crate::config::OutputMode;

    fn config(format: OutputFormat) -> DocConfig {
        DocConfig {
            input: PathBuf::from("in"),
            output: PathBuf::from("out"),
            base_url: None,
            format,
            output_mode: OutputMode::MultiPage,
            example_depth: 2,
            generate_index: true,
        }
    }

    fn discovered(type_path: &str, schema: Schema) -> DiscoveredSchema {
        DiscoveredSchema {
            path: PathBuf::from("test.schema.ron"),
            type_path: type_path.to_string(),
            schema,
        }
    }

    #[test]
    fn test_write_header_plain() {
        let mut output = String::new();
        write_header(
            &mut output,
            "TestType",
            Some("Test description"),
            OutputFormat::Plain,
        );
        assert_eq!(output, "# TestType\n\n");
    }

    #[test]
    fn test_write_header_starlight() {
        let mut output = String::new();
        write_header(
            &mut output,
            "TestType",
            Some("Test description"),
            OutputFormat::Starlight,
        );
        assert_eq!(
            output,
            "---\ntitle: \"TestType\"\ndescription: \"Test description\"\n---\n\n"
        );
    }

    #[test]
    fn starlight_description_escapes_backslashes() {
        let mut output = String::new();
        write_header(
            &mut output,
            "T",
            Some("Saved to C:\\temp, \"quoted\""),
            OutputFormat::Starlight,
        );
        assert!(
            output.contains(r#"description: "Saved to C:\\temp, \"quoted\"""#),
            "{output}"
        );
    }

    #[test]
    fn test_struct_docs() {
        let schemas = vec![discovered(
            "my::Person",
            Schema::new(TypeKind::Struct {
                fields: vec![
                    Field::new("name", TypeKind::String).with_doc("The name"),
                    Field::optional("age", TypeKind::I32).with_doc("The age"),
                ],
            }),
        )];
        let config = config(OutputFormat::Plain);
        let page = DocGenerator::new(&schemas, &config).type_page(&schemas[0]);

        assert!(page.starts_with("# Person\n\n"));
        assert!(page.contains("## Fields"));
        assert!(page.contains("| `name` | `String` | Yes | The name |"));
        assert!(page.contains("| `age` | `i32` | No | The age |"));
        assert!(page.contains("## Example\n\n```ron\n(\n    name: \"...\",\n)\n```"));
    }

    #[test]
    fn variant_sections_have_type_qualified_anchors_and_shared_tables() {
        let schemas = vec![
            discovered(
                "my::Shape",
                Schema::new(TypeKind::Enum {
                    variants: vec![
                        Variant::unit("Empty"),
                        Variant::tuple(
                            "Point",
                            vec![TypeKind::I32, TypeKind::TypeRef("my::Unit".into())],
                        ),
                        Variant::struct_variant(
                            "Rect",
                            vec![Field::flattened(
                                "size",
                                TypeKind::TypeRef("my::Size".into()),
                            )],
                        ),
                    ],
                }),
            ),
            discovered("my::Unit", Schema::new(TypeKind::Unit)),
            discovered("my::Size", Schema::new(TypeKind::Struct { fields: vec![] })),
        ];
        let config = config(OutputFormat::Plain);
        let page = DocGenerator::new(&schemas, &config).type_page(&schemas[0]);

        assert!(
            page.contains("| [`Point`](#my-Shape.Point) | Tuple |"),
            "{page}"
        );
        assert!(
            page.contains("### <a id=\"my-Shape.Point\"></a>`Point`"),
            "{page}"
        );
        // Tuple types are not wrapped in an extra pair of backticks.
        assert!(
            page.contains("**Type:** (`i32`, [`Unit`](./Unit.md))"),
            "{page}"
        );
        // Struct variants use the same table as structs, including the flattened note.
        assert!(
            page.contains("| `size` | [`Size`](./Size.md) | Yes | — *(flattened)* |"),
            "{page}"
        );
    }
}
