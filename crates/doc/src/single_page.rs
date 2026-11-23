//! Single-page documentation generator.

use crate::{
    generator::{DocGenerator, heading, write_header},
    index::group_by_module,
    link::{LinkFrom, type_anchor},
};

impl DocGenerator<'_> {
    /// One Markdown document with a table of contents and every type.
    ///
    /// Types carry explicit `<a id>` anchors qualified by their full path, so
    /// links work on GitHub and other CommonMark renderers and never collide.
    pub fn single_page(&self) -> String {
        let mut out = String::new();
        write_header(&mut out, "API Reference", None, self.config.format);

        let grouped = group_by_module(self.schemas);

        out.push_str(&heading(2, "Table of Contents"));
        for (module, schemas) in &grouped {
            out.push_str(&heading(3, module));
            for schema in schemas {
                let entry = format!(
                    "- [`{}`](#{})",
                    self.links.display_name(&schema.type_path),
                    type_anchor(&schema.type_path)
                );
                match schema.schema.doc.as_deref().and_then(|d| d.lines().next()) {
                    Some(line) if !line.is_empty() => {
                        out.push_str(&format!("{entry} — {line}\n"));
                    }
                    _ => out.push_str(&format!("{entry}\n")),
                }
            }
            out.push('\n');
        }
        out.push_str("---\n\n");

        for (module, schemas) in &grouped {
            out.push_str(&heading(2, module));
            for schema in schemas {
                out.push_str(&heading(
                    3,
                    &format!(
                        "<a id=\"{}\"></a>{}",
                        type_anchor(&schema.type_path),
                        self.links.display_name(&schema.type_path)
                    ),
                ));
                self.write_type_content(&mut out, schema, LinkFrom::SamePage, 4);
                out.push_str("**Example:**\n\n");
                out.push_str(&self.example(&schema.schema));
                out.push('\n');
            }
        }

        out
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use ron2::schema::{Schema, TypeKind, Variant};

    use super::*;
    use crate::{
        config::{DocConfig, OutputFormat, OutputMode},
        discovery::DiscoveredSchema,
    };

    fn make_schema(type_path: &str, doc: Option<&str>) -> DiscoveredSchema {
        let schema = match doc {
            Some(d) => Schema::with_doc(d, TypeKind::Unit),
            None => Schema::new(TypeKind::Unit),
        };
        DiscoveredSchema {
            path: PathBuf::from("test.schema.ron"),
            type_path: type_path.to_string(),
            schema,
        }
    }

    fn single_page(schemas: &[DiscoveredSchema]) -> String {
        let config = DocConfig {
            input: PathBuf::from("test"),
            output: PathBuf::from("test"),
            base_url: None,
            format: OutputFormat::Plain,
            output_mode: OutputMode::SinglePage,
            example_depth: 2,
            generate_index: false,
        };
        DocGenerator::new(schemas, &config).single_page()
    }

    #[test]
    fn test_single_page_generation() {
        let output = single_page(&[
            make_schema("my_crate::Config", Some("Main configuration.")),
            make_schema("my_crate::Server", Some("Server settings.")),
        ]);

        assert!(output.contains("# API Reference"));
        assert!(output.contains("## Table of Contents"));
        assert!(output.contains("### my_crate"));
        assert!(output.contains("[`Config`](#my_crate-config)"));
        assert!(output.contains("[`Server`](#my_crate-server)"));
        assert!(output.contains("### <a id=\"my_crate-config\"></a>Config\n"));
        assert!(!output.contains("{#"), "no Pandoc-only heading attributes");
        assert!(output.contains("Main configuration."));
    }

    #[test]
    fn test_single_page_with_collision() {
        let output = single_page(&[
            make_schema("crate_a::Config", Some("Config A")),
            make_schema("crate_b::Config", Some("Config B")),
        ]);
        assert!(output.contains("[`crate_a::Config`](#crate_a-config)"));
        assert!(output.contains("[`crate_b::Config`](#crate_b-config)"));
    }

    #[test]
    fn variant_anchors_do_not_collide_across_types() {
        let enum_schema = |path: &str| DiscoveredSchema {
            path: PathBuf::from("test.schema.ron"),
            type_path: path.to_string(),
            schema: Schema::new(TypeKind::Enum {
                variants: vec![Variant::tuple("Custom", vec![TypeKind::I32])],
            }),
        };
        let output = single_page(&[enum_schema("app::Color"), enum_schema("app::Size")]);
        assert!(output.contains("<a id=\"app-color-custom\"></a>"));
        assert!(output.contains("<a id=\"app-size-custom\"></a>"));
    }
}
