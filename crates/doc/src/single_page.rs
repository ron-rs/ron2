//! Single-page documentation generator.

use std::collections::BTreeMap;

use crate::{
    config::DocConfig,
    discovery::DiscoveredSchema,
    generator::{generate_type_content, generate_type_example, LinkMode},
    link::LinkResolver,
};

/// Generate a single markdown document containing all type documentation.
pub fn generate_single_page(schemas: &[DiscoveredSchema], config: &DocConfig) -> String {
    let mut output = String::new();
    let resolver = LinkResolver::new(schemas, None);

    // Title
    output.push_str("# API Reference\n\n");

    // Group schemas by module (first segment of type path)
    let grouped = group_by_module(schemas);

    // Generate table of contents
    write_toc(&mut output, &grouped, &resolver);

    output.push_str("---\n\n");

    // Generate content for each module group
    for (module, module_schemas) in &grouped {
        output.push_str(&format!("## {}\n\n", module));

        for schema in module_schemas {
            let display_name = resolver.display_name(&schema.type_path);
            let anchor = LinkResolver::type_path_to_anchor(&schema.type_path);

            // Type heading with explicit anchor
            output.push_str(&format!("### {} {{#{}}}\n\n", display_name, anchor));

            // Type content (description + fields/variants, heading level 4)
            output.push_str(&generate_type_content(
                schema,
                &resolver,
                LinkMode::Anchor,
                4,
            ));

            // Example
            output.push_str("**Example:**\n\n");
            output.push_str(&generate_type_example(
                &schema.schema,
                config.example_depth,
                schemas,
            ));

            output.push('\n');
        }
    }

    output
}

/// Group schemas by their first module segment.
fn group_by_module(schemas: &[DiscoveredSchema]) -> BTreeMap<String, Vec<&DiscoveredSchema>> {
    let mut grouped: BTreeMap<String, Vec<&DiscoveredSchema>> = BTreeMap::new();

    for schema in schemas {
        let module = schema
            .type_path
            .split("::")
            .next()
            .unwrap_or(&schema.type_path)
            .to_string();
        grouped.entry(module).or_default().push(schema);
    }

    // Sort schemas within each group by short name
    for schemas in grouped.values_mut() {
        schemas.sort_by(|a, b| {
            let a_name = a.type_path.split("::").last().unwrap_or(&a.type_path);
            let b_name = b.type_path.split("::").last().unwrap_or(&b.type_path);
            a_name.cmp(b_name)
        });
    }

    grouped
}

/// Write the table of contents.
fn write_toc(
    output: &mut String,
    grouped: &BTreeMap<String, Vec<&DiscoveredSchema>>,
    resolver: &LinkResolver,
) {
    output.push_str("## Table of Contents\n\n");

    for (module, schemas) in grouped {
        output.push_str(&format!("### {}\n\n", module));

        for schema in schemas {
            let display_name = resolver.display_name(&schema.type_path);
            let anchor = LinkResolver::type_path_to_anchor(&schema.type_path);
            let doc_first_line = schema
                .schema
                .doc
                .as_ref()
                .and_then(|d| d.lines().next())
                .unwrap_or("");

            if doc_first_line.is_empty() {
                output.push_str(&format!("- [`{}`](#{})\n", display_name, anchor));
            } else {
                output.push_str(&format!(
                    "- [`{}`](#{}) — {}\n",
                    display_name, anchor, doc_first_line
                ));
            }
        }

        output.push('\n');
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use ron2::schema::{Schema, TypeKind};

    use super::*;

    fn make_schema(type_path: &str, doc: Option<&str>) -> DiscoveredSchema {
        let schema = if let Some(d) = doc {
            Schema::with_doc(d, TypeKind::Unit)
        } else {
            Schema::new(TypeKind::Unit)
        };
        DiscoveredSchema {
            path: PathBuf::from("test.schema.ron"),
            type_path: type_path.to_string(),
            schema,
        }
    }

    #[test]
    fn test_group_by_module() {
        let schemas = vec![
            make_schema("crate_a::Config", None),
            make_schema("crate_a::Server", None),
            make_schema("crate_b::Client", None),
        ];
        let grouped = group_by_module(&schemas);

        assert_eq!(grouped.len(), 2);
        assert_eq!(grouped.get("crate_a").map(|v| v.len()), Some(2));
        assert_eq!(grouped.get("crate_b").map(|v| v.len()), Some(1));
    }

    #[test]
    fn test_single_page_generation() {
        let schemas = vec![
            make_schema("my_crate::Config", Some("Main configuration.")),
            make_schema("my_crate::Server", Some("Server settings.")),
        ];
        let config = DocConfig {
            input: PathBuf::from("test"),
            output: PathBuf::from("test"),
            base_url: None,
            format: crate::config::OutputFormat::Plain,
            output_mode: crate::config::OutputMode::SinglePage,
            example_depth: 2,
            generate_index: false,
        };

        let output = generate_single_page(&schemas, &config);

        assert!(output.contains("# API Reference"));
        assert!(output.contains("## Table of Contents"));
        assert!(output.contains("### my_crate"));
        assert!(output.contains("[`Config`](#my_crate-config)"));
        assert!(output.contains("[`Server`](#my_crate-server)"));
        assert!(output.contains("### Config {#my_crate-config}"));
        assert!(output.contains("Main configuration."));
    }

    #[test]
    fn test_single_page_with_collision() {
        let schemas = vec![
            make_schema("crate_a::Config", Some("Config A")),
            make_schema("crate_b::Config", Some("Config B")),
        ];
        let config = DocConfig {
            input: PathBuf::from("test"),
            output: PathBuf::from("test"),
            base_url: None,
            format: crate::config::OutputFormat::Plain,
            output_mode: crate::config::OutputMode::SinglePage,
            example_depth: 2,
            generate_index: false,
        };

        let output = generate_single_page(&schemas, &config);

        // With collision, should show full paths
        assert!(output.contains("[`crate_a::Config`](#crate_a-config)"));
        assert!(output.contains("[`crate_b::Config`](#crate_b-config)"));
    }
}
