//! Index page and sidebar generation.

use std::collections::BTreeMap;

use crate::{
    config::OutputFormat,
    discovery::DiscoveredSchema,
    link::{type_path_short_name, type_path_to_md_path},
};

/// Generate an index page listing all types.
pub fn generate_index(schemas: &[DiscoveredSchema], format: OutputFormat) -> String {
    let mut output = String::new();

    // Header
    match format {
        OutputFormat::Starlight => {
            output.push_str("---\n");
            output.push_str("title: Schema Reference\n");
            output.push_str("description: \"Index of all RON schema types\"\n");
            output.push_str("---\n\n");
        }
        OutputFormat::Plain => {
            output.push_str("# Schema Reference\n\n");
        }
    }

    // Group by module (first path segment)
    let grouped = group_by_module(schemas);

    for (module, module_schemas) in &grouped {
        output.push_str(&format!("## {}\n\n", module));

        for schema in module_schemas {
            let short_name = type_path_short_name(&schema.type_path);
            let md_path = type_path_to_md_path(&schema.type_path);
            let description = schema
                .schema
                .doc
                .as_ref()
                .map(|d| first_line(d))
                .unwrap_or_default();

            if description.is_empty() {
                output.push_str(&format!("- [`{}`](./{})\n", short_name, md_path));
            } else {
                output.push_str(&format!(
                    "- [`{}`](./{}) — {}\n",
                    short_name, md_path, description
                ));
            }
        }

        output.push('\n');
    }

    output
}

/// Generate a sidebar JSON file for Starlight/Docusaurus.
pub fn generate_sidebar(schemas: &[DiscoveredSchema]) -> String {
    let grouped = group_by_module(schemas);

    let mut sidebar = Vec::new();

    for (module, module_schemas) in &grouped {
        let items: Vec<String> = module_schemas
            .iter()
            .map(|s| type_path_to_md_path(&s.type_path))
            .collect();

        sidebar.push(serde_json::json!({
            "label": module,
            "items": items
        }));
    }

    serde_json::to_string_pretty(&sidebar).unwrap_or_else(|_| "[]".to_string())
}

/// Group schemas by their first module segment.
fn group_by_module(schemas: &[DiscoveredSchema]) -> BTreeMap<String, Vec<&DiscoveredSchema>> {
    let mut groups: BTreeMap<String, Vec<&DiscoveredSchema>> = BTreeMap::new();

    for schema in schemas {
        let module = schema
            .type_path
            .split("::")
            .next()
            .unwrap_or("_root")
            .to_string();

        groups.entry(module).or_default().push(schema);
    }

    groups
}

/// Get the first line of a string.
fn first_line(s: &str) -> &str {
    s.lines().next().unwrap_or(s)
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use ron2::schema::{Schema, TypeKind};

    use super::*;

    fn make_schema(type_path: &str, doc: Option<&str>) -> DiscoveredSchema {
        DiscoveredSchema {
            path: PathBuf::from("test.schema.ron"),
            type_path: type_path.to_string(),
            schema: if let Some(d) = doc {
                Schema::with_doc(d, TypeKind::Unit)
            } else {
                Schema::new(TypeKind::Unit)
            },
        }
    }

    #[test]
    fn test_generate_index() {
        let schemas = vec![
            make_schema("my_crate::Config", Some("Configuration type")),
            make_schema("my_crate::Settings", None),
        ];

        let index = generate_index(&schemas, OutputFormat::Plain);
        assert!(index.contains("# Schema Reference"));
        assert!(index.contains("## my_crate"));
        assert!(index.contains("[`Config`]"));
        assert!(index.contains("Configuration type"));
    }

    #[test]
    fn test_group_by_module() {
        let schemas = vec![
            make_schema("crate_a::TypeA", None),
            make_schema("crate_b::TypeB", None),
            make_schema("crate_a::TypeC", None),
        ];

        let grouped = group_by_module(&schemas);
        assert_eq!(grouped.len(), 2);
        assert_eq!(grouped.get("crate_a").map(|v| v.len()), Some(2));
        assert_eq!(grouped.get("crate_b").map(|v| v.len()), Some(1));
    }
}
