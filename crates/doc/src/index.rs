//! Index page and sidebar generation.

use std::collections::BTreeMap;

use crate::{
    discovery::DiscoveredSchema,
    generator::{DocGenerator, heading, write_header},
    link::{LinkFrom, type_path_short_name},
};

impl DocGenerator<'_> {
    /// An index page listing all types, grouped by crate.
    pub fn index(&self) -> String {
        let mut out = String::new();
        write_header(
            &mut out,
            "Schema Reference",
            Some("Index of all RON schema types"),
            self.config.format,
        );

        for (module, schemas) in group_by_module(self.schemas) {
            out.push_str(&heading(2, module));
            for schema in schemas {
                let link = format!(
                    "- [`{}`]({})",
                    self.links.display_name(&schema.type_path),
                    self.links.href(&schema.type_path, LinkFrom::Index)
                );
                match schema.schema.doc.as_deref().and_then(|d| d.lines().next()) {
                    Some(line) if !line.is_empty() => out.push_str(&format!("{link} — {line}\n")),
                    _ => out.push_str(&format!("{link}\n")),
                }
            }
            out.push('\n');
        }
        out
    }

    /// A Starlight sidebar definition (JSON) with one group per crate.
    ///
    /// Items link to the generated pages; set `base_url` to the route the
    /// output directory is served under so the links are absolute.
    pub fn sidebar(&self) -> String {
        let sidebar: Vec<_> = group_by_module(self.schemas)
            .into_iter()
            .map(|(module, schemas)| {
                let items: Vec<_> = schemas
                    .iter()
                    .map(|s| {
                        serde_json::json!({
                            "label": type_path_short_name(&s.type_path),
                            "link": self.links.href(&s.type_path, LinkFrom::Index),
                        })
                    })
                    .collect();
                serde_json::json!({ "label": module, "items": items })
            })
            .collect();
        serde_json::to_string_pretty(&sidebar).unwrap_or_else(|_| "[]".to_string())
    }
}

/// Group schemas by their first path segment (the crate), sorted by type path.
///
/// Types without a module path are grouped under `_root`.
pub(crate) fn group_by_module(
    schemas: &[DiscoveredSchema],
) -> BTreeMap<&str, Vec<&DiscoveredSchema>> {
    let mut groups: BTreeMap<&str, Vec<&DiscoveredSchema>> = BTreeMap::new();
    for schema in schemas {
        let module = match schema.type_path.split_once("::") {
            Some((module, _)) => module,
            None => "_root",
        };
        groups.entry(module).or_default().push(schema);
    }
    for group in groups.values_mut() {
        group.sort_by(|a, b| a.type_path.cmp(&b.type_path));
    }
    groups
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use ron2::schema::{Schema, TypeKind};

    use super::*;
    use crate::config::{DocConfig, OutputFormat, OutputMode};

    fn make_schema(type_path: &str, doc: Option<&str>) -> DiscoveredSchema {
        DiscoveredSchema {
            path: PathBuf::from("test.schema.ron"),
            type_path: type_path.to_string(),
            schema: match doc {
                Some(d) => Schema::with_doc(d, TypeKind::Unit),
                None => Schema::new(TypeKind::Unit),
            },
        }
    }

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

    #[test]
    fn test_generate_index() {
        let schemas = vec![
            make_schema("my_crate::Config", Some("Configuration type")),
            make_schema("my_crate::Settings", None),
        ];
        let config = config(OutputFormat::Plain);
        let index = DocGenerator::new(&schemas, &config).index();
        assert!(index.contains("# Schema Reference"));
        assert!(index.contains("## my_crate"));
        assert!(index.contains("- [`Config`](./my_crate/Config.md) — Configuration type\n"));
        assert!(index.contains("- [`Settings`](./my_crate/Settings.md)\n"));
    }

    #[test]
    fn starlight_index_and_sidebar_use_routes() {
        let schemas = vec![make_schema("my_crate::Config", None)];
        let config = config(OutputFormat::Starlight);
        let generator = DocGenerator::new(&schemas, &config);
        assert!(generator.index().contains("(./my_crate/config/)"));
        assert!(
            generator
                .sidebar()
                .contains("\"link\": \"./my_crate/config/\"")
        );
    }

    #[test]
    fn test_group_by_module() {
        let schemas = vec![
            make_schema("crate_b::TypeB", None),
            make_schema("crate_a::TypeC", None),
            make_schema("crate_a::TypeA", None),
            make_schema("Loose", None),
        ];
        let grouped = group_by_module(&schemas);
        let names = |module: &str| -> Vec<&str> {
            grouped[module]
                .iter()
                .map(|s| s.type_path.as_str())
                .collect()
        };
        assert_eq!(names("crate_a"), ["crate_a::TypeA", "crate_a::TypeC"]);
        assert_eq!(names("crate_b"), ["crate_b::TypeB"]);
        assert_eq!(names("_root"), ["Loose"]);
    }
}
