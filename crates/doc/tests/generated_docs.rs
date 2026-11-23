//! End-to-end checks of generated documentation for the showcase schemas.

use std::{
    path::{Component, Path, PathBuf},
    time::{Duration, Instant},
};

use example_showcase::GameConfig;
use ron2::{
    FromRon,
    schema::{Field, RonSchema, Schema, TypeKind, collect::collect_schemas},
};
use ron2_doc::{
    DiscoveredSchema, DocConfig, DocGenerator, LinkResolver, OutputFormat, OutputMode,
    example::generate_example,
};

fn showcase_schemas() -> Vec<DiscoveredSchema> {
    collect_schemas::<GameConfig>()
        .expect("collect showcase schemas")
        .schemas
        .into_iter()
        .map(|(type_path, schema)| DiscoveredSchema {
            path: PathBuf::from(format!("{type_path}.schema.ron")),
            type_path,
            schema,
        })
        .collect()
}

fn config(format: OutputFormat, example_depth: usize) -> DocConfig {
    DocConfig {
        input: PathBuf::from("schemas"),
        output: PathBuf::from("docs"),
        base_url: None,
        format,
        output_mode: OutputMode::MultiPage,
        example_depth,
        generate_index: true,
    }
}

#[test]
fn every_showcase_example_is_valid_ron() {
    let schemas = showcase_schemas();
    let links = LinkResolver::new(&schemas, None, OutputFormat::Plain);
    for depth in 0..=3 {
        for schema in &schemas {
            let example = generate_example(&schema.schema, depth, &links);
            if let Err(err) = ron2::ast::parse_document(&example) {
                panic!(
                    "example for {} at depth {depth} does not parse: {err}\n{example}",
                    schema.type_path
                );
            }
        }
    }
}

#[test]
fn fully_expanded_showcase_example_deserializes() {
    let schemas = showcase_schemas();
    let links = LinkResolver::new(&schemas, None, OutputFormat::Plain);
    let root = GameConfig::type_path().expect("GameConfig type path");
    let root = schemas
        .iter()
        .find(|s| s.type_path == root)
        .expect("GameConfig schema");
    let example = generate_example(&root.schema, 8, &links);
    if let Err(err) = GameConfig::from_ron(&example) {
        panic!("example does not deserialize: {err}\n{example}");
    }
}

/// Collect `](target)` link targets from Markdown.
fn link_targets(markdown: &str) -> Vec<&str> {
    markdown
        .split("](")
        .skip(1)
        .filter_map(|rest| rest.split(')').next())
        .collect()
}

/// Resolve `link` relative to the directory of `page` and normalize `..`.
fn resolve_relative(page: &Path, link: &str) -> PathBuf {
    let mut out = PathBuf::new();
    for component in page
        .parent()
        .unwrap_or(Path::new(""))
        .join(link)
        .components()
    {
        match component {
            Component::ParentDir => {
                out.pop();
            }
            Component::CurDir => {}
            other => out.push(other),
        }
    }
    out
}

#[test]
fn cross_type_links_point_at_generated_pages() {
    let schemas = showcase_schemas();
    let config = config(OutputFormat::Plain, 2);
    let generator = DocGenerator::new(&schemas, &config);
    let pages: Vec<(PathBuf, String)> = schemas
        .iter()
        .map(|s| {
            (
                PathBuf::from(generator.page_path(s)),
                generator.type_page(s),
            )
        })
        .chain(std::iter::once((
            PathBuf::from("index.md"),
            generator.index(),
        )))
        .collect();

    let mut checked = 0;
    for (page, content) in &pages {
        for link in link_targets(content)
            .into_iter()
            .filter(|l| !l.starts_with('#'))
        {
            let target = resolve_relative(page, link);
            assert!(
                pages.iter().any(|(p, _)| *p == target),
                "{} links to {link}, which resolves to missing page {}",
                page.display(),
                target.display()
            );
            checked += 1;
        }
    }
    assert!(
        checked > schemas.len(),
        "expected cross-type links, found {checked}"
    );
}

#[test]
fn generation_scales_linearly_with_schema_count() {
    // Each type references the next one, and all live in one crate.
    let count = 2000;
    let schemas: Vec<DiscoveredSchema> = (0..count)
        .map(|i| DiscoveredSchema {
            path: PathBuf::from(format!("t{i}.schema.ron")),
            type_path: format!("big::T{i}"),
            schema: Schema::new(TypeKind::Struct {
                fields: vec![
                    Field::new(
                        "next",
                        TypeKind::TypeRef(format!("big::T{}", (i + 1) % count)),
                    ),
                    Field::new("value", TypeKind::I32),
                ],
            }),
        })
        .collect();
    let config = config(OutputFormat::Plain, 2);

    let start = Instant::now();
    let generator = DocGenerator::new(&schemas, &config);
    let total: usize = schemas.iter().map(|s| generator.type_page(s).len()).sum();
    let elapsed = start.elapsed();

    assert!(total > 0);
    // Quadratic resolver rebuilding took tens of seconds for a few hundred schemas.
    assert!(
        elapsed < Duration::from_secs(5),
        "generating {count} pages took {elapsed:?}"
    );
}
