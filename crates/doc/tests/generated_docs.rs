//! End-to-end checks of generated documentation for the showcase schemas.

use std::{
    path::PathBuf,
    time::{Duration, Instant},
};

use example_showcase::GameConfig;
use ron2::{
    FromRon,
    schema::{Field, RonSchema, Schema, TypeKind, Variant, collect::collect_schemas},
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
            schema: std::sync::Arc::unwrap_or_clone(schema),
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

fn discovered(type_path: &str, kind: TypeKind) -> DiscoveredSchema {
    DiscoveredSchema {
        path: PathBuf::from(format!("{type_path}.schema.ron")),
        type_path: type_path.to_string(),
        schema: Schema::with_doc(format!("Doc of {type_path}\nsecond line | pipe"), kind),
    }
}

/// Schemas with name collisions, deep modules, cycles and a variant whose
/// qualified name equals another type's path.
fn tricky_schemas() -> Vec<DiscoveredSchema> {
    let type_ref = |p: &str| TypeKind::TypeRef(p.to_string());
    let strukt = |fields| TypeKind::Struct { fields };
    vec![
        discovered(
            "a::Config",
            strukt(vec![
                Field::new("port", TypeKind::U16),
                Field::new("b", type_ref("b::Config")),
            ]),
        ),
        discovered(
            "b::Config",
            strukt(vec![
                Field::new("host", TypeKind::String),
                Field::new("a", type_ref("a::Config")),
            ]),
        ),
        discovered(
            "c::User",
            strukt(vec![
                Field::new("ambiguous", type_ref("Config")),
                Field::new("tree", type_ref("c::Tree")),
                Field::new("deep", type_ref("x::y::z::Deep")),
                Field::new("loose", type_ref("Loose")),
                Field::new("ship", type_ref("game::Ship")),
            ]),
        ),
        discovered(
            "c::Tree",
            strukt(vec![
                Field::new("kids", TypeKind::List(Box::new(type_ref("c::Tree")))),
                Field::flattened("me", TypeKind::Option(Box::new(type_ref("c::Tree")))),
            ]),
        ),
        discovered(
            "x::y::z::Deep",
            strukt(vec![
                Field::new("up", type_ref("c::User")),
                Field::new("sibling", type_ref("x::y::Other")),
            ]),
        ),
        discovered(
            "x::y::Other",
            TypeKind::Enum {
                variants: vec![
                    Variant::tuple("T", vec![type_ref("x::y::z::Deep")]),
                    Variant::struct_variant("S", vec![Field::new("d", type_ref("x::y::z::Deep"))]),
                ],
            },
        ),
        discovered("Loose", strukt(vec![Field::new("u", type_ref("c::User"))])),
        discovered(
            "game::Ship",
            TypeKind::Enum {
                variants: vec![
                    Variant::tuple("Fighter", vec![type_ref("game::Ship::Fighter")]),
                    Variant::unit("Docked"),
                ],
            },
        ),
        discovered(
            "game::Ship::Fighter",
            strukt(vec![Field::new("guns", TypeKind::U8)]),
        ),
        discovered(
            "HTTP::Ünïcode_Type",
            strukt(vec![Field::new("x", type_ref("c::User"))]),
        ),
    ]
}

/// Collect `](target)` link targets from Markdown.
fn link_targets(markdown: &str) -> Vec<&str> {
    markdown
        .split("](")
        .skip(1)
        .filter_map(|rest| rest.split(')').next())
        .collect()
}

/// Collect `<a id="...">` anchor ids from Markdown.
fn anchor_ids(markdown: &str) -> Vec<&str> {
    markdown
        .split("<a id=\"")
        .skip(1)
        .filter_map(|rest| rest.split('"').next())
        .collect()
}

/// Resolve `link` against `base` path segments, normalizing `.` and `..`.
fn resolve_segments(base: &[String], link: &str) -> Vec<String> {
    let mut out = base.to_vec();
    for segment in link.split('/') {
        match segment {
            "" | "." => {}
            ".." => {
                out.pop();
            }
            s => out.push(s.to_string()),
        }
    }
    out
}

fn segments(path: &str) -> Vec<String> {
    path.split('/').map(str::to_string).collect()
}

/// Check every link and example of every output flavour; returns the problems.
fn check_output(schemas: &[DiscoveredSchema]) -> Vec<String> {
    let mut problems = Vec::new();
    let mut check_page = |label: &str, content: &str, link_ok: &dyn Fn(&str) -> bool| {
        let ids = anchor_ids(content);
        let mut seen = std::collections::HashSet::new();
        for id in &ids {
            if !seen.insert(id) {
                problems.push(format!("{label}: duplicate anchor {id}"));
            }
        }
        for link in link_targets(content) {
            let ok = match link.strip_prefix('#') {
                Some(anchor) => ids.contains(&anchor),
                None => link_ok(link),
            };
            if !ok {
                problems.push(format!("{label}: broken link {link}"));
            }
        }
        for block in content.split("```ron\n").skip(1) {
            let example = block.split("\n```").next().unwrap_or("");
            if let Err(err) = ron2::ast::parse_document(example) {
                problems.push(format!("{label}: invalid example ({err}):\n{example}"));
            }
        }
    };

    for depth in [0, 1, 2, 5] {
        // Plain Markdown: links are relative to the page's directory.
        let plain = config(OutputFormat::Plain, depth);
        let generator = DocGenerator::new(schemas, &plain);
        let mut pages: Vec<(Vec<String>, String)> = schemas
            .iter()
            .map(|s| (segments(&generator.page_path(s)), generator.type_page(s)))
            .collect();
        pages.push((vec!["index.md".to_string()], generator.index()));
        for (page, content) in &pages {
            let link_ok = |link: &str| {
                let target = resolve_segments(&page[..page.len() - 1], link);
                pages.iter().any(|(p, _)| *p == target)
            };
            check_page(&format!("plain {page:?} d{depth}"), content, &link_ok);
        }

        // Starlight: links are root-relative routes of slugified segments.
        let starlight = config(OutputFormat::Starlight, depth);
        let generator = DocGenerator::new(schemas, &starlight);
        let routes: Vec<Vec<String>> = schemas
            .iter()
            .map(|s| {
                s.type_path
                    .split("::")
                    .map(ron2_doc::link::slugify)
                    .collect()
            })
            .collect();
        let link_ok = |link: &str| {
            link.starts_with('/')
                && link.ends_with('/')
                && routes.contains(&resolve_segments(&[], link))
        };
        for schema in schemas {
            let label = format!("starlight {} d{depth}", schema.type_path);
            check_page(&label, &generator.type_page(schema), &link_ok);
        }
        check_page("starlight index", &generator.index(), &link_ok);
        // Sidebar links, rewritten as Markdown links for the same check.
        let sidebar_links: String = generator
            .sidebar()
            .split("\"link\": \"")
            .skip(1)
            .filter_map(|rest| rest.split('"').next())
            .map(|link| format!("[x]({link})\n"))
            .collect();
        assert!(!sidebar_links.is_empty(), "sidebar has links");
        check_page("starlight sidebar", &sidebar_links, &link_ok);

        // Single page: every link is an anchor on the page.
        for format in [OutputFormat::Plain, OutputFormat::Starlight] {
            let mut config = config(format, depth);
            config.output_mode = OutputMode::SinglePage;
            let page = DocGenerator::new(schemas, &config).single_page();
            check_page(&format!("single {format:?} d{depth}"), &page, &|_| false);
        }
    }
    problems
}

#[test]
fn generated_links_anchors_and_examples_are_valid() {
    for (name, schemas) in [
        ("showcase", showcase_schemas()),
        ("tricky", tricky_schemas()),
    ] {
        let problems = check_output(&schemas);
        assert!(
            problems.is_empty(),
            "{name}: {} problems:\n{}",
            problems.len(),
            problems.join("\n")
        );
    }
}

#[test]
fn cross_type_links_exist() {
    let schemas = showcase_schemas();
    let config = config(OutputFormat::Plain, 2);
    let generator = DocGenerator::new(&schemas, &config);
    let links: usize = schemas
        .iter()
        .map(|s| link_targets(&generator.type_page(s)).len())
        .sum();
    assert!(
        links > schemas.len(),
        "expected cross-type links, found {links}"
    );
}

/// A chain of `count` structs, each referencing the next.
fn chain(count: usize) -> Vec<DiscoveredSchema> {
    (0..count)
        .map(|i| {
            discovered(
                &format!("big::T{i}"),
                TypeKind::Struct {
                    fields: vec![
                        Field::new(
                            "next",
                            TypeKind::TypeRef(format!("big::T{}", (i + 1) % count)),
                        ),
                        Field::new("value", TypeKind::I32),
                    ],
                },
            )
        })
        .collect()
}

fn time_all_pages(schemas: &[DiscoveredSchema]) -> Duration {
    let config = config(OutputFormat::Plain, 2);
    let start = Instant::now();
    let generator = DocGenerator::new(schemas, &config);
    let total: usize = schemas.iter().map(|s| generator.type_page(s).len()).sum();
    assert!(total > 0);
    start.elapsed()
}

#[test]
fn generation_time_grows_linearly_with_schema_count() {
    let small = time_all_pages(&chain(500));
    let large = time_all_pages(&chain(2000));
    // 4x the schemas: linear work takes ~4x, the old quadratic resolver ~16x.
    // The floor keeps timer noise on tiny durations from failing the test.
    let ratio = large.as_secs_f64() / small.max(Duration::from_millis(20)).as_secs_f64();
    assert!(
        ratio < 10.0,
        "500 schemas took {small:?}, 2000 took {large:?} (ratio {ratio:.1})"
    );
}
