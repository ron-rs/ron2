//! `TypeRef` resolution and cross-reference links.

use ahash::{HashMap, HashMapExt};
use ron2::schema::TypeKind;

use crate::{config::OutputFormat, discovery::DiscoveredSchema};

/// The page a link is written on, which decides how its target is addressed.
#[derive(Debug, Clone, Copy)]
pub enum LinkFrom<'p> {
    /// Single-page output: link to anchors on the same page.
    SamePage,
    /// The index page at the root of the output directory.
    Index,
    /// The page of the type with this path.
    Type(&'p str),
}

/// Resolves `TypeRef` paths to discovered schemas and renders links to them.
///
/// Build it once per run: construction indexes all schemas, and lookups are
/// hash-map based.
pub struct LinkResolver<'a> {
    by_path: HashMap<&'a str, &'a DiscoveredSchema>,
    by_short_name: HashMap<&'a str, Vec<&'a DiscoveredSchema>>,
    base_url: Option<&'a str>,
    format: OutputFormat,
}

impl<'a> LinkResolver<'a> {
    /// Index the discovered schemas.
    ///
    /// `base_url` makes links absolute (e.g. `/docs/types`); `format` selects
    /// between Markdown file links and Starlight routes.
    pub fn new(
        schemas: &'a [DiscoveredSchema],
        base_url: Option<&'a str>,
        format: OutputFormat,
    ) -> Self {
        let mut by_path = HashMap::with_capacity(schemas.len());
        let mut by_short_name: HashMap<&str, Vec<&DiscoveredSchema>> =
            HashMap::with_capacity(schemas.len());
        for schema in schemas {
            by_path.insert(schema.type_path.as_str(), schema);
            by_short_name
                .entry(type_path_short_name(&schema.type_path))
                .or_default()
                .push(schema);
        }
        Self {
            by_path,
            by_short_name,
            base_url,
            format,
        }
    }

    /// Find the discovered schema a `TypeRef` points to.
    ///
    /// An exact path match wins. Otherwise the paths may differ in their module
    /// prefix (for example when schemas were discovered from a subdirectory),
    /// so a unique schema whose path is a suffix of the `TypeRef` (or the other
    /// way round) is accepted. Ambiguous references resolve to nothing.
    pub fn resolve(&self, type_ref: &str) -> Option<&'a DiscoveredSchema> {
        let type_ref = strip_generics(type_ref);
        if let Some(schema) = self.by_path.get(type_ref) {
            return Some(schema);
        }
        let mut matches = self
            .by_short_name
            .get(type_path_short_name(type_ref))?
            .iter()
            .filter(|s| {
                is_path_suffix(&s.type_path, type_ref) || is_path_suffix(type_ref, &s.type_path)
            });
        let first = matches.next()?;
        matches.next().is_none().then_some(*first)
    }

    /// The name to show for a type: its short name, or the full path when
    /// several discovered types share that short name.
    pub fn display_name<'p>(&self, type_path: &'p str) -> &'p str {
        let short = type_path_short_name(type_path);
        match self.by_short_name.get(short) {
            Some(same_name) if same_name.len() > 1 => type_path,
            _ => short,
        }
    }

    /// The link target for a type's documentation, as seen from `from`.
    pub fn href(&self, type_path: &str, from: LinkFrom<'_>) -> String {
        if let LinkFrom::SamePage = from {
            return format!("#{}", type_anchor(type_path));
        }
        let starlight = matches!(self.format, OutputFormat::Starlight);
        let target = self.route_segments(type_path);
        let trailing = if starlight { "/" } else { "" };
        if let Some(base) = self.base_url {
            let base = base.trim_end_matches('/');
            return format!("{base}/{}{trailing}", target.join("/"));
        }

        let from_dir: Vec<String> = match from {
            LinkFrom::SamePage | LinkFrom::Index => Vec::new(),
            // A Starlight page is served as a directory; a Markdown page lives in one.
            LinkFrom::Type(path) if starlight => self.route_segments(path),
            LinkFrom::Type(path) => {
                let mut segments = self.route_segments(path);
                segments.pop();
                segments
            }
        };

        let common = from_dir
            .iter()
            .zip(&target)
            .take_while(|(a, b)| a == b)
            .count();
        let rest = target[common..].join("/");
        let up = from_dir.len() - common;
        match (up, rest.is_empty()) {
            (0, true) => "./".to_string(),
            (0, false) => format!("./{rest}{trailing}"),
            (_, true) => "../".repeat(up),
            (_, false) => format!("{}{rest}{trailing}", "../".repeat(up)),
        }
    }

    /// Path segments of a type's page: Markdown file path, or Starlight route.
    fn route_segments(&self, type_path: &str) -> Vec<String> {
        let mut segments: Vec<String> = type_path.split("::").map(str::to_string).collect();
        match self.format {
            OutputFormat::Plain => {
                if let Some(last) = segments.last_mut() {
                    last.push_str(".md");
                }
            }
            OutputFormat::Starlight => {
                for segment in &mut segments {
                    *segment = slugify(segment);
                }
            }
        }
        segments
    }

    /// Format a type as Markdown, linking `TypeRef`s that resolve to a discovered schema.
    pub fn type_to_markdown(&self, kind: &TypeKind, from: LinkFrom<'_>) -> String {
        match kind {
            TypeKind::Option(inner) => format!("Option<{}>", self.type_to_markdown(inner, from)),
            TypeKind::List(inner) => format!("List<{}>", self.type_to_markdown(inner, from)),
            TypeKind::Map { key, value } => format!(
                "Map<{}, {}>",
                self.type_to_markdown(key, from),
                self.type_to_markdown(value, from)
            ),
            TypeKind::Tuple(types) => {
                let items: Vec<_> = types
                    .iter()
                    .map(|t| self.type_to_markdown(t, from))
                    .collect();
                format!("({})", items.join(", "))
            }
            TypeKind::TypeRef(path) => match self.resolve(path) {
                Some(target) => format!(
                    "[`{}`]({})",
                    self.display_name(&target.type_path),
                    self.href(&target.type_path, from)
                ),
                None => format!("`{}`", type_path_short_name(path)),
            },
            TypeKind::Struct { .. } => "`struct`".to_string(),
            TypeKind::Enum { .. } => "`enum`".to_string(),
            primitive => format!("`{primitive}`"),
        }
    }
}

/// The HTML anchor id of a type in single-page output. Unique per type path.
pub fn type_anchor(type_path: &str) -> String {
    type_path
        .split("::")
        .map(|segment| {
            segment
                .chars()
                .map(|c| match c {
                    c if c.is_ascii_alphanumeric() || c == '_' => c.to_ascii_lowercase(),
                    _ => '-',
                })
                .collect::<String>()
        })
        .collect::<Vec<_>>()
        .join("-")
}

/// Convert a type path to the Markdown file path of its page.
///
/// Example: `my_crate::config::AppConfig` -> `my_crate/config/AppConfig.md`
pub fn type_path_to_md_path(type_path: &str) -> String {
    type_path.replace("::", "/") + ".md"
}

/// Get the short name from a type path.
///
/// Example: `my_crate::config::AppConfig` -> `AppConfig`
pub fn type_path_short_name(type_path: &str) -> &str {
    type_path.rsplit("::").next().unwrap_or(type_path)
}

/// `my_crate::Wrapper<T>` -> `my_crate::Wrapper`
fn strip_generics(type_path: &str) -> &str {
    type_path.split('<').next().unwrap_or(type_path).trim()
}

/// Whether the `::`-separated segments of `suffix` end `path`.
fn is_path_suffix(suffix: &str, path: &str) -> bool {
    path == suffix
        || path
            .strip_suffix(suffix)
            .is_some_and(|prefix| prefix.ends_with("::"))
}

/// Slugify a path segment the way Astro/Starlight derive routes from file names.
fn slugify(segment: &str) -> String {
    segment
        .chars()
        .filter_map(|c| match c {
            c if c.is_alphanumeric() || c == '_' || c == '-' => Some(c.to_ascii_lowercase()),
            ' ' => Some('-'),
            _ => None,
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use ron2::schema::Schema;

    use super::*;

    fn make_schema(type_path: &str) -> DiscoveredSchema {
        DiscoveredSchema {
            path: PathBuf::from("test.schema.ron"),
            type_path: type_path.to_string(),
            schema: Schema::new(TypeKind::Unit),
        }
    }

    fn plain(schemas: &[DiscoveredSchema]) -> LinkResolver<'_> {
        LinkResolver::new(schemas, None, OutputFormat::Plain)
    }

    fn type_ref(path: &str) -> TypeKind {
        TypeKind::TypeRef(path.to_string())
    }

    #[test]
    fn test_link_resolver_primitives() {
        let schemas = vec![make_schema("Config")];
        let resolver = plain(&schemas);
        let from = LinkFrom::Index;

        assert_eq!(resolver.type_to_markdown(&TypeKind::Bool, from), "`bool`");
        assert_eq!(
            resolver.type_to_markdown(&TypeKind::String, from),
            "`String`"
        );
        assert_eq!(resolver.type_to_markdown(&TypeKind::I32, from), "`i32`");
    }

    #[test]
    fn test_link_resolver_compound() {
        let schemas = vec![make_schema("Config")];
        let resolver = plain(&schemas);
        let from = LinkFrom::Index;

        let option_string = TypeKind::Option(Box::new(TypeKind::String));
        assert_eq!(
            resolver.type_to_markdown(&option_string, from),
            "Option<`String`>"
        );
        let list_i32 = TypeKind::List(Box::new(TypeKind::I32));
        assert_eq!(resolver.type_to_markdown(&list_i32, from), "List<`i32`>");
    }

    #[test]
    fn test_link_resolver_typeref_resolves_to_discovered() {
        // Discovered as "Config" (short path), referenced as "my_crate::Config".
        let schemas = vec![make_schema("Config")];
        let resolver = plain(&schemas);
        assert_eq!(
            resolver.type_to_markdown(&type_ref("my_crate::Config"), LinkFrom::Index),
            "[`Config`](./Config.md)"
        );
    }

    #[test]
    fn test_link_resolver_with_base_url() {
        let schemas = vec![make_schema("Config")];
        let resolver = LinkResolver::new(&schemas, Some("/docs/types/"), OutputFormat::Plain);
        assert_eq!(
            resolver.type_to_markdown(&type_ref("my_crate::Config"), LinkFrom::Type("Config")),
            "[`Config`](/docs/types/Config.md)"
        );
    }

    #[test]
    fn links_between_namespaced_pages_are_relative_to_the_current_page() {
        let schemas = vec![
            make_schema("game::GameConfig"),
            make_schema("game::WindowConfig"),
            make_schema("game::render::Shader"),
            make_schema("other::Thing"),
        ];
        let resolver = plain(&schemas);
        let from = LinkFrom::Type("game::GameConfig");
        assert_eq!(
            resolver.href("game::WindowConfig", from),
            "./WindowConfig.md"
        );
        assert_eq!(
            resolver.href("game::render::Shader", from),
            "./render/Shader.md"
        );
        assert_eq!(resolver.href("other::Thing", from), "../other/Thing.md");
        assert_eq!(
            resolver.href("game::GameConfig", LinkFrom::Type("game::render::Shader")),
            "../GameConfig.md"
        );
        assert_eq!(
            resolver.href("game::GameConfig", LinkFrom::Index),
            "./game/GameConfig.md"
        );
    }

    #[test]
    fn starlight_links_use_routes_without_md_extension() {
        let schemas = vec![
            make_schema("game::GameConfig"),
            make_schema("game::WindowConfig"),
        ];
        let resolver = LinkResolver::new(&schemas, None, OutputFormat::Starlight);
        // Starlight serves game/GameConfig.md at .../game/gameconfig/.
        assert_eq!(
            resolver.href("game::WindowConfig", LinkFrom::Type("game::GameConfig")),
            "../windowconfig/"
        );
        assert_eq!(
            resolver.href("game::WindowConfig", LinkFrom::Index),
            "./game/windowconfig/"
        );
        let with_base = LinkResolver::new(&schemas, Some("/types"), OutputFormat::Starlight);
        assert_eq!(
            with_base.href("game::WindowConfig", LinkFrom::Index),
            "/types/game/windowconfig/"
        );
    }

    #[test]
    fn exact_match_wins_over_short_name() {
        let schemas = vec![
            make_schema("crate_a::Config"),
            make_schema("crate_b::Config"),
        ];
        let resolver = plain(&schemas);
        let resolved = |path: &str| resolver.resolve(path).map(|s| s.type_path.as_str());
        assert_eq!(resolved("crate_b::Config"), Some("crate_b::Config"));
        assert_eq!(resolved("crate_a::Config"), Some("crate_a::Config"));
        // Ambiguous or foreign references do not resolve to an arbitrary type.
        assert_eq!(resolved("Config"), None);
        assert_eq!(resolved("crate_c::Config"), None);
        assert_eq!(
            resolver.type_to_markdown(&type_ref("crate_c::Config"), LinkFrom::Index),
            "`Config`"
        );
    }

    #[test]
    fn test_type_path_to_md_path() {
        assert_eq!(
            type_path_to_md_path("my_crate::config::AppConfig"),
            "my_crate/config/AppConfig.md"
        );
    }

    #[test]
    fn test_type_path_short_name() {
        assert_eq!(
            type_path_short_name("my_crate::config::AppConfig"),
            "AppConfig"
        );
        assert_eq!(type_path_short_name("Config"), "Config");
    }

    #[test]
    fn test_display_name_with_and_without_collision() {
        let schemas = vec![
            make_schema("crate_a::Config"),
            make_schema("crate_b::Config"),
            make_schema("crate_b::Server"),
        ];
        let resolver = plain(&schemas);
        assert_eq!(resolver.display_name("crate_a::Config"), "crate_a::Config");
        assert_eq!(resolver.display_name("crate_b::Server"), "Server");
    }

    #[test]
    fn test_type_anchor() {
        assert_eq!(
            type_anchor("my_crate::config::AppConfig"),
            "my_crate-config-appconfig"
        );
        assert_eq!(type_anchor("Config"), "config");
    }

    #[test]
    fn test_same_page_links_use_anchors() {
        let schemas = vec![
            make_schema("crate_a::Config"),
            make_schema("crate_b::Config"),
        ];
        let resolver = plain(&schemas);
        assert_eq!(
            resolver.type_to_markdown(&type_ref("crate_a::Config"), LinkFrom::SamePage),
            "[`crate_a::Config`](#crate_a-config)"
        );
    }
}
