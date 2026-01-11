//! Cross-reference link resolution for TypeRef.

use std::collections::HashMap;

use ron_schema::TypeKind;

use crate::discovery::DiscoveredSchema;

/// A resolver for TypeRef paths to markdown file paths.
pub struct LinkResolver<'a> {
    /// Map from TypeRef path to the discovered schema's type_path
    type_ref_to_path: HashMap<&'a str, &'a str>,
    /// Map from short name to type_path for fallback matching
    short_name_to_path: HashMap<&'a str, &'a str>,
    /// Map from short name to count of types with that name (for collision detection)
    short_name_counts: HashMap<&'a str, usize>,
    /// Base URL for links
    base_url: Option<&'a str>,
}

impl<'a> LinkResolver<'a> {
    /// Create a new link resolver from discovered schemas.
    pub fn new(schemas: &'a [DiscoveredSchema], base_url: Option<&'a str>) -> Self {
        let mut type_ref_to_path = HashMap::new();
        let mut short_name_to_path = HashMap::new();
        let mut short_name_counts: HashMap<&str, usize> = HashMap::new();

        for schema in schemas {
            // Map the full type path to itself
            type_ref_to_path.insert(schema.type_path.as_str(), schema.type_path.as_str());

            // Also map short name for fallback resolution and track collision counts
            let short_name = type_path_short_name(&schema.type_path);
            short_name_to_path.insert(short_name, schema.type_path.as_str());
            *short_name_counts.entry(short_name).or_insert(0) += 1;

            // Also check if any TypeRefs in this schema should map to discovered schemas
            // This handles cases where TypeRef uses full path but file is at short path
            collect_type_refs(&schema.schema.kind, &mut |type_ref| {
                // Try to find a matching schema by short name
                let ref_short_name = type_path_short_name(type_ref);
                for s in schemas {
                    if type_path_short_name(&s.type_path) == ref_short_name {
                        type_ref_to_path.insert(type_ref, s.type_path.as_str());
                        break;
                    }
                }
            });
        }

        Self {
            type_ref_to_path,
            short_name_to_path,
            short_name_counts,
            base_url,
        }
    }

    /// Check if a short name has collisions (multiple types with the same short name).
    pub fn has_collision(&self, short_name: &str) -> bool {
        self.short_name_counts.get(short_name).is_some_and(|c| *c > 1)
    }

    /// Get the display name for a type path.
    /// Returns the short name if unambiguous, otherwise the qualified path.
    pub fn display_name(&self, type_path: &str) -> String {
        let short_name = type_path_short_name(type_path);
        if self.has_collision(short_name) {
            type_path.to_string()
        } else {
            short_name.to_string()
        }
    }

    /// Convert a type path to an anchor ID for single-page mode.
    /// Uses the full path to guarantee uniqueness.
    pub fn type_path_to_anchor(type_path: &str) -> String {
        type_path.replace("::", "-").to_lowercase()
    }

    /// Resolve a TypeRef path to a markdown link path.
    fn resolve_type_ref(&self, type_ref: &str) -> String {
        // First try to find in our direct map
        if let Some(resolved_path) = self.type_ref_to_path.get(type_ref) {
            return type_path_to_md_path(resolved_path);
        }

        // Try short name fallback
        let short_name = type_path_short_name(type_ref);
        if let Some(resolved_path) = self.short_name_to_path.get(short_name) {
            return type_path_to_md_path(resolved_path);
        }

        // Fall back to converting the TypeRef directly
        type_path_to_md_path(type_ref)
    }

    /// Format a TypeKind as a markdown string with links for TypeRefs.
    pub fn type_to_markdown(&self, type_kind: &TypeKind) -> String {
        match type_kind {
            // Primitives - just inline code
            TypeKind::Bool => "`bool`".to_string(),
            TypeKind::I8 => "`i8`".to_string(),
            TypeKind::I16 => "`i16`".to_string(),
            TypeKind::I32 => "`i32`".to_string(),
            TypeKind::I64 => "`i64`".to_string(),
            TypeKind::I128 => "`i128`".to_string(),
            TypeKind::U8 => "`u8`".to_string(),
            TypeKind::U16 => "`u16`".to_string(),
            TypeKind::U32 => "`u32`".to_string(),
            TypeKind::U64 => "`u64`".to_string(),
            TypeKind::U128 => "`u128`".to_string(),
            TypeKind::F32 => "`f32`".to_string(),
            TypeKind::F64 => "`f64`".to_string(),
            TypeKind::Char => "`char`".to_string(),
            TypeKind::String => "`String`".to_string(),
            TypeKind::Unit => "`()`".to_string(),

            // Compound types - format with inner type links
            TypeKind::Option(inner) => {
                format!("Option<{}>", self.type_to_markdown(inner))
            }
            TypeKind::List(inner) => {
                format!("List<{}>", self.type_to_markdown(inner))
            }
            TypeKind::Map { key, value } => {
                format!(
                    "Map<{}, {}>",
                    self.type_to_markdown(key),
                    self.type_to_markdown(value)
                )
            }
            TypeKind::Tuple(types) => {
                let items: Vec<_> = types.iter().map(|t| self.type_to_markdown(t)).collect();
                format!("({})", items.join(", "))
            }

            // TypeRef - create a link
            TypeKind::TypeRef(path) => {
                let display_name = type_path_short_name(path);
                let md_path = self.resolve_type_ref(path);

                if let Some(base) = self.base_url {
                    format!(
                        "[`{}`]({}/{})",
                        display_name,
                        base.trim_end_matches('/'),
                        md_path
                    )
                } else {
                    format!("[`{}`](./{})", display_name, md_path)
                }
            }

            // Struct and Enum inline - just show the kind
            TypeKind::Struct { .. } => "`struct`".to_string(),
            TypeKind::Enum { .. } => "`enum`".to_string(),
        }
    }

    /// Format a TypeKind as markdown with anchor links for single-page mode.
    pub fn type_to_markdown_anchor(&self, type_kind: &TypeKind) -> String {
        match type_kind {
            // Primitives - just inline code
            TypeKind::Bool => "`bool`".to_string(),
            TypeKind::I8 => "`i8`".to_string(),
            TypeKind::I16 => "`i16`".to_string(),
            TypeKind::I32 => "`i32`".to_string(),
            TypeKind::I64 => "`i64`".to_string(),
            TypeKind::I128 => "`i128`".to_string(),
            TypeKind::U8 => "`u8`".to_string(),
            TypeKind::U16 => "`u16`".to_string(),
            TypeKind::U32 => "`u32`".to_string(),
            TypeKind::U64 => "`u64`".to_string(),
            TypeKind::U128 => "`u128`".to_string(),
            TypeKind::F32 => "`f32`".to_string(),
            TypeKind::F64 => "`f64`".to_string(),
            TypeKind::Char => "`char`".to_string(),
            TypeKind::String => "`String`".to_string(),
            TypeKind::Unit => "`()`".to_string(),

            // Compound types - format with inner type links
            TypeKind::Option(inner) => {
                format!("Option<{}>", self.type_to_markdown_anchor(inner))
            }
            TypeKind::List(inner) => {
                format!("List<{}>", self.type_to_markdown_anchor(inner))
            }
            TypeKind::Map { key, value } => {
                format!(
                    "Map<{}, {}>",
                    self.type_to_markdown_anchor(key),
                    self.type_to_markdown_anchor(value)
                )
            }
            TypeKind::Tuple(types) => {
                let items: Vec<_> = types
                    .iter()
                    .map(|t| self.type_to_markdown_anchor(t))
                    .collect();
                format!("({})", items.join(", "))
            }

            // TypeRef - create an anchor link
            TypeKind::TypeRef(path) => {
                let resolved = self.resolve_type_ref_path(path);
                let display = self.display_name(&resolved);
                let anchor = Self::type_path_to_anchor(&resolved);
                format!("[`{}`](#{})", display, anchor)
            }

            // Struct and Enum inline - just show the kind
            TypeKind::Struct { .. } => "`struct`".to_string(),
            TypeKind::Enum { .. } => "`enum`".to_string(),
        }
    }

    /// Resolve a TypeRef path to the actual type_path (for use in anchors).
    fn resolve_type_ref_path(&self, type_ref: &str) -> String {
        // First try to find in our direct map
        if let Some(resolved_path) = self.type_ref_to_path.get(type_ref) {
            return (*resolved_path).to_string();
        }

        // Try short name fallback
        let short_name = type_path_short_name(type_ref);
        if let Some(resolved_path) = self.short_name_to_path.get(short_name) {
            return (*resolved_path).to_string();
        }

        // Fall back to the original path
        type_ref.to_string()
    }
}

/// Collect all TypeRef paths from a TypeKind.
fn collect_type_refs<'a>(kind: &'a TypeKind, collector: &mut impl FnMut(&'a str)) {
    match kind {
        TypeKind::TypeRef(path) => collector(path),
        TypeKind::Option(inner) | TypeKind::List(inner) => collect_type_refs(inner, collector),
        TypeKind::Map { key, value } => {
            collect_type_refs(key, collector);
            collect_type_refs(value, collector);
        }
        TypeKind::Tuple(types) => {
            for t in types {
                collect_type_refs(t, collector);
            }
        }
        TypeKind::Struct { fields } => {
            for f in fields {
                collect_type_refs(&f.ty, collector);
            }
        }
        TypeKind::Enum { variants } => {
            for v in variants {
                match &v.kind {
                    ron_schema::VariantKind::Tuple(types) => {
                        for t in types {
                            collect_type_refs(t, collector);
                        }
                    }
                    ron_schema::VariantKind::Struct(fields) => {
                        for f in fields {
                            collect_type_refs(&f.ty, collector);
                        }
                    }
                    ron_schema::VariantKind::Unit => {}
                }
            }
        }
        _ => {}
    }
}

/// Convert a type path to a markdown file path.
///
/// Example: `my_crate::config::AppConfig` -> `my_crate/config/AppConfig.md`
pub fn type_path_to_md_path(type_path: &str) -> String {
    type_path.replace("::", "/") + ".md"
}

/// Get the short name from a type path.
///
/// Example: `my_crate::config::AppConfig` -> `AppConfig`
pub fn type_path_short_name(type_path: &str) -> &str {
    type_path.split("::").last().unwrap_or(type_path)
}

#[cfg(test)]
mod tests {
    use super::*;
    use ron_schema::Schema;
    use std::path::PathBuf;

    fn make_schema(type_path: &str) -> DiscoveredSchema {
        DiscoveredSchema {
            path: PathBuf::from("test.schema.ron"),
            type_path: type_path.to_string(),
            schema: Schema::new(TypeKind::Unit),
        }
    }

    #[test]
    fn test_link_resolver_primitives() {
        let schemas = vec![make_schema("Config")];
        let resolver = LinkResolver::new(&schemas, None);

        assert_eq!(resolver.type_to_markdown(&TypeKind::Bool), "`bool`");
        assert_eq!(resolver.type_to_markdown(&TypeKind::String), "`String`");
        assert_eq!(resolver.type_to_markdown(&TypeKind::I32), "`i32`");
    }

    #[test]
    fn test_link_resolver_compound() {
        let schemas = vec![make_schema("Config")];
        let resolver = LinkResolver::new(&schemas, None);

        let option_string = TypeKind::Option(Box::new(TypeKind::String));
        assert_eq!(resolver.type_to_markdown(&option_string), "Option<`String`>");

        let list_i32 = TypeKind::List(Box::new(TypeKind::I32));
        assert_eq!(resolver.type_to_markdown(&list_i32), "List<`i32`>");
    }

    #[test]
    fn test_link_resolver_typeref_resolves_to_discovered() {
        // Schema is discovered as "Config" (short path)
        // but TypeRef uses "my_crate::Config" (full path)
        let schemas = vec![make_schema("Config")];
        let resolver = LinkResolver::new(&schemas, None);

        let type_ref = TypeKind::TypeRef("my_crate::Config".to_string());
        // Should resolve to Config.md, not my_crate/Config.md
        assert_eq!(
            resolver.type_to_markdown(&type_ref),
            "[`Config`](./Config.md)"
        );
    }

    #[test]
    fn test_link_resolver_with_base_url() {
        let schemas = vec![make_schema("Config")];
        let resolver = LinkResolver::new(&schemas, Some("/docs/types"));

        let type_ref = TypeKind::TypeRef("my_crate::Config".to_string());
        assert_eq!(
            resolver.type_to_markdown(&type_ref),
            "[`Config`](/docs/types/Config.md)"
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
    fn test_collision_detection_no_collision() {
        let schemas = vec![
            make_schema("crate_a::Config"),
            make_schema("crate_b::Server"),
        ];
        let resolver = LinkResolver::new(&schemas, None);

        assert!(!resolver.has_collision("Config"));
        assert!(!resolver.has_collision("Server"));
        assert_eq!(resolver.display_name("crate_a::Config"), "Config");
        assert_eq!(resolver.display_name("crate_b::Server"), "Server");
    }

    #[test]
    fn test_collision_detection_with_collision() {
        let schemas = vec![
            make_schema("crate_a::Config"),
            make_schema("crate_b::Config"),
        ];
        let resolver = LinkResolver::new(&schemas, None);

        assert!(resolver.has_collision("Config"));
        // When collision exists, display_name returns full path
        assert_eq!(resolver.display_name("crate_a::Config"), "crate_a::Config");
        assert_eq!(resolver.display_name("crate_b::Config"), "crate_b::Config");
    }

    #[test]
    fn test_type_path_to_anchor() {
        assert_eq!(
            LinkResolver::type_path_to_anchor("my_crate::config::AppConfig"),
            "my_crate-config-appconfig"
        );
        assert_eq!(LinkResolver::type_path_to_anchor("Config"), "config");
    }

    #[test]
    fn test_type_to_markdown_anchor() {
        let schemas = vec![make_schema("Config")];
        let resolver = LinkResolver::new(&schemas, None);

        let type_ref = TypeKind::TypeRef("Config".to_string());
        assert_eq!(
            resolver.type_to_markdown_anchor(&type_ref),
            "[`Config`](#config)"
        );
    }

    #[test]
    fn test_type_to_markdown_anchor_with_collision() {
        let schemas = vec![
            make_schema("crate_a::Config"),
            make_schema("crate_b::Config"),
        ];
        let resolver = LinkResolver::new(&schemas, None);

        let type_ref = TypeKind::TypeRef("crate_a::Config".to_string());
        assert_eq!(
            resolver.type_to_markdown_anchor(&type_ref),
            "[`crate_a::Config`](#crate_a-config)"
        );
    }
}
