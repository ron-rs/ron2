//! RON example generation from schemas.
//!
//! Examples are skeletons with placeholder values. They are always valid RON
//! syntax; when `TypeRef` expansion stops at the depth limit, a shallow
//! placeholder value with a comment naming the type is used instead.

use ron2::schema::{Field, Schema, TypeKind, Variant, VariantKind};

use crate::link::{LinkResolver, type_path_short_name};

const INDENT: &str = "    ";

/// Generate a skeleton RON example from a schema.
///
/// `max_depth` limits how many nested `TypeRef`s are expanded; `links`
/// resolves them to the discovered schemas.
pub fn generate_example(schema: &Schema, max_depth: usize, links: &LinkResolver<'_>) -> String {
    Example { links, max_depth }.value(&schema.kind, 0, 0)
}

struct Example<'l, 'a> {
    links: &'l LinkResolver<'a>,
    max_depth: usize,
}

impl Example<'_, '_> {
    /// An example value of `kind`. `depth` counts expanded `TypeRef`s and
    /// `indent` is the nesting level of the line the value starts on.
    fn value(&self, kind: &TypeKind, depth: usize, indent: usize) -> String {
        match kind {
            TypeKind::Option(inner) => format!("Some({})", self.value(inner, depth, indent)),
            TypeKind::List(inner) => format!("[{}]", self.value(inner, depth, indent)),
            TypeKind::Array { element, len } => {
                let element = self.value(element, depth, indent);
                format!("[{}]", vec![element; *len].join(", "))
            }
            TypeKind::Map { key, value } => format!(
                "{{{}: {}}}",
                self.value(key, depth, indent),
                self.value(value, depth, indent)
            ),
            TypeKind::Tuple(types) => {
                let items: Vec<_> = types.iter().map(|t| self.value(t, depth, indent)).collect();
                format!("({})", items.join(", "))
            }
            TypeKind::Struct { fields } => self.struct_body("", fields, depth, indent),
            TypeKind::Enum { variants } => match variants.first() {
                Some(variant) => self.variant(variant, depth, indent),
                None => "/* no variants */ ()".to_string(),
            },
            TypeKind::TypeRef(path) => {
                let name = type_path_short_name(path);
                match self.links.resolve(path) {
                    Some(target) if depth < self.max_depth => {
                        self.value(&target.schema.kind, depth + 1, indent)
                    }
                    Some(target) => placeholder(&target.schema.kind, name),
                    None => format!("/* {name} */ ()"),
                }
            }
            primitive => primitive_example(primitive),
        }
    }

    fn variant(&self, variant: &Variant, depth: usize, indent: usize) -> String {
        match &variant.kind {
            VariantKind::Unit => variant.name.clone(),
            VariantKind::Tuple(types) => {
                let items: Vec<_> = types.iter().map(|t| self.value(t, depth, indent)).collect();
                format!("{}({})", variant.name, items.join(", "))
            }
            VariantKind::Struct(fields) => self.struct_body(&variant.name, fields, depth, indent),
        }
    }

    /// `name(field: value, ...)` with the required fields, flattened fields inlined.
    fn struct_body(&self, name: &str, fields: &[Field], depth: usize, indent: usize) -> String {
        let mut entries = Vec::new();
        self.required_fields(fields, depth, indent + 1, &mut Vec::new(), &mut entries);
        if entries.is_empty() {
            return format!("{name}()");
        }
        let inner = INDENT.repeat(indent + 1);
        let body: String = entries
            .iter()
            .map(|(field, value)| format!("{inner}{field}: {value},\n"))
            .collect();
        format!("{name}(\n{body}{})", INDENT.repeat(indent))
    }

    fn required_fields(
        &self,
        fields: &[Field],
        depth: usize,
        indent: usize,
        // `TypeRef`s being flattened, to stop on self-flattening schemas.
        flattening: &mut Vec<String>,
        out: &mut Vec<(String, String)>,
    ) {
        for field in fields {
            if !field.flattened {
                if !field.optional {
                    out.push((field.name.clone(), self.value(&field.ty, depth, indent)));
                }
                continue;
            }
            // A flattened struct contributes its fields; a flattened map contributes none.
            let (kind, type_ref) = match &field.ty {
                TypeKind::TypeRef(path) => match self.links.resolve(path) {
                    Some(target) if !flattening.contains(path) => (&target.schema.kind, Some(path)),
                    _ => continue,
                },
                kind => (kind, None),
            };
            if let TypeKind::Struct { fields } = kind {
                flattening.extend(type_ref.cloned());
                self.required_fields(fields, depth, indent, flattening, out);
                if type_ref.is_some() {
                    flattening.pop();
                }
            }
        }
    }
}

/// A valid value for a type whose expansion stopped at the depth limit.
///
/// Never follows `TypeRef`s, so it terminates for recursive schemas.
fn placeholder(kind: &TypeKind, name: &str) -> String {
    match kind {
        TypeKind::Option(_) => "None".to_string(),
        TypeKind::List(_) => "[]".to_string(),
        TypeKind::Array { element, len } => {
            format!("[{}]", vec![placeholder(element, name); *len].join(", "))
        }
        TypeKind::Map { .. } => "{}".to_string(),
        TypeKind::Enum { variants } => match variants.first() {
            Some(Variant {
                name,
                kind: VariantKind::Unit,
                ..
            }) => name.clone(),
            Some(variant) => format!("{}(/* ... */)", variant.name),
            None => "/* no variants */ ()".to_string(),
        },
        TypeKind::Struct { .. } | TypeKind::Tuple(_) | TypeKind::TypeRef(_) => {
            format!("(/* {name} */)")
        }
        primitive => primitive_example(primitive),
    }
}

fn primitive_example(kind: &TypeKind) -> String {
    match kind {
        TypeKind::Bool => "true",
        TypeKind::String => "\"...\"",
        TypeKind::Char => "'?'",
        TypeKind::F32 | TypeKind::F64 => "0.0",
        TypeKind::Unit => "()",
        _ => "0",
    }
    .to_string()
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use ron2::schema::Field;

    use super::*;
    use crate::{config::OutputFormat, discovery::DiscoveredSchema};

    fn discovered(type_path: &str, kind: TypeKind) -> DiscoveredSchema {
        DiscoveredSchema {
            path: PathBuf::from("test.schema.ron"),
            type_path: type_path.to_string(),
            schema: Schema::new(kind),
        }
    }

    fn example(kind: TypeKind, schemas: &[DiscoveredSchema], max_depth: usize) -> String {
        let links = LinkResolver::new(schemas, None, OutputFormat::Plain);
        let example = generate_example(&Schema::new(kind), max_depth, &links);
        if let Err(err) = ron2::ast::parse_document(&example) {
            panic!("example is not valid RON ({err}):\n{example}");
        }
        example
    }

    fn type_ref(path: &str) -> TypeKind {
        TypeKind::TypeRef(path.to_string())
    }

    #[test]
    fn test_generate_primitive_examples() {
        assert_eq!(example(TypeKind::Bool, &[], 2), "true");
        assert_eq!(example(TypeKind::String, &[], 2), "\"...\"");
        assert_eq!(example(TypeKind::I32, &[], 2), "0");
    }

    #[test]
    fn test_generate_compound_examples() {
        let option_string = TypeKind::Option(Box::new(TypeKind::String));
        assert_eq!(example(option_string, &[], 2), "Some(\"...\")");
        assert_eq!(
            example(TypeKind::List(Box::new(TypeKind::I32)), &[], 2),
            "[0]"
        );
        let array = TypeKind::Array {
            element: Box::new(TypeKind::U8),
            len: 3,
        };
        assert_eq!(example(array, &[], 2), "[0, 0, 0]");
    }

    #[test]
    fn test_generate_struct_example() {
        let struct_kind = TypeKind::Struct {
            fields: vec![
                Field::new("name", TypeKind::String),
                Field::new("age", TypeKind::I32),
                Field::optional("nick", TypeKind::String),
            ],
        };
        assert_eq!(
            example(struct_kind, &[], 2),
            "(\n    name: \"...\",\n    age: 0,\n)"
        );
    }

    #[test]
    fn test_generate_enum_example() {
        let enum_kind = TypeKind::Enum {
            variants: vec![Variant::unit("Low"), Variant::unit("High")],
        };
        assert_eq!(example(enum_kind, &[], 2), "Low");
    }

    #[test]
    fn struct_variant_without_required_fields_uses_parentheses() {
        let enum_kind = TypeKind::Enum {
            variants: vec![Variant::struct_variant(
                "Empty",
                vec![Field::optional("x", TypeKind::I32)],
            )],
        };
        assert_eq!(example(enum_kind, &[], 2), "Empty()");
    }

    #[test]
    fn depth_cutoff_emits_valid_placeholders() {
        let schemas = vec![
            discovered(
                "t::Tree",
                TypeKind::Struct {
                    fields: vec![
                        Field::new("child", type_ref("t::Tree")),
                        Field::new("kind", type_ref("t::Kind")),
                    ],
                },
            ),
            discovered(
                "t::Kind",
                TypeKind::Enum {
                    variants: vec![Variant::tuple("Leaf", vec![TypeKind::I32])],
                },
            ),
        ];
        let out = example(type_ref("t::Tree"), &schemas, 1);
        assert!(out.contains("child: (/* Tree */)"), "{out}");
        assert!(out.contains("kind: Leaf(/* ... */)"), "{out}");
        // Unknown types still produce a value.
        assert_eq!(
            example(type_ref("t::Unknown"), &schemas, 2),
            "/* Unknown */ ()"
        );
    }

    #[test]
    fn flattened_fields_are_inlined() {
        let schemas = vec![
            discovered(
                "t::Outer",
                TypeKind::Struct {
                    fields: vec![
                        Field::new("a", TypeKind::I32),
                        Field::flattened("inner", type_ref("t::Inner")),
                        Field::flattened(
                            "extra",
                            TypeKind::Map {
                                key: Box::new(TypeKind::String),
                                value: Box::new(TypeKind::String),
                            },
                        ),
                    ],
                },
            ),
            discovered(
                "t::Inner",
                TypeKind::Struct {
                    fields: vec![
                        Field::new("b", TypeKind::Bool),
                        Field::flattened("again", type_ref("t::Inner")),
                    ],
                },
            ),
        ];
        assert_eq!(
            example(type_ref("t::Outer"), &schemas, 2),
            "(\n    a: 0,\n    b: true,\n)"
        );
    }

    #[test]
    fn typeref_lookup_matches_link_resolution() {
        // Discovered under a shorter path than the TypeRef uses.
        let schemas = vec![discovered(
            "Config",
            TypeKind::Struct {
                fields: vec![Field::new("port", TypeKind::U16)],
            },
        )];
        assert_eq!(
            example(type_ref("my_crate::Config"), &schemas, 2),
            "(\n    port: 0,\n)"
        );
    }
}
