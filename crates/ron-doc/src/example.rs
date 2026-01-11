//! RON example generation from schemas.

use std::collections::HashMap;

use ron_schema::{Schema, TypeKind, Variant, VariantKind};

use crate::discovery::DiscoveredSchema;

/// Generate a skeleton RON example from a schema.
pub fn generate_example(
    schema: &Schema,
    max_depth: usize,
    schemas: &HashMap<&str, &Schema>,
) -> String {
    generate_type_example(&schema.kind, max_depth, schemas, 0, 0)
}

fn generate_type_example(
    kind: &TypeKind,
    max_depth: usize,
    schemas: &HashMap<&str, &Schema>,
    current_depth: usize,
    indent_level: usize,
) -> String {
    let indent = "    ".repeat(indent_level);
    let inner_indent = "    ".repeat(indent_level + 1);

    match kind {
        // Primitives with placeholder values
        TypeKind::Bool => "true".to_string(),
        TypeKind::String => "\"...\"".to_string(),
        TypeKind::Char => "'?'".to_string(),
        TypeKind::I8 | TypeKind::I16 | TypeKind::I32 | TypeKind::I64 | TypeKind::I128 => {
            "0".to_string()
        }
        TypeKind::U8 | TypeKind::U16 | TypeKind::U32 | TypeKind::U64 | TypeKind::U128 => {
            "0".to_string()
        }
        TypeKind::F32 | TypeKind::F64 => "0.0".to_string(),
        TypeKind::Unit => "()".to_string(),

        // Compound types
        TypeKind::Option(inner) => {
            let inner_example =
                generate_type_example(inner, max_depth, schemas, current_depth, indent_level);
            format!("Some({})", inner_example)
        }
        TypeKind::List(inner) => {
            let inner_example =
                generate_type_example(inner, max_depth, schemas, current_depth, indent_level);
            format!("[{}]", inner_example)
        }
        TypeKind::Map { key, value } => {
            let key_example =
                generate_type_example(key, max_depth, schemas, current_depth, indent_level);
            let value_example =
                generate_type_example(value, max_depth, schemas, current_depth, indent_level);
            format!("{{ {}: {} }}", key_example, value_example)
        }
        TypeKind::Tuple(types) => {
            let items: Vec<_> = types
                .iter()
                .map(|t| generate_type_example(t, max_depth, schemas, current_depth, indent_level))
                .collect();
            format!("({})", items.join(", "))
        }

        // Struct: show all required fields
        TypeKind::Struct { fields } => {
            let required_fields: Vec<_> = fields.iter().filter(|f| !f.optional).collect();

            if required_fields.is_empty() {
                return "()".to_string();
            }

            let field_examples: Vec<_> = required_fields
                .iter()
                .map(|f| {
                    let value = generate_type_example(
                        &f.ty,
                        max_depth,
                        schemas,
                        current_depth + 1,
                        indent_level + 1,
                    );
                    format!("{}{}: {}", inner_indent, f.name, value)
                })
                .collect();

            format!("(\n{},\n{})", field_examples.join(",\n"), indent)
        }

        // Enum: show first variant
        TypeKind::Enum { variants } => {
            if let Some(variant) = variants.first() {
                generate_variant_example(variant, max_depth, schemas, current_depth, indent_level)
            } else {
                "/* no variants */".to_string()
            }
        }

        // TypeRef: resolve or use placeholder
        TypeKind::TypeRef(path) => {
            if current_depth < max_depth {
                if let Some(resolved) = schemas.get(path.as_str()) {
                    return generate_type_example(
                        &resolved.kind,
                        max_depth,
                        schemas,
                        current_depth + 1,
                        indent_level,
                    );
                }
            }
            // Use short name as placeholder
            let short_name = path.split("::").last().unwrap_or(path);
            format!("/* {} */", short_name)
        }
    }
}

fn generate_variant_example(
    variant: &Variant,
    max_depth: usize,
    schemas: &HashMap<&str, &Schema>,
    current_depth: usize,
    indent_level: usize,
) -> String {
    match &variant.kind {
        VariantKind::Unit => variant.name.clone(),
        VariantKind::Tuple(types) => {
            let items: Vec<_> = types
                .iter()
                .map(|t| generate_type_example(t, max_depth, schemas, current_depth, indent_level))
                .collect();
            format!("{}({})", variant.name, items.join(", "))
        }
        VariantKind::Struct(fields) => {
            let inner_indent = "    ".repeat(indent_level + 1);
            let field_examples: Vec<_> = fields
                .iter()
                .filter(|f| !f.optional)
                .map(|f| {
                    let value = generate_type_example(
                        &f.ty,
                        max_depth,
                        schemas,
                        current_depth + 1,
                        indent_level + 1,
                    );
                    format!("{}{}: {}", inner_indent, f.name, value)
                })
                .collect();

            if field_examples.is_empty() {
                format!("{} {{}}", variant.name)
            } else {
                let indent = "    ".repeat(indent_level);
                format!(
                    "{}(\n{},\n{})",
                    variant.name,
                    field_examples.join(",\n"),
                    indent
                )
            }
        }
    }
}

/// Build a lookup map from type path to schema.
pub fn build_schema_map(schemas: &[DiscoveredSchema]) -> HashMap<&str, &Schema> {
    schemas
        .iter()
        .map(|s| (s.type_path.as_str(), &s.schema))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use ron_schema::Field;

    #[test]
    fn test_generate_primitive_examples() {
        let schemas = HashMap::new();
        assert_eq!(
            generate_type_example(&TypeKind::Bool, 2, &schemas, 0, 0),
            "true"
        );
        assert_eq!(
            generate_type_example(&TypeKind::String, 2, &schemas, 0, 0),
            "\"...\""
        );
        assert_eq!(
            generate_type_example(&TypeKind::I32, 2, &schemas, 0, 0),
            "0"
        );
    }

    #[test]
    fn test_generate_compound_examples() {
        let schemas = HashMap::new();

        let option_string = TypeKind::Option(Box::new(TypeKind::String));
        assert_eq!(
            generate_type_example(&option_string, 2, &schemas, 0, 0),
            "Some(\"...\")"
        );

        let list_i32 = TypeKind::List(Box::new(TypeKind::I32));
        assert_eq!(
            generate_type_example(&list_i32, 2, &schemas, 0, 0),
            "[0]"
        );
    }

    #[test]
    fn test_generate_struct_example() {
        let schemas = HashMap::new();

        let struct_kind = TypeKind::Struct {
            fields: vec![
                Field::new("name", TypeKind::String),
                Field::new("age", TypeKind::I32),
            ],
        };

        let example = generate_type_example(&struct_kind, 2, &schemas, 0, 0);
        assert!(example.contains("name: \"...\""));
        assert!(example.contains("age: 0"));
    }

    #[test]
    fn test_generate_enum_example() {
        let schemas = HashMap::new();

        let enum_kind = TypeKind::Enum {
            variants: vec![
                Variant::unit("Low"),
                Variant::unit("Medium"),
                Variant::unit("High"),
            ],
        };

        let example = generate_type_example(&enum_kind, 2, &schemas, 0, 0);
        assert_eq!(example, "Low");
    }
}
