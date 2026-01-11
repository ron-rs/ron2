//! Integration tests for LSP completions using the showcase example schemas.
//!
//! These tests verify that the LSP provides correct completions when working
//! with schemas that use fully qualified TypeRef paths.

use std::path::PathBuf;

use tower_lsp::lsp_types::{Position, Url};

// Import from ron-lsp (need to make these pub in lib.rs or use a test helper)
// For now, we'll test through the public API

/// Get the path to the showcase schemas directory.
fn showcase_schemas_dir() -> PathBuf {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    PathBuf::from(manifest_dir)
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("schemas")
}

mod completion_integration {
    use super::*;
    use ron2_lsp::{Document, SchemaResolver};

    /// Helper to create a document with content.
    fn make_doc(content: &str) -> Document {
        Document::new(
            Url::parse("file:///test.ron").unwrap(),
            content.to_string(),
            1,
        )
    }

    /// Helper to create a resolver with showcase schemas.
    fn make_resolver() -> SchemaResolver {
        let resolver = SchemaResolver::new();
        resolver.set_schema_dirs(vec![showcase_schemas_dir()]);
        resolver
    }

    #[test]
    fn test_schema_resolution_with_type_attribute() {
        let content = r#"#![type = "ron_showcase::GameConfig"]

(
    title: "Test Game",
)
"#;
        let doc = make_doc(content);
        let resolver = make_resolver();

        // Should resolve the schema
        let schema = resolver.resolve_schema(&doc);
        assert!(schema.is_some(), "Should resolve GameConfig schema");

        let schema = schema.unwrap();
        assert_eq!(schema.doc, Some("Main game configuration.".to_string()));
    }

    #[test]
    fn test_field_completions_for_struct() {
        let content = r#"#![type = "ron_showcase::GameConfig"]

(
    title: "Test",

)"#;
        let doc = make_doc(content);
        let resolver = make_resolver();

        // Position after "title: \"Test\",\n    " - should suggest remaining fields
        let completions = ron2_lsp::provide_completions(&doc, Position::new(4, 4), &resolver);

        // Should have completions for remaining fields
        let labels: Vec<_> = completions.iter().map(|c| c.label.as_str()).collect();

        // Should suggest version, window, graphics, audio, keybindings, player
        assert!(
            labels.contains(&"version"),
            "Should suggest 'version' field"
        );
        assert!(labels.contains(&"window"), "Should suggest 'window' field");
        assert!(
            labels.contains(&"graphics"),
            "Should suggest 'graphics' field"
        );
        assert!(labels.contains(&"audio"), "Should suggest 'audio' field");
        assert!(
            labels.contains(&"keybindings"),
            "Should suggest 'keybindings' field"
        );
        assert!(labels.contains(&"player"), "Should suggest 'player' field");

        // Should NOT suggest title (already used)
        assert!(
            !labels.contains(&"title"),
            "Should not suggest 'title' (already used)"
        );
    }

    #[test]
    fn test_field_completion_shows_typeref_type() {
        let content = r#"#![type = "ron_showcase::GameConfig"]

(
    title: "Test",

)"#;
        let doc = make_doc(content);
        let resolver = make_resolver();

        let completions = ron2_lsp::provide_completions(&doc, Position::new(4, 4), &resolver);

        // Find the 'window' completion
        let window_completion = completions.iter().find(|c| c.label == "window");
        assert!(
            window_completion.is_some(),
            "Should have 'window' completion"
        );

        let window = window_completion.unwrap();
        // Detail should show the fully qualified TypeRef path
        assert!(
            window
                .detail
                .as_ref()
                .unwrap()
                .contains("ron_showcase::WindowConfig"),
            "Window field should show TypeRef type: {:?}",
            window.detail
        );
    }

    #[test]
    fn test_enum_variant_completions() {
        let content = r#"#![type = "ron_showcase::GraphicsQuality"]

"#;
        let doc = make_doc(content);
        let resolver = make_resolver();

        // At root level for an enum type
        let completions = ron2_lsp::provide_completions(&doc, Position::new(2, 0), &resolver);

        let labels: Vec<_> = completions.iter().map(|c| c.label.as_str()).collect();

        // Should suggest all enum variants
        assert!(labels.contains(&"Low"), "Should suggest 'Low' variant");
        assert!(
            labels.contains(&"Medium"),
            "Should suggest 'Medium' variant"
        );
        assert!(labels.contains(&"High"), "Should suggest 'High' variant");
        assert!(labels.contains(&"Ultra"), "Should suggest 'Ultra' variant");
    }

    #[test]
    fn test_enum_with_struct_variants() {
        let content = r#"#![type = "ron_showcase::ShipType"]

"#;
        let doc = make_doc(content);
        let resolver = make_resolver();

        let completions = ron2_lsp::provide_completions(&doc, Position::new(2, 0), &resolver);

        let labels: Vec<_> = completions.iter().map(|c| c.label.as_str()).collect();

        // Should suggest all variants
        assert!(labels.contains(&"Scout"), "Should suggest 'Scout' variant");
        assert!(
            labels.contains(&"Fighter"),
            "Should suggest 'Fighter' variant"
        );
        assert!(
            labels.contains(&"Cruiser"),
            "Should suggest 'Cruiser' variant"
        );

        // Fighter should show struct fields in detail
        let fighter = completions.iter().find(|c| c.label == "Fighter").unwrap();
        let detail = fighter.detail.as_ref().unwrap();
        assert!(
            detail.contains("weapon_slots") && detail.contains("shield_strength"),
            "Fighter detail should show struct fields: {}",
            detail
        );
    }

    #[test]
    fn test_struct_snippet_completion_at_root() {
        let content = r#"#![type = "ron_showcase::WindowConfig"]

"#;
        let doc = make_doc(content);
        let resolver = make_resolver();

        let completions = ron2_lsp::provide_completions(&doc, Position::new(2, 0), &resolver);

        // Should suggest struct snippet
        let struct_completion = completions.iter().find(|c| c.label == "(...)");
        assert!(
            struct_completion.is_some(),
            "Should suggest struct snippet at root"
        );

        let snippet = struct_completion.unwrap();
        let insert_text = snippet.insert_text.as_ref().unwrap();

        // Should include required fields (width and height are required)
        assert!(
            insert_text.contains("width"),
            "Snippet should include 'width' field"
        );
        assert!(
            insert_text.contains("height"),
            "Snippet should include 'height' field"
        );
    }

    #[test]
    fn test_no_completions_without_schema() {
        let content = r#"(
    some_field: 123,
)"#;
        let doc = make_doc(content);
        let resolver = make_resolver();

        // No type attribute, so no schema
        let completions = ron2_lsp::provide_completions(&doc, Position::new(1, 4), &resolver);
        assert!(
            completions.is_empty(),
            "Should have no completions without schema"
        );
    }

    #[test]
    fn test_no_completions_with_invalid_type() {
        let content = r#"#![type = "nonexistent::Type"]

(
    field:
)"#;
        let doc = make_doc(content);
        let resolver = make_resolver();

        let completions = ron2_lsp::provide_completions(&doc, Position::new(3, 11), &resolver);
        assert!(
            completions.is_empty(),
            "Should have no completions with invalid type"
        );
    }

    #[test]
    fn test_completion_context_after_colon() {
        let content = r#"#![type = "ron_showcase::GameConfig"]

(
    title:
)"#;
        let doc = make_doc(content);

        // After the colon, context should be Value
        let context = doc.context_at_position(3, 11);
        assert_eq!(
            context,
            ron2_lsp::CompletionContext::Value,
            "After colon should be Value context"
        );
    }

    #[test]
    fn test_completion_context_after_comma() {
        let content = r#"#![type = "ron_showcase::GameConfig"]

(
    title: "Test",

)"#;
        let doc = make_doc(content);

        // After comma and newline, should be FieldName context
        let context = doc.context_at_position(4, 4);
        assert_eq!(
            context,
            ron2_lsp::CompletionContext::FieldName,
            "After comma should be FieldName context"
        );
    }

    #[test]
    fn test_completion_context_at_root() {
        let content = r#"#![type = "ron_showcase::GameConfig"]

"#;
        let doc = make_doc(content);

        // At empty root
        let context = doc.context_at_position(2, 0);
        assert_eq!(
            context,
            ron2_lsp::CompletionContext::Root,
            "Empty document should be Root context"
        );
    }

    #[test]
    fn test_field_documentation_in_completion() {
        let content = r#"#![type = "ron_showcase::GameConfig"]

(

)"#;
        let doc = make_doc(content);
        let resolver = make_resolver();

        let completions = ron2_lsp::provide_completions(&doc, Position::new(3, 4), &resolver);

        // Find the 'title' completion
        let title = completions.iter().find(|c| c.label == "title");
        assert!(title.is_some(), "Should have 'title' completion");

        let title = title.unwrap();
        assert!(
            title.documentation.is_some(),
            "Title completion should have documentation"
        );
    }

    #[test]
    fn test_value_context_detected() {
        // The completion system now tracks which field is being edited and
        // provides field-type-specific completions.
        let content = r#"#![type = "ron_showcase::GameConfig"]

(
    title:
)"#;
        let doc = make_doc(content);
        let resolver = make_resolver();

        // After "title:" - value context should be detected
        let context = doc.context_at_position(3, 11);
        assert_eq!(
            context,
            ron2_lsp::CompletionContext::Value,
            "After colon should be Value context"
        );

        // For a String field like 'title', no specific completions are provided
        let completions = ron2_lsp::provide_completions(&doc, Position::new(3, 11), &resolver);
        assert!(
            completions.is_empty(),
            "String field should have no completions (user types literal)"
        );
    }

    #[test]
    fn test_enum_value_completions_for_field() {
        // Test that enum completions work when editing a field that has an enum type
        let content = r#"#![type = "ron_showcase::GameConfig"]

(
    graphics:
)"#;
        let doc = make_doc(content);
        let resolver = make_resolver();

        // After "graphics:" - value context, should suggest enum variants
        let completions = ron2_lsp::provide_completions(&doc, Position::new(3, 14), &resolver);

        let labels: Vec<_> = completions.iter().map(|c| c.label.as_str()).collect();

        // Should suggest all GraphicsQuality variants
        assert!(labels.contains(&"Low"), "Should suggest 'Low' variant");
        assert!(
            labels.contains(&"Medium"),
            "Should suggest 'Medium' variant"
        );
        assert!(labels.contains(&"High"), "Should suggest 'High' variant");
        assert!(labels.contains(&"Ultra"), "Should suggest 'Ultra' variant");
    }

    #[test]
    fn test_map_value_completions_for_field() {
        // Test that map completions work for a Map field
        let content = r#"#![type = "ron_showcase::GameConfig"]

(
    keybindings:
)"#;
        let doc = make_doc(content);
        let resolver = make_resolver();

        // After "keybindings:" - should suggest map snippet
        let completions = ron2_lsp::provide_completions(&doc, Position::new(3, 17), &resolver);

        let map_completion = completions.iter().find(|c| c.label == "{}");
        assert!(
            map_completion.is_some(),
            "Map field should suggest empty map snippet"
        );
    }

    #[test]
    fn test_struct_value_completions_for_typeref_field() {
        // Test that struct completions work for a TypeRef field pointing to a struct
        let content = r#"#![type = "ron_showcase::GameConfig"]

(
    window:
)"#;
        let doc = make_doc(content);
        let resolver = make_resolver();

        // After "window:" - should suggest struct snippet (WindowConfig is a struct)
        let completions = ron2_lsp::provide_completions(&doc, Position::new(3, 12), &resolver);

        let struct_completion = completions.iter().find(|c| c.label == "(...)");
        assert!(
            struct_completion.is_some(),
            "TypeRef to struct should suggest struct snippet"
        );
    }
}
