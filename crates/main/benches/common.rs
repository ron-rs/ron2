//! Shared test data for benchmarks.

/// Medium config with nested structures (~500 bytes).
pub const MEDIUM_CONFIG: &str = r#"Config(
    name: "test application",
    version: (1, 2, 3),
    settings: {
        "debug": true,
        "timeout": 30,
        "nested": {
            "a": 1,
            "b": 2,
        },
    },
    tags: ["web", "api", "production"],
    metadata: Some({
        "author": "test",
        "license": "MIT",
    }),
)"#;

/// Config with comments for testing comment preservation.
pub const WITH_COMMENTS: &str = r#"// Application configuration
Config(
    // Server settings
    name: "test", // inline comment
    /* block comment */
    port: 8080,
    // Database configuration
    database: (
        host: "localhost",
        port: 5432, // PostgreSQL default
    ),
)"#;

/// Generate a large config (~10KB) with many items.
pub fn large_config() -> String {
    let mut s = String::with_capacity(12000);
    s.push_str("Config(\n");
    s.push_str("    name: \"large configuration\",\n");
    s.push_str("    version: (1, 0, 0),\n");

    // Large array
    s.push_str("    items: [\n");
    for i in 0..100 {
        s.push_str(&format!(
            "        Item(id: {i}, name: \"item_{i}\", value: {}.{}, enabled: {}),\n",
            i * 10,
            i % 10,
            i % 2 == 0
        ));
    }
    s.push_str("    ],\n");

    // Large map
    s.push_str("    settings: {\n");
    for i in 0..50 {
        s.push_str(&format!(
            "        \"setting_{i}\": {{ \"value\": {i}, \"enabled\": {} }},\n",
            i % 3 != 0
        ));
    }
    s.push_str("    },\n");

    // Nested structure
    s.push_str("    nested: (\n");
    s.push_str("        level1: (\n");
    s.push_str("            level2: (\n");
    s.push_str("                level3: (\n");
    s.push_str("                    data: [1, 2, 3, 4, 5],\n");
    s.push_str("                    name: \"deeply nested\",\n");
    s.push_str("                ),\n");
    s.push_str("            ),\n");
    s.push_str("        ),\n");
    s.push_str("    ),\n");

    s.push_str(")\n");
    s
}

/// Returns all test inputs as (name, data) pairs.
pub fn test_inputs() -> Vec<(&'static str, String)> {
    vec![
        ("medium", MEDIUM_CONFIG.to_string()),
        ("with_comments", WITH_COMMENTS.to_string()),
        ("large", large_config()),
    ]
}
