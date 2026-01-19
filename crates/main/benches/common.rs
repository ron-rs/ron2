//! Shared test data for benchmarks.

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

/// Config with comments for testing comment preservation.
pub fn with_comments() -> String {
    r#"// Application configuration
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
)"#
    .to_string()
}

/// Similar config without comments (for comparison).
pub fn without_comments() -> String {
    r#"Config(
    name: "test",
    port: 8080,
    database: (
        host: "localhost",
        port: 5432,
    ),
)"#
    .to_string()
}
