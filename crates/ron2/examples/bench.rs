use std::time::Instant;

const ITERATIONS: usize = 10000;

const SAMPLE: &str = r#"Config {
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
}"#;

fn main() {
    // Warmup
    for _ in 0..100 {
        let _ = ron2::from_str(SAMPLE);
        let doc = ron2::ast::parse_document(SAMPLE).unwrap();
        let _ = ron2::ast::to_value(&doc);
    }

    // Benchmark direct path
    let start = Instant::now();
    for _ in 0..ITERATIONS {
        let _ = ron2::from_str(SAMPLE).unwrap();
    }
    let direct_time = start.elapsed();

    // Benchmark AST path
    let start = Instant::now();
    for _ in 0..ITERATIONS {
        let doc = ron2::ast::parse_document(SAMPLE).unwrap();
        let _ = ron2::ast::to_value(&doc).unwrap();
    }
    let ast_time = start.elapsed();

    println!(
        "Direct path: {:?} ({:.2} µs/iter)",
        direct_time,
        direct_time.as_micros() as f64 / ITERATIONS as f64
    );
    println!(
        "AST path:    {:?} ({:.2} µs/iter)",
        ast_time,
        ast_time.as_micros() as f64 / ITERATIONS as f64
    );
    println!(
        "Ratio:       {:.2}x",
        ast_time.as_nanos() as f64 / direct_time.as_nanos() as f64
    );
}
