//! AST parser profiling harness for cargo-instruments.
//!
//! Run with:
//! ```sh
//! cargo instruments -t "Time Profiler" --release -p ron-showcase --bin profile_ast
//! ```

use std::{hint::black_box, time::Instant};

const ITERATIONS: usize = 100_000;

fn main() {
    let input = include_str!("../../data/game.ron");

    // Warmup
    for _ in 0..1000 {
        let doc = ron2::ast::parse_document(black_box(input)).unwrap();
        black_box(doc);
    }

    let start = Instant::now();
    for _ in 0..ITERATIONS {
        let doc = ron2::ast::parse_document(black_box(input)).unwrap();
        black_box(doc);
    }
    let elapsed = start.elapsed();

    eprintln!(
        "Parsed {} iterations in {:.2?} ({:.2?}/iter, {:.0} ops/sec)",
        ITERATIONS,
        elapsed,
        elapsed / ITERATIONS as u32,
        ITERATIONS as f64 / elapsed.as_secs_f64()
    );
}
