//! Parsing benchmarks for ron2.
//!
//! Compares different parsing methods:
//! - AST parsing via `parse_document()`
//! - AST parsing followed by Value conversion
//!
//! Note: `from_str()` internally uses `parse_document()` + `to_value()`,
//! so there is no separate "direct value" parser to benchmark.

mod common;

use criterion::{BenchmarkId, Criterion, Throughput, criterion_group, criterion_main};

/// Benchmark AST parsing using `parse_document()`.
fn bench_ast_parsing(c: &mut Criterion) {
    let mut group = c.benchmark_group("parsing/ast");

    for (name, input) in common::test_inputs() {
        group.throughput(Throughput::Bytes(input.len() as u64));

        group.bench_with_input(BenchmarkId::from_parameter(name), &input, |b, input| {
            b.iter(|| ron2::ast::parse_document(input).unwrap());
        });
    }

    group.finish();
}

/// Benchmark AST parsing followed by Value conversion.
fn bench_ast_to_value(c: &mut Criterion) {
    let mut group = c.benchmark_group("parsing/ast_to_value");

    for (name, input) in common::test_inputs() {
        group.throughput(Throughput::Bytes(input.len() as u64));

        group.bench_with_input(BenchmarkId::from_parameter(name), &input, |b, input| {
            b.iter(|| {
                let doc = ron2::ast::parse_document(input).unwrap();
                ron2::ast::to_value(&doc).unwrap().unwrap()
            });
        });
    }

    group.finish();
}

/// Compare all parsing methods side by side.
fn bench_parsing_comparison(c: &mut Criterion) {
    let mut group = c.benchmark_group("parsing/comparison");

    // Use medium config for comparison
    let input = common::MEDIUM_CONFIG;
    group.throughput(Throughput::Bytes(input.len() as u64));

    group.bench_function("ast_only", |b| {
        b.iter(|| ron2::ast::parse_document(input).unwrap());
    });

    group.bench_function("ast_then_value", |b| {
        b.iter(|| {
            let doc = ron2::ast::parse_document(input).unwrap();
            ron2::ast::to_value(&doc).unwrap().unwrap()
        });
    });

    group.finish();
}

criterion_group!(
    benches,
    bench_ast_parsing,
    bench_ast_to_value,
    bench_parsing_comparison,
);

criterion_main!(benches);
