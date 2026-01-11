//! Serialization benchmarks for ron2.
//!
//! Compares different serialization methods:
//! - Value compact serialization via `to_ron_with(Minimal)`
//! - Value pretty serialization via `to_ron()`
//! - AST round-trip serialization via `serialize_document()`

mod common;

use criterion::{BenchmarkId, Criterion, Throughput, criterion_group, criterion_main};
use ron2::ast::FormatConfig;
use ron2::ToRon;

/// Benchmark compact Value serialization using `to_ron_with(Minimal)`.
fn bench_value_compact(c: &mut Criterion) {
    let mut group = c.benchmark_group("serialization/value_compact");

    for (name, input) in common::test_inputs() {
        let value: ron2::Value = ron2::from_str(&input).unwrap();
        let output_size = value.to_ron_with(&FormatConfig::minimal()).unwrap().len();
        group.throughput(Throughput::Bytes(output_size as u64));

        group.bench_with_input(BenchmarkId::from_parameter(name), &value, |b, value| {
            b.iter(|| value.to_ron_with(&FormatConfig::minimal()).unwrap());
        });
    }

    group.finish();
}

/// Benchmark pretty Value serialization using `to_ron()`.
fn bench_value_pretty(c: &mut Criterion) {
    let mut group = c.benchmark_group("serialization/value_pretty");

    for (name, input) in common::test_inputs() {
        let value: ron2::Value = ron2::from_str(&input).unwrap();
        let output_size = value.to_ron().unwrap().len();
        group.throughput(Throughput::Bytes(output_size as u64));

        group.bench_with_input(BenchmarkId::from_parameter(name), &value, |b, value| {
            b.iter(|| value.to_ron().unwrap());
        });
    }

    group.finish();
}

/// Benchmark AST round-trip serialization using `serialize_document()`.
fn bench_ast_serialize(c: &mut Criterion) {
    let mut group = c.benchmark_group("serialization/ast_roundtrip");

    for (name, input) in common::test_inputs() {
        let doc = ron2::ast::parse_document(&input).unwrap();
        group.throughput(Throughput::Bytes(input.len() as u64));

        group.bench_with_input(BenchmarkId::from_parameter(name), &doc, |b, doc| {
            b.iter(|| ron2::ast::serialize_document(doc).unwrap());
        });
    }

    group.finish();
}

/// Compare all serialization methods side by side.
fn bench_serialization_comparison(c: &mut Criterion) {
    let mut group = c.benchmark_group("serialization/comparison");

    // Use medium config for comparison
    let input = common::MEDIUM_CONFIG;
    let value: ron2::Value = ron2::from_str(input).unwrap();
    let doc = ron2::ast::parse_document(input).unwrap();

    // Use input size as throughput baseline
    group.throughput(Throughput::Bytes(input.len() as u64));

    group.bench_function("value_compact", |b| {
        b.iter(|| value.to_ron_with(&FormatConfig::minimal()).unwrap());
    });

    group.bench_function("value_pretty", |b| {
        b.iter(|| value.to_ron().unwrap());
    });

    group.bench_function("ast_roundtrip", |b| {
        b.iter(|| ron2::ast::serialize_document(&doc).unwrap());
    });

    group.finish();
}

criterion_group!(
    benches,
    bench_value_compact,
    bench_value_pretty,
    bench_ast_serialize,
    bench_serialization_comparison,
);

criterion_main!(benches);
