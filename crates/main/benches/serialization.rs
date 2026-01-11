//! Serialization benchmarks for ron2.
//!
//! Compares different serialization methods:
//! - Value compact serialization via `to_string()`
//! - Value pretty serialization via `to_string_pretty()`
//! - AST round-trip serialization via `serialize_document()`

mod common;

use criterion::{BenchmarkId, Criterion, Throughput, criterion_group, criterion_main};
use ron2::PrettyConfig;

/// Benchmark compact Value serialization using `to_string()`.
fn bench_value_compact(c: &mut Criterion) {
    let mut group = c.benchmark_group("serialization/value_compact");

    for (name, input) in common::test_inputs() {
        let value = ron2::from_str(&input).unwrap();
        let output_size = ron2::to_string(&value).unwrap().len();
        group.throughput(Throughput::Bytes(output_size as u64));

        group.bench_with_input(BenchmarkId::from_parameter(name), &value, |b, value| {
            b.iter(|| ron2::to_string(value).unwrap());
        });
    }

    group.finish();
}

/// Benchmark pretty Value serialization using `to_string_pretty()`.
fn bench_value_pretty(c: &mut Criterion) {
    let mut group = c.benchmark_group("serialization/value_pretty");
    let config = PrettyConfig::new();

    for (name, input) in common::test_inputs() {
        let value = ron2::from_str(&input).unwrap();
        let output_size = ron2::to_string_pretty(&value, config.clone())
            .unwrap()
            .len();
        group.throughput(Throughput::Bytes(output_size as u64));

        group.bench_with_input(BenchmarkId::from_parameter(name), &value, |b, value| {
            b.iter(|| ron2::to_string_pretty(value, config.clone()).unwrap());
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
    let value = ron2::from_str(input).unwrap();
    let doc = ron2::ast::parse_document(input).unwrap();
    let config = PrettyConfig::new();

    // Use input size as throughput baseline
    group.throughput(Throughput::Bytes(input.len() as u64));

    group.bench_function("value_compact", |b| {
        b.iter(|| ron2::to_string(&value).unwrap());
    });

    group.bench_function("value_pretty", |b| {
        b.iter(|| ron2::to_string_pretty(&value, config.clone()).unwrap());
    });

    group.bench_function("ast_roundtrip", |b| {
        b.iter(|| ron2::ast::serialize_document(&doc).unwrap());
    });

    group.finish();
}

/// Benchmark serialization to a pre-allocated buffer.
fn bench_to_writer(c: &mut Criterion) {
    let mut group = c.benchmark_group("serialization/to_writer");

    let input = common::MEDIUM_CONFIG;
    let value = ron2::from_str(input).unwrap();
    let expected_size = ron2::to_string(&value).unwrap().len();

    group.throughput(Throughput::Bytes(expected_size as u64));

    group.bench_function("to_string", |b| {
        b.iter(|| ron2::to_string(&value).unwrap());
    });

    group.bench_function("to_writer_preallocated", |b| {
        let mut buffer = String::with_capacity(expected_size * 2);
        b.iter(|| {
            buffer.clear();
            ron2::to_writer(&mut buffer, &value).unwrap();
            buffer.len()
        });
    });

    group.finish();
}

criterion_group!(
    benches,
    bench_value_compact,
    bench_value_pretty,
    bench_ast_serialize,
    bench_serialization_comparison,
    bench_to_writer,
);

criterion_main!(benches);
