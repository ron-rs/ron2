//! Round-trip benchmarks for ron2.
//!
//! Benchmarks complete parse → serialize cycles for different paths:
//! - Value path: from_str → to_ron
//! - AST path: parse_document → serialize_document
//! - AST format path: parse_document → format_document

mod common;

use criterion::{BenchmarkId, Criterion, Throughput, criterion_group, criterion_main};
use ron2::{FormatConfig, ToRon};

/// Benchmark Value round-trip (compact output).
fn bench_value_roundtrip_compact(c: &mut Criterion) {
    let mut group = c.benchmark_group("roundtrip/value_compact");

    for (name, input) in common::test_inputs() {
        group.throughput(Throughput::Bytes(input.len() as u64));

        group.bench_with_input(BenchmarkId::from_parameter(name), &input, |b, input| {
            b.iter(|| {
                let value = input.parse::<ron2::Value>().unwrap();
                value.to_ron_with(&FormatConfig::minimal()).unwrap()
            });
        });
    }

    group.finish();
}

/// Benchmark Value round-trip (pretty output).
fn bench_value_roundtrip_pretty(c: &mut Criterion) {
    let mut group = c.benchmark_group("roundtrip/value_pretty");

    for (name, input) in common::test_inputs() {
        group.throughput(Throughput::Bytes(input.len() as u64));

        group.bench_with_input(BenchmarkId::from_parameter(name), &input, |b, input| {
            b.iter(|| {
                let value = input.parse::<ron2::Value>().unwrap();
                value.to_ron().unwrap()
            });
        });
    }

    group.finish();
}

/// Benchmark AST round-trip (perfect preservation).
fn bench_ast_roundtrip(c: &mut Criterion) {
    let mut group = c.benchmark_group("roundtrip/ast_serialize");

    for (name, input) in common::test_inputs() {
        group.throughput(Throughput::Bytes(input.len() as u64));

        group.bench_with_input(BenchmarkId::from_parameter(name), &input, |b, input| {
            b.iter(|| {
                let doc = ron2::ast::parse_document(input).unwrap();
                ron2::ast::serialize_document(&doc).unwrap()
            });
        });
    }

    group.finish();
}

/// Benchmark AST round-trip with formatting.
fn bench_ast_format_roundtrip(c: &mut Criterion) {
    let mut group = c.benchmark_group("roundtrip/ast_format");
    let config = FormatConfig::default();

    for (name, input) in common::test_inputs() {
        group.throughput(Throughput::Bytes(input.len() as u64));

        group.bench_with_input(BenchmarkId::from_parameter(name), &input, |b, input| {
            b.iter(|| {
                let doc = ron2::ast::parse_document(input).unwrap();
                ron2::ast::format_document(&doc, &config)
            });
        });
    }

    group.finish();
}

/// Compare all round-trip methods side by side.
fn bench_roundtrip_comparison(c: &mut Criterion) {
    let mut group = c.benchmark_group("roundtrip/comparison");

    // Use medium config for comparison
    let input = common::MEDIUM_CONFIG;
    let format_config = FormatConfig::default();

    group.throughput(Throughput::Bytes(input.len() as u64));

    group.bench_function("value_compact", |b| {
        b.iter(|| {
            let value = input.parse::<ron2::Value>().unwrap();
            value.to_ron_with(&FormatConfig::minimal()).unwrap()
        });
    });

    group.bench_function("value_pretty", |b| {
        b.iter(|| {
            let value = input.parse::<ron2::Value>().unwrap();
            value.to_ron().unwrap()
        });
    });

    group.bench_function("ast_serialize", |b| {
        b.iter(|| {
            let doc = ron2::ast::parse_document(input).unwrap();
            ron2::ast::serialize_document(&doc).unwrap()
        });
    });

    group.bench_function("ast_format", |b| {
        b.iter(|| {
            let doc = ron2::ast::parse_document(input).unwrap();
            ron2::ast::format_document(&doc, &format_config)
        });
    });

    group.finish();
}

/// Benchmark multiple round-trips (stability test).
fn bench_multiple_roundtrips(c: &mut Criterion) {
    let mut group = c.benchmark_group("roundtrip/stability");

    let input = common::MEDIUM_CONFIG;

    group.bench_function("value_3x", |b| {
        b.iter(|| {
            let v1 = input.parse::<ron2::Value>().unwrap();
            let s1 = v1.to_ron_with(&FormatConfig::minimal()).unwrap();
            let v2 = &s1.parse::<ron2::Value>().unwrap();
            let s2 = v2.to_ron_with(&FormatConfig::minimal()).unwrap();
            let v3 = &s2.parse::<ron2::Value>().unwrap();
            v3.to_ron_with(&FormatConfig::minimal()).unwrap()
        });
    });

    group.bench_function("ast_3x", |b| {
        b.iter(|| {
            let d1 = ron2::ast::parse_document(input).unwrap();
            let s1 = ron2::ast::serialize_document(&d1).unwrap();
            let d2 = ron2::ast::parse_document(&s1).unwrap();
            let s2 = ron2::ast::serialize_document(&d2).unwrap();
            let d3 = ron2::ast::parse_document(&s2).unwrap();
            ron2::ast::serialize_document(&d3).unwrap()
        });
    });

    group.finish();
}

criterion_group!(
    benches,
    bench_value_roundtrip_compact,
    bench_value_roundtrip_pretty,
    bench_ast_roundtrip,
    bench_ast_format_roundtrip,
    bench_roundtrip_comparison,
    bench_multiple_roundtrips,
);

criterion_main!(benches);
