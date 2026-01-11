//! AST formatting benchmarks for ron2.
//!
//! Benchmarks the `format_document()` function which reformats RON
//! while preserving comments.

mod common;

use criterion::{BenchmarkId, Criterion, Throughput, criterion_group, criterion_main};
use ron2::ast::FormatConfig;

/// Benchmark AST formatting with default config.
fn bench_format_default(c: &mut Criterion) {
    let mut group = c.benchmark_group("formatting/default");
    let config = FormatConfig::default();

    for (name, input) in common::test_inputs() {
        let doc = ron2::ast::parse_document(&input).unwrap();
        let output_size = ron2::ast::format_document(&doc, &config).len();
        group.throughput(Throughput::Bytes(output_size as u64));

        group.bench_with_input(BenchmarkId::from_parameter(name), &doc, |b, doc| {
            b.iter(|| ron2::ast::format_document(doc, &config));
        });
    }

    group.finish();
}

/// Benchmark formatting with custom indent (2 spaces).
fn bench_format_custom_indent(c: &mut Criterion) {
    let mut group = c.benchmark_group("formatting/indent_2");
    let config = FormatConfig::new().indent("  ");

    for (name, input) in common::test_inputs() {
        let doc = ron2::ast::parse_document(&input).unwrap();
        let output_size = ron2::ast::format_document(&doc, &config).len();
        group.throughput(Throughput::Bytes(output_size as u64));

        group.bench_with_input(BenchmarkId::from_parameter(name), &doc, |b, doc| {
            b.iter(|| ron2::ast::format_document(doc, &config));
        });
    }

    group.finish();
}

/// Benchmark formatting with narrow char limit (forces more multiline).
fn bench_format_narrow(c: &mut Criterion) {
    let mut group = c.benchmark_group("formatting/narrow");
    let config = FormatConfig::new().char_limit(40);

    for (name, input) in common::test_inputs() {
        let doc = ron2::ast::parse_document(&input).unwrap();
        let output_size = ron2::ast::format_document(&doc, &config).len();
        group.throughput(Throughput::Bytes(output_size as u64));

        group.bench_with_input(BenchmarkId::from_parameter(name), &doc, |b, doc| {
            b.iter(|| ron2::ast::format_document(doc, &config));
        });
    }

    group.finish();
}

/// Compare formatting vs round-trip serialization.
fn bench_format_vs_serialize(c: &mut Criterion) {
    let mut group = c.benchmark_group("formatting/vs_serialize");

    // Use medium config for comparison
    let input = common::MEDIUM_CONFIG;
    let doc = ron2::ast::parse_document(input).unwrap();
    let format_config = FormatConfig::default();

    group.throughput(Throughput::Bytes(input.len() as u64));

    group.bench_function("format_document", |b| {
        b.iter(|| ron2::ast::format_document(&doc, &format_config));
    });

    group.bench_function("serialize_document", |b| {
        b.iter(|| ron2::ast::serialize_document(&doc).unwrap());
    });

    group.finish();
}

/// Benchmark formatting with comments specifically.
fn bench_format_with_comments(c: &mut Criterion) {
    let mut group = c.benchmark_group("formatting/comments");

    let input = common::WITH_COMMENTS;
    let doc = ron2::ast::parse_document(input).unwrap();
    let config = FormatConfig::default();
    let output_size = ron2::ast::format_document(&doc, &config).len();

    group.throughput(Throughput::Bytes(output_size as u64));

    group.bench_function("with_comments", |b| {
        b.iter(|| ron2::ast::format_document(&doc, &config));
    });

    // Compare with same-size input without comments
    let no_comments = common::MEDIUM_CONFIG;
    let doc_no_comments = ron2::ast::parse_document(no_comments).unwrap();

    group.bench_function("without_comments", |b| {
        b.iter(|| ron2::ast::format_document(&doc_no_comments, &config));
    });

    group.finish();
}

criterion_group!(
    benches,
    bench_format_default,
    bench_format_custom_indent,
    bench_format_narrow,
    bench_format_vs_serialize,
    bench_format_with_comments,
);

criterion_main!(benches);
