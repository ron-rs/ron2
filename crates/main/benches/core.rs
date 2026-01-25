//! Core benchmarks for ron2.
//!
//! Consolidates parsing, serialization, and formatting benchmarks.
//! Uses large input only for meaningful performance measurement.

mod common;

use std::hint::black_box;

use criterion::{Criterion, Throughput, criterion_group, criterion_main};
use ron2::{__internal::Lexer, ToRon, fmt::FormatConfig};

/// Benchmark AST parsing using `parse_document()`.
fn bench_parse_ast(c: &mut Criterion) {
    let mut group = c.benchmark_group("core");
    let input = common::large_config();

    group.throughput(Throughput::Bytes(input.len() as u64));

    group.bench_function("parse_ast", |b| {
        b.iter(|| ron2::ast::parse_document(&input).unwrap());
    });

    group.finish();
}

/// Benchmark AST parsing followed by Value conversion.
fn bench_parse_value(c: &mut Criterion) {
    let mut group = c.benchmark_group("core");
    let input = common::large_config();

    group.throughput(Throughput::Bytes(input.len() as u64));

    group.bench_function("parse_value", |b| {
        b.iter(|| {
            let doc = ron2::ast::parse_document(&input).unwrap();
            ron2::ast::to_value(&doc).unwrap().unwrap()
        });
    });

    group.finish();
}

/// Benchmark AST serialization using `serialize_document()`.
fn bench_serialize_ast(c: &mut Criterion) {
    let mut group = c.benchmark_group("core");
    let input = common::large_config();
    let doc = ron2::ast::parse_document(&input).unwrap();

    group.throughput(Throughput::Bytes(input.len() as u64));

    group.bench_function("serialize_ast", |b| {
        b.iter(|| ron2::ast::serialize_document(&doc).unwrap());
    });

    group.finish();
}

/// Benchmark Value serialization using `to_ron()`.
fn bench_serialize_value(c: &mut Criterion) {
    let mut group = c.benchmark_group("core");
    let input = common::large_config();
    let value: ron2::Value = input.parse().unwrap();
    let output_size = value.to_ron().unwrap().len();

    group.throughput(Throughput::Bytes(output_size as u64));

    group.bench_function("serialize_value", |b| {
        b.iter(|| value.to_ron().unwrap());
    });

    group.finish();
}

/// Benchmark AST formatting with default config.
fn bench_format(c: &mut Criterion) {
    let mut group = c.benchmark_group("core");
    let input = common::large_config();
    let doc = ron2::ast::parse_document(&input).unwrap();
    let config = FormatConfig::default();
    let output_size = ron2::fmt::format_document(&doc, &config).len();

    group.throughput(Throughput::Bytes(output_size as u64));

    group.bench_function("format", |b| {
        b.iter(|| ron2::fmt::format_document(&doc, &config));
    });

    group.finish();
}

/// Benchmark formatting with vs without comments.
fn bench_comments(c: &mut Criterion) {
    let mut group = c.benchmark_group("core/comments");
    let config = FormatConfig::default();

    // With comments
    let with_comments = common::with_comments();
    let doc_with = ron2::ast::parse_document(&with_comments).unwrap();
    let output_size_with = ron2::fmt::format_document(&doc_with, &config).len();
    group.throughput(Throughput::Bytes(output_size_with as u64));

    group.bench_function("with", |b| {
        b.iter(|| ron2::fmt::format_document(&doc_with, &config));
    });

    // Without comments (similar size input)
    let without_comments = common::without_comments();
    let doc_without = ron2::ast::parse_document(&without_comments).unwrap();
    let output_size_without = ron2::fmt::format_document(&doc_without, &config).len();
    group.throughput(Throughput::Bytes(output_size_without as u64));

    group.bench_function("without", |b| {
        b.iter(|| ron2::fmt::format_document(&doc_without, &config));
    });

    group.finish();
}

/// Benchmark pure lexer tokenization (no AST construction).
fn bench_lexer(c: &mut Criterion) {
    let mut group = c.benchmark_group("core");
    let input = common::large_config();

    // Print stats once
    let token_count = Lexer::new(&input).count();
    let bytes = input.len();
    eprintln!(
        "Lexer stats: {} bytes, {} tokens, {:.1} bytes/token",
        bytes,
        token_count,
        bytes as f64 / token_count as f64
    );

    group.throughput(Throughput::Bytes(input.len() as u64));

    // Lexer without trivia (value parsing mode)
    group.bench_function("lexer", |b| {
        b.iter(|| {
            let lexer = Lexer::new(&input);
            for token in lexer {
                black_box(token);
            }
        });
    });

    // Lexer with trivia (AST parsing mode)
    group.bench_function("lexer_trivia", |b| {
        b.iter(|| {
            let lexer = Lexer::new(&input).with_trivia(true);
            for token in lexer {
                black_box(token);
            }
        });
    });

    group.finish();
}

criterion_group!(
    benches,
    bench_lexer,
    bench_parse_ast,
    bench_parse_value,
    bench_serialize_ast,
    bench_serialize_value,
    bench_format,
    bench_comments,
);

criterion_main!(benches);
