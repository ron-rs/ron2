//! Typed serialization/deserialization benchmarks for ron2.
//!
//! Compares ron2's derive-based approach (ToRon/FromRon) against
//! the original `ron` crate's serde-based approach.

use criterion::{Criterion, Throughput, criterion_group, criterion_main};
use ron_derive::{FromRon, ToRon};
use ron2::{FromRon, ToRon};
use serde::{Deserialize, Serialize};

// ============================================================================
// Test Types - derive both ron2 and serde traits
// ============================================================================

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, ToRon, FromRon)]
struct Config {
    name: String,
    version: (u32, u32, u32),
    debug: bool,
    timeout: u32,
    tags: Vec<String>,
    metadata: Option<Metadata>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, ToRon, FromRon)]
struct Metadata {
    author: String,
    license: String,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, ToRon, FromRon)]
struct Item {
    id: u32,
    name: String,
    value: f64,
    enabled: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, ToRon, FromRon)]
struct LargeConfig {
    name: String,
    version: (u32, u32, u32),
    items: Vec<Item>,
}

// ============================================================================
// Test Data
// ============================================================================

fn small_config() -> Config {
    Config {
        name: "test application".to_string(),
        version: (1, 2, 3),
        debug: true,
        timeout: 30,
        tags: vec!["web".to_string(), "api".to_string(), "production".to_string()],
        metadata: Some(Metadata {
            author: "test".to_string(),
            license: "MIT".to_string(),
        }),
    }
}

fn large_config() -> LargeConfig {
    LargeConfig {
        name: "large configuration".to_string(),
        version: (1, 0, 0),
        items: (0..100)
            .map(|i| Item {
                id: i,
                name: format!("item_{}", i),
                value: (i * 10) as f64 + (i % 10) as f64 / 10.0,
                enabled: i % 2 == 0,
            })
            .collect(),
    }
}

// ============================================================================
// Serialization Benchmarks
// ============================================================================

fn bench_serialize(c: &mut Criterion) {
    let mut group = c.benchmark_group("typed/serialize");

    // Small config
    let config = small_config();
    let ron2_output = config.to_ron().unwrap();
    group.throughput(Throughput::Bytes(ron2_output.len() as u64));

    group.bench_function("ron2_small", |b| {
        b.iter(|| config.to_ron().unwrap());
    });

    group.bench_function("ron_serde_small", |b| {
        b.iter(|| ron::to_string(&config).unwrap());
    });

    // Large config
    let large = large_config();
    let ron2_large_output = large.to_ron().unwrap();
    group.throughput(Throughput::Bytes(ron2_large_output.len() as u64));

    group.bench_function("ron2_large", |b| {
        b.iter(|| large.to_ron().unwrap());
    });

    group.bench_function("ron_serde_large", |b| {
        b.iter(|| ron::to_string(&large).unwrap());
    });

    group.finish();
}

// ============================================================================
// Deserialization Benchmarks
// ============================================================================

fn bench_deserialize(c: &mut Criterion) {
    let mut group = c.benchmark_group("typed/deserialize");

    // Small config - use ron2 output as input (both parsers should handle it)
    let config = small_config();
    let input = config.to_ron().unwrap();
    group.throughput(Throughput::Bytes(input.len() as u64));

    group.bench_function("ron2_small", |b| {
        b.iter(|| Config::from_ron(&input).unwrap());
    });

    group.bench_function("ron_serde_small", |b| {
        b.iter(|| ron::from_str::<Config>(&input).unwrap());
    });

    // Large config
    let large = large_config();
    let large_input = large.to_ron().unwrap();
    group.throughput(Throughput::Bytes(large_input.len() as u64));

    group.bench_function("ron2_large", |b| {
        b.iter(|| LargeConfig::from_ron(&large_input).unwrap());
    });

    group.bench_function("ron_serde_large", |b| {
        b.iter(|| ron::from_str::<LargeConfig>(&large_input).unwrap());
    });

    group.finish();
}

// ============================================================================
// Round-trip Benchmarks
// ============================================================================

fn bench_roundtrip(c: &mut Criterion) {
    let mut group = c.benchmark_group("typed/roundtrip");

    // Small config
    let config = small_config();
    let input = config.to_ron().unwrap();
    group.throughput(Throughput::Bytes(input.len() as u64));

    group.bench_function("ron2_small", |b| {
        b.iter(|| {
            let s = config.to_ron().unwrap();
            Config::from_ron(&s).unwrap()
        });
    });

    group.bench_function("ron_serde_small", |b| {
        b.iter(|| {
            let s = ron::to_string(&config).unwrap();
            ron::from_str::<Config>(&s).unwrap()
        });
    });

    // Large config
    let large = large_config();
    let large_input = large.to_ron().unwrap();
    group.throughput(Throughput::Bytes(large_input.len() as u64));

    group.bench_function("ron2_large", |b| {
        b.iter(|| {
            let s = large.to_ron().unwrap();
            LargeConfig::from_ron(&s).unwrap()
        });
    });

    group.bench_function("ron_serde_large", |b| {
        b.iter(|| {
            let s = ron::to_string(&large).unwrap();
            ron::from_str::<LargeConfig>(&s).unwrap()
        });
    });

    group.finish();
}

criterion_group!(benches, bench_serialize, bench_deserialize, bench_roundtrip,);

criterion_main!(benches);
