//! Typed serialization/deserialization benchmarks for ron2.
//!
//! Compares ron2's derive-based approach (ToRon/FromRon) against
//! the original `ron` crate's serde-based approach.
//! Uses large config only for meaningful performance measurement.

use criterion::{Criterion, Throughput, criterion_group, criterion_main};
use ron2::{FromRon, ToRon};
use ron2_derive::{FromRon, ToRon};
use serde::{Deserialize, Serialize};

// ============================================================================
// Test Types - derive both ron2 and serde traits
// ============================================================================

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

    let config = large_config();
    let ron2_output = config.to_ron().unwrap();
    group.throughput(Throughput::Bytes(ron2_output.len() as u64));

    group.bench_function("ron2", |b| {
        b.iter(|| config.to_ron().unwrap());
    });

    group.bench_function("serde", |b| {
        b.iter(|| ron::to_string(&config).unwrap());
    });

    group.finish();
}

// ============================================================================
// Deserialization Benchmarks
// ============================================================================

fn bench_deserialize(c: &mut Criterion) {
    let mut group = c.benchmark_group("typed/deserialize");

    let config = large_config();
    let input = config.to_ron().unwrap();
    group.throughput(Throughput::Bytes(input.len() as u64));

    group.bench_function("ron2", |b| {
        b.iter(|| LargeConfig::from_ron(&input).unwrap());
    });

    group.bench_function("serde", |b| {
        b.iter(|| ron::from_str::<LargeConfig>(&input).unwrap());
    });

    group.finish();
}

criterion_group!(benches, bench_serialize, bench_deserialize);

criterion_main!(benches);
