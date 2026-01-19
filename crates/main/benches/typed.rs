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
// Field Scaling Test Types - measure O(n²) vs O(n) field lookup
// ============================================================================

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, ToRon, FromRon)]
struct Fields10 {
    f01: i32,
    f02: i32,
    f03: i32,
    f04: i32,
    f05: i32,
    f06: i32,
    f07: i32,
    f08: i32,
    f09: i32,
    f10: i32,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, ToRon, FromRon)]
struct Fields20 {
    f01: i32,
    f02: i32,
    f03: i32,
    f04: i32,
    f05: i32,
    f06: i32,
    f07: i32,
    f08: i32,
    f09: i32,
    f10: i32,
    f11: i32,
    f12: i32,
    f13: i32,
    f14: i32,
    f15: i32,
    f16: i32,
    f17: i32,
    f18: i32,
    f19: i32,
    f20: i32,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, ToRon, FromRon)]
struct Fields30 {
    f01: i32,
    f02: i32,
    f03: i32,
    f04: i32,
    f05: i32,
    f06: i32,
    f07: i32,
    f08: i32,
    f09: i32,
    f10: i32,
    f11: i32,
    f12: i32,
    f13: i32,
    f14: i32,
    f15: i32,
    f16: i32,
    f17: i32,
    f18: i32,
    f19: i32,
    f20: i32,
    f21: i32,
    f22: i32,
    f23: i32,
    f24: i32,
    f25: i32,
    f26: i32,
    f27: i32,
    f28: i32,
    f29: i32,
    f30: i32,
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

fn fields10() -> Fields10 {
    Fields10 {
        f01: 1,
        f02: 2,
        f03: 3,
        f04: 4,
        f05: 5,
        f06: 6,
        f07: 7,
        f08: 8,
        f09: 9,
        f10: 10,
    }
}

fn fields20() -> Fields20 {
    Fields20 {
        f01: 1,
        f02: 2,
        f03: 3,
        f04: 4,
        f05: 5,
        f06: 6,
        f07: 7,
        f08: 8,
        f09: 9,
        f10: 10,
        f11: 11,
        f12: 12,
        f13: 13,
        f14: 14,
        f15: 15,
        f16: 16,
        f17: 17,
        f18: 18,
        f19: 19,
        f20: 20,
    }
}

fn fields30() -> Fields30 {
    Fields30 {
        f01: 1,
        f02: 2,
        f03: 3,
        f04: 4,
        f05: 5,
        f06: 6,
        f07: 7,
        f08: 8,
        f09: 9,
        f10: 10,
        f11: 11,
        f12: 12,
        f13: 13,
        f14: 14,
        f15: 15,
        f16: 16,
        f17: 17,
        f18: 18,
        f19: 19,
        f20: 20,
        f21: 21,
        f22: 22,
        f23: 23,
        f24: 24,
        f25: 25,
        f26: 26,
        f27: 27,
        f28: 28,
        f29: 29,
        f30: 30,
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

// ============================================================================
// Field Scaling Benchmarks - measure O(n²) vs O(n) lookup performance
// ============================================================================

fn bench_field_scaling(c: &mut Criterion) {
    let mut group = c.benchmark_group("typed/field_scaling");

    // 10 fields
    let data10 = fields10();
    let input10 = data10.to_ron().unwrap();
    group.bench_function("10_fields", |b| {
        b.iter(|| Fields10::from_ron(&input10).unwrap());
    });

    // 20 fields
    let data20 = fields20();
    let input20 = data20.to_ron().unwrap();
    group.bench_function("20_fields", |b| {
        b.iter(|| Fields20::from_ron(&input20).unwrap());
    });

    // 30 fields
    let data30 = fields30();
    let input30 = data30.to_ron().unwrap();
    group.bench_function("30_fields", |b| {
        b.iter(|| Fields30::from_ron(&input30).unwrap());
    });

    group.finish();
}

criterion_group!(
    benches,
    bench_serialize,
    bench_deserialize,
    bench_field_scaling
);

criterion_main!(benches);
