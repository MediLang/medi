use criterion::{criterion_group, criterion_main, BatchSize, Criterion};
use medic_runtime::gc::{GarbageCollector, GcParams};
use medic_runtime::GcRef;

fn params_with_env(nursery_default: usize, max_pause_ms: u64) -> GcParams {
    let nursery = std::env::var("MEDI_GC_NURSERY_BYTES")
        .ok()
        .and_then(|s| s.parse::<usize>().ok())
        .unwrap_or(nursery_default);
    GcParams {
        nursery_threshold_bytes: nursery,
        gen_promotion_threshold: 2,
        max_pause_ms,
    }
}

#[allow(dead_code)]
fn bench_leak_cycles(c: &mut Criterion) {
    let mut group = c.benchmark_group("leak_cycles");
    group.bench_function("alloc_free_cycles_return_to_baseline", |b| {
        b.iter_batched(
            || GarbageCollector::with_params(params_with_env(1 << 16, 10)),
            |mut gc| {
                // cycle 1: allocate a bunch, then drop
                let mut roots = Vec::new();
                for i in 0..20_000u32 {
                    let s = format!("s{i}");
                    let r = gc.allocate::<String>(s);
                    if i % 64 == 0 {
                        gc.add_root(&r);
                        roots.push(r);
                    }
                }
                for r in &roots {
                    gc.remove_root(r);
                }
                gc.collect_garbage();
                let baseline = gc.stats().total_objects;
                // cycle 2: repeat
                let mut roots2 = Vec::new();
                for i in 0..20_000u32 {
                    let s = format!("t{i}");
                    let r = gc.allocate::<String>(s);
                    if i % 64 == 0 {
                        gc.add_root(&r);
                        roots2.push(r);
                    }
                }
                for r in &roots2 {
                    gc.remove_root(r);
                }
                gc.collect_garbage();
                let after = gc.stats().total_objects;
                // Expect to return near baseline (allow a small cushion for implementation details)
                assert!(
                    after <= baseline + 10,
                    "object table did not return to baseline: after={after}, baseline={baseline}"
                );
            },
            BatchSize::SmallInput,
        )
    });
    group.finish();
}

#[cfg(feature = "gc-incremental")]
fn bench_incremental_100mb(c: &mut Criterion) {
    let mut group = c.benchmark_group("incremental_100mb");
    group.bench_function("strings_payload_approx_100mb_p99_step_budget", |b| {
        b.iter_batched(
            || {
                let mut gc = GarbageCollector::with_params(params_with_env(1 << 16, 5));
                // Build ~100MB by payload length (string overhead not counted)
                let target_bytes: usize = 100 * 1024 * 1024;
                let mut bytes = 0usize;
                let payload = "x".repeat(64);
                let mut roots = Vec::new();
                while bytes < target_bytes {
                    let r = gc.allocate::<String>(payload.clone());
                    if bytes % (64 * 1024) == 0 {
                        gc.add_root(&r);
                        roots.push(r);
                    }
                    bytes += payload.len();
                }
                gc.incremental_start_major();
                (gc, roots)
            },
            |(mut gc, roots)| {
                let budget_ms = 5u64; // respect GcParams in params_with_env()
                let mut step_times = Vec::new();
                loop {
                    let t0 = std::time::Instant::now();
                    let done = gc.incremental_step_with_params();
                    step_times.push(t0.elapsed());
                    if done {
                        break;
                    }
                }
                if std::env::var("BENCH_ASSERT").ok().as_deref() == Some("1") {
                    step_times.sort();
                    if !step_times.is_empty() {
                        let p99_idx = ((step_times.len() as f64) * 0.99).floor() as usize;
                        let p99 = step_times[p99_idx.min(step_times.len() - 1)];
                        assert!(
                            p99.as_millis() as u64 <= budget_ms,
                            "p99 step {}ms exceeds budget {}ms",
                            p99.as_millis(),
                            budget_ms
                        );
                    }
                }
                // cleanup roots
                for r in &roots {
                    gc.remove_root(r);
                }
                gc.collect_garbage();
            },
            BatchSize::SmallInput,
        )
    });
    group.finish();
}

fn bench_minor_gc_pause(c: &mut Criterion) {
    let mut group = c.benchmark_group("minor_gc_pause");
    group.bench_function("alloc_small_strings_minor_collect", |b| {
        b.iter_batched(
            || GarbageCollector::with_params(params_with_env(1 << 16, 10)),
            |mut gc| {
                for i in 0..10_000u32 {
                    let s = format!("s{i}");
                    let r = gc.allocate::<String>(s);
                    if i % 32 == 0 {
                        gc.add_root(&r);
                        gc.remove_root(&r);
                    }
                    if i % 256 == 0 {
                        gc.minor_collect();
                    }
                }
                // snapshot stats so optimizer can't elide calls
                let st = gc.stats();
                criterion::black_box(st.last_minor_pause_ms);
            },
            BatchSize::SmallInput,
        )
    });
    group.finish();
}
#[cfg(feature = "gc-incremental")]
fn bench_incremental_major(c: &mut Criterion) {
    let mut group = c.benchmark_group("incremental_major");
    group.bench_function("step_latency_and_total_completion", |b| {
        b.iter_batched(
            || {
                let mut gc = GarbageCollector::with_params(params_with_env(1 << 16, 5));
                // Build a heap with many objects and edges
                let mut roots = Vec::new();
                for i in 0..10_000u32 {
                    let s = format!("s{i}");
                    let r = gc.allocate::<String>(s);
                    if i % 100 == 0 {
                        gc.add_root(&r);
                        roots.push(r);
                    }
                }
                gc.incremental_start_major();
                gc
            },
            |mut gc| {
                let stats = gc.stats();
                criterion::black_box(stats.total_objects);
                let start = std::time::Instant::now();
                let mut step_times = Vec::new();
                let budget_ms = 5u64; // mirrors GcParams in setup
                loop {
                    let t0 = std::time::Instant::now();
                    let done = gc.incremental_step_with_params();
                    step_times.push(t0.elapsed());
                    if done {
                        break;
                    }
                }
                let total = start.elapsed();
                // Optional assertion on p99 vs budget
                if std::env::var("BENCH_ASSERT").ok().as_deref() == Some("1") {
                    step_times.sort();
                    let len = step_times.len();
                    if len > 0 {
                        let p99_idx = ((len as f64) * 0.99).floor() as usize;
                        let p99 = step_times[p99_idx.min(len - 1)];
                        assert!(
                            p99.as_millis() as u64 <= budget_ms,
                            "p99 step {}ms exceeds budget {}ms",
                            p99.as_millis(),
                            budget_ms
                        );
                    }
                }
                criterion::black_box((step_times.len(), total));
            },
            BatchSize::SmallInput,
        )
    });
    group.finish();
}

fn bench_major_gc_pause(c: &mut Criterion) {
    let mut group = c.benchmark_group("major_gc_pause");
    group.bench_function("alloc_many_then_major_collect", |b| {
        b.iter_batched(
            || GarbageCollector::with_params(params_with_env(1 << 18, 10)),
            |mut gc| {
                // Allocate ~100MB of strings (approx; string overhead ignored for simplicity)
                let target_bytes: usize = 100 * 1024 * 1024;
                let mut bytes = 0usize;
                let mut roots = Vec::new();
                let payload = "x".repeat(64); // 64B each string body
                while bytes < target_bytes {
                    let r = gc.allocate::<String>(payload.clone());
                    if bytes % (64 * 1024) == 0 {
                        gc.add_root(&r);
                        roots.push(r);
                    }
                    bytes += payload.len();
                }
                gc.collect_garbage();
                let st = gc.stats();
                criterion::black_box(st.last_major_pause_ms);
                for r in &roots {
                    gc.remove_root(r);
                }
                gc.collect_garbage();
            },
            BatchSize::SmallInput,
        )
    });
    group.finish();
}

fn bench_throughput(c: &mut Criterion) {
    let mut group = c.benchmark_group("throughput");
    group.bench_function("alloc_mutate_with_and_without_minor", |b| {
        b.iter(|| {
            let mut gc = GarbageCollector::with_params(params_with_env(1 << 16, 10));
            for i in 0..50_000u32 {
                let s = format!("s{i}");
                let r = gc.allocate::<String>(s);
                if i % 2 == 0 {
                    gc.add_root(&r);
                    gc.remove_root(&r);
                }
                if i % 512 == 0 {
                    gc.minor_collect();
                }
            }
        })
    });
    group.finish();
}

#[cfg(not(feature = "gc-incremental"))]
criterion_group!(
    benches,
    bench_minor_gc_pause,
    bench_major_gc_pause,
    bench_throughput
);
#[cfg(feature = "gc-incremental")]
criterion_group!(
    benches,
    bench_minor_gc_pause,
    bench_major_gc_pause,
    bench_throughput,
    bench_leak_cycles,
    bench_incremental_major,
    bench_incremental_heapsizes,
    bench_incremental_100mb,
    bench_minor_array_strings
);
criterion_main!(benches);

#[cfg(feature = "gc-incremental")]
fn bench_incremental_heapsizes(c: &mut Criterion) {
    let mut group = c.benchmark_group("incremental_heapsizes");
    for &n in &[10_000u32, 50_000u32, 100_000u32] {
        group.bench_with_input(format!("strings_{}", n), &n, |b, &n| {
            b.iter_batched(
                || {
                    let mut gc = GarbageCollector::with_params(params_with_env(1 << 16, 5));
                    // Populate heap with strings
                    for i in 0..n {
                        let s = format!("s{i}");
                        let r = gc.allocate::<String>(s);
                        if i % 128 == 0 {
                            gc.add_root(&r);
                        }
                    }
                    gc.incremental_start_major();
                    gc
                },
                |mut gc| {
                    let mut step_times = Vec::new();
                    let budget_ms = 5u64; // mirrors GcParams in setup
                    loop {
                        let t0 = std::time::Instant::now();
                        let done = gc.incremental_step_with_params();
                        step_times.push(t0.elapsed());
                        if done {
                            break;
                        }
                    }
                    // compute simple p99 (approx) from collected durations
                    step_times.sort();
                    let len = step_times.len();
                    if len > 0 {
                        let p99_idx = ((len as f64) * 0.99).floor() as usize;
                        let p99 = step_times[p99_idx.min(len - 1)];
                        if std::env::var("BENCH_ASSERT").ok().as_deref() == Some("1") {
                            assert!(
                                p99.as_millis() as u64 <= budget_ms,
                                "p99 step {}ms exceeds budget {}ms",
                                p99.as_millis(),
                                budget_ms
                            );
                        }
                        criterion::black_box(p99);
                    }
                },
                BatchSize::SmallInput,
            )
        });
    }
    group.finish();
}

#[allow(dead_code)]
fn bench_minor_array_strings(c: &mut Criterion) {
    let mut group = c.benchmark_group("minor_gc_array_strings");
    group.bench_function("array_of_1k_strings_minor_collect", |b| {
        b.iter_batched(
            || {
                GarbageCollector::with_params(GcParams {
                    nursery_threshold_bytes: 1 << 16,
                    gen_promotion_threshold: 2,
                    max_pause_ms: 10,
                })
            },
            |mut gc| {
                let mut arr: Vec<GcRef<String>> = Vec::with_capacity(1_000);
                for i in 0..1_000u32 {
                    let s = format!("s{i}");
                    let r = gc.allocate::<String>(s);
                    gc.add_root(&r);
                    arr.push(r);
                }
                gc.minor_collect();
                criterion::black_box(arr.len());
            },
            BatchSize::SmallInput,
        )
    });
    group.finish();
}
