use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use std::time::Duration;

use tlvxc_runtime::Scheduler;

fn bench_spawn_overhead(c: &mut Criterion) {
    let mut g = c.benchmark_group("spawn_overhead");
    g.sample_size(20);
    g.measurement_time(Duration::from_secs(3));

    for &jobs in &[1_000usize, 5_000, 10_000] {
        g.throughput(Throughput::Elements(jobs as u64));
        g.bench_with_input(BenchmarkId::from_parameter(jobs), &jobs, |b, &n| {
            b.iter(|| {
                let sched = Scheduler::new(Some(2));
                static CNT: AtomicUsize = AtomicUsize::new(0);
                CNT.store(0, Ordering::SeqCst);
                for _ in 0..n {
                    sched.spawn(|_| {
                        CNT.fetch_add(1, Ordering::SeqCst);
                    });
                }
                // spin-wait with small sleeps to allow completion
                while CNT.load(Ordering::SeqCst) < n {
                    std::thread::sleep(Duration::from_micros(50));
                }
                sched.shutdown();
                sched.join();
            });
        });
    }
    g.finish();
}

fn bench_throughput_scaling(c: &mut Criterion) {
    let mut g = c.benchmark_group("throughput_scaling");
    g.sample_size(10);
    g.measurement_time(Duration::from_secs(15));

    // Use increasing worker counts up to 16.
    let configs = [1usize, 2, 4, 8, 16];

    for &threads in &configs {
        g.bench_with_input(
            BenchmarkId::from_parameter(format!("threads={threads}")),
            &threads,
            |b, &t| {
                b.iter_custom(|iters| {
                    use std::time::Instant;
                    let mut total = Duration::ZERO;
                    for _ in 0..iters {
                        // Fixed total workload per iteration to assess scaling.
                        let total_jobs: usize = 200;
                        let heavy = std::env::var("TOLVEX_BENCH_HEAVY")
                            .ok()
                            .map(|s| s == "1")
                            .unwrap_or(false);
                        let compute_iters: u32 = if heavy { 20_000_000 } else { 5_000_000 };
                        let sched = Scheduler::new(Some(t));
                        let cnt = Arc::new(AtomicUsize::new(0));
                        let start = Instant::now();
                        for _ in 0..total_jobs {
                            let cntc = cnt.clone();
                            sched.spawn(move |_| {
                                let mut x: u64 = black_box(0x9E3779B97F4A7C15);
                                for i in 0..compute_iters {
                                    let ii = black_box(i as u64);
                                    x = x.wrapping_mul(1664525).wrapping_add(1013904223) ^ ii;
                                }
                                black_box(x);
                                core::sync::atomic::fence(core::sync::atomic::Ordering::SeqCst);
                                cntc.fetch_add(1, Ordering::SeqCst);
                            });
                        }
                        while cnt.load(Ordering::SeqCst) < total_jobs {
                            std::thread::sleep(Duration::from_millis(2));
                        }
                        sched.shutdown();
                        sched.join();
                        total += start.elapsed();
                    }
                    if iters == 0 {
                        total = Duration::from_micros(1);
                    }
                    total
                });
            },
        );
    }

    g.finish();
}

fn bench_steal_heavy(c: &mut Criterion) {
    let mut g = c.benchmark_group("steal_heavy");
    g.sample_size(15);
    g.measurement_time(Duration::from_secs(4));

    g.bench_function("steal_mixed_workload", |b| {
        b.iter(|| {
            let sched = Scheduler::new(Some(8));
            let cnt = Arc::new(AtomicUsize::new(0));
            let total_jobs = 50_000usize;
            for i in 0..total_jobs {
                let cntc = cnt.clone();
                // Alternate between small and medium jobs to encourage stealing
                if i % 3 == 0 {
                    sched.spawn(move |_| {
                        for _ in 0..64 {
                            core::hint::spin_loop();
                        }
                        cntc.fetch_add(1, Ordering::SeqCst);
                    });
                } else {
                    sched.spawn(move |_| {
                        for _ in 0..8 {
                            core::hint::spin_loop();
                        }
                        cntc.fetch_add(1, Ordering::SeqCst);
                    });
                }
            }
            while cnt.load(Ordering::SeqCst) < total_jobs {
                std::thread::sleep(Duration::from_millis(1));
            }
            sched.shutdown();
            sched.join();
        });
    });

    g.finish();
}

criterion_group!(
    benches,
    bench_spawn_overhead,
    bench_throughput_scaling,
    bench_steal_heavy
);
criterion_main!(benches);
