use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use std::time::Duration;

use tlvxc_runtime::{Priority, Scheduler};

#[test]
fn parallel_sum_large() {
    let sched = Scheduler::new(Some(8));
    let n = 1_000_000usize;
    let data: Arc<Vec<u32>> = Arc::new((0..n as u32).collect());
    let chunks = 128usize;
    let chunk_size = n.div_ceil(chunks);
    let sum_atomic = Arc::new(AtomicUsize::new(0));

    for ci in 0..chunks {
        let start = ci * chunk_size;
        let end = ((ci + 1) * chunk_size).min(n);
        if start >= end {
            break;
        }
        let data_arc = data.clone();
        let acc = sum_atomic.clone();
        sched.spawn(move |_| {
            let mut s: usize = 0;
            for i in start..end {
                s += data_arc[i] as usize;
            }
            acc.fetch_add(s, Ordering::SeqCst);
        });
    }

    // Wait for completion
    let expected: usize = data.iter().map(|&v| v as usize).sum();
    let deadline = std::time::Instant::now() + Duration::from_secs(45);
    while sum_atomic.load(Ordering::SeqCst) != expected {
        assert!(
            std::time::Instant::now() < deadline,
            "parallel_sum_large timeout"
        );
        std::thread::sleep(Duration::from_millis(5));
    }

    // Clean shutdown
    sched.shutdown();
    sched.join();
}

#[test]
fn contention_many_increments() {
    let sched = Scheduler::new(Some(8));
    let tasks = 20_000usize;
    let counter = Arc::new(AtomicUsize::new(0));
    for _ in 0..tasks {
        let c = counter.clone();
        sched.spawn(move |_| {
            // do a tiny bit of work to trigger contention and stealing
            for _ in 0..8 {
                core::hint::spin_loop();
            }
            c.fetch_add(1, Ordering::SeqCst);
        });
    }

    let deadline = std::time::Instant::now() + Duration::from_secs(45);
    while counter.load(Ordering::SeqCst) != tasks {
        assert!(
            std::time::Instant::now() < deadline,
            "contention_many_increments timeout"
        );
        std::thread::sleep(Duration::from_millis(2));
    }

    sched.shutdown();
    sched.join();
}

#[test]
fn priority_fairness_smoke() {
    let sched = Scheduler::new(Some(4));
    let hi = Arc::new(AtomicUsize::new(0));
    let lo = Arc::new(AtomicUsize::new(0));

    for _ in 0..2_000 {
        let h = hi.clone();
        sched.spawn_with_priority(Priority::High, move |_| {
            h.fetch_add(1, Ordering::SeqCst);
        });
        let l = lo.clone();
        sched.spawn_with_priority(Priority::Low, move |_| {
            l.fetch_add(1, Ordering::SeqCst);
        });
    }

    let deadline = std::time::Instant::now() + Duration::from_secs(10);
    while hi.load(Ordering::SeqCst) < 2_000 || lo.load(Ordering::SeqCst) < 2_000 {
        assert!(
            std::time::Instant::now() < deadline,
            "priority_fairness_smoke timeout"
        );
        std::thread::sleep(Duration::from_millis(1));
    }

    sched.shutdown();
    sched.join();
}
