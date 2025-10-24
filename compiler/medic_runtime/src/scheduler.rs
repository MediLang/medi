use crossbeam_deque::{Injector, Steal, Stealer, Worker};
use std::fs;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::thread::{self, JoinHandle};

use crate::error::{report_error, RecoveryPolicy, RuntimeError, SchedulerErrorKind};

#[cfg(feature = "rt_zones")]
use crate::rt::RtRegion;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Priority {
    Low,
    Normal,
    High,
}

// Build a NUMA-aware ordering of cores (Linux): group by node, interleave nodes,
// and map logical CPU indices to core_affinity::CoreId. Returns None when sysfs
// is unavailable or parsing fails.
fn build_numa_aware_core_order() -> Option<Vec<core_affinity::CoreId>> {
    // Read /sys/devices/system/node/node*/cpulist if present
    let entries = fs::read_dir("/sys/devices/system/node").ok()?;
    let mut nodes: Vec<(u32, Vec<usize>)> = Vec::new();
    for e in entries.flatten() {
        let name = e.file_name();
        let s = name.to_string_lossy();
        if !s.starts_with("node") {
            continue;
        }
        let idx: u32 = s[4..].parse().ok()?;
        let cpulist_path = e.path().join("cpulist");
        let content = fs::read_to_string(cpulist_path).ok()?;
        let mut cpus: Vec<usize> = Vec::new();
        for part in content.trim().split(',') {
            if let Some((a, b)) = part.split_once('-') {
                if let (Ok(start), Ok(end)) = (a.trim().parse::<usize>(), b.trim().parse::<usize>())
                {
                    for c in start..=end {
                        cpus.push(c);
                    }
                }
            } else if let Ok(one) = part.trim().parse::<usize>() {
                cpus.push(one);
            }
        }
        cpus.sort_unstable();
        nodes.push((idx, cpus));
    }
    if nodes.is_empty() {
        return None;
    }
    nodes.sort_by_key(|(i, _)| *i);
    // Round-robin across nodes to build an interleaved list of logical CPU indices
    let max_len = nodes.iter().map(|(_, v)| v.len()).max().unwrap_or(0);
    let mut interleaved: Vec<usize> = Vec::new();
    for k in 0..max_len {
        for (_, v) in nodes.iter() {
            if let Some(&cpu) = v.get(k) {
                interleaved.push(cpu);
            }
        }
    }
    // Map logical CPU indices to CoreId list order
    let all = core_affinity::get_core_ids()?;
    // Build lookup by cpu id
    let max_id = interleaved.iter().copied().max().unwrap_or(0);
    let mut by_id: Vec<Option<core_affinity::CoreId>> = vec![None; max_id + 1];
    for cid in all {
        if cid.id < by_id.len() {
            by_id[cid.id] = Some(cid);
        }
    }
    let mut out: Vec<core_affinity::CoreId> = Vec::new();
    for cpu in interleaved {
        if let Some(cid) = by_id.get(cpu).copied().flatten() {
            out.push(cid);
        }
    }
    if out.is_empty() {
        None
    } else {
        Some(out)
    }
}

pub struct TaskCtx {
    #[cfg(feature = "rt_zones")]
    region: RtRegion<{ 64 * 1024 }>, // 64 KiB transient region per worker by default
}

impl TaskCtx {
    #[cfg(feature = "rt_zones")]
    #[inline]
    pub fn region(&self) -> &RtRegion<{ 64 * 1024 }> {
        &self.region
    }
}

pub struct Scheduler {
    running: AtomicBool,
    inject_high: Injector<Job>,
    inject_norm: Injector<Job>,
    inject_low: Injector<Job>,
    stealers: Mutex<Vec<Stealer<Job>>>,
    threads: Mutex<Vec<JoinHandle<()>>>,
    policy: Mutex<RecoveryPolicy>,
}

type JobFn = Box<dyn FnOnce(&mut TaskCtx) + Send + 'static>;

struct Job {
    f: Option<JobFn>,
}

impl Scheduler {
    pub fn new(num_threads: Option<usize>) -> Arc<Self> {
        Self::new_with_policy(num_threads, RecoveryPolicy::SkipTask)
    }

    pub fn new_with_policy(num_threads: Option<usize>, policy: RecoveryPolicy) -> Arc<Self> {
        // Prefer physical cores when available; fall back to logical CPUs
        let default_threads = core_affinity::get_core_ids()
            .map(|ids| ids.len())
            .unwrap_or_else(num_cpus::get);
        let n = num_threads.unwrap_or(default_threads).max(1);
        let inject_high = Injector::new();
        let inject_norm = Injector::new();
        let inject_low = Injector::new();

        // Create per-thread workers locally and collect stealers for shared scheduler
        let mut local_workers: Vec<Worker<Job>> = Vec::with_capacity(n);
        let mut stealers = Vec::with_capacity(n);
        for _ in 0..n {
            // LIFO local queue improves cache locality; stealing happens from the other end
            let w = Worker::new_lifo();
            stealers.push(w.stealer());
            local_workers.push(w);
        }

        let sched = Arc::new(Scheduler {
            running: AtomicBool::new(true),
            inject_high,
            inject_norm,
            inject_low,
            stealers: Mutex::new(stealers),
            threads: Mutex::new(Vec::new()),
            policy: Mutex::new(policy),
        });

        // Spawn worker threads, moving each worker into its thread
        let core_ids = build_numa_aware_core_order()
            .or_else(|| {
                core_affinity::get_core_ids().map(|ids| {
                    // Best-effort interleaving: spread across physical cores by taking evens then odds
                    let mut order = Vec::with_capacity(ids.len());
                    for i in (0..ids.len()).step_by(2) {
                        order.push(ids[i]);
                    }
                    for i in (1..ids.len()).step_by(2) {
                        order.push(ids[i]);
                    }
                    order
                })
            })
            .map(Arc::new);
        for (idx, worker) in local_workers.into_iter().enumerate() {
            let s = Arc::clone(&sched);
            let core_ids_cloned = core_ids.clone();
            let handle = thread::spawn(move || {
                // Attempt to pin this thread to a specific core for better scaling at high thread counts
                if let Some(ids) = core_ids_cloned.as_ref() {
                    if let Some(core) = ids.get(idx % ids.len()) {
                        let _ = core_affinity::set_for_current(*core);
                    }
                }
                worker_loop(s, idx, worker)
            });
            sched.threads.lock().unwrap().push(handle);
        }

        sched
    }

    #[inline]
    pub fn spawn<F>(&self, f: F)
    where
        F: FnOnce(&mut TaskCtx) + Send + 'static,
    {
        self.spawn_with_priority(Priority::Normal, f)
    }

    pub fn spawn_with_priority<F>(&self, pri: Priority, f: F)
    where
        F: FnOnce(&mut TaskCtx) + Send + 'static,
    {
        let job = Job {
            f: Some(Box::new(f)),
        };
        match pri {
            Priority::High => self.inject_high.push(job),
            Priority::Normal => self.inject_norm.push(job),
            Priority::Low => self.inject_low.push(job),
        }
        // Nudge all workers to pick up newly enqueued work
        if let Ok(ths) = self.threads.lock() {
            for h in ths.iter() {
                h.thread().unpark();
            }
        }
    }

    pub fn shutdown(self: &Arc<Self>) {
        self.running.store(false, Ordering::SeqCst);
        // Push empty jobs to wake workers
        let ths = self.threads.lock().unwrap();
        for _ in ths.iter() {
            self.inject_low.push(Job { f: None });
        }
        // Unpark workers
        for h in ths.iter() {
            h.thread().unpark();
        }
        // We can not join here without taking ownership; provide a separate join method
    }

    pub fn join(self: &Arc<Self>) {
        // Drain and join threads
        let mut threads = self.threads.lock().unwrap();
        for h in threads.drain(..) {
            let _ = h.join();
        }
    }

    #[inline]
    pub fn set_policy(&self, p: RecoveryPolicy) {
        if let Ok(mut g) = self.policy.lock() {
            *g = p;
        }
    }

    /// Ensure the scheduler has active worker threads; restart if previously shut down.
    pub fn ensure_running(self: &Arc<Self>, num_threads: Option<usize>) {
        let mut threads = self.threads.lock().unwrap();
        if self.running.load(Ordering::SeqCst) && !threads.is_empty() {
            return;
        }
        // Mark running and (re)spawn workers
        self.running.store(true, Ordering::SeqCst);

        // Determine thread count
        let default_threads = core_affinity::get_core_ids()
            .map(|ids| ids.len())
            .unwrap_or_else(num_cpus::get);
        let n = num_threads.unwrap_or(default_threads).max(1);

        // Build local workers and stealers
        let mut local_workers: Vec<Worker<Job>> = Vec::with_capacity(n);
        let mut stealers = Vec::with_capacity(n);
        for _ in 0..n {
            let w = Worker::new_lifo();
            stealers.push(w.stealer());
            local_workers.push(w);
        }
        *self.stealers.lock().unwrap() = stealers;

        // Determine core pinning order
        let core_ids = build_numa_aware_core_order()
            .or_else(|| {
                core_affinity::get_core_ids().map(|ids| {
                    let mut order = Vec::with_capacity(ids.len());
                    for i in (0..ids.len()).step_by(2) {
                        order.push(ids[i]);
                    }
                    for i in (1..ids.len()).step_by(2) {
                        order.push(ids[i]);
                    }
                    order
                })
            })
            .map(Arc::new);

        // Spawn and store thread handles
        for (idx, worker) in local_workers.into_iter().enumerate() {
            let s = Arc::clone(self);
            let core_ids_cloned = core_ids.clone();
            let handle = thread::spawn(move || {
                if let Some(ids) = core_ids_cloned.as_ref() {
                    if let Some(core) = ids.get(idx % ids.len()) {
                        let _ = core_affinity::set_for_current(*core);
                    }
                }
                worker_loop(s, idx, worker)
            });
            threads.push(handle);
        }
    }
}

fn pop_or_steal<'a>(
    w: &'a Worker<Job>,
    stealers: &'a [Stealer<Job>],
    inj_high: &'a Injector<Job>,
    inj_norm: &'a Injector<Job>,
    inj_low: &'a Injector<Job>,
) -> Option<Job> {
    // 1) local
    if let Some(j) = w.pop() {
        return Some(j);
    }
    // 2) high -> normal -> low injectors
    if let Some(j) = inj_high.steal().success() {
        return Some(j);
    }
    if let Some(j) = inj_norm.steal().success() {
        return Some(j);
    }
    if let Some(j) = inj_low.steal().success() {
        return Some(j);
    }
    // 3) steal from others
    for s in stealers {
        match s.steal() {
            Steal::Success(j) => return Some(j),
            Steal::Retry => continue,
            Steal::Empty => {}
        }
    }
    None
}

fn worker_loop(s: Arc<Scheduler>, idx: usize, local: Worker<Job>) {
    // Clone stealers once; release the lock immediately to avoid blocking restarts.
    let stealers: Vec<Stealer<Job>> = {
        let stealers_locked = s.stealers.lock().unwrap();
        stealers_locked
            .iter()
            .enumerate()
            .filter_map(|(i, st)| if i == idx { None } else { Some(st.clone()) })
            .collect()
    };

    #[cfg(feature = "rt_zones")]
    let region: RtRegion<{ 64 * 1024 }> = RtRegion::new();

    let mut ctx = TaskCtx {
        #[cfg(feature = "rt_zones")]
        region,
    };

    while s.running.load(Ordering::SeqCst) {
        if let Some(mut job) = pop_or_steal(
            &local,
            &stealers,
            &s.inject_high,
            &s.inject_norm,
            &s.inject_low,
        ) {
            if let Some(f) = job.f.take() {
                let res = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| f(&mut ctx)));
                match res {
                    Ok(()) => {
                        #[cfg(feature = "rt_zones")]
                        unsafe {
                            ctx.region.reset();
                        }
                    }
                    Err(p) => {
                        // Try to extract a message using combinators
                        let msg = p
                            .downcast_ref::<&str>()
                            .map(|s| (*s).to_string())
                            .or_else(|| p.downcast_ref::<String>().cloned());
                        let err =
                            RuntimeError::Scheduler(SchedulerErrorKind::TaskPanic { message: msg });
                        report_error(&err);
                        let shutdown = {
                            if let Ok(g) = s.policy.lock() {
                                matches!(*g, RecoveryPolicy::ShutdownScheduler)
                            } else {
                                false
                            }
                        };
                        if shutdown {
                            s.running.store(false, Ordering::SeqCst);
                        }
                        // Nudge all workers to check queues after a panic
                        if let Ok(ths) = s.threads.lock() {
                            for h in ths.iter() {
                                h.thread().unpark();
                            }
                        }
                    }
                }
            }
            continue;
        }
        // Park briefly to avoid busy spin
        std::thread::park_timeout(std::time::Duration::from_micros(50));
    }
}
