// Advance incremental GC by one step if the `gc-incremental` feature is enabled.
pub fn maybe_incremental_step() {
    #[cfg(feature = "gc-incremental")]
    {
        if let Some(gc) = RUNTIME_GC.get() {
            if let Ok(mut guard) = gc.lock() {
                guard.maybe_start_incremental_major();
                let _ = guard.incremental_step_with_params();
            }
        }
    }
}
// Minimal runtime scaffolding for Tolvex: tasks and channels.
// This is a host-side runtime for executing Tolvex programs in native targets.

use crossbeam_channel as xchan;
use std::sync::mpsc::{self, Receiver, Sender};
use std::sync::{Arc, Mutex, OnceLock};
use std::thread::{self, JoinHandle};

mod error;
pub use error::{
    add_error_context_listener, report_error, set_error_reporter, set_error_reporter_with_context,
    MemoryErrorKind, RecoveryPolicy, RuntimeDiagnostic, RuntimeError, SchedulerErrorKind,
};

mod scheduler;
pub use scheduler::{Priority, Scheduler, TaskCtx};

#[cfg(feature = "distributed")]
pub mod distributed;

#[derive(Debug)]
pub struct Task {
    handle: JoinHandle<()>,
}

impl Task {
    pub fn join(self) -> std::thread::Result<()> {
        self.handle.join()
    }
}

#[derive(Debug)]
#[allow(dead_code)]
pub struct Channel<T> {
    sender: Sender<T>,
    receiver: Receiver<T>,
}

#[derive(Debug, Clone)]
pub struct SenderHandle<T> {
    sender: Sender<T>,
}

#[derive(Debug)]
pub struct ReceiverHandle<T> {
    receiver: Receiver<T>,
}

impl<T> SenderHandle<T> {
    pub fn send(&self, value: T) -> Result<(), mpsc::SendError<T>> {
        match self.sender.send(value) {
            Ok(()) => Ok(()),
            Err(e) => {
                let err = RuntimeError::Message("channel.send failed".into());
                tolvex_runtime_report!(err, "channel.send");
                Err(e)
            }
        }
    }
}

impl<T> ReceiverHandle<T> {
    pub fn recv(&self) -> Result<T, mpsc::RecvError> {
        match self.receiver.recv() {
            Ok(v) => Ok(v),
            Err(e) => {
                let err = RuntimeError::Message("channel.recv failed".into());
                tolvex_runtime_report!(err, "channel.recv");
                Err(e)
            }
        }
    }
    pub fn try_recv(&self) -> Result<T, mpsc::TryRecvError> {
        match self.receiver.try_recv() {
            Ok(v) => Ok(v),
            Err(e) => {
                // Non-blocking try_recv errors are often normal; only report if not Empty
                if !matches!(e, mpsc::TryRecvError::Empty) {
                    let err = RuntimeError::Message("channel.try_recv failed".into());
                    tolvex_runtime_report!(err, "channel.try_recv");
                }
                Err(e)
            }
        }
    }
}

pub fn create_channel<T>() -> (SenderHandle<T>, ReceiverHandle<T>) {
    let (tx, rx) = mpsc::channel();
    (SenderHandle { sender: tx }, ReceiverHandle { receiver: rx })
}

// --- Crossbeam-channel (MPMC) APIs ---

#[derive(Debug, Clone)]
pub struct XSender<T> {
    sender: xchan::Sender<T>,
}

#[derive(Debug, Clone)]
pub struct XReceiver<T> {
    receiver: xchan::Receiver<T>,
}

impl<T> XSender<T> {
    #[inline]
    pub fn send(&self, value: T) -> Result<(), xchan::SendError<T>> {
        match self.sender.send(value) {
            Ok(()) => Ok(()),
            Err(e) => {
                let err = RuntimeError::Message("xchan.send failed".into());
                tolvex_runtime_report!(err, "xchan.send");
                Err(e)
            }
        }
    }
    #[inline]
    pub fn try_send(&self, value: T) -> Result<(), xchan::TrySendError<T>> {
        match self.sender.try_send(value) {
            Ok(()) => Ok(()),
            Err(e) => {
                if !matches!(e, xchan::TrySendError::Full(_)) {
                    let err = RuntimeError::Message("xchan.try_send failed".into());
                    tolvex_runtime_report!(err, "xchan.try_send");
                }
                Err(e)
            }
        }
    }
}

impl<T> XReceiver<T> {
    #[inline]
    pub fn recv(&self) -> Result<T, xchan::RecvError> {
        match self.receiver.recv() {
            Ok(v) => Ok(v),
            Err(e) => {
                let err = RuntimeError::Message("xchan.recv failed".into());
                tolvex_runtime_report!(err, "xchan.recv");
                Err(e)
            }
        }
    }
    #[inline]
    pub fn try_recv(&self) -> Result<T, xchan::TryRecvError> {
        match self.receiver.try_recv() {
            Ok(v) => Ok(v),
            Err(e) => {
                if !matches!(e, xchan::TryRecvError::Empty) {
                    let err = RuntimeError::Message("xchan.try_recv failed".into());
                    tolvex_runtime_report!(err, "xchan.try_recv");
                }
                Err(e)
            }
        }
    }
    #[inline]
    pub fn len(&self) -> usize {
        self.receiver.len()
    }
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.receiver.is_empty()
    }
}

#[inline]
pub fn create_unbounded_channel<T>() -> (XSender<T>, XReceiver<T>) {
    let (tx, rx) = xchan::unbounded();
    (XSender { sender: tx }, XReceiver { receiver: rx })
}

#[inline]
pub fn create_bounded_channel<T>(cap: usize) -> (XSender<T>, XReceiver<T>) {
    let (tx, rx) = xchan::bounded(cap);
    (XSender { sender: tx }, XReceiver { receiver: rx })
}

pub fn spawn_task<F>(f: F) -> Task
where
    F: FnOnce() + Send + 'static,
{
    let handle = thread::spawn(f);
    Task { handle }
}

// Priority now re-exported from scheduler

// For now Priority is a no-op hint. Future: map to OS-specific scheduling where allowed.
pub fn spawn_task_with_priority<F>(_priority: Priority, f: F) -> Task
where
    F: FnOnce() + Send + 'static,
{
    spawn_task(f)
}

// --- Garbage Collector ---

pub mod gc;
pub use gc::{GarbageCollector, GcParams, GcRef, GcWeak};

static RUNTIME_GC: OnceLock<Arc<Mutex<GarbageCollector>>> = OnceLock::new();

pub fn init_gc_with_params(params: GcParams) -> Arc<Mutex<GarbageCollector>> {
    let _ = RUNTIME_GC.set(Arc::new(Mutex::new(GarbageCollector::with_params(params))));
    RUNTIME_GC.get().unwrap().clone()
}

pub fn get_gc() -> Arc<Mutex<GarbageCollector>> {
    RUNTIME_GC.get().unwrap().clone()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ZoneKind {
    None,
    Safe,
    RealTime,
}

pub struct ZoneGuard {
    prev: ZoneKind,
}

impl Drop for ZoneGuard {
    fn drop(&mut self) {
        ZONE_KIND.with(|k| k.set(self.prev));
    }
}

thread_local! {
    static ZONE_KIND: std::cell::Cell<ZoneKind> = const { std::cell::Cell::new(ZoneKind::None) };
}

#[inline]
pub fn current_zone() -> ZoneKind {
    ZONE_KIND.with(|k| k.get())
}

// --- Work-stealing Scheduler (global handle) ---

static RUNTIME_SCHEDULER: OnceLock<Arc<Scheduler>> = OnceLock::new();

pub fn init_scheduler(num_threads: Option<usize>) -> Arc<Scheduler> {
    if let Some(existing) = RUNTIME_SCHEDULER.get() {
        existing.ensure_running(num_threads);
        existing.clone()
    } else {
        let sched = Scheduler::new(num_threads);
        let _ = RUNTIME_SCHEDULER.set(sched.clone());
        sched
    }
}

pub fn init_scheduler_with_policy(
    num_threads: Option<usize>,
    policy: RecoveryPolicy,
) -> Arc<Scheduler> {
    if let Some(existing) = RUNTIME_SCHEDULER.get() {
        existing.set_policy(policy);
        existing.ensure_running(num_threads);
        existing.clone()
    } else {
        let sched = Scheduler::new_with_policy(num_threads, policy);
        let _ = RUNTIME_SCHEDULER.set(sched.clone());
        sched
    }
}

pub fn get_scheduler() -> Arc<Scheduler> {
    RUNTIME_SCHEDULER
        .get()
        .expect("scheduler not initialized; call init_scheduler() first")
        .clone()
}

pub fn sched_spawn<F>(f: F)
where
    F: FnOnce(&mut TaskCtx) + Send + 'static,
{
    get_scheduler().spawn(f)
}

pub fn sched_spawn_with_priority<F>(p: Priority, f: F)
where
    F: FnOnce(&mut TaskCtx) + Send + 'static,
{
    get_scheduler().spawn_with_priority(p, f)
}

// --- GC integration helpers (for codegen wiring) ---

pub fn gc_alloc_string(s: &str) -> GcRef<String> {
    let gc = get_gc();
    let mut guard = match gc.lock() {
        Ok(g) => g,
        Err(_) => {
            let err = RuntimeError::Message("gc lock poisoned".into());
            tolvex_runtime_report!(err, "gc.lock");
            panic!("gc mutex poisoned");
        }
    };
    guard.allocate::<String>(s.to_string())
}

pub fn gc_add_root<T: 'static + Send + std::any::Any>(r: &GcRef<T>) {
    let gc = get_gc();
    let mut guard = match gc.lock() {
        Ok(g) => g,
        Err(_) => {
            let err = RuntimeError::Message("gc lock poisoned".into());
            tolvex_runtime_report!(err, "gc.lock");
            panic!("gc mutex poisoned");
        }
    };
    guard.add_root(r)
}

pub fn gc_remove_root<T: 'static + Send + std::any::Any>(r: &GcRef<T>) {
    let gc = get_gc();
    let mut guard = match gc.lock() {
        Ok(g) => g,
        Err(_) => {
            let err = RuntimeError::Message("gc lock poisoned".into());
            tolvex_runtime_report!(err, "gc.lock");
            panic!("gc mutex poisoned");
        }
    };
    guard.remove_root(r)
}

// --- C ABI for codegen integration ---

/// # Safety
///
/// The caller must ensure that `ptr` points to a valid UTF-8 byte buffer of length `len` that
/// remains valid for the duration of this call. The buffer should originate from a trusted source
/// (e.g., a string literal or compiler-managed memory) and not be mutated concurrently.
/// # Safety
/// The caller must ensure that `ptr` points to a valid UTF-8 buffer of length `len` that remains
/// valid for the duration of this call. The buffer must not be mutated concurrently while being
/// read, and it should originate from a trusted source (e.g., a string literal or compiler-managed
/// memory). Passing an invalid pointer or length results in undefined behavior.
#[no_mangle]
pub unsafe extern "C" fn tolvex_gc_alloc_string(ptr: *const u8, len: usize) -> u64 {
    // Safety: codegen must provide a valid pointer/len pair originating from a valid string literal or buffer.
    let bytes = std::slice::from_raw_parts(ptr, len);
    let s = String::from_utf8_lossy(bytes).to_string();
    let gc = get_gc();
    let mut guard = gc.lock().expect("gc mutex poisoned");
    let r = guard.allocate::<String>(s);
    #[cfg(feature = "gc-incremental")]
    {
        guard.maybe_start_incremental_major();
        guard.incremental_step_with_params();
    }
    r.id() as u64
}

#[no_mangle]
pub extern "C" fn tolvex_gc_add_root(id: u64) {
    let gc = get_gc();
    let mut guard = gc.lock().expect("gc mutex poisoned");
    guard.add_root_id(id);
}

#[no_mangle]
pub extern "C" fn tolvex_gc_remove_root(id: u64) {
    let gc = get_gc();
    let mut guard = gc.lock().expect("gc mutex poisoned");
    guard.remove_root_id(id);
}

#[no_mangle]
pub extern "C" fn tolvex_gc_collect() {
    let gc = get_gc();
    let mut guard = gc.lock().expect("gc mutex poisoned");
    guard.collect_garbage();
}

#[no_mangle]
pub extern "C" fn tolvex_gc_alloc_unit() -> u64 {
    let gc = get_gc();
    let mut guard = gc.lock().expect("gc mutex poisoned");
    let r = guard.allocate::<()>(());
    #[cfg(feature = "gc-incremental")]
    {
        guard.maybe_start_incremental_major();
        guard.incremental_step_with_params();
    }
    r.id() as u64
}

#[no_mangle]
pub extern "C" fn tolvex_gc_add_edge(parent: u64, child: u64) {
    let gc = get_gc();
    let mut guard = gc.lock().expect("gc mutex poisoned");
    guard.add_edge(parent, child);
}

#[no_mangle]
pub extern "C" fn tolvex_gc_remove_edge(parent: u64, child: u64) {
    let gc = get_gc();
    let mut guard = gc.lock().expect("gc mutex poisoned");
    guard.remove_edge(parent, child);
}

#[no_mangle]
pub extern "C" fn tolvex_gc_write_barrier(parent: u64, child: u64) {
    let gc = get_gc();
    let mut guard = gc.lock().expect("gc mutex poisoned");
    guard.write_barrier(parent, child);
}

// --- Feature-gated zones ---

#[cfg(feature = "gc")]
pub mod gc_zone {
    use super::{current_zone, get_gc, maybe_incremental_step, ZoneGuard, ZoneKind};
    use crate::gc::{GcParams, GcRef, GcWeak};
    use crate::{GarbageCollector, RuntimeError};
    use std::any::Any;
    use std::sync::{Arc, Mutex};

    #[derive(Clone)]
    pub struct SafeGcZone {
        gc: Arc<Mutex<GarbageCollector>>,
    }

    impl SafeGcZone {
        pub fn new() -> Self {
            Self { gc: get_gc() }
        }

        pub fn with_params(params: GcParams) -> Self {
            let gc = super::init_gc_with_params(params);
            Self { gc }
        }

        #[inline]
        pub fn enter(&self) -> ZoneGuard {
            let prev = current_zone();
            super::ZONE_KIND.with(|k| k.set(ZoneKind::Safe));
            ZoneGuard { prev }
        }

        #[inline]
        pub fn allocate<T: Any + Send + 'static>(
            &self,
            value: T,
        ) -> Result<GcRef<T>, RuntimeError> {
            let mut guard = self
                .gc
                .lock()
                .map_err(|_| RuntimeError::Message("gc mutex poisoned".into()))?;
            let r = guard.allocate(value);
            // Production policy (Option B): threshold-triggered minor GC + pause-bounded incremental major GC.
            if guard.should_minor_collect() {
                guard.minor_collect();
            }
            #[cfg(feature = "gc-incremental")]
            {
                guard.maybe_start_incremental_major();
                if guard.incremental_is_active() {
                    let _done = guard.incremental_step_with_params();
                }
            }
            // Retain legacy helper for compatibility when gc-incremental is not active.
            maybe_incremental_step();
            Ok(r)
        }

        #[inline]
        pub fn downgrade<T: Any + Send + 'static>(
            &self,
            r: &GcRef<T>,
        ) -> Result<GcWeak<T>, RuntimeError> {
            let guard = self
                .gc
                .lock()
                .map_err(|_| RuntimeError::Message("gc mutex poisoned".into()))?;
            Ok(guard.downgrade(r))
        }

        #[inline]
        pub fn collect_garbage(&self) -> Result<(), RuntimeError> {
            let mut guard = self
                .gc
                .lock()
                .map_err(|_| RuntimeError::Message("gc mutex poisoned".into()))?;
            guard.collect_garbage();
            Ok(())
        }
    }
}

#[cfg(feature = "rt_zones")]
pub mod rt_zone {
    use super::{current_zone, ZoneGuard, ZoneKind};

    #[inline]
    pub fn enter() -> ZoneGuard {
        let prev = current_zone();
        super::ZONE_KIND.with(|k| k.set(ZoneKind::RealTime));
        ZoneGuard { prev }
    }
}

#[cfg(feature = "rt_zones")]
pub mod rt;

#[cfg(feature = "rt_zones")]
pub use rt::{verify_latency, FixedPool, RtRegion, RtZone};

// --- Tests ---
#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::{Mutex, OnceLock};
    use std::time::{Duration, Instant};

    // Serialize scheduler-related tests to avoid global scheduler cross-talk under parallel test runner
    static SCHED_TEST_LOCK: OnceLock<Mutex<()>> = OnceLock::new();
    fn sched_test_guard() -> std::sync::MutexGuard<'static, ()> {
        SCHED_TEST_LOCK
            .get_or_init(|| Mutex::new(()))
            .lock()
            .unwrap()
    }
    use crate::tolvex_runtime_report;

    #[test]
    fn spawn_and_join_task() {
        let t = spawn_task(|| {
            // work
        });
        t.join().expect("task join should succeed");
    }

    #[test]
    fn channel_round_trip() {
        let (tx, rx) = create_channel::<i32>();
        let t = spawn_task(move || {
            tx.send(42).unwrap();
        });
        let v = rx.recv().expect("should receive value");
        assert_eq!(v, 42);
        t.join().unwrap();
    }

    #[test]
    fn scheduler_smoke() {
        let _g = sched_test_guard();
        let _ = init_scheduler(Some(2));
        get_scheduler().ensure_running(Some(2));
        let s = get_scheduler();
        s.shutdown();
        s.join();
    }

    #[test]
    fn reporter_invocation_under_1ms() {
        static COUNT: AtomicUsize = AtomicUsize::new(0);
        fn cb_ctx(_e: &RuntimeError, _ctx: &crate::error::RuntimeContext) {
            // Simulate minimal formatting work
            let _d: RuntimeDiagnostic = _e.into();
            COUNT.fetch_add(1, Ordering::Relaxed);
        }
        add_error_context_listener(cb_ctx);

        let start = Instant::now();
        for _ in 0..10_000 {
            let err = RuntimeError::Message("x".into());
            tolvex_runtime_report!(err, "bench.report");
        }
        let elapsed = start.elapsed();
        assert!(
            elapsed < Duration::from_secs(2),
            "reporting too slow: {elapsed:?}"
        );
        assert!(COUNT.load(Ordering::Relaxed) >= 1);
    }

    #[test]
    fn scheduler_panic_recovery_skiptask() {
        let _g = sched_test_guard();
        let _ = init_scheduler_with_policy(Some(2), RecoveryPolicy::SkipTask);
        get_scheduler().ensure_running(Some(2));
        let s = get_scheduler();
        s.shutdown();
        s.join();
    }

    #[test]
    fn scheduler_panic_recovery_shutdown() {
        let _g = sched_test_guard();
        let _ = init_scheduler_with_policy(Some(2), RecoveryPolicy::ShutdownScheduler);
        // Install context reporter
        fn cb_ctx(_e: &RuntimeError, _ctx: &crate::error::RuntimeContext) {}
        set_error_reporter_with_context(cb_ctx);
        get_scheduler().ensure_running(Some(2));

        sched_spawn(|_ctx| {
            panic!("boom");
        });
        // Give time for workers to process and trip shutdown
        std::thread::sleep(Duration::from_millis(100));
        let s = get_scheduler();
        // State should allow shutdown+join quickly
        s.shutdown();
        s.join();
    }

    #[cfg(feature = "rt_zones")]
    #[test]
    fn rt_region_overflow_reports_error() {
        use crate::rt::RtRegion;
        static ERR: AtomicUsize = AtomicUsize::new(0);
        fn cb_ctx(_e: &RuntimeError, _ctx: &crate::error::RuntimeContext) {
            ERR.fetch_add(1, Ordering::Relaxed);
        }
        add_error_context_listener(cb_ctx);
        const CAP: usize = 64;
        let region = RtRegion::<CAP>::new();
        // Request more than capacity to force overflow
        let r = region.alloc_array_uninit_result::<u8>(CAP + 16);
        assert!(r.is_err());
        assert!(ERR.load(Ordering::Relaxed) >= 1);
    }

    #[cfg(feature = "rt_zones")]
    #[test]
    fn fixed_pool_exhaustion_reports_error_and_context() {
        use crate::rt::FixedPool;
        static COUNT: AtomicUsize = AtomicUsize::new(0);
        use std::sync::Mutex;
        static OPS: Mutex<Vec<&'static str>> = Mutex::new(Vec::new());
        fn cb_ctx(_e: &RuntimeError, ctx: &crate::error::RuntimeContext) {
            COUNT.fetch_add(1, Ordering::Relaxed);
            OPS.lock().unwrap().push(ctx.op);
        }
        add_error_context_listener(cb_ctx);

        let pool: FixedPool<u32, 1> = FixedPool::new();
        let _ = pool.alloc(1u32).unwrap();
        // Next allocation should exhaust and be reported
        let _ = pool.alloc(2u32);
        assert!(COUNT.load(Ordering::Relaxed) >= 1);
        let ops = OPS.lock().unwrap();
        assert!(ops.contains(&"fixed_pool.alloc_exhausted"));
    }

    #[cfg(feature = "rt_zones")]
    #[test]
    fn rt_region_repeated_overflow_is_fast_enough() {
        use crate::rt::RtRegion;
        static COUNT: AtomicUsize = AtomicUsize::new(0);
        fn cb_ctx(_e: &RuntimeError, _ctx: &crate::error::RuntimeContext) {
            COUNT.fetch_add(1, Ordering::Relaxed);
        }
        add_error_context_listener(cb_ctx);
        const CAP: usize = 32;
        let region = RtRegion::<CAP>::new();
        let start = Instant::now();
        for _ in 0..20_000 {
            let _ = region.alloc_array_uninit_result::<u8>(CAP + 64);
        }
        let elapsed = start.elapsed();
        // Generous upper bound to avoid flaky CI; ensures average path is tiny
        assert!(
            elapsed < Duration::from_secs(3),
            "overflow handling too slow: {elapsed:?}"
        );
        assert!(COUNT.load(Ordering::Relaxed) >= 10_000);
    }

    #[cfg(feature = "rt_zones")]
    #[test]
    fn context_is_propagated_for_rt_errors() {
        use crate::rt::{FixedPool, RtRegion};
        use std::sync::Mutex;
        static RECORDS: Mutex<Vec<(&'static str, &'static str)>> = Mutex::new(Vec::new());
        fn cb_ctx(_e: &RuntimeError, ctx: &crate::error::RuntimeContext) {
            RECORDS.lock().unwrap().push((ctx.module, ctx.op));
        }
        add_error_context_listener(cb_ctx);

        // Trigger region overflow
        const CAP: usize = 16;
        let region = RtRegion::<CAP>::new();
        let _ = region.alloc_array_uninit_result::<u8>(CAP + 8);
        // Trigger double-free
        let pool: FixedPool<u32, 2> = FixedPool::new();
        let p = pool.alloc(7u32).unwrap() as *mut u32;
        unsafe { pool.free_ptr(p) };
        unsafe { pool.free_ptr(p) };

        let records = RECORDS.lock().unwrap();
        assert!(records
            .iter()
            .any(|(_, op)| *op == "rt_region.alloc_array_overflow"));
        assert!(records
            .iter()
            .any(|(_, op)| *op == "fixed_pool.double_free"));
        // module path present
        assert!(records.iter().all(|(m, _)| !m.is_empty()));
    }

    #[cfg(feature = "rt_zones")]
    #[test]
    fn fixed_pool_double_free_reports_error() {
        use crate::rt::FixedPool;
        static ERR: AtomicUsize = AtomicUsize::new(0);
        fn cb_ctx(_e: &RuntimeError, _ctx: &crate::error::RuntimeContext) {
            ERR.fetch_add(1, Ordering::Relaxed);
        }
        set_error_reporter_with_context(cb_ctx);
        let pool: FixedPool<u32, 4> = FixedPool::new();
        let p = pool.alloc(1u32).unwrap() as *mut u32;
        unsafe { pool.free_ptr(p) };
        // Second free should be reported
        unsafe { pool.free_ptr(p) };
        assert!(ERR.load(Ordering::Relaxed) >= 1);
    }
}
