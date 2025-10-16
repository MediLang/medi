/// Advance incremental GC by one step if the `gc-incremental` feature is enabled.
pub fn maybe_incremental_step() {
    #[cfg(feature = "gc-incremental")]
    {
        if let Some(gc) = RUNTIME_GC.get() {
            if let Ok(mut guard) = gc.lock() {
                let _ = guard.incremental_step_with_params();
            }
        }
    }
}
// Minimal runtime scaffolding for Medi: tasks and channels.
// This is a host-side runtime for executing Medi programs in native targets.

use std::sync::mpsc::{self, Receiver, Sender};
use std::sync::{Arc, Mutex, OnceLock};
use std::thread::{self, JoinHandle};

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
        self.sender.send(value)
    }
}

impl<T> ReceiverHandle<T> {
    pub fn recv(&self) -> Result<T, mpsc::RecvError> {
        self.receiver.recv()
    }
    pub fn try_recv(&self) -> Result<T, mpsc::TryRecvError> {
        self.receiver.try_recv()
    }
}

pub fn create_channel<T>() -> (SenderHandle<T>, ReceiverHandle<T>) {
    let (tx, rx) = mpsc::channel();
    (SenderHandle { sender: tx }, ReceiverHandle { receiver: rx })
}

pub fn spawn_task<F>(f: F) -> Task
where
    F: FnOnce() + Send + 'static,
{
    let handle = thread::spawn(f);
    Task { handle }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Priority {
    Low,
    Normal,
    High,
}

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

// --- GC integration helpers (for codegen wiring) ---

pub fn gc_alloc_string(s: &str) -> GcRef<String> {
    let gc = get_gc();
    let mut guard = gc.lock().expect("gc mutex poisoned");
    guard.allocate::<String>(s.to_string())
}

pub fn gc_add_root<T: 'static + Send + std::any::Any>(r: &GcRef<T>) {
    let gc = get_gc();
    let mut guard = gc.lock().expect("gc mutex poisoned");
    guard.add_root(r)
}

pub fn gc_remove_root<T: 'static + Send + std::any::Any>(r: &GcRef<T>) {
    let gc = get_gc();
    let mut guard = gc.lock().expect("gc mutex poisoned");
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
pub unsafe extern "C" fn medi_gc_alloc_string(ptr: *const u8, len: usize) -> u64 {
    // Safety: codegen must provide a valid pointer/len pair originating from a valid string literal or buffer.
    let bytes = std::slice::from_raw_parts(ptr, len);
    let s = String::from_utf8_lossy(bytes).to_string();
    let gc = get_gc();
    let mut guard = gc.lock().expect("gc mutex poisoned");
    let r = guard.allocate::<String>(s);
    #[cfg(feature = "gc-incremental")]
    {
        guard.incremental_step_with_params();
    }
    r.id() as u64
}

#[no_mangle]
pub extern "C" fn medi_gc_add_root(id: u64) {
    let gc = get_gc();
    let mut guard = gc.lock().expect("gc mutex poisoned");
    guard.add_root_id(id);
}

#[no_mangle]
pub extern "C" fn medi_gc_remove_root(id: u64) {
    let gc = get_gc();
    let mut guard = gc.lock().expect("gc mutex poisoned");
    guard.remove_root_id(id);
}

#[no_mangle]
pub extern "C" fn medi_gc_collect() {
    let gc = get_gc();
    let mut guard = gc.lock().expect("gc mutex poisoned");
    guard.collect_garbage();
}

#[no_mangle]
pub extern "C" fn medi_gc_alloc_unit() -> u64 {
    let gc = get_gc();
    let mut guard = gc.lock().expect("gc mutex poisoned");
    let r = guard.allocate::<()>(());
    #[cfg(feature = "gc-incremental")]
    {
        guard.incremental_step_with_params();
    }
    r.id() as u64
}

#[no_mangle]
pub extern "C" fn medi_gc_add_edge(parent: u64, child: u64) {
    let gc = get_gc();
    let mut guard = gc.lock().expect("gc mutex poisoned");
    guard.add_edge(parent, child);
}

#[no_mangle]
pub extern "C" fn medi_gc_remove_edge(parent: u64, child: u64) {
    let gc = get_gc();
    let mut guard = gc.lock().expect("gc mutex poisoned");
    guard.remove_edge(parent, child);
}

#[no_mangle]
pub extern "C" fn medi_gc_write_barrier(parent: u64, child: u64) {
    let gc = get_gc();
    let mut guard = gc.lock().expect("gc mutex poisoned");
    guard.write_barrier(parent, child);
}

// --- Feature-gated zones ---

#[cfg(feature = "gc")]
pub mod gc_zone {
    /// Stub for a safe GC zone. Future: track allocations, implement collection.
    #[derive(Debug, Default)]
    pub struct SafeGc;

    impl SafeGc {
        pub fn new() -> Self {
            Self
        }
        pub fn collect_garbage(&self) { /* no-op stub */
        }
    }
}

#[cfg(feature = "rt_zones")]
pub mod rt_zone {
    /// Stub for a simplified real-time zone (no dynamic allocation policy).
    #[derive(Debug, Default)]
    pub struct RtZone;

    impl RtZone {
        pub fn new() -> Self {
            Self
        }
        pub fn enter(&self) { /* no-op stub */
        }
        pub fn exit(&self) { /* no-op stub */
        }
    }
}

// --- Tests ---
#[cfg(test)]
mod tests {
    use super::*;

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
}
