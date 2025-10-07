//! Minimal runtime scaffolding for Medi: tasks and channels.
//! This is a host-side runtime for executing Medi programs in native targets.

use std::sync::mpsc::{self, Receiver, Sender};
use std::thread::{self, JoinHandle};

#[derive(Debug)]
pub struct Task {
    handle: JoinHandle<()>,
}

impl Task {
    pub fn join(self) -> std::thread::Result<()> { self.handle.join() }
}

#[derive(Debug)]
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
    pub fn send(&self, value: T) -> Result<(), mpsc::SendError<T>> { self.sender.send(value) }
}

impl<T> ReceiverHandle<T> {
    pub fn recv(&self) -> Result<T, mpsc::RecvError> { self.receiver.recv() }
    pub fn try_recv(&self) -> Result<T, mpsc::TryRecvError> { self.receiver.try_recv() }
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

// --- Feature-gated zones ---

#[cfg(feature = "gc")]
pub mod gc_zone {
    /// Stub for a safe GC zone. Future: track allocations, implement collection.
    #[derive(Debug, Default)]
    pub struct SafeGc;

    impl SafeGc {
        pub fn new() -> Self { Self }
        pub fn collect_garbage(&self) { /* no-op stub */ }
    }
}

#[cfg(feature = "rt_zones")]
pub mod rt_zone {
    /// Stub for a simplified real-time zone (no dynamic allocation policy).
    #[derive(Debug, Default)]
    pub struct RtZone;

    impl RtZone {
        pub fn new() -> Self { Self }
        pub fn enter(&self) { /* no-op stub */ }
        pub fn exit(&self) { /* no-op stub */ }
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
