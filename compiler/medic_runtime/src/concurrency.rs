//! Structured concurrency, cancellation, and timeout utilities for the Medi runtime.
//! Integrates with the work-stealing scheduler to provide scoped task groups and
//! cooperative cancellation tokens.

use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Condvar, Mutex};
use std::thread;
use std::time::{Duration, Instant};

use crate::scheduler::{Priority, Scheduler, TaskCtx};

#[derive(Debug, Clone)]
pub struct CancellationToken {
    cancelled: Arc<AtomicBool>,
}

impl CancellationToken {
    /// Create a new cancellation token in the non-cancelled state.
    pub fn new() -> Self {
        Self {
            cancelled: Arc::new(AtomicBool::new(false)),
        }
    }

    /// Cancel the token. All checks will return true after this call.
    pub fn cancel(&self) {
        self.cancelled.store(true, Ordering::SeqCst);
    }

    /// Check if the token has been cancelled.
    pub fn is_cancelled(&self) -> bool {
        self.cancelled.load(Ordering::SeqCst)
    }

    /// Return early with a custom value if cancelled.
    pub fn check<T>(&self, value: T) -> Result<T, Cancelled> {
        if self.is_cancelled() {
            Err(Cancelled)
        } else {
            Ok(value)
        }
    }

    /// Block the current thread until the token is cancelled or the timeout elapses.
    pub fn wait_until_cancelled(&self, timeout: Duration) -> bool {
        let deadline = Instant::now() + timeout;
        while !self.is_cancelled() {
            if Instant::now() >= deadline {
                return false;
            }
            thread::yield_now();
        }
        true
    }
}

impl Default for CancellationToken {
    fn default() -> Self {
        Self::new()
    }
}

/// Error indicating a cancellation occurred.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Cancelled;

impl std::fmt::Display for Cancelled {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "operation cancelled")
    }
}

impl std::error::Error for Cancelled {}

/// A scoped task group that ensures all spawned tasks complete before the group is dropped.
/// Tasks can be spawned with a shared cancellation token.
pub struct TaskGroup {
    scheduler: Arc<Scheduler>,
    token: CancellationToken,
    pending: Arc<AtomicUsize>,
    join_state: Arc<JoinState>,
}

struct JoinState {
    mu: Mutex<()>,
    cv: Condvar,
}

impl JoinState {
    fn new() -> Self {
        Self {
            mu: Mutex::new(()),
            cv: Condvar::new(),
        }
    }

    fn notify_all(&self) {
        self.cv.notify_all();
    }

    fn wait_until_zero(&self, pending: &AtomicUsize, deadline: Option<Instant>) {
        let mut g = self.mu.lock().unwrap_or_else(|e| e.into_inner());
        loop {
            if pending.load(Ordering::SeqCst) == 0 {
                return;
            }
            if let Some(d) = deadline {
                let now = Instant::now();
                if now >= d {
                    return;
                }
                let to_wait = d.saturating_duration_since(now);
                let (ng, _res) = match self.cv.wait_timeout(g, to_wait) {
                    Ok(v) => v,
                    Err(e) => e.into_inner(),
                };
                g = ng;
            } else {
                g = self.cv.wait(g).unwrap_or_else(|e| e.into_inner());
            }
        }
    }
}

impl TaskGroup {
    /// Create a new task group bound to the given scheduler.
    pub fn new(scheduler: Arc<Scheduler>) -> Self {
        Self {
            scheduler,
            token: CancellationToken::new(),
            pending: Arc::new(AtomicUsize::new(0)),
            join_state: Arc::new(JoinState::new()),
        }
    }

    /// Return a handle to the shared cancellation token for this group.
    pub fn token(&self) -> CancellationToken {
        self.token.clone()
    }

    /// Spawn a task in this group with normal priority.
    pub fn spawn<F, R>(&self, f: F) -> TaskHandle<R>
    where
        F: FnOnce(&mut TaskCtx, CancellationToken) -> R + Send + 'static,
        R: Send + 'static,
    {
        self.spawn_with_priority(Priority::Normal, f)
    }

    /// Spawn a task in this group with a specific priority.
    pub fn spawn_with_priority<F, R>(&self, priority: Priority, f: F) -> TaskHandle<R>
    where
        F: FnOnce(&mut TaskCtx, CancellationToken) -> R + Send + 'static,
        R: Send + 'static,
    {
        let token = self.token.clone();
        let pending = self.pending.clone();
        let join_state = self.join_state.clone();
        pending.fetch_add(1, Ordering::SeqCst);

        let (tx, rx) = std::sync::mpsc::channel();
        self.scheduler.spawn_with_priority(priority, move |ctx| {
            let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| f(ctx, token.clone())));
            let _ = tx.send(result);
            pending.fetch_sub(1, Ordering::SeqCst);
            join_state.notify_all();
        });

        TaskHandle { receiver: rx }
    }

    /// Cancel all tasks in the group via the shared token.
    pub fn cancel_all(&self) {
        self.token.cancel();
    }

    /// Wait until all tasks in the group have completed.
    pub fn join(&self) {
        self.join_state
            .wait_until_zero(&self.pending, None);
    }

    pub fn join_timeout(&self, timeout: Duration) -> bool {
        let deadline = Instant::now() + timeout;
        self.join_state
            .wait_until_zero(&self.pending, Some(deadline));
        self.pending.load(Ordering::SeqCst) == 0
    }
}

impl Drop for TaskGroup {
    fn drop(&mut self) {
        self.join();
    }
}

/// A handle to a spawned task that can be used to await its result.
pub struct TaskHandle<T> {
    receiver: std::sync::mpsc::Receiver<std::thread::Result<T>>,
}

impl<T> TaskHandle<T> {
    /// Block until the task completes and return its result.
    /// Returns an error if the task panicked.
    pub fn join(self) -> Result<T, TaskPanic> {
        match self.receiver.recv() {
            Ok(Ok(v)) => Ok(v),
            Ok(Err(_)) => Err(TaskPanic),
            Err(_) => panic!("TaskHandle: sender dropped prematurely"),
        }
    }

    /// Try to read the result without blocking.
    pub fn try_join(&self) -> Result<Result<T, TaskPanic>, TryJoinError> {
        match self.receiver.try_recv() {
            Ok(Ok(v)) => Ok(Ok(v)),
            Ok(Err(_)) => Ok(Err(TaskPanic)),
            Err(std::sync::mpsc::TryRecvError::Empty) => Err(TryJoinError::NotReady),
            Err(std::sync::mpsc::TryRecvError::Disconnected) => {
                panic!("TaskHandle: sender dropped prematurely")
            }
        }
    }
}

/// Error indicating a task panicked.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TaskPanic;

impl std::fmt::Display for TaskPanic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "task panicked")
    }
}

impl std::error::Error for TaskPanic {}

/// Error when trying to join a task that hasn’t finished yet.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TryJoinError {
    NotReady,
}

impl std::fmt::Display for TryJoinError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TryJoinError::NotReady => write!(f, "task not ready"),
        }
    }
}

impl std::error::Error for TryJoinError {}

/// Run a closure with a timeout. If the timeout elapses, the cancellation token is cancelled.
pub fn with_timeout<F, R>(duration: Duration, f: F) -> Result<R, TimeoutElapsed>
where
    F: FnOnce(CancellationToken) -> R,
{
    let token = CancellationToken::new();
    let token_clone = token.clone();
    let (tx, rx) = std::sync::mpsc::channel();

    // Spawn a timeout thread that cancels the token after the duration.
    thread::spawn(move || {
        thread::sleep(duration);
        token_clone.cancel();
        let _ = tx.send(());
    });

    let result = f(token.clone());

    // If the timeout already fired, return an error.
    if rx.try_recv().is_ok() || token.is_cancelled() {
        return Err(TimeoutElapsed);
    }

    Ok(result)
}

/// Error indicating a timeout elapsed.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TimeoutElapsed;

impl std::fmt::Display for TimeoutElapsed {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "timeout elapsed")
    }
}

impl std::error::Error for TimeoutElapsed {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::init_scheduler;

    #[test]
    fn task_group_scope_and_join() {
        let sched = init_scheduler(Some(2));
        let group = TaskGroup::new(sched);
        let token = group.token();
        let handle = group.spawn(|_ctx, tok| {
            tok.check(42u32).unwrap()
        });
        assert!(!token.is_cancelled());
        group.join();
        assert_eq!(handle.join().unwrap(), 42);
    }

    #[test]
    fn cancellation_propagates() {
        let sched = init_scheduler(Some(2));
        let group = TaskGroup::new(sched);
        let token = group.token();
        let handle = group.spawn(|_ctx, tok| {
            while !tok.is_cancelled() {
                thread::yield_now();
            }
            99
        });
        token.cancel();
        group.join();
        assert_eq!(handle.join().unwrap(), 99);
    }

    #[test]
    fn with_timeout_cancels_token() {
        let result = with_timeout(Duration::from_millis(50), |tok| {
            while !tok.is_cancelled() {
                thread::yield_now();
            }
            123
        });
        assert_eq!(result, Err(TimeoutElapsed));
    }

    #[test]
    fn task_group_join_timeout() {
        let sched = init_scheduler(Some(2));
        let group = TaskGroup::new(sched);
        group.spawn(|_ctx, _tok| {
            thread::sleep(Duration::from_millis(50));
            1u32
        });

        assert!(!group.join_timeout(Duration::from_millis(1)));
        assert!(group.join_timeout(Duration::from_secs(2)));
    }
}
