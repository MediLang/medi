use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Condvar, Mutex};
use std::time::{Duration, Instant};

use crate::{RuntimeError, Scheduler, TaskCtx};

#[derive(Clone, Debug)]
pub struct CancellationToken {
    cancelled: Arc<AtomicBool>,
}

impl CancellationToken {
    pub fn new() -> Self {
        Self {
            cancelled: Arc::new(AtomicBool::new(false)),
        }
    }

    #[inline]
    pub fn cancel(&self) {
        self.cancelled.store(true, Ordering::SeqCst);
    }

    #[inline]
    pub fn is_cancelled(&self) -> bool {
        self.cancelled.load(Ordering::SeqCst)
    }
}

impl Default for CancellationToken {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Debug)]
pub struct TaskControl {
    token: CancellationToken,
    deadline: Option<Instant>,
}

impl TaskControl {
    #[inline]
    pub fn token(&self) -> &CancellationToken {
        &self.token
    }

    #[inline]
    pub fn deadline(&self) -> Option<Instant> {
        self.deadline
    }

    #[inline]
    pub fn is_cancelled(&self) -> bool {
        if self.token.is_cancelled() {
            return true;
        }
        if let Some(d) = self.deadline {
            return Instant::now() >= d;
        }
        false
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WaitError {
    Cancelled,
    Timeout,
}

struct WaitState {
    remaining: Mutex<usize>,
    cv: Condvar,
}

impl WaitState {
    fn new() -> Self {
        Self {
            remaining: Mutex::new(0),
            cv: Condvar::new(),
        }
    }

    fn inc(&self) {
        if let Ok(mut g) = self.remaining.lock() {
            *g += 1;
        }
    }

    fn dec(&self) {
        if let Ok(mut g) = self.remaining.lock() {
            *g = g.saturating_sub(1);
            if *g == 0 {
                self.cv.notify_all();
            }
        }
    }

    fn wait(&self, deadline: Option<Instant>) -> Result<(), WaitError> {
        let mut g = self.remaining.lock().map_err(|_| WaitError::Cancelled)?;
        loop {
            if *g == 0 {
                return Ok(());
            }
            if let Some(d) = deadline {
                let now = Instant::now();
                if now >= d {
                    return Err(WaitError::Timeout);
                }
                let to_wait = d.saturating_duration_since(now);
                let (ng, res) = self
                    .cv
                    .wait_timeout(g, to_wait)
                    .map_err(|_| WaitError::Cancelled)?;
                g = ng;
                if res.timed_out() {
                    return Err(WaitError::Timeout);
                }
            } else {
                g = self.cv.wait(g).map_err(|_| WaitError::Cancelled)?;
            }
        }
    }
}
pub struct TaskGroup {
    sched: Arc<Scheduler>,
    token: CancellationToken,
    deadline: Option<Instant>,
    wait_state: Arc<WaitState>,
    spawned: AtomicUsize,
}

impl TaskGroup {
    pub fn new(sched: Arc<Scheduler>) -> Self {
        Self {
            sched,
            token: CancellationToken::new(),
            deadline: None,
            wait_state: Arc::new(WaitState::new()),
            spawned: AtomicUsize::new(0),
        }
    }

    pub fn with_deadline(mut self, deadline: Instant) -> Self {
        self.deadline = Some(deadline);
        self
    }

    pub fn with_timeout(self, timeout: Duration) -> Self {
        self.with_deadline(Instant::now() + timeout)
    }

    #[inline]
    pub fn token(&self) -> CancellationToken {
        self.token.clone()
    }

    #[inline]
    pub fn cancel(&self) {
        self.token.cancel();
    }

    pub fn spawn<F>(&self, f: F)
    where
        F: FnOnce(&mut TaskCtx, &TaskControl) + Send + 'static,
    {
        self.wait_state.inc();
        self.spawned.fetch_add(1, Ordering::SeqCst);

        let token = self.token.clone();
        let deadline = self.deadline;
        let ws = self.wait_state.clone();

        self.sched.spawn(move |ctx| {
            let control = TaskControl { token, deadline };
            if !control.is_cancelled() {
                f(ctx, &control);
            }
            ws.dec();
        });
    }

    pub fn wait(&self) -> Result<(), WaitError> {
        self.wait_state.wait(self.deadline)
    }

    pub fn wait_with_timeout(&self, timeout: Duration) -> Result<(), WaitError> {
        self.wait_state.wait(Some(Instant::now() + timeout))
    }

    pub fn wait_or_cancel(&self) -> Result<(), WaitError> {
        match self.wait() {
            Ok(()) => Ok(()),
            Err(e) => {
                self.cancel();
                Err(e)
            }
        }
    }
}

impl From<WaitError> for RuntimeError {
    fn from(e: WaitError) -> Self {
        match e {
            WaitError::Cancelled => RuntimeError::Message("task group cancelled".into()),
            WaitError::Timeout => RuntimeError::Message("task group timeout".into()),
        }
    }
}
