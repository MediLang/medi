use std::fmt;
use std::sync::{Mutex, OnceLock};

/// Describes recoverability policy for runtime task failures.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RecoveryPolicy {
    /// Log and continue processing other work.
    SkipTask,
    /// Stop the scheduler after logging the error.
    ShutdownScheduler,
}

/// Memory-related runtime errors from RT zones or pools.
#[derive(Debug, Clone)]
pub enum MemoryErrorKind {
    /// Region bump allocation would exceed capacity.
    RegionOverflow { requested: usize, capacity: usize },
    /// Fixed pool has no free slots.
    PoolExhausted { capacity: usize },
    /// Free called more than once for the same slot.
    DoubleFree { index: usize },
    /// Free pointer did not map to pool storage.
    InvalidFree,
}

#[derive(Debug, Clone)]
pub enum SchedulerErrorKind {
    /// A task panicked. Message may be None if not downcastable.
    TaskPanic { message: Option<String> },
}

#[derive(Debug, Clone)]
pub enum RuntimeError {
    Memory(MemoryErrorKind),
    Scheduler(SchedulerErrorKind),
    /// Generic runtime error with a message.
    Message(String),
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RuntimeError::Memory(MemoryErrorKind::RegionOverflow {
                requested,
                capacity,
            }) => {
                write!(
                    f,
                    "region allocation of {requested} bytes exceeds capacity {capacity}"
                )
            }
            RuntimeError::Memory(MemoryErrorKind::PoolExhausted { capacity }) => {
                write!(f, "fixed pool exhausted (capacity {capacity})")
            }
            RuntimeError::Memory(MemoryErrorKind::DoubleFree { index }) => {
                write!(f, "double free detected for slot {index}")
            }
            RuntimeError::Memory(MemoryErrorKind::InvalidFree) => {
                write!(f, "attempted to free pointer not owned by pool")
            }
            RuntimeError::Scheduler(SchedulerErrorKind::TaskPanic { message }) => {
                if let Some(m) = message {
                    write!(f, "task panicked: {m}")
                } else {
                    write!(f, "task panicked")
                }
            }
            RuntimeError::Message(s) => write!(f, "{s}"),
        }
    }
}

/// A minimal diagnostic payload for runtime errors.
#[derive(Debug, Clone)]
pub struct RuntimeDiagnostic {
    pub message: String,
}

impl From<&RuntimeError> for RuntimeDiagnostic {
    fn from(e: &RuntimeError) -> Self {
        Self {
            message: e.to_string(),
        }
    }
}

/// Global error reporter hook. Very lightweight and optional.
static REPORTER: OnceLock<fn(&RuntimeError)> = OnceLock::new();
static REPORTER_CTX: OnceLock<fn(&RuntimeError, &RuntimeContext)> = OnceLock::new();
type CtxListener = fn(&RuntimeError, &RuntimeContext);
static CONTEXT_LISTENERS: OnceLock<Mutex<Vec<CtxListener>>> = OnceLock::new();

/// Install a global runtime error reporter callback.
/// The callback must be fast (<1ms) and non-panicking.
pub fn set_error_reporter(cb: fn(&RuntimeError)) {
    let _ = REPORTER.set(cb);
}

/// Report a runtime error via the global reporter when installed.
#[inline]
pub fn report_error(err: &RuntimeError) {
    if let Some(cb) = REPORTER.get().copied() {
        cb(err);
    } else {
        // Best-effort default logging with minimal overhead
        eprintln!("[medi-runtime] {err}");
    }
}

/// Source-context for runtime errors.
#[derive(Debug, Clone, Copy)]
pub struct RuntimeContext {
    pub file: &'static str,
    pub line: u32,
    pub column: u32,
    pub module: &'static str,
    pub op: &'static str,
}

/// Install a context-aware reporter callback. Preferred when detailed diagnostics are desired.
pub fn set_error_reporter_with_context(cb: fn(&RuntimeError, &RuntimeContext)) {
    let _ = REPORTER_CTX.set(cb);
}

/// Add an additional context listener; useful when multiple components/tests want to observe errors.
pub fn add_error_context_listener(cb: fn(&RuntimeError, &RuntimeContext)) {
    let list = CONTEXT_LISTENERS.get_or_init(|| Mutex::new(Vec::new()));
    if let Ok(mut v) = list.lock() {
        v.push(cb);
    }
}

/// Report an error with source context when a context-aware reporter is set;
/// falls back to the non-context reporter otherwise.
#[inline]
pub fn report_error_with_ctx(err: &RuntimeError, ctx: &RuntimeContext) {
    if let Some(cb) = REPORTER_CTX.get().copied() {
        cb(err, ctx);
    } else {
        report_error(err);
    }
    if let Some(list) = CONTEXT_LISTENERS.get() {
        if let Ok(guards) = list.lock() {
            for l in guards.iter() {
                l(err, ctx);
            }
        }
    }
}

/// Convenience macro to report an error with file/line/column/module/op captured at callsite.
#[macro_export]
macro_rules! tolvex_runtime_report {
    ($err:expr, $op:expr) => {{
        let ctx = $crate::error::RuntimeContext {
            file: file!(),
            line: line!() as u32,
            column: column!() as u32,
            module: module_path!(),
            op: $op,
        };
        $crate::error::report_error_with_ctx(&$err, &ctx);
    }};
}
