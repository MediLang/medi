# Medi Runtime

Host-side runtime for Medi programs with task-based parallelism, channels, optional real-time memory primitives, and a unified error/diagnostics pathway.

## Features

- **Tasks**
  - `spawn_task(F) -> Task`
  - `spawn_task_with_priority(Priority, F) -> Task`
  - `Task::join()` waits for task completion.
- **Channels**
  - `create_channel<T>() -> (SenderHandle<T>, ReceiverHandle<T>)`
  - `SenderHandle::send(T)`, `ReceiverHandle::recv()` / `try_recv()`.
- **Zones (feature-gated)**
  - `rt_zones` ⇒ real-time primitives: `RtRegion`, `FixedPool`, and helpers.
  - `gc` ⇒ GC zone stubs for future integration.
- **Diagnostics**
   - Typed runtime errors with optional source-context (file, line, column, module, op).
   - Global reporter hook and multiple listeners for observability.

## Enabling Feature Flags

Add features in your Cargo manifest or pass them via CLI.

```toml
[dependencies]
medic_runtime = { path = "compiler/medic_runtime", features = ["gc", "rt_zones"] }
```

```bash
cargo test -p medic_runtime --features gc,rt_zones
```

## Quick Examples

Spawn a task and join:

```rust
use medic_runtime::{spawn_task};

let t = spawn_task(|| {
    // work here
});
t.join().unwrap();
```

Channel round-trip:

```rust
use medic_runtime::{create_channel, spawn_task};

let (tx, rx) = create_channel::<i32>();
let t = spawn_task(move || {
    tx.send(42).unwrap();
});
assert_eq!(rx.recv().unwrap(), 42);
t.join().unwrap();
```

GC Zone (stub, behind feature flag):

```rust
#[cfg(feature = "gc")]
{
    use medic_runtime::gc_zone::SafeGc;
    let gc = SafeGc::new();
    gc.collect_garbage(); // no-op stub
}
```

RT Zone (feature: rt_zones):

```rust
#[cfg(feature = "rt_zones")]
{
    use medic_runtime::rt_zone::RtZone;
    let rt = RtZone::new();
    rt.enter();
    // time-critical section
    rt.exit();
}
```

## Error Handling and Diagnostics

### Types

- `RuntimeError`: umbrella error for the runtime.
  - `Memory(MemoryErrorKind)` with cases like `RegionOverflow { requested, capacity }`, `PoolExhausted { capacity }`, `InvalidFree`, `DoubleFree { index }`.
  - `Scheduler(SchedulerErrorKind)` with `TaskPanic { message: Option<String> }`.
  - `Message(String)` generic message.
- `RuntimeDiagnostic`: lightweight display wrapper.
- `RecoveryPolicy`: `SkipTask` or `ShutdownScheduler` for panic handling.

### Reporting APIs

- `set_error_reporter(cb: fn(&RuntimeError))` minimal reporter (no context).
- `set_error_reporter_with_context(cb: fn(&RuntimeError, &RuntimeContext))` preferred for detailed diagnostics.
- `add_error_context_listener(cb: fn(&RuntimeError, &RuntimeContext))` add multiple observers (e.g., tests, metrics).
- Macro `medi_runtime_report!(err, "op_tag")` captures source context automatically (file/line/column/module/op) and reports via context-aware reporter when set, or falls back to minimal.

Common op tags emitted by the runtime:

- Memory/RT zones
  - `rt_region.alloc_uninit_overflow`
  - `rt_region.alloc_array_overflow`
  - `fixed_pool.alloc_exhausted`
  - `fixed_pool.invalid_free`
  - `fixed_pool.double_free`
- Scheduler
  - `scheduler.task_panic` (reported as a `RuntimeError::Scheduler`)
- Channels (on errors only)
  - `channel.send`, `channel.recv`, `channel.try_recv`
  - `xchan.send`, `xchan.try_send`, `xchan.recv`, `xchan.try_recv`
- GC
  - `gc.lock` when a GC mutex is poisoned

### Example: custom reporter

```rust
use medic_runtime::{
  set_error_reporter_with_context,
  RuntimeError, RuntimeDiagnostic,
};

fn main() {
  fn reporter(err: &RuntimeError, ctx: &medic_runtime::error::RuntimeContext) {
    let diag: RuntimeDiagnostic = err.into();
    eprintln!(
      "[medi-runtime] {} @{}:{}:{} op={}",
      diag.message, ctx.file, ctx.line, ctx.column, ctx.op
    );
  }
  set_error_reporter_with_context(reporter);
}
```

## Scheduler and Recovery

- Initialize: `init_scheduler(num_threads)` or `init_scheduler_with_policy(num_threads, RecoveryPolicy)`.
- Global handle is reused across calls; policy can be updated via `Scheduler::set_policy`.
- Panic handling uses `std::panic::catch_unwind`; on panic, logs via reporter and either skips the task or shuts down the scheduler depending on `RecoveryPolicy`.
- `Scheduler::ensure_running(num_threads)` restarts worker threads if previously shut down.

## Real-time Memory (feature: rt_zones)

- `RtRegion<const BYTES: usize>` bump allocator with `alloc_*` and `alloc_*_result` variants.
  - `*_result` variants report overflow with context via `medi_runtime_report!` and return `Result`.
- `FixedPool<T, const N: usize>` O(1) pool with `alloc` and `free_ptr` (via `PoolBox` for RAII).
  - Reports exhaustion, invalid frees, and double frees with context tags.

Example: detect exhaustion

```rust
use medic_runtime::{FixedPool, add_error_context_listener, RuntimeError};

let pool: FixedPool<u32, 1> = FixedPool::new();
add_error_context_listener(|_e: &RuntimeError, ctx| {
  eprintln!("op tag: {} in {}", ctx.op, ctx.module);
});
let _ = pool.alloc(1u32).unwrap();
let _ = pool.alloc(2u32); // reports fixed_pool.alloc_exhausted
```

## Performance

- Error handling overhead targeted to < 5%; context capture/reporting paths aim for < 1ms per event in worst cases.
- RT memory operations are O(1); tests include latency guards and IoT-like loops to keep per-op worst-case within budget.

## Notes

- Current implementation uses std threads and mpsc/crossbeam channels.
- Priority is a hint and currently a no-op; future versions may map to OS-specific scheduling.
- RT zone APIs are stable for current use; details may evolve while preserving ergonomics.
