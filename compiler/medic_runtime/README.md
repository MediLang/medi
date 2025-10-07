# Medi Runtime

Host-side runtime primitives for Medi programs: task-based parallelism and channel-based message passing. Optional memory-management zones are exposed behind feature flags and are currently stubs to establish forward-compatible ergonomics.

## Features

- **Tasks**
  - `spawn_task(F) -> Task`
  - `spawn_task_with_priority(Priority, F) -> Task`
  - `Task::join()` waits for task completion.
- **Channels**
  - `create_channel<T>() -> (SenderHandle<T>, ReceiverHandle<T>)`
  - `SenderHandle::send(T)`, `ReceiverHandle::recv()` / `try_recv()`.
- **Zones (feature-gated stubs)**
  - `gc` feature ⇒ `gc_zone::SafeGc` with `collect_garbage()`
  - `rt_zones` feature ⇒ `rt_zone::RtZone` with `enter()` / `exit()`

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

RT Zone (stub, behind feature flag):

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

## Notes

- Current implementation uses std threads and mpsc channels.
- Priority is a hint and currently a no-op; future versions may map to OS-specific scheduling.
- Zone APIs are stubs and may evolve without breaking call sites.
