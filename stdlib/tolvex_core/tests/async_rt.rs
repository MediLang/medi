use tolvex_core::{block_on, spawn_async};
use std::future::Future;
use std::pin::Pin;
use std::task::{Context, Poll};

struct YieldOnce {
    yielded: bool,
}

impl Future for YieldOnce {
    type Output = i32;

    fn poll(mut self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Self::Output> {
        if self.yielded {
            Poll::Ready(42)
        } else {
            self.yielded = true;
            Poll::Pending
        }
    }
}

#[test]
fn block_on_completes_pending_then_ready() {
    let v = block_on(YieldOnce { yielded: false });
    assert_eq!(v, 42);
}

#[test]
fn spawn_async_joins_result() {
    let h = spawn_async(async { 7usize });
    let v = h.join().unwrap();
    assert_eq!(v, 7);
}
