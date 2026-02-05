use std::future::Future;
use std::pin::Pin;
use std::sync::{Arc, Condvar, Mutex};
use std::task::{Context, Poll, RawWaker, RawWakerVTable, Waker};
use std::thread;

fn noop_raw_waker() -> RawWaker {
    fn clone(_: *const ()) -> RawWaker {
        noop_raw_waker()
    }
    fn wake(_: *const ()) {}
    fn wake_by_ref(_: *const ()) {}
    fn drop(_: *const ()) {}

    static VTABLE: RawWakerVTable = RawWakerVTable::new(clone, wake, wake_by_ref, drop);
    RawWaker::new(std::ptr::null(), &VTABLE)
}

fn noop_waker() -> Waker {
    unsafe { Waker::from_raw(noop_raw_waker()) }
}

pub fn block_on<F: Future>(future: F) -> F::Output {
    let mut future = Box::pin(future);
    let waker = noop_waker();
    let mut cx = Context::from_waker(&waker);

    loop {
        match Future::poll(Pin::as_mut(&mut future), &mut cx) {
            Poll::Ready(v) => return v,
            Poll::Pending => thread::yield_now(),
        }
    }
}

#[derive(Clone)]
pub struct JoinHandle<T> {
    shared: Arc<Shared<T>>,
}

struct Shared<T> {
    state: Mutex<State<T>>,
    cv: Condvar,
}

enum State<T> {
    Running,
    Done(T),
    Panicked,
}

impl<T> JoinHandle<T> {
    pub fn join(self) -> Result<T, JoinError> {
        let mut g = self.shared.state.lock().map_err(|_| JoinError::Panicked)?;
        loop {
            match std::mem::replace(&mut *g, State::Running) {
                State::Running => {
                    *g = State::Running;
                    g = self.shared.cv.wait(g).map_err(|_| JoinError::Panicked)?;
                }
                State::Done(v) => return Ok(v),
                State::Panicked => return Err(JoinError::Panicked),
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JoinError {
    Panicked,
}

pub fn spawn_async<F>(fut: F) -> JoinHandle<F::Output>
where
    F: Future + Send + 'static,
    F::Output: Send + 'static,
{
    let shared = Arc::new(Shared {
        state: Mutex::new(State::Running),
        cv: Condvar::new(),
    });
    let shared2 = shared.clone();

    thread::spawn(move || {
        let res = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| block_on(fut)));
        if let Ok(mut g) = shared2.state.lock() {
            *g = match res {
                Ok(v) => State::Done(v),
                Err(_) => State::Panicked,
            };
            shared2.cv.notify_all();
        }
    });

    JoinHandle { shared }
}
