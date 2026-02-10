use std::collections::VecDeque;

#[derive(Debug, Clone)]
pub struct RingBuffer<T> {
    buf: VecDeque<T>,
    cap: usize,
    dropped: usize,
}

impl<T> RingBuffer<T> {
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            buf: VecDeque::with_capacity(cap),
            cap,
            dropped: 0,
        }
    }

    pub fn push(&mut self, item: T) {
        if self.cap == 0 {
            self.dropped += 1;
            return;
        }

        if self.buf.len() == self.cap {
            self.buf.pop_front();
            self.dropped += 1;
        }
        self.buf.push_back(item);
    }

    pub fn pop(&mut self) -> Option<T> {
        self.buf.pop_front()
    }

    pub fn len(&self) -> usize {
        self.buf.len()
    }

    pub fn is_empty(&self) -> bool {
        self.buf.is_empty()
    }

    pub fn dropped(&self) -> usize {
        self.dropped
    }
}

pub fn sliding_window_sum(xs: &[f64], window: usize) -> Vec<f64> {
    if window == 0 || xs.len() < window {
        return Vec::new();
    }

    let mut out = Vec::with_capacity(xs.len() - window + 1);
    let mut sum = 0.0;

    for i in 0..xs.len() {
        sum += xs[i];
        if i >= window {
            sum -= xs[i - window];
        }
        if i + 1 >= window {
            out.push(sum);
        }
    }

    out
}
