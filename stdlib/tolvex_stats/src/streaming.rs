//! Streaming/online statistics utilities

use crate::stable::Welford;

#[derive(Debug, Clone, Copy, Default)]
pub struct StreamingStats {
    w: Welford,
}

impl StreamingStats {
    pub fn new() -> Self {
        Self { w: Welford::new() }
    }
    pub fn update(&mut self, value: f64) {
        self.w.update(value);
    }
    pub fn count(&self) -> u64 {
        self.w.count()
    }
    pub fn mean(&self) -> f64 {
        self.w.mean()
    }
    pub fn variance(&self) -> f64 {
        self.w.variance_sample()
    }
    pub fn stddev(&self) -> f64 {
        self.w.stddev_sample()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn streaming_matches_batch() {
        let data = [1.0, 2.0, 3.0, 4.0, 5.0];
        let mut s = StreamingStats::new();
        for &x in &data {
            s.update(x);
        }
        let m = data.iter().sum::<f64>() / data.len() as f64;
        let v = data.iter().map(|x| (x - m).powi(2)).sum::<f64>() / (data.len() as f64 - 1.0);
        assert!((s.mean() - m).abs() < 1e-12);
        assert!((s.variance() - v).abs() < 1e-12);
    }
}
