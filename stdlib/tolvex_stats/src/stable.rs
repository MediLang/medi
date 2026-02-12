//! Numerically stable algorithms: Welford variance and Kahan summation

/// Kahan compensated summation for improved floating-point accuracy
#[derive(Debug, Clone, Copy, Default)]
pub struct KahanSum {
    sum: f64,
    c: f64, // compensation
}

impl KahanSum {
    pub fn new() -> Self {
        Self { sum: 0.0, c: 0.0 }
    }
    pub fn add(&mut self, x: f64) {
        let y = x - self.c;
        let t = self.sum + y;
        self.c = (t - self.sum) - y;
        self.sum = t;
    }
    pub fn value(&self) -> f64 {
        self.sum
    }
}

/// Welford's online algorithm for mean and variance
#[derive(Debug, Clone, Copy, Default)]
pub struct Welford {
    n: u64,
    mean: f64,
    m2: f64,
}

impl Welford {
    pub fn new() -> Self {
        Self {
            n: 0,
            mean: 0.0,
            m2: 0.0,
        }
    }
    pub fn update(&mut self, x: f64) {
        self.n += 1;
        let n_f = self.n as f64;
        let delta = x - self.mean;
        self.mean += delta / n_f;
        let delta2 = x - self.mean;
        self.m2 += delta * delta2;
    }
    pub fn count(&self) -> u64 {
        self.n
    }
    pub fn mean(&self) -> f64 {
        self.mean
    }
    pub fn variance_sample(&self) -> f64 {
        if self.n < 2 {
            f64::NAN
        } else {
            self.m2 / ((self.n as f64) - 1.0)
        }
    }
    pub fn stddev_sample(&self) -> f64 {
        self.variance_sample().sqrt()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn kahan_reduces_error() {
        // Summing alternating small numbers can accumulate error
        let mut s_naive = 0.0;
        let mut ks = KahanSum::new();
        for _ in 0..1_000_000 {
            s_naive += 1e-8;
            ks.add(1e-8);
        }
        // Both should be close but Kahan should not be worse
        assert!((ks.value() - 0.01_f64).abs() < 1e-6);
        assert!((s_naive - 0.01_f64).abs() < 1e-4);
    }

    #[test]
    fn welford_matches_batch_stats() {
        let data = [1.0, 2.0, 3.0, 4.0];
        let mut w = Welford::new();
        for &x in &data {
            w.update(x);
        }
        let mean = data.iter().sum::<f64>() / data.len() as f64;
        let var = data.iter().map(|x| (x - mean).powi(2)).sum::<f64>() / (data.len() as f64 - 1.0);
        assert!((w.mean() - mean).abs() < 1e-12);
        assert!((w.variance_sample() - var).abs() < 1e-12);
    }
}
