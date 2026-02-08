use core::cmp::Ordering;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct StandardScaler {
    pub mean: f64,
    pub std: f64,
}

impl StandardScaler {
    pub fn fit(xs: &[f64]) -> Self {
        if xs.is_empty() {
            return Self {
                mean: 0.0,
                std: 0.0,
            };
        }
        let mean = xs.iter().sum::<f64>() / xs.len() as f64;
        let mut var = 0.0;
        for &x in xs {
            let d = x - mean;
            var += d * d;
        }
        var /= xs.len() as f64;
        let std = var.sqrt();
        Self { mean, std }
    }

    pub fn transform(&self, x: f64) -> f64 {
        if self.std == 0.0 {
            0.0
        } else {
            (x - self.mean) / self.std
        }
    }

    pub fn inverse_transform(&self, z: f64) -> f64 {
        if self.std == 0.0 {
            self.mean
        } else {
            z * self.std + self.mean
        }
    }

    pub fn transform_batch(&self, xs: &[f64]) -> Vec<f64> {
        xs.iter().map(|&x| self.transform(x)).collect()
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct LinearRegression1D {
    pub slope: f64,
    pub intercept: f64,
}

impl LinearRegression1D {
    pub fn fit(xs: &[f64], ys: &[f64]) -> Option<Self> {
        if xs.len() != ys.len() || xs.len() < 2 {
            return None;
        }

        let n = xs.len() as f64;
        let x_mean = xs.iter().sum::<f64>() / n;
        let y_mean = ys.iter().sum::<f64>() / n;

        let mut sxx = 0.0;
        let mut sxy = 0.0;
        for (&x, &y) in xs.iter().zip(ys.iter()) {
            let dx = x - x_mean;
            sxx += dx * dx;
            sxy += dx * (y - y_mean);
        }

        if sxx == 0.0 {
            return None;
        }

        let slope = sxy / sxx;
        let intercept = y_mean - slope * x_mean;
        Some(Self { slope, intercept })
    }

    pub fn predict(&self, x: f64) -> f64 {
        self.slope * x + self.intercept
    }

    pub fn predict_batch(&self, xs: &[f64]) -> Vec<f64> {
        xs.iter().map(|&x| self.predict(x)).collect()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LogisticRegression1D {
    pub w: f64,
    pub b: f64,
}

impl Default for LogisticRegression1D {
    fn default() -> Self {
        Self { w: 0.0, b: 0.0 }
    }
}

impl LogisticRegression1D {
    pub fn fit(xs: &[f64], ys: &[bool], lr: f64, steps: usize) -> Option<Self> {
        if xs.len() != ys.len() || xs.is_empty() {
            return None;
        }
        if lr.partial_cmp(&0.0) != Some(Ordering::Greater) || steps == 0 {
            return None;
        }

        let n = xs.len() as f64;
        let mut w = 0.0;
        let mut b = 0.0;

        for _ in 0..steps {
            let mut grad_w = 0.0;
            let mut grad_b = 0.0;

            for (&x, &y) in xs.iter().zip(ys.iter()) {
                let y_num = if y { 1.0 } else { 0.0 };
                let p = sigmoid(w * x + b);
                let diff = p - y_num;
                grad_w += diff * x;
                grad_b += diff;
            }

            grad_w /= n;
            grad_b /= n;

            w -= lr * grad_w;
            b -= lr * grad_b;
        }

        Some(Self { w, b })
    }

    pub fn predict_proba(&self, x: f64) -> f64 {
        sigmoid(self.w * x + self.b)
    }

    pub fn predict(&self, x: f64, threshold: f64) -> bool {
        self.predict_proba(x) >= threshold
    }
}

fn sigmoid(x: f64) -> f64 {
    1.0 / (1.0 + (-x).exp())
}
