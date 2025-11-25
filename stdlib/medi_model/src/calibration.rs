//! Calibration utilities: Platt scaling and isotonic regression (stub)

/// Logistic function
fn sigmoid(z: f64) -> f64 {
    1.0 / (1.0 + (-z).exp())
}

/// Platt scaling: p = sigmoid(a * s + b)
#[derive(Debug, Clone, Copy)]
pub struct PlattScaling {
    pub a: f64,
    pub b: f64,
}

impl PlattScaling {
    /// Fit Platt scaling parameters using simple gradient descent on log-loss.
    ///
    /// - `predictions`: model scores (logits or probabilities in [0,1])
    /// - `labels`: ground-truth booleans (true=1, false=0)
    ///
    /// Note: For robustness, we clamp inputs when used as probabilities.
    pub fn fit(predictions: &[f64], labels: &[bool]) -> Self {
        assert_eq!(predictions.len(), labels.len());
        let n = predictions.len().max(1) as f64;

        // Initialize a, b. If predictions look like probabilities, start at identity.
        let mut a = 1.0f64;
        let mut b = 0.0f64;

        // Learning rate and iterations (small and safe for stability)
        let lr = 0.1f64;
        let iters = 500usize;

        for _ in 0..iters {
            let mut grad_a = 0.0f64;
            let mut grad_b = 0.0f64;
            for (s, &y) in predictions.iter().zip(labels.iter()) {
                // Use score as-is; if looks like probability, map to pseudo-logit by identity
                let score = *s;
                let y_f = if y { 1.0 } else { 0.0 };
                let p = sigmoid(a * score + b);
                // d/d(a): (p - y) * score; d/d(b): (p - y)
                let diff = p - y_f;
                grad_a += diff * score;
                grad_b += diff;
            }
            grad_a /= n;
            grad_b /= n;
            a -= lr * grad_a;
            b -= lr * grad_b;
        }
        Self { a, b }
    }

    /// Transform a raw score to calibrated probability
    pub fn transform(&self, prediction: f64) -> f64 {
        sigmoid(self.a * prediction + self.b)
    }
}

/// Isotonic regression with Pool-Adjacent-Violators (PAV)
/// Fits a non-decreasing mapping from scores to calibrated probabilities.
#[derive(Debug, Clone)]
pub struct IsotonicRegression {
    /// Breakpoints (sorted scores) defining piecewise-constant regions
    breakpoints: Vec<f64>,
    /// Fitted values for each region
    values: Vec<f64>,
}

impl IsotonicRegression {
    /// Fit isotonic regression using PAV on (scores, labels) pairs.
    /// Scores are continuous, labels are bool (true=1, false=0).
    pub fn fit(scores: &[f64], labels: &[bool]) -> Self {
        assert_eq!(scores.len(), labels.len());
        let n = scores.len();
        if n == 0 {
            return Self {
                breakpoints: vec![],
                values: vec![],
            };
        }

        // Sort by score ascending
        let mut pairs: Vec<(f64, f64, f64)> = scores
            .iter()
            .cloned()
            .zip(labels.iter().map(|&y| if y { 1.0 } else { 0.0 }))
            .map(|(s, y)| (s, y, 1.0)) // (score, sum_y, weight)
            .collect();
        pairs.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap());

        // Initialize blocks: each point its own block, with mean = y
        let mut block_scores: Vec<f64> = pairs.iter().map(|p| p.0).collect();
        let mut block_sum: Vec<f64> = pairs.iter().map(|p| p.1).collect();
        let mut block_w: Vec<f64> = pairs.iter().map(|p| p.2).collect();
        let mut m = n; // current number of blocks

        // Pool-adjacent-violators algorithm
        let mut i = 0;
        while i < m - 1 {
            let mean_i = block_sum[i] / block_w[i];
            let mean_j = block_sum[i + 1] / block_w[i + 1];
            if mean_i <= mean_j {
                i += 1;
                continue;
            }
            // Merge i and i+1
            block_sum[i] += block_sum[i + 1];
            block_w[i] += block_w[i + 1];
            // Remove block i+1
            block_sum.remove(i + 1);
            block_w.remove(i + 1);
            block_scores.remove(i + 1);
            m -= 1;
            // Step back if possible to recheck monotonicity
            if i > 0 {
                i = i.saturating_sub(1);
            }
        }

        let values: Vec<f64> = block_sum
            .iter()
            .zip(block_w.iter())
            .map(|(s, w)| (s / w).clamp(0.0, 1.0))
            .collect();

        // Use the first score of each block as breakpoint
        let breakpoints = block_scores;

        Self {
            breakpoints,
            values,
        }
    }

    /// Transform a score to calibrated probability by locating its block.
    pub fn transform(&self, score: f64) -> f64 {
        if self.breakpoints.is_empty() {
            return 0.5;
        }
        match self
            .breakpoints
            .binary_search_by(|b| b.partial_cmp(&score).unwrap())
        {
            Ok(idx) => self.values[idx],
            Err(pos) => {
                let idx = if pos == 0 { 0 } else { pos - 1 };
                self.values[idx]
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn platt_scaling_basic_monotonic() {
        // Synthetic: higher scores indicate positive label
        let scores = vec![-2.0, -1.0, -0.5, 0.0, 0.5, 1.0, 2.0];
        let labels = vec![false, false, false, true, true, true, true];
        let ps = PlattScaling::fit(&scores, &labels);
        // Check monotonicity and reasonable calibrated values
        let p_low = ps.transform(-2.0);
        let p_mid = ps.transform(0.0);
        let p_high = ps.transform(2.0);
        assert!(p_low < p_mid && p_mid < p_high);
        assert!(p_low < 0.5 && p_high > 0.5);
    }

    #[test]
    fn isotonic_regression_monotonic_and_reasonable() {
        let scores = vec![-2.0, -1.0, -0.5, 0.0, 0.7, 1.0, 2.0];
        let labels = vec![false, false, false, true, true, true, true];
        let iso = IsotonicRegression::fit(&scores, &labels);
        let p1 = iso.transform(-1.0);
        let p2 = iso.transform(0.0);
        let p3 = iso.transform(1.0);
        assert!(p1 <= p2 && p2 <= p3);
        assert!(p1 < 0.6 && p3 > 0.4);
    }
}
