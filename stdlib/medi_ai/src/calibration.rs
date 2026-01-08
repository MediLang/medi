//! Model calibration utilities for probability calibration.
//!
//! Provides Platt scaling and isotonic regression for calibrating
//! model predictions to well-calibrated probabilities.

use serde::{Deserialize, Serialize};

/// Platt scaling for probability calibration using logistic regression.
///
/// Transforms raw model outputs into calibrated probabilities using
/// the sigmoid function: P(y=1|f) = 1 / (1 + exp(A*f + B))
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PlattScaling {
    /// Slope parameter (A)
    pub a: f64,
    /// Intercept parameter (B)
    pub b: f64,
}

impl Default for PlattScaling {
    fn default() -> Self {
        Self { a: -1.0, b: 0.0 }
    }
}

impl PlattScaling {
    /// Create a new PlattScaling with default parameters
    pub fn new() -> Self {
        Self::default()
    }

    /// Fit Platt scaling parameters from predictions and binary labels.
    ///
    /// Uses a simplified gradient descent approach to find A and B that
    /// minimize the negative log-likelihood.
    ///
    /// # Arguments
    /// * `predictions` - Raw model outputs (scores/logits)
    /// * `labels` - Binary labels (true = positive class)
    pub fn fit(predictions: &[f64], labels: &[bool]) -> Self {
        assert_eq!(
            predictions.len(),
            labels.len(),
            "Predictions and labels must have same length"
        );

        if predictions.is_empty() {
            return Self::default();
        }

        // Target probabilities with Laplace smoothing
        let n_pos = labels.iter().filter(|&&l| l).count() as f64;
        let n_neg = labels.len() as f64 - n_pos;
        let t_pos = (n_pos + 1.0) / (n_pos + 2.0);
        let t_neg = 1.0 / (n_neg + 2.0);

        let targets: Vec<f64> = labels
            .iter()
            .map(|&l| if l { t_pos } else { t_neg })
            .collect();

        // Simple gradient descent
        let mut a = 0.0;
        let mut b = 0.0;
        let lr = 0.01;
        let max_iter = 1000;
        let tol = 1e-8;

        for _ in 0..max_iter {
            let mut grad_a = 0.0;
            let mut grad_b = 0.0;

            for ((&pred, &target), _) in predictions.iter().zip(targets.iter()).zip(labels.iter()) {
                let p = sigmoid(a * pred + b);
                let diff = p - target;
                grad_a += diff * pred;
                grad_b += diff;
            }

            grad_a /= predictions.len() as f64;
            grad_b /= predictions.len() as f64;

            let new_a = a - lr * grad_a;
            let new_b = b - lr * grad_b;

            if (new_a - a).abs() < tol && (new_b - b).abs() < tol {
                break;
            }

            a = new_a;
            b = new_b;
        }

        Self { a, b }
    }

    /// Transform a raw prediction into a calibrated probability
    pub fn transform(&self, prediction: f64) -> f64 {
        sigmoid(self.a * prediction + self.b)
    }

    /// Transform multiple predictions
    pub fn transform_batch(&self, predictions: &[f64]) -> Vec<f64> {
        predictions.iter().map(|&p| self.transform(p)).collect()
    }
}

/// Sigmoid function
fn sigmoid(x: f64) -> f64 {
    1.0 / (1.0 + (-x).exp())
}

/// Isotonic regression for probability calibration.
///
/// Fits a non-decreasing step function to map predictions to probabilities.
/// Uses the Pool Adjacent Violators (PAV) algorithm.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IsotonicRegression {
    /// Sorted prediction thresholds
    pub thresholds: Vec<f64>,
    /// Corresponding calibrated values
    pub values: Vec<f64>,
}

impl Default for IsotonicRegression {
    fn default() -> Self {
        Self {
            thresholds: vec![0.0, 1.0],
            values: vec![0.0, 1.0],
        }
    }
}

impl IsotonicRegression {
    /// Create a new IsotonicRegression with default (identity) mapping
    pub fn new() -> Self {
        Self::default()
    }

    /// Fit isotonic regression from predictions and binary labels.
    ///
    /// Uses the Pool Adjacent Violators (PAV) algorithm.
    pub fn fit(predictions: &[f64], labels: &[bool]) -> Self {
        assert_eq!(
            predictions.len(),
            labels.len(),
            "Predictions and labels must have same length"
        );

        if predictions.is_empty() {
            return Self::default();
        }

        // Sort by predictions
        let mut pairs: Vec<(f64, f64)> = predictions
            .iter()
            .zip(labels.iter())
            .map(|(&p, &l)| (p, if l { 1.0 } else { 0.0 }))
            .collect();
        pairs.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap_or(std::cmp::Ordering::Equal));

        // PAV algorithm
        let n = pairs.len();
        let mut weights: Vec<f64> = vec![1.0; n];
        let mut values: Vec<f64> = pairs.iter().map(|(_, y)| *y).collect();

        let mut i = 0;
        while i < n - 1 {
            if values[i] > values[i + 1] {
                // Pool adjacent violators
                let new_val = (values[i] * weights[i] + values[i + 1] * weights[i + 1])
                    / (weights[i] + weights[i + 1]);
                let new_weight = weights[i] + weights[i + 1];

                values[i] = new_val;
                weights[i] = new_weight;
                values.remove(i + 1);
                weights.remove(i + 1);

                // Check backwards
                i = i.saturating_sub(1);
            } else {
                i += 1;
            }
        }

        // Build thresholds from remaining blocks
        let mut thresholds = Vec::new();
        let mut final_values = Vec::new();

        let mut idx = 0;
        for (j, &w) in weights.iter().enumerate() {
            let block_size = w as usize;
            if block_size > 0 {
                thresholds.push(pairs[idx].0);
                final_values.push(values[j]);
                idx += block_size;
            }
        }

        // Ensure we have at least two points
        if thresholds.len() < 2 {
            return Self::default();
        }

        Self {
            thresholds,
            values: final_values,
        }
    }

    /// Transform a raw prediction into a calibrated probability
    pub fn transform(&self, prediction: f64) -> f64 {
        if self.thresholds.is_empty() {
            return prediction.clamp(0.0, 1.0);
        }

        // Find the appropriate bin
        let pos = self
            .thresholds
            .iter()
            .position(|&t| t > prediction)
            .unwrap_or(self.thresholds.len());

        if pos == 0 {
            self.values[0]
        } else if pos >= self.values.len() {
            *self.values.last().unwrap_or(&prediction)
        } else {
            // Linear interpolation between adjacent points
            let t0 = self.thresholds[pos - 1];
            let t1 = self.thresholds[pos];
            let v0 = self.values[pos - 1];
            let v1 = self.values[pos];

            if (t1 - t0).abs() < 1e-10 {
                v0
            } else {
                let alpha = (prediction - t0) / (t1 - t0);
                v0 + alpha * (v1 - v0)
            }
        }
    }

    /// Transform multiple predictions
    pub fn transform_batch(&self, predictions: &[f64]) -> Vec<f64> {
        predictions.iter().map(|&p| self.transform(p)).collect()
    }
}

/// Calibration error metric (Expected Calibration Error)
pub fn expected_calibration_error(predictions: &[f64], labels: &[bool], n_bins: usize) -> f64 {
    if predictions.is_empty() || n_bins == 0 {
        return 0.0;
    }

    let mut bin_totals = vec![0.0; n_bins];
    let mut bin_correct = vec![0.0; n_bins];
    let mut bin_counts = vec![0usize; n_bins];

    for (&pred, &label) in predictions.iter().zip(labels.iter()) {
        let bin = ((pred * n_bins as f64) as usize).min(n_bins - 1);
        bin_totals[bin] += pred;
        bin_correct[bin] += if label { 1.0 } else { 0.0 };
        bin_counts[bin] += 1;
    }

    let n = predictions.len() as f64;
    let mut ece = 0.0;

    for i in 0..n_bins {
        if bin_counts[i] > 0 {
            let avg_pred = bin_totals[i] / bin_counts[i] as f64;
            let avg_correct = bin_correct[i] / bin_counts[i] as f64;
            ece += (bin_counts[i] as f64 / n) * (avg_pred - avg_correct).abs();
        }
    }

    ece
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_platt_scaling_transform() {
        let platt = PlattScaling { a: -1.0, b: 0.0 };
        let p = platt.transform(0.0);
        assert!((p - 0.5).abs() < 1e-10);
    }

    #[test]
    fn test_platt_scaling_fit() {
        let predictions = vec![0.1, 0.3, 0.5, 0.7, 0.9];
        let labels = vec![false, false, true, true, true];
        let platt = PlattScaling::fit(&predictions, &labels);

        // After fitting, higher predictions should map to higher probabilities
        let p_low = platt.transform(0.1);
        let p_high = platt.transform(0.9);
        assert!(p_high > p_low);
    }

    #[test]
    fn test_isotonic_regression_transform() {
        let iso = IsotonicRegression {
            thresholds: vec![0.0, 0.5, 1.0],
            values: vec![0.0, 0.5, 1.0],
        };
        let p = iso.transform(0.25);
        assert!((p - 0.25).abs() < 1e-10);
    }

    #[test]
    fn test_isotonic_regression_fit() {
        let predictions = vec![0.1, 0.2, 0.3, 0.7, 0.8, 0.9];
        let labels = vec![false, false, false, true, true, true];
        let iso = IsotonicRegression::fit(&predictions, &labels);

        // After fitting, higher predictions should map to higher probabilities
        let p_low = iso.transform(0.1);
        let p_high = iso.transform(0.9);
        assert!(p_high >= p_low);
    }

    #[test]
    fn test_expected_calibration_error() {
        // Perfectly calibrated predictions
        let predictions = vec![0.1, 0.3, 0.5, 0.7, 0.9];
        let labels = vec![false, false, true, true, true];
        let ece = expected_calibration_error(&predictions, &labels, 5);
        // ECE should be relatively low for reasonably calibrated predictions
        assert!(ece < 0.5);
    }

    #[test]
    fn test_platt_batch() {
        let platt = PlattScaling { a: -1.0, b: 0.0 };
        let preds = vec![0.0, 0.5, 1.0];
        let calibrated = platt.transform_batch(&preds);
        assert_eq!(calibrated.len(), 3);
    }
}
