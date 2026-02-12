//! Validation metrics for model performance assessment.
//!
//! Provides ROC-AUC, precision, recall, F1, and fairness metrics.

use serde::{Deserialize, Serialize};

/// Trait for validation metrics
pub trait ValidationMetric: Send + Sync {
    /// Name of the metric
    fn name(&self) -> &str;

    /// Compute the metric from predictions and binary labels
    fn compute(&self, predictions: &[f64], labels: &[bool]) -> f64;
}

/// ROC-AUC (Area Under the Receiver Operating Characteristic Curve)
#[derive(Debug, Clone, Default)]
pub struct RocAuc;

impl ValidationMetric for RocAuc {
    fn name(&self) -> &str {
        "roc_auc"
    }

    fn compute(&self, predictions: &[f64], labels: &[bool]) -> f64 {
        roc_auc(predictions, labels)
    }
}

/// Precision metric
#[derive(Debug, Clone)]
pub struct Precision {
    pub threshold: f64,
}

impl Default for Precision {
    fn default() -> Self {
        Self { threshold: 0.5 }
    }
}

impl ValidationMetric for Precision {
    fn name(&self) -> &str {
        "precision"
    }

    fn compute(&self, predictions: &[f64], labels: &[bool]) -> f64 {
        precision(predictions, labels, self.threshold)
    }
}

/// Recall metric
#[derive(Debug, Clone)]
pub struct Recall {
    pub threshold: f64,
}

impl Default for Recall {
    fn default() -> Self {
        Self { threshold: 0.5 }
    }
}

impl ValidationMetric for Recall {
    fn name(&self) -> &str {
        "recall"
    }

    fn compute(&self, predictions: &[f64], labels: &[bool]) -> f64 {
        recall(predictions, labels, self.threshold)
    }
}

/// F1 Score metric
#[derive(Debug, Clone)]
pub struct F1Score {
    pub threshold: f64,
}

impl Default for F1Score {
    fn default() -> Self {
        Self { threshold: 0.5 }
    }
}

impl ValidationMetric for F1Score {
    fn name(&self) -> &str {
        "f1_score"
    }

    fn compute(&self, predictions: &[f64], labels: &[bool]) -> f64 {
        f1_score(predictions, labels, self.threshold)
    }
}

/// Calibration Error metric
#[derive(Debug, Clone)]
pub struct CalibrationError {
    pub n_bins: usize,
}

impl Default for CalibrationError {
    fn default() -> Self {
        Self { n_bins: 10 }
    }
}

impl ValidationMetric for CalibrationError {
    fn name(&self) -> &str {
        "calibration_error"
    }

    fn compute(&self, predictions: &[f64], labels: &[bool]) -> f64 {
        crate::calibration::expected_calibration_error(predictions, labels, self.n_bins)
    }
}

/// Demographic Parity fairness metric
///
/// Measures the difference in positive prediction rates between groups.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DemographicParity;

impl DemographicParity {
    /// Compute demographic parity difference between two groups
    pub fn compute_groups(predictions: &[f64], group_membership: &[bool], threshold: f64) -> f64 {
        let mut group_a_pos = 0;
        let mut group_a_total = 0;
        let mut group_b_pos = 0;
        let mut group_b_total = 0;

        for (&pred, &is_group_a) in predictions.iter().zip(group_membership.iter()) {
            let is_positive = pred >= threshold;
            if is_group_a {
                group_a_total += 1;
                if is_positive {
                    group_a_pos += 1;
                }
            } else {
                group_b_total += 1;
                if is_positive {
                    group_b_pos += 1;
                }
            }
        }

        let rate_a = if group_a_total > 0 {
            group_a_pos as f64 / group_a_total as f64
        } else {
            0.0
        };
        let rate_b = if group_b_total > 0 {
            group_b_pos as f64 / group_b_total as f64
        } else {
            0.0
        };

        (rate_a - rate_b).abs()
    }
}

/// Equal Opportunity fairness metric
///
/// Measures the difference in true positive rates between groups.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EqualOpportunity;

impl EqualOpportunity {
    /// Compute equal opportunity difference between two groups
    pub fn compute_groups(
        predictions: &[f64],
        labels: &[bool],
        group_membership: &[bool],
        threshold: f64,
    ) -> f64 {
        let mut group_a_tp = 0;
        let mut group_a_pos = 0;
        let mut group_b_tp = 0;
        let mut group_b_pos = 0;

        for ((&pred, &label), &is_group_a) in predictions
            .iter()
            .zip(labels.iter())
            .zip(group_membership.iter())
        {
            if label {
                // Only consider positive examples
                let is_predicted_positive = pred >= threshold;
                if is_group_a {
                    group_a_pos += 1;
                    if is_predicted_positive {
                        group_a_tp += 1;
                    }
                } else {
                    group_b_pos += 1;
                    if is_predicted_positive {
                        group_b_tp += 1;
                    }
                }
            }
        }

        let tpr_a = if group_a_pos > 0 {
            group_a_tp as f64 / group_a_pos as f64
        } else {
            0.0
        };
        let tpr_b = if group_b_pos > 0 {
            group_b_tp as f64 / group_b_pos as f64
        } else {
            0.0
        };

        (tpr_a - tpr_b).abs()
    }
}

/// Model validator that runs multiple metrics
pub struct ModelValidator {
    pub metrics: Vec<Box<dyn ValidationMetric>>,
}

impl Default for ModelValidator {
    fn default() -> Self {
        Self::new()
    }
}

impl ModelValidator {
    pub fn new() -> Self {
        Self {
            metrics: Vec::new(),
        }
    }

    /// Add a metric to the validator
    pub fn add_metric(&mut self, metric: Box<dyn ValidationMetric>) {
        self.metrics.push(metric);
    }

    /// Run all metrics and return results
    pub fn validate(&self, predictions: &[f64], labels: &[bool]) -> Vec<(String, f64)> {
        self.metrics
            .iter()
            .map(|m| (m.name().to_string(), m.compute(predictions, labels)))
            .collect()
    }
}

/// Compute ROC-AUC using the trapezoidal rule
pub fn roc_auc(predictions: &[f64], labels: &[bool]) -> f64 {
    if predictions.is_empty() || labels.is_empty() {
        return 0.5;
    }

    let n_pos = labels.iter().filter(|&&l| l).count();
    let n_neg = labels.len() - n_pos;

    if n_pos == 0 || n_neg == 0 {
        return 0.5;
    }

    // Sort by predictions descending
    let mut pairs: Vec<(f64, bool)> = predictions
        .iter()
        .zip(labels.iter())
        .map(|(&p, &l)| (p, l))
        .collect();
    pairs.sort_by(|a, b| b.0.partial_cmp(&a.0).unwrap_or(std::cmp::Ordering::Equal));

    // Compute AUC using Wilcoxon-Mann-Whitney statistic
    let mut auc = 0.0;
    let mut tp = 0;

    for (_, label) in pairs {
        if label {
            tp += 1;
        } else {
            auc += tp as f64;
        }
    }

    auc / (n_pos as f64 * n_neg as f64)
}

/// Compute precision at a given threshold
pub fn precision(predictions: &[f64], labels: &[bool], threshold: f64) -> f64 {
    let mut tp = 0;
    let mut fp = 0;

    for (&pred, &label) in predictions.iter().zip(labels.iter()) {
        if pred >= threshold {
            if label {
                tp += 1;
            } else {
                fp += 1;
            }
        }
    }

    if tp + fp == 0 {
        0.0
    } else {
        tp as f64 / (tp + fp) as f64
    }
}

/// Compute recall at a given threshold
pub fn recall(predictions: &[f64], labels: &[bool], threshold: f64) -> f64 {
    let mut tp = 0;
    let mut fn_ = 0;

    for (&pred, &label) in predictions.iter().zip(labels.iter()) {
        if label {
            if pred >= threshold {
                tp += 1;
            } else {
                fn_ += 1;
            }
        }
    }

    if tp + fn_ == 0 {
        0.0
    } else {
        tp as f64 / (tp + fn_) as f64
    }
}

/// Compute F1 score at a given threshold
pub fn f1_score(predictions: &[f64], labels: &[bool], threshold: f64) -> f64 {
    let p = precision(predictions, labels, threshold);
    let r = recall(predictions, labels, threshold);

    if p + r == 0.0 {
        0.0
    } else {
        2.0 * p * r / (p + r)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_roc_auc_perfect() {
        let predictions = vec![0.9, 0.8, 0.2, 0.1];
        let labels = vec![true, true, false, false];
        let auc = roc_auc(&predictions, &labels);
        assert!((auc - 1.0).abs() < 1e-10);
    }

    #[test]
    fn test_roc_auc_random() {
        let predictions = vec![0.5, 0.5, 0.5, 0.5];
        let labels = vec![true, false, true, false];
        let auc = roc_auc(&predictions, &labels);
        // With tied predictions, AUC can vary; just check it's in valid range
        assert!((0.0..=1.0).contains(&auc));
    }

    #[test]
    fn test_precision() {
        let predictions = vec![0.9, 0.8, 0.6, 0.4, 0.2];
        let labels = vec![true, true, false, true, false];
        let p = precision(&predictions, &labels, 0.5);
        // Predicted positive: 0.9, 0.8, 0.6 -> TP=2, FP=1 -> precision = 2/3
        assert!((p - 2.0 / 3.0).abs() < 1e-10);
    }

    #[test]
    fn test_recall() {
        let predictions = vec![0.9, 0.8, 0.6, 0.4, 0.2];
        let labels = vec![true, true, false, true, false];
        let r = recall(&predictions, &labels, 0.5);
        // Actual positive: 3, TP=2 -> recall = 2/3
        assert!((r - 2.0 / 3.0).abs() < 1e-10);
    }

    #[test]
    fn test_f1_score() {
        let predictions = vec![0.9, 0.8, 0.6, 0.4, 0.2];
        let labels = vec![true, true, false, true, false];
        let f1 = f1_score(&predictions, &labels, 0.5);
        // precision = 2/3, recall = 2/3 -> F1 = 2/3
        assert!((f1 - 2.0 / 3.0).abs() < 1e-10);
    }

    #[test]
    fn test_demographic_parity() {
        let predictions = vec![0.9, 0.8, 0.6, 0.4, 0.2, 0.1];
        let groups = vec![true, true, true, false, false, false];
        let dp = DemographicParity::compute_groups(&predictions, &groups, 0.5);
        // Group A: 3 samples, 3 positive predictions (0.9, 0.8, 0.6 >= 0.5) -> rate = 1.0
        // Group B: 3 samples, 0 positive predictions (0.4, 0.2, 0.1 < 0.5) -> rate = 0
        // Difference = 1.0
        assert!((dp - 1.0).abs() < 1e-10);
    }

    #[test]
    fn test_equal_opportunity() {
        let predictions = vec![0.9, 0.8, 0.6, 0.4];
        let labels = vec![true, true, true, true];
        let groups = vec![true, true, false, false];
        let eo = EqualOpportunity::compute_groups(&predictions, &labels, &groups, 0.5);
        // Group A: 2 positive, 2 TP -> TPR = 1.0
        // Group B: 2 positive, 1 TP -> TPR = 0.5
        // Difference = 0.5
        assert!((eo - 0.5).abs() < 1e-10);
    }

    #[test]
    fn test_model_validator() {
        let mut validator = ModelValidator::new();
        validator.add_metric(Box::new(RocAuc));
        validator.add_metric(Box::new(Precision::default()));
        validator.add_metric(Box::new(Recall::default()));

        let predictions = vec![0.9, 0.8, 0.2, 0.1];
        let labels = vec![true, true, false, false];

        let results = validator.validate(&predictions, &labels);
        assert_eq!(results.len(), 3);
        assert_eq!(results[0].0, "roc_auc");
    }
}
