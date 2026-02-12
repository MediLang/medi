//! Validation metrics framework

pub trait ValidationMetric {
    fn name(&self) -> &str;
    fn compute(&self, predictions: &[f64], labels: &[bool]) -> f64;
}

fn threshold_preds(preds: &[f64], t: f64) -> Vec<bool> {
    preds.iter().map(|&p| p >= t).collect()
}

fn confusion(preds: &[bool], labels: &[bool]) -> (u64, u64, u64, u64) {
    let mut tp = 0;
    let mut fp = 0;
    let mut tn = 0;
    let mut fn_ = 0;
    for (p, &y) in preds.iter().zip(labels.iter()) {
        match (*p, y) {
            (true, true) => tp += 1,
            (true, false) => fp += 1,
            (false, true) => fn_ += 1,
            (false, false) => tn += 1,
        }
    }
    (tp, fp, tn, fn_)
}

pub struct Accuracy {
    pub threshold: f64,
}
impl Default for Accuracy {
    fn default() -> Self {
        Self { threshold: 0.5 }
    }
}
impl ValidationMetric for Accuracy {
    fn name(&self) -> &str {
        "accuracy"
    }
    fn compute(&self, predictions: &[f64], labels: &[bool]) -> f64 {
        let preds = threshold_preds(predictions, self.threshold);
        let (tp, fp, tn, fn_) = confusion(&preds, labels);
        let total = (tp + fp + tn + fn_) as f64;
        if total == 0.0 {
            return 0.0;
        }
        (tp + tn) as f64 / total
    }
}

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
        let preds = threshold_preds(predictions, self.threshold);
        let (tp, fp, _tn, _fn) = confusion(&preds, labels);
        let denom = (tp + fp) as f64;
        if denom == 0.0 {
            0.0
        } else {
            tp as f64 / denom
        }
    }
}

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
        let preds = threshold_preds(predictions, self.threshold);
        let (tp, _fp, _tn, fn_) = confusion(&preds, labels);
        let denom = (tp + fn_) as f64;
        if denom == 0.0 {
            0.0
        } else {
            tp as f64 / denom
        }
    }
}

pub struct F1 {
    pub threshold: f64,
}
impl Default for F1 {
    fn default() -> Self {
        Self { threshold: 0.5 }
    }
}
impl ValidationMetric for F1 {
    fn name(&self) -> &str {
        "f1"
    }
    fn compute(&self, predictions: &[f64], labels: &[bool]) -> f64 {
        let p = Precision::default().compute(predictions, labels);
        let r = Recall::default().compute(predictions, labels);
        if p + r == 0.0 {
            0.0
        } else {
            2.0 * p * r / (p + r)
        }
    }
}

pub struct RocAuc;
impl ValidationMetric for RocAuc {
    fn name(&self) -> &str {
        "roc_auc"
    }
    fn compute(&self, predictions: &[f64], labels: &[bool]) -> f64 {
        // Rank-based AUC: (sum_ranks_pos - P*(P+1)/2) / (P*N)
        let n = predictions.len();
        if n == 0 {
            return 0.0;
        }
        let p_count = labels.iter().filter(|&&y| y).count();
        let n_count = n - p_count;
        if p_count == 0 || n_count == 0 {
            return 0.5;
        }

        // Pair predictions with labels and indices
        let mut items: Vec<(f64, bool)> = predictions
            .iter()
            .cloned()
            .zip(labels.iter().cloned())
            .collect();
        // Sort ascending by score to assign ranks (handling ties by average rank)
        items.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap());

        // Assign ranks with tie handling
        let mut ranks: Vec<f64> = vec![0.0; n];
        let mut i = 0;
        while i < n {
            let j = {
                let mut k = i + 1;
                while k < n && items[k].0 == items[i].0 {
                    k += 1;
                }
                k
            };
            // average rank for ties (1-based ranks)
            let avg_rank = (i as f64 + 1.0 + j as f64) / 2.0;
            for r in ranks.iter_mut().take(j).skip(i) {
                *r = avg_rank;
            }
            i = j;
        }

        // Sum ranks of positives
        let mut sum_ranks_pos = 0.0;
        for (idx, (_, y)) in items.iter().enumerate() {
            if *y {
                sum_ranks_pos += ranks[idx];
            }
        }

        let p = p_count as f64;
        let n_neg = n_count as f64;
        (sum_ranks_pos - p * (p + 1.0) / 2.0) / (p * n_neg)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_metrics() {
        let preds = vec![0.1, 0.4, 0.6, 0.9];
        let labels = vec![false, false, true, true];
        let acc = Accuracy::default().compute(&preds, &labels);
        let p = Precision::default().compute(&preds, &labels);
        let r = Recall::default().compute(&preds, &labels);
        let f1 = F1::default().compute(&preds, &labels);
        let auc = RocAuc.compute(&preds, &labels);
        assert!(acc > 0.5 && p > 0.5 && r > 0.5 && f1 > 0.5 && auc > 0.5);
    }
}
