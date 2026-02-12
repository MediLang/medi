pub struct Confusion {
    pub tp: usize,
    pub fp: usize,
    pub tn: usize,
    pub fn_: usize,
}

pub fn confusion_matrix(labels: &[bool], scores: &[f64], threshold: f64) -> Confusion {
    let mut c = Confusion {
        tp: 0,
        fp: 0,
        tn: 0,
        fn_: 0,
    };
    for (i, &y) in labels.iter().enumerate() {
        let pred = scores[i] >= threshold;
        match (y, pred) {
            (true, true) => c.tp += 1,
            (false, true) => c.fp += 1,
            (false, false) => c.tn += 1,
            (true, false) => c.fn_ += 1,
        }
    }
    c
}

pub fn roc_points(labels: &[bool], scores: &[f64], thresholds: &[f64]) -> Vec<(f64, f64)> {
    let mut pts = Vec::with_capacity(thresholds.len());
    for &t in thresholds {
        let c = confusion_matrix(labels, scores, t);
        let tp = c.tp as f64;
        let fp = c.fp as f64;
        let tn = c.tn as f64;
        let fn_ = c.fn_ as f64;
        let tpr = if tp + fn_ == 0.0 {
            0.0
        } else {
            tp / (tp + fn_)
        };
        let fpr = if fp + tn == 0.0 { 0.0 } else { fp / (fp + tn) };
        pts.push((fpr, tpr));
    }
    pts
}

/// Compute AUC using the trapezoidal rule over ROC points (fpr, tpr).
/// Assumes points are ordered in increasing FPR.
pub fn auc_from_roc(points: &[(f64, f64)]) -> f64 {
    if points.len() < 2 {
        return 0.0;
    }
    let mut auc = 0.0;
    for w in points.windows(2) {
        let (x0, y0) = w[0];
        let (x1, y1) = w[1];
        let dx = x1 - x0;
        if dx < 0.0 {
            continue;
        }
        auc += dx * (y0 + y1) / 2.0;
    }
    auc
}

/// Brier score: mean squared error between predicted probabilities and binary outcomes.
pub fn brier_score(labels: &[bool], probs: &[f64]) -> f64 {
    if labels.len() != probs.len() || labels.is_empty() {
        return f64::NAN;
    }
    let mut sum = 0.0;
    for (y, &p) in labels.iter().zip(probs.iter()) {
        let y_num = if *y { 1.0 } else { 0.0 };
        let diff = p - y_num;
        sum += diff * diff;
    }
    sum / (labels.len() as f64)
}
