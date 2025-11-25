//! Fairness metrics: demographic parity and equal opportunity

/// Simple group-level statistics
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct GroupStats {
    /// Positive prediction rate P(ŷ=1 | group)
    pub positive_rate: f64,
    /// True positive rate P(ŷ=1 | y=1, group)
    pub tpr: f64,
}

/// Compute demographic parity difference: |P(ŷ=1|A=0) - P(ŷ=1|A=1)|
/// Returns (group0_stats, group1_stats, abs_diff)
pub fn demographic_parity(
    predictions: &[f64],
    labels: &[bool],
    groups: &[bool],
) -> (GroupStats, GroupStats, f64) {
    assert!(predictions.len() == labels.len() && labels.len() == groups.len());
    let mut n0 = 0usize;
    let mut pos0 = 0usize;
    let mut n1 = 0usize;
    let mut pos1 = 0usize;
    for ((&p, &_y), &g) in predictions.iter().zip(labels.iter()).zip(groups.iter()) {
        let yhat = p >= 0.5;
        if !g {
            n0 += 1;
            if yhat {
                pos0 += 1;
            }
        } else {
            n1 += 1;
            if yhat {
                pos1 += 1;
            }
        }
    }
    let pr0 = if n0 == 0 {
        0.0
    } else {
        pos0 as f64 / n0 as f64
    };
    let pr1 = if n1 == 0 {
        0.0
    } else {
        pos1 as f64 / n1 as f64
    };
    (
        GroupStats {
            positive_rate: pr0,
            tpr: 0.0,
        },
        GroupStats {
            positive_rate: pr1,
            tpr: 0.0,
        },
        (pr0 - pr1).abs(),
    )
}

/// Compute equal opportunity difference: |TPR(group0) - TPR(group1)|
/// Returns (group0_stats, group1_stats, abs_diff)
pub fn equal_opportunity(
    predictions: &[f64],
    labels: &[bool],
    groups: &[bool],
) -> (GroupStats, GroupStats, f64) {
    assert!(predictions.len() == labels.len() && labels.len() == groups.len());
    let mut pos0 = 0usize;
    let mut tp0 = 0usize;
    let mut pos1 = 0usize;
    let mut tp1 = 0usize;
    for ((&p, &y), &g) in predictions.iter().zip(labels.iter()).zip(groups.iter()) {
        if y {
            if !g {
                pos0 += 1;
                if p >= 0.5 {
                    tp0 += 1;
                }
            } else {
                pos1 += 1;
                if p >= 0.5 {
                    tp1 += 1;
                }
            }
        }
    }
    let tpr0 = if pos0 == 0 {
        0.0
    } else {
        tp0 as f64 / pos0 as f64
    };
    let tpr1 = if pos1 == 0 {
        0.0
    } else {
        tp1 as f64 / pos1 as f64
    };
    (
        GroupStats {
            positive_rate: 0.0,
            tpr: tpr0,
        },
        GroupStats {
            positive_rate: 0.0,
            tpr: tpr1,
        },
        (tpr0 - tpr1).abs(),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fairness_metrics_basic() {
        let preds = vec![0.1, 0.6, 0.7, 0.2];
        let labels = vec![false, true, true, false];
        let groups = vec![false, false, true, true];
        let (_g0, _g1, dp) = demographic_parity(&preds, &labels, &groups);
        let (_g0, _g1, eo) = equal_opportunity(&preds, &labels, &groups);
        assert!(dp >= 0.0 && eo >= 0.0);
    }
}
