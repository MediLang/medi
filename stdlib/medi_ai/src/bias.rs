use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GroupBiasMetric {
    pub group: String,
    pub mean_score: f64,
    pub count: usize,
}

pub fn compute_group_bias(predictions: &[(f64, String)]) -> Vec<GroupBiasMetric> {
    use std::collections::BTreeMap;

    let mut sums: BTreeMap<String, (f64, usize)> = BTreeMap::new();

    for (score, group) in predictions {
        let entry = sums.entry(group.clone()).or_insert((0.0, 0));
        entry.0 += *score;
        entry.1 += 1;
    }

    let mut out = Vec::new();
    for (group, (sum, count)) in sums {
        if count > 0 {
            out.push(GroupBiasMetric {
                group,
                mean_score: sum / (count as f64),
                count,
            });
        }
    }

    out
}
