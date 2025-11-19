use serde::{Deserialize, Serialize};

use crate::RiskScorer;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FeatureContribution {
    pub feature_index: usize,
    pub weight: f64,
    pub contribution: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Explanation {
    pub prediction: f64,
    pub bias: f64,
    pub contributions: Vec<FeatureContribution>,
}

pub fn explain(model: &RiskScorer, features: &[f64]) -> Explanation {
    let mut contributions = Vec::new();
    let mut sum = 0.0;

    for (idx, (w, x)) in model.weights.iter().zip(features.iter()).enumerate() {
        let c = w * x;
        contributions.push(FeatureContribution {
            feature_index: idx,
            weight: *w,
            contribution: c,
        });
        sum += c;
    }

    let prediction = (sum + model.bias).clamp(0.0, 1.0);

    Explanation {
        prediction,
        bias: model.bias,
        contributions,
    }
}
