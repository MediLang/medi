use serde::{Deserialize, Serialize};
use std::fs::read_to_string;

/// Minimal trait for prediction models
pub trait Model {
    fn name(&self) -> &str;
}

/// A simple risk scoring model that sums weighted features
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RiskScorer {
    pub weights: Vec<f64>,
    pub bias: f64,
    pub model_name: String,
}

impl Model for RiskScorer {
    fn name(&self) -> &str {
        &self.model_name
    }
}

impl RiskScorer {
    pub fn predict(&self, features: &[f64]) -> f64 {
        let s = self
            .weights
            .iter()
            .zip(features.iter())
            .map(|(w, x)| w * x)
            .sum::<f64>();
        (s + self.bias).clamp(0.0, 1.0) // clamp to [0,1]
    }

    pub fn load_model(path: impl AsRef<std::path::Path>) -> Result<RiskScorer, serde_json::Error> {
        let s = read_to_string(path).map_err(serde_json::Error::io)?;
        serde_json::from_str::<RiskScorer>(&s)
    }
}
