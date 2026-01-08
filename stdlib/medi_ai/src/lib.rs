use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
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

pub mod bias;
pub mod calibration;
pub mod cohort;
pub mod decision_support;
pub mod explain;
pub mod imaging;
pub mod metrics;
pub mod nlp;
pub mod registry;
pub mod risk;
pub mod validation;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EdgeModel {
    pub inner: RiskScorer,
    pub target: String,
    pub format: String,
    pub quantized: bool,
}

impl RiskScorer {
    pub fn optimize(&self, target: &str, format: &str, quantize: bool) -> EdgeModel {
        EdgeModel {
            inner: self.clone(),
            target: target.to_string(),
            format: format.to_string(),
            quantized: quantize,
        }
    }
}

pub fn predict_risk(
    data: &JsonValue,
    condition: &str,
    timeframe: &str,
    features: &[String],
) -> risk::RiskPrediction {
    risk::predict_risk(data, condition, timeframe, features)
}

pub fn stratify_risk(prediction: &risk::RiskPrediction) -> risk::RiskStratum {
    risk::stratify_risk(prediction)
}

pub fn load_model(path: impl AsRef<std::path::Path>) -> Result<RiskScorer, serde_json::Error> {
    RiskScorer::load_model(path)
}
