use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RiskPrediction {
    pub condition: String,
    pub timeframe: String,
    pub score: f64,
    pub features_used: Vec<String>,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum RiskStratum {
    Low,
    Moderate,
    High,
}

pub fn predict_risk(
    data: &JsonValue,
    condition: &str,
    timeframe: &str,
    features: &[String],
) -> RiskPrediction {
    let mut numeric_features = Vec::new();
    for name in features {
        if let Some(value) = data.get(name) {
            if let Some(x) = value.as_f64() {
                numeric_features.push(x);
            }
        }
    }

    let score = if numeric_features.is_empty() {
        0.0
    } else {
        let sum: f64 = numeric_features.iter().copied().sum();
        let avg = sum / (numeric_features.len() as f64);
        avg.clamp(0.0, 1.0)
    };

    RiskPrediction {
        condition: condition.to_string(),
        timeframe: timeframe.to_string(),
        score,
        features_used: features.to_vec(),
    }
}

pub fn stratify_risk(prediction: &RiskPrediction) -> RiskStratum {
    if prediction.score >= 0.7 {
        RiskStratum::High
    } else if prediction.score >= 0.3 {
        RiskStratum::Moderate
    } else {
        RiskStratum::Low
    }
}
