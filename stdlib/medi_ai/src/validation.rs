use serde::{Deserialize, Serialize};

use crate::RiskScorer;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationResult {
    pub is_valid: bool,
    pub issues: Vec<String>,
}

pub fn validate_model(model: &RiskScorer) -> ValidationResult {
    let mut issues = Vec::new();

    if model.weights.is_empty() {
        issues.push("model has no weights".to_string());
    }

    if model.weights.len() > 1024 {
        issues.push("model has many weights; consider simplifying".to_string());
    }

    if model.weights.iter().any(|w| w.is_nan()) {
        issues.push("model contains NaN weights".to_string());
    }

    if model.weights.iter().any(|w| w.abs() > 10.0) {
        issues.push("model contains very large weights; check for scaling issues".to_string());
    }

    if model.weights.iter().any(|w| *w < 0.0) {
        issues.push(
            "model contains negative weights; verify this is clinically intended".to_string(),
        );
    }

    ValidationResult {
        is_valid: issues.is_empty(),
        issues,
    }
}
