use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LungNoduleDetectionResult {
    pub num_suspected_nodules: usize,
    pub sensitivity: String,
    pub avg_confidence: Option<f64>,
}

pub fn detect_lung_nodules(
    images: &JsonValue,
    sensitivity: &str,
    return_confidence: bool,
) -> LungNoduleDetectionResult {
    let num_images = match images {
        JsonValue::Array(v) => v.len(),
        _ => 0,
    };

    let base_count = num_images.saturating_sub(1);
    let num_suspected_nodules = base_count;

    let avg_confidence = if return_confidence {
        Some(match sensitivity {
            "high" => 0.6,
            "low" => 0.8,
            _ => 0.7,
        })
    } else {
        None
    };

    LungNoduleDetectionResult {
        num_suspected_nodules,
        sensitivity: sensitivity.to_string(),
        avg_confidence,
    }
}
