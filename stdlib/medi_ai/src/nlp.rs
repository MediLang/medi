use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NlpAnalysis {
    pub symptoms: Vec<String>,
    pub medications: Vec<String>,
    pub sentiment: Option<String>,
}

pub fn analyze_notes(text: &str, extract: &[String]) -> NlpAnalysis {
    let lower = text.to_lowercase();

    let mut symptoms = Vec::new();
    let mut medications = Vec::new();

    if extract.iter().any(|e| e == "symptoms") {
        if lower.contains("cough") {
            symptoms.push("cough".to_string());
        }
        if lower.contains("fever") {
            symptoms.push("fever".to_string());
        }
    }

    if extract.iter().any(|e| e == "medications") {
        if lower.contains("metformin") {
            medications.push("metformin".to_string());
        }
        if lower.contains("aspirin") {
            medications.push("aspirin".to_string());
        }
    }

    let sentiment = if extract.iter().any(|e| e == "sentiment") {
        if lower.contains("concern") || lower.contains("worsening") {
            Some("negative".to_string())
        } else if lower.contains("improved") || lower.contains("stable") {
            Some("positive".to_string())
        } else {
            Some("neutral".to_string())
        }
    } else {
        None
    };

    NlpAnalysis {
        symptoms,
        medications,
        sentiment,
    }
}
