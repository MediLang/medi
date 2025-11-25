use medi_ai::{load_model, stratify_risk, RiskScorer};
use serde_json::json;
use std::fs;

#[test]
fn patient_risk_prediction_workflow() {
    // 1. Load patient data (synthetic)
    let patient = json!({
        "age": 0.6,      // scaled features [0,1]
        "bmi": 0.4,
        "a1c": 0.7,
    });

    // 2. Extract features by names
    let feature_names = vec!["age".to_string(), "bmi".to_string(), "a1c".to_string()];
    let features: Vec<f64> = feature_names
        .iter()
        .map(|k| patient.get(k).and_then(|v| v.as_f64()).unwrap_or(0.0))
        .collect();

    // 3. Create and save a simple model JSON to temp path, then load
    let tmp = tempfile::tempdir().unwrap();
    let model_path = tmp.path().join("risk_model.json");
    let model = RiskScorer {
        weights: vec![0.2, 0.3, 0.5],
        bias: 0.0,
        model_name: "demo".into(),
    };
    fs::write(&model_path, serde_json::to_string(&model).unwrap()).unwrap();
    let model = load_model(&model_path).expect("loaded model");

    // 4. Predict a risk score
    let score = model.predict(&features);
    assert!((0.0..=1.0).contains(&score));

    // 5. Stratify
    let pred = medi_ai::risk::RiskPrediction {
        condition: "diabetes".into(),
        timeframe: "5y".into(),
        score,
        features_used: feature_names,
    };
    let stratum = stratify_risk(&pred);
    assert!(matches!(
        stratum,
        medi_ai::risk::RiskStratum::Low
            | medi_ai::risk::RiskStratum::Moderate
            | medi_ai::risk::RiskStratum::High
    ));
}
