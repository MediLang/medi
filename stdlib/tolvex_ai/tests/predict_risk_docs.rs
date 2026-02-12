use tolvex_ai::predict_risk;
use serde_json::json;

#[test]
fn docs_style_predict_risk_uses_numeric_features_and_is_deterministic() {
    let patient_data = json!({
        "age": 60.0,
        "bp": 140.0,
        "bmi": 32.0,
        "medications": 2.0,
        "comorbidities": 3.0,
    });

    let features = vec![
        "age".to_string(),
        "bp".to_string(),
        "bmi".to_string(),
        "medications".to_string(),
        "comorbidities".to_string(),
    ];

    let risk = predict_risk(&patient_data, "heart_failure", "5_years", &features);

    assert_eq!(risk.condition, "heart_failure");
    assert_eq!(risk.timeframe, "5_years");
    assert_eq!(risk.features_used, features);

    let expected_score = {
        let vals = [60.0, 140.0, 32.0, 2.0, 3.0];
        let sum: f64 = vals.iter().copied().sum();
        let avg = sum / (vals.len() as f64);
        avg.clamp(0.0, 1.0)
    };

    assert!((risk.score - expected_score).abs() < 1e-12);

    let again = predict_risk(&patient_data, "heart_failure", "5_years", &features);
    assert!((risk.score - again.score).abs() < 1e-12);
}
