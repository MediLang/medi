use medi_ai::{predict_risk, stratify_risk, RiskScorer};
use serde_json::json;

fn main() {
    // Model
    let model = RiskScorer {
        weights: vec![0.2, 0.5, 0.3],
        bias: 0.1,
        model_name: "demo".into(),
    };
    let score = model.predict(&[0.4, 0.2, 0.9]);
    println!("score={score:.3}");

    // End-to-end risk helpers
    let patient = json!({"age": 65, "bmi": 33.1, "a1c": 7.8});
    let pred = predict_risk(
        &patient,
        "diabetes",
        "5y",
        &["age".into(), "bmi".into(), "a1c".into()],
    );
    let stratum = stratify_risk(&pred);
    println!("pred={:.3} stratum={stratum:?}", pred.score);
}
