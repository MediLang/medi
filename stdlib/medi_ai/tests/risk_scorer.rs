use medi_ai::{Model, RiskScorer};

#[test]
fn tiny_dataset_risk_scorer() {
    let model = RiskScorer {
        weights: vec![0.3, 0.4, 0.5],
        bias: -0.1,
        model_name: "tiny-risk".into(),
    };
    assert_eq!(model.name(), "tiny-risk");

    let x1 = [0.0, 0.0, 0.0];
    let x2 = [1.0, 0.0, 0.0];
    let x3 = [0.0, 1.0, 1.0];

    let s1 = model.predict(&x1); // clamp( -0.1 ) => 0
    let s2 = model.predict(&x2); // 0.3 - 0.1 = 0.2
    let s3 = model.predict(&x3); // 0.4 + 0.5 - 0.1 = 0.8

    assert!((s1 - 0.0).abs() < 1e-12);
    assert!((s2 - 0.2).abs() < 1e-12);
    assert!((s3 - 0.8).abs() < 1e-12);
}
