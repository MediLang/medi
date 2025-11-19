use medi_ai::{explain::explain, RiskScorer};

#[test]
fn docs_style_explain_aligns_with_risk_scorer_prediction() {
    let model = RiskScorer {
        weights: vec![0.3, 0.4, 0.5],
        bias: -0.1,
        model_name: "tiny-risk".into(),
    };

    let features = [1.0, 0.0, 1.0];

    let explanation = explain(&model, &features);

    assert_eq!(explanation.contributions.len(), 3);

    let predicted = model.predict(&features);
    assert!((explanation.prediction - predicted).abs() < 1e-12);
    assert_eq!(explanation.bias, model.bias);
}
