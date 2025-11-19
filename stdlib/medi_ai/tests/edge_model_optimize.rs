use medi_ai::{explain::explain, RiskScorer};

#[test]
fn optimize_preserves_prediction_behavior() {
    let model = RiskScorer {
        weights: vec![0.2, 0.3],
        bias: 0.1,
        model_name: "edge-test".into(),
    };

    let features = [1.0, 2.0];
    let base_pred = model.predict(&features);

    let edge_model = model.optimize("wearable", "wasm", true);

    let edge_pred = edge_model.inner.predict(&features);
    assert!((base_pred - edge_pred).abs() < 1e-12);

    let base_expl = explain(&model, &features);
    let edge_expl = explain(&edge_model.inner, &features);

    assert!((base_expl.prediction - edge_expl.prediction).abs() < 1e-12);
    assert_eq!(base_expl.contributions.len(), edge_expl.contributions.len());
}
