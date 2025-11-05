use medi_ai::RiskScorer;

#[test]
fn load_model_from_json_file() {
    let tmp = tempfile::tempdir().expect("tmpdir");
    let path = tmp.path().join("model.json");
    std::fs::write(
        &path,
        r#"{"weights":[0.1,0.2],"bias":0.0,"model_name":"m"}"#,
    )
    .expect("write");
    let model = RiskScorer::load_model(&path).expect("load");
    assert_eq!(model.model_name, "m");
    assert_eq!(model.weights, vec![0.1, 0.2]);
}
