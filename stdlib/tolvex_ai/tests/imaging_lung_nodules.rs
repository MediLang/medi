use tolvex_ai::imaging::detect_lung_nodules;
use serde_json::json;

#[test]
fn docs_style_lung_nodule_detection_is_deterministic() {
    let patient_ct_scans = json!([
        {"id": 1, "slice": 0},
        {"id": 1, "slice": 1},
        {"id": 1, "slice": 2},
    ]);

    let result = detect_lung_nodules(&patient_ct_scans, "high", true);

    assert_eq!(result.sensitivity, "high");
    assert_eq!(result.num_suspected_nodules, 2);
    assert!(result.avg_confidence.is_some());

    let again = detect_lung_nodules(&patient_ct_scans, "high", true);
    assert_eq!(result.num_suspected_nodules, again.num_suspected_nodules);
    assert_eq!(result.avg_confidence, again.avg_confidence);
}
