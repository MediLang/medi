use tolvex_data::{fhir::FHIRObservation, validate::validate_observation};

#[test]
fn observation_validation_rules() {
    // ok
    let ok = FHIRObservation {
        id: "o1".into(),
        code: "HR".into(),
        value: Some(72.0),
        unit: Some("bpm".into()),
    };
    validate_observation(&ok).expect("ok");

    // empty id
    let bad_id = FHIRObservation {
        id: " ".into(),
        code: "HR".into(),
        value: None,
        unit: None,
    };
    assert!(validate_observation(&bad_id)
        .unwrap_err()
        .message
        .contains("id"));

    // empty code
    let bad_code = FHIRObservation {
        id: "o2".into(),
        code: "".into(),
        value: None,
        unit: None,
    };
    assert!(validate_observation(&bad_code)
        .unwrap_err()
        .message
        .contains("code"));

    // non-finite value
    let bad_val = FHIRObservation {
        id: "o3".into(),
        code: "HR".into(),
        value: Some(f64::NAN),
        unit: None,
    };
    assert!(validate_observation(&bad_val)
        .unwrap_err()
        .message
        .contains("finite"));

    // empty unit
    let bad_unit = FHIRObservation {
        id: "o4".into(),
        code: "HR".into(),
        value: Some(60.0),
        unit: Some(" ".into()),
    };
    assert!(validate_observation(&bad_unit)
        .unwrap_err()
        .message
        .contains("unit"));
}
