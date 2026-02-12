use tolvex_data::{
    fhir::FHIRObservation,
    sanitize::{canonize_observation_code, normalize_observation_unit},
};

#[test]
fn observation_unit_and_code_sanitization() {
    let mut o = FHIRObservation {
        id: "o1".into(),
        code: "  hr ".into(),
        value: Some(72.0),
        unit: Some(" BPM ".into()),
    };
    canonize_observation_code(&mut o);
    normalize_observation_unit(&mut o);
    assert_eq!(o.code, "HR");
    assert_eq!(o.unit.as_deref(), Some("bpm"));
}
