use medi_data::fhir::{
    observation_from_json, observation_to_json, patient_from_json, patient_to_json,
    FHIRObservation, FHIRPatient,
};

#[test]
fn patient_json_roundtrip() {
    let p = FHIRPatient {
        id: "p1".into(),
        given_name: Some("Ada".into()),
        family_name: Some("Lovelace".into()),
        birth_date: Some("1815-12-10".into()),
    };
    let json = patient_to_json(&p).expect("serialize");
    let back = patient_from_json(&json).expect("deserialize");
    assert_eq!(p, back);
}

#[test]
fn observation_json_roundtrip() {
    let o = FHIRObservation {
        id: "o1".into(),
        code: "heart_rate".into(),
        value: Some(72.0),
        unit: Some("bpm".into()),
    };
    let json = observation_to_json(&o).expect("serialize");
    let back = observation_from_json(&json).expect("deserialize");
    assert_eq!(o, back);
}
