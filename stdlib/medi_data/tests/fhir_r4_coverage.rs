use medi_data::fhir::{
    condition_from_json, condition_to_json, diagnostic_report_from_json, diagnostic_report_to_json,
    encounter_from_json, encounter_to_json, medication_from_json, medication_to_json,
    observation_from_json, observation_to_json, patient_from_json, patient_to_json,
    procedure_from_json, procedure_to_json, FHIRCondition, FHIRDiagnosticReport, FHIREncounter,
    FHIRMedication, FHIRObservation, FHIRPatient, FHIRProcedure,
};
use medi_data::validate::{
    validate_condition, validate_diagnostic_report, validate_encounter, validate_medication,
    validate_observation, validate_patient, validate_procedure,
};

#[test]
fn patient_json_roundtrip_and_validation() {
    let p = FHIRPatient {
        id: "p1".into(),
        given_name: Some("Jane".into()),
        family_name: Some("Doe".into()),
        birth_date: Some("1985-12-24".into()),
    };
    validate_patient(&p).unwrap();

    let json = patient_to_json(&p).unwrap();
    let p2 = patient_from_json(&json).unwrap();
    assert_eq!(p, p2);
}

#[test]
fn observation_json_roundtrip_and_validation() {
    let o = FHIRObservation {
        id: "o1".into(),
        code: "HR".into(),
        value: Some(72.0),
        unit: Some("bpm".into()),
    };
    validate_observation(&o).unwrap();

    let json = observation_to_json(&o).unwrap();
    let o2 = observation_from_json(&json).unwrap();
    assert_eq!(o, o2);
}

#[test]
fn medication_json_roundtrip_and_validation() {
    let m = FHIRMedication {
        id: "m1".into(),
        code: "RX123".into(),
        form: Some("tablet".into()),
        ingredients: vec!["acetaminophen".into()],
    };
    validate_medication(&m).unwrap();

    let json = medication_to_json(&m).unwrap();
    let m2 = medication_from_json(&json).unwrap();
    assert_eq!(m, m2);
}

#[test]
fn procedure_json_roundtrip_and_validation() {
    let p = FHIRProcedure {
        id: "pr1".into(),
        code: "PROC".into(),
        performed_date: Some("2025-01-01".into()),
        subject: "p1".into(),
    };
    validate_procedure(&p).unwrap();

    let json = procedure_to_json(&p).unwrap();
    let p2 = procedure_from_json(&json).unwrap();
    assert_eq!(p, p2);
}

#[test]
fn condition_json_roundtrip_and_validation() {
    let c = FHIRCondition {
        id: "c1".into(),
        code: "I10".into(),
        clinical_status: "active".into(),
        verification_status: "confirmed".into(),
        subject: "p1".into(),
    };
    validate_condition(&c).unwrap();

    let json = condition_to_json(&c).unwrap();
    let c2 = condition_from_json(&json).unwrap();
    assert_eq!(c, c2);
}

#[test]
fn encounter_json_roundtrip_and_validation() {
    let e = FHIREncounter {
        id: "e1".into(),
        class_: Some("outpatient".into()),
        start_date: Some("2025-01-01".into()),
        end_date: Some("2025-01-02".into()),
        subject: "p1".into(),
    };
    validate_encounter(&e).unwrap();

    let json = encounter_to_json(&e).unwrap();
    let e2 = encounter_from_json(&json).unwrap();
    assert_eq!(e, e2);
}

#[test]
fn diagnostic_report_json_roundtrip_and_validation() {
    let r = FHIRDiagnosticReport {
        id: "dr1".into(),
        code: "LAB".into(),
        result_codes: vec!["HB".into(), "WBC".into()],
        subject: "p1".into(),
    };
    validate_diagnostic_report(&r).unwrap();

    let json = diagnostic_report_to_json(&r).unwrap();
    let r2 = diagnostic_report_from_json(&json).unwrap();
    assert_eq!(r, r2);
}
