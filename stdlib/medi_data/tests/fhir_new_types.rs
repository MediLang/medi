use medi_data::validate::{
    validate_condition, validate_diagnostic_report, validate_encounter, validate_medication,
    validate_procedure,
};
use medi_data::{fhir::*, fhir_any::FHIRAny, query::fhir_query};

#[test]
fn medication_json_and_validate() {
    let med = FHIRMedication {
        id: "m1".into(),
        code: "RX123".into(),
        form: Some("tablet".into()),
        ingredients: vec!["ingA".into(), "ingB".into()],
    };
    validate_medication(&med).unwrap();
    let s = medication_to_json(&med).unwrap();
    let back = medication_from_json(&s).unwrap();
    assert_eq!(back, med);
}

#[test]
fn medication_validation_fails_on_empty_code() {
    let med = FHIRMedication {
        id: "m1".into(),
        code: "".into(),
        form: None,
        ingredients: vec![],
    };
    assert!(validate_medication(&med).is_err());
}

#[test]
fn procedure_json_and_validate() {
    let proc_ = FHIRProcedure {
        id: "p1".into(),
        code: "PROC".into(),
        performed_date: Some("20250101".into()),
        subject: "pat1".into(),
    };
    validate_procedure(&proc_).unwrap();
    let s = procedure_to_json(&proc_).unwrap();
    let back = procedure_from_json(&s).unwrap();
    assert_eq!(back, proc_);
}

#[test]
fn condition_json_and_validate() {
    let cond = FHIRCondition {
        id: "c1".into(),
        code: "COND".into(),
        clinical_status: "active".into(),
        verification_status: "confirmed".into(),
        subject: "pat1".into(),
    };
    validate_condition(&cond).unwrap();
    let s = condition_to_json(&cond).unwrap();
    let back = condition_from_json(&s).unwrap();
    assert_eq!(back, cond);
}

#[test]
fn encounter_json_and_validate() {
    let enc = FHIREncounter {
        id: "e1".into(),
        class_: Some("outpatient".into()),
        start_date: Some("2025-01-02".into()),
        end_date: None,
        subject: "pat1".into(),
    };
    validate_encounter(&enc).unwrap();
    let s = encounter_to_json(&enc).unwrap();
    let back = encounter_from_json(&s).unwrap();
    assert_eq!(back, enc);
}

#[test]
fn diagnostic_report_json_and_validate() {
    let rep = FHIRDiagnosticReport {
        id: "r1".into(),
        code: "DRPT".into(),
        result_codes: vec!["OBS1".into(), "OBS2".into()],
        subject: "pat1".into(),
    };
    validate_diagnostic_report(&rep).unwrap();
    let s = diagnostic_report_to_json(&rep).unwrap();
    let back = diagnostic_report_from_json(&s).unwrap();
    assert_eq!(back, rep);
}

#[test]
fn execute_any_matches_new_variants() {
    let med = FHIRMedication {
        id: "m1".into(),
        code: "RX123".into(),
        form: None,
        ingredients: vec![],
    };
    let proc_ = FHIRProcedure {
        id: "p1".into(),
        code: "PROC".into(),
        performed_date: None,
        subject: "pat1".into(),
    };
    let cond = FHIRCondition {
        id: "c1".into(),
        code: "COND".into(),
        clinical_status: "active".into(),
        verification_status: "confirmed".into(),
        subject: "pat1".into(),
    };
    let enc = FHIREncounter {
        id: "e1".into(),
        class_: Some("inpatient".into()),
        start_date: None,
        end_date: None,
        subject: "pat1".into(),
    };
    let rep = FHIRDiagnosticReport {
        id: "r1".into(),
        code: "DRPT".into(),
        result_codes: vec![],
        subject: "pat1".into(),
    };

    let any: Vec<FHIRAny> = vec![
        FHIRAny::Medication(med.clone()),
        FHIRAny::Procedure(proc_.clone()),
        FHIRAny::Condition(cond.clone()),
        FHIRAny::Encounter(enc.clone()),
        FHIRAny::DiagnosticReport(rep.clone()),
    ];
    let q_code = fhir_query("Any").filter_eq("code", "COND").build();
    let out = q_code.execute_any(&any);
    assert!(out
        .iter()
        .any(|a| matches!(a, FHIRAny::Condition(x) if x.id == "c1")));

    let q_subj = fhir_query("Any").filter_eq("subject", "pat1").build();
    let out2 = q_subj.execute_any(&any);
    assert!(out2.len() >= 3); // procedure, condition, encounter, report all match subject

    let q_form = fhir_query("Any").filter_eq("form", "tablet").build();
    let binding = vec![FHIRAny::Medication(FHIRMedication {
        id: "m2".into(),
        code: "RX".into(),
        form: Some("tablet".into()),
        ingredients: vec![],
    })];
    let out3 = q_form.execute_any(&binding);
    assert!(out3
        .iter()
        .any(|a| matches!(a, FHIRAny::Medication(x) if x.id == "m2")));
}
