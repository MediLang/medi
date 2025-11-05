use medi_data::{
    fhir::FHIRPatient,
    validate::{validate_fhir, validate_patient},
};

#[test]
fn validate_patient_id_required() {
    let p = FHIRPatient {
        id: "".into(),
        given_name: None,
        family_name: None,
        birth_date: None,
    };
    let err = validate_fhir(&p).unwrap_err();
    assert!(err.message.contains("id"));
}

#[test]
fn validate_patient_birth_date_formats() {
    let p_ok1 = FHIRPatient {
        id: "p1".into(),
        given_name: Some("Ada".into()),
        family_name: None,
        birth_date: Some("1980-01-02".into()),
    };
    let p_ok2 = FHIRPatient {
        id: "p2".into(),
        given_name: None,
        family_name: Some("Lovelace".into()),
        birth_date: Some("19800102".into()),
    };
    let p_bad = FHIRPatient {
        id: "p3".into(),
        given_name: Some("Ada".into()),
        family_name: None,
        birth_date: Some("1980/01/02".into()),
    };

    validate_patient(&p_ok1).expect("ok1");
    validate_patient(&p_ok2).expect("ok2");
    let err = validate_patient(&p_bad).unwrap_err();
    assert!(err.message.contains("birth_date"));
}
