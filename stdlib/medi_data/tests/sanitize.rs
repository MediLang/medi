use medi_data::{
    fhir::FHIRPatient,
    sanitize::{normalize_patient_birth_date, trim_patient_names},
};

#[test]
fn sanitize_names_and_birth_date() {
    let mut p = FHIRPatient {
        id: "p1".into(),
        given_name: Some("  Ada ".into()),
        family_name: Some("  Lovelace  ".into()),
        birth_date: Some("1815-12-10".into()),
    };
    trim_patient_names(&mut p);
    assert_eq!(p.given_name.as_deref(), Some("Ada"));
    assert_eq!(p.family_name.as_deref(), Some("Lovelace"));
    normalize_patient_birth_date(&mut p);
    assert_eq!(p.birth_date.as_deref(), Some("18151210"));
}
