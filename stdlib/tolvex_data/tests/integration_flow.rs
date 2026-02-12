use tolvex_data::{
    fhir::FHIRPatient, fhir_query, hl7::parse_hl7, hl7_fhir::hl7_to_fhir_patient_minimal,
    query::Predicate, storage::SecureStore, storage_file::FileStore, validate::validate_patient,
};

#[test]
fn hl7_to_fhir_validate_store_query_flow() {
    // Minimal HL7 with PID fields: PID-3 (id), PID-5 (name family^given), PID-7 (dob)
    let hl7 = "MSH|^~\\&|SRC|FAC|DST|HOSP|202501010101||ADT^A01|123|P|2.5\r\
               PID|1|ALTID|P12345||Doe^Jane||19851224|F\r";

    // Parse & convert
    let _ = parse_hl7(hl7).expect("hl7 parse ok");
    let patient: FHIRPatient = hl7_to_fhir_patient_minimal(hl7).expect("converted patient");

    // Validate
    validate_patient(&patient).expect("valid patient");

    // Store
    let tmp = tempfile::tempdir().expect("tmpdir");
    let store = FileStore::new(tmp.path()).expect("filestore");
    store.save("patient_", &patient).expect("save");

    // Query by OR group: (family_name contains CI "doe" OR given_name eq-ci "jane") AND birth_date <= 1985-12-31
    let q = fhir_query("Patient")
        .any_in_group(vec![
            Predicate::ContainsCi {
                key: "family_name".into(),
                value: "doe".into(),
            },
            Predicate::EqCi {
                key: "given_name".into(),
                value: "jane".into(),
            },
        ])
        .filter_date_le("birth_date", "1985-12-31")
        .build();

    let binding = [patient];
    let results = q.execute_patients(&binding);
    assert_eq!(results.len(), 1);
    assert_eq!(results[0].family_name.as_deref(), Some("Doe"));
    assert_eq!(results[0].given_name.as_deref(), Some("Jane"));
}
