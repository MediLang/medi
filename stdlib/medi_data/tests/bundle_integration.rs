use medi_data::{
    fhir::FHIRPatient,
    fhir_any::FHIRAny,
    fhir_bundle::{BundleType, FHIRBundle},
    storage::SecureStore,
    storage_file::FileStore,
};

#[test]
fn bundle_validate_and_store_flow() {
    // Create two simple patients
    let p1 = FHIRPatient {
        id: "p1".into(),
        family_name: Some("Doe".into()),
        given_name: Some("Jane".into()),
        birth_date: Some("1980-01-01".into()),
    };
    let p2 = FHIRPatient {
        id: "p2".into(),
        family_name: Some("Doe".into()),
        given_name: Some("John".into()),
        birth_date: Some("1979-05-05".into()),
    };

    // Build a collection bundle
    let bundle = FHIRBundle::collection()
        .with_entry(FHIRAny::Patient(p1.clone()))
        .with_entry(FHIRAny::Patient(p2.clone()));

    // Validate
    bundle.validate().expect("bundle valid");

    // Store each resource
    let tmp = tempfile::tempdir().expect("tmpdir");
    let store = FileStore::new(tmp.path()).expect("filestore");
    store.save("patient_p1", &p1).expect("save p1");
    store.save("patient_p2", &p2).expect("save p2");

    // Roundtrip JSON
    let s = serde_json::to_string(&bundle).expect("to json");
    let back: FHIRBundle = serde_json::from_str(&s).expect("from json");
    assert!(matches!(back.bundle_type, BundleType::Collection));
    assert_eq!(back.entries.len(), 2);
}
