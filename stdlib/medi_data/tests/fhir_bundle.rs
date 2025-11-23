use medi_data::{BundleType, FHIRAny, FHIRBundle, FHIRObservation, FHIRPatient};

#[test]
fn bundle_roundtrip_and_validate() {
    let p = FHIRPatient {
        id: "p1".into(),
        given_name: Some("Jane".into()),
        family_name: Some("Doe".into()),
        birth_date: None,
    };
    let o = FHIRObservation {
        id: "o1".into(),
        code: "heart-rate".into(),
        value: Some(72.0),
        unit: Some("bpm".into()),
    };

    let b = FHIRBundle::new(
        BundleType::Collection,
        vec![FHIRAny::Patient(p.clone()), FHIRAny::Observation(o.clone())],
    );
    b.validate().unwrap();

    let s = b.to_json().unwrap();
    let back = FHIRBundle::from_json(&s).unwrap();
    assert_eq!(back.bundle_type, BundleType::Collection);
    assert_eq!(back.entries.len(), 2);

    // Extract patients only
    let patients: Vec<&FHIRAny> = back.extract(|e| matches!(e, FHIRAny::Patient(_)));
    assert_eq!(patients.len(), 1);
}

#[test]
fn transaction_requires_entries() {
    let b = FHIRBundle::new(BundleType::Transaction, vec![]);
    let err = b.validate().unwrap_err();
    assert!(err.to_string().contains("at least one entry"));
}
