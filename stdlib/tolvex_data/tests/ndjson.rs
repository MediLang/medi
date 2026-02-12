use tolvex_data::{
    fhir::FHIRPatient,
    ndjson::{from_ndjson, to_ndjson},
};

#[test]
fn roundtrip_ndjson_patients() {
    let items = vec![
        FHIRPatient {
            id: "p1".into(),
            given_name: Some("Ada".into()),
            family_name: Some("Lovelace".into()),
            birth_date: Some("1815-12-10".into()),
        },
        FHIRPatient {
            id: "p2".into(),
            given_name: Some("Alan".into()),
            family_name: Some("Turing".into()),
            birth_date: Some("1912-06-23".into()),
        },
    ];
    let s = to_ndjson(&items).expect("to_ndjson");
    let back: Vec<FHIRPatient> = from_ndjson(&s).expect("from_ndjson");
    assert_eq!(items, back);
}
