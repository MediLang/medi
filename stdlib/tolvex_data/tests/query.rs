use tolvex_data::{fhir::FHIRPatient, fhir_query};

#[test]
fn query_patients_eq_and_contains() {
    let data = vec![
        FHIRPatient {
            id: "p1".into(),
            given_name: Some("Ada".into()),
            family_name: Some("Lovelace".into()),
            birth_date: None,
        },
        FHIRPatient {
            id: "p2".into(),
            given_name: Some("Alan".into()),
            family_name: Some("Turing".into()),
            birth_date: None,
        },
        FHIRPatient {
            id: "p3".into(),
            given_name: Some("Grace".into()),
            family_name: Some("Hopper".into()),
            birth_date: None,
        },
    ];

    let q = fhir_query("Patient")
        .filter_eq("id", "p2")
        .filter_contains("family_name", "Tur")
        .build();

    let out = q.execute_patients(&data);
    assert_eq!(out.len(), 1);
    assert_eq!(out[0].id, "p2");
}
