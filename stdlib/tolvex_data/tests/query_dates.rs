use tolvex_data::{fhir::FHIRPatient, fhir_query};

#[test]
fn or_groups_and_date_filters_for_patients() {
    let data = vec![
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
        FHIRPatient {
            id: "p3".into(),
            given_name: Some("Grace".into()),
            family_name: Some("Hopper".into()),
            birth_date: Some("1906-12-09".into()),
        },
    ];

    // (family_name contains "Tur" AND birth_date >= 1900-01-01) OR (id == p1)
    let q = fhir_query("Patient")
        .filter_contains("family_name", "Tur")
        .filter_date_ge("birth_date", "1900-01-01")
        .or_group()
        .filter_eq("id", "p1")
        .build();

    let out = q.execute_patients(&data);
    let ids: Vec<&str> = out.iter().map(|p| p.id.as_str()).collect();
    assert_eq!(ids.len(), 2);
    assert!(ids.contains(&"p1"));
    assert!(ids.contains(&"p2"));
}
