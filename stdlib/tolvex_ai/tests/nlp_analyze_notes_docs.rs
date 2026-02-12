use tolvex_ai::nlp::analyze_notes;

#[test]
fn docs_style_analyze_notes_extracts_expected_entities() {
    let clinical_notes = "Patient reports cough and fever. Currently taking metformin and aspirin. Condition has improved.";

    let extract = vec![
        "symptoms".to_string(),
        "medications".to_string(),
        "sentiment".to_string(),
    ];

    let analysis = analyze_notes(clinical_notes, &extract);

    assert!(analysis.symptoms.contains(&"cough".to_string()));
    assert!(analysis.symptoms.contains(&"fever".to_string()));

    assert!(analysis.medications.contains(&"metformin".to_string()));
    assert!(analysis.medications.contains(&"aspirin".to_string()));

    assert_eq!(analysis.sentiment.as_deref(), Some("positive"));

    let again = analyze_notes(clinical_notes, &extract);
    assert_eq!(analysis.symptoms, again.symptoms);
    assert_eq!(analysis.medications, again.medications);
    assert_eq!(analysis.sentiment, again.sentiment);
}
