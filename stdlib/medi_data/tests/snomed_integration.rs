use medi_data::snomed::{
    create_default_snomed_db, validate_snomed_code, validate_snomed_term, SnomedDatabase,
};

#[test]
fn snomed_db_lookup_heart_failure() {
    let db = create_default_snomed_db();
    let concept = db.lookup("80891009").expect("concept found");
    assert_eq!(concept.code, "80891009");
    assert_eq!(concept.term, "Heart failure");
    assert_eq!(concept.hierarchy.len(), 2);
}

#[test]
fn snomed_db_lookup_diabetes() {
    let db = create_default_snomed_db();
    let concept = db.lookup("73211009").expect("concept found");
    assert_eq!(concept.code, "73211009");
    assert_eq!(concept.term, "Diabetes mellitus");
}

#[test]
fn snomed_db_lookup_hypertension() {
    let db = create_default_snomed_db();
    let concept = db.lookup("38341003").expect("concept found");
    assert_eq!(concept.code, "38341003");
    assert_eq!(concept.term, "Hypertension");
}

#[test]
fn snomed_db_lookup_nonexistent() {
    let db = create_default_snomed_db();
    assert!(db.lookup("99999999").is_none());
}

#[test]
fn snomed_db_search_by_term_diabetes() {
    let db = create_default_snomed_db();
    let results = db.search_by_term("diabetes");
    assert!(!results.is_empty());
    assert!(results.iter().any(|c| c.code == "73211009"));
}

#[test]
fn snomed_db_search_by_term_heart() {
    let db = create_default_snomed_db();
    let results = db.search_by_term("heart");
    assert_eq!(results.len(), 1);
    assert_eq!(results[0].code, "80891009");
}

#[test]
fn snomed_db_search_by_term_procedure() {
    let db = create_default_snomed_db();
    let results = db.search_by_term("procedure");
    assert!(!results.is_empty());
}

#[test]
fn snomed_db_is_valid_code() {
    let db = create_default_snomed_db();
    assert!(db.is_valid_code("80891009"));
    assert!(db.is_valid_code("73211009"));
    assert!(!db.is_valid_code("99999999"));
}

#[test]
fn snomed_db_get_hierarchy() {
    let db = create_default_snomed_db();
    let hierarchy = db.get_hierarchy("80891009").expect("hierarchy found");
    assert_eq!(hierarchy.len(), 2);
    assert_eq!(hierarchy[0], "Disease");
    assert_eq!(hierarchy[1], "Cardiovascular disease");
}

#[test]
fn snomed_db_count() {
    let db = create_default_snomed_db();
    assert_eq!(db.count(), 10);
}

#[test]
fn snomed_db_add_custom_concept() {
    let mut db = SnomedDatabase::new();
    db.add_concept(
        "12345678".to_string(),
        "Custom concept".to_string(),
        vec!["Category".to_string()],
    );
    assert!(db.is_valid_code("12345678"));
    let concept = db.lookup("12345678").expect("concept found");
    assert_eq!(concept.term, "Custom concept");
}

#[test]
fn snomed_db_search_case_insensitive() {
    let db = create_default_snomed_db();
    let results_lower = db.search_by_term("heart");
    let results_upper = db.search_by_term("HEART");
    let results_mixed = db.search_by_term("HeArT");
    assert_eq!(results_lower.len(), results_upper.len());
    assert_eq!(results_lower.len(), results_mixed.len());
}

#[test]
fn validate_snomed_code_valid_80891009() {
    assert!(validate_snomed_code("80891009").is_ok());
}

#[test]
fn validate_snomed_code_valid_73211009() {
    assert!(validate_snomed_code("73211009").is_ok());
}

#[test]
fn validate_snomed_code_valid_long() {
    assert!(validate_snomed_code("123456789012345678").is_ok());
}

#[test]
fn validate_snomed_code_empty() {
    assert!(validate_snomed_code("").is_err());
}

#[test]
fn validate_snomed_code_whitespace() {
    assert!(validate_snomed_code("   ").is_err());
}

#[test]
fn validate_snomed_code_too_short() {
    assert!(validate_snomed_code("1234").is_err());
}

#[test]
fn validate_snomed_code_too_long() {
    assert!(validate_snomed_code("1234567890123456789").is_err());
}

#[test]
fn validate_snomed_code_non_digit() {
    assert!(validate_snomed_code("8089100A").is_err());
}

#[test]
fn validate_snomed_code_with_dash() {
    assert!(validate_snomed_code("80891-009").is_err());
}

#[test]
fn validate_snomed_code_with_space() {
    assert!(validate_snomed_code("80891 009").is_err());
}

#[test]
fn validate_snomed_term_valid() {
    assert!(validate_snomed_term("Heart failure").is_ok());
}

#[test]
fn validate_snomed_term_valid_long() {
    let long_term = "A".repeat(255);
    assert!(validate_snomed_term(&long_term).is_ok());
}

#[test]
fn validate_snomed_term_empty() {
    assert!(validate_snomed_term("").is_err());
}

#[test]
fn validate_snomed_term_whitespace() {
    assert!(validate_snomed_term("   ").is_err());
}

#[test]
fn validate_snomed_term_too_long() {
    let long_term = "A".repeat(256);
    assert!(validate_snomed_term(&long_term).is_err());
}

#[test]
fn snomed_db_lookup_asthma() {
    let db = create_default_snomed_db();
    let concept = db.lookup("195967001").expect("concept found");
    assert_eq!(concept.code, "195967001");
    assert_eq!(concept.term, "Asthma");
}

#[test]
fn snomed_db_lookup_hyperlipidemia() {
    let db = create_default_snomed_db();
    let concept = db.lookup("6142004").expect("concept found");
    assert_eq!(concept.code, "6142004");
    assert_eq!(concept.term, "Hyperlipidemia");
}

#[test]
fn snomed_db_lookup_medicinal_product() {
    let db = create_default_snomed_db();
    let concept = db.lookup("763158003").expect("concept found");
    assert_eq!(concept.code, "763158003");
    assert_eq!(concept.term, "Medicinal product");
    assert!(concept
        .hierarchy
        .contains(&"Pharmaceutical product".to_string()));
}

#[test]
fn snomed_db_search_multiple_results() {
    let db = create_default_snomed_db();
    let results = db.search_by_term("product");
    assert!(results.len() >= 2);
}

#[test]
fn snomed_db_empty_search() {
    let db = create_default_snomed_db();
    let results = db.search_by_term("nonexistent_term_xyz");
    assert_eq!(results.len(), 0);
}

#[test]
fn snomed_db_multiple_concepts_same_hierarchy() {
    let db = create_default_snomed_db();
    let heart_failure = db.lookup("80891009").expect("heart failure");
    let hypertension = db.lookup("38341003").expect("hypertension");
    assert_eq!(heart_failure.hierarchy[1], "Cardiovascular disease");
    assert_eq!(hypertension.hierarchy[1], "Cardiovascular disease");
}

#[test]
fn snomed_db_new_empty() {
    let db = SnomedDatabase::new();
    assert_eq!(db.count(), 0);
    assert!(db.lookup("80891009").is_none());
}

#[test]
fn snomed_db_default_trait() {
    let db = SnomedDatabase::default();
    assert_eq!(db.count(), 0);
}
