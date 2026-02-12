use tolvex_data::loinc::{
    create_default_loinc_db, map_loinc_to_snomed, validate_loinc_code, validate_loinc_component,
    LoincDatabase,
};

#[test]
fn loinc_db_lookup_glucose() {
    let db = create_default_loinc_db();
    let code = db.lookup("2345-7").expect("code found");
    assert_eq!(code.code, "2345-7");
    assert_eq!(code.component, "Glucose");
    assert_eq!(code.system, "Serum or Plasma");
}

#[test]
fn loinc_db_lookup_sodium() {
    let db = create_default_loinc_db();
    let code = db.lookup("2951-2").expect("code found");
    assert_eq!(code.code, "2951-2");
    assert_eq!(code.component, "Sodium");
}

#[test]
fn loinc_db_lookup_potassium() {
    let db = create_default_loinc_db();
    let code = db.lookup("2823-3").expect("code found");
    assert_eq!(code.code, "2823-3");
    assert_eq!(code.component, "Potassium");
}

#[test]
fn loinc_db_lookup_creatinine() {
    let db = create_default_loinc_db();
    let code = db.lookup("2160-0").expect("code found");
    assert_eq!(code.code, "2160-0");
    assert_eq!(code.component, "Creatinine");
}

#[test]
fn loinc_db_lookup_nonexistent() {
    let db = create_default_loinc_db();
    assert!(db.lookup("9999-9").is_none());
}

#[test]
fn loinc_db_search_by_component_glucose() {
    let db = create_default_loinc_db();
    let results = db.search_by_component("Glucose");
    assert_eq!(results.len(), 1);
    assert_eq!(results[0].code, "2345-7");
}

#[test]
fn loinc_db_search_by_component_electrolytes() {
    let db = create_default_loinc_db();
    let results = db.search_by_component("Sodium");
    assert!(!results.is_empty());
    assert!(results.iter().any(|c| c.code == "2951-2"));
}

#[test]
fn loinc_db_search_by_long_name() {
    let db = create_default_loinc_db();
    let results = db.search_by_long_name("Glucose");
    assert_eq!(results.len(), 1);
    assert_eq!(results[0].code, "2345-7");
}

#[test]
fn loinc_db_search_by_long_name_serum() {
    let db = create_default_loinc_db();
    let results = db.search_by_long_name("Serum");
    assert!(!results.is_empty());
}

#[test]
fn loinc_db_is_valid_code() {
    let db = create_default_loinc_db();
    assert!(db.is_valid_code("2345-7"));
    assert!(db.is_valid_code("2951-2"));
    assert!(!db.is_valid_code("9999-9"));
}

#[test]
fn loinc_db_count() {
    let db = create_default_loinc_db();
    assert_eq!(db.count(), 10);
}

#[test]
fn loinc_db_get_by_system() {
    let db = create_default_loinc_db();
    let results = db.get_by_system("Serum or Plasma");
    assert!(!results.is_empty());
    assert!(results.iter().all(|c| c.system == "Serum or Plasma"));
}

#[test]
fn loinc_db_new_empty() {
    let db = LoincDatabase::new();
    assert_eq!(db.count(), 0);
    assert!(db.lookup("2345-7").is_none());
}

#[test]
fn loinc_db_default_trait() {
    let db = LoincDatabase::default();
    assert_eq!(db.count(), 0);
}

#[test]
fn validate_loinc_code_valid_2345_7() {
    assert!(validate_loinc_code("2345-7").is_ok());
}

#[test]
fn validate_loinc_code_valid_2951_2() {
    assert!(validate_loinc_code("2951-2").is_ok());
}

#[test]
fn validate_loinc_code_valid_5_digit() {
    assert!(validate_loinc_code("12345-6").is_ok());
}

#[test]
fn validate_loinc_code_empty() {
    assert!(validate_loinc_code("").is_err());
}

#[test]
fn validate_loinc_code_whitespace() {
    assert!(validate_loinc_code("   ").is_err());
}

#[test]
fn validate_loinc_code_no_dash() {
    assert!(validate_loinc_code("23457").is_err());
}

#[test]
fn validate_loinc_code_multiple_dashes() {
    assert!(validate_loinc_code("234-5-7").is_err());
}

#[test]
fn validate_loinc_code_non_digit_numeric() {
    assert!(validate_loinc_code("234A-7").is_err());
}

#[test]
fn validate_loinc_code_non_digit_check() {
    assert!(validate_loinc_code("2345-A").is_err());
}

#[test]
fn validate_loinc_code_too_short_numeric() {
    assert!(validate_loinc_code("234-7").is_err());
}

#[test]
fn validate_loinc_code_too_long_numeric() {
    assert!(validate_loinc_code("123456-7").is_err());
}

#[test]
fn validate_loinc_code_multiple_check_digits() {
    assert!(validate_loinc_code("2345-78").is_err());
}

#[test]
fn validate_loinc_component_valid() {
    assert!(validate_loinc_component("Glucose").is_ok());
}

#[test]
fn validate_loinc_component_valid_long() {
    let long_component = "A".repeat(255);
    assert!(validate_loinc_component(&long_component).is_ok());
}

#[test]
fn validate_loinc_component_empty() {
    assert!(validate_loinc_component("").is_err());
}

#[test]
fn validate_loinc_component_whitespace() {
    assert!(validate_loinc_component("   ").is_err());
}

#[test]
fn validate_loinc_component_too_long() {
    let long_component = "A".repeat(256);
    assert!(validate_loinc_component(&long_component).is_err());
}

#[test]
fn map_loinc_to_snomed_glucose() {
    let snomed = map_loinc_to_snomed("2345-7").expect("mapping found");
    assert_eq!(snomed, "80184007");
}

#[test]
fn map_loinc_to_snomed_sodium() {
    let snomed = map_loinc_to_snomed("2951-2").expect("mapping found");
    assert_eq!(snomed, "39802001");
}

#[test]
fn map_loinc_to_snomed_potassium() {
    let snomed = map_loinc_to_snomed("2823-3").expect("mapping found");
    assert_eq!(snomed, "87739001");
}

#[test]
fn map_loinc_to_snomed_creatinine() {
    let snomed = map_loinc_to_snomed("2160-0").expect("mapping found");
    assert_eq!(snomed, "70901008");
}

#[test]
fn map_loinc_to_snomed_nonexistent() {
    assert!(map_loinc_to_snomed("9999-9").is_none());
}

#[test]
fn loinc_db_lookup_bilirubin() {
    let db = create_default_loinc_db();
    let code = db.lookup("1975-2").expect("code found");
    assert_eq!(code.code, "1975-2");
    assert_eq!(code.component, "Bilirubin.total");
}

#[test]
fn loinc_db_lookup_albumin() {
    let db = create_default_loinc_db();
    let code = db.lookup("2986-3").expect("code found");
    assert_eq!(code.code, "2986-3");
    assert_eq!(code.component, "Albumin");
}

#[test]
fn loinc_db_search_case_insensitive() {
    let db = create_default_loinc_db();
    let results_lower = db.search_by_component("glucose");
    let results_upper = db.search_by_component("GLUCOSE");
    let results_mixed = db.search_by_component("GlUcOsE");
    assert_eq!(results_lower.len(), results_upper.len());
    assert_eq!(results_lower.len(), results_mixed.len());
}

#[test]
fn loinc_db_multiple_codes_same_system() {
    let db = create_default_loinc_db();
    let results = db.get_by_system("Serum or Plasma");
    assert!(results.len() >= 10);
    assert!(results.iter().all(|c| c.system == "Serum or Plasma"));
}

#[test]
fn loinc_db_lookup_chloride() {
    let db = create_default_loinc_db();
    let code = db.lookup("2075-0").expect("code found");
    assert_eq!(code.code, "2075-0");
    assert_eq!(code.component, "Chloride");
}

#[test]
fn loinc_db_lookup_bun() {
    let db = create_default_loinc_db();
    let code = db.lookup("3094-0").expect("code found");
    assert_eq!(code.code, "3094-0");
    assert_eq!(code.component, "Urea nitrogen");
}

#[test]
fn map_loinc_to_snomed_all_codes() {
    let codes = vec![
        ("2345-7", "80184007"),
        ("2951-2", "39802001"),
        ("2823-3", "87739001"),
        ("2075-0", "87839004"),
        ("3094-0", "3024322009"),
        ("2160-0", "70901008"),
        ("2885-2", "2890005"),
        ("2777-7", "24636002"),
        ("2986-3", "61789008"),
        ("1975-2", "14631000"),
    ];

    for (loinc, expected_snomed) in codes {
        let snomed = map_loinc_to_snomed(loinc).expect("mapping found");
        assert_eq!(snomed, expected_snomed);
    }
}
