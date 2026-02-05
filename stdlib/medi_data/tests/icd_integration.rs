use medi_data::icd::{
    create_default_icd_db, validate_icd10_code, validate_icd11_code, validate_icd_description,
    IcdDatabase, IcdVersion,
};

#[test]
fn icd_db_lookup_cholera() {
    let db = create_default_icd_db();
    let code = db.lookup("A00").expect("code found");
    assert_eq!(code.code, "A00");
    assert_eq!(code.description, "Cholera");
    assert_eq!(code.version, IcdVersion::ICD10);
}

#[test]
fn icd_db_lookup_diabetes_type1() {
    let db = create_default_icd_db();
    let code = db.lookup("E10").expect("code found");
    assert_eq!(code.code, "E10");
    assert_eq!(code.description, "Type 1 diabetes mellitus");
    assert_eq!(code.version, IcdVersion::ICD10);
}

#[test]
fn icd_db_lookup_hypertension() {
    let db = create_default_icd_db();
    let code = db.lookup("I10").expect("code found");
    assert_eq!(code.code, "I10");
    assert_eq!(code.description, "Essential (primary) hypertension");
}

#[test]
fn icd_db_lookup_asthma() {
    let db = create_default_icd_db();
    let code = db.lookup("J45").expect("code found");
    assert_eq!(code.code, "J45");
    assert_eq!(code.description, "Asthma");
}

#[test]
fn icd_db_lookup_nonexistent() {
    let db = create_default_icd_db();
    assert!(db.lookup("ZZZ").is_none());
}

#[test]
fn icd_db_search_by_description_diabetes() {
    let db = create_default_icd_db();
    let results = db.search_by_description("diabetes");
    assert!(!results.is_empty());
    assert!(results.iter().any(|c| c.code == "E10"));
}

#[test]
fn icd_db_search_by_description_asthma() {
    let db = create_default_icd_db();
    let results = db.search_by_description("asthma");
    assert!(!results.is_empty());
    assert!(results.iter().any(|c| c.code == "J45"));
}

#[test]
fn icd_db_search_by_category() {
    let db = create_default_icd_db();
    let results = db.search_by_category("Infectious and parasitic diseases");
    assert!(!results.is_empty());
    assert!(results.iter().any(|c| c.code == "A00"));
}

#[test]
fn icd_db_is_valid_code() {
    let db = create_default_icd_db();
    assert!(db.is_valid_code("A00"));
    assert!(db.is_valid_code("E10"));
    assert!(!db.is_valid_code("ZZZ"));
}

#[test]
fn icd_db_get_hierarchy() {
    let db = create_default_icd_db();
    let (parent, children) = db.get_hierarchy("A00").expect("hierarchy found");
    assert_eq!(parent, None);
    assert_eq!(children.len(), 2);
    assert!(children.contains(&"A00.0".to_string()));
}

#[test]
fn icd_db_count() {
    let db = create_default_icd_db();
    assert_eq!(db.count(), 12);
}

#[test]
fn icd_db_get_by_version_icd10() {
    let db = create_default_icd_db();
    let results = db.get_by_version(IcdVersion::ICD10);
    assert!(!results.is_empty());
    assert!(results.iter().all(|c| c.version == IcdVersion::ICD10));
}

#[test]
fn icd_db_get_by_version_icd11() {
    let db = create_default_icd_db();
    let results = db.get_by_version(IcdVersion::ICD11);
    assert!(!results.is_empty());
    assert!(results.iter().all(|c| c.version == IcdVersion::ICD11));
}

#[test]
fn icd_db_get_parent() {
    let db = create_default_icd_db();
    let parent = db.get_parent("A00.0").expect("parent found");
    assert_eq!(parent.code, "A00");
}

#[test]
fn icd_db_get_parent_root() {
    let db = create_default_icd_db();
    assert!(db.get_parent("A00").is_none());
}

#[test]
fn icd_db_get_children() {
    let db = create_default_icd_db();
    let children = db.get_children("A00");
    assert_eq!(children.len(), 2);
    assert!(children.iter().any(|c| c.code == "A00.0"));
    assert!(children.iter().any(|c| c.code == "A00.1"));
}

#[test]
fn icd_db_get_children_leaf() {
    let db = create_default_icd_db();
    let children = db.get_children("A00.0");
    assert_eq!(children.len(), 0);
}

#[test]
fn icd_db_new_empty() {
    let db = IcdDatabase::new();
    assert_eq!(db.count(), 0);
    assert!(db.lookup("A00").is_none());
}

#[test]
fn icd_db_default_trait() {
    let db = IcdDatabase::default();
    assert_eq!(db.count(), 0);
}

#[test]
fn validate_icd10_code_valid_a00() {
    assert!(validate_icd10_code("A00").is_ok());
}

#[test]
fn validate_icd10_code_valid_e10() {
    assert!(validate_icd10_code("E10").is_ok());
}

#[test]
fn validate_icd10_code_valid_with_decimal() {
    assert!(validate_icd10_code("A00.0").is_ok());
}

#[test]
fn validate_icd10_code_empty() {
    assert!(validate_icd10_code("").is_err());
}

#[test]
fn validate_icd10_code_whitespace() {
    assert!(validate_icd10_code("   ").is_err());
}

#[test]
fn validate_icd10_code_too_short() {
    assert!(validate_icd10_code("AB").is_err());
}

#[test]
fn validate_icd10_code_too_long() {
    assert!(validate_icd10_code("ABCDEFGH").is_err());
}

#[test]
fn validate_icd10_code_lowercase() {
    assert!(validate_icd10_code("a00").is_err());
}

#[test]
fn validate_icd10_code_starts_with_number() {
    assert!(validate_icd10_code("100").is_err());
}

#[test]
fn validate_icd11_code_valid_ba80() {
    assert!(validate_icd11_code("BA80").is_ok());
}

#[test]
fn validate_icd11_code_valid_long() {
    assert!(validate_icd11_code("BA80000").is_ok());
}

#[test]
fn validate_icd11_code_empty() {
    assert!(validate_icd11_code("").is_err());
}

#[test]
fn validate_icd11_code_whitespace() {
    assert!(validate_icd11_code("   ").is_err());
}

#[test]
fn validate_icd11_code_too_short() {
    assert!(validate_icd11_code("AB").is_err());
}

#[test]
fn validate_icd11_code_too_long() {
    assert!(validate_icd11_code("ABCDEFGHI").is_err());
}

#[test]
fn validate_icd11_code_lowercase() {
    assert!(validate_icd11_code("ba80").is_err());
}

#[test]
fn validate_icd_description_valid() {
    assert!(validate_icd_description("Cholera").is_ok());
}

#[test]
fn validate_icd_description_valid_long() {
    let long_desc = "A".repeat(500);
    assert!(validate_icd_description(&long_desc).is_ok());
}

#[test]
fn validate_icd_description_empty() {
    assert!(validate_icd_description("").is_err());
}

#[test]
fn validate_icd_description_whitespace() {
    assert!(validate_icd_description("   ").is_err());
}

#[test]
fn validate_icd_description_too_long() {
    let long_desc = "A".repeat(501);
    assert!(validate_icd_description(&long_desc).is_err());
}

#[test]
fn icd_db_lookup_cholera_subtype_cholerae() {
    let db = create_default_icd_db();
    let code = db.lookup("A00.0").expect("code found");
    assert_eq!(code.code, "A00.0");
    assert_eq!(code.parent, Some("A00".to_string()));
}

#[test]
fn icd_db_lookup_cholera_subtype_el_tor() {
    let db = create_default_icd_db();
    let code = db.lookup("A00.1").expect("code found");
    assert_eq!(code.code, "A00.1");
    assert_eq!(code.parent, Some("A00".to_string()));
}

#[test]
fn icd_db_lookup_diabetes_hyperosmolarity() {
    let db = create_default_icd_db();
    let code = db.lookup("E10.0").expect("code found");
    assert_eq!(code.code, "E10.0");
    assert_eq!(
        code.description,
        "Type 1 diabetes mellitus with hyperosmolarity"
    );
}

#[test]
fn icd_db_lookup_diabetes_ketoacidosis() {
    let db = create_default_icd_db();
    let code = db.lookup("E10.1").expect("code found");
    assert_eq!(code.code, "E10.1");
    assert_eq!(
        code.description,
        "Type 1 diabetes mellitus with ketoacidosis"
    );
}

#[test]
fn icd_db_lookup_asthma_allergic() {
    let db = create_default_icd_db();
    let code = db.lookup("J45.0").expect("code found");
    assert_eq!(code.code, "J45.0");
    assert_eq!(code.description, "Predominantly allergic asthma");
}

#[test]
fn icd_db_lookup_asthma_nonallergic() {
    let db = create_default_icd_db();
    let code = db.lookup("J45.1").expect("code found");
    assert_eq!(code.code, "J45.1");
    assert_eq!(code.description, "Nonallergic asthma");
}

#[test]
fn icd_db_lookup_icd11_diabetes() {
    let db = create_default_icd_db();
    let code = db.lookup("BA80").expect("code found");
    assert_eq!(code.code, "BA80");
    assert_eq!(code.version, IcdVersion::ICD11);
}

#[test]
fn icd_db_search_case_insensitive() {
    let db = create_default_icd_db();
    let results_lower = db.search_by_description("cholera");
    let results_upper = db.search_by_description("CHOLERA");
    let results_mixed = db.search_by_description("ChOlErA");
    assert_eq!(results_lower.len(), results_upper.len());
    assert_eq!(results_lower.len(), results_mixed.len());
}

#[test]
fn icd_db_hierarchy_chain() {
    let db = create_default_icd_db();
    let parent_of_child = db.get_parent("A00.0").expect("parent found");
    assert_eq!(parent_of_child.code, "A00");
    let grandparent = db.get_parent(&parent_of_child.code);
    assert!(grandparent.is_none());
}
