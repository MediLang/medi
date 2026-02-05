use std::collections::BTreeMap;

#[derive(Debug, Clone, PartialEq)]
pub struct SnomedConcept {
    pub code: String,
    pub term: String,
    pub hierarchy: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct SnomedDatabase {
    concepts: BTreeMap<String, SnomedConcept>,
}

impl SnomedDatabase {
    pub fn new() -> Self {
        Self {
            concepts: BTreeMap::new(),
        }
    }

    pub fn add_concept(&mut self, code: String, term: String, hierarchy: Vec<String>) {
        self.concepts.insert(
            code.clone(),
            SnomedConcept {
                code,
                term,
                hierarchy,
            },
        );
    }

    pub fn lookup(&self, code: &str) -> Option<SnomedConcept> {
        self.concepts.get(code).cloned()
    }

    pub fn search_by_term(&self, term: &str) -> Vec<SnomedConcept> {
        let term_lower = term.to_lowercase();
        self.concepts
            .values()
            .filter(|c| c.term.to_lowercase().contains(&term_lower))
            .cloned()
            .collect()
    }

    pub fn is_valid_code(&self, code: &str) -> bool {
        self.concepts.contains_key(code)
    }

    pub fn get_hierarchy(&self, code: &str) -> Option<Vec<String>> {
        self.concepts.get(code).map(|c| c.hierarchy.clone())
    }

    pub fn count(&self) -> usize {
        self.concepts.len()
    }
}

impl Default for SnomedDatabase {
    fn default() -> Self {
        Self::new()
    }
}

pub fn create_default_snomed_db() -> SnomedDatabase {
    let mut db = SnomedDatabase::new();

    db.add_concept(
        "80891009".to_string(),
        "Heart failure".to_string(),
        vec!["Disease".to_string(), "Cardiovascular disease".to_string()],
    );

    db.add_concept(
        "73211009".to_string(),
        "Diabetes mellitus".to_string(),
        vec!["Disease".to_string(), "Metabolic disease".to_string()],
    );

    db.add_concept(
        "38341003".to_string(),
        "Hypertension".to_string(),
        vec!["Disease".to_string(), "Cardiovascular disease".to_string()],
    );

    db.add_concept(
        "6142004".to_string(),
        "Hyperlipidemia".to_string(),
        vec!["Disease".to_string(), "Metabolic disease".to_string()],
    );

    db.add_concept(
        "195967001".to_string(),
        "Asthma".to_string(),
        vec!["Disease".to_string(), "Respiratory disease".to_string()],
    );

    db.add_concept(
        "413350009".to_string(),
        "Finding by site".to_string(),
        vec!["Finding".to_string()],
    );

    db.add_concept(
        "386053000".to_string(),
        "Evaluation procedure".to_string(),
        vec!["Procedure".to_string()],
    );

    db.add_concept(
        "71388002".to_string(),
        "Procedure".to_string(),
        vec!["Procedure".to_string()],
    );

    db.add_concept(
        "373873005".to_string(),
        "Pharmaceutical / biologic product".to_string(),
        vec!["Substance".to_string()],
    );

    db.add_concept(
        "763158003".to_string(),
        "Medicinal product".to_string(),
        vec![
            "Substance".to_string(),
            "Pharmaceutical product".to_string(),
        ],
    );

    db
}

#[derive(Debug, Clone, PartialEq)]
pub struct SnomedError {
    pub message: String,
}

impl SnomedError {
    pub fn new(msg: impl Into<String>) -> Self {
        Self {
            message: msg.into(),
        }
    }
}

pub fn validate_snomed_code(code: &str) -> Result<(), SnomedError> {
    if code.trim().is_empty() {
        return Err(SnomedError::new("SNOMED code must not be empty"));
    }

    if !code.chars().all(|c| c.is_ascii_digit()) {
        return Err(SnomedError::new("SNOMED code must contain only digits"));
    }

    if code.len() < 5 || code.len() > 18 {
        return Err(SnomedError::new(
            "SNOMED code must be between 5 and 18 digits",
        ));
    }

    Ok(())
}

pub fn validate_snomed_term(term: &str) -> Result<(), SnomedError> {
    if term.trim().is_empty() {
        return Err(SnomedError::new("SNOMED term must not be empty"));
    }

    if term.len() > 255 {
        return Err(SnomedError::new(
            "SNOMED term must not exceed 255 characters",
        ));
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validate_snomed_code_valid() {
        assert!(validate_snomed_code("80891009").is_ok());
        assert!(validate_snomed_code("73211009").is_ok());
    }

    #[test]
    fn test_validate_snomed_code_empty() {
        assert!(validate_snomed_code("").is_err());
        assert!(validate_snomed_code("   ").is_err());
    }

    #[test]
    fn test_validate_snomed_code_non_digit() {
        assert!(validate_snomed_code("8089100A").is_err());
        assert!(validate_snomed_code("80891-09").is_err());
    }

    #[test]
    fn test_validate_snomed_code_length() {
        assert!(validate_snomed_code("1234").is_err());
        assert!(validate_snomed_code("123456789012345678901").is_err());
    }

    #[test]
    fn test_validate_snomed_term_valid() {
        assert!(validate_snomed_term("Heart failure").is_ok());
        assert!(validate_snomed_term("Diabetes mellitus").is_ok());
    }

    #[test]
    fn test_validate_snomed_term_empty() {
        assert!(validate_snomed_term("").is_err());
        assert!(validate_snomed_term("   ").is_err());
    }

    #[test]
    fn test_validate_snomed_term_too_long() {
        let long_term = "a".repeat(256);
        assert!(validate_snomed_term(&long_term).is_err());
    }
}
