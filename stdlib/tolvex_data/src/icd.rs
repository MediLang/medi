use std::collections::BTreeMap;

#[derive(Debug, Clone, PartialEq)]
pub struct IcdCode {
    pub code: String,
    pub description: String,
    pub category: String,
    pub parent: Option<String>,
    pub children: Vec<String>,
    pub version: IcdVersion,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IcdVersion {
    ICD10,
    ICD11,
}

#[derive(Debug, Clone)]
pub struct IcdDatabase {
    codes: BTreeMap<String, IcdCode>,
}

impl IcdDatabase {
    pub fn new() -> Self {
        Self {
            codes: BTreeMap::new(),
        }
    }

    pub fn add_code(&mut self, code: IcdCode) {
        self.codes.insert(code.code.clone(), code);
    }

    pub fn lookup(&self, code: &str) -> Option<IcdCode> {
        self.codes.get(code).cloned()
    }

    pub fn search_by_description(&self, description: &str) -> Vec<IcdCode> {
        let desc_lower = description.to_lowercase();
        self.codes
            .values()
            .filter(|c| c.description.to_lowercase().contains(&desc_lower))
            .cloned()
            .collect()
    }

    pub fn search_by_category(&self, category: &str) -> Vec<IcdCode> {
        self.codes
            .values()
            .filter(|c| c.category == category)
            .cloned()
            .collect()
    }

    pub fn is_valid_code(&self, code: &str) -> bool {
        self.codes.contains_key(code)
    }

    pub fn get_hierarchy(&self, code: &str) -> Option<(Option<String>, Vec<String>)> {
        self.codes
            .get(code)
            .map(|c| (c.parent.clone(), c.children.clone()))
    }

    pub fn count(&self) -> usize {
        self.codes.len()
    }

    pub fn get_by_version(&self, version: IcdVersion) -> Vec<IcdCode> {
        self.codes
            .values()
            .filter(|c| c.version == version)
            .cloned()
            .collect()
    }

    pub fn get_parent(&self, code: &str) -> Option<IcdCode> {
        self.codes
            .get(code)
            .and_then(|c| c.parent.as_ref())
            .and_then(|parent_code| self.codes.get(parent_code).cloned())
    }

    pub fn get_children(&self, code: &str) -> Vec<IcdCode> {
        self.codes
            .get(code)
            .map(|c| {
                c.children
                    .iter()
                    .filter_map(|child_code| self.codes.get(child_code).cloned())
                    .collect()
            })
            .unwrap_or_default()
    }
}

impl Default for IcdDatabase {
    fn default() -> Self {
        Self::new()
    }
}

pub fn create_default_icd_db() -> IcdDatabase {
    let mut db = IcdDatabase::new();

    db.add_code(IcdCode {
        code: "A00".to_string(),
        description: "Cholera".to_string(),
        category: "Infectious and parasitic diseases".to_string(),
        parent: None,
        children: vec!["A00.0".to_string(), "A00.1".to_string()],
        version: IcdVersion::ICD10,
    });

    db.add_code(IcdCode {
        code: "A00.0".to_string(),
        description: "Cholera with vibrio cholerae 01 biotype cholerae".to_string(),
        category: "Infectious and parasitic diseases".to_string(),
        parent: Some("A00".to_string()),
        children: vec![],
        version: IcdVersion::ICD10,
    });

    db.add_code(IcdCode {
        code: "A00.1".to_string(),
        description: "Cholera with vibrio cholerae 01 biotype el tor".to_string(),
        category: "Infectious and parasitic diseases".to_string(),
        parent: Some("A00".to_string()),
        children: vec![],
        version: IcdVersion::ICD10,
    });

    db.add_code(IcdCode {
        code: "E10".to_string(),
        description: "Type 1 diabetes mellitus".to_string(),
        category: "Endocrine, nutritional and metabolic diseases".to_string(),
        parent: None,
        children: vec!["E10.0".to_string(), "E10.1".to_string()],
        version: IcdVersion::ICD10,
    });

    db.add_code(IcdCode {
        code: "E10.0".to_string(),
        description: "Type 1 diabetes mellitus with hyperosmolarity".to_string(),
        category: "Endocrine, nutritional and metabolic diseases".to_string(),
        parent: Some("E10".to_string()),
        children: vec![],
        version: IcdVersion::ICD10,
    });

    db.add_code(IcdCode {
        code: "E10.1".to_string(),
        description: "Type 1 diabetes mellitus with ketoacidosis".to_string(),
        category: "Endocrine, nutritional and metabolic diseases".to_string(),
        parent: Some("E10".to_string()),
        children: vec![],
        version: IcdVersion::ICD10,
    });

    db.add_code(IcdCode {
        code: "I10".to_string(),
        description: "Essential (primary) hypertension".to_string(),
        category: "Diseases of the circulatory system".to_string(),
        parent: None,
        children: vec![],
        version: IcdVersion::ICD10,
    });

    db.add_code(IcdCode {
        code: "J45".to_string(),
        description: "Asthma".to_string(),
        category: "Diseases of the respiratory system".to_string(),
        parent: None,
        children: vec!["J45.0".to_string(), "J45.1".to_string()],
        version: IcdVersion::ICD10,
    });

    db.add_code(IcdCode {
        code: "J45.0".to_string(),
        description: "Predominantly allergic asthma".to_string(),
        category: "Diseases of the respiratory system".to_string(),
        parent: Some("J45".to_string()),
        children: vec![],
        version: IcdVersion::ICD10,
    });

    db.add_code(IcdCode {
        code: "J45.1".to_string(),
        description: "Nonallergic asthma".to_string(),
        category: "Diseases of the respiratory system".to_string(),
        parent: Some("J45".to_string()),
        children: vec![],
        version: IcdVersion::ICD10,
    });

    db.add_code(IcdCode {
        code: "BA80".to_string(),
        description: "Diabetes mellitus".to_string(),
        category: "Endocrine, nutritional and metabolic diseases".to_string(),
        parent: None,
        children: vec!["BA80.0".to_string()],
        version: IcdVersion::ICD11,
    });

    db.add_code(IcdCode {
        code: "BA80.0".to_string(),
        description: "Type 1 diabetes mellitus".to_string(),
        category: "Endocrine, nutritional and metabolic diseases".to_string(),
        parent: Some("BA80".to_string()),
        children: vec![],
        version: IcdVersion::ICD11,
    });

    db
}

#[derive(Debug, Clone, PartialEq)]
pub struct IcdError {
    pub message: String,
}

impl IcdError {
    pub fn new(msg: impl Into<String>) -> Self {
        Self {
            message: msg.into(),
        }
    }
}

pub fn validate_icd10_code(code: &str) -> Result<(), IcdError> {
    if code.trim().is_empty() {
        return Err(IcdError::new("ICD-10 code must not be empty"));
    }

    if code.len() < 3 || code.len() > 7 {
        return Err(IcdError::new(
            "ICD-10 code must be between 3 and 7 characters",
        ));
    }

    let first_char = code.chars().next().unwrap_or(' ');
    if !first_char.is_ascii_uppercase() {
        return Err(IcdError::new(
            "ICD-10 code must start with uppercase letter",
        ));
    }

    Ok(())
}

pub fn validate_icd11_code(code: &str) -> Result<(), IcdError> {
    if code.trim().is_empty() {
        return Err(IcdError::new("ICD-11 code must not be empty"));
    }

    if code.len() < 3 || code.len() > 8 {
        return Err(IcdError::new(
            "ICD-11 code must be between 3 and 8 characters",
        ));
    }

    let first_char = code.chars().next().unwrap_or(' ');
    if !first_char.is_ascii_uppercase() {
        return Err(IcdError::new(
            "ICD-11 code must start with uppercase letter",
        ));
    }

    Ok(())
}

pub fn validate_icd_description(description: &str) -> Result<(), IcdError> {
    if description.trim().is_empty() {
        return Err(IcdError::new("ICD description must not be empty"));
    }

    if description.len() > 500 {
        return Err(IcdError::new(
            "ICD description must not exceed 500 characters",
        ));
    }

    Ok(())
}
