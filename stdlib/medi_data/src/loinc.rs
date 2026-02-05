use std::collections::BTreeMap;

#[derive(Debug, Clone, PartialEq)]
pub struct LoincCode {
    pub code: String,
    pub long_name: String,
    pub short_name: String,
    pub component: String,
    pub property: String,
    pub time_aspect: String,
    pub system: String,
    pub scale_type: String,
    pub method: Option<String>,
}

#[derive(Debug, Clone)]
pub struct LoincDatabase {
    codes: BTreeMap<String, LoincCode>,
}

impl LoincDatabase {
    pub fn new() -> Self {
        Self {
            codes: BTreeMap::new(),
        }
    }

    pub fn add_code(&mut self, code: LoincCode) {
        self.codes.insert(code.code.clone(), code);
    }

    pub fn lookup(&self, code: &str) -> Option<LoincCode> {
        self.codes.get(code).cloned()
    }

    pub fn search_by_component(&self, component: &str) -> Vec<LoincCode> {
        let component_lower = component.to_lowercase();
        self.codes
            .values()
            .filter(|c| c.component.to_lowercase().contains(&component_lower))
            .cloned()
            .collect()
    }

    pub fn search_by_long_name(&self, name: &str) -> Vec<LoincCode> {
        let name_lower = name.to_lowercase();
        self.codes
            .values()
            .filter(|c| c.long_name.to_lowercase().contains(&name_lower))
            .cloned()
            .collect()
    }

    pub fn is_valid_code(&self, code: &str) -> bool {
        self.codes.contains_key(code)
    }

    pub fn count(&self) -> usize {
        self.codes.len()
    }

    pub fn get_by_system(&self, system: &str) -> Vec<LoincCode> {
        self.codes
            .values()
            .filter(|c| c.system == system)
            .cloned()
            .collect()
    }
}

impl Default for LoincDatabase {
    fn default() -> Self {
        Self::new()
    }
}

pub fn create_default_loinc_db() -> LoincDatabase {
    let mut db = LoincDatabase::new();

    db.add_code(LoincCode {
        code: "2345-7".to_string(),
        long_name: "Glucose [Mass/volume] in Serum or Plasma".to_string(),
        short_name: "Glucose Serum/Plasma".to_string(),
        component: "Glucose".to_string(),
        property: "Mass concentration".to_string(),
        time_aspect: "Pt".to_string(),
        system: "Serum or Plasma".to_string(),
        scale_type: "Qn".to_string(),
        method: None,
    });

    db.add_code(LoincCode {
        code: "2951-2".to_string(),
        long_name: "Sodium [Moles/volume] in Serum or Plasma".to_string(),
        short_name: "Sodium Serum/Plasma".to_string(),
        component: "Sodium".to_string(),
        property: "Substance concentration".to_string(),
        time_aspect: "Pt".to_string(),
        system: "Serum or Plasma".to_string(),
        scale_type: "Qn".to_string(),
        method: None,
    });

    db.add_code(LoincCode {
        code: "2823-3".to_string(),
        long_name: "Potassium [Moles/volume] in Serum or Plasma".to_string(),
        short_name: "Potassium Serum/Plasma".to_string(),
        component: "Potassium".to_string(),
        property: "Substance concentration".to_string(),
        time_aspect: "Pt".to_string(),
        system: "Serum or Plasma".to_string(),
        scale_type: "Qn".to_string(),
        method: None,
    });

    db.add_code(LoincCode {
        code: "2075-0".to_string(),
        long_name: "Chloride [Moles/volume] in Serum or Plasma".to_string(),
        short_name: "Chloride Serum/Plasma".to_string(),
        component: "Chloride".to_string(),
        property: "Substance concentration".to_string(),
        time_aspect: "Pt".to_string(),
        system: "Serum or Plasma".to_string(),
        scale_type: "Qn".to_string(),
        method: None,
    });

    db.add_code(LoincCode {
        code: "3094-0".to_string(),
        long_name: "Urea nitrogen [Mass/volume] in Serum or Plasma".to_string(),
        short_name: "BUN Serum/Plasma".to_string(),
        component: "Urea nitrogen".to_string(),
        property: "Mass concentration".to_string(),
        time_aspect: "Pt".to_string(),
        system: "Serum or Plasma".to_string(),
        scale_type: "Qn".to_string(),
        method: None,
    });

    db.add_code(LoincCode {
        code: "2160-0".to_string(),
        long_name: "Creatinine [Mass/volume] in Serum or Plasma".to_string(),
        short_name: "Creatinine Serum/Plasma".to_string(),
        component: "Creatinine".to_string(),
        property: "Mass concentration".to_string(),
        time_aspect: "Pt".to_string(),
        system: "Serum or Plasma".to_string(),
        scale_type: "Qn".to_string(),
        method: None,
    });

    db.add_code(LoincCode {
        code: "2885-2".to_string(),
        long_name: "Protein [Mass/volume] in Serum or Plasma".to_string(),
        short_name: "Protein Serum/Plasma".to_string(),
        component: "Protein".to_string(),
        property: "Mass concentration".to_string(),
        time_aspect: "Pt".to_string(),
        system: "Serum or Plasma".to_string(),
        scale_type: "Qn".to_string(),
        method: None,
    });

    db.add_code(LoincCode {
        code: "2777-7".to_string(),
        long_name: "Phosphate [Mass/volume] in Serum or Plasma".to_string(),
        short_name: "Phosphate Serum/Plasma".to_string(),
        component: "Phosphate".to_string(),
        property: "Mass concentration".to_string(),
        time_aspect: "Pt".to_string(),
        system: "Serum or Plasma".to_string(),
        scale_type: "Qn".to_string(),
        method: None,
    });

    db.add_code(LoincCode {
        code: "2986-3".to_string(),
        long_name: "Albumin [Mass/volume] in Serum or Plasma".to_string(),
        short_name: "Albumin Serum/Plasma".to_string(),
        component: "Albumin".to_string(),
        property: "Mass concentration".to_string(),
        time_aspect: "Pt".to_string(),
        system: "Serum or Plasma".to_string(),
        scale_type: "Qn".to_string(),
        method: None,
    });

    db.add_code(LoincCode {
        code: "1975-2".to_string(),
        long_name: "Bilirubin.total [Mass/volume] in Serum or Plasma".to_string(),
        short_name: "Bilirubin Total Serum/Plasma".to_string(),
        component: "Bilirubin.total".to_string(),
        property: "Mass concentration".to_string(),
        time_aspect: "Pt".to_string(),
        system: "Serum or Plasma".to_string(),
        scale_type: "Qn".to_string(),
        method: None,
    });

    db
}

#[derive(Debug, Clone, PartialEq)]
pub struct LoincError {
    pub message: String,
}

impl LoincError {
    pub fn new(msg: impl Into<String>) -> Self {
        Self {
            message: msg.into(),
        }
    }
}

pub fn validate_loinc_code(code: &str) -> Result<(), LoincError> {
    if code.trim().is_empty() {
        return Err(LoincError::new("LOINC code must not be empty"));
    }

    let parts: Vec<&str> = code.split('-').collect();
    if parts.len() != 2 {
        return Err(LoincError::new("LOINC code must have format XXXXX-Y"));
    }

    let numeric_part = parts[0];
    let check_digit = parts[1];

    if !numeric_part.chars().all(|c| c.is_ascii_digit()) {
        return Err(LoincError::new(
            "LOINC code numeric part must contain only digits",
        ));
    }

    if numeric_part.len() < 4 || numeric_part.len() > 5 {
        return Err(LoincError::new(
            "LOINC code numeric part must be 4-5 digits",
        ));
    }

    if !check_digit.chars().all(|c| c.is_ascii_digit()) {
        return Err(LoincError::new("LOINC check digit must be a digit"));
    }

    if check_digit.len() != 1 {
        return Err(LoincError::new("LOINC check digit must be a single digit"));
    }

    Ok(())
}

pub fn validate_loinc_component(component: &str) -> Result<(), LoincError> {
    if component.trim().is_empty() {
        return Err(LoincError::new("LOINC component must not be empty"));
    }

    if component.len() > 255 {
        return Err(LoincError::new(
            "LOINC component must not exceed 255 characters",
        ));
    }

    Ok(())
}

pub fn map_loinc_to_snomed(loinc_code: &str) -> Option<String> {
    match loinc_code {
        "2345-7" => Some("80184007".to_string()),
        "2951-2" => Some("39802001".to_string()),
        "2823-3" => Some("87739001".to_string()),
        "2075-0" => Some("87839004".to_string()),
        "3094-0" => Some("3024322009".to_string()),
        "2160-0" => Some("70901008".to_string()),
        "2885-2" => Some("2890005".to_string()),
        "2777-7" => Some("24636002".to_string()),
        "2986-3" => Some("61789008".to_string()),
        "1975-2" => Some("14631000".to_string()),
        _ => None,
    }
}
