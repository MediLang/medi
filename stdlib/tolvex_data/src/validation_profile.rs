use std::collections::HashMap;
use std::fs;
use std::path::Path;

use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;

use crate::fhir_any::FHIRAny;
use crate::validate::{ValidationError, ValidationErrorKind};

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ValidationProfile {
    pub name: String,
    pub resource_type: String,
    /// Field names that must be present and non-empty (if string) or non-null.
    pub required_fields: Vec<String>,
    /// Cardinality constraints for array fields: (min, Some(max)) or (min, None).
    pub cardinality: HashMap<String, (u32, Option<u32>)>,
}

impl ValidationProfile {
    pub fn new(name: impl Into<String>, resource_type: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            resource_type: resource_type.into(),
            required_fields: Vec::new(),
            cardinality: HashMap::new(),
        }
    }
}

/// Validate a JSON object (converted from a FHIR resource) against the profile.
pub fn validate_with_profile_json(
    obj: &JsonValue,
    profile: &ValidationProfile,
) -> Result<(), ValidationError> {
    // Type check (if provided inside the JSON as a synthetic field)
    if let Some(rt) = obj.get("resourceType").and_then(|v| v.as_str()) {
        if !profile.resource_type.eq_ignore_ascii_case(rt) {
            return Err(ValidationErrorKind::TypeMismatch {
                expected: profile.resource_type.clone(),
                found: rt.to_string(),
            }
            .into());
        }
    }

    // Required fields present and non-empty (best-effort)
    for field in &profile.required_fields {
        let v = obj.get(field);
        match v {
            None | Some(JsonValue::Null) => {
                return Err(ValidationErrorKind::MissingField {
                    field: field.clone(),
                }
                .into());
            }
            Some(JsonValue::String(s)) if s.trim().is_empty() => {
                return Err(ValidationErrorKind::EmptyField {
                    field: field.clone(),
                }
                .into());
            }
            _ => {}
        }
    }

    // Cardinality checks for arrays
    for (field, (min, max)) in &profile.cardinality {
        let arr_len = match obj.get(field) {
            Some(JsonValue::Array(a)) => a.len() as u32,
            None | Some(JsonValue::Null) => 0,
            _ => {
                return Err(ValidationErrorKind::WrongType {
                    field: field.clone(),
                    expected: "array",
                }
                .into());
            }
        };
        if arr_len < *min {
            return Err(ValidationErrorKind::Cardinality {
                field: field.clone(),
                len: arr_len,
                min: *min,
                max: *max,
            }
            .into());
        }
        if let Some(m) = max {
            if arr_len > *m {
                return Err(ValidationErrorKind::Cardinality {
                    field: field.clone(),
                    len: arr_len,
                    min: *min,
                    max: Some(*m),
                }
                .into());
            }
        }
    }

    Ok(())
}

/// Validate an in-memory resource by converting to JSON first (using serde).
/// Note: this uses best-effort JSON conversion, so field names should match struct field names.
pub fn validate_with_profile_any(
    item: &FHIRAny,
    profile: &ValidationProfile,
) -> Result<(), ValidationError> {
    // Ensure resource types match by variant
    let item_rt = match item {
        FHIRAny::Patient(_) => "Patient",
        FHIRAny::Observation(_) => "Observation",
        FHIRAny::MedicalEvent(_) => "MedicalEvent",
        FHIRAny::Medication(_) => "Medication",
        FHIRAny::Procedure(_) => "Procedure",
        FHIRAny::Condition(_) => "Condition",
        FHIRAny::Encounter(_) => "Encounter",
        FHIRAny::DiagnosticReport(_) => "DiagnosticReport",
    };
    if !profile.resource_type.eq_ignore_ascii_case(item_rt) {
        return Err(ValidationErrorKind::TypeMismatch {
            expected: profile.resource_type.clone(),
            found: item_rt.to_string(),
        }
        .into());
    }

    // Convert only the inner resource to JSON so fields are at top-level
    let mut obj = match item {
        FHIRAny::Patient(v) => serde_json::to_value(v),
        FHIRAny::Observation(v) => serde_json::to_value(v),
        FHIRAny::MedicalEvent(v) => serde_json::to_value(v),
        FHIRAny::Medication(v) => serde_json::to_value(v),
        FHIRAny::Procedure(v) => serde_json::to_value(v),
        FHIRAny::Condition(v) => serde_json::to_value(v),
        FHIRAny::Encounter(v) => serde_json::to_value(v),
        FHIRAny::DiagnosticReport(v) => serde_json::to_value(v),
    }
    .map_err(|e| ValidationError::new(e.to_string()))?;
    if let JsonValue::Object(ref mut map) = obj {
        // Inject a synthetic resourceType for easier invariants
        map.entry("resourceType".to_string())
            .or_insert(JsonValue::String(item_rt.to_string()));
    }

    validate_with_profile_json(&obj, profile)
}

/// Load profiles from a JSON string of an array of ValidationProfile objects.
pub fn load_profiles_from_str(json: &str) -> Result<Vec<ValidationProfile>, serde_json::Error> {
    serde_json::from_str::<Vec<ValidationProfile>>(json)
}

/// Load profiles from a JSON file path.
pub fn load_profiles_from_file<P: AsRef<Path>>(
    path: P,
) -> Result<Vec<ValidationProfile>, Box<dyn std::error::Error>> {
    let data = fs::read_to_string(path)?;
    let v = load_profiles_from_str(&data)?;
    Ok(v)
}

/// Built-in profiles packaged with the crate (JSON array), parsed at runtime.
pub fn load_builtin_profiles() -> Result<Vec<ValidationProfile>, serde_json::Error> {
    // SAFETY: file is packaged at compile time via include_str!
    const BUILTIN: &str = include_str!("../resources/builtin_profiles.json");
    load_profiles_from_str(BUILTIN)
}
