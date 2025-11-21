use std::collections::HashMap;

use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;

use crate::fhir_any::FHIRAny;
use crate::validate::ValidationError;

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
            return Err(ValidationError::new(format!(
                "Profile '{}' expects resource_type '{}' but got '{}'",
                profile.name, profile.resource_type, rt
            )));
        }
    }

    // Required fields present and non-empty (best-effort)
    for field in &profile.required_fields {
        let v = obj.get(field);
        match v {
            None | Some(JsonValue::Null) => {
                return Err(ValidationError::new(format!(
                    "Missing required field: {field}"
                )));
            }
            Some(JsonValue::String(s)) if s.trim().is_empty() => {
                return Err(ValidationError::new(format!(
                    "Field '{field}' must not be empty"
                )));
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
                return Err(ValidationError::new(format!(
                    "Field '{field}' must be an array for cardinality check"
                )));
            }
        };
        if arr_len < *min {
            return Err(ValidationError::new(format!(
                "Field '{field}' has length {arr_len} but minimum is {min}"
            )));
        }
        if let Some(m) = max {
            if arr_len > *m {
                return Err(ValidationError::new(format!(
                    "Field '{field}' has length {arr_len} but maximum is {m}"
                )));
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
        return Err(ValidationError::new(format!(
            "Profile '{}' expects resource_type '{}' but got '{}'",
            profile.name, profile.resource_type, item_rt
        )));
    }

    // Convert with serde to JSON object
    let mut obj = serde_json::to_value(item).map_err(|e| ValidationError::new(e.to_string()))?;
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
