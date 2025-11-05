use crate::fhir::FHIRResource;
use crate::fhir::{FHIRMedicalEvent, FHIRObservation, FHIRPatient};
use crate::fhir_any::FHIRAny;

#[derive(Debug, Clone, PartialEq)]
pub struct ValidationError {
    pub message: String,
}

/// Basic per-profile required fields validation over FHIRAny.
/// Delegates to typed validators for Patient, Observation, and MedicalEvent.
pub fn validate_any_basic(item: &FHIRAny) -> Result<(), ValidationError> {
    match item {
        FHIRAny::Patient(p) => validate_patient(p),
        FHIRAny::Observation(o) => validate_observation(o),
        FHIRAny::MedicalEvent(e) => validate_medical_event(e),
    }
}

pub fn validate_medical_event(e: &FHIRMedicalEvent) -> Result<(), ValidationError> {
    if e.id.trim().is_empty() {
        return Err(ValidationError::new("MedicalEvent.id must not be empty"));
    }
    if e.code.trim().is_empty() {
        return Err(ValidationError::new("MedicalEvent.code must not be empty"));
    }
    if let Some(sd) = &e.start_date {
        // Accept YYYY-MM-DD or YYYYMMDD
        let ok = (sd.len() == 10 && sd.as_bytes()[4] == b'-' && sd.as_bytes()[7] == b'-')
            || (sd.len() == 8 && sd.chars().all(|c| c.is_ascii_digit()));
        if !ok {
            return Err(ValidationError::new(
                "MedicalEvent.start_date must be YYYY-MM-DD or YYYYMMDD",
            ));
        }
    }
    Ok(())
}

pub fn validate_observation(o: &FHIRObservation) -> Result<(), ValidationError> {
    if o.id.trim().is_empty() {
        return Err(ValidationError::new("Observation.id must not be empty"));
    }
    if o.code.trim().is_empty() {
        return Err(ValidationError::new("Observation.code must not be empty"));
    }
    // Code must be ASCII uppercase letters/numbers after trimming
    {
        let c = o.code.trim();
        if !c
            .chars()
            .all(|ch| ch.is_ascii_uppercase() || ch.is_ascii_digit())
        {
            return Err(ValidationError::new(
                "Observation.code must be uppercase A-Z/0-9",
            ));
        }
    }
    if let Some(v) = o.value {
        if !v.is_finite() {
            return Err(ValidationError::new("Observation.value must be finite"));
        }
    }
    if let Some(u) = &o.unit {
        if u.trim().is_empty() {
            return Err(ValidationError::new(
                "Observation.unit, if present, must not be empty",
            ));
        }
    }
    // If code implies a unit, require non-empty unit (simple rule)
    {
        let code = o.code.as_str();
        let unit_opt = o.unit.as_ref().map(|s| s.trim().to_ascii_lowercase());
        match code {
            "HR" => {
                if unit_opt.as_deref() != Some("bpm") {
                    return Err(ValidationError::new("Observation HR requires unit bpm"));
                }
            }
            "BP" => {
                if unit_opt.as_deref() != Some("mmhg") {
                    return Err(ValidationError::new("Observation BP requires unit mmhg"));
                }
            }
            "TEMP" => match unit_opt.as_deref() {
                Some("c") | Some("f") => {}
                _ => {
                    return Err(ValidationError::new(
                        "Observation TEMP requires unit c or f",
                    ))
                }
            },
            _ => {}
        }
    }
    Ok(())
}

impl ValidationError {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

pub fn validate_fhir<T: FHIRResource>(resource: &T) -> Result<(), ValidationError> {
    if resource.id().trim().is_empty() {
        return Err(ValidationError::new("FHIR resource id must not be empty"));
    }

    // Type-specific light checks
    match resource.resource_type() {
        "Patient" => {
            // For deeper validation, prefer using `validate_patient` with a typed FHIRPatient value.
        }
        "Observation" => {
            // Can't downcast here; callers should use validate_observation when they have a typed value.
        }
        _ => {}
    }

    Ok(())
}

pub fn validate_patient(p: &FHIRPatient) -> Result<(), ValidationError> {
    if p.id.trim().is_empty() {
        return Err(ValidationError::new("Patient.id must not be empty"));
    }
    // Require at least one name when present: at least one of given_name/family_name must be non-empty if provided
    let has_given = p
        .given_name
        .as_ref()
        .map(|s| !s.trim().is_empty())
        .unwrap_or(false);
    let has_family = p
        .family_name
        .as_ref()
        .map(|s| !s.trim().is_empty())
        .unwrap_or(false);
    if !(has_given || has_family) {
        return Err(ValidationError::new(
            "Patient must have at least one non-empty name (given or family)",
        ));
    }
    if let Some(bd) = &p.birth_date {
        // Accept formats YYYY-MM-DD or YYYYMMDD minimally (common in HL7 and simplified FHIR examples)
        fn parse_parts(bd: &str) -> Option<(i32, i32, i32)> {
            if bd.len() == 10 && &bd[4..5] == "-" && &bd[7..8] == "-" {
                let y = bd[0..4].parse().ok()?;
                let m = bd[5..7].parse().ok()?;
                let d = bd[8..10].parse().ok()?;
                Some((y, m, d))
            } else if bd.len() == 8 && bd.chars().all(|c| c.is_ascii_digit()) {
                let y = bd[0..4].parse().ok()?;
                let m = bd[4..6].parse().ok()?;
                let d = bd[6..8].parse().ok()?;
                Some((y, m, d))
            } else {
                None
            }
        }
        let (_y, m, d) = parse_parts(bd).ok_or_else(|| {
            ValidationError::new("Patient.birth_date must be YYYY-MM-DD or YYYYMMDD")
        })?;
        let month_valid = (1..=12).contains(&m);
        let day_valid = (1..=31).contains(&d); // naive day check
        if !month_valid || !day_valid {
            return Err(ValidationError::new(
                "Patient.birth_date must be YYYY-MM-DD or YYYYMMDD",
            ));
        }
    }
    Ok(())
}
