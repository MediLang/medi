use crate::fhir::{FHIRCondition, FHIRDiagnosticReport, FHIREncounter, FHIRProcedure};

/// Types that carry a patient subject id.
pub trait HasSubject {
    fn subject(&self) -> &str;
}

impl HasSubject for FHIRProcedure {
    fn subject(&self) -> &str {
        &self.subject
    }
}
impl HasSubject for FHIRCondition {
    fn subject(&self) -> &str {
        &self.subject
    }
}
impl HasSubject for FHIREncounter {
    fn subject(&self) -> &str {
        &self.subject
    }
}
impl HasSubject for FHIRDiagnosticReport {
    fn subject(&self) -> &str {
        &self.subject
    }
}

/// Link conditions to encounters by subject id (simple heuristic).
/// Returns tuples of (condition, optional matching encounter).
/// If multiple encounters match, picks the first.
pub fn link_conditions_to_encounters(
    conditions: &[FHIRCondition],
    encounters: &[FHIREncounter],
) -> Vec<(FHIRCondition, Option<FHIREncounter>)> {
    conditions
        .iter()
        .cloned()
        .map(|c| {
            let enc = encounters.iter().find(|e| e.subject == c.subject).cloned();
            (c, enc)
        })
        .collect()
}

/// Build a minimal DiagnosticReport from a set of observation codes for a subject.
/// Caller is responsible for ensuring observations exist separately.
pub fn diagnostic_report_from_observation_codes(
    id: impl Into<String>,
    subject: impl Into<String>,
    report_code: impl Into<String>,
    observation_codes: &[impl AsRef<str>],
) -> FHIRDiagnosticReport {
    FHIRDiagnosticReport {
        id: id.into(),
        code: report_code.into(),
        result_codes: observation_codes
            .iter()
            .map(|s| s.as_ref().to_string())
            .collect(),
        subject: subject.into(),
    }
}
