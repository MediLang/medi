use crate::fhir::FHIRObservation;
use crate::fhir::FHIRPatient;

/// Normalize YYYY-MM-DD or YYYYMMDD, returns YYYYMMDD.
pub fn normalize_date_yyyymmdd(s: &str) -> String {
    if s.len() == 10 && s.as_bytes().get(4) == Some(&b'-') && s.as_bytes().get(7) == Some(&b'-') {
        s.chars().filter(|&c| c != '-').collect::<String>()
    } else {
        s.to_string()
    }
}

/// Bulk sanitize observations: canonize code, normalize unit aliases, then normalize unit case/whitespace.
pub fn sanitize_observations_bulk(obs: &mut [FHIRObservation]) {
    for o in obs {
        canonize_observation_code(o);
        normalize_observation_unit_alias(o);
        normalize_observation_unit(o);
    }
}

/// Trim leading/trailing whitespace from patient given/family names.
pub fn trim_patient_names(p: &mut FHIRPatient) {
    if let Some(g) = p.given_name.take() {
        p.given_name = Some(g.trim().to_string());
    }
    if let Some(f) = p.family_name.take() {
        p.family_name = Some(f.trim().to_string());
    }
}

/// Normalize patient birth_date into YYYYMMDD (if present and matches known formats).
pub fn normalize_patient_birth_date(p: &mut FHIRPatient) {
    if let Some(b) = p.birth_date.take() {
        let n = normalize_date_yyyymmdd(&b);
        p.birth_date = Some(n);
    }
}

/// Lowercase and trim the observation unit, e.g., " BPM " -> "bpm".
pub fn normalize_observation_unit(o: &mut FHIRObservation) {
    if let Some(u) = o.unit.take() {
        o.unit = Some(u.trim().to_ascii_lowercase());
    }
}

/// Canonize observation code: trim and uppercase ASCII.
pub fn canonize_observation_code(o: &mut FHIRObservation) {
    let c = o.code.trim().to_ascii_uppercase();
    o.code = c;
}

/// Map common unit aliases to canonical forms (lowercase):
/// - beats/min, beat/min, beats per minute -> bpm
/// - mmhg variants -> mmhg
/// - celsius variants -> c
pub fn normalize_observation_unit_alias(o: &mut FHIRObservation) {
    if let Some(u0) = o.unit.take() {
        let u = u0.trim().to_ascii_lowercase();
        let canon = match u.as_str() {
            "bpm" | "beat/min" | "beats/min" | "beats per minute" | "beats-per-minute" => "bpm",
            "mmhg" | "mm hg" => "mmhg",
            "c" | "celsius" | "°c" | "degc" | "deg c" => "c",
            other => other,
        };
        o.unit = Some(canon.to_string());
    }
}
