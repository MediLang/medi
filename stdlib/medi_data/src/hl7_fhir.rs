use crate::fhir::{FHIRMedicalEvent, FHIRPatient};
use crate::hl7::{parse_hl7, HL7Message};

/// Very minimal HL7 -> FHIR Patient conversion using PID segment
/// - PID-5: Patient Name (family^given)
/// - PID-7: Date/Time of Birth (YYYYMMDD)
/// - PID-3 or PID-2 could be used for ID; we fallback to PID-3 (first component) else PID-2 else ""
pub fn hl7_to_fhir_patient_minimal(msg: &str) -> Option<FHIRPatient> {
    let parsed = parse_hl7(msg).ok()?;
    pid_to_patient(&parsed)
}

fn pid_to_patient(parsed: &HL7Message) -> Option<FHIRPatient> {
    let pid = parsed.segments.iter().find(|s| s.name == "PID")?;
    // HL7 fields are 1-based after the segment name; our vec excludes the name, so index 0 is field 1
    // PID-3: Patient Identifier List; take first component before '^'
    let id = pid
        .fields
        .get(2)
        .and_then(|s| s.split('^').next())
        .filter(|s| !s.is_empty())
        .map(|s| s.to_string())
        // fallback to PID-2 if PID-3 is empty
        .or_else(|| pid.fields.get(1).map(|s| s.to_string()))
        .unwrap_or_default();

    // PID-5: Patient Name (family^given)
    let (family_name, given_name) = pid
        .fields
        .get(4)
        .map(|f| {
            let mut comps = f.split('^');
            (
                comps.next().map(|s| s.to_string()),
                comps.next().map(|s| s.to_string()),
            )
        })
        .unwrap_or((None, None));

    // PID-7: Birth Date (YYYYMMDD); convert to YYYY-MM-DD if possible
    let birth_date_raw = pid.fields.get(6).cloned();
    let birth_date = birth_date_raw.map(|s| {
        if s.len() == 8 && s.chars().all(|c| c.is_ascii_digit()) {
            format!("{}-{}-{}", &s[0..4], &s[4..6], &s[6..8])
        } else {
            s
        }
    });

    Some(FHIRPatient {
        id,
        given_name,
        family_name,
        birth_date,
    })
}

/// Very minimal HL7 -> FHIR MedicalEvent conversion using PV1 segment
/// - PV1-19: Visit Number used for id (first component)
/// - PV1-2: Patient Class or PV1-10: Hospital Service used for code (prefers PV1-10, else PV1-2)
/// - PV1-44: Admitted Date/Time used for start_date (YYYYMMDD -> YYYY-MM-DD)
pub fn hl7_to_medical_event_minimal(msg: &str) -> Option<FHIRMedicalEvent> {
    let parsed = parse_hl7(msg).ok()?;
    pv1_to_medical_event(&parsed)
}

fn pv1_to_medical_event(parsed: &HL7Message) -> Option<FHIRMedicalEvent> {
    let pv1 = parsed.segments.iter().find(|s| s.name == "PV1")?;
    // PV1-19 Visit Number for id (be tolerant to off-by-one variations)
    let visit_idx_candidates = [18usize, 19, 17, 20];
    let mut id = String::new();
    for idx in visit_idx_candidates {
        if let Some(v) = pv1.fields.get(idx) {
            let v = v.trim();
            if !v.is_empty() {
                id = v.to_string();
                break;
            }
        }
    }
    // code prefer PV1-10 (Hospital Service). Fallback: PV1-2 (Patient Class), PV1-3 comp 4 (location facility), neighbors
    let mut code = String::new();
    let code_candidates = [9usize, 10, 8, 2, 1];
    for idx in code_candidates {
        if let Some(v) = pv1.fields.get(idx) {
            let v = v.trim();
            if !v.is_empty() {
                // If it's a composite location (e.g., W^101^1^HOSP), pick the last component as code
                let comp = v.split('^').next_back().unwrap_or(v);
                if !comp.is_empty() {
                    code = comp.to_string();
                    break;
                }
            }
        }
    }
    // start_date: scan from end to find an 8-digit yyyymmdd token
    let mut start_date = None;
    for s in pv1.fields.iter().rev() {
        let t = s.trim();
        if t.len() >= 8 && t.chars().take(8).all(|c| c.is_ascii_digit()) {
            start_date = Some(format!("{}-{}-{}", &t[0..4], &t[4..6], &t[6..8]));
            break;
        }
    }
    Some(FHIRMedicalEvent {
        id,
        code,
        start_date,
        description: None,
    })
}
