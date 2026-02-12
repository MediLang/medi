use crate::fhir::{FHIRMedicalEvent, FHIRObservation};
use crate::hl7::{parse_hl7, HL7Message};

/// Map OBR segments to minimal FHIRObservation list
/// - id: OBR-2 (Placer Order Number) or OBR-3 (Filler Order Number) or index
/// - code: OBR-4 (Universal Service ID), first component
pub fn hl7_to_observations_from_obr(msg: &str) -> Option<Vec<FHIRObservation>> {
    let parsed = parse_hl7(msg).ok()?;
    Some(obrs_to_observations(&parsed))
}

/// Build observations from OBR with an optional observation date (OBR-7) alongside each.
/// Returns Vec of (FHIRObservation, Option<YYYY-MM-DD date>)
pub fn hl7_observations_from_obr_with_time(
    msg: &str,
) -> Option<Vec<(FHIRObservation, Option<String>)>> {
    let parsed = parse_hl7(msg).ok()?;
    let mut out = Vec::new();
    let mut idx = 0usize;
    for seg in &parsed.segments {
        if seg.name != "OBR" {
            continue;
        }
        idx += 1;
        let id = seg
            .fields
            .get(1)
            .and_then(|s| {
                let t = s.trim();
                if t.is_empty() {
                    None
                } else {
                    Some(t.to_string())
                }
            })
            .or_else(|| {
                seg.fields.get(2).and_then(|s| {
                    let t = s.trim();
                    if t.is_empty() {
                        None
                    } else {
                        Some(t.to_string())
                    }
                })
            })
            .unwrap_or_else(|| idx.to_string());
        let code = seg
            .fields
            .get(3)
            .and_then(|s| s.split('^').next())
            .unwrap_or("")
            .to_string();
        let mut time: Option<String> = None;
        for s in &seg.fields {
            let t = s.trim();
            if t.len() >= 8 && t.chars().take(8).all(|c| c.is_ascii_digit()) {
                time = Some(format!("{}-{}-{}", &t[0..4], &t[4..6], &t[6..8]));
                break;
            }
        }
        out.push((
            FHIRObservation {
                id,
                code,
                value: None,
                unit: None,
            },
            time,
        ));
    }
    Some(out)
}

fn obrs_to_observations(parsed: &HL7Message) -> Vec<FHIRObservation> {
    let mut out = Vec::new();
    let mut idx = 0usize;
    for seg in &parsed.segments {
        if seg.name != "OBR" {
            continue;
        }
        idx += 1;
        let id = seg
            .fields
            .get(1)
            .and_then(|s| {
                let t = s.trim();
                if t.is_empty() {
                    None
                } else {
                    Some(t.to_string())
                }
            })
            .or_else(|| {
                seg.fields.get(2).and_then(|s| {
                    let t = s.trim();
                    if t.is_empty() {
                        None
                    } else {
                        Some(t.to_string())
                    }
                })
            })
            .unwrap_or_else(|| idx.to_string());
        let code = seg
            .fields
            .get(3)
            .and_then(|s| s.split('^').next())
            .unwrap_or("")
            .to_string();
        out.push(FHIRObservation {
            id,
            code,
            value: None,
            unit: None,
        });
    }
    out
}

/// Map ORC to a minimal MedicalEvent
/// - id: ORC-2 (Placer Order Number) or ORC-3 (Filler Order Number)
/// - code: ORC-1 (Order Control)
/// - start_date: ORC-9 (Date/Time of Transaction), take yyyymmdd -> YYYY-MM-DD
pub fn hl7_to_medical_event_from_orc(msg: &str) -> Option<FHIRMedicalEvent> {
    let parsed = parse_hl7(msg).ok()?;
    orc_to_event(&parsed)
}

fn orc_to_event(parsed: &HL7Message) -> Option<FHIRMedicalEvent> {
    let orc = parsed.segments.iter().find(|s| s.name == "ORC")?;
    let id = orc
        .fields
        .get(1)
        .or_else(|| orc.fields.get(2))
        .map(|s| s.trim().to_string())
        .unwrap_or_default();
    let code = orc
        .fields
        .first()
        .map(|s| s.trim().to_string())
        .unwrap_or_default();
    let start_date = orc.fields.get(8).and_then(|s| {
        let t = s.trim();
        if t.len() >= 8 && t.chars().take(8).all(|c| c.is_ascii_digit()) {
            Some(format!("{}-{}-{}", &t[0..4], &t[4..6], &t[6..8]))
        } else {
            None
        }
    });
    Some(FHIRMedicalEvent {
        id,
        code,
        start_date,
        description: None,
    })
}

/// Return true if an NK1 (Next of Kin) segment with a name is present
/// Can be used by callers to decide to enrich Patient contacts.
pub fn hl7_has_nk1_with_name(msg: &str) -> bool {
    if let Ok(parsed) = parse_hl7(msg) {
        for seg in &parsed.segments {
            if seg.name == "NK1" {
                // NK1-2: Name (family^given)
                if let Some(name) = seg.fields.get(1) {
                    if !name.trim().is_empty() {
                        return true;
                    }
                }
            }
        }
    }
    false
}

/// Extract OBR observation date/times (OBR-7) as YYYY-MM-DD strings when available.
pub fn hl7_obr_dates_minimal(msg: &str) -> Vec<String> {
    let mut out = Vec::new();
    if let Ok(parsed) = parse_hl7(msg) {
        for seg in &parsed.segments {
            if seg.name == "OBR" {
                for s in &seg.fields {
                    let t = s.trim();
                    if t.len() >= 8 && t.chars().take(8).all(|c| c.is_ascii_digit()) {
                        out.push(format!("{}-{}-{}", &t[0..4], &t[4..6], &t[6..8]));
                        break;
                    }
                }
            }
        }
    }
    out
}

/// Extract NK1 contacts as (family, given) pairs from NK1-2 (family^given)
pub fn hl7_extract_nk1_contacts(msg: &str) -> Vec<(String, String)> {
    let mut contacts = Vec::new();
    if let Ok(parsed) = parse_hl7(msg) {
        for seg in &parsed.segments {
            if seg.name == "NK1" {
                if let Some(name) = seg.fields.get(1) {
                    let mut comps = name.split('^');
                    let fam = comps.next().unwrap_or("").trim().to_string();
                    let giv = comps.next().unwrap_or("").trim().to_string();
                    if !fam.is_empty() || !giv.is_empty() {
                        contacts.push((fam, giv));
                    }
                }
            }
        }
    }
    contacts
}
