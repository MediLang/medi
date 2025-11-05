use crate::fhir::FHIRObservation;
use crate::hl7::{parse_hl7, HL7Message};

/// Minimal OBX -> FHIRObservation mapping
/// - id: OBX-1 (Set ID) if present, else uses sequential index starting at 1 as string
/// - code: OBX-3 (Observation Identifier), take first component before '^'
/// - value: OBX-5, parse f64 if numeric, else None
/// - unit: OBX-6, take first component before '^'
pub fn hl7_to_observations_minimal(msg: &str) -> Option<Vec<FHIRObservation>> {
    let parsed = parse_hl7(msg).ok()?;
    Some(obxs_to_observations(&parsed))
}

fn obxs_to_observations(parsed: &HL7Message) -> Vec<FHIRObservation> {
    let mut out = Vec::new();
    let mut idx = 0usize;
    for seg in &parsed.segments {
        if seg.name != "OBX" {
            continue;
        }
        idx += 1;
        // OBX-1 Set ID
        let id = seg
            .fields
            .first()
            .map(|s| s.as_str())
            .filter(|s| !s.is_empty())
            .unwrap_or("");
        let id = if id.is_empty() {
            idx.to_string()
        } else {
            id.to_string()
        };
        // OBX-3 Identifier
        let code = seg
            .fields
            .get(2)
            .and_then(|s| s.split('^').next())
            .unwrap_or("")
            .to_string();
        // OBX-2 Data Type
        let dt = seg.fields.get(1).map(|s| s.as_str()).unwrap_or("");
        // OBX-5 Value (interpret based on OBX-2)
        let raw_val = seg.fields.get(4).map(|s| s.as_str()).unwrap_or("");
        let value = match dt {
            "NM" => raw_val.parse::<f64>().ok(),
            "ST" => raw_val.trim().parse::<f64>().ok(),
            "TS" => None,  // timestamp; unsupported in numeric value
            "CWE" => None, // coded with exceptions; keep code from OBX-3
            _ => raw_val.parse::<f64>().ok(),
        };
        // OBX-6 Units
        let unit = seg
            .fields
            .get(5)
            .and_then(|s| s.split('^').next())
            .map(|s| s.to_string());

        out.push(FHIRObservation {
            id,
            code,
            value,
            unit,
        });
    }
    out
}
