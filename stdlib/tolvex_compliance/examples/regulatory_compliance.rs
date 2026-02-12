use tolvex_compliance::{
    check_hipaa_keywords, deidentify_field, make_audit_entry, redact_phi, AuditTrail,
    HipaaKeywordRule, RuleSeverity,
};
use serde_json::Value as JsonValue;
use std::fs;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let path = "examples/use_cases/patient_record_phi.json";
    let raw = fs::read_to_string(path)?;
    let mut json: JsonValue = serde_json::from_str(&raw)?;

    let rules = vec![
        HipaaKeywordRule {
            id: "phi_ssn".to_string(),
            description: "SSN should not appear in free-text".to_string(),
            keywords: vec!["123-45-6789".to_string()],
            severity: RuleSeverity::Error,
        },
        HipaaKeywordRule {
            id: "phi_name".to_string(),
            description: "Patient full name should not appear in free-text".to_string(),
            keywords: vec!["jane".to_string(), "doe".to_string()],
            severity: RuleSeverity::Warning,
        },
    ];

    // 1) Compliance check (pre-deid)
    let before = serde_json::to_string(&json)?;
    let res_before = check_hipaa_keywords(&before, &rules);
    println!(
        "HIPAA keyword results (before): passed={} failed={}",
        res_before.iter().filter(|r| r.passed).count(),
        res_before.iter().filter(|r| !r.passed).count()
    );
    for r in &res_before {
        if !r.passed {
            if let Some(msg) = &r.message {
                println!("- {msg}");
            }
        }
    }

    // 2) De-identification: redact SSN and free-text note.
    if let Some(ssn) = json
        .get_mut("patient")
        .and_then(|p| p.get_mut("ssn"))
        .and_then(|v| v.as_str())
        .map(|s| s.to_string())
    {
        let new_val = deidentify_field(&ssn, "phi.redact");
        json["patient"]["ssn"] = JsonValue::String(new_val);
    }

    // De-identify structured name fields as well (so keyword rules won't match there).
    if let Some(given) = json
        .get_mut("patient")
        .and_then(|p| p.get_mut("name"))
        .and_then(|n| n.get_mut("given"))
        .and_then(|v| v.as_str())
        .map(|s| s.to_string())
    {
        json["patient"]["name"]["given"] =
            JsonValue::String(deidentify_field(&given, "phi.redact"));
    }
    if let Some(family) = json
        .get_mut("patient")
        .and_then(|p| p.get_mut("name"))
        .and_then(|n| n.get_mut("family"))
        .and_then(|v| v.as_str())
        .map(|s| s.to_string())
    {
        json["patient"]["name"]["family"] =
            JsonValue::String(deidentify_field(&family, "phi.redact"));
    }

    if let Some(note) = json
        .get("note")
        .and_then(|v| v.as_str())
        .map(|s| s.to_string())
    {
        // demonstrate direct helper as well
        json["note"] = JsonValue::String(redact_phi(&note));
    }

    // 3) Audit trail
    let mut audit = AuditTrail::new();
    audit.push(make_audit_entry(
        "compliance_demo",
        "deidentify",
        "patient_record_phi.json",
        "2025-12-23T00:00:00Z",
    ));

    // 4) Compliance check (post-deid)
    let after = serde_json::to_string(&json)?;
    let res_after = check_hipaa_keywords(&after, &rules);
    println!(
        "HIPAA keyword results (after): passed={} failed={}",
        res_after.iter().filter(|r| r.passed).count(),
        res_after.iter().filter(|r| !r.passed).count()
    );

    println!("audit entries: {}", audit.len());
    Ok(())
}
