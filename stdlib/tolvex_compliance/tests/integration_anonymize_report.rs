use tolvex_compliance::*;
use serde_json::json;

#[test]
fn fhir_compliance_anonymize_report_flow() {
    // 1. Load patient data with PHI-like content
    let data = json!({
        "patient": {
            "id": "p-001",
            "name": { "family": "Doe", "given": ["Jane"] },
            "phone": "+1-555-123-4567",
            "email": "jane.doe@example.com"
        },
        "note": "Patient reports allergy to penicillin"
    });

    // 2. Check compliance using simple HIPAA keyword rules
    let rules = vec![HipaaKeywordRule {
        id: "phi_keywords".into(),
        description: "Detect PHI keywords".into(),
        keywords: vec!["penicillin".into(), "ssn".into(), "phone".into()],
        severity: RuleSeverity::Warning,
    }];
    let results = check_hipaa_keywords(&data.to_string(), &rules);

    // 3. Anonymize phone/email fields via helpers
    let mut anonymized = data.clone();
    if let Some(obj) = anonymized
        .get_mut("patient")
        .and_then(|v| v.as_object_mut())
    {
        if let Some(v) = obj.get_mut("phone") {
            *v = json!(mask_phi(v.as_str().unwrap_or("")));
        }
        if let Some(v) = obj.get_mut("email") {
            *v = json!(hash_phi(v.as_str().unwrap_or("")));
        }
    }

    // 4. Generate report summary
    let report = build_compliance_report_summary(
        ReportKind::CmsQuality,
        Some(ComplianceProfile::Hipaa),
        &results,
    );

    // Assertions
    assert!(report.summary.total >= 1);
    assert!(report.summary.passed + report.summary.failed == report.summary.total);
}
