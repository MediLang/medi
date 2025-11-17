use medi_compliance::{
    build_compliance_report_summary, check_compliance_profile, has_consent, make_audit_entry,
    retention_decision, AuditTrail, ComplianceProfile, ComplianceStandard, ConsentRecord,
    ConsentScope, ConsentStatus, DetailedComplianceRule, ReportKind, RetentionAction,
    RetentionPolicy,
};

#[test]
fn profile_check_and_report_summary() {
    let rules = vec![DetailedComplianceRule {
        id: "r1".into(),
        description: "Example HIPAA rule".into(),
        standard: ComplianceStandard::Hipaa,
        severity: medi_compliance::RuleSeverity::Error,
        tags: vec!["hipaa".into()],
    }];

    let results = check_compliance_profile("data", &ComplianceProfile::Hipaa, &rules);
    assert_eq!(results.len(), 1);
    assert!(results[0].passed);

    let summary = build_compliance_report_summary(
        ReportKind::Fda21Cfr11,
        Some(ComplianceProfile::Hipaa),
        &results,
    );
    assert_eq!(summary.summary.total, 1);
    assert_eq!(summary.summary.passed, 1);
    assert_eq!(summary.summary.failed, 0);
}

#[test]
fn audit_trail_collects_entries() {
    let mut trail = AuditTrail::new();
    assert!(trail.is_empty());

    let e1 = make_audit_entry("user", "read", "resource", "2025-01-01T00:00:00Z");
    trail.push(e1);

    assert_eq!(trail.len(), 1);
    assert!(!trail.is_empty());
}

#[test]
fn consent_scopes_and_expiry() {
    let consent = ConsentRecord {
        id: "c1".into(),
        subject: "patient-123".into(),
        scopes: vec![ConsentScope {
            resource: "ehr".into(),
            action: "read".into(),
        }],
        status: ConsentStatus::Active,
        expires_at: Some("2025-12-31T23:59:59Z".into()),
    };

    assert!(has_consent(
        &consent,
        "ehr",
        "read",
        Some("2025-01-01T00:00:00Z")
    ));
    assert!(!has_consent(
        &consent,
        "ehr",
        "read",
        Some("2026-01-01T00:00:00Z")
    ));
    assert!(!has_consent(
        &consent,
        "ehr",
        "write",
        Some("2025-01-01T00:00:00Z")
    ));
}

#[test]
fn retention_policies_choose_first_applicable() {
    let policies = vec![
        RetentionPolicy {
            min_age_days: 0,
            max_age_days: Some(365),
            action: RetentionAction::Retain,
        },
        RetentionPolicy {
            min_age_days: 366,
            max_age_days: None,
            action: RetentionAction::Delete,
        },
    ];

    let a1 = retention_decision(100, &policies).unwrap();
    assert_eq!(a1, RetentionAction::Retain);

    let a2 = retention_decision(400, &policies).unwrap();
    assert_eq!(a2, RetentionAction::Delete);

    let a3 = retention_decision(10_000, &policies).unwrap();
    assert_eq!(a3, RetentionAction::Delete);
}
