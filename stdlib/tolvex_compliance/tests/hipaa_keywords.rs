use tolvex_compliance::{
    check_hipaa_keywords, deidentify_field, hash_phi, mask_phi, redact_phi, run_hipaa_bundle,
    ConsentRecord, ConsentScope, ConsentStatus, HipaaKeywordRule, RetentionAction, RetentionPolicy,
};

#[test]
fn hipaa_keywords_detect_phi_like_strings() {
    let rules = vec![HipaaKeywordRule {
        id: "k1".into(),
        description: "Detect SSN".into(),
        keywords: vec!["ssn".into(), "social security".into()],
        severity: tolvex_compliance::RuleSeverity::Error,
    }];

    let data = "Patient SSN: 123-45-6789";
    let results = check_hipaa_keywords(data, &rules);
    assert_eq!(results.len(), 1);
    assert!(!results[0].passed);
    assert!(results[0]
        .message
        .as_ref()
        .unwrap()
        .to_lowercase()
        .contains("ssn"));

    let ok_results = check_hipaa_keywords("No sensitive identifiers here", &rules);
    assert_eq!(ok_results.len(), 1);
    assert!(ok_results[0].passed);
    assert!(ok_results[0].message.is_none());
}

#[test]
fn tag_based_deid_helper_respects_tags() {
    let v = "123-45-6789";
    assert_eq!(mask_phi(v), deidentify_field(v, "mask"));
    assert_eq!(mask_phi(v), deidentify_field(v, "PHI.MASK"));

    let h1 = hash_phi(v);
    let h2 = deidentify_field(v, "hash");
    assert_eq!(h1, h2);

    let r1 = redact_phi(v);
    let r2 = deidentify_field(v, "phi.redact");
    assert_eq!(r1, r2);

    // Unknown tag -> value unchanged
    assert_eq!(v.to_string(), deidentify_field(v, "unknown"));
}

#[test]
fn hipaa_bundle_combines_keyword_consent_and_retention() {
    let rules = vec![HipaaKeywordRule {
        id: "k1".into(),
        description: "Detect phone".into(),
        keywords: vec!["phone".into()],
        severity: tolvex_compliance::RuleSeverity::Warning,
    }];

    let consent = ConsentRecord {
        id: "c1".into(),
        subject: "patient-123".into(),
        scopes: vec![ConsentScope {
            resource: "ehr".into(),
            action: "read".into(),
        }],
        status: ConsentStatus::Active,
        expires_at: None,
    };

    let retention_policies = vec![RetentionPolicy {
        min_age_days: 0,
        max_age_days: Some(365),
        action: RetentionAction::Retain,
    }];

    let report = run_hipaa_bundle(
        "Contact phone: 555-1234",
        &rules,
        Some(&consent),
        Some("ehr"),
        Some("read"),
        Some(100),
        &retention_policies,
    );

    assert_eq!(report.keyword_results.len(), 1);
    assert!(!report.keyword_results[0].passed);
    assert!(report.consent_ok);
    assert_eq!(report.retention_action, Some(RetentionAction::Retain));
    assert_eq!(report.overall.total, 3); // keyword + consent + retention
}
