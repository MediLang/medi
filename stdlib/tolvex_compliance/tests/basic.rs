use tolvex_compliance::{
    check_contains_forbidden, check_hipaa_compliance, Anonymizer, ComplianceRule,
    ContainsForbiddenRule, MaskAnonymizer, NoOpAnonymizer, RedactAnonymizer, Sha256Anonymizer,
};

#[test]
fn anonymizers_work() {
    let noop = NoOpAnonymizer;
    assert_eq!(noop.anonymize("abc"), "abc");

    let sha = Sha256Anonymizer;
    let out = sha.anonymize("abc");
    assert_eq!(
        out,
        "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    );

    let mask = MaskAnonymizer;
    assert_eq!(mask.anonymize("1234567890"), "******7890");
    assert_eq!(mask.anonymize("abcd"), "****");

    let red = RedactAnonymizer;
    assert_eq!(red.anonymize("anything"), "[REDACTED]");
}

#[test]
fn simple_rules() {
    let data = "this contains ssn and phone";
    let rules = vec![ContainsForbiddenRule {
        id: "r1".into(),
        keywords: vec!["ssn".into(), "credit".into()],
    }];
    let results = check_contains_forbidden(data, &rules);
    assert_eq!(results.len(), 1);
    assert!(!results[0].passed);
    assert!(results[0].message.as_ref().unwrap().contains("ssn"));
}

#[test]
fn placeholder_hipaa_check() {
    let rules = vec![ComplianceRule {
        id: "hipaa1".into(),
        description: "placeholder".into(),
    }];
    let results = check_hipaa_compliance("data", &rules);
    assert_eq!(results.len(), 1);
    assert!(results[0].passed);
}
