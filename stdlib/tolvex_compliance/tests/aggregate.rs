use tolvex_compliance::{summarize_results, ComplianceResult};

#[test]
fn summarize_counts_passed_failed() {
    let results = vec![
        ComplianceResult {
            rule_id: "r1".into(),
            passed: true,
            message: None,
        },
        ComplianceResult {
            rule_id: "r2".into(),
            passed: false,
            message: Some("x".into()),
        },
        ComplianceResult {
            rule_id: "r3".into(),
            passed: true,
            message: None,
        },
    ];
    let s = summarize_results(&results);
    assert_eq!(s.total, 3);
    assert_eq!(s.passed, 2);
    assert_eq!(s.failed, 1);
}
