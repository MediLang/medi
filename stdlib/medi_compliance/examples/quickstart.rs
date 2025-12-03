use medi_compliance::{
    build_compliance_report_summary, run_hipaa_bundle, ComplianceProfile, HipaaKeywordRule,
    ReportKind, RuleSeverity,
};

fn main() {
    let rules = vec![HipaaKeywordRule {
        id: "k1".into(),
        description: "Detect SSN".into(),
        keywords: vec!["ssn".into(), "social security".into()],
        severity: RuleSeverity::Error,
    }];

    let report = run_hipaa_bundle(
        "Patient SSN: 123-45-6789",
        &rules,
        None,
        None,
        None,
        Some(400),
        &[],
    );
    let summary = build_compliance_report_summary(
        ReportKind::Fda21Cfr11,
        Some(ComplianceProfile::Hipaa),
        &report.keyword_results,
    );
    println!(
        "passed={} failed={} total={}",
        summary.summary.passed, summary.summary.failed, summary.summary.total
    );
}
