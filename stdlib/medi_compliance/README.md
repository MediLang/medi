# medi_compliance

HIPAA/GDPR-oriented helpers: keyword checks, consent/retention, de-identification, simple rule engine, and report summaries.

## Quick start

```rust
use medi_compliance::{
  HipaaKeywordRule, RuleSeverity, run_hipaa_bundle,
  ComplianceProfile, build_compliance_report_summary, ReportKind,
};

let rules = vec![HipaaKeywordRule{
  id: "k1".into(), description: "Detect SSN".into(),
  keywords: vec!["ssn".into(), "social security".into()],
  severity: RuleSeverity::Error,
}];

let report = run_hipaa_bundle(
  "Patient SSN: 123-45-6789",
  &rules,
  None, None, None,
  Some(400),
  &[]
);

let summary = build_compliance_report_summary(ReportKind::Fda21Cfr11, Some(ComplianceProfile::Hipaa), &report.keyword_results);
println!("passed={}", summary.summary.passed);
```

## Examples

See `examples/` for runnable programs.
