# Getting Started: medi_compliance

HIPAA keyword rule and bundle.

```rust
use medi_compliance::{HipaaKeywordRule, RuleSeverity, run_hipaa_bundle};

let rules = vec![HipaaKeywordRule{ id: "k1".into(), description: "Detect SSN".into(), keywords: vec!["ssn".into()], severity: RuleSeverity::Error }];
let report = run_hipaa_bundle("Patient SSN: 123-45-6789", &rules, None, None, None, None, &[]);
assert!(report.overall.failed >= 1);
```
