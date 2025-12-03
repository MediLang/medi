# Getting Started: medi_ai

Minimal risk prediction and stratification.

```rust
use medi_ai::{RiskScorer, predict_risk, stratify_risk};
use serde_json::json;

let model = RiskScorer { weights: vec![0.2, 0.5, 0.3], bias: 0.1, model_name: "demo".into() };
let score = model.predict(&[0.4, 0.2, 0.9]);

let patient = json!({"age": 65, "bmi": 33.1, "a1c": 7.8});
let pred = predict_risk(&patient, "diabetes", "5y", &["age".into(), "bmi".into(), "a1c".into()]);
let stratum = stratify_risk(&pred);
```
