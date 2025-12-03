# medi_ai

AI helpers for Medi: simple risk prediction, bias checking, cohort operations, explainability, and edge model packaging.

## Quick start

```rust
use medi_ai::{RiskScorer, predict_risk, stratify_risk};
use serde_json::json;

// Load a simple linear scorer
let model = RiskScorer { weights: vec![0.2, 0.5, 0.3], bias: 0.1, model_name: "demo".into() };
let features = [0.4, 0.2, 0.9];
let score = model.predict(&features);

// End-to-end helper
let patient = json!({"age": 65, "bmi": 33.1, "a1c": 7.8});
let pred = predict_risk(&patient, "diabetes", "5y", &["age".into(), "bmi".into(), "a1c".into()]);
let stratum = stratify_risk(&pred);
println!("risk={:.3} stratum={:?}", pred.score, stratum);
```

## Examples

See `examples/` for runnable programs.
