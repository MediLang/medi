# medi_model

Model registry and utilities for Medi: metadata, metrics, calibration, feature importance, fairness, and quantization.

## Quick start

```rust
use medi_model::{ModelRegistry, ModelMetadata};

let mut reg = ModelRegistry::default();
reg.register(ModelMetadata{
  id: "risk_demo".into(),
  name: "Risk Demo".into(),
  version: "0.1.0".into(),
  task: "risk".into(),
  input_schema: None,
  output_schema: None,
});
assert!(reg.get("risk_demo").is_some());
```

## Examples

See `examples/` for runnable programs.
