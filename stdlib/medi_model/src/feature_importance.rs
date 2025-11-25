//! Perturbation-based feature importance

use crate::backend::{BackendError, ModelBackend, Tensor};
use std::collections::HashMap;

/// Compute simple perturbation-based feature importance by measuring
/// absolute change in model output when each feature is perturbed by epsilon.
///
/// - `model`: model implementing `ModelBackend`
/// - `baseline`: baseline feature vector used as starting point
/// - `features`: magnitude to perturb (if empty, uses baseline as reference)
/// - Returns per-feature importance scores
pub fn compute_feature_importance(
    model: &dyn ModelBackend,
    baseline: &[f64],
    features: &[f64],
) -> Result<Vec<f64>, BackendError> {
    let n = baseline.len();
    let eps = 1e-3_f64;

    // Helper to run model and get scalar output
    let run = |x: &[f64]| -> Result<f64, BackendError> {
        let data: Vec<f32> = x.iter().map(|v| *v as f32).collect();
        let t = Tensor::new("input".into(), vec![n], data)?;
        let mut inputs = HashMap::new();
        inputs.insert("input".into(), t);
        let out = model.infer(inputs)?;
        // Prefer "output" tensor if exists, else first entry
        let value = if let Some(t) = out.get("output") {
            t.data.first().copied().unwrap_or(0.0)
        } else if let Some((_k, t)) = out.iter().next() {
            t.data.first().copied().unwrap_or(0.0)
        } else {
            0.0
        };
        Ok(value as f64)
    };

    let y0 = run(baseline)?;

    let mut importances = vec![0.0_f64; n];
    for i in 0..n {
        let mut x_pos = baseline.to_vec();
        let mut x_neg = baseline.to_vec();
        let mag = if features.is_empty() {
            1.0
        } else {
            features[i].abs().max(1e-6)
        };
        x_pos[i] += eps * mag;
        x_neg[i] -= eps * mag;
        let y_pos = run(&x_pos)?;
        let y_neg = run(&x_neg)?;
        // Central difference approximation of gradient magnitude
        importances[i] = ((y_pos - y_neg) / (2.0 * eps)).abs();
        // Also factor in absolute deviation from baseline output
        let dev = (y_pos - y0).abs().max((y_neg - y0).abs());
        importances[i] = 0.5 * (importances[i] + dev);
    }
    Ok(importances)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backend::{ModelBackend, Tensor};
    use crate::metadata::{ModelMetadata, ModelType, ModelVersion};

    struct MockLinear {
        w: Vec<f32>,
        b: f32,
        loaded: bool,
    }
    impl ModelBackend for MockLinear {
        fn load_model(
            &mut self,
            _model_bytes: &[u8],
            _m: &ModelMetadata,
        ) -> Result<(), BackendError> {
            self.loaded = true;
            Ok(())
        }
        fn infer(
            &self,
            inputs: HashMap<String, Tensor>,
        ) -> Result<HashMap<String, Tensor>, BackendError> {
            if !self.loaded {
                return Err(BackendError::InferenceError("not loaded".into()));
            }
            let t = inputs
                .get("input")
                .ok_or_else(|| BackendError::InvalidInput("missing input".into()))?;
            let y = t
                .data
                .iter()
                .zip(self.w.iter())
                .map(|(x, w)| x * w)
                .sum::<f32>()
                + self.b;
            let out = Tensor::new("output".into(), vec![1], vec![y])?;
            let mut map = HashMap::new();
            map.insert("output".into(), out);
            Ok(map)
        }
        fn backend_name(&self) -> &str {
            "mock_linear"
        }
        fn is_available(&self) -> bool {
            true
        }
    }

    #[test]
    fn importance_orders_by_weights() {
        let mut model = MockLinear {
            w: vec![0.1, 0.0, 2.0],
            b: 0.0,
            loaded: false,
        };
        let meta = ModelMetadata::new(
            "id".into(),
            "m".into(),
            ModelVersion::new(1, 0, 0),
            ModelType::Custom("mock".into()),
        );
        model.load_model(&[], &meta).unwrap();
        let baseline = vec![0.0, 0.0, 0.0];
        let feats = vec![1.0, 1.0, 1.0];
        let imp = compute_feature_importance(&model, &baseline, &feats).unwrap();
        // feature 3 should be most important by large margin
        assert!(imp[2] > imp[0] && imp[2] > imp[1]);
    }
}
