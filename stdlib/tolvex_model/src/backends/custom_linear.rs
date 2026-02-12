//! Example custom backend: simple linear model y = W·x + b

use crate::backend::{BackendError, ModelBackend, Tensor};
use crate::metadata::ModelMetadata;
use std::collections::HashMap;

/// A minimal custom backend implementing a linear model
#[derive(Clone)]
pub struct CustomLinearBackend {
    weights: Vec<f32>,
    bias: f32,
    loaded: bool,
}

impl CustomLinearBackend {
    pub fn new(weights: Vec<f32>, bias: f32) -> Self {
        Self {
            weights,
            bias,
            loaded: false,
        }
    }
}

impl Default for CustomLinearBackend {
    fn default() -> Self {
        Self {
            weights: vec![],
            bias: 0.0,
            loaded: false,
        }
    }
}

impl ModelBackend for CustomLinearBackend {
    fn load_model(
        &mut self,
        _model_bytes: &[u8],
        _metadata: &ModelMetadata,
    ) -> Result<(), BackendError> {
        // Nothing to load; parameters provided via constructor
        self.loaded = true;
        Ok(())
    }

    fn infer(
        &self,
        mut inputs: HashMap<String, Tensor>,
    ) -> Result<HashMap<String, Tensor>, BackendError> {
        if !self.loaded {
            return Err(BackendError::InferenceError("Model not loaded".into()));
        }
        let input = inputs
            .remove("input")
            .ok_or_else(|| BackendError::InvalidInput("missing 'input' tensor".into()))?;
        if input.shape.len() != 1 {
            return Err(BackendError::InvalidInput("input must be 1-D".into()));
        }
        if input.data.len() != self.weights.len() {
            return Err(BackendError::InvalidInput(format!(
                "input length {} != weights length {}",
                input.data.len(),
                self.weights.len()
            )));
        }
        let dot: f32 = input
            .data
            .iter()
            .zip(self.weights.iter())
            .map(|(x, w)| x * w)
            .sum::<f32>()
            + self.bias;
        let output = Tensor::new("output".into(), vec![1], vec![dot])?;
        let mut out = HashMap::new();
        out.insert("output".into(), output);
        Ok(out)
    }

    fn backend_name(&self) -> &str {
        "custom_linear"
    }

    fn is_available(&self) -> bool {
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn custom_linear_infer() {
        let mut backend = CustomLinearBackend::new(vec![0.5, 1.5, -1.0], 0.25);
        backend
            .load_model(
                &[],
                &ModelMetadata {
                    id: "id".into(),
                    name: "name".into(),
                    version: crate::metadata::ModelVersion::new(1, 0, 0),
                    model_type: crate::metadata::ModelType::Custom("custom".into()),
                    description: None,
                    author: None,
                    created_at: chrono::Utc::now().to_rfc3339(),
                    updated_at: None,
                    input_shapes: Default::default(),
                    output_shapes: Default::default(),
                    tags: Default::default(),
                },
            )
            .unwrap();
        let mut inputs = HashMap::new();
        inputs.insert(
            "input".into(),
            Tensor::new("input".into(), vec![3], vec![2.0, -1.0, 0.5]).unwrap(),
        );
        let out = backend.infer(inputs).unwrap();
        let y = out.get("output").unwrap();
        assert_eq!(y.shape, vec![1]);
        // y = 0.5*2.0 + 1.5*(-1.0) + (-1.0)*0.5 + 0.25 = 1.0 - 1.5 - 0.5 + 0.25 = -0.75
        assert!((y.data[0] - (-0.75)).abs() < 1e-6);
    }
}
