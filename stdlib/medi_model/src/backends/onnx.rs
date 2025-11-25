//! ONNX Runtime backend implementation
//!
//! Note: This is a stub implementation. Full ONNX Runtime integration requires
//! additional work to match the current ort API (v2.0.0-rc.10).
//! For production use, implement proper tensor conversion and session management.

use crate::backend::{BackendError, ModelBackend, Tensor};
use crate::metadata::ModelMetadata;
use std::collections::HashMap;

/// ONNX Runtime backend (stub implementation)
pub struct OnnxBackend {
    model_loaded: bool,
    _model_bytes: Vec<u8>,
}

impl OnnxBackend {
    pub fn new() -> Self {
        Self {
            model_loaded: false,
            _model_bytes: Vec::new(),
        }
    }
}

impl Default for OnnxBackend {
    fn default() -> Self {
        Self::new()
    }
}

impl ModelBackend for OnnxBackend {
    fn load_model(
        &mut self,
        model_bytes: &[u8],
        _metadata: &ModelMetadata,
    ) -> Result<(), BackendError> {
        // TODO: Implement actual ONNX Runtime session creation
        // This requires matching the current ort API which has changed significantly
        self._model_bytes = model_bytes.to_vec();
        self.model_loaded = true;
        Ok(())
    }

    fn infer(
        &self,
        inputs: HashMap<String, Tensor>,
    ) -> Result<HashMap<String, Tensor>, BackendError> {
        if !self.model_loaded {
            return Err(BackendError::InferenceError("Model not loaded".into()));
        }

        // TODO: Implement actual ONNX Runtime inference
        // For now, return a stub response
        let mut result = HashMap::new();
        for (name, tensor) in inputs {
            // Echo input as output for testing
            result.insert(format!("{}_output", name), tensor);
        }
        Ok(result)
    }

    fn backend_name(&self) -> &str {
        "onnx"
    }

    fn is_available(&self) -> bool {
        // ONNX Runtime is available if the feature is enabled
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn onnx_backend_creation() {
        let backend = OnnxBackend::new();
        assert_eq!(backend.backend_name(), "onnx");
        assert!(backend.is_available());
    }

    // Note: Full integration tests would require actual ONNX model files
    // These would be added in integration tests with sample models
}
