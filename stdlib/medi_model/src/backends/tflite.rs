//! TensorFlow Lite backend (stub implementation)

use crate::backend::{BackendError, ModelBackend, Tensor};
use crate::metadata::ModelMetadata;
use std::collections::HashMap;

/// TensorFlow Lite backend (stub)
pub struct TfliteBackend {
    model_loaded: bool,
    _model_bytes: Vec<u8>,
}

impl TfliteBackend {
    pub fn new() -> Self {
        Self {
            model_loaded: false,
            _model_bytes: Vec::new(),
        }
    }
}

impl Default for TfliteBackend {
    fn default() -> Self {
        Self::new()
    }
}

impl ModelBackend for TfliteBackend {
    fn load_model(
        &mut self,
        model_bytes: &[u8],
        _metadata: &ModelMetadata,
    ) -> Result<(), BackendError> {
        // TODO: Implement real TFLite interpreter loading when crate is available
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
        // Stub: echo inputs with suffix
        let mut out = HashMap::new();
        for (k, v) in inputs {
            out.insert(format!("{k}_output"), v);
        }
        Ok(out)
    }

    fn backend_name(&self) -> &str {
        "tflite"
    }

    fn is_available(&self) -> bool {
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tflite_backend_creation() {
        let b = TfliteBackend::new();
        assert_eq!(b.backend_name(), "tflite");
        assert!(b.is_available());
    }
}
