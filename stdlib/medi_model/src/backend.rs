//! Pluggable backend trait and error types for model inference

use crate::metadata::ModelMetadata;
use std::collections::HashMap;
use thiserror::Error;

/// Errors that can occur during backend operations
#[derive(Debug, Error)]
pub enum BackendError {
    #[error("Model loading failed: {0}")]
    LoadError(String),
    #[error("Inference failed: {0}")]
    InferenceError(String),
    #[error("Invalid input: {0}")]
    InvalidInput(String),
    #[error("Backend not available: {0}")]
    BackendUnavailable(String),
    #[error("Serialization error: {0}")]
    SerializationError(String),
}

/// Tensor data wrapper for inputs/outputs
#[derive(Debug, Clone)]
pub struct Tensor {
    pub name: String,
    pub shape: Vec<usize>,
    pub data: Vec<f32>,
}

impl Tensor {
    pub fn new(name: String, shape: Vec<usize>, data: Vec<f32>) -> Result<Self, BackendError> {
        let expected_size: usize = shape.iter().product();
        if data.len() != expected_size {
            return Err(BackendError::InvalidInput(format!(
                "Tensor {} data length {} does not match shape {:?} (expected {})",
                name,
                data.len(),
                shape,
                expected_size
            )));
        }
        Ok(Self { name, shape, data })
    }
}

/// Pluggable backend trait for model inference
///
/// Implementations provide framework-specific model loading and inference.
pub trait ModelBackend: Send + Sync {
    /// Load a model from bytes
    fn load_model(
        &mut self,
        model_bytes: &[u8],
        metadata: &ModelMetadata,
    ) -> Result<(), BackendError>;

    /// Run inference with the loaded model
    fn infer(
        &self,
        inputs: HashMap<String, Tensor>,
    ) -> Result<HashMap<String, Tensor>, BackendError>;

    /// Get backend name/identifier
    fn backend_name(&self) -> &str;

    /// Check if backend is available (dependencies installed, etc.)
    fn is_available(&self) -> bool;

    /// Optional: serialize model state for persistence
    fn serialize_model(&self) -> Result<Vec<u8>, BackendError> {
        Err(BackendError::SerializationError(
            "Serialization not supported by this backend".into(),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tensor_creation_valid() {
        let t = Tensor::new("input".into(), vec![2, 3], vec![1.0; 6]).unwrap();
        assert_eq!(t.name, "input");
        assert_eq!(t.shape, vec![2, 3]);
        assert_eq!(t.data.len(), 6);
    }

    #[test]
    fn tensor_creation_invalid_size() {
        let result = Tensor::new("input".into(), vec![2, 3], vec![1.0; 5]);
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), BackendError::InvalidInput(_)));
    }
}
