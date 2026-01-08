//! Model Registry with versioning and pluggable backend support.
//!
//! Provides a registry for managing ML models with metadata, versioning,
//! and support for different inference backends (ONNX, TFLite, custom).

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::{Arc, RwLock};

/// Error type for model operations
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ModelError {
    NotFound(String),
    InvalidInput(String),
    InferenceError(String),
    SerializationError(String),
}

impl std::fmt::Display for ModelError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ModelError::NotFound(msg) => write!(f, "Model not found: {msg}"),
            ModelError::InvalidInput(msg) => write!(f, "Invalid input: {msg}"),
            ModelError::InferenceError(msg) => write!(f, "Inference error: {msg}"),
            ModelError::SerializationError(msg) => write!(f, "Serialization error: {msg}"),
        }
    }
}

impl std::error::Error for ModelError {}

/// Output type for model predictions
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum OutputType {
    Probability,
    Classification,
    Regression,
    MultiLabel,
}

/// Metadata for a registered model
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModelMetadata {
    pub name: String,
    pub description: String,
    pub input_features: Vec<String>,
    pub output_type: OutputType,
    pub created_at: String,
    pub performance_metrics: HashMap<String, f64>,
}

impl Default for ModelMetadata {
    fn default() -> Self {
        Self {
            name: String::new(),
            description: String::new(),
            input_features: Vec::new(),
            output_type: OutputType::Probability,
            created_at: String::new(),
            performance_metrics: HashMap::new(),
        }
    }
}

/// Trait for pluggable model backends (ONNX, TFLite, custom)
pub trait ModelBackend: Send + Sync {
    /// Run inference on a single input
    fn predict(&self, features: &[f64]) -> Result<Vec<f64>, ModelError>;

    /// Run batch inference
    fn predict_batch(&self, features: &[Vec<f64>]) -> Result<Vec<Vec<f64>>, ModelError>;

    /// Get the backend type identifier
    fn backend_type(&self) -> &str;
}

/// A simple linear model backend for demonstration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LinearBackend {
    pub weights: Vec<f64>,
    pub bias: f64,
}

impl ModelBackend for LinearBackend {
    fn predict(&self, features: &[f64]) -> Result<Vec<f64>, ModelError> {
        if features.len() != self.weights.len() {
            return Err(ModelError::InvalidInput(format!(
                "Expected {} features, got {}",
                self.weights.len(),
                features.len()
            )));
        }
        let sum: f64 = self
            .weights
            .iter()
            .zip(features.iter())
            .map(|(w, x)| w * x)
            .sum();
        Ok(vec![(sum + self.bias).clamp(0.0, 1.0)])
    }

    fn predict_batch(&self, features: &[Vec<f64>]) -> Result<Vec<Vec<f64>>, ModelError> {
        features.iter().map(|f| self.predict(f)).collect()
    }

    fn backend_type(&self) -> &str {
        "linear"
    }
}

/// ONNX Runtime backend (stub - requires onnxruntime crate for full implementation)
#[derive(Debug, Clone)]
pub struct OnnxBackend {
    pub model_path: String,
    // In a full implementation, this would hold the ONNX session
}

impl OnnxBackend {
    pub fn new(model_path: impl Into<String>) -> Self {
        Self {
            model_path: model_path.into(),
        }
    }
}

impl ModelBackend for OnnxBackend {
    fn predict(&self, features: &[f64]) -> Result<Vec<f64>, ModelError> {
        // Stub: In production, use onnxruntime crate
        // For now, return a placeholder based on input sum
        let sum: f64 = features.iter().sum();
        Ok(vec![sum.abs().fract()])
    }

    fn predict_batch(&self, features: &[Vec<f64>]) -> Result<Vec<Vec<f64>>, ModelError> {
        features.iter().map(|f| self.predict(f)).collect()
    }

    fn backend_type(&self) -> &str {
        "onnx"
    }
}

/// TensorFlow Lite backend (stub - requires tflite crate for full implementation)
#[derive(Debug, Clone)]
pub struct TFLiteBackend {
    pub model_path: String,
    // In a full implementation, this would hold the TFLite interpreter
}

impl TFLiteBackend {
    pub fn new(model_path: impl Into<String>) -> Self {
        Self {
            model_path: model_path.into(),
        }
    }
}

impl ModelBackend for TFLiteBackend {
    fn predict(&self, features: &[f64]) -> Result<Vec<f64>, ModelError> {
        // Stub: In production, use tflite crate
        let sum: f64 = features.iter().sum();
        Ok(vec![sum.abs().fract()])
    }

    fn predict_batch(&self, features: &[Vec<f64>]) -> Result<Vec<Vec<f64>>, ModelError> {
        features.iter().map(|f| self.predict(f)).collect()
    }

    fn backend_type(&self) -> &str {
        "tflite"
    }
}

/// A registered model with its backend and metadata
pub struct RegisteredModel {
    pub id: String,
    pub version: String,
    pub backend: Arc<dyn ModelBackend>,
    pub metadata: ModelMetadata,
}

impl RegisteredModel {
    pub fn predict(&self, features: &[f64]) -> Result<Vec<f64>, ModelError> {
        self.backend.predict(features)
    }

    pub fn predict_batch(&self, features: &[Vec<f64>]) -> Result<Vec<Vec<f64>>, ModelError> {
        self.backend.predict_batch(features)
    }
}

/// Thread-safe model registry with versioning support
pub struct ModelRegistry {
    models: RwLock<HashMap<String, RegisteredModel>>,
}

impl Default for ModelRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl ModelRegistry {
    pub fn new() -> Self {
        Self {
            models: RwLock::new(HashMap::new()),
        }
    }

    /// Register a model with the given ID, version, backend, and metadata
    pub fn register(
        &self,
        id: impl Into<String>,
        version: impl Into<String>,
        backend: Arc<dyn ModelBackend>,
        metadata: ModelMetadata,
    ) -> Result<(), ModelError> {
        let id = id.into();
        let version = version.into();
        let key = format!("{id}:{version}");

        let model = RegisteredModel {
            id: id.clone(),
            version,
            backend,
            metadata,
        };

        let mut models = self
            .models
            .write()
            .map_err(|_| ModelError::InferenceError("Failed to acquire write lock".to_string()))?;
        models.insert(key, model);
        Ok(())
    }

    /// Get a model by ID and version
    pub fn get(&self, id: &str, version: &str) -> Result<&RegisteredModel, ModelError> {
        let key = format!("{id}:{version}");
        let models = self
            .models
            .read()
            .map_err(|_| ModelError::InferenceError("Failed to acquire read lock".to_string()))?;

        // Note: This is a simplified implementation. In production, you'd use
        // a different approach to avoid lifetime issues with the RwLock guard.
        if models.contains_key(&key) {
            // Return a reference - in practice, you'd clone or use Arc
            Err(ModelError::NotFound(format!(
                "Use get_and_predict for thread-safe access to model {key}"
            )))
        } else {
            Err(ModelError::NotFound(key))
        }
    }

    /// Get a model and run prediction (thread-safe)
    pub fn predict(
        &self,
        id: &str,
        version: &str,
        features: &[f64],
    ) -> Result<Vec<f64>, ModelError> {
        let key = format!("{id}:{version}");
        let models = self
            .models
            .read()
            .map_err(|_| ModelError::InferenceError("Failed to acquire read lock".to_string()))?;

        let model = models
            .get(&key)
            .ok_or_else(|| ModelError::NotFound(key.clone()))?;
        model.predict(features)
    }

    /// List all registered model IDs and versions
    pub fn list(&self) -> Result<Vec<(String, String)>, ModelError> {
        let models = self
            .models
            .read()
            .map_err(|_| ModelError::InferenceError("Failed to acquire read lock".to_string()))?;

        Ok(models
            .values()
            .map(|m| (m.id.clone(), m.version.clone()))
            .collect())
    }

    /// Remove a model by ID and version
    pub fn remove(&self, id: &str, version: &str) -> Result<(), ModelError> {
        let key = format!("{id}:{version}");
        let mut models = self
            .models
            .write()
            .map_err(|_| ModelError::InferenceError("Failed to acquire write lock".to_string()))?;

        models
            .remove(&key)
            .map(|_| ())
            .ok_or(ModelError::NotFound(key))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_linear_backend() {
        let backend = LinearBackend {
            weights: vec![0.5, 0.5],
            bias: 0.0,
        };
        let result = backend.predict(&[0.4, 0.6]).unwrap();
        assert!((result[0] - 0.5).abs() < 1e-10);
    }

    #[test]
    fn test_linear_backend_batch() {
        let backend = LinearBackend {
            weights: vec![1.0, 1.0],
            bias: 0.0,
        };
        let inputs = vec![vec![0.2, 0.3], vec![0.4, 0.1]];
        let results = backend.predict_batch(&inputs).unwrap();
        assert_eq!(results.len(), 2);
        assert!((results[0][0] - 0.5).abs() < 1e-10);
        assert!((results[1][0] - 0.5).abs() < 1e-10);
    }

    #[test]
    fn test_registry_register_and_predict() {
        let registry = ModelRegistry::new();
        let backend = Arc::new(LinearBackend {
            weights: vec![0.5, 0.5],
            bias: 0.0,
        });
        let metadata = ModelMetadata {
            name: "test_model".to_string(),
            description: "A test model".to_string(),
            input_features: vec!["f1".to_string(), "f2".to_string()],
            output_type: OutputType::Probability,
            created_at: "2026-01-08".to_string(),
            performance_metrics: HashMap::new(),
        };

        registry
            .register("diabetes_risk", "1.0.0", backend, metadata)
            .unwrap();

        let result = registry
            .predict("diabetes_risk", "1.0.0", &[0.4, 0.6])
            .unwrap();
        assert!((result[0] - 0.5).abs() < 1e-10);
    }

    #[test]
    fn test_registry_list() {
        let registry = ModelRegistry::new();
        let backend = Arc::new(LinearBackend {
            weights: vec![1.0],
            bias: 0.0,
        });

        registry
            .register("model_a", "1.0", backend.clone(), ModelMetadata::default())
            .unwrap();
        registry
            .register("model_b", "2.0", backend, ModelMetadata::default())
            .unwrap();

        let list = registry.list().unwrap();
        assert_eq!(list.len(), 2);
    }

    #[test]
    fn test_onnx_backend_stub() {
        let backend = OnnxBackend::new("model.onnx");
        assert_eq!(backend.backend_type(), "onnx");
        let result = backend.predict(&[0.5, 0.5]).unwrap();
        assert_eq!(result.len(), 1);
    }

    #[test]
    fn test_tflite_backend_stub() {
        let backend = TFLiteBackend::new("model.tflite");
        assert_eq!(backend.backend_type(), "tflite");
        let result = backend.predict(&[0.5, 0.5]).unwrap();
        assert_eq!(result.len(), 1);
    }
}
