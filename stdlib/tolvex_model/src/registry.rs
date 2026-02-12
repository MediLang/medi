//! Thread-safe model registry with versioning support

use crate::backend::{BackendError, ModelBackend, Tensor};
use crate::metadata::{ModelMetadata, ModelVersion};
use parking_lot::RwLock;
use std::collections::HashMap;
use std::sync::Arc;
use thiserror::Error;

/// Errors that can occur during registry operations
#[derive(Debug, Error)]
pub enum RegistryError {
    #[error("Model not found: {0}")]
    ModelNotFound(String),
    #[error("Model already exists: {0}")]
    ModelAlreadyExists(String),
    #[error("Backend error: {0}")]
    BackendError(#[from] BackendError),
    #[error("Serialization error: {0}")]
    SerializationError(String),
    #[error("Invalid model data: {0}")]
    InvalidModelData(String),
}

/// Registered model entry containing metadata, backend, and model bytes
struct ModelEntry {
    metadata: ModelMetadata,
    backend: Box<dyn ModelBackend>,
    model_bytes: Vec<u8>,
}

/// Thread-safe model registry
///
/// Provides registration, lookup, and inference for models with versioning support.
pub struct ModelRegistry {
    models: Arc<RwLock<HashMap<String, ModelEntry>>>,
}

impl ModelRegistry {
    /// Create a new empty registry
    pub fn new() -> Self {
        Self {
            models: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Register a new model with its backend
    pub fn register(
        &self,
        metadata: ModelMetadata,
        backend: Box<dyn ModelBackend>,
        model_bytes: Vec<u8>,
    ) -> Result<(), RegistryError> {
        let mut models = self.models.write();

        if models.contains_key(&metadata.id) {
            return Err(RegistryError::ModelAlreadyExists(metadata.id.clone()));
        }

        let mut backend_mut = backend;
        backend_mut.load_model(&model_bytes, &metadata)?;

        models.insert(
            metadata.id.clone(),
            ModelEntry {
                metadata,
                backend: backend_mut,
                model_bytes,
            },
        );

        Ok(())
    }

    /// Get model metadata by ID
    pub fn get_metadata(&self, model_id: &str) -> Result<ModelMetadata, RegistryError> {
        let models = self.models.read();
        models
            .get(model_id)
            .map(|entry| entry.metadata.clone())
            .ok_or_else(|| RegistryError::ModelNotFound(model_id.to_string()))
    }

    /// List all registered model IDs
    pub fn list_models(&self) -> Vec<String> {
        let models = self.models.read();
        models.keys().cloned().collect()
    }

    /// Find models by version
    pub fn find_by_version(&self, version: &ModelVersion) -> Vec<ModelMetadata> {
        let models = self.models.read();
        models
            .values()
            .filter(|entry| &entry.metadata.version == version)
            .map(|entry| entry.metadata.clone())
            .collect()
    }

    /// Find models by tag
    pub fn find_by_tag(&self, key: &str, value: &str) -> Vec<ModelMetadata> {
        let models = self.models.read();
        models
            .values()
            .filter(|entry| {
                entry
                    .metadata
                    .tags
                    .get(key)
                    .map(|v| v == value)
                    .unwrap_or(false)
            })
            .map(|entry| entry.metadata.clone())
            .collect()
    }

    /// Run inference on a registered model
    pub fn infer(
        &self,
        model_id: &str,
        inputs: HashMap<String, Tensor>,
    ) -> Result<HashMap<String, Tensor>, RegistryError> {
        let models = self.models.read();
        let entry = models
            .get(model_id)
            .ok_or_else(|| RegistryError::ModelNotFound(model_id.to_string()))?;

        entry.backend.infer(inputs).map_err(RegistryError::from)
    }

    /// Unregister a model by ID
    pub fn unregister(&self, model_id: &str) -> Result<(), RegistryError> {
        let mut models = self.models.write();
        models
            .remove(model_id)
            .ok_or_else(|| RegistryError::ModelNotFound(model_id.to_string()))?;
        Ok(())
    }

    /// Export model metadata and bytes for serialization
    pub fn export_model(&self, model_id: &str) -> Result<(ModelMetadata, Vec<u8>), RegistryError> {
        let models = self.models.read();
        let entry = models
            .get(model_id)
            .ok_or_else(|| RegistryError::ModelNotFound(model_id.to_string()))?;
        Ok((entry.metadata.clone(), entry.model_bytes.clone()))
    }
}

impl Default for ModelRegistry {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::metadata::{ModelMetadata, ModelType, ModelVersion};

    // Mock backend for testing
    struct MockBackend {
        name: String,
        loaded: bool,
    }

    impl MockBackend {
        fn new(name: String) -> Self {
            Self {
                name,
                loaded: false,
            }
        }
    }

    impl ModelBackend for MockBackend {
        fn load_model(
            &mut self,
            _model_bytes: &[u8],
            _metadata: &ModelMetadata,
        ) -> Result<(), BackendError> {
            self.loaded = true;
            Ok(())
        }

        fn infer(
            &self,
            inputs: HashMap<String, Tensor>,
        ) -> Result<HashMap<String, Tensor>, BackendError> {
            // Echo inputs as outputs for testing
            Ok(inputs)
        }

        fn backend_name(&self) -> &str {
            &self.name
        }

        fn is_available(&self) -> bool {
            true
        }
    }

    #[test]
    fn registry_register_and_get() {
        let registry = ModelRegistry::new();
        let metadata = ModelMetadata::new(
            "model-1".into(),
            "Test Model".into(),
            ModelVersion::new(1, 0, 0),
            ModelType::Custom("mock".into()),
        );
        let backend = Box::new(MockBackend::new("mock".into()));
        let model_bytes = vec![1, 2, 3, 4];

        registry
            .register(metadata.clone(), backend, model_bytes)
            .unwrap();

        let retrieved = registry.get_metadata("model-1").unwrap();
        assert_eq!(retrieved.id, "model-1");
        assert_eq!(retrieved.name, "Test Model");
    }

    #[test]
    fn registry_duplicate_registration_fails() {
        let registry = ModelRegistry::new();
        let metadata = ModelMetadata::new(
            "model-1".into(),
            "Test Model".into(),
            ModelVersion::new(1, 0, 0),
            ModelType::Custom("mock".into()),
        );
        let backend1 = Box::new(MockBackend::new("mock".into()));
        let backend2 = Box::new(MockBackend::new("mock".into()));

        registry
            .register(metadata.clone(), backend1, vec![1, 2, 3])
            .unwrap();
        let result = registry.register(metadata, backend2, vec![4, 5, 6]);
        assert!(result.is_err());
        assert!(matches!(
            result.unwrap_err(),
            RegistryError::ModelAlreadyExists(_)
        ));
    }

    #[test]
    fn registry_list_models() {
        let registry = ModelRegistry::new();
        let meta1 = ModelMetadata::new(
            "model-1".into(),
            "Model 1".into(),
            ModelVersion::new(1, 0, 0),
            ModelType::Custom("mock".into()),
        );
        let meta2 = ModelMetadata::new(
            "model-2".into(),
            "Model 2".into(),
            ModelVersion::new(1, 0, 0),
            ModelType::Custom("mock".into()),
        );

        registry
            .register(meta1, Box::new(MockBackend::new("mock".into())), vec![])
            .unwrap();
        registry
            .register(meta2, Box::new(MockBackend::new("mock".into())), vec![])
            .unwrap();

        let models = registry.list_models();
        assert_eq!(models.len(), 2);
        assert!(models.contains(&"model-1".to_string()));
        assert!(models.contains(&"model-2".to_string()));
    }

    #[test]
    fn registry_find_by_version() {
        let registry = ModelRegistry::new();
        let v1 = ModelVersion::new(1, 0, 0);
        let v2 = ModelVersion::new(2, 0, 0);

        let meta1 = ModelMetadata::new(
            "model-1".into(),
            "Model 1".into(),
            v1.clone(),
            ModelType::Custom("mock".into()),
        );
        let meta2 = ModelMetadata::new(
            "model-2".into(),
            "Model 2".into(),
            v2,
            ModelType::Custom("mock".into()),
        );

        registry
            .register(meta1, Box::new(MockBackend::new("mock".into())), vec![])
            .unwrap();
        registry
            .register(meta2, Box::new(MockBackend::new("mock".into())), vec![])
            .unwrap();

        let found = registry.find_by_version(&v1);
        assert_eq!(found.len(), 1);
        assert_eq!(found[0].id, "model-1");
    }

    #[test]
    fn registry_find_by_tag() {
        let registry = ModelRegistry::new();
        let meta1 = ModelMetadata::new(
            "model-1".into(),
            "Model 1".into(),
            ModelVersion::new(1, 0, 0),
            ModelType::Custom("mock".into()),
        )
        .with_tag("task".into(), "classification".into());

        let meta2 = ModelMetadata::new(
            "model-2".into(),
            "Model 2".into(),
            ModelVersion::new(1, 0, 0),
            ModelType::Custom("mock".into()),
        )
        .with_tag("task".into(), "regression".into());

        registry
            .register(meta1, Box::new(MockBackend::new("mock".into())), vec![])
            .unwrap();
        registry
            .register(meta2, Box::new(MockBackend::new("mock".into())), vec![])
            .unwrap();

        let found = registry.find_by_tag("task", "classification");
        assert_eq!(found.len(), 1);
        assert_eq!(found[0].id, "model-1");
    }

    #[test]
    fn registry_infer() {
        let registry = ModelRegistry::new();
        let metadata = ModelMetadata::new(
            "model-1".into(),
            "Test Model".into(),
            ModelVersion::new(1, 0, 0),
            ModelType::Custom("mock".into()),
        );
        let backend = Box::new(MockBackend::new("mock".into()));

        registry.register(metadata, backend, vec![]).unwrap();

        let mut inputs = HashMap::new();
        inputs.insert(
            "input".into(),
            Tensor::new("input".into(), vec![1, 3], vec![1.0; 3]).unwrap(),
        );

        let outputs = registry.infer("model-1", inputs).unwrap();
        assert_eq!(outputs.len(), 1);
        assert!(outputs.contains_key("input"));
    }

    #[test]
    fn registry_unregister() {
        let registry = ModelRegistry::new();
        let metadata = ModelMetadata::new(
            "model-1".into(),
            "Test Model".into(),
            ModelVersion::new(1, 0, 0),
            ModelType::Custom("mock".into()),
        );
        let backend = Box::new(MockBackend::new("mock".into()));

        registry.register(metadata, backend, vec![]).unwrap();
        assert_eq!(registry.list_models().len(), 1);

        registry.unregister("model-1").unwrap();
        assert_eq!(registry.list_models().len(), 0);
    }
}
