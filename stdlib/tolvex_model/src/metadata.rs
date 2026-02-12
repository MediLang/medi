//! Model metadata and versioning structures

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Semantic version for models
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct ModelVersion {
    pub major: u32,
    pub minor: u32,
    pub patch: u32,
}

impl ModelVersion {
    pub fn new(major: u32, minor: u32, patch: u32) -> Self {
        Self {
            major,
            minor,
            patch,
        }
    }

    pub fn parse(s: &str) -> Result<Self, String> {
        let parts: Vec<&str> = s.split('.').collect();
        if parts.len() != 3 {
            return Err(format!("Invalid version format: {s}"));
        }
        let major = parts[0]
            .parse()
            .map_err(|_| format!("Invalid major version: {}", parts[0]))?;
        let minor = parts[1]
            .parse()
            .map_err(|_| format!("Invalid minor version: {}", parts[1]))?;
        let patch = parts[2]
            .parse()
            .map_err(|_| format!("Invalid patch version: {}", parts[2]))?;
        Ok(Self::new(major, minor, patch))
    }
}

impl std::fmt::Display for ModelVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}.{}", self.major, self.minor, self.patch)
    }
}

/// Model type/framework identifier
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ModelType {
    ONNX,
    TensorFlowLite,
    Custom(String),
}

/// Comprehensive metadata for a registered model
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModelMetadata {
    /// Unique model identifier
    pub id: String,
    /// Model name
    pub name: String,
    /// Model version
    pub version: ModelVersion,
    /// Model type/framework
    pub model_type: ModelType,
    /// Model description
    pub description: Option<String>,
    /// Author/creator
    pub author: Option<String>,
    /// Creation timestamp (ISO 8601)
    pub created_at: String,
    /// Last update timestamp (ISO 8601)
    pub updated_at: Option<String>,
    /// Input tensor shapes (name -> shape)
    pub input_shapes: HashMap<String, Vec<usize>>,
    /// Output tensor shapes (name -> shape)
    pub output_shapes: HashMap<String, Vec<usize>>,
    /// Additional custom tags/labels
    pub tags: HashMap<String, String>,
}

impl ModelMetadata {
    pub fn new(id: String, name: String, version: ModelVersion, model_type: ModelType) -> Self {
        Self {
            id,
            name,
            version,
            model_type,
            description: None,
            author: None,
            created_at: chrono::Utc::now().to_rfc3339(),
            updated_at: None,
            input_shapes: HashMap::new(),
            output_shapes: HashMap::new(),
            tags: HashMap::new(),
        }
    }

    pub fn with_description(mut self, description: String) -> Self {
        self.description = Some(description);
        self
    }

    pub fn with_author(mut self, author: String) -> Self {
        self.author = Some(author);
        self
    }

    pub fn with_input_shape(mut self, name: String, shape: Vec<usize>) -> Self {
        self.input_shapes.insert(name, shape);
        self
    }

    pub fn with_output_shape(mut self, name: String, shape: Vec<usize>) -> Self {
        self.output_shapes.insert(name, shape);
        self
    }

    pub fn with_tag(mut self, key: String, value: String) -> Self {
        self.tags.insert(key, value);
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn version_parse_and_display() {
        let v = ModelVersion::parse("1.2.3").unwrap();
        assert_eq!(v.major, 1);
        assert_eq!(v.minor, 2);
        assert_eq!(v.patch, 3);
        assert_eq!(v.to_string(), "1.2.3");
    }

    #[test]
    fn version_ordering() {
        let v1 = ModelVersion::new(1, 0, 0);
        let v2 = ModelVersion::new(1, 1, 0);
        let v3 = ModelVersion::new(2, 0, 0);
        assert!(v1 < v2);
        assert!(v2 < v3);
    }

    #[test]
    fn metadata_builder() {
        let meta = ModelMetadata::new(
            "model-1".into(),
            "Test Model".into(),
            ModelVersion::new(1, 0, 0),
            ModelType::ONNX,
        )
        .with_description("A test model".into())
        .with_author("Test Author".into())
        .with_input_shape("input".into(), vec![1, 3, 224, 224])
        .with_output_shape("output".into(), vec![1, 1000])
        .with_tag("task".into(), "classification".into());

        assert_eq!(meta.id, "model-1");
        assert_eq!(meta.name, "Test Model");
        assert_eq!(meta.description, Some("A test model".into()));
        assert_eq!(meta.input_shapes.get("input"), Some(&vec![1, 3, 224, 224]));
        assert_eq!(meta.tags.get("task"), Some(&"classification".into()));
    }
}
