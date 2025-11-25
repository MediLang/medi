//! Model Registry and Pluggable Backend System for Medi
//!
//! Provides a thread-safe model registry with versioning, metadata support,
//! and pluggable backends for different ML frameworks (ONNX, TensorFlow Lite, custom).

pub mod backend;
pub mod backends;
pub mod metadata;
pub mod registry;

pub use backend::{BackendError, ModelBackend, Tensor};
pub use metadata::{ModelMetadata, ModelType, ModelVersion};
pub use registry::{ModelRegistry, RegistryError};

#[cfg(feature = "onnx")]
pub use backends::onnx::OnnxBackend;
