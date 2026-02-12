//! Model Registry and Pluggable Backend System for Medi
//!
//! Provides a thread-safe model registry with versioning, metadata support,
//! and pluggable backends for different ML frameworks (ONNX, TensorFlow Lite, custom).

pub mod backend;
pub mod backends;
pub mod calibration;
pub mod fairness;
pub mod feature_importance;
pub mod metadata;
pub mod metrics;
pub mod quantization;
pub mod registry;

pub use backend::{BackendError, ModelBackend, Tensor};
pub use calibration::IsotonicRegression;
pub use calibration::PlattScaling;
pub use fairness::{demographic_parity, equal_opportunity, GroupStats};
pub use feature_importance::compute_feature_importance;
pub use metadata::{ModelMetadata, ModelType, ModelVersion};
pub use metrics::{Accuracy, Precision, Recall, RocAuc, ValidationMetric, F1};
pub use quantization::{dequantize_linear_i8, quantize_linear_i8, QuantParams};
pub use registry::{ModelRegistry, RegistryError};

#[cfg(feature = "onnx")]
pub use backends::onnx::OnnxBackend;
