//! Backend implementations for different ML frameworks

#[cfg(feature = "onnx")]
pub mod onnx;

#[cfg(feature = "onnx")]
pub use onnx::OnnxBackend;

// TensorFlow Lite backend (stub)
pub mod tflite;
pub use tflite::TfliteBackend;

// Example custom backend: simple linear model y = W*x + b
pub mod custom_linear;
pub use custom_linear::CustomLinearBackend;
