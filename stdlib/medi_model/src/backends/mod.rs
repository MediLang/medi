//! Backend implementations for different ML frameworks

#[cfg(feature = "onnx")]
pub mod onnx;

#[cfg(feature = "onnx")]
pub use onnx::OnnxBackend;
