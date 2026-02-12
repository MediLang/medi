use tolvex_model::backends::CustomLinearBackend;
use tolvex_model::{ModelMetadata, ModelRegistry, ModelType, ModelVersion};

fn main() {
    let reg = ModelRegistry::new();

    // Build metadata compatible with current API
    let meta = ModelMetadata::new(
        "risk_demo".into(),
        "Risk Demo".into(),
        ModelVersion::new(0, 1, 0),
        ModelType::Custom("custom_linear".into()),
    );

    // Minimal custom backend and empty model bytes (backend is parameterized)
    let backend = Box::new(CustomLinearBackend::new(vec![0.2, 0.5, 0.3], 0.1));
    let model_bytes: Vec<u8> = vec![];

    // Register and query metadata
    reg.register(meta.clone(), backend, model_bytes)
        .expect("register");
    let fetched = reg.get_metadata("risk_demo").expect("get");
    println!("model {} {}", fetched.id, fetched.name);
}
