// Type environment for Medic language in Rust
// Mimics TypeScript's TypeEnv for variable/function scopes

use medic_type::types::MediType;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct TypeEnv {
    parent: Option<Box<TypeEnv>>,
    symbols: HashMap<String, MediType>,
}

impl Default for TypeEnv {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeEnv {
    pub fn new() -> Self {
        TypeEnv {
            parent: None,
            symbols: HashMap::new(),
        }
    }

    /// Create a new environment pre-populated with common primitives and healthcare types.
    pub fn with_prelude() -> Self {
        let mut env = Self::new();
        use medic_type::types::MediType;
        // Primitives
        env.insert("Int".to_string(), MediType::Int);
        env.insert("Float".to_string(), MediType::Float);
        env.insert("Bool".to_string(), MediType::Bool);
        env.insert("String".to_string(), MediType::String);
        // Healthcare domain
        env.insert("PatientId".to_string(), MediType::PatientId);
        env.insert("Vital".to_string(), MediType::Vital);
        env.insert("LabResult".to_string(), MediType::LabResult);
        env.insert("FHIRPatient".to_string(), MediType::FHIRPatient);
        env.insert("Observation".to_string(), MediType::Observation);
        env.insert("Diagnosis".to_string(), MediType::Diagnosis);
        env.insert("Medication".to_string(), MediType::Medication);
        env.insert("MedicalRecord".to_string(), MediType::MedicalRecord);
        env
    }

    pub fn with_parent(parent: TypeEnv) -> Self {
        TypeEnv {
            parent: Some(Box::new(parent)),
            symbols: HashMap::new(),
        }
    }

    pub fn insert(&mut self, name: String, typ: MediType) {
        self.symbols.insert(name, typ);
    }

    pub fn get(&self, name: &str) -> Option<&MediType> {
        match self.symbols.get(name) {
            Some(typ) => Some(typ),
            None => self.parent.as_ref().and_then(|p| p.get(name)),
        }
    }
}
