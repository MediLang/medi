// Type system definitions for Medi language in Rust
// This mirrors the MediType union and healthcare-specific types from TypeScript

#[derive(Debug, Clone, PartialEq)]
pub enum MediType {
    Int,
    Float,
    Bool,
    String,
    Void,
    Unknown,
    HealthcareEntity(HealthcareEntityKind),
    Function {
        params: Vec<MediType>,
        return_type: Box<MediType>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum HealthcareEntityKind {
    Patient,
    Observation,
    Medication,
    // Add more as needed
}
