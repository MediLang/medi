// Type system definitions for Medi language in Rust
// This mirrors the MediType union and healthcare-specific types from TypeScript

use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum MediType {
    Int,
    Float,
    Bool,
    String,
    Void,
    Unknown,
    // For struct-like types (member access)
    Struct(HashMap<String, MediType>),
    // For records (named fields, e.g. healthcare queries)
    Record(Vec<(String, MediType)>),
    // For lists/arrays
    List(Box<MediType>),
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
