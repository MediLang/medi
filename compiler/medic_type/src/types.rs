// Type system definitions for Medic language in Rust
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
    Range(Box<MediType>),
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

impl MediType {
    /// Checks if the type is numeric (`Int` or `Float`).
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::MediType;
    /// assert!(MediType::Int.is_numeric());
    /// assert!(MediType::Float.is_numeric());
    /// assert!(!MediType::Bool.is_numeric());
    /// ```    pub fn is_numeric(&self) -> bool {
        matches!(self, MediType::Int | MediType::Float)
    }

    /// Determines whether two `MediType` values can be compared.
    ///
    /// Numeric types (`Int` and `Float`) are mutually comparable. Types that are exactly equal are also considered comparable. All other type combinations are not comparable.
    ///
    /// # Examples
    ///
    /// ```
    /// use medic_types::MediType;
    ///
    /// assert!(MediType::Int.is_comparable_with(&MediType::Float));
    /// assert!(MediType::String.is_comparable_with(&MediType::String));
    /// assert!(!MediType::Int.is_comparable_with(&MediType::String));
    /// ```    pub fn is_comparable_with(&self, other: &Self) -> bool {
        match (self, other) {
            // Numeric types can be compared with each other
            (MediType::Int, MediType::Int)
            | (MediType::Float, MediType::Float)
            | (MediType::Int, MediType::Float)
            | (MediType::Float, MediType::Int) => true,
            // Same types can be compared
            (a, b) if a == b => true,
            // Other types are not comparable
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum HealthcareEntityKind {
    Patient,
    Observation,
    Medication,
    // Add more as needed
}
