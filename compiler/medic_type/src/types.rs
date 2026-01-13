// Type system definitions for Medic language in Rust
// This mirrors the MediType union and healthcare-specific types from TypeScript

use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum MediType {
    Int,
    Float,
    Bool,
    String,
    /// Type variable for generics (e.g., T, U)
    TypeVar(String),
    /// Absence of a value
    Void,
    Unknown,
    Range(Box<MediType>),
    /// Quantity wrapper for numeric values, e.g., results of medical operators like 'of'
    Quantity(Box<MediType>),
    // For struct-like types (member access)
    Struct(HashMap<String, MediType>),
    // For records (named fields, e.g. healthcare queries)
    Record(Vec<(String, MediType)>),
    // For lists/arrays
    List(Box<MediType>),
    /// Generic healthcare entity bucket (kept for backward-compatibility)
    HealthcareEntity(HealthcareEntityKind),
    /// Domain-specific first-class types
    PatientId,
    /// Electronic health record or chart abstraction
    MedicalRecord,
    Vital,
    LabResult,
    FHIRPatient,
    Observation,
    /// Clinical diagnosis type
    Diagnosis,
    /// Medication entity (order/administration)
    Medication,
    Function {
        params: Vec<MediType>,
        return_type: Box<MediType>,
        /// Optional privacy annotations for each parameter (parallel to params)
        param_privacy: Option<Vec<PrivacyAnnotation>>,
        /// Optional privacy annotation for the return type
        return_privacy: Option<PrivacyAnnotation>,
    },
    /// Named generic type application, e.g., FHIRBundle<Observation>
    Named {
        name: String,
        args: Vec<MediType>,
    },
}

impl MediType {
    /// Checks if the type is numeric (`Int` or `Float`).
    ///
    /// # Examples
    ///
    /// ```
    /// use medic_type::types::MediType;
    /// assert!(MediType::Int.is_numeric());
    /// assert!(MediType::Float.is_numeric());
    /// assert!(!MediType::Bool.is_numeric());
    /// ```
    pub fn is_numeric(&self) -> bool {
        match self {
            MediType::Int | MediType::Float => true,
            MediType::Quantity(inner) => inner.is_numeric(),
            _ => false,
        }
    }

    /// Determines whether two `MediType` values can be compared.
    ///
    /// Numeric types (`Int` and `Float`) are mutually comparable. Types that are exactly equal are also considered comparable. All other type combinations are not comparable.
    ///
    /// # Examples
    ///
    /// ```
    /// use medic_type::types::MediType;
    ///
    /// assert!(MediType::Int.is_comparable_with(&MediType::Float));
    /// assert!(MediType::String.is_comparable_with(&MediType::String));
    /// assert!(!MediType::Int.is_comparable_with(&MediType::String));
    /// ```
    pub fn is_comparable_with(&self, other: &Self) -> bool {
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

    /// Simple assignability relation used by the basic type checker.
    /// Currently allows exact type matches and Int -> Float widening.
    pub fn is_assignable_to(&self, target: &Self) -> bool {
        match (self, target) {
            // Exact match
            (a, b) if a == b => true,
            // Widening conversion: Int to Float
            (MediType::Int, MediType::Float) => true,
            // Quantity assignability (structurally): Quantity(T) -> Quantity(U) if T -> U
            (MediType::Quantity(inner_s), MediType::Quantity(inner_t)) => {
                inner_s.is_assignable_to(inner_t)
            }
            // List assignability: List(T) -> List(U) if T -> U
            (MediType::List(inner_s), MediType::List(inner_t)) => inner_s.is_assignable_to(inner_t),
            // Named generic assignability: Name<Args...> -> Name<Args...> if all args assignable
            (MediType::Named { name: sn, args: sa }, MediType::Named { name: tn, args: ta }) => {
                sn == tn
                    && sa.len() == ta.len()
                    && sa.iter().zip(ta.iter()).all(|(s, t)| s.is_assignable_to(t))
            }
            _ => false,
        }
    }
}

/// Coarse sink categories used for contextual authorization.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SinkClass {
    Log,
    Print,
    Export,
    Network,
    File,
}

/// Privacy label for data classification and access control.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrivacyAnnotation {
    /// Protected Health Information; sensitive by default
    PHI,
    /// Pseudonymized data (identifiers replaced, but still potentially re-identifiable)
    Pseudonymized,
    /// De-identified / anonymized data
    Anonymized,
    /// PHI that has explicit authorization for the current flow
    Authorized,
    /// PHI with explicit authorization limited to the specified sink class
    AuthorizedFor(SinkClass),
}

impl PrivacyAnnotation {
    /// Conservative join of two privacy labels for data-flow propagation.
    /// Rules:
    /// - PHI with anything -> PHI (most restrictive dominates)
    /// - Authorized with Authorized -> Authorized
    /// - Authorized with Anonymized -> Authorized (still PHI-capable)
    /// - Authorized with Pseudonymized -> PHI (conservatively treat as PHI)
    /// - Pseudonymized with Pseudonymized -> Pseudonymized
    /// - Pseudonymized with Anonymized -> Pseudonymized
    /// - Anonymized with Anonymized -> Anonymized
    pub fn join(self, other: PrivacyAnnotation) -> PrivacyAnnotation {
        use PrivacyAnnotation::*;
        match (self, other) {
            (PHI, _) | (_, PHI) => PHI,
            (Authorized, Authorized) => Authorized,
            (Authorized, Anonymized) | (Anonymized, Authorized) => Authorized,
            (Authorized, Pseudonymized) | (Pseudonymized, Authorized) => PHI,
            // AuthorizedFor joins
            (AuthorizedFor(a), AuthorizedFor(b)) => {
                if a == b {
                    AuthorizedFor(a)
                } else {
                    PHI
                }
            }
            (AuthorizedFor(_a), Authorized) | (Authorized, AuthorizedFor(_a)) => Authorized,
            (AuthorizedFor(_a), Anonymized) | (Anonymized, AuthorizedFor(_a)) => AuthorizedFor(_a),
            (AuthorizedFor(_), Pseudonymized) | (Pseudonymized, AuthorizedFor(_)) => PHI,
            (Pseudonymized, Pseudonymized) => Pseudonymized,
            (Pseudonymized, Anonymized) | (Anonymized, Pseudonymized) => Pseudonymized,
            (Anonymized, Anonymized) => Anonymized,
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
