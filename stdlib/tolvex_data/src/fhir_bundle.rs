use crate::fhir::{
    FHIRCondition, FHIRDiagnosticReport, FHIREncounter, FHIRMedication, FHIRObservation,
    FHIRPatient, FHIRProcedure,
};
use crate::fhir_any::FHIRAny;
use serde::{Deserialize, Serialize};
use std::collections::HashSet;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum BundleType {
    Collection,
    Transaction,
    Batch,
}

// --- 20.5: Processing stubs ---
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct BundleProcessReport {
    pub applied: usize,
    pub errors: Vec<String>,
}

#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum BundleProcessError {
    #[error("{0}")]
    Validation(String),
    #[error("Invalid bundle content for type: {0}")]
    InvalidForType(String),
}

/// Minimal processor trait. Host crate can implement this over its storage.
pub trait BundleProcessor {
    /// Called per-entry. Return Ok(()) if applied, Err(msg) to signal failure.
    fn apply(&mut self, entry: &FHIRAny) -> Result<(), String>;

    /// Optional hook for transaction begin; default no-op.
    fn begin(&mut self) {}
    /// Optional hook for transaction commit; default no-op.
    fn commit(&mut self) {}
    /// Optional hook for transaction rollback; default no-op.
    fn rollback(&mut self) {}
}

impl FHIRBundle {
    /// Process bundle according to its type using the provided processor.
    /// Transaction: stop on first error and rollback.
    /// Batch: continue, collect errors.
    pub fn process<P: BundleProcessor>(
        &self,
        processor: &mut P,
    ) -> Result<BundleProcessReport, BundleProcessError> {
        // Ensure structural validity first
        self.validate()
            .map_err(|e| BundleProcessError::Validation(e.to_string()))?;

        match self.bundle_type {
            BundleType::Transaction => {
                processor.begin();
                let mut applied = 0usize;
                for e in &self.entries {
                    if let Err(msg) = processor.apply(e) {
                        processor.rollback();
                        return Ok(BundleProcessReport {
                            applied,
                            errors: vec![msg],
                        });
                    }
                    applied += 1;
                }
                processor.commit();
                Ok(BundleProcessReport {
                    applied,
                    errors: vec![],
                })
            }
            BundleType::Batch => {
                let mut report = BundleProcessReport::default();
                for e in &self.entries {
                    match processor.apply(e) {
                        Ok(()) => report.applied += 1,
                        Err(msg) => report.errors.push(msg),
                    }
                }
                Ok(report)
            }
            BundleType::Collection => {
                Err(BundleProcessError::InvalidForType("Collection".to_string()))
            }
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct FHIRBundle {
    pub bundle_type: BundleType,
    pub entries: Vec<FHIRAny>,
}

impl FHIRBundle {
    pub fn new(bundle_type: BundleType, entries: Vec<FHIRAny>) -> Self {
        Self {
            bundle_type,
            entries,
        }
    }

    // --- Builders ---
    pub fn collection() -> Self {
        Self {
            bundle_type: BundleType::Collection,
            entries: Vec::new(),
        }
    }

    pub fn transaction() -> Self {
        Self {
            bundle_type: BundleType::Transaction,
            entries: Vec::new(),
        }
    }

    pub fn batch() -> Self {
        Self {
            bundle_type: BundleType::Batch,
            entries: Vec::new(),
        }
    }

    pub fn with_entry(mut self, entry: FHIRAny) -> Self {
        self.entries.push(entry);
        self
    }

    pub fn push_entry(&mut self, entry: FHIRAny) {
        self.entries.push(entry);
    }

    pub fn to_json(&self) -> Result<String, serde_json::Error> {
        serde_json::to_string(self)
    }

    pub fn from_json(s: &str) -> Result<Self, serde_json::Error> {
        serde_json::from_str::<Self>(s)
    }

    /// Basic validation of bundle structure.
    /// - Collection: entries can be any mix of resources
    /// - Transaction: requires non-empty and unique resource ids
    /// - Batch: requires non-empty
    pub fn validate(&self) -> Result<(), BundleError> {
        match self.bundle_type {
            BundleType::Collection => Ok(()),
            BundleType::Transaction => {
                if self.entries.is_empty() {
                    return Err(BundleError::EmptyTransactionOrBatch);
                }
                let mut seen: HashSet<&str> = HashSet::new();
                for r in &self.entries {
                    let id = match r {
                        FHIRAny::Patient(v) => v.id.as_str(),
                        FHIRAny::Observation(v) => v.id.as_str(),
                        FHIRAny::MedicalEvent(v) => v.id.as_str(),
                        FHIRAny::Medication(v) => v.id.as_str(),
                        FHIRAny::Procedure(v) => v.id.as_str(),
                        FHIRAny::Condition(v) => v.id.as_str(),
                        FHIRAny::Encounter(v) => v.id.as_str(),
                        FHIRAny::DiagnosticReport(v) => v.id.as_str(),
                    };
                    if !seen.insert(id) {
                        return Err(BundleError::DuplicateResourceId(id.to_string()));
                    }
                }
                Ok(())
            }
            BundleType::Batch => {
                if self.entries.is_empty() {
                    Err(BundleError::EmptyTransactionOrBatch)
                } else {
                    Ok(())
                }
            }
        }
    }

    /// Extract entries filtered by a simple discriminator function on FHIRAny.
    pub fn extract<P: Fn(&FHIRAny) -> bool>(&self, pred: P) -> Vec<&FHIRAny> {
        self.entries.iter().filter(|e| pred(e)).collect()
    }

    // --- Convenience typed extractors ---
    pub fn extract_patients(&self) -> Vec<&FHIRPatient> {
        self.entries
            .iter()
            .filter_map(|e| match e {
                FHIRAny::Patient(v) => Some(v),
                _ => None,
            })
            .collect()
    }

    pub fn extract_observations(&self) -> Vec<&FHIRObservation> {
        self.entries
            .iter()
            .filter_map(|e| match e {
                FHIRAny::Observation(v) => Some(v),
                _ => None,
            })
            .collect()
    }

    pub fn extract_conditions(&self) -> Vec<&FHIRCondition> {
        self.entries
            .iter()
            .filter_map(|e| match e {
                FHIRAny::Condition(v) => Some(v),
                _ => None,
            })
            .collect()
    }

    pub fn extract_encounters(&self) -> Vec<&FHIREncounter> {
        self.entries
            .iter()
            .filter_map(|e| match e {
                FHIRAny::Encounter(v) => Some(v),
                _ => None,
            })
            .collect()
    }

    pub fn extract_diagnostic_reports(&self) -> Vec<&FHIRDiagnosticReport> {
        self.entries
            .iter()
            .filter_map(|e| match e {
                FHIRAny::DiagnosticReport(v) => Some(v),
                _ => None,
            })
            .collect()
    }

    pub fn extract_procedures(&self) -> Vec<&FHIRProcedure> {
        self.entries
            .iter()
            .filter_map(|e| match e {
                FHIRAny::Procedure(v) => Some(v),
                _ => None,
            })
            .collect()
    }

    pub fn extract_medications(&self) -> Vec<&FHIRMedication> {
        self.entries
            .iter()
            .filter_map(|e| match e {
                FHIRAny::Medication(v) => Some(v),
                _ => None,
            })
            .collect()
    }
}

#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum BundleError {
    #[error("Transaction/Batch bundle must have at least one entry")]
    EmptyTransactionOrBatch,
    #[error("Duplicate resource id: {0}")]
    DuplicateResourceId(String),
    #[error("Invalid bundle content for type: {0}")]
    InvalidForType(String),
}
