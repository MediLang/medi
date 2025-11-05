//! Medi standard library for healthcare data primitives.
//!
//! Examples:
//! - FHIR JSON roundtrip for Patient and Observation
//! - In-memory querying via `QueryBuilder` with OR groups and date filters
//! - Basic validation via `validate_fhir` / `validate_patient` / `validate_observation`
//! - HL7 field components parsing (repetitions/components/subcomponents)
//! - File-backed storage (JSON) and optional AES-GCM storage
//!
//! Query OR groups + date filters:
//! ```
//! use medi_data::{fhir::FHIRPatient, fhir_query};
//! let data = vec![
//!   FHIRPatient { id: "p1".into(), given_name: None, family_name: Some("A".into()), birth_date: Some("1990-01-02".into()) },
//!   FHIRPatient { id: "p2".into(), given_name: None, family_name: Some("B".into()), birth_date: Some("1980-01-01".into()) },
//! ];
//! let q = fhir_query("Patient")
//!   .filter_contains("family_name", "A")
//!   .filter_date_ge("birth_date", "1985-01-01")
//!   .or_group()
//!   .filter_eq("id", "p2")
//!   .build();
//! let ids: Vec<&str> = q.execute_patients(&data).iter().map(|p| p.id.as_str()).collect();
//! assert!(ids.contains(&"p1"));
//! assert!(ids.contains(&"p2"));
//! ```
//!
//! HL7 components:
//! ```
//! use medi_data::hl7::parse_field_components;
//! let parsed = parse_field_components("A^B&C~D");
//! assert_eq!(parsed.len(), 2); // two repetitions
//! assert_eq!(parsed[0][1], vec!["B".to_string(), "C".to_string()]); // second component has subcomponents
//! ```
//!
//! Storage list/delete:
//! ```ignore
//! use medi_data::storage::SecureStore;
//! use medi_data::storage_file::FileStore;
//! let tmp = tempfile::tempdir().unwrap();
//! let store = FileStore::new(tmp.path()).unwrap();
//! store.save("k1", &42).unwrap();
//! store.save("k2", &43).unwrap();
//! let mut keys = store.list_keys().unwrap();
//! keys.sort();
//! assert_eq!(keys, vec!["k1", "k2"]);
//! store.remove("k1").unwrap();
//! ```
//!
//! Integration example: HL7 -> FHIR -> validate -> store -> query
//! ```ignore
//! use medi_data::{hl7_fhir::hl7_to_fhir_patient_minimal, validate::validate_patient, storage_file::FileStore, storage::SecureStore, fhir_query, query::Predicate};
//! let hl7 = "MSH|^~\\&|SRC|FAC|DST|HOSP|202501010101||ADT^A01|123|P|2.5\rPID|1|ALTID|P12345||Doe^Jane||19851224|F\r";
//! let patient = hl7_to_fhir_patient_minimal(hl7).unwrap();
//! validate_patient(&patient).unwrap();
//! let tmp = tempfile::tempdir().unwrap();
//! let store = FileStore::new(tmp.path()).unwrap();
//! store.save("patient_", &patient).unwrap();
//! let q = fhir_query("Patient")
//!   .any_in_group(vec![
//!     Predicate::ContainsCi { key: "family_name".into(), value: "doe".into() },
//!     Predicate::EqCi { key: "given_name".into(), value: "jane".into() },
//!   ])
//!   .filter_date_le("birth_date", "1985-12-31")
//!   .build();
//! let binding = [patient];
//! let out = q.execute_patients(&binding);
//! assert_eq!(out.len(), 1);
//! ```
//!
//! Getting Started:
//! - Define FHIR resources (Patient/Observation/MedicalEvent) via structs
//! - Validate with `validate_patient`/`validate_observation`/`validate_medical_event`
//! - Store data using `storage_file::FileStore` or the AES-GCM encrypted store (feature)
//! - Query in-memory collections using `fhir_query` and predicates
//!
//! Cookbook:
//! - Trim names and normalize dates using `sanitize` utilities
//! - Parse HL7 messages and map to FHIR using `hl7` + `hl7_fhir`
//! - Import/export NDJSON lines with `ndjson` helpers
pub mod dicom;
pub mod fhir;
pub mod fhir_any;
pub mod hl7;
pub mod hl7_fhir;
pub mod hl7_fhir_more;
pub mod hl7_fhir_obx;
pub mod ndjson;
pub mod query;
pub mod sanitize;
pub mod storage;
pub mod storage_composite;
pub mod storage_file;
pub mod validate;

pub use fhir::{FHIRObservation, FHIRPatient, FHIRResource};
pub use fhir_any::FHIRAny;
pub use query::{fhir_query, Query, QueryBuilder};
pub use validate::{validate_fhir, ValidationError};

#[cfg(feature = "encryption-aes-gcm")]
pub mod storage_aes_gcm;
#[cfg(feature = "encryption-aes-gcm")]
pub mod storage_encrypted;
