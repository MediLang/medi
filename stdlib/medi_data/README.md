# medi_data

A lightweight healthcare data module for Medi with FHIR-like structs, HL7/DICOM parsing helpers, storage backends, sanitization/validation, and in-memory querying.

## Workflow: Ingest → Sanitize → Validate → Store → Query → Export

### 1) Ingest

- Parse HL7 to FHIR-like types
```rust
use medi_data::hl7_fhir::{hl7_to_fhir_patient_minimal, hl7_to_medical_event_minimal};
let p = hl7_to_fhir_patient_minimal(hl7_msg).unwrap();
let e = hl7_to_medical_event_minimal(hl7_msg).unwrap();
```
- OBX → Observation
```rust
use medi_data::hl7_fhir_obx::hl7_to_observations_minimal;
let obs = hl7_to_observations_minimal(hl7_msg).unwrap();
```
- NDJSON
```rust
use medi_data::ndjson::{to_ndjson, from_ndjson};
let json = to_ndjson(&obs)?;
let back: Vec<_> = from_ndjson(&json)?;
```

### 2) Sanitize
```rust
use medi_data::sanitize::{trim_patient_names, normalize_patient_birth_date, canonize_observation_code, normalize_observation_unit};
let mut p = p;
trim_patient_names(&mut p);
normalize_patient_birth_date(&mut p);
for o in &mut obs { canonize_observation_code(o); normalize_observation_unit(o); }
```

### 3) Validate
```rust
use medi_data::validate::{validate_patient, validate_observation, validate_medical_event};
validate_patient(&p)?;
for o in &obs { validate_observation(o)?; }
validate_medical_event(&e)?;
```

### New FHIR resource types (Medication, Procedure, Condition, Encounter, DiagnosticReport)

```rust
use medi_data::fhir::*;
use medi_data::validate::{
  validate_medication, validate_procedure, validate_condition,
  validate_encounter, validate_diagnostic_report,
};

let med = FHIRMedication { id: "m1".into(), code: "RX123".into(), form: Some("tablet".into()), ingredients: vec!["ingA".into()] };
validate_medication(&med)?;

let proc_ = FHIRProcedure { id: "p1".into(), code: "PROC".into(), performed_date: Some("20250101".into()), subject: "pat1".into() };
validate_procedure(&proc_)?;

let cond = FHIRCondition { id: "c1".into(), code: "COND".into(), clinical_status: "active".into(), verification_status: "confirmed".into(), subject: "pat1".into() };
validate_condition(&cond)?;

let enc = FHIREncounter { id: "e1".into(), class_: Some("outpatient".into()), start_date: Some("2025-01-02".into()), end_date: None, subject: "pat1".into() };
validate_encounter(&enc)?;

let rep = FHIRDiagnosticReport { id: "r1".into(), code: "DRPT".into(), result_codes: vec!["OBS1".into(), "OBS2".into()], subject: "pat1".into() };
validate_diagnostic_report(&rep)?;

// JSON helpers
let s = medication_to_json(&med)?;
let back = medication_from_json(&s)?;
assert_eq!(back.id, med.id);
```

### 4) Store (File / Encrypted)
```rust
use medi_data::storage::{SecureStore};
use medi_data::storage_file::FileStore;
#[cfg(feature = "encryption-aes-gcm")]
use medi_data::storage_encrypted::EncryptedFileStore;

let fs = FileStore::new("./data")?;
fs.save("p1", &p)?;
#[cfg(feature = "encryption-aes-gcm")]
{
  let key = [0u8; 32];
  let enc = EncryptedFileStore::new("./secure", key)?;
  enc.save("p1", &p)?;
}
```

### 5) Query
```rust
use medi_data::{fhir_query};
let q = fhir_query("MedicalEvent")
    .filter_event_code_ci_contains("card")
    .filter_start_date_ge("2024-10-01")
    .filter_start_date_le("2024-10-31")
    .build();
let matched = q.execute_medical_events(&events);
```

#### Query over mixed resources (FHIRAny)
```rust
use medi_data::{fhir_any::FHIRAny, fhir_query};
use medi_data::fhir::*;

let any: Vec<FHIRAny> = vec![
  FHIRAny::Medication(FHIRMedication { id: "m1".into(), code: "RX123".into(), form: None, ingredients: vec![] }),
  FHIRAny::Condition(FHIRCondition { id: "c1".into(), code: "COND".into(), clinical_status: "active".into(), verification_status: "confirmed".into(), subject: "pat1".into() }),
  FHIRAny::Encounter(FHIREncounter { id: "e1".into(), class_: None, start_date: None, end_date: None, subject: "pat1".into() }),
];
let q = fhir_query("Any").filter_eq("subject", "pat1").build();
let out = q.execute_any(&any);
assert!(out.len() >= 2);
```

### Cookbook: HL7 Helpers
```rust
use medi_data::hl7_fhir_more::{
  hl7_to_medical_event_from_orc,
  hl7_to_observations_from_obr,
  hl7_obr_dates_minimal,
  hl7_observations_from_obr_with_time,
  hl7_extract_nk1_contacts,
};

let evt = hl7_to_medical_event_from_orc(hl7_msg).unwrap();
let obs = hl7_to_observations_from_obr(hl7_msg).unwrap();
let dates = hl7_obr_dates_minimal(hl7_msg);
let obs_with_time = hl7_observations_from_obr_with_time(hl7_msg).unwrap();
let contacts = hl7_extract_nk1_contacts(hl7_msg);
```

### Cookbook: Cross-resource helpers
```rust
use medi_data::convert::{link_conditions_to_encounters, diagnostic_report_from_observation_codes};
use medi_data::fhir::*;

let linked: Vec<(FHIRCondition, Option<FHIREncounter>)> =
  link_conditions_to_encounters(&conditions, &encounters);

let rep = diagnostic_report_from_observation_codes(
  "r1", "pat1", "DRPT", &["OBS1", "OBS2"],
);
```

### Cookbook: Bulk sanitize observations before validate/query
```rust
use medi_data::sanitize::sanitize_observations_bulk;
sanitize_observations_bulk(&mut observations);
for o in &observations { validate_observation(o)?; }
let q = fhir_query("Observation").filter_eq_ci("code", "HR").filter_eq_ci("unit", "bpm").build();
let matched = q.execute_observations(&observations);
```

### 6) Export
```rust
use medi_data::ndjson::to_ndjson;
let json = to_ndjson(&matched)?;
```

## Performance
- Criterion bench is included for Patient queries; additional benches demonstrate Observation and Event queries.

## Real datasets
- Optional ignored test `realdata.rs` uses env paths:
  - `MEDI_DATA_PATIENTS`
  - `MEDI_DATA_OBSERVATIONS`
  - `MEDI_DATA_EVENTS`

## Notes
- This is a pragmatic subset of FHIR/HL7 to enable basic flows, not full conformance. Add deeper validations and resource coverage as needed.

## Troubleshooting
- Validation fails for Observation.code: ensure you sanitize first (use `canonize_observation_code`) and supply uppercase codes.
- Missing units for HR/BP/TEMP: sanitize units (`normalize_observation_unit_alias` then `normalize_observation_unit`) and provide required units.
- HL7 parsing oddities: inputs may include spaces/newlines; parser trims segment lines and fields.
- Encrypted store: `EncryptedFileStore::from_env` expects a 64-hex-char AES-256 key.

## Performance tips
- Pre-sanitize and canonicalize data once before hot-path queries.
- Use grouped predicates to short-circuit quickly (OR-of-ANDs).
- Prefer exact predicates (Eq/EqCi) where possible; Contains/ContainsCi are slower.
- Benchmark with `cargo bench -p medi_data` and profile workloads; adjust data structures or caching accordingly.
