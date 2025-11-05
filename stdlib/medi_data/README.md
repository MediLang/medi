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
