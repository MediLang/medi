# Integration Tests and Benches (medi_data)

This folder and the benches in `../benches` provide end-to-end scenarios and performance measurements.

## Integration Scenarios

1. HL7 → FHIR → Validate → Store → Query
   - File: `../tests/integration_flow.rs`
   - Flow: parse HL7 → convert to FHIR Patient → validate → store via FileStore → query via fhir_query.

2. Bundle → Validate → Store
   - File: `../tests/bundle_integration.rs`
   - Flow: generate two Patients → collection Bundle → validate → store to FileStore → JSON roundtrip.

## Test Data Generators

- Module: `medi_data::testdata`
  - `patient_factory(family, given, dob) -> FHIRPatient`
  - `bundle_factory(n) -> FHIRBundle` (Collection with n Patients)
  - `synthetic_lab_results(n) -> Vec<FHIRObservation>`

## Performance Benches

- File: `../benches/integration_benches.rs`
  - `hl7_to_fhir_minimal` (ms/op): HL7 parsing and minimal conversion
  - `bundle_validate_100` and `bundle_validate_1000` (ms/op): validation throughput

Run benches:

```bash
cargo bench -p medi_data
```

Notes:
- Criterion controls statistical sampling; first run compiles benches.
- Use `--bench integration_benches` to run only integration benches.
