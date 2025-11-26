# Medi Stdlib: Integration Tests and Benches Overview

This document summarizes the end-to-end integration scenarios across stdlib crates and how to run benchmarks.

## Scenarios

- medi_data
  - HL7 → FHIR → Validate → Store → Query
    - File: stdlib/medi_data/tests/integration_flow.rs
  - Bundle → Validate → Store
    - File: stdlib/medi_data/tests/bundle_integration.rs

- medi_compliance
  - FHIR → Compliance check → Anonymize → Report
    - File: stdlib/medi_compliance/tests/integration_anonymize_report.rs

- medi_ai
  - Patient → Risk prediction → Stratification
    - File: stdlib/medi_ai/tests/integration_risk_workflow.rs

## Test Data Generators

- Module: medi_data::testdata
  - patient_factory(family, given, dob) -> FHIRPatient
  - bundle_factory(n) -> FHIRBundle (Collection)
  - synthetic_lab_results(n) -> Vec<FHIRObservation>

## Running

- Run all tests:
  ```bash
  cargo test --workspace
  ```

- Run integration benches (Criterion):
  ```bash
  cargo bench -p medi_data --bench integration_benches
  ```

## CI

- GitHub Actions runs fmt, clippy, tests on Linux/macOS/Windows.
- Coverage via cargo-tarpaulin on Linux with cobertura.xml artifact.
