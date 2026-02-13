# Tolvex Stdlib: Integration Tests and Benches Overview

This document summarizes the end-to-end integration scenarios across stdlib crates and how to run benchmarks.

## Scenarios

- tolvex_data
  - HL7 → FHIR → Validate → Store → Query
    - File: stdlib/tolvex_data/tests/integration_flow.rs
  - Bundle → Validate → Store
    - File: stdlib/tolvex_data/tests/bundle_integration.rs

- tolvex_compliance
  - FHIR → Compliance check → Anonymize → Report
    - File: stdlib/tolvex_compliance/tests/integration_anonymize_report.rs

- tolvex_ai
  - Patient → Risk prediction → Stratification
    - File: stdlib/tolvex_ai/tests/integration_risk_workflow.rs

## Test Data Generators

- Module: tolvex_data::testdata
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
  cargo bench -p tolvex_data --bench integration_benches
  ```

## CI

- GitHub Actions runs fmt, clippy, tests on Linux/macOS/Windows.
- Coverage via cargo-tarpaulin on Linux with cobertura.xml artifact.
