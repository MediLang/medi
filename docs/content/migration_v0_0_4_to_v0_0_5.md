# Migration: v0.0.4 → v0.0.5

- medi_data: new FHIR types (Medication, Procedure, Condition, Encounter, DiagnosticReport)
- medi_compliance: profile-aware rule engine scaffolding
- medi_ai: risk helpers consolidated
- medi_stats: added bootstrap, survival, streaming modules

## Steps
- Update Cargo.toml to latest versions
- Adjust imports for new modules and re-exports
- Validate examples still compile: `cargo test --workspace`
