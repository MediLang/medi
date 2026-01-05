
# Compatibility Matrix (v0.0.13)

This page summarizes crate versions, Rust edition, and notable feature flags for the v0.0.13 release.

## Crates

- medi_ai
  - Version: 0.0.13
  - Edition: 2021
  - Features: None (default)

- medi_compliance
  - Version: 0.0.13
  - Edition: 2021
  - Features: None (default)

- medi_data
  - Version: 0.0.13
  - Edition: 2021
  - Features:
    - encryption-aes-gcm (optional)

- medi_model
  - Version: 0.0.13
  - Edition: 2021
  - Features:
    - onnx (stub)

- medi_stats
  - Version: 0.0.13
  - Edition: 2021
  - Features:
    - pvalue (enables `statrs`)
    - fhir (enables `medi_data`)

- pymedi
  - Version: 0.0.13
  - Edition: 2021
  - Notes: Python bindings built with maturin/PyO3

## Toolchain and Targets

- Rust: stable, 2021 edition
- LLVM backend: requires LLVM 15.x when enabling the `llvm` feature in compiler paths
- Tested targets: x86-64 (primary), wasm32-wasi (limited), riscv32 (initial)

Notes:
- Internal inter-crate path dependencies are aligned to this workspace version.
- See CHANGELOG v0.0.13 for highlights and tests for detailed coverage.
