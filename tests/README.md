# Integration and System Tests

This directory hosts Medi's integration/system tests across compiler crates and end-to-end flows.

- **Location**
  - Top-level Rust integration tests in this folder and in `tests/src/`.
  - Workspace crates provide unit tests within each crate.

## Test Suites

- **Lexer/Parser/AST**
  - Files: `lexer_tests.rs`, `parser_tests.rs`
  - Goals: tokenization correctness (incl. error tokens), grammar coverage, recovery behavior.
  - Edge cases: invalid tokens, error at EOF, unmatched brackets, `|>` must not be tokenized as a pipeline operator (split into `|` and `>` by default).

- **Type system and checking**
  - Files: `src/hipaa.rs`, `src/privacy.rs`, `type_decl_integration.rs`, `src/let_annotations.rs`
  - Goals: type inference/annotations, HIPAA sink recognition, de-identification recognition via environment-driven metadata.

- **Codegen**
  - Files: `src/ir_codegen_tests.rs`, `src/x86_codegen_tests.rs`, `src/riscv_codegen_tests.rs`
  - Goals: IR and backend codegen smoke tests, minimal programs compile across targets (feature-gated LLVM backend).
  - Note: CI limits Windows to non-LLVM-dependent crates to ensure reliability.

- **Runtime/Size/Perf sanity**
  - Files: `src/riscv_size_tests.rs`
  - Goals: object size/sections sanity for embedded targets.

- **End-to-end execution hooks**
  - Files: `src/integration_exec.rs`
  - Goals: minimal flows that compile and run selected snippets via test harness.

## How to Run

- All tests (workspace):

```bash
cargo test --workspace
```

- Subset examples:

```bash
# Parser and lexer only
cargo test -p medic_parser -p medic_lexer

# Type checking focus
cargo test -p medic_type -p medic_typeck -- --nocapture

# Tests used on Windows CI matrix (avoid heavy LLVM deps)
cargo test -p medic_ast -p medic_parser -p medic_type -p medic_typeck --all-features
```

## Coverage in CI

- CI uses `cargo-tarpaulin` on Linux to generate `cobertura.xml`.
- Threshold enforced: `--fail-under 70`.
- Coverage is uploaded to Codecov. Configure thresholds in `codecov.yml`.

## Performance Gates

- Borrow checker performance gate runs in CI (job `borrowck-perf-gate`).
  - Quick criterion run, parses median time from output, fails if median exceeds threshold.

## Conventions

- Keep tests deterministic and hermetic; avoid network or file system dependencies unless scoped and documented.
- Prefer explicit assertions with clear failure messages aligned with clinician-friendly diagnostics.
- Add scenario comments where edge cases are non-obvious.
