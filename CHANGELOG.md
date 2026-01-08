# Changelog

All notable changes to the Medi programming language will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Planned improvements and documentation updates.

## [v0.0.17] - 2026-01-08

### Changed
- Diagnostics:
  - Improved parser diagnostic help text for missing variable names (e.g., `let = 100` now suggests `let x = 100`).
  - Improved type error readability with clinician-friendly wording and type names.
  - Added a parser warning for a common type annotation typo pattern (`let x int = ...` missing `:`).
- Documentation:
  - Added clinician self-testing artifacts and a summary report at `docs/user_testing/task17_clinician_test_plan.md`.

### Fixed
- Tests:
  - Added coverage for the improved `let = ...` diagnostic help text.

### Release alignment
- Bumped workspace crate versions to `v0.0.17`.

## [v0.0.16] - 2026-01-08

### Added
- **Feature-gated pipeline operator token (`pipeline_op`)**:
  - Optional lexer support for tokenizing `|>` as a single `PipeGreater` token behind the `pipeline_op` Cargo feature.
  - Chunked lexer cross-chunk merge handling for `|` at end-of-chunk followed by `>` at start-of-next-chunk when `pipeline_op` is enabled.
  - Tests for both feature-off (default) and feature-on behavior.

### Changed
- Documentation:
  - Updated `LANG_SPEC.md` to clarify that `|>` remains reserved at the language level for post-v0.1 semantics, while lexer tokenization can be feature-enabled.
  - Updated `compiler/medic_lexer/README.md` to document `pipeline_op` and corrected test commands.
- Release alignment:
  - Bumped workspace crate versions to `v0.0.16`.

## [v0.0.15] - 2026-01-07

### Added
- **Documentation Generator (Task 15)**:
  - Added `medic docs` support for Markdown, HTML, and JSON outputs.
  - Added doc tests (`medic test --doc`) extracted from fenced `medi` code blocks.
  - Added source linking in generated HTML docs.
  - Added documentation lints (missing docs/examples and broken intra-doc links).
  - Added multi-version docs support via `--version` with `latest/` redirect.

### Changed
- Release alignment:
  - Bumped workspace crate versions to `v0.0.15`.

## [v0.0.14] - 2026-01-06

### Added
- **Medipack (Task 14)**:
  - Introduced a dedicated `medipack` CLI crate (`compiler/medipack`) inspired by Cargo.
  - Added `medipack init` project scaffolding with `--name`, `--description`, and `--lib`.
  - Added `medipack check` to validate `medi.toml` without building.
  - Added `medipack clean` to remove build artifacts (`target/`).
  - Added `medipack build` integration with `medic` including pass-through args and a `--release` stub.
  - Extended `medi.toml` manifest schema to include optional `description`, `authors`, `license`, `edition`, detailed dependencies (`version`/`path`/`git`), and a healthcare-specific `[fhir]` section.

### Changed
- Release alignment:
  - Bumped workspace crate versions to `v0.0.14`.

## [v0.0.13] - 2026-01-05

### Added
- **Interactive REPL (Task 13)**:
  - `medic repl` now supports session commands: `:help`, `:quit`/`:q`/`:exit`, `:load <file>`, and `:vars`.
  - Improved multi-line input handling with delimiter balancing and commit detection.
  - Incremental parse/typecheck across submissions with persisted session state.
  - Basic evaluation and result printing for common expressions (with clinician-friendly diagnostics for parse/type errors).
  - Unit tests covering command handling, multi-line behavior, and incremental bindings.

### Changed
- Dependency updates:
  - Bumped `pyo3` to `0.24.1`.
  - Bumped `js-yaml` to `3.14.2`.
- CI:
  - Pinned Python to `3.12` for non-Windows runners to avoid PyO3 incompatibilities with newer default Python versions.
- Release alignment:
  - Aligned `pymedi` versions for the `v0.0.13` release.
  - Updated documentation compatibility matrix for `v0.0.13`.

## [v0.0.12] - 2025-12-31

### Added
- **CLI Compiler UX improvements (Task 12)**:
  - Expanded `medic --help` with clinician-friendly descriptions and examples
  - Added validation and richer flag help for compilation options
  - `--target` alias for `--emit` (LLVM backend)
  - Additional unit tests covering CLI help/version output and argument parsing

### Changed
- Improved argument documentation for `medic check`, including typed JSON input (`--types-json`) and codegen flags when `llvm-backend` is enabled.

## [v0.0.11] - 2025-12-31

### Added
- **Python FFI prototype (Task 11)**:
  - New `bindings/pymedi` PyO3/maturin-based prototype module
  - Exposed minimal APIs (`mean`, `validate_fhir_patient`, `fhir_query_stub`) for interop demos
  - Example scripts under `bindings/pymedi/examples/`
  - Python tests under `bindings/pymedi/tests/`
  - CI job to build a Linux wheel and run pytest

## [v0.0.10] - 2025-12-24

### Added
- **Performance Benchmarking Suite (Task 10.2)** enhancements:
  - Config-driven benchmark runs with tier selection and CLI overrides
  - Additional healthcare workloads including NDJSON roundtrip I/O and concurrency-focused validation
  - Richer benchmark metadata in reports (git SHA, CLI args, thread count) and best-effort RSS deltas
- **Comparative Analysis with Python/R (Task 10.3)**:
  - Expanded Python/R comparator workloads to mirror Medi healthcare workloads
  - Comparison section in benchmark report with per-workload ratios (Python/Medi, R/Medi)
  - Generated narrative comparison report with charts under `compiler/benches/benchdata/`

### Fixed
- R comparator JSON output compatibility for stable deserialization in the Rust benchmark runner.

## [v0.0.9] - 2025-12-23

### Added
- **End-to-end example use cases (Task 9)** with runnable artifacts:
  - Clinical Data Exploration (`examples/use_cases/clinical_data_exploration.medi` + datasets + Rust harness)
  - Basic Regulatory Compliance (`examples/use_cases/regulatory_compliance.medi` + PHI sample + Rust harness)
  - Simple Statistical Analysis (`examples/use_cases/statistical_analysis.medi` + trial CSV + Rust harness)
  - Synthetic Data Testing (`examples/use_cases/synthetic_data_testing.medi` + Rust harness)

### Fixed
- Example code cleanup for strict workspace linting (fmt/clippy) and deterministic runs.

## [v0.0.8] - 2025-12-17

### Added
- **Basic IDE with Visual Analytics (Task 8)**: Web-based IDE prototype with compiler-backed analysis.
  - New `medic_ide_server` crate providing HTTP API endpoints:
    - `/analyze`: diagnostics and privacy span analysis
    - `/complete`: code completion with keywords, builtins, and prelude symbols
    - `/hover`: type info, privacy labels, and units for tokens
  - CodeMirror 6 editor in `examples/browser/` with:
    - Medi syntax highlighting (comments, strings, keywords, operators)
    - Multi-language support (Medi, Python, JavaScript, SQL)
    - Server-backed async code completion with local fallback
    - Hover info display on cursor movement (keyup/mouseup)
  - Visual Analytics panel with privacy distribution bar chart
  - Visual Programming prototype:
    - Node-based canvas with FHIR Query, Filter, Output nodes
    - Drag-and-drop positioning and port-based connections
    - Code generation that appends Medi code to editor

### Changed
- `TypeEnv::with_prelude()` now includes common function types (`fhir_query`, `println`, `print`, `log`, `deidentify`, `anonymize`)
- Added `collect_symbol_types()` method to `TypeEnv` for IDE completion support

## [v0.0.7] - 2025-12-15

### Added
- `medic` now provides a structured command-line interface with subcommands (`check`, `json`, `repl`, `docs`, `pack`) and built-in `--help/--version`.
- REPL: `medic repl` for interactive parsing/typechecking feedback (with basic history and multi-line buffering).
- Documentation generator: `medic docs` emits Markdown from `///` / `//!` doc comments.
- Package manager foundation: `medic pack init|list` creates and reads a minimal `medipack.toml` manifest.

## [v0.0.6] - 2025-12-11

### Added
- **Privacy and Compliance Checking (Task 6)**: Full implementation of HIPAA/GDPR compliance checking stage in the compiler pipeline.
  - PHI data flow analysis via `PrivacyAnnotation` labels (`PHI`, `Pseudonymized`, `Anonymized`, `Authorized`, `AuthorizedFor`)
  - Detection of unprotected PHI flowing to sinks (print, log, network, file)
  - Verification of proper anonymization through de-identification function recognition
  - `regulate` construct integration for scoped compliance blocks
- New `compiler/medic_typeck/src/compliance.rs` module with `check_compliance()` function and `ComplianceViolation` types
- AST support for `RegulateNode` with standard identifier and body block
- Parser support for `regulate <STANDARD> { ... }` blocks

### Changed
- Compiler pipeline now runs compliance checking after type checking, surfacing violations as dedicated errors
- Type checker tracks privacy labels per expression span for compliance analysis
- Codegen LLVM module updated to handle regulate statements

## [v0.0.5] - 2025-12-03

### Added
- Documentation: new Cookbook (`docs/content/cookbook.md`) with runnable-style snippets:
  - FHIR validation and queries (`medi_data`)
  - De-identification and compliance (`medi_compliance`)
  - Risk prediction utilities (`medi_ai`)
  - Basic stats (`medi_stats`)
- Docs navigation updated to include Cookbook in `docs/mkdocs.yml`.

### Changed
- Codegen: normalized LLVM IR labels for unit conversions (`uconv.f` instead of `uconv.fi`) to align with tests.
- Tests: adjusted AST construction in borrow checker and let-annotation tests for SmallVec/Vec consistency.

### Fixed
- Task file cleanup: resolved malformed JSON and duplicate blocks in `.taskmaster/tasks/tasks.json`.
- Clippy warnings: removed redundant `.into()` conversions in tests; added targeted `#[allow(clippy::useless_conversion)]` in test modules where feature matrices require it.

## [v0.0.4] - 2025-10-24

### Added
- Comprehensive runtime error/diagnostics system in `compiler/medic_runtime`:
  - `RuntimeError`, `MemoryErrorKind`, `SchedulerErrorKind`, and `RuntimeDiagnostic`.
  - Context-aware reporting via `set_error_reporter_with_context` and `medi_runtime_report!` capturing file/line/module/op tags.
  - Multi-listener support with `add_error_context_listener` for observability and tests.
- Context tags for channels and GC paths:
  - Channels: `channel.send`, `channel.recv`, `channel.try_recv` (on non-empty failures).
  - Crossbeam: `xchan.send`, `xchan.try_send` (on non-full failures), `xchan.recv`, `xchan.try_recv` (on non-empty failures).
  - GC: `gc.lock` emitted when GC mutex is poisoned.
- Real-time memory (rt_zones) diagnostics:
  - `rt_region.alloc_*_overflow` tags for region overflow cases.
  - `fixed_pool.alloc_exhausted`, `fixed_pool.invalid_free`, `fixed_pool.double_free`.
- Documentation: new `compiler/medic_runtime/README.md` covering errors, recovery policies, RT memory, and examples.

### Changed
- Hardened scheduler tests to be deterministic (barrier/ack-based), serialized with a test mutex to avoid global scheduler cross-talk.
- Improved scheduler responsiveness by unparking workers after enqueue and after task panics; reduced lock hold time on `stealers`.

### Fixed
- Eliminated flakiness in scheduler tests (`scheduler_smoke`, `scheduler_panic_recovery_skiptask`).
- Cleaned warnings in runtime and examples (removed unused imports/mut, replaced bare `expect` on GC locks with context-tagged reports).

### Performance
- Diagnostics path validated with `reporter_invocation_under_1ms` test (10k events bounded well under 2s total); RT-zone latency tests remain green.

## [v0.0.3] - 2025-09-30

### Added
- LLVM backend integration behind `llvm-backend` feature in `compiler/medic/` backed by `medic_codegen_llvm` using Inkwell/LLVM 15.
- Target codegen support and CI artifacts for:
  - x86-64
  - wasm32-wasi
  - riscv32 (initial RV32)
- CI matrix in `.github/workflows/ci.yml` covering fmt/lint, tests (Linux/macOS/Windows), quick bench smoke, and per-target codegen artifacts.

### Improved
- Clinician-friendly diagnostics in parser for common mistakes (e.g., `=` vs `==`, unmatched brackets, `per` operator suggestions).
- Unified lexer error tokens across streaming and chunked lexers: `TokenType::Error("Invalid token '<lexeme>'")` with optional debug logging.
- Parser recovery around early lexer error tokens with cleaner EOF handling.
- Clippy and formatting fixes across the workspace; pre-commit hooks green.

### Examples
- Minimal codegen smoke programs emitted per target in CI for inspection.

### Notes
- Requires system LLVM 15.x when enabling the `llvm-backend` feature.

## [v0.0.2] - 2025-09-02

### Added
- Comprehensive type annotation tests for `let` statements in `tests/src/let_annotations.rs`:
  - Match, mismatch, and inference scenarios
  - Healthcare-specific annotations (e.g., `PatientId`)
  - Complex nested struct annotations across `type` declarations
  - Side type-table assertions via `TypeChecker::type_table()`

- Healthcare-specific type system capabilities:
  - Core medical types: `PatientId`, `Vital`, `LabResult`, `FHIRPatient`, `Observation`, etc.
  - Type inference for local `let` bindings and expressions
  - Privacy annotations and enforcement (e.g., `PHI`, `Pseudonymized`, `Anonymized`, `Authorized`, `AuthorizedFor`)
  - Environment-driven sink/de-identification recognition (`TypeEnv::{get_sink_fn,is_deid_fn}` integration)
  - HIPAA-style privacy/data-flow checks with detailed diagnostics
  - Unit tests covering healthcare types and privacy flows (see `tests/src/hipaa.rs`)

### Changed
- Aligned tests with current checker semantics for unknown annotations: `let y: Foo;` binds `Unknown` and records it in the type table (no error).
- Minor test construction fixes to match current `medic_ast` shapes (boxed nodes and span placement) and avoid borrow conflicts when reading `TypeChecker` side tables.

## [v0.0.1] - 2025-08-26

### Added
- Core lexer and parser for Medi with healthcare-aware tokens and constructs, per `LANG_SPEC.md`.
- Unicode-aware location tracking and robust streaming/chunked lexing paths.
- Abstract Syntax Tree (AST) generation with position preservation and traversal utilities.
- Clinician-friendly error reporting with clear messages and recovery mechanisms.
- Pipeline operator planning: feature-gated design for `|>` with default-off behavior, docs, and tests.

### Changed
- Documentation updates across docs/ to reflect current syntax, testing strategy, and benchmarks.
- Parser collections migrated to feature-agnostic `NodeList<T>` where applicable (e.g., match arms, call arguments) to align with `medic_ast` type aliases.

### Fixed
- Stability and performance improvements in chunked lexer; test coverage for edge cases.
- Replaced raw `String`/`Vec` usages in parser with feature-agnostic types and constructors:
  - `IdentifierNode::from_string`/`from_str_name` used consistently to avoid `IdentifierName` mismatches across features.
  - Example code updated to construct identifiers via `from_str_name`.

### Parser: Match Expression Tests & Docs (2025-08-25)
- **Added**
  - Comprehensive tests for match expressions in expression context, covering:
    - Trailing commas after the last arm
    - Missing comma error between arms
    - Nested match expressions in arm bodies
    - Block bodies as arm expressions
    - Literal pattern variants: int, float, string, bool
  - Example of concise match expression syntax in getting-started docs.
- **Changed**
  - Parser architecture docs updated to explicitly document concise match expression in expressions and supported pattern forms.

### Lexer Benchmarks and File Extension Standardization (2025-05-30)
- **Added**
  - Comprehensive benchmarking suite for lexer performance
  - Documentation for lexer performance characteristics and usage guidelines
  - Support for `.medi` file extension as the standard for Medi source files
- **Changed**
  - Updated all documentation and examples to use `.medi` extension
  - Refactored lexer benchmarks to handle large files more efficiently
- **Fixed**
  - Memory usage in chunked lexer implementation
  - File handling in benchmark utilities

### PR #11: Nested Expressions & Statements (2025-05-24)
- **Added**
  - Support for complex nested binary and block expressions
  - Medical-specific operators: `of` and `per` with custom precedence
  - Comprehensive test coverage for edge cases
- **Fixed**
  - Handling of extra semicolons in block expressions
  - Error handling for chained comparisons

### PR #9, #10: Operator Precedence (2025-05-23, 2025-05-18)
- **Added**
  - Operator precedence rules for all operators
  - Detailed documentation for operator behavior
  - Docstrings for better code documentation
- **Changed**
  - Refactored expression parsing logic
  - Improved error messages for operator precedence issues

### PR #6, #7, #8: Recursive Descent Parser (2025-05-17, 2025-05-15, 2025-05-15)
- **Added**
  - Comprehensive recursive descent parser implementation
  - Detailed documentation for parser components
  - Test cases for various language constructs
- **Changed**
  - Multiple iterations to improve parser accuracy
  - Enhanced error recovery mechanisms

### PR #5: Core Parser Implementation (2025-05-12)
- **Added**
  - Initial recursive descent parser structure
  - Basic expression and statement parsing
  - Foundation for future language features

### PR #3, #4: Documentation Setup (2025-05-11)
- **Added**
  - MkDocs configuration with Material theme
  - Initial documentation structure
  - Project roadmap and contributing guidelines
- **Fixed**
  - Documentation build configuration
  - Broken links and formatting issues

### PR #1: Project Initialization (2025-05-10)
- **Added**
  - Core language syntax parser
  - Lexer and basic parser infrastructure
  - Initial test suite
  - Project structure and build configuration

## [Pre-release Development]

### Initial Commit
- Project initialization
- Basic language design documentation
- Development environment setup
