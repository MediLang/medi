<div align="center">

<img src="./docs/content/assets/tolvex-logo.png" alt="Tolvex Logo" width="200">

# The Tolvex Programming Language

[Website](https://tolvex.dev) | [Documentation](https://tolvex.dev/docs) | [Contributing](CONTRIBUTING.md) | [Discord](https://discord.gg/JxE6dD285R)

![License](https://img.shields.io/badge/License-MIT-blue) ![Status](https://img.shields.io/badge/Status-Pre--alpha%20(Prototype)-orange) ![CodeRabbit Pull Request Reviews](https://img.shields.io/coderabbit/prs/github/TolvexLang/tolvex?utm_source=oss&utm_medium=github&utm_campaign=TolvexLang%2Ftolvex&labelColor=171717&color=FF570A&link=https%3A%2F%2Fcoderabbit.ai&label=CodeRabbit+Reviews) [![codecov](https://codecov.io/gh/TolvexLang/tolvex/branch/main/graph/badge.svg)](https://codecov.io/gh/TolvexLang/tolvex)

</div>

Tolvex is a programming language purpose-built for healthcare, designed to transform medical analytics with unparalleled ease, speed, and security. With a beginner-friendly syntax inspired by Python and R, high performance rivaling Julia, Rust, and C++, and native support for healthcare standards like FHIR, HL7, and DICOM, Tolvex empowers clinicians, researchers, and developers to unlock insights from complex medical data.

From genomic analysis to real-time patient monitoring, clinical trials to hospital operations, Tolvex delivers secure, scalable, and clinician-friendly solutions.

> **Note:** Tolvex was previously known as "Medi". The language, compiler, and all tooling have been renamed as of v0.1.6. See the [CHANGELOG](CHANGELOG.md) for migration details.

## Why Tolvex?

Healthcare demands tools that balance accessibility, performance, security, and compliance. Existing languages fall short:

- **Python/R:** Versatile but slow for big data, lack native healthcare standards, and require complex integrations.
- **SAS/Stata:** Expensive, proprietary, and cumbersome for modern workflows.
- **Julia:** Fast but not healthcare-specific, with a smaller ecosystem.

Tolvex fills these gaps with:

| Challenge | Tolvex's Solution |
|-----------|-------------------|
| Performance on big data | LLVM-compiled, near-C++ speed |
| Healthcare standards | Native FHIR, HL7, DICOM, genomics (FASTQ, VCF) |
| Compliance | Built-in `regulate` blocks, PHI tracking, HIPAA/GDPR automation |
| Edge/IoT deployment | WebAssembly, RISC-V targets for wearables and medical devices |
| Accessibility | Clinician-friendly syntax, visual IDE |

## Key Features

- **Beginner-Friendly Syntax:** Python-like readability with R-style data pipelines (`|>`). Declarative constructs like `fhir_query` and `plot_kaplan_meier` simplify complex tasks.
- **High Performance:** Compiled to machine code via LLVM. Supports parallel processing, GPU acceleration (CUDA/OpenCL), and targets x86-64, WebAssembly, and RISC-V.
- **Medical Data Science & AI:** Built-in statistical methods (`kaplan_meier`, `sir_model`), pre-trained models for diagnostics, federated learning for privacy-preserving analytics.
- **Privacy & Compliance:** `federated` and `dp` constructs for differential privacy. `regulate` blocks for automated HIPAA/GDPR/FDA compliance checks.
- **Healthcare Interoperability:** Native FHIR, HL7, DICOM support. Integration with Python (`py_call`), R (`r_call`), and healthcare systems (Epic, Cerner, AWS HealthLake).

## Example

```tlvx
// Query patients with diabetes and analyze outcomes
let diabetic_patients = fhir_query("Patient")
    |> filter(condition: icd10("E11"))  // Type 2 diabetes
    |> join(observations: "HbA1c");

// Run survival analysis with compliance checks
regulate { standard: "HIPAA", checks: ["phi_protected"] };
let survival = kaplan_meier(diabetic_patients, event: "hospitalization");
plot_kaplan_meier(survival, title: "Diabetes Outcomes");
```

## Quick Start

```sh
# Clone and build
git clone https://github.com/TolvexLang/tolvex.git
cd tolvex
cargo build --workspace

# Run tests
cargo test --workspace

# Compile a Tolvex program
tlvxc --emit=x86_64 --out=program.o example.tlvx
```

## Current Status

| Component | Status |
|-----------|--------|
| Lexer / Parser / AST | ✅ Done |
| Type System (inference, healthcare types, privacy annotations) | ✅ Done |
| LLVM Backend (x86-64, WASM, RISC-V) | ✅ Done |
| Memory Management (GC, borrow checker, real-time zones) | ✅ Done |
| Standard Library (`tolvex_data`, `tolvex_stats`, `tolvex_compliance`, `tolvex_ai`) | ✅ Done |
| Privacy/Compliance Checker (HIPAA, PHI flow analysis) | ✅ Done |
| Basic IDE (syntax highlighting, code completion) | ✅ Done |
| Example Use Cases | ✅ Done |
| Documentation & Benchmarks | 🔄 In Progress |
| CLI Compiler (`tlvxc`) | ✅ Done |
| REPL | ✅ Done |
| Package Manager | ✅ Done |
| Python FFI | ✅ Done |

## Project Structure

```
tolvex/
├── compiler/          # Rust compiler crates (tlvxc_lexer, tlvxc_parser, etc.)
├── stdlib/            # Standard library (tolvex_data, tolvex_stats, tolvex_compliance, tolvex_ai)
├── tests/             # Integration tests
├── examples/          # Example programs (.tlvx)
└── docs/              # Documentation (MkDocs)
```

See [docs/content/technical/file-structure.md](docs/content/technical/file-structure.md) for details.

## CLI Reference

```sh
# Basic compilation
tlvxc --emit=x86_64 --out=program.o source.tlvx

# With optimization and CPU targeting
tlvxc --emit=x86_64 --out=program.o \
  --opt=3 --cpu=haswell --features=+avx2,+sse4.2 \
  --opt-pipeline=default source.tlvx

# Available flags
#   --emit=x86_64        Target architecture
#   --out=<file>         Output file
#   --opt=0|1|2|3        Optimization level
#   --cpu=<name>         Target CPU (e.g., haswell)
#   --features=<csv>     CPU features (e.g., +avx2,+sse4.2)
#   --opt-pipeline=minimal|default
```

## Contributing

We welcome contributions! See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

**Focus areas:** compiler development, standard library, IDE, RISC-V support, healthcare use cases.

## License

MIT License. See [LICENSE](LICENSE).

## Community

- **X:** [@TolvexLang](https://twitter.com/TolvexLang)
- **Discord:** [discord.gg/JxE6dD285R](https://discord.gg/JxE6dD285R)
- **GitHub:** [github.com/TolvexLang/tolvex](https://github.com/TolvexLang/tolvex)

---

> *Join us in revolutionizing healthcare analytics with Tolvex!*