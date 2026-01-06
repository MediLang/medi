<div align="center">

<img src="./docs/content/assets/medi-logo.png" alt="Medi Logo" width="200">

# The Medi Programming Language

[Website](http://medi-lang.org) | [Documentation](https://medi-lang.org/docs) | [Contributing](CONTRIBUTING.md) | [Discord](https://discord.gg/JxE6dD285R)

![License](https://img.shields.io/badge/License-MIT-blue) ![Status](https://img.shields.io/badge/Status-Pre--alpha%20(Prototype)-orange) ![CodeRabbit Pull Request Reviews](https://img.shields.io/coderabbit/prs/github/MediLang/medi?utm_source=oss&utm_medium=github&utm_campaign=MediLang%2Fmedi&labelColor=171717&color=FF570A&link=https%3A%2F%2Fcoderabbit.ai&label=CodeRabbit+Reviews) [![codecov](https://codecov.io/gh/MediLang/medi/branch/main/graph/badge.svg)](https://codecov.io/gh/MediLang/medi)

</div>

Medi is a programming language purpose-built for healthcare, designed to transform medical analytics with unparalleled ease, speed, and security. With a beginner-friendly syntax inspired by Python and R, high performance rivaling Julia, Rust, and C++, and native support for healthcare standards like FHIR, HL7, and DICOM, Medi empowers clinicians, researchers, and developers to unlock insights from complex medical data.

From genomic analysis to real-time patient monitoring, clinical trials to hospital operations, Medi delivers secure, scalable, and clinician-friendly solutions.

## Why Medi?

Healthcare demands tools that balance accessibility, performance, security, and compliance. Existing languages fall short:

- **Python/R:** Versatile but slow for big data, lack native healthcare standards, and require complex integrations.
- **SAS/Stata:** Expensive, proprietary, and cumbersome for modern workflows.
- **Julia:** Fast but not healthcare-specific, with a smaller ecosystem.

Medi fills these gaps with:

| Challenge | Medi's Solution |
|-----------|-----------------|
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

```medi
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
git clone https://github.com/MediLang/medi.git
cd medi
cargo build --workspace

# Run tests
cargo test --workspace

# Try an example
cargo run -p medi_data --example clinical_data_exploration
```

## Current Status

| Component | Status |
|-----------|--------|
| Lexer / Parser / AST | ✅ Done |
| Type System (inference, healthcare types, privacy annotations) | ✅ Done |
| LLVM Backend (x86-64, WASM, RISC-V) | ✅ Done |
| Memory Management (GC, borrow checker, real-time zones) | ✅ Done |
| Standard Library (`medi_data`, `medi_stats`, `medi_compliance`, `medi_ai`) | ✅ Done |
| Privacy/Compliance Checker (HIPAA, PHI flow analysis) | ✅ Done |
| Basic IDE (syntax highlighting, code completion) | ✅ Done |
| Example Use Cases | ✅ Done |
| Documentation & Benchmarks | 🔄 In Progress |
| CLI Compiler (`medic`) | ✅ Done |
| REPL | ✅ Done |
| Package Manager (`medipack`) | ✅ Done |
| Python FFI | ✅ Done |

## Project Structure

```
medi/
├── compiler/          # Rust compiler crates (medic_lexer, medic_parser, etc.)
├── stdlib/            # Standard library (medi_data, medi_stats, medi_compliance, medi_ai)
├── tests/             # Integration tests
├── examples/          # Example programs and use cases
└── docs/              # Documentation (MkDocs)
```

See [docs/content/technical/file-structure.md](docs/content/technical/file-structure.md) for details.

## Contributing

We welcome contributions! See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

**Focus areas:** compiler development, standard library, IDE, RISC-V support, healthcare use cases.

## License

MIT License. See [LICENSE](LICENSE).

## Community

- **X:** [@MediLangHQ](https://twitter.com/MediLangHQ)
- **Discord:** [discord.gg/JxE6dD285R](https://discord.gg/JxE6dD285R)
- **GitHub:** [github.com/MediLang/medi](https://github.com/MediLang/medi)

---

> *Join us in revolutionizing healthcare analytics with Medi!*
