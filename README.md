<div align="center">

<img src="./docs/content/assets/medi-logo.png" alt="Medi Logo" width="200">

# The Medi Programming Language

[Website](http://medi-lang.org) | [Documentation](https://medi-lang.org/docs) | [Contributing](CONTRIBUTING.md) | [Discord](https://discord.gg/JxE6dD285R)

![License](https://img.shields.io/badge/License-MIT-blue) ![Status](https://img.shields.io/badge/Status-Pre--alpha%20(Prototype)-orange) ![CodeRabbit Pull Request Reviews](https://img.shields.io/coderabbit/prs/github/MediLang/medi?utm_source=oss&utm_medium=github&utm_campaign=MediLang%2Fmedi&labelColor=171717&color=FF570A&link=https%3A%2F%2Fcoderabbit.ai&label=CodeRabbit+Reviews) [![codecov](https://codecov.io/gh/MediLang/medi/branch/main/graph/badge.svg)](https://codecov.io/gh/MediLang/medi)

</div>

**Medi** is a programming language purpose-built for healthcare—combining Python/R-like syntax, compiled performance (LLVM), and native support for FHIR, HL7, DICOM, and regulatory compliance (HIPAA, GDPR, FDA).

## Why Medi?

| Challenge | Existing Tools | Medi's Solution |
|-----------|---------------|-----------------|
| Performance on big data | Python/R are slow | LLVM-compiled, near-C++ speed |
| Healthcare standards | Manual integrations | Native FHIR, HL7, DICOM, genomics |
| Compliance | Complex, error-prone | Built-in `regulate` blocks, PHI tracking |
| Edge/IoT deployment | Limited options | WebAssembly, RISC-V targets |
| Accessibility | Steep learning curve | Clinician-friendly syntax |

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
| CLI Compiler (`medic`) | 📋 Planned |
| REPL | 📋 Planned |
| Package Manager (`medipack`) | 📋 Planned |
| Python FFI | 📋 Planned |

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
