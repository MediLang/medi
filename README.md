<div align="center">

<img src="./docs/content/assets/medi-logo.png" alt="Medi Logo" width="200">

# The Medi Programming Language

[Website](http://medi-lang.org) | [Repository](https://github.com/MediLang/medi) | [Contributing](https://github.com/MediLang/medi/blob/main/CONTRIBUTING.md) | [Discord](https://discord.gg/JxE6dD285R)

![License](https://img.shields.io/badge/License-MIT-blue) ![Status](https://img.shields.io/badge/Status-Pre--alpha%20(Prototype)-orange) ![CodeRabbit Pull Request Reviews](https://img.shields.io/coderabbit/prs/github/MediLang/medi?utm_source=oss&utm_medium=github&utm_campaign=MediLang%2Fmedi&labelColor=171717&color=FF570A&link=https%3A%2F%2Fcoderabbit.ai&label=CodeRabbit+Reviews) [![codecov](https://codecov.io/gh/MediLang/medi/branch/main/graph/badge.svg)](https://codecov.io/gh/MediLang/medi)

</div>

Medi is a programming language purpose-built for healthcare, designed to transform medical analytics with unparalleled ease, speed, and security. With a beginner-friendly syntax inspired by Python and R, high performance rivaling Julia, Rust, and C++, and native support for healthcare standards like FHIR, HL7, and DICOM, Medi empowers clinicians, researchers, and developers to unlock insights from complex medical data.

From genomic analysis to real-time patient monitoring, clinical trials to hospital operations, Medi delivers secure, scalable, and clinician-friendly solutions. Its cutting-edge data science and AI capabilities enable predictive diagnostics, personalized medicine, and outbreak tracking, while its open-source ecosystem ensures versatility for emerging domains like telemedicine, medical imaging, and quantum computing.

## Why Medi?

Healthcare demands tools that balance accessibility, performance, security, and compliance. Existing languages fall short:

*   **Python/R:** Versatile but slow for big data, lack native healthcare standards, and require complex integrations.
*   **SAS/Stata:** Expensive, proprietary, and cumbersome for modern workflows.
*   **Julia:** Fast but not healthcare-specific, with a smaller ecosystem.

Medi fills these gaps with a language tailored for healthcare, offering:

*   **Native Healthcare Standards:** Built-in support for FHIR, HL7, DICOM, and genomic formats (FASTQ, VCF).
*   **Privacy-Preserving Analytics:** Federated learning and differential privacy for secure data science and AI.
*   **Real-Time IoT Processing:** Optimized for wearables and ICU devices, enabling instant insights.
*   **Regulatory Automation:** Automated compliance checks and reporting for HIPAA, GDPR, FDA, and EMA.
*   **Clinician-Friendly Syntax:** Intuitive, Python/R-like syntax and a visual IDE for non-programmers.
*   **High Performance:** Compiled to machine code via LLVM, with WebAssembly and RISC-V support for edge devices.
*   **Versatile Applications:** Supports genomics, trials, epidemiology, hospital operations, telemedicine, imaging, R&D, and mental health.

## Key Features

### 1. Beginner-Friendly Syntax

*   Inspired by Python’s readability and R’s data-centric pipelines (e.g., `|>` , similar to R’s `%>%`).
*   Declarative constructs (e.g., `fhir_query`, `plot_kaplan_meier`) simplify complex tasks.
*   Visual IDE with drag-and-drop analytics and natural language queries (e.g., “Show patients with diabetes”).

### 2. High Performance

*   **Compiled to Machine Code:** Uses LLVM for near-C++ performance, optimized for big data tasks like genomic analysis.
*   **WebAssembly Output:** Runs on edge devices (e.g., wearables, portable diagnostics) with low latency, ideal for real-time IoT.
*   **Parallel Processing:** Multi-threading (OpenMP) and distributed computing (MPI/Spark) for scalable analytics.
*   **GPU Support:** CUDA/OpenCL integration for AI-driven diagnostics and imaging (e.g., MRI segmentation).
*   **RISC-V Integration:** Targets RISC-V’s open-source ISA for low-cost, high-performance medical devices, with custom instructions for genomics, privacy, and imaging.
*   **Hybrid Memory Management:** Lightweight garbage collection (like Go) for simplicity, with Rust-inspired manual control (`scope`) for low-latency IoT tasks.

### 3. Medical Data Science and AI

Medi redefines healthcare analytics with powerful, accessible data science and AI tools, transforming patient care and research:

#### Data Science

*   **Advanced Statistical Analysis:** Built-in methods for clinical trials (e.g., `kaplan_meier` for survival curves), epidemiology (e.g., `sir_model` for outbreak modeling), and hospital analytics (e.g., `bed_allocation` for resource optimization).
*   **Big Data Mastery:** Scales effortlessly to massive datasets (e.g., 10TB genomic sequences, EHRs) with parallel processing and GPU acceleration.
*   **Stunning Visualizations:** Interactive plots (e.g., `plot_forest` for meta-analysis, `plot_risk_score` for patient risk) and web-based dashboards, deployable in seconds.
*   **Example:** A clinician can query “Find high-risk heart failure patients” and visualize trends with `plot_risk_score`, no coding required.

#### Artificial Intelligence

*   **Pre-Trained Models:** The `medi.ai` module offers models for diagnostics (e.g., detecting lung cancer from CT scans), risk prediction (e.g., `predict_risk` for heart failure), and NLP (e.g., analyzing mental health narratives).
*   **Federated Learning:** Train AI models across hospitals without sharing data, using `federated` for privacy-preserving analytics.
*   **Real-Time AI:** Low-latency inference on wearables (e.g., detecting arrhythmias in ECG data) with WebAssembly and RISC-V.
*   **Explainable AI:** Transparent outputs ensure clinical trust, critical for diagnostics and trials.
*   **Quantum Readiness:** Early support for quantum algorithms (e.g., Qiskit) for future drug discovery.
*   **Example:** A researcher trains a federated model to predict diabetes risk across global clinics, with results visualized in a dashboard, all in a few lines of Medi code.

#### Why It’s Appealing

*   **Clinician-Empowered:** Non-programmers can run AI models or statistical analyses via the visual IDE, democratizing data science.
*   **Streamlined Workflows:** Native FHIR/DICOM support and pre-built models reduce setup time compared to Python’s complex libraries.
*   **Transformative Impact:** From predicting patient outcomes to accelerating drug discovery, Medi’s AI and data science tools save lives and costs.

### 4. Privacy and Compliance

*   **Privacy-Preserving Analytics:** `federated` and `dp` constructs for federated learning and differential privacy, compliant with HIPAA/GDPR.
*   **Regulatory Automation:** `regulate` for compliance checks and `report` for FDA/EMA submissions, streamlining trial and AI validation.
*   **Security & De-identification:** Hardware-accelerated encryption (e.g., RISC-V crypto extensions) and anonymization helpers in `medi.compliance` such as `mask_phi`, `hash_phi`, `redact_phi`, and tag-based `deidentify_field(value, tag)` for PHI-like strings.

### 4.1 Example: Rust host using medi.compliance

```rust
use medi_compliance::{
    deidentify_field,
    run_hipaa_bundle,
    HipaaKeywordRule,
    RetentionAction,
    RetentionPolicy,
    ConsentRecord,
    ConsentScope,
    ConsentStatus,
    RuleSeverity,
};

fn main() {
    // PHI-like string
    let raw_ssn = "123-45-6789";
    let anonymized = deidentify_field(raw_ssn, "phi.hash");

    // Simple HIPAA keyword rule and bundle invocation
    let rules = vec![HipaaKeywordRule {
        id: "k1".into(),
        description: "Detect SSN".into(),
        keywords: vec!["ssn".into(), "social security".into()],
        severity: RuleSeverity::Error,
    }];

    let consent = ConsentRecord {
        id: "c1".into(),
        subject: "patient-123".into(),
        scopes: vec![ConsentScope {
            resource: "ehr".into(),
            action: "read".into(),
        }],
        status: ConsentStatus::Active,
        expires_at: None,
    };

    let retention_policies = vec![RetentionPolicy {
        min_age_days: 0,
        max_age_days: Some(365),
        action: RetentionAction::Retain,
    }];

    let report = run_hipaa_bundle(
        "Patient SSN: 123-45-6789",
        &rules,
        Some(&consent),
        Some("ehr"),
        Some("read"),
        Some(100),
        &retention_policies,
    );

    println!("HIPAA overall: passed={}", report.overall.failed == 0);
    println!("Anonymized SSN: {}", anonymized);
}
```

This example shows how a Rust host can combine standard library helpers (`deidentify_field`, `run_hipaa_bundle`) with the compiler's privacy checks on sinks and types.

### 5. Versatile Applications

Medi supports a wide range of healthcare domains, with modular modules:

*   **Genomics:** Sequence alignment, variant calling (`call_variants`).
*   **Clinical Trials:** Survival analysis, trial design (`kaplan_meier`, `power_analysis`).
*   **Epidemiology:** Disease modeling, outbreak tracking (`sir_model`).
*   **Hospital Analytics:** Resource optimization, cost prediction (`bed_allocation`).
*   **Telemedicine:** Real-time analytics for remote monitoring.
*   **Personalized Medicine:** Multi-omics integration for tailored treatments.
*   **Medical Imaging:** AI-driven analysis of MRI/CT scans.
*   **Pharmaceutical R&D:** Molecular modeling, adaptive trial design.
*   **Mental Health:** NLP for patient narratives, digital therapeutics.
*   **Future-Ready:** Quantum computing support for drug discovery.

### 6. Interoperability

*   Seamless integration with Python (`py_call`), R (`r_call`), SQL, and healthcare systems (e.g., Epic, Cerner, AWS HealthLake).
*   Compatible with existing tools like pandas, scikit-learn, and survival, easing adoption.

### 7. Rust-Inspired Design

*   Clean, modern syntax with explicit visibility modifiers (`pub` keyword).
*   Module system with a single-file approach for better organization and maintainability.
*   Expression-oriented programming with powerful pattern matching.
*   Uses `.medi` file extension for source files, following industry-standard practices.

## Getting Started

Medi is in pre-alpha, with a prototype under development. To contribute or follow progress:

*   Clone the Repository: `git clone https://github.com/MediLang/medi.git`
*   Join the Community: [X: @MediLangHQ](https://twitter.com/MediLangHQ) | [Discord]([Discord](https://discord.gg/JxE6dD285R))
*   Read the Docs: [medi-lang.org/docs](http://medi-lang.org/docs) (Coming Soon)
*   Contribute: See `CONTRIBUTING.md` for guidelines. Focus areas: medic, standard library, IDE, RISC-V, AI models.

## Build & Test

### Building the Project

Run all builds from the `medi/` directory root:

```sh
cargo build --workspace
```

### Running Tests

Run all tests for all crates:

```sh
cargo test --workspace
```

### Codegen CLI flags (x86-64)

When building with the LLVM backend feature, the `medic` CLI can emit x86-64 ELF object files and provides tuning flags for optimization, CPU, and features.

Basic usage:

```sh
medic --emit=x86_64 --out=program.o path/to/source.medi
```

Available flags:

- `--emit=x86_64`
- `--out=<file>`
- `--opt=0|1|2|3`  (0=None, 1=Less, 2=Default, 3=Aggressive)
- `--cpu=<name>`   (e.g., `haswell`)
- `--features=<csv>` (e.g., `+avx2,+sse4.2`)
- `--opt-pipeline=minimal|default` (selects a pass pipeline profile; default is `minimal`)

Examples:

```sh
# Default optimization/profile, generic x86-64
medic --emit=x86_64 --out=a.o input.medi

# Haswell with AVX2 at -O3 using the default (richer) pipeline
medic --emit=x86_64 --out=a.o \
  --opt=3 --cpu=haswell --features=+avx2,+sse4.2 \
  --opt-pipeline=default input.medi

# No optimization (useful for debugging generated code)
medic --emit=x86_64 --out=a.o --opt=0 input.medi
```

## Performance and RISC-V
Medi is engineered for high performance, critical for healthcare’s big data and real-time needs:

*   **LLVM Compilation:** Optimizes code to machine code, rivaling Julia/Rust for tasks like genomic alignment or AI inference.
*   **WebAssembly:** Enables low-latency analytics on edge devices, addressing Python’s latency issues for IoT.
*   **Parallel/GPU Processing:** Accelerates data science (e.g., epidemiology simulations) and AI (e.g., imaging analysis) with multi-threading and CUDA/OpenCL.
*   **Hybrid Memory Management:** Combines a low-pause garbage collector for simplicity with manual control (`scope`) for predictable IoT performance.

## Memory Management

Medi uses a hybrid approach to balance ease-of-use with predictable latency:

- **Generational GC (default):** Fast minor collections over a nursery with a remembered set to keep young pauses low (target p99 < 10ms in typical configurations).
- **Incremental Major GC (feature-gated):** Under the `gc-incremental` feature, Medi prototypes a tri-color incremental major collector with chunked marking/sweeping. Step budgets respect `GcParams.max_pause_ms` to keep pauses bounded, while end-to-end completion runs across many small steps.
- **Manual control for IoT:** For ultra-low-latency paths, use `scope` and other manual techniques (roadmap) alongside the GC.

### Tuning & Telemetry

- Tunables via `GcParams` (and environment variables set by the compiler/runtime):
  - `nursery_threshold_bytes` (nursery size)
  - `gen_promotion_threshold` (promotion cadence)
  - `max_pause_ms` (incremental step budget)
- Optional telemetry: set `MEDI_GC_LOG=1` to print `gc_minor_pause_ms` and `gc_major_pause_ms` during runs.
 - Runtime/env knobs:
   - `MEDI_GC_STEP_SCALE` (default: `3`). Higher values tighten per-step work caps in incremental major GC to reduce p99 step latency without code changes.
   - `MEDI_GC_NURSERY_BYTES` is read by benches to override nursery size at runtime for A/B experiments (see `compiler/medic_runtime/benches/gc_benches.rs`).

### Benchmarks

Criterion benches are available in `compiler/medic_runtime/benches/gc_benches.rs`:

```bash
cargo bench -p medic_runtime
# With incremental prototype enabled
cargo bench -p medic_runtime --features gc-incremental
```

Open HTML reports for p50/p99:

- `target/criterion/minor_gc_pause/alloc_small_strings_minor_collect/report/index.html`
- `target/criterion/major_gc_pause/alloc_many_then_major_collect/report/index.html`
- `target/criterion/throughput/alloc_mutate_with_and_without_minor/report/index.html`
- When `gc-incremental` is enabled: `target/criterion/incremental_major/step_latency_and_total_completion/report/index.html`

## RISC-V Integration

Medi leverages RISC-V, an open-source instruction set architecture, to enhance performance and accessibility:

#### Why RISC-V?:

*   **Cost-Effective:** Royalty-free ISA reduces costs for medical devices, supporting global health equity.
*   **Edge Computing:** Low-power RISC-V cores (e.g., SiFive E-series) are ideal for wearables and portable diagnostics.
*   **Custom Extensions:** Healthcare-specific instructions (e.g., `genomic_align`, `encrypt`) for genomics, privacy, and imaging.
*   **Scalability:** Supports microcontrollers to servers, covering IoT to hospital clusters.

#### Implementation:

*   LLVM medic targets RISC-V (RV32/RV64) with vector (RVV) and crypto extensions.
*   WebAssembly runs on RISC-V-based devices, optimized for low power.
*   Modules like `medi.genomics` and `medi.privacy` use RISC-V vector/crypto instructions.

#### Benefits:

*   Accelerates genomic analysis and real-time IoT on low-cost hardware.
*   Enhances security with hardware-accelerated encryption.
*   Future-proofs Medi as RISC-V adoption grows in healthcare.

## Example: Medi Script

Below is a sample Medi script showcasing data science, AI, and compliance in a multi-domain scenario: real-time IoT monitoring, genomic analysis, AI-driven risk prediction, and FDA reporting, optimized for RISC-V.

```mdi
// Configure RISC-V target for edge device (e.g., wearable)
target riscv {
  architecture: "RV32IMAFDC", // RISC-V 32-bit with vector extensions
  optimizations: ["vectorize", "low_power"]
};
{{ ... }}
// Validate compliance
regulate {
  standard: "FDA_21CFR11",
  checks: ["audit_trail", "data_integrity", "electronic_signatures"]
};
```

## Highlights

*   **Data Science:** Statistical analysis (`kaplan_meier`) and visualization (`plot_risk_score`) for trial insights.
*   **AI:** Predictive modeling (`predict_risk`) with federated learning for privacy.
*   **Performance:** `parallel`, `scope`, and RISC-V optimizations ensure speed and low latency.
*   **Healthcare-Specific:** Native FHIR/VCF, privacy, and compliance automation.
*   **Beginner-Friendly:** Intuitive syntax for clinicians.

## Community Sentiment

Discussions on X (as of May 2025) validate Medi’s relevance:

*   **Data Science/AI:** Demand for federated learning and clinician-accessible AI tools supports Medi’s `federated` and `medi.ai` features.
*   **Real-Time IoT:** Python’s latency issues for wearables align with Medi’s `stream` and RISC-V optimizations.
*   **Compliance:** Frustration with SAS’s manual reporting endorses Medi’s `regulate` and `report`.
*   **Privacy:** Need for secure genomics/EHR analytics supports Medi’s differential privacy.

## Audience & Personas

* **Clinician/Researcher**
  - Goal: Ask clinical questions, run stats/AI without deep programming.
  - Needs: FHIR-native data access, clear diagnostics, units/terminology help.
  - Value: Short, readable code; built-ins for trials/epi; privacy by default.

* **Data Scientist**
  - Goal: Analyze multi-modal healthcare data at scale.
  - Needs: Interop with Python/R, parallelism, reproducibility, compliance.
  - Value: Domain types (FHIR/DICOM), federated learning, UCUM normalization.

* **Health IT Engineer**
  - Goal: Integrate EHRs, imaging, devices, identity/security.
  - Needs: FHIR/HL7/DICOM adapters, SMART on FHIR, audit, deploy to edge.
  - Value: First-class standards, WASM/RISC-V targets, compliance hooks.

## Value Proposition vs Existing Languages

- **Python**: Massive ecosystem but slower and lacks native healthcare models.
  - Medi: LLVM-native performance, first-class FHIR/DICOM/terminologies, `regulate` for compliance; still interoperates via `py_call`.

- **Julia**: High performance, growing DS/ML ecosystem; not healthcare-specific.
  - Medi: Similar perf target with domain primitives and compliance baked in.

- **SAS/Stata**: Trusted in clinical settings; proprietary, expensive, slower iteration.
  - Medi: Open-source, modern tooling, automation of regulatory artifacts, edge/WASM targets.

## Getting Started

Medi is in pre-alpha, with a prototype under development. To contribute or follow progress:

*   Clone the Repository: `git clone https://github.com/MediLang/medi.git`
*   Join the Community: [X: @MediLangHQ](https://twitter.com/MediLangHQ) | [Discord]([Discord](https://discord.gg/JxE6dD285R))
*   Read the Docs: [medi-lang.org/docs](http://medi-lang.org/docs) (Coming Soon)
*   Contribute: See `CONTRIBUTING.md` for guidelines. Focus areas: medic, standard library, IDE, RISC-V, AI models.

## Current Status

| Area | Status |
| --- | --- |
| [Lexer/Parser/AST](.taskmaster/tasks/tasks.json) (Task 1) | Supported (prototype) |
| [Clinician-friendly diagnostics](.taskmaster/tasks/tasks.json) (Task 1, subtask 5) | In progress |
| [Type system core/inference](.taskmaster/tasks/tasks.json) (Task 2) | In progress |
| [LLVM backend (x86-64/WASM/RISC-V)](.taskmaster/tasks/tasks.json) (Task 3) | In progress |
| [Runtime/memory zones](.taskmaster/tasks/tasks.json) (Task 4) | In progress |
| [Standard library (data/stats/compliance/ai)](.taskmaster/tasks/tasks.json) (Task 5) | Planned/Prototype |
| [FHIR interop (REST, Search, Bulk)](.taskmaster/tasks/tasks.json) (Task 5: medi.data) | Planned |
| [DICOM/DICOMweb](.taskmaster/tasks/tasks.json) (Task 5: medi.data) | Planned |
| [Terminologies (SNOMED, LOINC, ICD, RxNorm, UCUM, CPT/HCPCS, NDC)](.taskmaster/tasks/tasks.json) (Task 5: medi.data) | Planned |
| [CLI/REPL](.taskmaster/tasks/tasks.json) (Task 7) | Planned |
| [Compliance checker (HIPAA/GDPR/Part 11)](.taskmaster/tasks/tasks.json) (Task 6) | Planned/Prototype (HIPAA bundle helpers) |

Legend: Supported = usable prototype; In progress = partial implementation; Planned = on roadmap.

## Development Roadmap

### Phase 1: Prototype (6-12 Months)

*   Build parser and LLVM-based `medic` compiler for core syntax using Rust-inspired approach.
*   Implement modules: `medi.data`, `medi.compliance`, `medi.stats`, `medi.ai`.
*   Develop a basic IDE with visual analytics and .medi file recognition.
*   Test with synthetic data (FHIR, VCF, IoT streams).
*   Target RISC-V (RV32) for IoT prototype.

### Phase 2: Pilot (12-18 Months)

*   Pilot with universities/hospitals on trials, IoT, or AI use cases.
*   Expand library (`medi.iot`, `medi.viz`, `medi.privacy`) and RISC-V support (RV64, vector extensions).
*   Share on X/GitHub for feedback.

### Phase 3: Production Release (18-36 Months)

*   Complete library (`medi.ai`, `medi.ops`) and plugin marketplace.
*   Optimize for big data, edge devices, and quantum computing.
*   Launch training programs and certifications.
*   Integrate with Epic, Cerner, and cloud platforms.

## Technical Architecture

*   **Compiler:** LLVM-based, targeting x86, ARM, RISC-V, and WebAssembly. Supports JIT for development, AOT for deployment.
*   **Runtime:** Lightweight, with MHE/DP libraries for privacy and WASM compatibility for edge devices.
*   **Memory Management:** Hybrid GC (low-pause, like Go) with manual control (`scope`, like Rust) for IoT.
*   **Standard Library:**
    *   `medi.data`: FHIR, HL7, DICOM, VCF.
    *   `medi.privacy`: Federated learning, differential privacy.
    *   `medi.iot`: Real-time streaming.
    *   `medi.stats`: Trial and epidemiology analytics.
    *   `medi.viz`: Interactive visualizations.
    *   `medi.compliance`: Regulatory automation.
    *   `medi.ai`: AI models for diagnostics, NLP, prediction.
    *   `medi.ops`: Hospital operations.

### Architecture Overview

```
Source (.medi)
  -> Lexer (`medic_lexer`)
  -> Parser / AST (`medic_parser`, `medic_ast`)
  -> Type System (`medic_type`) & Type Checker (`medic_typeck`)
  -> IR / Codegen (planned: `medic_codegen_llvm`)
  -> Binaries/WASM (planned) + Runtime (planned)
  -> CLI/REPL (planned: `medic`)
```

- Crates map:
  - `compiler/medic_lexer`: Tokenization
  - `compiler/medic_parser`: Parsing -> AST
  - `compiler/medic_ast`: AST definitions/utilities
  - `compiler/medic_type`: Types and inference primitives
  - `compiler/medic_typeck`: Type checking passes
  - Planned: `compiler/medic_codegen_llvm`, `compiler/medic_runtime`, CLI integration in `compiler/medic`

## Standards & Interoperability

Medi integrates with clinical data standards, terminologies, and protocols. Status tags: [Supported] [In progress] [Planned]. Current status: Planned unless otherwise noted.

- Data exchange & models
  - HL7 FHIR R4/R5 (REST, Search, Subscriptions, Bulk Data/Flat FHIR) [Planned] ([tracking: Task 5: medi.data](.taskmaster/tasks/tasks.json))
  - HL7 v2.x (ADT/ORM/ORU) [Planned] ([tracking: Task 5: medi.data](.taskmaster/tasks/tasks.json))
  - HL7 CDA [Planned] ([tracking: Task 5: medi.data](.taskmaster/tasks/tasks.json))
  - IHE profiles: XDS/XCA/MHD/PDQ/PIX/ATNA [Planned] ([tracking: Task 5: medi.data](.taskmaster/tasks/tasks.json))
  - openEHR (EHRbase, Archetypes/Templates) [Planned] ([tracking: Task 5: medi.data](.taskmaster/tasks/tasks.json))
  - OMOP CDM / OHDSI [Planned] ([tracking: Task 5](.taskmaster/tasks/tasks.json))
  - PCORnet CDM [Planned] ([tracking: Task 5](.taskmaster/tasks/tasks.json))
  - X12 HIPAA (270/271/837/835) [Planned] ([tracking: Task 5](.taskmaster/tasks/tasks.json))
  - NCPDP SCRIPT (eRx) [Planned] ([tracking: Task 5](.taskmaster/tasks/tasks.json))
  - CDISC SDTM/ADaM (clinical trials) [Planned] ([tracking: Task 5](.taskmaster/tasks/tasks.json))

- Imaging
  - DICOM (files, SR) [Planned] ([tracking: Task 5: medi.data](.taskmaster/tasks/tasks.json))
  - DICOMweb (QIDO-RS, WADO-RS, STOW-RS) [Planned] ([tracking: Task 5: medi.data](.taskmaster/tasks/tasks.json))

- Genomics
  - GA4GH: htsget, refget, Beacon v2, VRS, Phenopackets [Planned] ([tracking: Task 5](.taskmaster/tasks/tasks.json))
  - Formats: FASTQ, BAM/CRAM, VCF/BCF [Planned] ([tracking: Task 5](.taskmaster/tasks/tasks.json))

- Terminologies & units
  - SNOMED CT [Planned] (License required; see note) ([tracking: Task 5: medi.data](.taskmaster/tasks/tasks.json))
  - LOINC [Planned] ([tracking: Task 5: medi.data](.taskmaster/tasks/tasks.json))
  - ICD-10/ICD-11 [Planned] ([tracking: Task 5: medi.data](.taskmaster/tasks/tasks.json))
  - RxNorm, ATC, NDC/EMA [Planned] ([tracking: Task 5: medi.data](.taskmaster/tasks/tasks.json))
  - CPT/HCPCS [Planned] ([tracking: Task 5: medi.data](.taskmaster/tasks/tasks.json))
  - UCUM (units) [Planned] ([tracking: Task 5: medi.data](.taskmaster/tasks/tasks.json))

- Security, identity, and app integration
  - SMART on FHIR (OAuth2/OIDC, SMART scopes) [Planned] ([tracking: Task 6](.taskmaster/tasks/tasks.json))
  - mTLS/JWT/JOSE [Planned] ([tracking: Task 6](.taskmaster/tasks/tasks.json))
  - FHIR CDS Hooks [Planned] ([tracking: Task 6](.taskmaster/tasks/tasks.json))
  - Audit: IHE ATNA, FHIR AuditEvent [Planned] ([tracking: Task 6](.taskmaster/tasks/tasks.json))
  - Consent & provenance: FHIR Consent, W3C PROV [Planned] ([tracking: Task 6](.taskmaster/tasks/tasks.json))

- Public health & reporting
  - eCR Now / ELR [Planned] ([tracking: Task 6](.taskmaster/tasks/tasks.json))
  - FDA/EMA reporting templates (21 CFR Part 11 alignment) [Planned] ([tracking: Task 6](.taskmaster/tasks/tasks.json))

Licensing note: Some terminologies (e.g., SNOMED CT) require a license in certain jurisdictions. Medi will integrate via pluggable terminology services and will not redistribute proprietary content.

## Diagnostics and Error Experience

Clinician-friendly diagnostics aim to provide clear messages, code frames, and actionable suggestions.

Example (illustrative):

```
error[E1002]: Type mismatch: expected Number, found String
 --> patient_risk.medi:12:17
  |
12|   let bmi = weight / "height";
  |                  ^^^^^^^^^^^ expected Number
  |
help: did you mean to use the numeric value?
      try: height.value  // or normalize units via ucum("180 cm")
note: future versions will suggest unit conversions and terminology lookups.
```

## Compliance & Safety

Planned compliance-by-design features and security posture:

- Regulatory scope: HIPAA, GDPR, 21 CFR Part 11; relevant ISO standards (ISO 13485, ISO 14971) [Planned]
- Controls: `regulate { ... }` blocks for compile-time checks, audit artifacts, and reporting [Planned]
- Data governance: PHI tagging, data minimization, consent/provenance (FHIR Consent, W3C PROV) [Planned]
- Security: OAuth2/OIDC (SMART on FHIR), mTLS, JWT, key management (KMS/HSM), secrets management [Planned]
- Observability: structured logs, FHIR AuditEvent/IHE ATNA alignment [Planned]

Disclaimer: This is not legal advice. Compliance features are under active design and implementation.

## Project Structure (Rust-Inspired)

This repository follows a Rust-style workspace structure:

- **Cargo.toml (workspace, in medi/)**: Defines the workspace and its member crates.
- **compiler/**: All Rust source files for the Medic compiler (not in a src/ subdirectory).
- **library/**: (Optional) Standard library crate(s) for Medic.
- **src/**: Reserved for future Medi language source code. Do not use for the compiler.
- **tests/**: Integration and system tests for the compiler and library.

### Building the Project

Run all builds from the medi/ directory root:
```sh
cargo build --workspace
```

### Running Tests

Run all tests for all crates:
```sh
cargo test --workspace
```

### Codegen CLI flags (x86-64)

When building with the LLVM backend feature, the `medic` CLI can emit x86-64 ELF object files and provides tuning flags for optimization, CPU, and features.

Basic usage:

```sh
medic --emit=x86_64 --out=program.o path/to/source.medi
```

Available flags:

- `--emit=x86_64`
- `--out=<file>`
- `--opt=0|1|2|3`  (0=None, 1=Less, 2=Default, 3=Aggressive)
- `--cpu=<name>`   (e.g., `haswell`)
- `--features=<csv>` (e.g., `+avx2,+sse4.2`)
- `--opt-pipeline=minimal|default` (selects a pass pipeline profile; default is `minimal`)

Examples:

```sh
# Default optimization/profile, generic x86-64
medic --emit=x86_64 --out=a.o input.medi

# Haswell with AVX2 at -O3 using the default (richer) pipeline
medic --emit=x86_64 --out=a.o \
  --opt=3 --cpu=haswell --features=+avx2,+sse4.2 \
  --opt-pipeline=default input.medi

# No optimization (useful for debugging generated code)
medic --emit=x86_64 --out=a.o --opt=0 input.medi
```

Notes:

- Register allocation is handled by LLVM’s x86 backend. The `--opt`, `--cpu`, and `--features` flags influence the pass pipeline and instruction selection, indirectly affecting RA.
- Object emission is performed by the x86-64 `TargetMachine`; the module’s data layout is configured from the target for correct alignment.

### Directory Purposes
- `compiler/`: Rust crate for the Medic compiler (`medic`). All Rust source files go directly here.
- `library/`: Rust crate(s) for the Medic standard library.
- `src/`: Reserved for future Medi language source code (not used for compiler).
- `tests/`: Integration/system tests (Rust or other frameworks).

This structure closely follows the Rust project layout inspiration.

## Developer Notes

### Quantity IR (experimental, dev-only)

Medi supports an experimental, feature-flagged Quantity IR that represents quantities as a concrete LLVM struct `%Quantity = { double value, i32 unit_id }`. This is disabled by default and intended for development and experiments with unit-aware codegen.

Enable and test (requires LLVM 15.x available and backend enabled):

```bash
# Build the backend crate with Quantity IR enabled
cargo build -p medic_codegen_llvm --features "llvm,quantity_ir"

# Or run feature-guarded tests that exercise the Quantity IR paths
cargo test -p tests -p medic_codegen_llvm --features "llvm,quantity_ir" -- --nocapture
```

Current semantics under `quantity_ir`:

- Quantities lower to `%Quantity` with per-module interned unit ids.
- Known unit conversions lower to a multiply on the value field and update the unit id.
- Unknown conversions call a runtime shim `medi_convert_q(%Quantity, i32 to_unit_id)`.
- Arithmetic is conservative for now:
  - Add/Sub require both operands to have the same unit (same unit id). Otherwise, convert explicitly first using `->`.
  - Mul/Div and comparisons on quantities are currently disallowed until dimensional analysis is introduced.

When `quantity_ir` is disabled (default), quantities lower to plain `f64` and unit metadata is tracked in side tables for type checking and diagnostics. Known conversions compile to `fmul` and unknown pairs call `medi_convert(double, i32, i32)`.

## Contributing

Medi is open-source under the MIT license. We welcome contributions in:

*   Compiler development (LLVM, RISC-V).
*   Standard library modules (AI, genomics, IoT).
*   IDE enhancements (visual analytics, natural language queries).
*   Healthcare use case testing (trials, IoT, imaging, AI).

See `CONTRIBUTING.md` for details.

## License

MIT License. See `LICENSE` for details.

## Contact

<!-- *   **Email:** [contact@medi-lang.org](mailto:contact@medi-lang.org) -->
*   **X:** [@MediLangHQ](https://twitter.com/MediLangHQ)
*   **GitHub:** [github.com/MediLang/medi](https://github.com/MediLang/medi)
*   **Discord** [Discord](https://discord.gg/JxE6dD285R)

> *Join us in revolutionizing healthcare analytics with Medi!*
