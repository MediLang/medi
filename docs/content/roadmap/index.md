# Medi Ecosystem & Self-Hosting Roadmap

_Last updated: May 11, 2025_

Medi is a programming language purpose-built for healthcare analytics, with the tagline "Empowering Healthcare with Secure, Fast, and Clinician-Friendly Analytics." This roadmap merges our ecosystem vision and the self-hosting plan, inspired by Rust’s journey to maturity.

---

## 1. Ecosystem Vision & Phased Plan

### Phase 1: Functional Compiler in Rust (0–2 Years)
- **Goal:** Complete the Rust-written `medic` compiler with essential features, including Rust-like borrow checking for memory safety and concurrency.
- **Tasks:**
  - Finish recursive descent parser for Medi’s syntax (`fhir_query`, `federated`)
  - Implement type checking for healthcare types (e.g., `FHIRPatient`)
  - Add privacy/compliance checker (HIPAA/GDPR)
  - Ensure LLVM codegen supports WebAssembly and RISC-V
- **Milestone:** Medi 0.1 release, compiling simple healthcare analytics scripts

### Phase 2: Syntax & Ecosystem Stabilization (2–3 Years)
- **Goal:** Finalize Medi’s syntax and core features, expand the ecosystem
- **Tasks:**
  - Stabilize healthcare constructs (`fhir_query`, `regulate`, `predict_risk`)
  - Define the standard library (`medi.data`, `medi.ai`, `medi.compliance`)
  - Launch `medipack` (package manager) and `medipacks.io` registry
  - Gather community feedback, refine language
- **Milestone:** Medi 0.5 release, stable language core, ecosystem growth

### Phase 3: Begin Self-Hosting Transition (3–4 Years)
- **Goal:** Start rewriting the compiler in Medi, using Rust compiler to bootstrap
- **Tasks:**
  - Identify a stable Medi subset for compiler dev
  - Rewrite parser in Medi, port type checker and privacy checker
  - Interface with LLVM backend via FFI
- **Milestone:** Medi 0.8 release, partially self-hosted compiler

### Phase 4: Complete Self-Hosting (4–5 Years)
- **Goal:** Fully rewrite `medic` in Medi, achieving self-hosting
- **Tasks:**
  - Port all compiler components to Medi
  - Optimize performance
  - Bootstrap: use Rust `medic` to compile Medi `medic`, then self-compile
- **Milestone:** Medi 1.0 release, fully self-hosted

### Phase 5: Post-Self-Hosting (5+ Years)
- **Goal:** Maintain and enhance the self-hosted compiler
- **Tasks:**
  - Add new features to `medic` using Medi (e.g., federated learning optimizations)
  - Leverage Medi’s healthcare features to improve the compiler

---

## 2. Key Ecosystem Components (from ECOSYSTEM_ROADMAP.md)
- **Parser:** Healthcare-specific syntax, clinician-friendly errors, recursive descent (Rust → Medi)
- **Compiler (`medic`):** Privacy/compliance checking, LLVM backend, WebAssembly/RISC-V, self-hosting
- **Package Manager (`medipack`):** Healthcare registry, `Medi.toml`, visual IDE support
- **Privacy/Compliance Checker:** Compile-time enforcement of HIPAA/GDPR/FDA
- **Type System:** Healthcare types (`FHIRPatient`, `DICOMImage`), generics, traits
- **Macro System:** Declarative/procedural macros for healthcare tasks
- **Runtime:** Minimal, with privacy and compliance hooks

---

## 3. Lessons from Rust
- **Iterative Development:** Focus on a minimal, stable subset for self-hosting
- **Community Involvement:** Use GitHub Discussions, RFCs for stabilization
- **Bootstrapping Strategy:** Gradual transition (parser first, then compiler)
- **Borrow Checking:** Adopt Rust-like borrow checking for safety and concurrency in Medi, ensuring memory safety and preventing data races in healthcare analytics.

---

## 4. How to Contribute
Help us achieve self-hosting for Medi! Check out `CONTRIBUTING.md` for guidelines, and share your ideas on GitHub Discussions or X @MediLangHQ.
* Feature complete standard library
* Comprehensive documentation
* Performance benchmarking

### Milestone 7: Production Release (Month 24)
* Stability improvements
* Enterprise deployment options
* Training materials and certifications

### Milestone 8: Ecosystem Growth (Month 36)
* Plugin marketplace
* Integration partners
* Academic and industry adoption

## Getting Involved

We welcome contributions to help accelerate this roadmap:

* Check our [GitHub issues](https://github.com/MediLang/medi/issues) for tasks
* Join our [community forum](https://community.medi-lang.org)
* Follow our [X/Twitter account](https://twitter.com/MediLangHQ) for updates
* Reach out about pilot programs at research institutions

See our [Contributing Guide](../contributing/index.md) for more details.
