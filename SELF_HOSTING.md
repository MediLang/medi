# Self-Hosting Plan for Medi

Last Updated: May 11, 2025 | Repository: [github.com/MediLang/medi](https://github.com/MediLang/medi)

Medi is a programming language purpose-built for healthcare analytics, with the tagline "Empowering Healthcare with Secure, Fast, and Clinician-Friendly Analytics". Currently, Medi’s parser and compiler (`medic`) are being developed in Rust, a safe and performant language ideal for early-stage compiler development. Our long-term goal is to make Medi self-hosted, meaning the `medic` compiler will be written in Medi itself, similar to how Rust transitioned from OCaml to self-hosting by version 1.0 in 2015. This document outlines the feasibility, challenges, and plan to achieve self-hosting for Medi.

## Why Self-Hosting?

Self-hosting offers several benefits for Medi:

* **Maturity**: Demonstrates Medi’s maturity as a language capable of building complex software, boosting confidence in its ecosystem.
* **Simplified Contributions**: Developers can contribute to the compiler using Medi, reducing the need for Rust expertise.
* **Alignment**: Writing the compiler in Medi ensures it benefits from Medi’s own features (e.g., healthcare-specific types, macros).

## Feasibility

Self-hosting is achievable for Medi, following Rust’s precedent:

* **Current State**: Medi’s parser and compiler are written in Rust, providing a stable foundation. The parser uses recursive descent, and the compiler leverages LLVM for codegen, mirroring Rust’s early approach.
* **Rust’s Example**: Rust took ~5 years (2010–2015) to become self-hosted, moving from OCaml to Rust. Medi can aim for a similar timeline after its parser and compiler are functional.
* **Medi’s Advantage**: Rust’s initial implementation simplifies Medi’s early development, allowing us to focus on healthcare features (e.g., privacy checking, FHIR support) before self-hosting.

## Challenges

Achieving self-hosting involves several challenges, similar to Rust’s transition:

* **Syntax Stabilization**: Medi’s syntax (e.g., `fhir_query`, `federated`) must stabilize to avoid breaking changes during the rewrite.
* **Performance**: Early Medi code may be slower than Rust, impacting compiler performance until optimizations mature.
* **Bootstrapping**: Medi must compile itself, requiring a stable subset of the language and a bootstrapping process (using the Rust-written compiler to build the Medi-written compiler).
* **Complexity**: Porting the parser, type checker, privacy/compliance checker, and codegen to Medi is a significant effort.

## Plan to Achieve Self-Hosting

We aim to make Medi self-hosted within 4–5 years, following a phased approach inspired by Rust’s journey.

### Phase 1: Build a Functional Compiler in Rust (0–2 Years)

#### Goal
Complete the Rust-written `medic` compiler with essential features.

#### Tasks
* Finish the recursive descent parser for Medi’s syntax (e.g., `fhir_query`, `federated`).
* Implement type checking for healthcare types (e.g., `FHIRPatient`).
* Add the privacy/compliance checker to enforce HIPAA/GDPR rules.
* Ensure LLVM codegen supports WebAssembly and RISC-V targets.

#### Milestone
Medi 0.1 release, capable of compiling simple healthcare analytics scripts (e.g., the sample script in the README).

### Phase 2: Stabilize Medi’s Syntax and Semantics (2–3 Years)

#### Goal
Finalize Medi’s syntax and core features to minimize breaking changes.

#### Tasks
* Stabilize healthcare constructs (e.g., `fhir_query`, `regulate`, `predict_risk`).
* Define the standard library (`medi.data`, `medi.ai`, `medi.compliance`).
* Gather community feedback via GitHub Discussions and X (@MediLangHQ) to refine the language.

#### Milestone
Medi 0.5 release, with a stable language core suitable for self-hosting preparation.

### Phase 3: Begin Self-Hosting Transition (3–4 Years)

#### Goal
Start rewriting the `medic` compiler in Medi, using the Rust-written compiler to bootstrap.

#### Tasks
* Identify a subset of Medi suitable for compiler development (e.g., basic types, macros, error handling).
* Rewrite the parser in Medi, using recursive descent with Medi’s pattern matching and error handling (`Result`, `Option`).
* Port the type checker and privacy/compliance checker to Medi, leveraging Medi’s healthcare types.
* Reuse the existing LLVM codegen backend, interfacing with it from Medi via FFI.

#### Milestone
Medi 0.8 release, with a partially self-hosted compiler (e.g., parser in Medi, rest in Rust).

### Phase 4: Complete Self-Hosting (4–5 Years)

#### Goal
Fully rewrite `medic` in Medi, achieving self-hosting.

#### Tasks
* Finish porting all compiler components (type checker, privacy/compliance checker, codegen frontend) to Medi.
* Optimize the Medi-written compiler for performance, addressing any bottlenecks.
* Bootstrap the compiler: Use the Rust-written `medic` to compile the Medi-written `medic`, then use the Medi-written `medic` to compile itself.

#### Milestone
Medi 1.0 release, fully self-hosted, marking the language’s maturity.

### Phase 5: Post-Self-Hosting (5+ Years)

#### Goal
Maintain and enhance the self-hosted compiler.

#### Tasks
* Add new features to `medic` using Medi (e.g., advanced optimizations for federated learning).
* Leverage Medi’s healthcare features (e.g., federated macros) to improve the compiler itself.

## Lessons from Rust

* **Iterative Development**: Rust’s early years involved rapid iteration (e.g., removing typestate). Medi will focus on a minimal, stable subset for self-hosting.
* **Community Involvement**: Rust’s RFC process engaged the community in stabilizing the language. Medi will use GitHub Discussions for similar input.
* **Bootstrapping Strategy**: Rust’s gradual transition (parser first, then compiler) provides a blueprint for Medi, ensuring a smooth shift to self-hosting.

## Potential Challenges for Medi

* **Healthcare Complexity**: Features like privacy/compliance checking may be harder to port to Medi. Start with simpler components (e.g., parser).
* **Performance**: Medi’s early performance may lag behind Rust, impacting compiler speed. Optimize critical paths before self-hosting.
* **Learning Curve**: Contributors must learn Medi to work on the compiler, which may slow contributions initially. Provide extensive documentation and examples.

## How to Contribute

Help us achieve self-hosting for Medi! Check out `CONTRIBUTING.md` for guidelines, and share your ideas on GitHub Discussions or X @MediLangHQ.
