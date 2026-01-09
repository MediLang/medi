# Medi Programming Language Specification

## Overview

Medi is a high-level, domain-specific programming language purpose-built for healthcare, designed to transform medical analytics with unparalleled ease, speed, and security. With a beginner-friendly syntax inspired by Python and R, high performance rivaling Julia, Rust, and C++, and native support for healthcare standards like FHIR, HL7, and DICOM, Medi empowers clinicians, researchers, and developers to unlock insights from complex medical data.

This specification outlines Medi's syntax, data types, control structures, and domain-specific constructs as of version 0.1.0, with a focus on its unique features for healthcare applications, including federated learning, privacy-preserving analytics, and real-time IoT processing.

## Design Principles

- **Simplicity**: Intuitive syntax for developers with minimal programming experience, including clinicians
- **Safety**: Strong typing, privacy-preserving features, and automated compliance checks
- **Performance**: High-performance compilation via LLVM with WebAssembly and RISC-V support
- **Interoperability**: Native support for FHIR, HL7, DICOM, and genomic formats
- **AI & Analytics**: Built-in support for federated learning and medical data science
- **Real-time Processing**: Optimized for medical IoT devices and wearables

## Lexical Structure

### Source Code Representation
- Source files are encoded in UTF-8
- Line endings are platform-independent (\n or \r\n)
- Blocks are brace-delimited; indentation is not significant

### Tokens
1. **Identifiers**:
   - Start with letter or underscore
   - Followed by letters, digits, underscores
   - Case-sensitive
   - Cannot use reserved keywords

2. **Keywords**:
   ```
   module    import    fn       let      const   type
   struct    enum      trait    impl     pub     priv
   return    while     for      in       match   if
   else      loop      break    continue true    false
   nil       fhir_query regulate federated scope real_time
   ```

3. **Operators and Delimiters**:
   ```
   +    -    *    /    %    =    ==   !=   <    >    <=   >=
   +=   -=   *=   /=   %=   &&   ||   |    !    ->   =>   {    }    (    )    [    ]
   ??   ?:   →    of   per  .    ::   ,    :    ;    @    #    ?    ..   ..=  ...
   ```

4. **Literals**:
   - Integers: `42`, `0xFF`, `0b1010`
   - Floats: `3.14`, `1.0e-10`
   - Strings: `"hello"`, `"""multiline"""`
   - Boolean: `true`, `false`
   - DateTime: `2025-05-15T00:00:00Z`
   - Medical: `pid("PT123")`, `icd10("A00.0")`, `snomed("123456")`, `loinc("12345-6")`, `cpt("99213")`

### Lexer Error Tokens

- When the lexer encounters an invalid or unrecognized lexeme, it does not abort; instead it emits an error token.
- Form: `TokenType::Error("Invalid token '<lexeme>'")` with an attached `Location` (line, column, offset).
- This behavior is unified across both the streaming and chunked lexers via centralized conversion logic (`convert_logos_to_token`), ensuring consistent messaging regardless of lexing mode.
- Debug logging: When the optional `logging` feature is enabled, a debug log is emitted at the point of error token creation to aid diagnostics. Logs include the source location and offending lexeme (and for chunked lexing, whether the chunk is final). Enable at runtime with `RUST_LOG=debug` and a logger (e.g., `env_logger`).

### Comments
```medi
// Line comments
/* Block comments that can
   span multiple lines */
/// Documentation comments
```

## Syntax Structure

### Hello World Example

```medi
module HelloWorld

print("Welcome to Medi Programming Language!")
```

### Comments

```medi
// Single-line comment
/* Multi-line
   comment */
```

## Data Types

Medi supports a rich set of primitive and composite data types, with a focus on medical data representation.

### Primitive Types

- `int`: 64-bit integer (e.g., `42`)
- `float`: 64-bit floating-point number (e.g., `98.6`)
- `bool`: Boolean values (`true`, `false`)
- `string`: UTF-8 encoded text (e.g., `"Patient ID: 123"`)
- `datetime`: ISO 8601 date and time (e.g., `2025-05-15T12:11:00+08:00`)

### Medical-Specific Types

- `patient_id`: Unique identifier for patients, compliant with medical standards (e.g., `pid("PT-12345")`)
- `vital`: Record for vital signs (e.g., `vital(temperature: 37.2, pulse: 80)`)
- `lab_result`: Structured lab data (e.g., `lab_result(name: "CRP", value: 10.5, unit: "mg/L")`)

### Composite Types

- `list[T]`: Ordered collection (e.g., `list[int] = [1, 2, 3]`)
- `map[K, V]`: Key-value pairs (e.g., `map[string, float] = {"temp": 37.2, "pulse": 80}`)
- `record`: Structured data with named fields (e.g., `record Patient { name: string, age: int }`)

## Control Structures

### Conditionals

```medi
if temperature > 38.0 {
    print("Fever detected")
} else {
    print("Temperature normal")
}
```

### Loops

```medi
for reading in vitals {
    print("Pulse: ", reading.pulse)
}

while heart_rate > 100 {
    alert("Tachycardia detected")
}
```

### Pattern Matching

```medi
match lab_result {
    case lab_result(name: "CRP", value: v) if v > 10 => print("Elevated CRP")
    case _ => print("Normal result")
}
```

#### Notes on '|' and pipeline operator

- '|' is tokenized as `TokenType::BitOr` in all contexts. The parser disambiguates it as alternation inside patterns.
- There is no pipeline operator `|>` in v0.1 at the language level; it is reserved (see "Reserved & Future").
- The lexer can optionally tokenize `|>` as a single token behind a feature flag (`pipeline_op`). When the flag is disabled (default), `|>` tokenizes as `|` (BitOr) followed by `>` (Greater).

## Functions

Functions are defined with the `fn` keyword and support type annotations.

```medi
fn calculate_bmi(weight: float, height: float) -> float {
    return weight / (height * height)
}

let bmi = calculate_bmi(70.0, 1.75)
print("BMI: ", bmi)
```

### Domain-Specific Functions

- `fhir_query(resource: string, id: string, filter?: map[string, string]) -> record | list[record]`: Fetches FHIR-compliant medical data.
- `validate_vital(v: vital) -> bool`: Checks if vital signs are within normal ranges.

## Error Handling

v0.1 uses `Result<T, E>` and the `?` operator for error propagation. `try/catch` is planned for a future version.

```medi
fn load_patient(id: string) -> Result<record, Error> {
    let patient = fhir_query("Patient", id)? // propagate any fetch error
    Ok(patient)
}
```

## Modules and Imports

```medi
module ClinicalAnalytics

import DataProcessing

fn analyze_vitals(vitals: list[vital]) -> map[string, float] {
    // Analysis logic
}
```

## Domain-Specific Features

### Medical Data Integration

Medi provides built-in support for medical data standards:

```medi
let patient = fhir_query("Patient", "PT-12345")
let labs = fhir_query("Observation", "LAB-67890")
```

### Clinical Rules

Define rules for clinical decision support:

```medi
rule HighRiskSepsis {
    if vital.pulse > 100 and lab_result.value("CRP") > 10 {
        alert("High sepsis risk")
    }
}
```

## Standard Library

### Core Library (`medi::core`)
- Basic types and traits
- Collections and iterators
- Error handling
- Concurrency primitives
- I/O operations

### Healthcare Standards (`medi::standards`)
- `medi::standards::fhir` - FHIR data models and queries
- `medi::standards::hl7` - HL7 message handling
- `medi::standards::dicom` - DICOM image processing
- `medi::standards::icd` - ICD-10 and SNOMED CT

### Data Science (`medi::science`)
- `medi::science::stats` - Statistical analysis
- `medi::science::ml` - Machine learning
- `medi::science::genomics` - Genomic processing
- `medi::science::imaging` - Medical imaging

### Privacy and Security (`medi::privacy`)
- `medi::privacy::hipaa` - HIPAA compliance (integrates with helpers in `medi.compliance` for de-identification and keyword/bundle checks)
- `medi::privacy::gdpr` - GDPR requirements
- `medi::privacy::federated` - Federated learning
- `medi::privacy::crypto` - Medical-grade encryption

### Real-time Processing (`medi::rt`)
- `medi::rt::device` - Medical device interfaces
- `medi::rt::monitor` - Patient monitoring
- `medi::rt::alert` - Clinical alerting
- `medi::rt::stream` - Data streaming

## Example: Clinical Data Processing

```medi
module SepsisMonitor

import VitalSigns
import LabResults

fn monitor_patient(id: patient_id) -> bool {
    let vitals = query_vitals(id)
    let labs = fhir_query("Observation", id.value)

    for vital in vitals {
        if vital.temperature > 38.0 or vital.pulse > 100 {
            let crp = labs.find(lab => lab.name == "CRP")
            if crp.value > 10 {
                alert("Potential sepsis risk")
                return true
            }
        }
    }
    return false
}
```

## Grammar (Simplified BNF)

```ebnf
Program         ::= ModuleDecl? UseDecl* Declaration*
ModuleDecl      ::= 'module' QualifiedIdent
UseDecl         ::= 'import' ImportSpec
Declaration     ::= FunctionDecl | TypeDecl | ConstDecl | VarDecl

FunctionDecl    ::= 'fn' Identifier GenericParams? '(' Parameters? ')' ('->' Type)? Block
Parameters      ::= Parameter (',' Parameter)*
Parameter       ::= Pattern ':' Type

TypeDecl        ::= 'type' Identifier GenericParams? '=' Type
Type           ::= BasicType | UserType | GenericType | FunctionType
BasicType      ::= 'int' | 'float' | 'bool' | 'string' | 'datetime'
                 | 'patient_id' | 'vital' | 'lab_result'

Statement      ::= ExprStmt | LetStmt | ConstStmt | ReturnStmt
                 | IfStmt | WhileStmt | ForStmt | MatchStmt

Expression     ::= Literal | Identifier | CallExpr | BinaryExpr
                 | UnaryExpr | BlockExpr | IfExpr | MatchExpr

// Let statements
LetStmt        ::= 'let' Identifier (':' Type)? ('=' Expression)? ';'? 
```

### Healthcare-Specific Syntax
```ebnf
FHIRQuery      ::= 'fhir_query' '(' STRING (',' QueryParam)* ')'
QueryParam     ::= Identifier ':' Expression

FederatedBlock ::= 'federated' '(' STRING ')' Block

RegulateBlock  ::= 'regulate' '(' ComplianceSpec ')' Block
ComplianceSpec ::= 'hipaa' | 'gdpr' | 'fda' | STRING
```

## Language Integration Considerations

### Memory Management Integration

1. **Ownership Zones**
   - `safe` zone: Pure GC, no manual memory management
   - `real_time` zone: Manual memory management via `scope`
   - Cannot mix zones without explicit boundaries

2. **Type System Boundaries**
   ```medi
   // Safe zone - GC managed
   fn process_patient(p: Patient) -> Analysis {
       // Automatic memory management
   }

   // Real-time zone - Manual management
   real_time fn monitor_vitals(v: &Vitals) -> Alert {
       scope {
           // Manual memory management
       }
   }
   ```

### Syntax Resolution

1. **Whitespace Rules**
   - Blocks are brace-delimited in all zones
   - Indentation is not significant (recommended for readability)
   - Visual distinction between modes via annotations/keywords, not whitespace

2. **Healthcare Integration**
   - Medical types are first-class citizens
   - Standard types can be extended with medical traits
   - Clear syntax for medical operations:
     ```medi
     // Medical-specific syntax
     let bp = vital::blood_pressure(120, 80)
     
     // Standard programming syntax
     let avg = values.iter().sum() / values.len()
     ```

### Type System Clarity

1. **Medical Type Hierarchy**
   ```medi
   trait MedicalRecord {}
   trait PrivacyProtected {}
   
   struct Patient implements MedicalRecord, PrivacyProtected {
       // Patient data
   }
   ```

2. **Context-Aware Types**
   - Types know their privacy level
   - Automatic HIPAA/GDPR compliance
   - Clear error messages for violations

### Best Practices

1. **When to Use Each Feature**:
   - Use GC by default for application logic
   - Use manual memory management only for real-time requirements
   - Use medical types for healthcare data
   - Use standard types for general computation

2. **Integration Patterns**:
   - Keep medical and general code separate
   - Use clear boundaries between memory management zones
   - Follow the principle of least privilege for medical data

The grammar is inspired by multiple languages but maintains consistency through clear boundaries and rules:

## Execution Model

### Memory Management

1. **Ownership and Borrowing**
   - Values have a single owner
   - References can be borrowed as:
     - One mutable reference
     - Multiple immutable references
   - Lifetimes are tracked by the compiler

2. **Garbage Collection**
   - Hybrid approach combining:
     - Reference counting for deterministic cleanup
     - Generational GC for cycle collection
     - Manual memory regions via `scope` blocks

### Concurrency Model

1. **Task-based Parallelism**
   - Lightweight tasks scheduled by runtime
   - Channel-based message passing
   - Async/await for non-blocking I/O (deferred in v0.1)

2. **Data Race Prevention**
   - Static analysis via borrow checker
   - Thread-safe containers in standard library
   - Atomic types for lock-free operations

3. **Healthcare-Specific Features**
   - Federated computation isolation
   - Privacy boundary enforcement
   - Real-time guarantees for medical devices

### Runtime Features & Feature Flags

Medi provides a host-side runtime crate at `compiler/medic_runtime/` offering task-based parallelism and channel-based message passing. Optional zones for memory management are feature-gated for incremental adoption:

- **Tasks**: `spawn_task`, `spawn_task_with_priority(Priority)` create lightweight threads and return `Task` handles (`Task::join()`).
- **Channels**: `create_channel<T>() -> (SenderHandle<T>, ReceiverHandle<T>)` for typed message passing.
- **Feature flags** (in `medic_runtime`):
  - `gc`: Enables `gc_zone::SafeGc` stubs and `collect_garbage()` hooks for a safe, GC-oriented zone.
  - `rt_zones`: Enables `rt_zone::RtZone` stubs with `enter()/exit()` for simplified real-time regions.

Enable features in your Cargo manifest or via CLI:

```toml
[dependencies]
medic_runtime = { path = "compiler/medic_runtime", features = ["gc", "rt_zones"] }
```

```bash
cargo test -p medic_runtime --features gc,rt_zones
```

These stubs establish the ergonomic surface for future zone semantics (safe GC zone, constrained RT zone) and can evolve without breaking user code.

#### Real-Time Memory (rt_zones)

When the `rt_zones` feature is enabled in `medic_runtime`, two deterministic allocation primitives are available:

- `RtRegion<const BYTES: usize>`: a region/bump allocator with compile-time capacity for transient allocations.
  - `alloc<T>(value: T) -> Option<&mut T>`
  - `alloc_uninit<T>() -> Option<&mut MaybeUninit<T>>`
  - `alloc_array_uninit<T>(len: usize) -> Option<&mut [MaybeUninit<T>]>`
  - `unsafe fn reset(&self)`: frees the entire region; all prior references become invalid.

- `FixedPool<T, const N: usize>`: a fixed-size object pool with O(1) allocate/free.
  - `alloc(value: T) -> Option<&mut T>`
  - `unsafe fn free_ptr(&self, ptr: *mut T)`

RT code should use these APIs to achieve deterministic allocation latency. A typical pattern is to use a `FixedPool` for messages/objects and an `RtRegion` for transient buffers, periodically calling `reset()` at well-defined points (e.g., once per frame/tick) to reclaim memory.

RT sections can be annotated in code (at the AST level) using markers `rt_begin()` and `rt_end()`. The compiler provides an optional static check (enabled via environment variable `MEDI_RT_CHECK=1` in the CLI) that flags disallowed calls within RT sections, including but not limited to:

- `medi_gc_alloc_string`, `medi_gc_collect` (GC interactions)
- `spawn_task`, `create_channel` (host runtime concurrency APIs)

This check is conservative and name-based; it is intended as an aid to keep RT sections free from GC and blocking/dynamic behaviors.

Run-time example (requires `rt_zones`):

```bash
cargo run -p medic_runtime --example rt_iot --features rt_zones
```

This example demonstrates a sensor loop using `FixedPool` and `RtRegion`, with a periodic `reset()` pattern and prints worst-case per-iteration latency.

##### Safer Handles and Scoped Regions

- `PoolBox<T, N>`: RAII handle from `FixedPool::alloc_box(value)` that auto-returns to the pool when dropped.

```rust
let pool: FixedPool<MyMsg, 256> = FixedPool::new();
if let Some(mut msg) = pool.alloc_box(MyMsg { ..Default::default() }) {
    msg.field = 42;
} // msg dropped -> returned to pool
```

- `RtScope<BYTES>`: obtain with `let scope = region.scope();` to automatically reclaim all allocations on scope drop.

```rust
let region = RtRegion::<{ 16 * 1024 }>::new();
{
    let scope = region.scope();
    let buf = scope.alloc_array_uninit::<u8>(64).unwrap();
    // use buf...
} // scope dropped -> region.reset() invoked
```

- `RtZone<BYTES, T, N>`: unified wrapper bundling a region and pool with a single scoped entry/exit.

```rust
// Pool element type T, region capacity BYTES, pool capacity N
let zone: RtZone<MyMsg, { 16 * 1024 }, 256> = RtZone::new();
let scope = zone.scope();
// Pool allocation (auto-return on drop)
if let Some(mut msg) = scope.alloc_pool_box(MyMsg { ..Default::default() }) {
    // Region allocation (auto-reset on scope drop)
    let tmp = scope.alloc_region_array_uninit::<u8>(64).unwrap();
    // ...
}
```

#### RtZone: Unified Real-Time Zone

`RtZone<BYTES, T, N>` provides a convenience wrapper that combines a region (`RtRegion<BYTES>`) and a fixed-size pool (`FixedPool<T, N>`) under a single scoped guard for deterministic RT code.

- **Design goals**
- Deterministic O(1) allocations and frees.
- Minimize `unsafe` in user code by using RAII (`PoolBox`) and scoped resets (`RtZoneScope`).
- Provide ergonomic, structured lifetime management for IoT loops/ticks.

- **Invariants**
- `RtZoneScope` resets the region on drop; all region-backed references become invalid after scope exit.
- `PoolBox` auto-returns elements to the pool on drop; do not store `PoolBox` across scopes unless intended.
- The pool capacity `N` bounds concurrent live elements; allocation returns `None` when exhausted.

- **Usage patterns**
```rust
let zone: RtZone<MyMsg, { 16 * 1024 }, 256> = RtZone::new();
loop {
    let scope = zone.scope();
    if let Some(mut msg) = scope.alloc_pool_box(MyMsg::default()) {
        let tmp = scope.alloc_region_array_uninit::<u8>(64).unwrap();
        // process ...
    } // msg returned to pool here; region reclaimed when scope drops
    // next iteration gets a fresh region
}
```

- **Anti-patterns**
- Holding references into the region past scope end (UB). Prefer copying out minimal results if needed.
- Leaking `PoolBox` with `into_raw()` without arranging a paired `free_ptr` (advanced/unsafe only).
- Performing blocking I/O or GC-triggering calls inside RT sections.

- **Performance notes**
- Pool ops are LIFO O(1); region bump/align ops are O(1). Typical per-op latency validated to ≤ 50µs under tests.
- Choose `BYTES`/`N` to avoid runtime growth/reallocation. Use a periodic scope cadence (e.g., every tick) for predictable reclaim.

### Error Handling

1. **Result Type**
   ```medi
   Result<T, E> = Ok(T) | Err(E)
   ```

2. **Error Propagation**
   - `?` operator for early returns
   - Try/catch for exceptional cases
   - Error type hierarchy for medical errors

## Compilation Targets

- **Native Machine Code**: LLVM-based compilation for near-C++ performance
- **WebAssembly**: For edge devices and browser-based applications
- **RISC-V**: Support for medical devices with custom instructions for genomics and imaging

## Compiler Architecture

The Medi compiler (`medic`) consists of several stages:

1. **Parser**: Recursive descent parser handling healthcare-specific syntax
   - Converts source code into AST
   - Provides clinician-friendly error messages
   - Handles constructs like `fhir_query`, `federated`, `regulate`

2. **Type System**:
   - Healthcare-specific types (e.g., `FHIRPatient`, `VitalSigns`)
   - Privacy-aware type checking
   - Memory safety through Rust-like borrow checking

3. **Privacy & Compliance Checker**:
   - Enforces HIPAA/GDPR rules at compile time
   - Validates data access patterns
   - Ensures proper anonymization (using both type-level rules and standard library helpers such as `medi.compliance` de-identification and HIPAA bundle checks)

4. **Code Generation**:
   - LLVM-based backend
   - Targets:
     - Native machine code
     - WebAssembly for edge devices
     - RISC-V for medical hardware
   - Healthcare-specific optimizations

## Ecosystem Components

1. **Package Manager (`medipack`)**:
   - Dependency management
   - Build system
   - Package registry at medipacks.io

2. **Standard Library**:
   - `medi::fhir` - FHIR/HL7 integration
   - `medi::dicom` - Medical imaging
   - `medi::genomics` - Genomic analysis
   - `medi::ai` - Healthcare AI and ML
   - `medi::privacy` - Privacy-preserving analytics

3. **Development Tools**:
   - Visual IDE for clinicians
   - Documentation generator
   - Test framework
   - Profiler for performance analysis

## Implementation Timeline

### Phase 1: Foundation (0-2 Years)
- Complete Rust-written compiler
- Implement core language features
- Basic standard library
- Privacy/compliance checker

### Phase 2: Ecosystem Growth (2-3 Years)
- Launch `medipack` and registry
- Expand standard library
- Visual IDE beta
- Stabilize healthcare constructs

### Phase 3: Self-Hosting (3-4 Years)
- Begin compiler rewrite in Medi
- Port parser and type checker
- Interface with LLVM

### Phase 4: Maturity (4-5 Years)
- Complete self-hosted compiler
- Full ecosystem maturity
- Advanced AI capabilities
- Quantum computing support

## Reserved & Future (Post v0.1)

- Pipeline operator `|>`: left-associative; desugars `x |> f(a)` to `f(x, a)`; specify in v0.2.
  - Note: the lexer has optional feature-gated tokenization for `|>` via `pipeline_op`, but this does not imply parser or semantic support in v0.1.
- Async/await syntax and runtime: structured concurrency; deferred in v0.1.
- Units/Quantities (UCUM): quantity literals and compile-time unit checking; v0.2+.
- try/catch sugar: surface syntax layered over `Result`; planned.
- Target blocks `target riscv { ... }`: build-time configuration; not core semantics.


## Version History

- **0.1.0**: Initial specification, covering core syntax and medical features.

