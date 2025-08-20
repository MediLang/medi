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
   +=   -=   *=   /=   %=   &&   ||   !    ->   =>   {    }    (    )    [    ]
   .    ::   ,    :    ;    @    #    ?    ..   ..=  ...
   ```

4. **Literals**:
   - Integers: `42`, `0xFF`, `0b1010`
   - Floats: `3.14`, `1.0e-10`
   - Strings: `"hello"`, `"""multiline"""`
   - Boolean: `true`, `false`
   - DateTime: `2025-05-15T00:00:00Z`
   - Medical: `pid("PT123")`, `icd10("A00.0")`

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
- `medi::privacy::hipaa` - HIPAA compliance
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
   - Ensures proper anonymization

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
- Async/await syntax and runtime: structured concurrency; deferred in v0.1.
- Units/Quantities (UCUM): quantity literals and compile-time unit checking; v0.2+.
- try/catch sugar: surface syntax layered over `Result`; planned.
- Target blocks `target riscv { ... }`: build-time configuration; not core semantics.


## Version History

- **0.1.0**: Initial specification, covering core syntax and medical features.

