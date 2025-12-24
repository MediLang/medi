# Medi Basic Syntax

Medi's syntax is designed to be intuitive for both beginners and experienced programmers, with special attention to healthcare domain needs. Medi follows a Rust-inspired approach with clean, modern syntax while maintaining healthcare-specific features.

## File Extension

Medi source files use the `.medi` extension:  
```
myprogram.medi
patient_analysis.medi
```

## Variables and Types

Medi uses type inference but also supports explicit typing. Variables are declared with `let` and can be made mutable with `mut`:

```medi
// Type inference with let
let patient_name = "John Doe";  // string
let heart_rate = 75;           // int
let temperature = 98.6;        // float
let is_critical = false;       // bool

// Explicit type annotations
let patient_id: string = "P-12345";
let bp_systolic: int = 120;
let bmi: float = 22.5;

// Mutable variables
let mut count = 0;
count += 1;  // This works because count is mutable

// Constants (must be explicitly typed)
const MAX_HEART_RATE: int = 220;
const PI: float = 3.14159;
```

## Healthcare Data Types

Medi supports composite types that are commonly used to model clinical data. The language specification also defines medical-specific literals for identifiers and codes.

```medi
// Medical-specific literals
let patient = pid("PT-12345");
let dx = icd10("E11.65");

// Structured record types
record Vital {
  temperature_c: float,
  pulse_bpm: int
}

record Patient {
  id: patient_id,
  age: int,
  vitals: list[Vital]
}

let p = Patient{
  id: patient,
  age: 45,
  vitals: [Vital{temperature_c: 37.2, pulse_bpm: 80}]
};

// Map type for tagged metadata
let tags: map[string, string] = {"site": "clinic-a", "unit": "ICU"};
```

## Control Flow

Medi's control flow constructs are similar to Rust and other C-like languages:

```medi
// If-else statement
if heart_rate > 100 {
  print("Tachycardia detected");
} else if heart_rate < 60 {
  print("Bradycardia detected");
} else {
  print("Normal heart rate");
}

// For loop
for patient in patients {
  calculate_risk_score(patient);
}

// While loop
while monitoring_active {
  print("monitoring...");
}

// Pattern matching
match lab_result {
  case lab_result(name: "CRP", value: v) if v > 10 => print("Elevated CRP")
  case _ => print("Normal result")
}
```

## Functions

Functions are declared with the `fn` keyword:

```medi
// Basic function
fn calculate_bmi(weight_kg, height_m) {
  weight_kg / (height_m * height_m)
}

// Function with explicit types
fn is_hypertensive(systolic: int, diastolic: int) -> bool {
  systolic >= 140 || diastolic >= 90
}

// With default parameters
fn administer_medication(med_id: string, dose: float, route: string = "oral") {
  // Implementation
}
```

## Data Pipeline Operators

The pipeline operator `|>` is **reserved for a future version** (see the Language Specification).

## Healthcare-Specific Syntax

```medi
// FHIR queries
dataset diabetic_patients = fhir_query("Patient", filter: "condition=diabetes");

// Compliance checks
regulate {
  standard: "HIPAA",
  data: patient_records,
  checks: ["phi_identification", "access_control"]
};

// Privacy-preserving analytics
federated {
  sites: ["hospital_a", "hospital_b", "hospital_c"],
  model: "random_forest",
  target: "readmission_risk"
};
```

## Error Handling

v0.1 uses `Result<T, E>` and the `?` operator for error propagation.

```medi
fn load_patient(id: string) -> Result<record, Error> {
  let patient = fhir_query("Patient", id)?
  Ok(patient)
}
```

## Next Steps

* Try a [complete Medi program](first-program.md)
* Learn about [Medi's standard library](../reference/standard-library.md)
* Explore [Medical Data Science features](../key-features/medical-data-science.md)
