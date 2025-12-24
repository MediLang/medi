# Your First Medi Program

This guide walks you through writing a small Medi program and checking it with the current compiler CLI.

## A Simple Health Risk Calculator

Let's create a simple program that calculates a basic health risk score based on patient parameters.

Create a file named `risk_calculator.medi` with the following content:

```medi
// First Medi Program: Health Risk Calculator

// Define our risk calculation function
fn calculate_risk_score(age: int, systolic_bp: int, diastolic_bp: int, is_smoker: bool, has_diabetes: bool) -> int {
  // Start with base score based on age
  let mut score = age / 10;
  
  // Add points for blood pressure
  if systolic_bp >= 140 || diastolic_bp >= 90 {
    score += 2;
  } else if systolic_bp >= 120 || diastolic_bp >= 80 {
    score += 1;
  }
  
  // Add points for risk factors
  if is_smoker { score += 3; }
  if has_diabetes { score += 2; }
  
  score
}

// Define a patient record type
record Patient {
  name: string,
  age: int,
  systolic: int,
  diastolic: int,
  smoker: bool,
  diabetes: bool
}

// Sample patient data
let patients: list[Patient] = [
  Patient{name: "Patient A", age: 45, systolic: 130, diastolic: 85, smoker: true, diabetes: false},
  Patient{name: "Patient B", age: 60, systolic: 145, diastolic: 95, smoker: false, diabetes: true},
  Patient{name: "Patient C", age: 30, systolic: 115, diastolic: 75, smoker: false, diabetes: false}
];

// Compute a value so it stays typechecked end-to-end
let example_risk = calculate_risk_score(
  patients[0].age,
  patients[0].systolic,
  patients[0].diastolic,
  patients[0].smoker,
  patients[0].diabetes
);
```

## Checking the Program

From the repository root, typecheck the file with `medic`:

```bash
cargo run -p medic -- check risk_calculator.medi
```

## Key Concepts Demonstrated

1. **Function Definition**: The `calculate_risk_score` function
2. **Data Structures**: Using a `record` type for patient records
3. **Control Flow**: `if/else` statements and `for` loops
4. **Types**: Explicit types for parameters and returns

## Next Steps

* Try modifying the risk calculation formula
* Add more patient data
* Read the [Language Specification](../reference/language-spec.md)
* Learn about the [compiler and tools](../technical/tooling.md)
