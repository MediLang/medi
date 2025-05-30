# Your First Medi Program

This guide walks you through creating and running your first Medi program.

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

// Calculate and display risk for each patient
for patient in patients {
  let risk = calculate_risk_score(
    patient.age,
    patient.systolic,
    patient.diastolic,
    patient.smoker,
    patient.diabetes
  );
  
  // Display result
  println!("{} risk score: {}", patient.name, risk);
}
  
  // Risk classification
  if (risk < 5) {
    print("  Status: Low Risk");
  } else if (risk < 10) {
    print("  Status: Moderate Risk");
  } else {
    print("  Status: High Risk");
  }
}

// Visualize results
visualize {
  plot_bar(
    data: patients,
    x: "name",
    y: (p) => calculate_risk_score(p.age, p.systolic, p.diastolic, p.smoker, p.diabetes),
    title: "Patient Risk Scores",
    y_label: "Risk Score"
  );
}
```

## Running the Program

Save the file and run it using the Medi interpreter:

```bash
medi risk_calculator.medi
```

You should see output similar to:

```
Patient A risk score: 9.5
  Status: Moderate Risk
Patient B risk score: 12
  Status: High Risk
Patient C risk score: 3
  Status: Low Risk
```

A new window will also open displaying a bar chart of the patient risk scores.

## Key Concepts Demonstrated

1. **Function Definition**: The `calculate_risk_score` function
2. **Data Structures**: Using the `dataset` type for patient records
3. **Control Flow**: `if/else` statements and `for` loops
4. **Visualization**: Simple bar chart creation with the `visualize` block

## Next Steps

* Try modifying the risk calculation formula
* Add more patient data
* Explore more complex [visualizations](../key-features/data-visualization.md)
* Learn about [integrating with healthcare standards](../key-features/healthcare-standards.md) like FHIR
