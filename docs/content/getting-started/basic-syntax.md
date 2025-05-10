# Medi Basic Syntax

Medi's syntax is designed to be intuitive for both beginners and experienced programmers, with special attention to healthcare domain needs. Medi follows a Rust-inspired approach with clean, modern syntax while maintaining healthcare-specific features.

## File Extension

Medi source files use the `.mdi` extension:  
```
myprogram.mdi
patient_analysis.mdi
```

## Variables and Types

Medi uses type inference but also supports explicit typing:

```mdi
// Type inference
patient_name = "John Doe";  // String
heart_rate = 75;  // Integer
temperature = 98.6;  // Float

// Explicit typing
String patient_id = "P-12345";
Integer bp_systolic = 120;
Float bmi = 22.5;
```

## Healthcare Data Types

Medi includes native healthcare data types:

```mdi
// FHIR resources
Patient john = fhir_resource("Patient", id: "P-12345");

// Genomic data
VCF variants = load_vcf("sample.vcf");

// Time series (for vital signs)
TimeSeries ecg = load_series("ecg_data.csv", frequency: 250);
```

## Control Flow

Medi's control flow constructs are similar to Python and C-like languages:

```mdi
// If-else statement
if (heart_rate > 100) {
  alert("Tachycardia detected");
} else if (heart_rate < 60) {
  alert("Bradycardia detected");
} else {
  log("Normal heart rate");
}

// For loop
for (patient in patients) {
  calculate_risk_score(patient);
}

// While loop
while (monitoring_active) {
  read_vitals();
  sleep(1000);  // milliseconds
}
```

## Functions

Functions are declared with the `function` keyword:

```medi
// Basic function
function calculate_bmi(weight_kg, height_m) {
  return weight_kg / (height_m * height_m);
}

// Function with explicit types
function Boolean is_hypertensive(Integer systolic, Integer diastolic) {
  return systolic >= 140 || diastolic >= 90;
}

// With default parameters
function administer_medication(String med_id, Float dose, String route = "oral") {
  // Implementation
}
```

## Data Pipeline Operators

Medi supports data pipelines inspired by R and F#:

```medi
// Pipeline operator |>
patients
  |> filter(condition: "diabetes")
  |> sort(by: "a1c_level", descending: true)
  |> limit(10)
  |> plot_risk_score();
```

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

```medi
try {
  result = analyze_bloodwork(sample_id);
} catch (SampleNotFoundError e) {
  log("Sample not found: " + e.message);
  request_new_sample();
} catch (AnalysisError e) {
  log("Analysis failed: " + e.message);
  retry_analysis();
} finally {
  cleanup_resources();
}
```

## Next Steps

* Try a [complete Medi program](first-program.md)
* Learn about [Medi's standard library](../reference/standard-library.md)
* Explore [Medical Data Science features](../key-features/medical-data-science.md)
