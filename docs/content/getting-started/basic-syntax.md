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

Medi includes native support for healthcare data types and integrates with healthcare standards:

```medi
// FHIR resources (automatically mapped to Medi types)
let patient = fhir::Patient.get("P-12345");
let observations = fhir::Observation.search(patient: patient.id, category: "vital-signs");

// Working with medical records
let record = MedicalRecord {
  id: "MR-45678",
  patient: patient,
  conditions: ["Type 2 Diabetes", "Hypertension"],
  medications: [
    Medication { name: "Metformin", dosage: "500mg", frequency: "BID" },
    Medication { name: "Lisinopril", dosage: "10mg", frequency: "Daily" }
  ]
};

// Genomic data
let variants = vcf::load("sample.vcf");
let pathogenic_variants = variants.filter(|v| v.clinical_significance == "Pathogenic");

// Time series data (for vital signs, ECGs, etc.)
let ecg = timeseries::from_csv("ecg_data.csv")?;
let heart_rate_variability = ecg.calculate_hrv();

// Medical imaging
let mri = dicom::load("brain_scan.dcm")?;
let tumor_volume = mri.segment_tumor().calculate_volume();

// Working with medical codes
let icd10 = icd10::from_code("E11.65");  // Type 2 diabetes with hyperglycemia
let snomed = snomed::from_code("44054006");  // Diabetes mellitus
```

## Control Flow

Medi's control flow constructs are similar to Rust and other C-like languages:

```medi
// If-else statement
if heart_rate > 100 {
  alert("Tachycardia detected");
} else if heart_rate < 60 {
  alert("Bradycardia detected");
} else {
  log("Normal heart rate");
}

// For loop with pattern matching
for patient in patients {
  calculate_risk_score(patient);
}

// While loop
while monitoring_active {
  read_vitals();
  std::thread::sleep(std::time::Duration::from_millis(1000));
}

// Match expression (like switch/case but more powerful)
match patient_status {
  "critical" => {
    alert_doctor();
    increase_monitoring();
  },
  "stable" => log("Patient is stable"),
  _ => log("Unknown status"),
}

// Concise match expression syntax in expression context
// Equivalent to: match status { ... }
status {
  "ok" => 1,
  "warn" => 2,
  _ => 0,
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
