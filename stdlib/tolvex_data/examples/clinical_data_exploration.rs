use std::fs;

use tolvex_data::fhir::{FHIRObservation, FHIRPatient};
use tolvex_stats::mean;

fn parse_year(date: &str) -> Option<i32> {
    // Accept YYYY-MM-DD (as used by our sample data).
    date.get(0..4)?.parse::<i32>().ok()
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // These example datasets live at the repo level so they can be used by both
    // Rust harnesses and .tlvx programs.
    let patients_path = "examples/use_cases/clinical_patients.json";
    let obs_path = "examples/use_cases/clinical_observations.json";

    let patients_text = fs::read_to_string(patients_path)?;
    let obs_text = fs::read_to_string(obs_path)?;

    let patients: Vec<FHIRPatient> = serde_json::from_str(&patients_text)?;
    let observations: Vec<FHIRObservation> = serde_json::from_str(&obs_text)?;

    // Cohort selection: age > 65 (approximate by year; deterministic and adequate for demo).
    let reference_year = 2025;
    let cohort: Vec<&FHIRPatient> = patients
        .iter()
        .filter(|p| {
            p.birth_date
                .as_deref()
                .and_then(parse_year)
                .is_some_and(|y| reference_year - y > 65)
        })
        .collect();

    // Metric: average HbA1c across the sample observation set.
    // Note: current FHIRObservation type does not include subject linkage, so this demo
    // computes across the dataset rather than per-patient.
    let hba1c_values: Vec<f64> = observations
        .iter()
        .filter(|o| o.code.eq_ignore_ascii_case("HBA1C"))
        .filter_map(|o| o.value)
        .collect();

    println!("clinical cohort size (age>65): {}", cohort.len());
    if hba1c_values.is_empty() {
        println!("no HbA1c values found");
        return Ok(());
    }

    let avg = mean(&hba1c_values);
    println!("average HbA1c across dataset: {avg:.2}");

    Ok(())
}
