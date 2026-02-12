//! Test data generators for integration tests and benchmarks

use crate::fhir::{FHIRObservation, FHIRPatient};
use crate::fhir_any::FHIRAny;
use crate::fhir_bundle::{BundleType, FHIRBundle};

pub fn patient_factory(family: &str, given: &str, dob: &str) -> FHIRPatient {
    FHIRPatient {
        id: format!("{family}-{given}"),
        family_name: Some(family.to_string()),
        given_name: Some(given.to_string()),
        birth_date: Some(dob.to_string()),
    }
}

pub fn synthetic_lab_results(n: usize) -> Vec<FHIRObservation> {
    (0..n)
        .map(|i| FHIRObservation {
            id: format!("obs-{i}"),
            code: if i % 2 == 0 { "HGB" } else { "GLU" }.to_string(),
            value: Some(((i % 100) as f64) / 10.0),
            unit: Some("arb".into()),
        })
        .collect()
}

pub fn bundle_factory(n: usize) -> FHIRBundle {
    let mut b = FHIRBundle::new(BundleType::Collection, vec![]);
    for i in 0..n {
        let p = patient_factory("Doe", &format!("Jane{i}"), "1980-01-01");
        b.push_entry(FHIRAny::Patient(p));
    }
    b
}
