use std::fs;

use medi_data::{
    fhir::{FHIRMedicalEvent, FHIRObservation, FHIRPatient},
    fhir_query,
    ndjson::from_ndjson,
};

fn load_env(path_var: &str) -> Option<String> {
    std::env::var(path_var)
        .ok()
        .filter(|s| !s.trim().is_empty())
}

#[test]
#[ignore]
fn realdata_patients_observations_events_optional() {
    let patients_path = match load_env("MEDI_DATA_PATIENTS") {
        Some(p) => p,
        None => return,
    };
    let observations_path = match load_env("MEDI_DATA_OBSERVATIONS") {
        Some(p) => p,
        None => return,
    };
    let events_path = match load_env("MEDI_DATA_EVENTS") {
        Some(p) => p,
        None => return,
    };

    let patients_s = fs::read_to_string(&patients_path).expect("read patients");
    let observations_s = fs::read_to_string(&observations_path).expect("read observations");
    let events_s = fs::read_to_string(&events_path).expect("read events");

    let patients: Vec<FHIRPatient> = from_ndjson(&patients_s).expect("patients parse");
    let observations: Vec<FHIRObservation> =
        from_ndjson(&observations_s).expect("observations parse");
    let events: Vec<FHIRMedicalEvent> = from_ndjson(&events_s).expect("events parse");

    assert!(!patients.is_empty());
    assert!(!observations.is_empty());
    assert!(!events.is_empty());

    // Example query: find events in a date window
    let q = fhir_query("MedicalEvent")
        .filter_start_date_ge("2000-01-01")
        .filter_start_date_le("2100-01-01")
        .build();
    let matched = q.execute_medical_events(&events);
    assert!(matched.len() <= events.len());
}
