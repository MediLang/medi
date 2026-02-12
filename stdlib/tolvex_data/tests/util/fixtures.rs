use tolvex_data::{
    fhir::{FHIRMedicalEvent, FHIRObservation, FHIRPatient},
    ndjson::from_ndjson,
};

#[allow(dead_code)]
pub fn load_patients() -> Vec<FHIRPatient> {
    let s = include_str!("../fixtures/patients.ndjson");
    from_ndjson(s).expect("patients fixtures parse")
}

#[allow(dead_code)]
pub fn load_events() -> Vec<FHIRMedicalEvent> {
    let s = include_str!("../fixtures/events.ndjson");
    from_ndjson(s).expect("events fixtures parse")
}

pub fn load_observations() -> Vec<FHIRObservation> {
    let s = include_str!("../fixtures/observations.ndjson");
    from_ndjson(s).expect("observations fixtures parse")
}
