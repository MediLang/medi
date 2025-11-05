use medi_data::{
    fhir::{FHIRMedicalEvent, FHIRPatient},
    fhir_query,
    ndjson::from_ndjson,
};

#[test]
fn import_patients_events_and_query() {
    let patients_s = include_str!("fixtures/patients.ndjson");
    let events_s = include_str!("fixtures/events.ndjson");
    let patients: Vec<FHIRPatient> = from_ndjson(patients_s).expect("patients");
    let events: Vec<FHIRMedicalEvent> = from_ndjson(events_s).expect("events");

    assert_eq!(patients.len(), 2);
    assert_eq!(events.len(), 2);

    // Query events for cardiology
    let q = fhir_query("MedicalEvent")
        .filter_event_code_ci_contains("card")
        .build();
    let res = q.execute_medical_events(&events);
    assert_eq!(res.len(), 1);
    assert_eq!(res[0].id, "e1");
}
