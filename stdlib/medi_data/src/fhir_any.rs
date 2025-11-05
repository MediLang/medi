use crate::fhir::{FHIRMedicalEvent, FHIRObservation, FHIRPatient};

#[derive(Debug, Clone)]
pub enum FHIRAny {
    Patient(FHIRPatient),
    Observation(FHIRObservation),
    MedicalEvent(FHIRMedicalEvent),
}
