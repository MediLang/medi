use crate::fhir::{
    FHIRCondition, FHIRDiagnosticReport, FHIREncounter, FHIRMedicalEvent, FHIRMedication,
    FHIRObservation, FHIRPatient, FHIRProcedure,
};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FHIRAny {
    Patient(FHIRPatient),
    Observation(FHIRObservation),
    MedicalEvent(FHIRMedicalEvent),
    Medication(FHIRMedication),
    Procedure(FHIRProcedure),
    Condition(FHIRCondition),
    Encounter(FHIREncounter),
    DiagnosticReport(FHIRDiagnosticReport),
}
