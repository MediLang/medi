use serde::{Deserialize, Serialize};

pub trait FHIRResource {
    fn resource_type(&self) -> &'static str;
    fn id(&self) -> &str;
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct FHIRPatient {
    pub id: String,
    pub given_name: Option<String>,
    pub family_name: Option<String>,
    pub birth_date: Option<String>,
}

impl FHIRResource for FHIRPatient {
    fn resource_type(&self) -> &'static str {
        "Patient"
    }
    fn id(&self) -> &str {
        &self.id
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct FHIRObservation {
    pub id: String,
    pub code: String,
    pub value: Option<f64>,
    pub unit: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct FHIRMedicalEvent {
    pub id: String,
    pub code: String,
    pub start_date: Option<String>, // YYYY-MM-DD or YYYYMMDD
    pub description: Option<String>,
}

impl FHIRResource for FHIRMedicalEvent {
    fn resource_type(&self) -> &'static str {
        "MedicalEvent"
    }
    fn id(&self) -> &str {
        &self.id
    }
}

impl FHIRResource for FHIRObservation {
    fn resource_type(&self) -> &'static str {
        "Observation"
    }
    fn id(&self) -> &str {
        &self.id
    }
}

pub fn patient_from_json(s: &str) -> Result<FHIRPatient, serde_json::Error> {
    serde_json::from_str::<FHIRPatient>(s)
}

pub fn patient_to_json(p: &FHIRPatient) -> Result<String, serde_json::Error> {
    serde_json::to_string(p)
}

pub fn observation_from_json(s: &str) -> Result<FHIRObservation, serde_json::Error> {
    serde_json::from_str::<FHIRObservation>(s)
}

pub fn observation_to_json(o: &FHIRObservation) -> Result<String, serde_json::Error> {
    serde_json::to_string(o)
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct FHIRMedication {
    pub id: String,
    pub code: String,
    pub form: Option<String>,
    pub ingredients: Vec<String>,
}

impl FHIRResource for FHIRMedication {
    fn resource_type(&self) -> &'static str {
        "Medication"
    }
    fn id(&self) -> &str {
        &self.id
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct FHIRProcedure {
    pub id: String,
    pub code: String,
    pub performed_date: Option<String>, // YYYY-MM-DD or YYYYMMDD
    pub subject: String,                // Patient reference (id)
}

impl FHIRResource for FHIRProcedure {
    fn resource_type(&self) -> &'static str {
        "Procedure"
    }
    fn id(&self) -> &str {
        &self.id
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct FHIRCondition {
    pub id: String,
    pub code: String,
    pub clinical_status: String,
    pub verification_status: String,
    pub subject: String,
}

impl FHIRResource for FHIRCondition {
    fn resource_type(&self) -> &'static str {
        "Condition"
    }
    fn id(&self) -> &str {
        &self.id
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct FHIREncounter {
    pub id: String,
    pub class_: Option<String>,     // e.g., inpatient, outpatient
    pub start_date: Option<String>, // YYYY-MM-DD or YYYYMMDD
    pub end_date: Option<String>,   // YYYY-MM-DD or YYYYMMDD
    pub subject: String,
}

impl FHIRResource for FHIREncounter {
    fn resource_type(&self) -> &'static str {
        "Encounter"
    }
    fn id(&self) -> &str {
        &self.id
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct FHIRDiagnosticReport {
    pub id: String,
    pub code: String,
    pub result_codes: Vec<String>,
    pub subject: String,
}

impl FHIRResource for FHIRDiagnosticReport {
    fn resource_type(&self) -> &'static str {
        "DiagnosticReport"
    }
    fn id(&self) -> &str {
        &self.id
    }
}

pub fn medication_from_json(s: &str) -> Result<FHIRMedication, serde_json::Error> {
    serde_json::from_str::<FHIRMedication>(s)
}

pub fn medication_to_json(m: &FHIRMedication) -> Result<String, serde_json::Error> {
    serde_json::to_string(m)
}

pub fn procedure_from_json(s: &str) -> Result<FHIRProcedure, serde_json::Error> {
    serde_json::from_str::<FHIRProcedure>(s)
}

pub fn procedure_to_json(p: &FHIRProcedure) -> Result<String, serde_json::Error> {
    serde_json::to_string(p)
}

pub fn condition_from_json(s: &str) -> Result<FHIRCondition, serde_json::Error> {
    serde_json::from_str::<FHIRCondition>(s)
}

pub fn condition_to_json(c: &FHIRCondition) -> Result<String, serde_json::Error> {
    serde_json::to_string(c)
}

pub fn encounter_from_json(s: &str) -> Result<FHIREncounter, serde_json::Error> {
    serde_json::from_str::<FHIREncounter>(s)
}

pub fn encounter_to_json(e: &FHIREncounter) -> Result<String, serde_json::Error> {
    serde_json::to_string(e)
}

pub fn diagnostic_report_from_json(s: &str) -> Result<FHIRDiagnosticReport, serde_json::Error> {
    serde_json::from_str::<FHIRDiagnosticReport>(s)
}

pub fn diagnostic_report_to_json(r: &FHIRDiagnosticReport) -> Result<String, serde_json::Error> {
    serde_json::to_string(r)
}
