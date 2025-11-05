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
