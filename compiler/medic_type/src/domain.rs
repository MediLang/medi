// Healthcare domain types for Medi language
// These are runtime-side representations useful for validation and future integrations.

use crate::traits::{
    Auditable, HealthcareFormat, Identifiable, MedicalRecordTrait, PrivacyProtected, Serializable,
    Timestamped, Validatable, ValidationCtx, ValidationError,
};
use crate::types::PrivacyAnnotation;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PatientID(pub String);

impl Identifiable for PatientID {
    fn id(&self) -> &str {
        &self.0
    }
}

impl Validatable for PatientID {
    fn validate(&self) -> Result<(), ValidationError> {
        if self.0.trim().is_empty() {
            return Err(ValidationError::EmptyField {
                field: "patient_id",
            });
        }
        Ok(())
    }
}

impl PrivacyProtected for PatientID {
    fn privacy_annotation(&self) -> PrivacyAnnotation {
        PrivacyAnnotation::PHI
    }
}

impl Serializable for PatientID {
    fn serialize(&self, format: HealthcareFormat) -> String {
        match format {
            HealthcareFormat::Json | HealthcareFormat::FhirJson => {
                format!("{{\"patient_id\":\"{}\"}}", self.0)
            }
            HealthcareFormat::Hl7V2 => format!("PID|{}", self.0),
            HealthcareFormat::Dicom => format!("DICOM:PatientID={}", self.0),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LabResult {
    pub code: String, // e.g., LOINC code
    pub value: f64,
    pub unit: String,      // e.g., mg/dL
    pub timestamp_ms: i64, // epoch millis UTC
    pub created_by: String,
}

impl Timestamped for LabResult {
    fn timestamp_millis(&self) -> i64 {
        self.timestamp_ms
    }
}

impl Auditable for LabResult {
    fn created_by(&self) -> &str {
        &self.created_by
    }
    fn created_at_millis(&self) -> i64 {
        self.timestamp_ms
    }
}

impl Validatable for LabResult {
    fn validate(&self) -> Result<(), ValidationError> {
        if self.code.trim().is_empty() {
            return Err(ValidationError::EmptyField { field: "code" });
        }
        if self.unit.trim().is_empty() {
            return Err(ValidationError::EmptyField { field: "unit" });
        }
        if !self.timestamp_ms.is_positive() {
            return Err(ValidationError::InvalidFormat {
                field: "timestamp_ms",
                message: "must be a positive epoch millis",
            });
        }
        Ok(())
    }

    fn validate_with_ctx(&self, ctx: &ValidationCtx) -> Result<(), ValidationError> {
        // Base checks
        self.validate()?;

        // Code-system and UCUM checks
        if !ctx.is_valid_loinc(&self.code) {
            return Err(ValidationError::UnknownCode {
                system: "LOINC",
                code: self.code.clone(),
            });
        }
        if !ctx.is_valid_ucum(&self.unit) {
            return Err(ValidationError::InvalidUnit {
                unit: self.unit.clone(),
            });
        }
        // If preferred units exist for this LOINC, ensure current unit is allowed
        if let Some(units) = ctx.loinc_to_units.get(&self.code) {
            if !units.contains(&self.unit) {
                // Accept unknown mapping but flag as format issue
                return Err(ValidationError::InvalidFormat {
                    field: "unit",
                    message: "unit not preferred for given LOINC",
                });
            }
        }

        // Reference range check if available
        if let Some((min, max)) = ctx.reference_range(&self.code, &self.unit) {
            if self.value < min || self.value > max {
                return Err(ValidationError::OutOfReferenceRange {
                    code: self.code.clone(),
                    unit: self.unit.clone(),
                    min,
                    max,
                    actual: self.value,
                });
            }
        }
        Ok(())
    }
}

impl PrivacyProtected for LabResult {
    fn privacy_annotation(&self) -> PrivacyAnnotation {
        PrivacyAnnotation::PHI
    }
}

impl Serializable for LabResult {
    fn serialize(&self, format: HealthcareFormat) -> String {
        match format {
            HealthcareFormat::Json | HealthcareFormat::FhirJson => format!(
                "{{\"code\":\"{}\",\"value\":{},\"unit\":\"{}\",\"timestamp_ms\":{},\"created_by\":\"{}\"}}",
                self.code, self.value, self.unit, self.timestamp_ms, self.created_by
            ),
            HealthcareFormat::Hl7V2 => format!(
                "OBX|1|NM|{}||{}|{}|||F|||{}|{}",
                self.code, self.value, self.unit, self.created_by, self.timestamp_ms
            ),
            HealthcareFormat::Dicom => format!(
                "DICOM:LabCode={};Value={};Unit={};Time={}",
                self.code, self.value, self.unit, self.timestamp_ms
            ),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Vital {
    pub name: String, // e.g., "heart_rate", "systolic_bp"
    pub value: f64,
    pub unit: String, // e.g., "bpm", "mmHg"
    pub timestamp_ms: i64,
    pub created_by: String,
}

impl Timestamped for Vital {
    fn timestamp_millis(&self) -> i64 {
        self.timestamp_ms
    }
}

impl Auditable for Vital {
    fn created_by(&self) -> &str {
        &self.created_by
    }
    fn created_at_millis(&self) -> i64 {
        self.timestamp_ms
    }
}

impl Validatable for Vital {
    fn validate(&self) -> Result<(), ValidationError> {
        if self.name.trim().is_empty() {
            return Err(ValidationError::EmptyField { field: "name" });
        }
        if self.unit.trim().is_empty() {
            return Err(ValidationError::EmptyField { field: "unit" });
        }
        if !self.timestamp_ms.is_positive() {
            return Err(ValidationError::InvalidFormat {
                field: "timestamp_ms",
                message: "must be a positive epoch millis",
            });
        }
        Ok(())
    }
}

impl PrivacyProtected for Vital {
    fn privacy_annotation(&self) -> PrivacyAnnotation {
        PrivacyAnnotation::PHI
    }
}

impl Serializable for Vital {
    fn serialize(&self, format: HealthcareFormat) -> String {
        match format {
            HealthcareFormat::Json | HealthcareFormat::FhirJson => format!(
                "{{\"name\":\"{}\",\"value\":{},\"unit\":\"{}\",\"timestamp_ms\":{},\"created_by\":\"{}\"}}",
                self.name, self.value, self.unit, self.timestamp_ms, self.created_by
            ),
            HealthcareFormat::Hl7V2 => format!(
                "OBX|1|NM|{}||{}|{}|||F|||{}|{}",
                self.name, self.value, self.unit, self.created_by, self.timestamp_ms
            ),
            HealthcareFormat::Dicom => format!(
                "DICOM:VitalName={};Value={};Unit={};Time={}",
                self.name, self.value, self.unit, self.timestamp_ms
            ),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Diagnosis {
    pub code: String, // ICD-10 or SNOMED
    pub description: String,
    pub created_by: String,
    pub created_at_ms: i64,
}

impl Auditable for Diagnosis {
    fn created_by(&self) -> &str {
        &self.created_by
    }
    fn created_at_millis(&self) -> i64 {
        self.created_at_ms
    }
}

impl Validatable for Diagnosis {
    fn validate(&self) -> Result<(), ValidationError> {
        if self.code.trim().is_empty() {
            return Err(ValidationError::EmptyField { field: "code" });
        }
        if self.description.trim().is_empty() {
            return Err(ValidationError::EmptyField {
                field: "description",
            });
        }
        if !self.created_at_ms.is_positive() {
            return Err(ValidationError::InvalidFormat {
                field: "created_at_ms",
                message: "must be positive epoch millis",
            });
        }
        Ok(())
    }

    fn validate_with_ctx(&self, ctx: &ValidationCtx) -> Result<(), ValidationError> {
        self.validate()?;
        // Treat Diagnosis code as SNOMED for validation purposes here
        if !ctx.is_valid_snomed(&self.code) {
            return Err(ValidationError::UnknownCode {
                system: "SNOMED",
                code: self.code.clone(),
            });
        }
        Ok(())
    }
}

impl PrivacyProtected for Diagnosis {
    fn privacy_annotation(&self) -> PrivacyAnnotation {
        PrivacyAnnotation::PHI
    }
}

impl Serializable for Diagnosis {
    fn serialize(&self, format: HealthcareFormat) -> String {
        match format {
            HealthcareFormat::Json | HealthcareFormat::FhirJson => format!(
                "{{\"code\":\"{}\",\"description\":\"{}\",\"created_by\":\"{}\",\"created_at_ms\":{}}}",
                self.code, self.description, self.created_by, self.created_at_ms
            ),
            HealthcareFormat::Hl7V2 => format!("DG1|1||{}^{}|||{}", self.code, self.description, self.created_at_ms),
            HealthcareFormat::Dicom => {
                format!("DICOM:DiagnosisCode={};Desc={}", self.code, self.description)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Medication {
    pub name: String,
    pub dose: String, // textual dose (e.g., "5 mg")
    pub route: Option<String>,
    pub created_by: String,
    pub created_at_ms: i64,
}

impl Auditable for Medication {
    fn created_by(&self) -> &str {
        &self.created_by
    }
    fn created_at_millis(&self) -> i64 {
        self.created_at_ms
    }
}

impl Validatable for Medication {
    fn validate(&self) -> Result<(), ValidationError> {
        if self.name.trim().is_empty() {
            return Err(ValidationError::EmptyField { field: "name" });
        }
        if self.dose.trim().is_empty() {
            return Err(ValidationError::EmptyField { field: "dose" });
        }
        if !self.created_at_ms.is_positive() {
            return Err(ValidationError::InvalidFormat {
                field: "created_at_ms",
                message: "must be positive epoch millis",
            });
        }
        Ok(())
    }
}

impl PrivacyProtected for Medication {
    fn privacy_annotation(&self) -> PrivacyAnnotation {
        PrivacyAnnotation::PHI
    }
}

impl Serializable for Medication {
    fn serialize(&self, format: HealthcareFormat) -> String {
        match format {
            HealthcareFormat::Json | HealthcareFormat::FhirJson => format!(
                "{{\"name\":\"{}\",\"dose\":\"{}\",\"route\":{},\"created_by\":\"{}\",\"created_at_ms\":{}}}",
                self.name,
                self.dose,
                self.route
                    .as_ref()
                    .map(|r| format!("\"{r}\""))
                    .unwrap_or_else(|| "null".to_string()),
                self.created_by,
                self.created_at_ms
            ),
            HealthcareFormat::Hl7V2 => {
                let route = self.route.as_deref().unwrap_or("");
                format!("RXE|{}|{}|{}", self.name, self.dose, route)
            }
            HealthcareFormat::Dicom => {
                format!("DICOM:MedicationName={};Dose={}", self.name, self.dose)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MedicalRecord {
    pub patient_id: PatientID,
    pub diagnoses: Vec<Diagnosis>,
    pub medications: Vec<Medication>,
    pub labs: Vec<LabResult>,
    pub created_by: String,
    pub created_at_ms: i64,
}

impl Identifiable for MedicalRecord {
    fn id(&self) -> &str {
        &self.patient_id.0
    }
}

impl Timestamped for MedicalRecord {
    fn timestamp_millis(&self) -> i64 {
        self.created_at_ms
    }
}

impl Auditable for MedicalRecord {
    fn created_by(&self) -> &str {
        &self.created_by
    }
    fn created_at_millis(&self) -> i64 {
        self.created_at_ms
    }
}

impl Validatable for MedicalRecord {
    fn validate(&self) -> Result<(), ValidationError> {
        if self.patient_id.0.trim().is_empty() {
            return Err(ValidationError::EmptyField {
                field: "patient_id",
            });
        }
        // Basic sanity: at least one clinical component exists
        if self.diagnoses.is_empty() && self.medications.is_empty() && self.labs.is_empty() {
            return Err(ValidationError::InvariantViolation(
                "medical record must contain at least one of: diagnosis, medication, or lab result",
            ));
        }
        Ok(())
    }
}

impl PrivacyProtected for MedicalRecord {
    fn privacy_annotation(&self) -> PrivacyAnnotation {
        PrivacyAnnotation::PHI
    }
}

impl Serializable for MedicalRecord {
    fn serialize(&self, format: HealthcareFormat) -> String {
        match format {
            HealthcareFormat::Json | HealthcareFormat::FhirJson => {
                let diagnoses = self
                    .diagnoses
                    .iter()
                    .map(|d| d.serialize(HealthcareFormat::Json))
                    .collect::<Vec<_>>()
                    .join(",");
                let medications = self
                    .medications
                    .iter()
                    .map(|m| m.serialize(HealthcareFormat::Json))
                    .collect::<Vec<_>>()
                    .join(",");
                let labs = self
                    .labs
                    .iter()
                    .map(|l| l.serialize(HealthcareFormat::Json))
                    .collect::<Vec<_>>()
                    .join(",");

                format!(
                    "{{\"patient_id\":\"{}\",\"diagnoses\":[{}],\"medications\":[{}],\"labs\":[{}],\"created_by\":\"{}\",\"created_at_ms\":{}}}",
                    self.patient_id.0,
                    diagnoses,
                    medications,
                    labs,
                    self.created_by,
                    self.created_at_ms
                )
            }
            HealthcareFormat::Hl7V2 => {
                let mut out = Vec::new();
                out.push(self.patient_id.serialize(HealthcareFormat::Hl7V2));
                out.extend(
                    self.diagnoses
                        .iter()
                        .map(|d| d.serialize(HealthcareFormat::Hl7V2)),
                );
                out.extend(
                    self.medications
                        .iter()
                        .map(|m| m.serialize(HealthcareFormat::Hl7V2)),
                );
                out.extend(
                    self.labs
                        .iter()
                        .map(|l| l.serialize(HealthcareFormat::Hl7V2)),
                );
                out.join("\n")
            }
            HealthcareFormat::Dicom => {
                format!("DICOM:MedicalRecord.PatientID={}", self.patient_id.0)
            }
        }
    }
}

impl MedicalRecordTrait for MedicalRecord {
    fn patient_id(&self) -> &PatientID {
        &self.patient_id
    }

    fn diagnoses(&self) -> &[Diagnosis] {
        &self.diagnoses
    }

    fn medications(&self) -> &[Medication] {
        &self.medications
    }

    fn labs(&self) -> &[LabResult] {
        &self.labs
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn medical_record_trait_has_clinical_data() {
        let mr = MedicalRecord {
            patient_id: PatientID("p1".into()),
            diagnoses: vec![],
            medications: vec![],
            labs: vec![],
            created_by: "dr.jones".into(),
            created_at_ms: 1,
        };
        assert!(!mr.has_clinical_data());

        let mr2 = MedicalRecord {
            diagnoses: vec![Diagnosis {
                code: "E11.9".into(),
                description: "Type 2 diabetes".into(),
                created_by: "dr.jones".into(),
                created_at_ms: 1,
            }],
            ..mr
        };
        assert!(mr2.has_clinical_data());
    }

    #[test]
    fn audit_record_extracts_metadata() {
        let mr = MedicalRecord {
            patient_id: PatientID("p1".into()),
            diagnoses: vec![Diagnosis {
                code: "E11.9".into(),
                description: "Type 2 diabetes".into(),
                created_by: "dr.jones".into(),
                created_at_ms: 1,
            }],
            medications: vec![],
            labs: vec![],
            created_by: "dr.smith".into(),
            created_at_ms: 42,
        };

        let audit = mr.audit_record();
        assert_eq!(audit.created_by, "dr.smith");
        assert_eq!(audit.created_at_millis, 42);
    }

    #[test]
    fn medical_record_serializes_to_json() {
        let mr = MedicalRecord {
            patient_id: PatientID("p1".into()),
            diagnoses: vec![Diagnosis {
                code: "E11.9".into(),
                description: "Type 2 diabetes".into(),
                created_by: "dr.jones".into(),
                created_at_ms: 1,
            }],
            medications: vec![Medication {
                name: "Metformin".into(),
                dose: "500 mg".into(),
                route: Some("oral".into()),
                created_by: "dr.jones".into(),
                created_at_ms: 1,
            }],
            labs: vec![],
            created_by: "dr.smith".into(),
            created_at_ms: 42,
        };

        let s = mr.serialize(HealthcareFormat::Json);
        assert!(s.contains("\"patient_id\":\"p1\""));
        assert!(s.contains("\"diagnoses\":"));
        assert!(s.contains("\"medications\":"));
    }

    #[test]
    fn lab_result_validates() {
        let lr = LabResult {
            code: "2345-7".into(),
            value: 95.0,
            unit: "mg/dL".into(),
            timestamp_ms: 1_700_000_000_000,
            created_by: "dr.smith".into(),
        };
        assert!(lr.validate().is_ok());
    }

    #[test]
    fn diagnosis_requires_code_and_description() {
        let dx = Diagnosis {
            code: "".into(),
            description: "Type 2 diabetes".into(),
            created_by: "dr.jones".into(),
            created_at_ms: 1,
        };
        assert!(dx.validate().is_err());
        let dx2 = Diagnosis {
            code: "E11.9".into(),
            description: "".into(),
            created_by: "dr.jones".into(),
            created_at_ms: 1,
        };
        assert!(dx2.validate().is_err());
    }

    #[test]
    fn vital_requires_name_unit_and_positive_timestamp() {
        let v = Vital {
            name: "heart_rate".into(),
            value: 72.0,
            unit: "bpm".into(),
            timestamp_ms: 1,
            created_by: "nurse".into(),
        };
        assert!(v.validate().is_ok());

        let bad = Vital {
            name: "".into(),
            value: 98.6,
            unit: "F".into(),
            timestamp_ms: 1,
            created_by: "nurse".into(),
        };
        assert!(bad.validate().is_err());
    }

    #[test]
    fn medication_requires_name_and_dose() {
        let med = Medication {
            name: "Metformin".into(),
            dose: "500 mg".into(),
            route: Some("oral".into()),
            created_by: "dr.jones".into(),
            created_at_ms: 1,
        };
        assert!(med.validate().is_ok());
        let bad = Medication {
            name: "".into(),
            dose: "".into(),
            route: None,
            created_by: "dr.jones".into(),
            created_at_ms: 1,
        };
        assert!(bad.validate().is_err());
    }

    #[test]
    fn medical_record_requires_patient_and_content() {
        let lr = LabResult {
            code: "2345-7".into(),
            value: 95.0,
            unit: "mg/dL".into(),
            timestamp_ms: 1,
            created_by: "dr".into(),
        };
        let mr = MedicalRecord {
            patient_id: PatientID("123".into()),
            diagnoses: vec![],
            medications: vec![],
            labs: vec![lr],
            created_by: "clerk".into(),
            created_at_ms: 2,
        };
        assert!(mr.validate().is_ok());

        let empty = MedicalRecord {
            patient_id: PatientID("".into()),
            diagnoses: vec![],
            medications: vec![],
            labs: vec![],
            created_by: "clerk".into(),
            created_at_ms: 2,
        };
        assert!(empty.validate().is_err());
    }

    #[test]
    fn patient_id_non_empty() {
        assert!(PatientID("abc".into()).validate().is_ok());
        assert!(PatientID("".into()).validate().is_err());
    }

    #[test]
    fn lab_result_validate_with_ctx_checks_loinc_ucum_and_ranges() {
        let ctx = ValidationCtx::with_demo_data();

        // Unknown LOINC -> UnknownCode
        let lr_unknown = LabResult {
            code: "9999-9".into(),
            value: 90.0,
            unit: "mg/dL".into(),
            timestamp_ms: 1,
            created_by: "dr".into(),
        };
        let err = lr_unknown.validate_with_ctx(&ctx).unwrap_err();
        assert!(matches!(
            err,
            ValidationError::UnknownCode {
                system: "LOINC",
                ..
            }
        ));

        // Invalid UCUM -> InvalidUnit
        let lr_bad_unit = LabResult {
            code: "2345-7".into(),
            value: 90.0,
            unit: "weird".into(),
            timestamp_ms: 1,
            created_by: "dr".into(),
        };
        let err = lr_bad_unit.validate_with_ctx(&ctx).unwrap_err();
        assert!(matches!(err, ValidationError::InvalidUnit { .. }));

        // Not preferred unit (exists in UCUM but not mapped for code) -> InvalidFormat("unit not preferred...")
        // Use an allowed UCUM like "g/dL" that's not in the preferred set for 2345-7
        let lr_not_preferred = LabResult {
            code: "2345-7".into(),
            value: 90.0,
            unit: "g/dL".into(),
            timestamp_ms: 1,
            created_by: "dr".into(),
        };
        let err = lr_not_preferred.validate_with_ctx(&ctx).unwrap_err();
        match err {
            ValidationError::InvalidFormat { field, message } => {
                assert_eq!(field, "unit");
                assert!(message.contains("preferred"));
            }
            other => panic!("unexpected error: {other:?}"),
        }

        // Out of reference range -> OutOfReferenceRange
        let lr_oor = LabResult {
            code: "2345-7".into(),
            value: 200.0,
            unit: "mg/dL".into(),
            timestamp_ms: 1,
            created_by: "dr".into(),
        };
        let err = lr_oor.validate_with_ctx(&ctx).unwrap_err();
        assert!(
            matches!(err, ValidationError::OutOfReferenceRange { code, unit, .. } if code == "2345-7" && unit == "mg/dL")
        );

        // In range and preferred unit -> Ok
        let lr_ok = LabResult {
            code: "2345-7".into(),
            value: 90.0,
            unit: "mg/dL".into(),
            timestamp_ms: 1,
            created_by: "dr".into(),
        };
        assert!(lr_ok.validate_with_ctx(&ctx).is_ok());
    }

    #[test]
    fn diagnosis_validate_with_ctx_checks_snomed() {
        let ctx = ValidationCtx::with_demo_data();
        // Invalid
        let dx_bad = Diagnosis {
            code: "000000".into(),
            description: "bad".into(),
            created_by: "dr".into(),
            created_at_ms: 1,
        };
        let err = dx_bad.validate_with_ctx(&ctx).unwrap_err();
        assert!(matches!(
            err,
            ValidationError::UnknownCode {
                system: "SNOMED",
                ..
            }
        ));

        // Valid
        let dx_ok = Diagnosis {
            code: "38341003".into(),
            description: "HTN".into(),
            created_by: "dr".into(),
            created_at_ms: 1,
        };
        assert!(dx_ok.validate_with_ctx(&ctx).is_ok());
    }
}
