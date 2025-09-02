// Healthcare domain traits for Medic type system

/// Provides a stable identifier for domain entities (e.g., PatientID)
pub trait Identifiable {
    fn id(&self) -> &str;
}

/// Provides timestamp semantics in epoch milliseconds (UTC)
pub trait Timestamped {
    fn timestamp_millis(&self) -> i64;
}

/// Basic audit metadata common to healthcare records
pub trait Auditable {
    fn created_by(&self) -> &str;
    fn created_at_millis(&self) -> i64;
}

/// Context carrying lightweight in-memory registries for UCUM, LOINC, SNOMED, and reference ranges.
#[derive(Debug, Clone, Default)]
pub struct ValidationCtx {
    /// Known valid UCUM units (e.g., "mg/dL", "mmHg", "bpm")
    pub ucum_units: std::collections::HashSet<String>,
    /// Known LOINC codes and their preferred UCUM units
    pub loinc_to_units: std::collections::HashMap<String, std::collections::HashSet<String>>,
    /// Known SNOMED codes
    pub snomed_codes: std::collections::HashSet<String>,
    /// Reference ranges keyed by (code, unit) -> (min, max)
    pub reference_ranges: std::collections::HashMap<(String, String), (f64, f64)>,
    /// Known contraindicated medication pairs (stored in case-insensitive, sorted order)
    pub contraindicated_meds: std::collections::HashSet<(String, String)>,
}

impl ValidationCtx {
    /// Construct a small demo ctx with a few common entries. Suitable for tests.
    pub fn with_demo_data() -> Self {
        use std::collections::{HashMap, HashSet};
        let mut ucum_units: HashSet<String> =
            ["mg/dL", "mmHg", "bpm", "g/dL", "%", "kg", "cm", "mmol/L"]
                .into_iter()
                .map(|s| s.to_string())
                .collect();
        // Include some case variants sometimes found in data
        ucum_units.insert("MG/DL".to_string());

        let mut loinc_to_units: HashMap<String, HashSet<String>> = HashMap::new();
        loinc_to_units.insert(
            "2345-7".to_string(), // Glucose [Mass/volume] in Serum or Plasma
            ["mg/dL", "mmol/L"]
                .into_iter()
                .map(|s| s.to_string())
                .collect(),
        );

        let snomed_codes: HashSet<String> = [
            "44054006", // Diabetes mellitus type 2 (disorder)
            "38341003", // Hypertension
        ]
        .into_iter()
        .map(|s| s.to_string())
        .collect();

        let mut reference_ranges: HashMap<(String, String), (f64, f64)> = HashMap::new();
        reference_ranges.insert(("2345-7".into(), "mg/dL".into()), (70.0, 99.0));
        reference_ranges.insert(("2345-7".into(), "mmol/L".into()), (3.9, 5.5));

        // Demo contraindicated medication pairs
        let mut contraindicated_meds: HashSet<(String, String)> = HashSet::new();
        // Store pairs sorted case-insensitively for easy lookup
        let norm_pair = |a: &str, b: &str| -> (String, String) {
            let a_l = a.to_ascii_lowercase();
            let b_l = b.to_ascii_lowercase();
            if a_l <= b_l {
                (a_l, b_l)
            } else {
                (b_l, a_l)
            }
        };
        contraindicated_meds.insert(norm_pair("warfarin", "ibuprofen"));
        contraindicated_meds.insert(norm_pair("nitroglycerin", "sildenafil"));

        ValidationCtx {
            ucum_units,
            loinc_to_units,
            snomed_codes,
            reference_ranges,
            contraindicated_meds,
        }
    }

    pub fn is_valid_ucum(&self, unit: &str) -> bool {
        if unit.is_empty() {
            return false;
        }
        self.ucum_units.contains(unit) || self.ucum_units.contains(&unit.to_ascii_lowercase())
    }

    pub fn is_valid_loinc(&self, code: &str) -> bool {
        self.loinc_to_units.contains_key(code)
    }

    pub fn is_valid_snomed(&self, code: &str) -> bool {
        self.snomed_codes.contains(code)
    }

    /// Returns the reference range for a (code, unit) if known.
    pub fn reference_range(&self, code: &str, unit: &str) -> Option<(f64, f64)> {
        self.reference_ranges
            .get(&(code.to_string(), unit.to_string()))
            .cloned()
    }

    /// Returns true if the two medication names form a known contraindicated pair.
    /// Matching is case-insensitive and order-insensitive.
    pub fn is_contraindicated_meds(&self, a: &str, b: &str) -> bool {
        let a_l = a.to_ascii_lowercase();
        let b_l = b.to_ascii_lowercase();
        let key = if a_l <= b_l { (a_l, b_l) } else { (b_l, a_l) };
        self.contraindicated_meds.contains(&key)
    }
}

/// Simple validation trait used by domain types
pub trait Validatable {
    /// Legacy validation without external context
    fn validate(&self) -> Result<(), ValidationError>;

    /// Context-aware validation using `ValidationCtx` (UCUM/LOINC/SNOMED ranges, etc.)
    /// Default implementation falls back to `validate()`.
    fn validate_with_ctx(&self, ctx: &ValidationCtx) -> Result<(), ValidationError> {
        let _ = ctx; // default impl does not use the context
        self.validate()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValidationError {
    EmptyField {
        field: &'static str,
    },
    OutOfRange {
        field: &'static str,
        min: f64,
        max: f64,
        actual: f64,
    },
    InvalidFormat {
        field: &'static str,
        message: &'static str,
    },
    InvariantViolation(&'static str),
    /// UCUM unit not recognized
    InvalidUnit {
        unit: String,
    },
    /// Unknown code in a specific code system
    UnknownCode {
        system: &'static str,
        code: String,
    },
    /// Value outside known clinical reference range for (code, unit)
    OutOfReferenceRange {
        code: String,
        unit: String,
        min: f64,
        max: f64,
        actual: f64,
    },
}

impl std::fmt::Display for ValidationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValidationError::EmptyField { field } => write!(f, "'{field}' cannot be empty."),
            ValidationError::OutOfRange {
                field,
                min,
                max,
                actual,
            } => write!(
                f,
                "Field '{field}' value {actual} is out of allowed range [{min}, {max}]."
            ),
            ValidationError::InvalidFormat { field, message } => {
                write!(f, "Field '{field}' has invalid format: {message}.")
            }
            ValidationError::InvariantViolation(msg) => write!(f, "Invariant violation: {msg}."),
            ValidationError::InvalidUnit { unit } => {
                write!(f, "Unit '{unit}' is not a valid UCUM unit.")
            }
            ValidationError::UnknownCode { system, code } => {
                write!(f, "Unknown {system} code '{code}'.")
            }
            ValidationError::OutOfReferenceRange {
                code,
                unit,
                min,
                max,
                actual,
            } => write!(
                f,
                "{code} value {actual} {unit} is outside the reference range [{min}, {max}]."
            ),
        }
    }
}
