use crate::fhir::FHIRMedicalEvent;
use crate::fhir::{FHIRObservation, FHIRPatient};
use crate::fhir_any::FHIRAny;

#[derive(Debug, Clone)]
pub enum Predicate {
    Eq { key: String, value: String },
    Contains { key: String, value: String },
    EqCi { key: String, value: String },
    ContainsCi { key: String, value: String },
    Gt { key: String, value: f64 },
    Ge { key: String, value: f64 },
    Lt { key: String, value: f64 },
    Le { key: String, value: f64 },
    Between { key: String, min: f64, max: f64 },
    DateGe { key: String, value_yyyymmdd: String },
    DateLe { key: String, value_yyyymmdd: String },
    Any(Vec<Predicate>), // OR within the same group
}

#[derive(Debug, Clone)]
pub struct QueryBuilder {
    resource_type: String,
    // OR-of-ANDs: any group passing means match; inside each group all predicates must pass
    groups: Vec<Vec<Predicate>>, // at least one group always exists
}

impl QueryBuilder {
    pub fn new(resource_type: impl Into<String>) -> Self {
        Self {
            resource_type: resource_type.into(),
            groups: vec![Vec::new()],
        }
    }

    pub fn filter_eq(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.groups.last_mut().unwrap().push(Predicate::Eq {
            key: key.into(),
            value: value.into(),
        });
        self
    }

    pub fn filter_contains(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.groups.last_mut().unwrap().push(Predicate::Contains {
            key: key.into(),
            value: value.into(),
        });
        self
    }

    pub fn filter_contains_ci(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.groups.last_mut().unwrap().push(Predicate::ContainsCi {
            key: key.into(),
            value: value.into(),
        });
        self
    }

    pub fn filter_eq_ci(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.groups.last_mut().unwrap().push(Predicate::EqCi {
            key: key.into(),
            value: value.into(),
        });
        self
    }

    pub fn filter_gt(mut self, key: impl Into<String>, value: f64) -> Self {
        self.groups.last_mut().unwrap().push(Predicate::Gt {
            key: key.into(),
            value,
        });
        self
    }

    pub fn filter_ge(mut self, key: impl Into<String>, value: f64) -> Self {
        self.groups.last_mut().unwrap().push(Predicate::Ge {
            key: key.into(),
            value,
        });
        self
    }

    pub fn filter_lt(mut self, key: impl Into<String>, value: f64) -> Self {
        self.groups.last_mut().unwrap().push(Predicate::Lt {
            key: key.into(),
            value,
        });
        self
    }

    pub fn filter_le(mut self, key: impl Into<String>, value: f64) -> Self {
        self.groups.last_mut().unwrap().push(Predicate::Le {
            key: key.into(),
            value,
        });
        self
    }

    pub fn filter_between(mut self, key: impl Into<String>, min: f64, max: f64) -> Self {
        self.groups.last_mut().unwrap().push(Predicate::Between {
            key: key.into(),
            min,
            max,
        });
        self
    }

    pub fn filter_date_ge(
        mut self,
        key: impl Into<String>,
        value_yyyy_mm_dd_or_yyyymmdd: impl Into<String>,
    ) -> Self {
        let normalized = normalize_date(value_yyyy_mm_dd_or_yyyymmdd.into());
        self.groups.last_mut().unwrap().push(Predicate::DateGe {
            key: key.into(),
            value_yyyymmdd: normalized,
        });
        self
    }

    pub fn filter_date_le(
        mut self,
        key: impl Into<String>,
        value_yyyy_mm_dd_or_yyyymmdd: impl Into<String>,
    ) -> Self {
        let normalized = normalize_date(value_yyyy_mm_dd_or_yyyymmdd.into());
        self.groups.last_mut().unwrap().push(Predicate::DateLe {
            key: key.into(),
            value_yyyymmdd: normalized,
        });
        self
    }

    pub fn or_group(mut self) -> Self {
        self.groups.push(Vec::new());
        self
    }

    pub fn any_in_group(mut self, preds: Vec<Predicate>) -> Self {
        self.groups.last_mut().unwrap().push(Predicate::Any(preds));
        self
    }

    pub fn filter_family_name_ci_contains(mut self, value: impl Into<String>) -> Self {
        self.groups.last_mut().unwrap().push(Predicate::ContainsCi {
            key: "family_name".into(),
            value: value.into(),
        });
        self
    }

    pub fn filter_given_name_ci_eq(mut self, value: impl Into<String>) -> Self {
        self.groups.last_mut().unwrap().push(Predicate::EqCi {
            key: "given_name".into(),
            value: value.into(),
        });
        self
    }

    pub fn filter_event_code_ci_contains(mut self, value: impl Into<String>) -> Self {
        self.groups.last_mut().unwrap().push(Predicate::ContainsCi {
            key: "code".into(),
            value: value.into(),
        });
        self
    }

    pub fn filter_start_date_ge(mut self, value_yyyy_mm_dd_or_yyyymmdd: impl Into<String>) -> Self {
        let normalized = normalize_date(value_yyyy_mm_dd_or_yyyymmdd.into());
        self.groups.last_mut().unwrap().push(Predicate::DateGe {
            key: "start_date".into(),
            value_yyyymmdd: normalized,
        });
        self
    }

    pub fn filter_start_date_le(mut self, value_yyyy_mm_dd_or_yyyymmdd: impl Into<String>) -> Self {
        let normalized = normalize_date(value_yyyy_mm_dd_or_yyyymmdd.into());
        self.groups.last_mut().unwrap().push(Predicate::DateLe {
            key: "start_date".into(),
            value_yyyymmdd: normalized,
        });
        self
    }

    pub fn build(self) -> Query {
        Query {
            resource_type: self.resource_type,
            groups: self.groups,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Query {
    pub resource_type: String,
    pub groups: Vec<Vec<Predicate>>, // OR-of-ANDs
}

impl Query {
    fn matches_str(preds: &[Predicate], key: &str, candidate: Option<&str>) -> bool {
        for p in preds {
            match p {
                Predicate::Eq { key: k, value } if k == key => {
                    if candidate != Some(value.as_str()) {
                        return false;
                    }
                }
                Predicate::Contains { key: k, value } if k == key => {
                    if let Some(c) = candidate {
                        if !c.contains(value) {
                            return false;
                        }
                    } else {
                        return false;
                    }
                }
                Predicate::EqCi { key: k, value } if k == key => {
                    if let Some(c) = candidate {
                        if !c.eq_ignore_ascii_case(value) {
                            return false;
                        }
                    } else {
                        return false;
                    }
                }
                Predicate::ContainsCi { key: k, value } if k == key => {
                    if let Some(c) = candidate {
                        if !c.to_ascii_lowercase().contains(&value.to_ascii_lowercase()) {
                            return false;
                        }
                    } else {
                        return false;
                    }
                }
                _ => {}
            }
        }
        true
    }

    fn matches_f64(preds: &[Predicate], key: &str, candidate: Option<f64>) -> bool {
        for p in preds {
            match p {
                Predicate::Gt { key: k, value } if k == key => {
                    if !candidate.map(|v| v > *value).unwrap_or(false) {
                        return false;
                    }
                }
                Predicate::Ge { key: k, value } if k == key => {
                    if !candidate.map(|v| v >= *value).unwrap_or(false) {
                        return false;
                    }
                }
                Predicate::Lt { key: k, value } if k == key => {
                    if !candidate.map(|v| v < *value).unwrap_or(false) {
                        return false;
                    }
                }
                Predicate::Le { key: k, value } if k == key => {
                    if !candidate.map(|v| v <= *value).unwrap_or(false) {
                        return false;
                    }
                }
                Predicate::Between { key: k, min, max } if k == key => {
                    if !candidate.map(|v| v >= *min && v <= *max).unwrap_or(false) {
                        return false;
                    }
                }
                Predicate::Eq { key: k, value } if k == key => {
                    // Allow equality on numeric by parsing value
                    if let Ok(num) = value.parse::<f64>() {
                        if !candidate
                            .map(|v| (v - num).abs() < f64::EPSILON)
                            .unwrap_or(false)
                        {
                            return false;
                        }
                    }
                }
                _ => {}
            }
        }
        true
    }

    fn matches_date(preds: &[Predicate], key: &str, candidate: Option<&str>) -> bool {
        if !preds.iter().any(|p| {
            matches!(p, Predicate::DateGe{key: k, ..} if k==key)
                || matches!(p, Predicate::DateLe{key: k, ..} if k==key)
        }) {
            return true;
        }
        let cand_norm = candidate.map(|s| normalize_date(s.to_string()));
        for p in preds {
            match p {
                Predicate::DateGe {
                    key: k,
                    value_yyyymmdd,
                } if k == key => {
                    if !cand_norm
                        .as_deref()
                        .map(|c| c >= value_yyyymmdd.as_str())
                        .unwrap_or(false)
                    {
                        return false;
                    }
                }
                Predicate::DateLe {
                    key: k,
                    value_yyyymmdd,
                } if k == key => {
                    if !cand_norm
                        .as_deref()
                        .map(|c| c <= value_yyyymmdd.as_str())
                        .unwrap_or(false)
                    {
                        return false;
                    }
                }
                _ => {}
            }
        }
        true
    }

    pub fn execute_patients<'a>(&self, data: &'a [FHIRPatient]) -> Vec<&'a FHIRPatient> {
        data.iter()
            .filter(|p| {
                // OR-of-ANDs across groups
                self.groups.iter().any(|group| {
                    let base = Self::matches_str(group, "id", Some(p.id.as_str()))
                        && Self::matches_str(group, "given_name", p.given_name.as_deref())
                        && Self::matches_str(group, "family_name", p.family_name.as_deref())
                        && Self::matches_date(group, "birth_date", p.birth_date.as_deref());
                    base && Self::matches_any(group, p)
                })
            })
            .collect()
    }

    pub fn execute_observations<'a>(
        &self,
        data: &'a [FHIRObservation],
    ) -> Vec<&'a FHIRObservation> {
        data.iter()
            .filter(|o| {
                self.groups.iter().any(|group| {
                    let base = Self::matches_str(group, "id", Some(o.id.as_str()))
                        && Self::matches_str(group, "code", Some(o.code.as_str()))
                        && Self::matches_str(group, "unit", o.unit.as_deref())
                        && Self::matches_f64(group, "value", o.value);
                    base && Self::matches_any_obs(group, o)
                })
            })
            .collect()
    }

    pub fn execute_medical_events<'a>(
        &self,
        data: &'a [FHIRMedicalEvent],
    ) -> Vec<&'a FHIRMedicalEvent> {
        data.iter()
            .filter(|e| {
                self.groups.iter().any(|group| {
                    let base = Self::matches_str(group, "id", Some(e.id.as_str()))
                        && Self::matches_str(group, "code", Some(e.code.as_str()))
                        && Self::matches_str(group, "description", e.description.as_deref())
                        && Self::matches_date(group, "start_date", e.start_date.as_deref());
                    base
                })
            })
            .collect()
    }

    pub fn execute_any<'a>(&self, data: &'a [FHIRAny]) -> Vec<&'a FHIRAny> {
        data.iter()
            .filter(|item| match item {
                FHIRAny::Patient(p) => self.groups.iter().any(|group| {
                    let base = Self::matches_str(group, "id", Some(p.id.as_str()))
                        && Self::matches_str(group, "given_name", p.given_name.as_deref())
                        && Self::matches_str(group, "family_name", p.family_name.as_deref())
                        && Self::matches_date(group, "birth_date", p.birth_date.as_deref());
                    base && Self::matches_any(group, p)
                }),
                FHIRAny::Observation(o) => self.groups.iter().any(|group| {
                    let base = Self::matches_str(group, "id", Some(o.id.as_str()))
                        && Self::matches_str(group, "code", Some(o.code.as_str()))
                        && Self::matches_str(group, "unit", o.unit.as_deref())
                        && Self::matches_f64(group, "value", o.value);
                    base && Self::matches_any_obs(group, o)
                }),
                FHIRAny::MedicalEvent(e) => self.groups.iter().any(|group| {
                    let base = Self::matches_str(group, "id", Some(e.id.as_str()))
                        && Self::matches_str(group, "code", Some(e.code.as_str()))
                        && Self::matches_str(group, "description", e.description.as_deref())
                        && Self::matches_date(group, "start_date", e.start_date.as_deref());
                    base
                }),
            })
            .collect()
    }
}

pub fn fhir_query(resource_type: &str) -> QueryBuilder {
    QueryBuilder::new(resource_type)
}

fn normalize_date(mut s: String) -> String {
    if s.len() == 10 && s.as_bytes()[4] == b'-' && s.as_bytes()[7] == b'-' {
        s.retain(|c| c != '-');
        return s;
    }
    s
}

impl Query {
    // Evaluate Any(Vec<Predicate>) as OR within group for Patients
    fn matches_any(group: &[Predicate], p: &FHIRPatient) -> bool {
        for pred in group {
            if let Predicate::Any(inner) = pred {
                let any_ok = inner.iter().any(|ip| match ip {
                    Predicate::Eq { key, .. }
                    | Predicate::Contains { key, .. }
                    | Predicate::EqCi { key, .. }
                    | Predicate::ContainsCi { key, .. } => {
                        let val = match key.as_str() {
                            "id" => Some(p.id.as_str()),
                            "given_name" => p.given_name.as_deref(),
                            "family_name" => p.family_name.as_deref(),
                            _ => None,
                        };
                        Query::matches_str(std::slice::from_ref(ip), key, val)
                    }
                    Predicate::DateGe { key, .. } | Predicate::DateLe { key, .. } => {
                        let val = match key.as_str() {
                            "birth_date" => p.birth_date.as_deref(),
                            _ => None,
                        };
                        Query::matches_date(std::slice::from_ref(ip), key, val)
                    }
                    _ => true,
                });
                if !any_ok {
                    return false;
                }
            }
        }
        true
    }

    // Evaluate Any(Vec<Predicate>) as OR within group for Observations
    fn matches_any_obs(group: &[Predicate], o: &FHIRObservation) -> bool {
        for pred in group {
            if let Predicate::Any(inner) = pred {
                let any_ok = inner.iter().any(|ip| match ip {
                    Predicate::Eq { key, .. }
                    | Predicate::Contains { key, .. }
                    | Predicate::EqCi { key, .. }
                    | Predicate::ContainsCi { key, .. } => {
                        let val = match key.as_str() {
                            "id" => Some(o.id.as_str()),
                            "code" => Some(o.code.as_str()),
                            "unit" => o.unit.as_deref(),
                            _ => None,
                        };
                        Query::matches_str(std::slice::from_ref(ip), key, val)
                    }
                    Predicate::Between { key, .. }
                    | Predicate::Gt { key, .. }
                    | Predicate::Ge { key, .. }
                    | Predicate::Lt { key, .. }
                    | Predicate::Le { key, .. } => {
                        let val = match key.as_str() {
                            "value" => o.value,
                            _ => None,
                        };
                        Query::matches_f64(std::slice::from_ref(ip), key, val)
                    }
                    _ => true,
                });
                if !any_ok {
                    return false;
                }
            }
        }
        true
    }
}
