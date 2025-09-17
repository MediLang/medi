// Type environment for Medic language in Rust
// Mimics TypeScript's TypeEnv for variable/function scopes

use medic_type::types::{MediType, PrivacyAnnotation};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SinkKind {
    Log,
    Print,
    Export,
    Network,
    File,
}

#[derive(Debug, Clone)]
pub struct TypeEnv {
    parent: Option<Box<TypeEnv>>,
    symbols: HashMap<String, MediType>,
    privacy: HashMap<String, PrivacyAnnotation>,
    // Function metadata for HIPAA/privacy checks
    sinks: HashMap<String, SinkKind>,
    deid_fns: HashSet<String>,
    // Optional per-sink minimum privacy policy (by sink name)
    sink_min_privacy: HashMap<String, PrivacyAnnotation>,
    // Optional per-sink-kind minimum privacy policy (defaults by kind)
    sink_kind_min_privacy: HashMap<SinkKind, PrivacyAnnotation>,
}

impl Default for TypeEnv {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeEnv {
    pub fn new() -> Self {
        Self {
            parent: None,
            symbols: HashMap::new(),
            privacy: HashMap::new(),
            sinks: HashMap::new(),
            deid_fns: HashSet::new(),
            sink_min_privacy: HashMap::new(),
            sink_kind_min_privacy: HashMap::new(),
        }
    }

    /// Create a new environment pre-populated with common primitives and healthcare types.
    pub fn with_prelude() -> Self {
        let mut env = Self::new();
        use medic_type::types::MediType;
        // Primitives
        env.insert("Int".to_string(), MediType::Int);
        env.insert("Float".to_string(), MediType::Float);
        env.insert("Bool".to_string(), MediType::Bool);
        env.insert("String".to_string(), MediType::String);
        // Healthcare domain
        env.insert("PatientId".to_string(), MediType::PatientId);
        env.insert("Vital".to_string(), MediType::Vital);
        env.insert("LabResult".to_string(), MediType::LabResult);
        env.insert("FHIRPatient".to_string(), MediType::FHIRPatient);
        env.insert("Observation".to_string(), MediType::Observation);
        env.insert("Diagnosis".to_string(), MediType::Diagnosis);
        env.insert("Medication".to_string(), MediType::Medication);
        env.insert("MedicalRecord".to_string(), MediType::MedicalRecord);
        // Pre-register common de-identification functions
        env.set_deid_fn("deidentify");
        env.set_deid_fn("anonymize");
        env.set_deid_fn("deidentify_patient");
        env.set_deid_fn("deid");
        // Pre-register common sinks
        env.set_sink_fn("print", SinkKind::Print);
        env.set_sink_fn("println", SinkKind::Print);
        env.set_sink_fn("log", SinkKind::Log);
        env.set_sink_fn("debug", SinkKind::Log);
        env.set_sink_fn("trace", SinkKind::Log);
        env.set_sink_fn("export", SinkKind::Export);
        env.set_sink_fn("writeFile", SinkKind::File);
        env.set_sink_fn("write_file", SinkKind::File);
        env.set_sink_fn("save", SinkKind::File);
        env.set_sink_fn("send_http", SinkKind::Network);
        env.set_sink_fn("http_post", SinkKind::Network);
        env.set_sink_fn("http_get", SinkKind::Network);
        env.set_sink_fn("network_send", SinkKind::Network);
        env.set_sink_fn("send", SinkKind::Network);
        // Opinionated defaults: Network/File require Anonymized at minimum
        env.set_sink_kind_min_privacy(SinkKind::Network, PrivacyAnnotation::Anonymized);
        env.set_sink_kind_min_privacy(SinkKind::File, PrivacyAnnotation::Anonymized);
        // Other kinds left unset by default; projects may configure as needed
        env
    }

    pub fn with_parent(parent: TypeEnv) -> Self {
        TypeEnv {
            parent: Some(Box::new(parent)),
            symbols: HashMap::new(),
            privacy: HashMap::new(),
            sinks: HashMap::new(),
            deid_fns: HashSet::new(),
            sink_min_privacy: HashMap::new(),
            sink_kind_min_privacy: HashMap::new(),
        }
    }

    pub fn insert(&mut self, name: String, typ: MediType) {
        self.symbols.insert(name, typ);
    }

    pub fn get(&self, name: &str) -> Option<&MediType> {
        match self.symbols.get(name) {
            Some(typ) => Some(typ),
            None => self.parent.as_ref().and_then(|p| p.get(name)),
        }
    }

    /// Collect all function symbols (name, MediType) visible in this environment (including parents).
    /// If a symbol is shadowed in a child env, the child's binding wins.
    pub fn collect_function_types(&self) -> Vec<(String, MediType)> {
        use std::collections::HashSet;
        let mut out: Vec<(String, MediType)> = Vec::new();
        let mut seen: HashSet<String> = HashSet::new();

        // Walk chain from child to parent so child bindings shadow
        let mut cur: Option<&TypeEnv> = Some(self);
        while let Some(env) = cur {
            for (k, v) in env.symbols.iter() {
                if seen.contains(k) {
                    continue;
                }
                if let MediType::Function { .. } = v {
                    out.push((k.clone(), v.clone()));
                    seen.insert(k.clone());
                }
            }
            cur = env.parent.as_deref();
        }
        out
    }

    /// Set a privacy label for a symbol in the current scope
    pub fn set_privacy(&mut self, name: String, label: PrivacyAnnotation) {
        self.privacy.insert(name, label);
    }

    /// Get the privacy label for a symbol, searching parent scopes if needed
    pub fn get_privacy(&self, name: &str) -> Option<PrivacyAnnotation> {
        if let Some(p) = self.privacy.get(name) {
            Some(*p)
        } else {
            self.parent.as_ref().and_then(|p| p.get_privacy(name))
        }
    }

    /// Register a function symbol as a sink with an associated kind.
    pub fn set_sink_fn(&mut self, name: impl Into<String>, kind: SinkKind) {
        self.sinks.insert(name.into(), kind);
    }

    /// Returns the sink kind if the function is a registered sink, searching parents.
    pub fn get_sink_fn(&self, name: &str) -> Option<SinkKind> {
        if let Some(k) = self.sinks.get(name) {
            Some(*k)
        } else {
            self.parent.as_ref().and_then(|p| p.get_sink_fn(name))
        }
    }

    /// Register a function as a de-identification routine.
    pub fn set_deid_fn(&mut self, name: impl Into<String>) {
        self.deid_fns.insert(name.into());
    }

    /// Check whether a function is registered as de-id (searching parents).
    pub fn is_deid_fn(&self, name: &str) -> bool {
        if self.deid_fns.contains(name) {
            true
        } else {
            self.parent
                .as_ref()
                .map(|p| p.is_deid_fn(name))
                .unwrap_or(false)
        }
    }

    /// Set a minimum required privacy label for a given sink name.
    pub fn set_sink_min_privacy(&mut self, name: impl Into<String>, min: PrivacyAnnotation) {
        self.sink_min_privacy.insert(name.into(), min);
    }

    /// Get the minimum required privacy label for a given sink name, honoring parent lookup.
    pub fn get_sink_min_privacy(&self, name: &str) -> Option<PrivacyAnnotation> {
        if let Some(m) = self.sink_min_privacy.get(name) {
            return Some(*m);
        }
        if let Some(p) = &self.parent {
            return p.get_sink_min_privacy(name);
        }
        None
    }

    /// Set a default minimum required privacy label for a given sink kind.
    pub fn set_sink_kind_min_privacy(&mut self, kind: SinkKind, min: PrivacyAnnotation) {
        self.sink_kind_min_privacy.insert(kind, min);
    }

    /// Get default minimum required privacy for a sink kind, honoring parent lookup.
    pub fn get_sink_kind_min_privacy(&self, kind: SinkKind) -> Option<PrivacyAnnotation> {
        if let Some(m) = self.sink_kind_min_privacy.get(&kind) {
            return Some(*m);
        }
        if let Some(p) = &self.parent {
            return p.get_sink_kind_min_privacy(kind);
        }
        None
    }
}
