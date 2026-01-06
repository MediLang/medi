use std::collections::BTreeMap;
use std::fmt;

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
pub struct Manifest {
    pub name: String,
    pub version: String,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,

    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub authors: Vec<String>,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub license: Option<String>,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub edition: Option<String>,

    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub dependencies: BTreeMap<String, Dependency>,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub fhir: Option<FhirConfig>,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
#[serde(untagged)]
pub enum Dependency {
    Version(String),
    Detailed(DetailedDependency),
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
pub struct DetailedDependency {
    pub version: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub path: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub git: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub branch: Option<String>,
    #[serde(default)]
    pub optional: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize, Default)]
pub struct FhirConfig {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub version: Option<String>,
    #[serde(default)]
    pub strict: bool,
}

impl Manifest {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            version: "0.1.0".to_string(),
            description: None,
            authors: Vec::new(),
            license: None,
            edition: Some("2025".to_string()),
            dependencies: BTreeMap::new(),
            fhir: None,
        }
    }

    pub fn to_toml(&self) -> Result<String, toml::ser::Error> {
        toml::to_string_pretty(self)
    }
}

impl fmt::Display for Manifest {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} v{}", self.name, self.version)
    }
}

pub fn parse_manifest(text: &str) -> Result<Manifest, toml::de::Error> {
    toml::from_str::<Manifest>(text)
}

pub fn generate_manifest(name: &str, description: Option<&str>) -> String {
    let mut manifest = Manifest::new(name);
    manifest.description = description.map(|s| s.to_string());

    let mut out = String::new();
    out.push_str(&format!("name = \"{}\"\n", manifest.name));
    out.push_str(&format!("version = \"{}\"\n", manifest.version));
    if let Some(desc) = &manifest.description {
        out.push_str(&format!("description = \"{desc}\"\n"));
    }
    out.push_str("edition = \"2025\"\n");
    out.push_str("\n# Add your project authors\n");
    out.push_str("# authors = [\"Your Name <you@example.com>\"]\n");
    out.push_str("\n# SPDX license identifier\n");
    out.push_str("# license = \"MIT\"\n");
    out.push_str("\n[dependencies]\n");
    out.push_str("# medi_data = \"0.1\"\n");
    out.push_str("\n# Optional FHIR configuration for healthcare interoperability\n");
    out.push_str("# [fhir]\n");
    out.push_str("# version = \"R4\"\n");
    out.push_str("# strict = true\n");
    out
}
