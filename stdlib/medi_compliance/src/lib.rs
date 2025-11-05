use serde::{Deserialize, Serialize};

/// Basic HIPAA compliance rule primitive
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct ComplianceRule {
    pub id: String,
    pub description: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct ComplianceSummary {
    pub total: usize,
    pub passed: usize,
    pub failed: usize,
}

pub fn summarize_results(results: &[ComplianceResult]) -> ComplianceSummary {
    let total = results.len();
    let passed = results.iter().filter(|r| r.passed).count();
    let failed = total - passed;
    ComplianceSummary {
        total,
        passed,
        failed,
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct ComplianceResult {
    pub rule_id: String,
    pub passed: bool,
    pub message: Option<String>,
}

pub trait Anonymizer {
    fn anonymize(&self, input: &str) -> String;
}

pub struct NoOpAnonymizer;
impl Anonymizer for NoOpAnonymizer {
    fn anonymize(&self, input: &str) -> String {
        input.to_string()
    }
}

pub struct Sha256Anonymizer;
impl Anonymizer for Sha256Anonymizer {
    fn anonymize(&self, input: &str) -> String {
        use sha2::{Digest, Sha256};
        let mut hasher = Sha256::new();
        hasher.update(input.as_bytes());
        let out = hasher.finalize();
        format!("{out:x}")
    }
}

/// Mask anonymizer: replaces all but last 4 visible characters with '*'
pub struct MaskAnonymizer;
impl Anonymizer for MaskAnonymizer {
    fn anonymize(&self, input: &str) -> String {
        let len = input.chars().count();
        if len <= 4 {
            return "*".repeat(len);
        }
        let keep = 4;
        let masked = "*".repeat(len - keep);
        let tail: String = input.chars().skip(len - keep).collect();
        format!("{masked}{tail}")
    }
}

/// Redact anonymizer: returns a constant tag
pub struct RedactAnonymizer;
impl Anonymizer for RedactAnonymizer {
    fn anonymize(&self, _input: &str) -> String {
        "[REDACTED]".to_string()
    }
}

/// Audit trail entry
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct AuditEntry {
    pub actor: String,
    pub action: String,
    pub resource: String,
    pub timestamp: String,
}

/// Naive compliance check: returns all rules as passed with a message.
pub fn check_hipaa_compliance(_data: &str, rules: &[ComplianceRule]) -> Vec<ComplianceResult> {
    rules
        .iter()
        .map(|r| ComplianceResult {
            rule_id: r.id.clone(),
            passed: true,
            message: Some(format!(
                "Rule '{}' considered passed (placeholder)",
                r.description
            )),
        })
        .collect()
}

/// Contains-forbidden rule: fails if any keyword is found (case-insensitive)
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct ContainsForbiddenRule {
    pub id: String,
    pub keywords: Vec<String>,
}

pub fn check_contains_forbidden(
    data: &str,
    rules: &[ContainsForbiddenRule],
) -> Vec<ComplianceResult> {
    let lower = data.to_lowercase();
    rules
        .iter()
        .map(|r| {
            let mut found = None;
            for kw in &r.keywords {
                if lower.contains(&kw.to_lowercase()) {
                    found = Some(kw.clone());
                    break;
                }
            }
            match found {
                Some(kw) => ComplianceResult {
                    rule_id: r.id.clone(),
                    passed: false,
                    message: Some(format!("found forbidden keyword: {kw}")),
                },
                None => ComplianceResult {
                    rule_id: r.id.clone(),
                    passed: true,
                    message: None,
                },
            }
        })
        .collect()
}
