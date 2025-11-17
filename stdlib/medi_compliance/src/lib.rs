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

/// De-identification helper: apply masking strategy to PHI-like strings.
pub fn mask_phi(input: &str) -> String {
    let mask = MaskAnonymizer;
    mask.anonymize(input)
}

/// De-identification helper: apply a one-way hash strategy to PHI-like strings.
pub fn hash_phi(input: &str) -> String {
    let sha = Sha256Anonymizer;
    sha.anonymize(input)
}

/// De-identification helper: fully redact PHI-like strings.
pub fn redact_phi(input: &str) -> String {
    let red = RedactAnonymizer;
    red.anonymize(input)
}

/// Tag-based de-identification helper.
///
/// Common tags (case-insensitive):
/// - "mask" or "phi.mask" -> `mask_phi`
/// - "hash" or "phi.hash" -> `hash_phi`
/// - "redact" or "phi.redact" -> `redact_phi`
///   Any other tag leaves the value unchanged.
pub fn deidentify_field(value: &str, tag: &str) -> String {
    let tag_lc = tag.to_ascii_lowercase();
    match tag_lc.as_str() {
        "mask" | "phi.mask" => mask_phi(value),
        "hash" | "phi.hash" => hash_phi(value),
        "redact" | "phi.redact" => redact_phi(value),
        _ => value.to_string(),
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
///
/// Internally this now routes through the richer standards/profile model by
/// constructing HIPAA `DetailedComplianceRule`s and invoking
/// `check_compliance_profile`. The public API and placeholder behavior are
/// preserved so existing callers and tests continue to work.
pub fn check_hipaa_compliance(data: &str, rules: &[ComplianceRule]) -> Vec<ComplianceResult> {
    // Bridge to the richer rule model for HIPAA.
    let detailed: Vec<DetailedComplianceRule> = rules
        .iter()
        .map(|r| DetailedComplianceRule {
            id: r.id.clone(),
            description: r.description.clone(),
            standard: ComplianceStandard::Hipaa,
            severity: RuleSeverity::Info,
            tags: vec!["hipaa".to_string()],
        })
        .collect();

    // We discard the detailed results for now and preserve the legacy
    // placeholder behavior, but this ensures the HIPAA pathway exercises the
    // new profile-aware engine.
    let _ = check_compliance_profile(data, &ComplianceProfile::Hipaa, &detailed);

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

/// Aggregate report for a HIPAA-oriented bundle of checks.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct HipaaBundleReport {
    pub keyword_results: Vec<ComplianceResult>,
    pub consent_ok: bool,
    pub retention_action: Option<RetentionAction>,
    pub overall: ComplianceSummary,
}

/// Run a simple HIPAA bundle:
/// - keyword rules over `data`
/// - optional consent check
/// - optional retention policy check
///
/// This function is intentionally lightweight and data-structure-oriented so
/// that host code can layer in additional sink/privacy checks enforced by the
/// compiler/type checker.
pub fn run_hipaa_bundle(
    data: &str,
    keyword_rules: &[HipaaKeywordRule],
    consent: Option<&ConsentRecord>,
    consent_resource: Option<&str>,
    consent_action: Option<&str>,
    age_days: Option<u32>,
    retention_policies: &[RetentionPolicy],
) -> HipaaBundleReport {
    let keyword_results = check_hipaa_keywords(data, keyword_rules);

    let consent_ok = match (consent, consent_resource, consent_action) {
        (Some(c), Some(res), Some(act)) => has_consent(c, res, act, None),
        _ => true,
    };

    let retention_action = age_days.and_then(|age| retention_decision(age, retention_policies));

    // Build a combined view for the overall summary.
    let mut combined: Vec<ComplianceResult> = keyword_results.clone();

    if let (Some(_c), Some(res), Some(act)) = (consent, consent_resource, consent_action) {
        combined.push(ComplianceResult {
            rule_id: "consent".to_string(),
            passed: consent_ok,
            message: Some(format!("Consent check for {res}:{act}")),
        });
    }

    if let Some(action) = &retention_action {
        combined.push(ComplianceResult {
            rule_id: "retention".to_string(),
            passed: !matches!(action, RetentionAction::Delete),
            message: Some(format!("Retention decision: {action:?}")),
        });
    }

    let overall = summarize_results(&combined);

    HipaaBundleReport {
        keyword_results,
        consent_ok,
        retention_action,
        overall,
    }
}

/// Simple HIPAA keyword rule specialized for PHI detection.
///
/// This is semantically similar to `ContainsForbiddenRule` but explicitly
/// associated with the HIPAA standard and a severity.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct HipaaKeywordRule {
    pub id: String,
    pub description: String,
    pub keywords: Vec<String>,
    pub severity: RuleSeverity,
}

/// Check for HIPAA-relevant keywords in `data` using `HipaaKeywordRule`s.
///
/// This function actually inspects the `data` payload, returning a
/// `ComplianceResult` per rule with pass/fail and a descriptive message when
/// a keyword is found.
pub fn check_hipaa_keywords(data: &str, rules: &[HipaaKeywordRule]) -> Vec<ComplianceResult> {
    let lower = data.to_lowercase();
    rules
        .iter()
        .map(|r| {
            let mut found: Option<String> = None;
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
                    message: Some(format!(
                        "HIPAA keyword rule '{}' (severity: {:?}) matched keyword: {kw}",
                        r.description, r.severity
                    )),
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

/// Regulatory standard for a given compliance rule.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum ComplianceStandard {
    /// U.S. HIPAA Privacy/Security rules and related guidance.
    Hipaa,
    /// EU General Data Protection Regulation.
    Gdpr,
    /// Canadian PIPEDA.
    Pipeda,
    /// Other or custom standard, identified by name (e.g., "FDA_21CFR11").
    Custom(String),
}

/// Severity for a compliance rule.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum RuleSeverity {
    Info,
    Warning,
    Error,
}

/// High-level compliance profile for grouping rules.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum ComplianceProfile {
    Hipaa,
    Gdpr,
    Pipeda,
    /// A named bundle of standards (e.g., "FDA_21CFR11", "CMS_Quality").
    Named(String),
}

/// Richer rule representation used for profile-aware checks.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct DetailedComplianceRule {
    /// Stable identifier for this rule.
    pub id: String,
    /// Human-readable description.
    pub description: String,
    /// Primary standard this rule is associated with.
    pub standard: ComplianceStandard,
    /// Severity if the rule fails.
    pub severity: RuleSeverity,
    /// Optional tags, such as "audit", "consent", "retention".
    pub tags: Vec<String>,
}

/// Basic profile-aware compliance check.
///
/// This is intentionally conservative and data-agnostic for now: it records
/// which rules would be evaluated for the given profile and returns them as
/// passed with an informational message. Callers can layer additional logic
/// on top or plug in more specific checkers for each tag/standard.
pub fn check_compliance_profile(
    data: &str,
    profile: &ComplianceProfile,
    rules: &[DetailedComplianceRule],
) -> Vec<ComplianceResult> {
    let _ = data;

    rules
        .iter()
        .filter(|rule| match (&rule.standard, profile) {
            (ComplianceStandard::Hipaa, ComplianceProfile::Hipaa) => true,
            (ComplianceStandard::Gdpr, ComplianceProfile::Gdpr) => true,
            (ComplianceStandard::Pipeda, ComplianceProfile::Pipeda) => true,
            (ComplianceStandard::Custom(name), ComplianceProfile::Named(p)) => name == p,
            // Allow cross-profile rules; callers can decide how to interpret them.
            _ => true,
        })
        .map(|rule| ComplianceResult {
            rule_id: rule.id.clone(),
            passed: true,
            message: Some(format!(
                "Rule '{}' for {:?} treated as passed (profile-aware placeholder)",
                rule.description, profile
            )),
        })
        .collect()
}

/// Simple in-memory audit trail wrapper around `AuditEntry`.
#[derive(Debug, Clone, Default, Serialize, Deserialize, PartialEq)]
pub struct AuditTrail {
    pub entries: Vec<AuditEntry>,
}

impl AuditTrail {
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
        }
    }

    pub fn push(&mut self, entry: AuditEntry) {
        self.entries.push(entry);
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}

/// Convenience helper to construct an `AuditEntry`.
pub fn make_audit_entry(
    actor: impl Into<String>,
    action: impl Into<String>,
    resource: impl Into<String>,
    timestamp: impl Into<String>,
) -> AuditEntry {
    AuditEntry {
        actor: actor.into(),
        action: action.into(),
        resource: resource.into(),
        timestamp: timestamp.into(),
    }
}

/// Status of a consent record.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum ConsentStatus {
    Active,
    Revoked,
    Expired,
}

/// Scope of consent: which resource and action are permitted.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct ConsentScope {
    pub resource: String,
    pub action: String,
}

/// Consent record for a subject.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct ConsentRecord {
    pub id: String,
    pub subject: String,
    pub scopes: Vec<ConsentScope>,
    pub status: ConsentStatus,
    /// Optional ISO 8601 expiration timestamp (e.g., "2025-05-15T00:00:00Z").
    pub expires_at: Option<String>,
}

/// Check whether a given consent record allows the specified action on a resource
/// at the current time.
///
/// - Returns false if the consent is revoked or expired.
/// - If `now_iso` is provided and `expires_at` is set and strictly less than `now_iso`
///   (lexicographically, assuming RFC 3339), the consent is treated as expired.
pub fn has_consent(
    consent: &ConsentRecord,
    resource: &str,
    action: &str,
    now_iso: Option<&str>,
) -> bool {
    match consent.status {
        ConsentStatus::Revoked | ConsentStatus::Expired => return false,
        ConsentStatus::Active => {}
    }

    if let (Some(exp), Some(now)) = (consent.expires_at.as_deref(), now_iso) {
        if exp < now {
            return false;
        }
    }

    consent
        .scopes
        .iter()
        .any(|s| s.resource == resource && s.action == action)
}

/// Action to take for data that matches a retention policy.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum RetentionAction {
    Retain,
    Anonymize,
    Delete,
}

/// Simple time-based retention policy expressed in days since creation.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct RetentionPolicy {
    /// Minimum age (days) at which this policy becomes applicable.
    pub min_age_days: u32,
    /// Optional maximum age; if None, there is no upper bound.
    pub max_age_days: Option<u32>,
    pub action: RetentionAction,
}

impl RetentionPolicy {
    pub fn applies_to(&self, age_days: u32) -> bool {
        if age_days < self.min_age_days {
            return false;
        }
        if let Some(max) = self.max_age_days {
            if age_days > max {
                return false;
            }
        }
        true
    }
}

/// Determine the retention action for a data item of `age_days` using the first
/// applicable policy in the list.
pub fn retention_decision(age_days: u32, policies: &[RetentionPolicy]) -> Option<RetentionAction> {
    for p in policies {
        if p.applies_to(age_days) {
            return Some(p.action.clone());
        }
    }
    None
}

/// Kind of regulatory report being summarized.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum ReportKind {
    Fda21Cfr11,
    CmsQuality,
    Custom(String),
}

/// Compact summary suitable for conversion to JSON or other report formats.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct ComplianceReportSummary {
    pub kind: ReportKind,
    pub profile: Option<ComplianceProfile>,
    pub summary: ComplianceSummary,
}

/// Build a report summary from raw compliance results.
pub fn build_compliance_report_summary(
    kind: ReportKind,
    profile: Option<ComplianceProfile>,
    results: &[ComplianceResult],
) -> ComplianceReportSummary {
    let summary = summarize_results(results);
    ComplianceReportSummary {
        kind,
        profile,
        summary,
    }
}
