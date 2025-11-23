use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;

#[allow(unused_imports)]
use regex::Regex;

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

// ===== Rule Engine (Task 21 foundation) =====

/// Atomic condition operators supported by the rule engine.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ConditionOp {
    /// Check that a field at `path` exists (or not) in a JSON-like payload.
    Exists { path: String, should_exist: bool },
    /// Check equality of a field value to a constant.
    Equals { path: String, value: JsonValue },
    /// Check that a field matches a regex pattern (string fields only).
    Regex { path: String, pattern: String },
    /// Check inequality of a field value to a constant.
    NotEquals { path: String, value: JsonValue },
    /// Check that a field is non-empty (strings: len>0 after trim; arrays: len>0; objects: len>0).
    NonEmpty { path: String },
    /// For strings: substring containment (case-sensitive). For arrays: membership by exact JsonValue equality.
    Contains { path: String, item: JsonValue },
    /// Numeric comparison: field > value (numbers only).
    GreaterThan { path: String, value: f64 },
    /// Numeric comparison: field < value (numbers only).
    LessThan { path: String, value: f64 },
}

/// A boolean expression over conditions.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum RuleExpr {
    Condition(ConditionOp),
    And(Vec<RuleExpr>),
    Or(Vec<RuleExpr>),
    Not(Box<RuleExpr>),
}

/// Evidence item produced during evaluation when a rule fails or matches.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct EvidenceItem {
    pub path: String,
    pub value_snippet: Option<String>,
}

/// Evaluate a rule expression against JSON data and return (passed, evidence).
pub fn evaluate_rule_expr(data: &JsonValue, expr: &RuleExpr) -> (bool, Vec<EvidenceItem>) {
    match expr {
        RuleExpr::Condition(cond) => evaluate_condition(data, cond),
        RuleExpr::And(list) => {
            let mut ev = Vec::new();
            for e in list {
                let (ok, sub) = evaluate_rule_expr(data, e);
                ev.extend(sub);
                if !ok {
                    return (false, ev);
                }
            }
            (true, ev)
        }
        RuleExpr::Or(list) => {
            let mut all_ev = Vec::new();
            for e in list {
                let (ok, sub) = evaluate_rule_expr(data, e);
                if ok {
                    return (true, sub);
                }
                all_ev.extend(sub);
            }
            (false, all_ev)
        }
        RuleExpr::Not(inner) => {
            let (ok, ev) = evaluate_rule_expr(data, inner);
            (!ok, ev)
        }
    }
}

fn evaluate_condition(data: &JsonValue, cond: &ConditionOp) -> (bool, Vec<EvidenceItem>) {
    match cond {
        ConditionOp::Exists { path, should_exist } => {
            let v = get_path(data, path);
            let exists = v.is_some();
            let pass = exists == *should_exist;
            let ev = EvidenceItem {
                path: path.clone(),
                value_snippet: v.map(snippet),
            };
            (pass, vec![ev])
        }
        ConditionOp::Equals { path, value } => {
            let v = get_path(data, path);
            let pass = v == Some(value);
            let ev = EvidenceItem {
                path: path.clone(),
                value_snippet: v.map(snippet),
            };
            (pass, vec![ev])
        }
        ConditionOp::NotEquals { path, value } => {
            let v = get_path(data, path);
            let pass = v != Some(value);
            let ev = EvidenceItem {
                path: path.clone(),
                value_snippet: v.map(snippet),
            };
            (pass, vec![ev])
        }
        ConditionOp::NonEmpty { path } => {
            let v = get_path(data, path);
            let pass = match v {
                Some(JsonValue::String(s)) => !s.trim().is_empty(),
                Some(JsonValue::Array(a)) => !a.is_empty(),
                Some(JsonValue::Object(o)) => !o.is_empty(),
                Some(_) => true, // numbers/bools considered non-empty
                None => false,
            };
            let ev = EvidenceItem {
                path: path.clone(),
                value_snippet: v.map(snippet),
            };
            (pass, vec![ev])
        }
        ConditionOp::Contains { path, item } => {
            let v = get_path(data, path);
            let pass = match (v, item) {
                (Some(JsonValue::String(s)), JsonValue::String(needle)) => s.contains(needle),
                (Some(JsonValue::Array(a)), it) => a.iter().any(|x| x == it),
                _ => false,
            };
            let ev = EvidenceItem {
                path: path.clone(),
                value_snippet: get_path(data, path).map(snippet),
            };
            (pass, vec![ev])
        }
        ConditionOp::GreaterThan { path, value } => {
            let v = get_path(data, path);
            let pass = match v {
                Some(JsonValue::Number(n)) => n.as_f64().is_some_and(|x| x > *value),
                _ => false,
            };
            let ev = EvidenceItem {
                path: path.clone(),
                value_snippet: v.map(snippet),
            };
            (pass, vec![ev])
        }
        ConditionOp::LessThan { path, value } => {
            let v = get_path(data, path);
            let pass = match v {
                Some(JsonValue::Number(n)) => n.as_f64().is_some_and(|x| x < *value),
                _ => false,
            };
            let ev = EvidenceItem {
                path: path.clone(),
                value_snippet: v.map(snippet),
            };
            (pass, vec![ev])
        }
        ConditionOp::Regex { path, pattern } => {
            let v = get_path(data, path);
            let pass = match (v, Regex::new(pattern)) {
                (Some(JsonValue::String(s)), Ok(re)) => re.is_match(s),
                _ => false,
            };
            let ev = EvidenceItem {
                path: path.clone(),
                value_snippet: get_path(data, path).map(snippet),
            };
            (pass, vec![ev])
        }
    }
}

/// Retrieve a value by a simple dot-path (e.g., "patient.name.family", indexes like [0] supported).
fn get_path<'a>(data: &'a JsonValue, path: &str) -> Option<&'a JsonValue> {
    let mut cur = data;
    if path.is_empty() {
        return Some(cur);
    }
    for seg in path.split('.') {
        if let Some((name, idx_opt)) = parse_segment(seg) {
            cur = cur.get(name)?;
            if let Some(idx) = idx_opt {
                cur = cur.get(idx)?;
            }
        } else {
            // No bracket, treat as plain key
            cur = cur.get(seg)?;
        }
    }
    Some(cur)
}

fn parse_segment(seg: &str) -> Option<(&str, Option<usize>)> {
    if let Some(bracket) = seg.find('[') {
        let name = &seg[..bracket];
        let rest = &seg[bracket..];
        if rest.ends_with(']') {
            let inner = &rest[1..rest.len() - 1];
            if let Ok(idx) = inner.parse::<usize>() {
                return Some((name, Some(idx)));
            }
        }
        Some((name, None))
    } else {
        None
    }
}

fn snippet(v: &JsonValue) -> String {
    match v {
        JsonValue::String(s) => {
            if s.len() > 64 {
                format!("{}…", &s[..64])
            } else {
                s.clone()
            }
        }
        _ => format!("{v}"),
    }
}

/// Convenience: evaluate a rule and return a ComplianceResult using severity/message.
pub fn evaluate_rule_to_result(
    rule_id: &str,
    severity: RuleSeverity,
    expr: &RuleExpr,
    data: &JsonValue,
) -> ComplianceResult {
    let (passed, evidence) = evaluate_rule_expr(data, expr);
    let message = if passed {
        None
    } else {
        let mut msg = format!("Rule '{rule_id}' failed (severity: {severity:?})");
        if !evidence.is_empty() {
            let details: Vec<String> = evidence
                .iter()
                .map(|e| match &e.value_snippet {
                    Some(v) => format!("{}={}", e.path, v),
                    None => e.path.clone(),
                })
                .collect();
            msg.push_str(&format!("; evidence: {}", details.join(", ")));
        }
        Some(msg)
    };
    ComplianceResult {
        rule_id: rule_id.to_string(),
        passed,
        message,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rule_engine_basic_conditions_and_composition() {
        let data = serde_json::json!({
            "patient": { "id": "p1", "name": { "family": "Doe", "given": ["Jane"] } },
            "age": 42,
            "tags": ["hipaa","phi"],
            "note": "Allergic to penicillin"
        });

        // Exists true
        let c1 = RuleExpr::Condition(ConditionOp::Exists {
            path: "patient.name.family".to_string(),
            should_exist: true,
        });
        assert!(evaluate_rule_expr(&data, &c1).0);

        // Equals
        let c2 = RuleExpr::Condition(ConditionOp::Equals {
            path: "patient.id".to_string(),
            value: JsonValue::String("p1".to_string()),
        });
        assert!(evaluate_rule_expr(&data, &c2).0);

        // Regex
        let c3 = RuleExpr::Condition(ConditionOp::Regex {
            path: "note".to_string(),
            pattern: "penicil+in".to_string(),
        });
        assert!(evaluate_rule_expr(&data, &c3).0);

        // NotEquals
        let neq = RuleExpr::Condition(ConditionOp::NotEquals {
            path: "patient.id".to_string(),
            value: JsonValue::String("p2".to_string()),
        });
        assert!(evaluate_rule_expr(&data, &neq).0);

        // NonEmpty: tags array and note string
        let ne_tags = RuleExpr::Condition(ConditionOp::NonEmpty {
            path: "tags".to_string(),
        });
        assert!(evaluate_rule_expr(&data, &ne_tags).0);
        let ne_note = RuleExpr::Condition(ConditionOp::NonEmpty {
            path: "note".to_string(),
        });
        assert!(evaluate_rule_expr(&data, &ne_note).0);

        // Contains
        let c_str = RuleExpr::Condition(ConditionOp::Contains {
            path: "note".to_string(),
            item: JsonValue::String("penicillin".to_string()),
        });
        assert!(evaluate_rule_expr(&data, &c_str).0);
        let c_arr = RuleExpr::Condition(ConditionOp::Contains {
            path: "tags".to_string(),
            item: JsonValue::String("phi".to_string()),
        });
        assert!(evaluate_rule_expr(&data, &c_arr).0);

        // GreaterThan / LessThan
        let gt = RuleExpr::Condition(ConditionOp::GreaterThan {
            path: "age".to_string(),
            value: 18.0,
        });
        assert!(evaluate_rule_expr(&data, &gt).0);
        let lt = RuleExpr::Condition(ConditionOp::LessThan {
            path: "age".to_string(),
            value: 100.0,
        });
        assert!(evaluate_rule_expr(&data, &lt).0);

        // Not
        let not_id = RuleExpr::Not(Box::new(RuleExpr::Condition(ConditionOp::Equals {
            path: "patient.id".to_string(),
            value: JsonValue::String("p2".to_string()),
        })));
        assert!(evaluate_rule_expr(&data, &not_id).0);

        // And
        let and = RuleExpr::And(vec![c1.clone(), c2.clone(), c3.clone()]);
        assert!(evaluate_rule_expr(&data, &and).0);

        // Or
        let or = RuleExpr::Or(vec![
            RuleExpr::Condition(ConditionOp::Equals {
                path: "patient.id".to_string(),
                value: JsonValue::String("no".to_string()),
            }),
            c2.clone(),
        ]);
        assert!(evaluate_rule_expr(&data, &or).0);
    }
}
