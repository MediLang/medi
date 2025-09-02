use medic_env::env::{SinkKind, TypeEnv};
use medic_type::types::{PrivacyAnnotation, SinkClass};

/// Simple runtime-facing privacy violation type for sink verification.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PrivacyViolation {
    Violation(String),
}

fn to_sink_class(kind: SinkKind) -> SinkClass {
    match kind {
        SinkKind::Log => SinkClass::Log,
        SinkKind::Print => SinkClass::Print,
        SinkKind::Export => SinkClass::Export,
        SinkKind::Network => SinkClass::Network,
        SinkKind::File => SinkClass::File,
    }
}

pub(crate) fn meets_min(label: PrivacyAnnotation, min: PrivacyAnnotation) -> bool {
    use PrivacyAnnotation::*;
    match min {
        // Requiring Anonymized means only fully anonymized data is acceptable.
        Anonymized => matches!(label, Anonymized),
        // Requiring Pseudonymized allows pseudonymized or anonymized (safer) data.
        Pseudonymized => matches!(label, Pseudonymized | Anonymized),
        // Requiring Authorized allows authorized data, or anonymized (safer). Pseudonymized still contains possible identifiers without consent -> not accepted.
        Authorized => matches!(label, Authorized | Anonymized),
        // Requiring AuthorizedFor(kind) allows AuthorizedFor(same kind), Authorized (broader), or Anonymized (safer)
        AuthorizedFor(k) => match label {
            AuthorizedFor(k2) if k2 == k => true,
            Authorized | Anonymized => true,
            _ => false,
        },
        // Requiring PHI imposes no additional restriction beyond base HIPAA rules handled above.
        PHI => true,
    }
}

/// Determine if sending data with `label` to the named `sink` is allowed under current rules.
///
/// Rules mirror the type checker's enforcement:
/// - PHI to any sink: violation
/// - Pseudonymized to Network/File sinks: violation; allowed to Print/Log/Export
/// - Anonymized: allowed
/// - Authorized: allowed (treated as permitted for sinks; use richer auth if needed)
pub fn verify_sink(
    env: &TypeEnv,
    sink: &str,
    label: PrivacyAnnotation,
) -> Result<(), PrivacyViolation> {
    // Try env metadata for sink kind first
    let kind = env.get_sink_fn(sink).or_else(|| fallback_sink_kind(sink));
    if let Some(kind) = kind {
        // Optional policy: enforce minimum required privacy for this sink
        if let Some(min) = env
            .get_sink_min_privacy(sink)
            .or_else(|| env.get_sink_kind_min_privacy(kind))
        {
            if !meets_min(label, min) {
                return Err(PrivacyViolation::Violation(format!(
                    "Policy violation: label {label:?} does not meet minimum {min:?} for sink '{sink}'"
                )));
            }
        }
        let sink_class = to_sink_class(kind);
        let is_violation = match label {
            PrivacyAnnotation::PHI => true,
            PrivacyAnnotation::Pseudonymized => matches!(kind, SinkKind::Network | SinkKind::File),
            PrivacyAnnotation::Anonymized => false,
            PrivacyAnnotation::Authorized => false,
            PrivacyAnnotation::AuthorizedFor(c) => c != sink_class,
        };
        if is_violation {
            let kind_str = match kind {
                SinkKind::Log => "log",
                SinkKind::Print => "print",
                SinkKind::Export => "export",
                SinkKind::Network => "network",
                SinkKind::File => "file",
            };
            return Err(PrivacyViolation::Violation(format!(
                "HIPAA violation: {label:?} data cannot be sent to {kind_str} sink '{sink}'"
            )));
        }
    }
    Ok(())
}

fn fallback_sink_kind(name: &str) -> Option<SinkKind> {
    match name {
        "print" | "println" => Some(SinkKind::Print),
        "log" | "debug" | "trace" => Some(SinkKind::Log),
        "export" => Some(SinkKind::Export),
        "writeFile" | "write_file" | "save" => Some(SinkKind::File),
        "send_http" | "http_post" | "http_get" | "network_send" | "send" => Some(SinkKind::Network),
        _ => None,
    }
}
