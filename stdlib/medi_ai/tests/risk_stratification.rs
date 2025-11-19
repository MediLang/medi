use medi_ai::risk::{RiskPrediction, RiskStratum};
use medi_ai::stratify_risk;

#[test]
fn risk_stratification_thresholds_are_deterministic() {
    let low = RiskPrediction {
        condition: "cond".to_string(),
        timeframe: "t".to_string(),
        score: 0.1,
        features_used: vec![],
    };
    let moderate = RiskPrediction {
        condition: "cond".to_string(),
        timeframe: "t".to_string(),
        score: 0.5,
        features_used: vec![],
    };
    let high = RiskPrediction {
        condition: "cond".to_string(),
        timeframe: "t".to_string(),
        score: 0.9,
        features_used: vec![],
    };

    assert_eq!(stratify_risk(&low), RiskStratum::Low);
    assert_eq!(stratify_risk(&moderate), RiskStratum::Moderate);
    assert_eq!(stratify_risk(&high), RiskStratum::High);
}
