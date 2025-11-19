use medi_ai::cohort::{build_cohort_risk_report, PatientInput};
use medi_ai::risk::RiskStratum;
use serde_json::json;

#[test]
fn cohort_risk_report_ties_prediction_stratification_and_bias_metrics() {
    let patients = vec![
        PatientInput {
            id: "p1".to_string(),
            data: json!({ "age": 40.0, "bp": 120.0 }),
            group: "group_a".to_string(),
        },
        PatientInput {
            id: "p2".to_string(),
            data: json!({ "age": 80.0, "bp": 180.0 }),
            group: "group_b".to_string(),
        },
    ];

    let features = vec!["age".to_string(), "bp".to_string()];

    let report = build_cohort_risk_report(&patients, "heart_failure", "5_years", &features);

    assert_eq!(report.patients.len(), 2);

    let p1 = &report.patients[0];
    let p2 = &report.patients[1];

    assert_eq!(p1.id, "p1");
    assert_eq!(p1.condition, "heart_failure");
    assert_eq!(p1.timeframe, "5_years");

    assert_eq!(p2.id, "p2");
    assert!(p2.score >= p1.score);

    match p2.stratum {
        RiskStratum::Moderate | RiskStratum::High => {}
        RiskStratum::Low => panic!("expected higher risk stratum for p2"),
    }

    assert_eq!(report.group_metrics.len(), 2);
}
