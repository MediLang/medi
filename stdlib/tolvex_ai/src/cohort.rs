use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;

use crate::bias::{compute_group_bias, GroupBiasMetric};
use crate::risk::{predict_risk, stratify_risk, RiskPrediction, RiskStratum};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PatientInput {
    pub id: String,
    pub data: JsonValue,
    pub group: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PatientRiskSummary {
    pub id: String,
    pub condition: String,
    pub timeframe: String,
    pub score: f64,
    pub stratum: RiskStratum,
    pub group: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CohortRiskReport {
    pub patients: Vec<PatientRiskSummary>,
    pub group_metrics: Vec<GroupBiasMetric>,
}

pub fn build_cohort_risk_report(
    patients: &[PatientInput],
    condition: &str,
    timeframe: &str,
    features: &[String],
) -> CohortRiskReport {
    let mut summaries = Vec::new();
    let mut group_scores = Vec::new();

    for p in patients {
        let pred: RiskPrediction = predict_risk(&p.data, condition, timeframe, features);
        let stratum = stratify_risk(&pred);

        group_scores.push((pred.score, p.group.clone()));

        summaries.push(PatientRiskSummary {
            id: p.id.clone(),
            condition: pred.condition,
            timeframe: pred.timeframe,
            score: pred.score,
            stratum,
            group: p.group.clone(),
        });
    }

    let group_metrics = compute_group_bias(&group_scores);

    CohortRiskReport {
        patients: summaries,
        group_metrics,
    }
}
