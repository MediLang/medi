use serde::{Deserialize, Serialize};

use crate::risk::RiskPrediction;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Recommendation {
    pub action: String,
    pub rationale: String,
    pub priority: String,
}

pub fn suggest_next_steps(risk: &RiskPrediction) -> Vec<Recommendation> {
    let mut out = Vec::new();

    if risk.score >= 0.7 {
        out.push(Recommendation {
            action: "flag_high_risk".to_string(),
            rationale: format!("predicted risk {:.2} for {}", risk.score, risk.condition),
            priority: "high".to_string(),
        });
        out.push(Recommendation {
            action: "schedule_follow_up".to_string(),
            rationale: "follow up due to elevated risk".to_string(),
            priority: "medium".to_string(),
        });
    } else if risk.score >= 0.3 {
        out.push(Recommendation {
            action: "monitor".to_string(),
            rationale: format!("moderate risk {:.2}", risk.score),
            priority: "medium".to_string(),
        });
    } else {
        out.push(Recommendation {
            action: "routine_care".to_string(),
            rationale: "no elevated risk detected".to_string(),
            priority: "low".to_string(),
        });
    }

    out
}
