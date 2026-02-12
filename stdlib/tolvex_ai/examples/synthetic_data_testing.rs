use tolvex_ai::{predict_risk, stratify_risk};
use serde_json::json;

fn main() {
    // Deterministic synthetic cohort (hand-rolled; stable for docs/tests).
    let mut scores: Vec<f64> = Vec::new();
    for i in 0..10 {
        let age = 40 + i; // 40..49
        let bmi = 25.0 + (i as f64) * 0.5;
        let a1c = 5.6 + (i as f64) * 0.2;

        // `predict_risk` currently averages numeric features and clamps to [0,1].
        // Provide normalized features to avoid saturating at 1.0.
        let patient = json!({
            "age": (age as f64) / 100.0,
            "bmi": bmi / 50.0,
            "a1c": a1c / 10.0
        });
        let pred = predict_risk(
            &patient,
            "diabetes",
            "5y",
            &["age".into(), "bmi".into(), "a1c".into()],
        );
        let stratum = stratify_risk(&pred);
        println!("p{i}: score={:.3} stratum={stratum:?}", pred.score);
        scores.push(pred.score);
    }

    let avg = scores.iter().sum::<f64>() / (scores.len() as f64);
    println!("avg_risk={avg:.3}");
}
