use medi_stats::t_test_welch;
use std::fs;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let path = "examples/use_cases/trial_results.csv";
    let csv = fs::read_to_string(path)?;

    let mut control: Vec<f64> = Vec::new();
    let mut treatment: Vec<f64> = Vec::new();

    for (i, line) in csv.lines().enumerate() {
        if i == 0 {
            continue;
        }
        let parts: Vec<&str> = line.split(',').collect();
        if parts.len() != 2 {
            continue;
        }
        let group = parts[0].trim();
        let outcome: f64 = match parts[1].trim().parse() {
            Ok(v) => v,
            Err(_) => continue,
        };

        match group {
            "control" => control.push(outcome),
            "treatment" => treatment.push(outcome),
            _ => {}
        }
    }

    if control.is_empty() || treatment.is_empty() {
        return Err("missing control or treatment rows".into());
    }

    // Always compute at least t and df (no feature flags).
    let res = t_test_welch(&treatment, &control);
    println!("t={:.4} df={:.2}", res.t_stat, res.df);

    // Optionally compute p-value if medi_stats was built with the feature.
    #[cfg(feature = "pvalue")]
    {
        use medi_stats::t_test_pvalue_welch;
        let (_t, _df, p) = t_test_pvalue_welch(&treatment, &control);
        println!("p_value={:.6}", p);
    }

    Ok(())
}
