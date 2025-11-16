//! Clinical trial utilities commonly used in design and analysis.

/// Absolute risk reduction (ARR) = control_event_rate - treatment_event_rate
pub fn absolute_risk_reduction(cer: f64, ter: f64) -> f64 {
    cer - ter
}

/// Relative risk (RR) = treatment_event_rate / control_event_rate
pub fn relative_risk(cer: f64, ter: f64) -> f64 {
    if cer == 0.0 {
        f64::NAN
    } else {
        ter / cer
    }
}

/// Number needed to treat (NNT) = 1 / ARR (absolute value). Infinity when ARR == 0.
pub fn number_needed_to_treat(cer: f64, ter: f64) -> f64 {
    let arr = absolute_risk_reduction(cer, ter);
    if arr == 0.0 {
        f64::INFINITY
    } else {
        (1.0 / arr).abs()
    }
}

/// Pooled standard deviation for two groups (unbiased, uses n-1 for each group)
pub fn pooled_stddev(s1: f64, n1: usize, s2: f64, n2: usize) -> f64 {
    if n1 < 2 || n2 < 2 {
        return f64::NAN;
    }
    let n1m1 = (n1 as f64) - 1.0;
    let n2m1 = (n2 as f64) - 1.0;
    let numerator = n1m1 * s1.powi(2) + n2m1 * s2.powi(2);
    let denominator = n1m1 + n2m1;
    if denominator == 0.0 {
        f64::NAN
    } else {
        (numerator / denominator).sqrt()
    }
}
