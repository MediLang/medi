pub fn rate_per(population_events: f64, population: f64, per: f64) -> f64 {
    if population == 0.0 {
        f64::NAN
    } else {
        (population_events / population) * per
    }
}

pub fn age_standardized_rate(group_rates: &[f64], weights: &[f64]) -> f64 {
    if group_rates.len() != weights.len() || group_rates.is_empty() {
        return f64::NAN;
    }
    let wsum: f64 = weights.iter().sum();
    if wsum == 0.0 {
        return f64::NAN;
    }
    let mut s = 0.0;
    for i in 0..group_rates.len() {
        s += group_rates[i] * weights[i];
    }
    s / wsum
}
