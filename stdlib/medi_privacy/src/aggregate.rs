use rand::Rng;

use crate::{gaussian_mechanism, laplace_mechanism, PrivacyBudget};

pub fn private_sum_laplace(
    xs: &[f64],
    sensitivity: f64,
    epsilon: f64,
    rng: &mut impl Rng,
) -> Option<f64> {
    let s = xs.iter().sum::<f64>();
    laplace_mechanism(s, sensitivity, epsilon, rng)
}

pub fn private_mean_laplace(
    xs: &[f64],
    sensitivity: f64,
    epsilon: f64,
    rng: &mut impl Rng,
) -> Option<f64> {
    if xs.is_empty() {
        return None;
    }
    let mean = xs.iter().sum::<f64>() / (xs.len() as f64);
    laplace_mechanism(mean, sensitivity, epsilon, rng)
}

pub fn private_sum_gaussian(
    xs: &[f64],
    sensitivity: f64,
    epsilon: f64,
    delta: f64,
    rng: &mut impl Rng,
) -> Option<f64> {
    let s = xs.iter().sum::<f64>();
    gaussian_mechanism(s, sensitivity, epsilon, delta, rng)
}

pub fn private_histogram_laplace(
    counts: &[u64],
    sensitivity: f64,
    epsilon: f64,
    rng: &mut impl Rng,
) -> Option<Vec<f64>> {
    let mut out = Vec::with_capacity(counts.len());
    for &c in counts {
        let noisy = laplace_mechanism(c as f64, sensitivity, epsilon, rng)?;
        out.push(noisy.max(0.0));
    }
    Some(out)
}

pub fn private_sum_laplace_budgeted(
    xs: &[f64],
    sensitivity: f64,
    epsilon: f64,
    budget: &mut PrivacyBudget,
    rng: &mut impl Rng,
) -> Option<f64> {
    if !budget.spend(epsilon, 0.0) {
        return None;
    }
    private_sum_laplace(xs, sensitivity, epsilon, rng)
}
