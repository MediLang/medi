//! Bootstrap confidence intervals and permutation tests
use rand::prelude::SliceRandom;
use rand::{rngs::StdRng, Rng, SeedableRng};

/// Compute a bootstrap confidence interval for a statistic over data.
/// - `statistic`: function mapping slice -> scalar (e.g., mean)
/// - `n_bootstrap`: number of bootstrap resamples
/// - `confidence`: e.g., 0.95
pub fn bootstrap_ci(
    data: &[f64],
    statistic: fn(&[f64]) -> f64,
    n_bootstrap: usize,
    confidence: f64,
) -> (f64, f64) {
    assert!(!data.is_empty());
    assert!(n_bootstrap > 0);
    assert!(confidence > 0.0 && confidence < 1.0);
    let alpha = (1.0 - confidence) / 2.0;

    let mut rng = StdRng::from_entropy();
    let n = data.len();
    let mut stats = Vec::with_capacity(n_bootstrap);
    let mut sample = vec![0.0f64; n];

    for _ in 0..n_bootstrap {
        for slot in sample.iter_mut().take(n) {
            let idx = rng.gen_range(0..n);
            *slot = data[idx];
        }
        stats.push(statistic(&sample));
    }

    stats.sort_by(|a, b| a.partial_cmp(b).unwrap());
    let lo_idx = ((alpha * n_bootstrap as f64).floor() as usize).min(n_bootstrap - 1);
    let hi_idx = (((1.0 - alpha) * n_bootstrap as f64).ceil() as usize)
        .saturating_sub(1)
        .min(n_bootstrap - 1);
    (stats[lo_idx], stats[hi_idx])
}

/// Two-sample permutation test for difference in means.
/// Returns the proportion of permuted differences with |diff| >= observed |diff| (two-sided p-value).
pub fn permutation_test_diff_means(a: &[f64], b: &[f64], n_perm: usize) -> f64 {
    assert!(!a.is_empty() && !b.is_empty());
    let obs = mean(a) - mean(b);
    let mut pool = Vec::with_capacity(a.len() + b.len());
    pool.extend_from_slice(a);
    pool.extend_from_slice(b);

    let mut rng = StdRng::from_entropy();
    let mut extreme = 0usize;
    for _ in 0..n_perm {
        // shuffle and split
        pool.shuffle(&mut rng);
        let (a_s, b_s) = pool.split_at(a.len());
        let perm_diff = mean(a_s) - mean(b_s);
        if perm_diff.abs() >= obs.abs() {
            extreme += 1;
        }
    }
    (extreme as f64 + 1.0) / (n_perm as f64 + 1.0) // add-one smoothing
}

fn mean(xs: &[f64]) -> f64 {
    xs.iter().sum::<f64>() / xs.len() as f64
}

#[cfg(test)]
mod tests {
    use super::*;

    fn stat_mean(xs: &[f64]) -> f64 {
        xs.iter().sum::<f64>() / xs.len() as f64
    }

    #[test]
    fn bootstrap_ci_contains_true_mean() {
        let data = vec![1.0, 2.0, 3.0, 4.0, 5.0];
        let (lo, hi) = bootstrap_ci(&data, stat_mean, 500, 0.90);
        let true_mean = stat_mean(&data);
        assert!(lo <= true_mean && true_mean <= hi);
    }

    #[test]
    fn permutation_test_reasonable() {
        let a = vec![1.0, 1.1, 0.9, 1.0, 1.2];
        let b = vec![2.0, 2.1, 2.2, 1.9, 2.05];
        let p = permutation_test_diff_means(&a, &b, 500);
        assert!(p < 0.1);
    }
}
