mod bootstrap;
/// Basic statistical functions for Medi.
///
/// P-value example (two-sided Welch t-test) using a t distribution (example ignored in doctest):
/// ```ignore
/// use tolvex_stats::{t_test_welch};
/// use statrs::distribution::{StudentsT, ContinuousCDF};
/// let a = [1.0, 2.0, 3.0, 4.0];
/// let b = [2.0, 3.0, 4.0, 5.0];
/// let res = t_test_welch(&a, &b);
/// let t = res.t_stat.abs();
/// let df = res.df;
/// let tdist = StudentsT::new(0.0, 1.0, df).unwrap();
/// let cdf = tdist.cdf(t);
/// let p_two_sided = 2.0 * (1.0 - cdf);
/// println!("p-value ~= {}", p_two_sided);
/// ```
mod clinical;
mod epidemiology;
#[cfg(feature = "fhir")]
mod fhir_adapters;
mod ml;
mod population;
mod power;
mod predictive;
mod quality;
mod risk;
mod stable;
mod streaming;
mod survival;
mod timeseries;
mod viz;

pub use bootstrap::*;
pub use clinical::*;
pub use epidemiology::*;
#[cfg(feature = "fhir")]
pub use fhir_adapters::*;
pub use ml::*;
pub use population::*;
pub use power::*;
pub use predictive::*;
pub use quality::*;
pub use risk::*;
pub use stable::*;
pub use streaming::*;
pub use survival::*;
pub use timeseries::*;
pub use viz::*;

pub fn mean(xs: &[f64]) -> f64 {
    if xs.is_empty() {
        return f64::NAN;
    }
    xs.iter().sum::<f64>() / (xs.len() as f64)
}

pub fn median(mut xs: Vec<f64>) -> f64 {
    if xs.is_empty() {
        return f64::NAN;
    }
    xs.sort_by(|a, b| a.partial_cmp(b).unwrap());
    let n = xs.len();
    if n % 2 == 1 {
        xs[n / 2]
    } else {
        (xs[n / 2 - 1] + xs[n / 2]) / 2.0
    }
}

/// Unbiased sample standard deviation (n-1 denominator)
pub fn stddev_sample(xs: &[f64]) -> f64 {
    let n = xs.len();
    if n < 2 {
        return f64::NAN;
    }
    let m = mean(xs);
    let var = xs.iter().map(|x| (x - m).powi(2)).sum::<f64>() / ((n as f64) - 1.0);
    var.sqrt()
}

/// Welch's t-test (unequal variances)
pub struct TTestResult {
    pub t_stat: f64,
    pub df: f64,
}

pub fn t_test_welch(a: &[f64], b: &[f64]) -> TTestResult {
    let ma = mean(a);
    let mb = mean(b);
    let sa = stddev_sample(a);
    let sb = stddev_sample(b);
    let na = a.len() as f64;
    let nb = b.len() as f64;
    let se2 = sa.powi(2) / na + sb.powi(2) / nb;
    let t = if se2 == 0.0 {
        0.0
    } else {
        (ma - mb) / se2.sqrt()
    };
    // Welch–Satterthwaite approximation
    let num = se2.powi(2);
    let den = (sa.powi(2) / na).powi(2) / (na - 1.0) + (sb.powi(2) / nb).powi(2) / (nb - 1.0);
    let df = if den == 0.0 { f64::INFINITY } else { num / den };
    TTestResult { t_stat: t, df }
}

/// Convenience function returning (t_stat, df, p_value two-sided) for Welch's t-test.
#[cfg(feature = "pvalue")]
pub fn t_test_pvalue_welch(a: &[f64], b: &[f64]) -> (f64, f64, f64) {
    use statrs::distribution::{ContinuousCDF, StudentsT};
    let res = t_test_welch(a, b);
    let t = res.t_stat.abs();
    let df = res.df;
    let tdist = StudentsT::new(0.0, 1.0, df).unwrap();
    let cdf = tdist.cdf(t);
    let p_two_sided = 2.0 * (1.0 - cdf);
    (res.t_stat, df, p_two_sided)
}
