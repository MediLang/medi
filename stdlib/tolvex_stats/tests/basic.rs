use approx::assert_abs_diff_eq;
use tolvex_stats::{mean, median, stddev_sample, t_test_welch};

#[test]
fn mean_median_stddev() {
    let xs = [1.0, 2.0, 3.0, 4.0];
    assert_abs_diff_eq!(mean(&xs), 2.5, epsilon = 1e-12);
    assert_abs_diff_eq!(median(xs.to_vec()), 2.5, epsilon = 1e-12);
    // Sample stddev for [1,2,3,4] is sqrt(1.6666...) ~ 1.29099
    assert_abs_diff_eq!(stddev_sample(&xs), 1.2909944487358056, epsilon = 1e-12);
}

#[test]
fn welch_t_test_smoke() {
    let a = [1.0, 2.0, 3.0, 4.0];
    let b = [2.0, 3.0, 4.0, 5.0];
    let res = t_test_welch(&a, &b);
    // Means differ by -1 with similar variance; t should be finite and not NaN
    assert!(res.t_stat.is_finite());
    assert!(res.df.is_finite());
}
