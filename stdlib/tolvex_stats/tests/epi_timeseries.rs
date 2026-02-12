use tolvex_stats::{
    exponential_moving_average, incidence_rate, moving_average, npv, ppv, prevalence, rolling_std,
    sensitivity, specificity,
};

#[test]
fn epi_measures() {
    assert!((incidence_rate(10.0, 1000.0) - 0.01).abs() < 1e-12);
    assert!((prevalence(50.0, 1000.0) - 0.05).abs() < 1e-12);
    assert!((sensitivity(80.0, 20.0) - 0.8).abs() < 1e-12);
    assert!((specificity(90.0, 10.0) - 0.9).abs() < 1e-12);
    assert!((ppv(80.0, 20.0) - 0.8).abs() < 1e-12);
    assert!((npv(90.0, 10.0) - 0.9).abs() < 1e-12);
}

#[test]
fn ts_moving_and_ema() {
    let xs = [1.0, 2.0, 3.0, 4.0];
    let ma = moving_average(&xs, 2);
    assert_eq!(ma.len(), 3);
    assert!((ma[0] - 1.5).abs() < 1e-12);
    assert!((ma[1] - 2.5).abs() < 1e-12);
    assert!((ma[2] - 3.5).abs() < 1e-12);

    let ema = exponential_moving_average(&xs, 0.5);
    assert_eq!(ema.len(), 4);
    // simple progression sanity checks
    assert!(ema[1] > ema[0]);
    assert!(ema[2] > ema[1]);
}

#[test]
fn ts_rolling_std() {
    let xs = [1.0, 2.0, 3.0, 4.0];
    let rs = rolling_std(&xs, 2);
    assert_eq!(rs.len(), 3);
    // stddev of pairs: [1,2],[2,3],[3,4] is sqrt(0.5)
    for v in rs {
        assert!((v - std::f64::consts::FRAC_1_SQRT_2).abs() < 1e-12);
    }
}
