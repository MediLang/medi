use approx::assert_abs_diff_eq;
use tolvex_stats::{LinearRegression1D, LogisticRegression1D, StandardScaler};

#[test]
fn standard_scaler_roundtrip_and_degenerate() {
    let xs = [1.0, 2.0, 3.0, 4.0];
    let sc = StandardScaler::fit(&xs);

    // round trip
    for &x in &xs {
        let z = sc.transform(x);
        let x2 = sc.inverse_transform(z);
        assert_abs_diff_eq!(x2, x, epsilon = 1e-12);
    }

    // degenerate
    let ys = [5.0, 5.0, 5.0];
    let sc2 = StandardScaler::fit(&ys);
    assert_eq!(sc2.transform(5.0), 0.0);
    assert_abs_diff_eq!(sc2.inverse_transform(123.0), 5.0, epsilon = 1e-12);
}

#[test]
fn linear_regression_1d_fits_line() {
    // y = 2x + 1
    let xs = [0.0, 1.0, 2.0, 3.0];
    let ys = [1.0, 3.0, 5.0, 7.0];

    let m = LinearRegression1D::fit(&xs, &ys).expect("fit");
    assert_abs_diff_eq!(m.slope, 2.0, epsilon = 1e-12);
    assert_abs_diff_eq!(m.intercept, 1.0, epsilon = 1e-12);
    assert_abs_diff_eq!(m.predict(4.0), 9.0, epsilon = 1e-12);
}

#[test]
fn linear_regression_1d_rejects_degenerate_x() {
    let xs = [1.0, 1.0, 1.0];
    let ys = [0.0, 1.0, 2.0];
    assert!(LinearRegression1D::fit(&xs, &ys).is_none());
}

#[test]
fn logistic_regression_1d_learns_monotone_separation() {
    // Labels switch from false to true as x increases
    let xs = [0.0, 1.0, 2.0, 3.0, 4.0, 5.0];
    let ys = [false, false, false, true, true, true];

    let model = LogisticRegression1D::fit(&xs, &ys, 0.1, 2000).expect("fit");

    // Higher x should yield higher probability
    let p_lo = model.predict_proba(1.0);
    let p_hi = model.predict_proba(4.0);
    assert!(p_hi > p_lo);

    // Classification sanity at 0.5 threshold
    assert!(!model.predict(0.0, 0.5));
    assert!(model.predict(5.0, 0.5));
}

#[test]
fn logistic_regression_1d_rejects_bad_inputs() {
    let xs = [0.0, 1.0];
    let ys = [true];
    assert!(LogisticRegression1D::fit(&xs, &ys, 0.1, 10).is_none());
    assert!(LogisticRegression1D::fit(&[0.0], &[true], 0.0, 10).is_none());
    assert!(LogisticRegression1D::fit(&[0.0], &[true], 0.1, 0).is_none());
}
