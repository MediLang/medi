use approx::assert_abs_diff_eq;
use tolvex_stats::{
    absolute_risk_reduction, bin_series, cohen_d, normalize_min_max, number_needed_to_treat,
    pooled_stddev, relative_risk,
};

#[test]
fn clinical_risk_reduction_rr_nnt() {
    let cer = 0.20;
    let ter = 0.10;

    assert_abs_diff_eq!(absolute_risk_reduction(cer, ter), 0.10, epsilon = 1e-12);
    assert_abs_diff_eq!(relative_risk(cer, ter), 0.5, epsilon = 1e-12);
    assert_abs_diff_eq!(number_needed_to_treat(cer, ter), 10.0, epsilon = 1e-12);

    // CER = 0 produces NaN RR
    assert!(relative_risk(0.0, 0.1).is_nan());

    // ARR == 0 produces infinite NNT
    assert!(number_needed_to_treat(0.1, 0.1).is_infinite());
}

#[test]
fn clinical_pooled_stddev_basic_and_edge_cases() {
    // Identical stddevs and sample sizes -> pooled is that stddev.
    let s = 2.0;
    let pooled = pooled_stddev(s, 10, s, 10);
    assert_abs_diff_eq!(pooled, s, epsilon = 1e-12);

    // Edge case: insufficient n -> NaN
    assert!(pooled_stddev(1.0, 1, 1.0, 10).is_nan());
    assert!(pooled_stddev(1.0, 10, 1.0, 1).is_nan());
}

#[test]
fn viz_bin_series_counts_and_edges() {
    let xs = [0.0, 0.1, 0.9, 1.0, 1.1, 2.0];
    let edges = [0.0, 1.0, 2.0];

    // bins: [0,1), [1,2] (upper endpoint inclusive for last edge)
    let counts = bin_series(&xs, &edges);
    assert_eq!(counts, vec![3, 3]);

    // Too few edges -> empty
    let empty = bin_series(&xs, &[0.0]);
    assert!(empty.is_empty());
}

#[test]
fn viz_normalize_min_max_basic_and_degenerate() {
    let xs = [2.0, 4.0, 6.0];
    let out = normalize_min_max(&xs);
    assert_eq!(out.len(), 3);
    assert_abs_diff_eq!(out[0], 0.0, epsilon = 1e-12);
    assert_abs_diff_eq!(out[1], 0.5, epsilon = 1e-12);
    assert_abs_diff_eq!(out[2], 1.0, epsilon = 1e-12);

    let empty = normalize_min_max(&[]);
    assert!(empty.is_empty());

    // Degenerate range -> all zeros
    let same = [3.0, 3.0, 3.0];
    let out2 = normalize_min_max(&same);
    assert_eq!(out2, vec![0.0, 0.0, 0.0]);
}

#[test]
fn power_cohen_d_basic() {
    // simple effect size
    assert_abs_diff_eq!(cohen_d(2.0, 1.0, 2.0), 0.5, epsilon = 1e-12);

    // sd <= 0 => returns 0 per implementation
    assert_eq!(cohen_d(2.0, 1.0, 0.0), 0.0);
    assert_eq!(cohen_d(2.0, 1.0, -1.0), 0.0);
}
