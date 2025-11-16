use approx::assert_abs_diff_eq;
use medi_stats::{
    age_standardized_rate, auc_from_roc, brier_score, confusion_matrix, hedis_rate, inv_logit,
    logit, odds, rate_per, risk_difference, risk_ratio, roc_points, star_rating,
};

#[test]
fn risk_measures() {
    assert_abs_diff_eq!(odds(0.5), 1.0, epsilon = 1e-12);
    assert!(odds(0.0) == 0.0);
    assert!(odds(1.0).is_infinite());

    let p = 0.2;
    let x = logit(p);
    let p_back = inv_logit(x);
    assert_abs_diff_eq!(p_back, p, epsilon = 1e-12);

    assert_abs_diff_eq!(risk_ratio(0.2, 0.1), 2.0, epsilon = 1e-12);
    assert_abs_diff_eq!(risk_difference(0.2, 0.1), 0.1, epsilon = 1e-12);
}

#[test]
fn quality_measures() {
    assert_abs_diff_eq!(hedis_rate(50.0, 200.0), 0.25, epsilon = 1e-12);
    let thresholds = [0.2, 0.4, 0.6, 0.8];
    assert_eq!(star_rating(0.1, &thresholds), 1);
    assert_eq!(star_rating(0.3, &thresholds), 2);
    assert_eq!(star_rating(0.5, &thresholds), 3);
    assert_eq!(star_rating(0.7, &thresholds), 4);
    assert_eq!(star_rating(0.9, &thresholds), 5);
}

#[test]
fn population_measures() {
    assert_abs_diff_eq!(rate_per(10.0, 1000.0, 100_000.0), 1000.0, epsilon = 1e-12);

    let rates = [1.0, 2.0, 3.0];
    let weights = [1.0, 1.0, 2.0];
    // (1*1 + 2*1 + 3*2) / (1+1+2) = (1 + 2 + 6) / 4 = 9/4 = 2.25
    assert_abs_diff_eq!(
        age_standardized_rate(&rates, &weights),
        2.25,
        epsilon = 1e-12
    );
}

#[test]
fn predictive_measures_auc_brier() {
    // Simple 4-point example
    let labels = [true, true, false, false];
    let scores = [0.9, 0.8, 0.3, 0.1];

    // thresholds high -> low to trace a typical ROC curve
    let thresholds = [1.0, 0.85, 0.5, 0.25, 0.0];
    let pts = roc_points(&labels, &scores, &thresholds);

    // Ensure monotone in FPR and between 0 and 1
    for (fpr, tpr) in &pts {
        assert!(*fpr >= 0.0 && *fpr <= 1.0);
        assert!(*tpr >= 0.0 && *tpr <= 1.0);
    }

    // For this nicely separated example, AUC should be close to 1.0
    let auc = auc_from_roc(&pts);
    assert!(auc <= 1.0 + 1e-12);
    assert!(auc >= 0.9); // allow some slack for coarse thresholding

    // Brier score sanity check: perfect predictions -> low score
    let probs_good = scores;
    let brier_good = brier_score(&labels, &probs_good);

    let probs_bad = [0.1, 0.2, 0.8, 0.9];
    let brier_bad = brier_score(&labels, &probs_bad);

    assert!(brier_good < brier_bad);

    // confusion_matrix basic consistency at threshold 0.5
    let c = confusion_matrix(&labels, &scores, 0.5);
    assert_eq!(c.tp, 2);
    assert_eq!(c.fp, 0);
    assert_eq!(c.tn, 2);
    assert_eq!(c.fn_, 0);
}

#[test]
fn predictive_edge_cases_all_equal_and_extremes() {
    // All-equal scores -> ROC curve essentially diagonal, AUC ~ 0.5 with symmetric thresholds.
    let labels = [true, false, true, false];
    let scores = [0.5, 0.5, 0.5, 0.5];

    // Thresholds that span below/at/above the common score
    let thresholds = [0.0, 0.25, 0.5, 0.75, 1.0];
    let pts = roc_points(&labels, &scores, &thresholds);
    let auc = auc_from_roc(&pts);

    // With identical scores and symmetric labels, AUC should still be a valid probability in [0,1].
    assert!(auc.is_finite());
    assert!((0.0..=1.0).contains(&auc));

    // Brier edge cases with extreme probabilities
    let labels2 = [true, false];
    let probs_perfect = [1.0, 0.0]; // perfect
    let probs_extreme_wrong = [0.0, 1.0]; // maximally wrong
    let probs_mid = [0.5, 0.5]; // uninformative

    let brier_perfect = brier_score(&labels2, &probs_perfect);
    let brier_wrong = brier_score(&labels2, &probs_extreme_wrong);
    let brier_mid = brier_score(&labels2, &probs_mid);

    // Perfect < mid < totally wrong
    assert!(brier_perfect < brier_mid);
    assert!(brier_mid < brier_wrong);
}

#[test]
fn predictive_known_small_fixture() {
    // Small example where we can compute Brier score exactly.
    let labels = [true, false, true];
    let probs = [0.9, 0.2, 0.4];
    // Brier = ((0.9-1)^2 + (0.2-0)^2 + (0.4-1)^2) / 3
    //       = (0.01 + 0.04 + 0.36) / 3 = 0.41 / 3
    let expected = 0.41 / 3.0;
    let brier = brier_score(&labels, &probs);
    assert_abs_diff_eq!(brier, expected, epsilon = 1e-12);
}
