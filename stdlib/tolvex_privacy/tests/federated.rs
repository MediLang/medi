use approx::assert_abs_diff_eq;

use tolvex_privacy::{fedavg, FederatedCoordinator, ModelUpdate};

#[test]
fn fedavg_weighted_average() {
    let u1 = ModelUpdate::new("a", vec![1.0, 1.0], 1);
    let u2 = ModelUpdate::new("b", vec![3.0, 5.0], 3);
    let agg = fedavg(&[u1, u2]).expect("agg");

    // weighted by num_examples: (1/4)*[1,1] + (3/4)*[3,5] = [2.5, 4.0]
    assert_abs_diff_eq!(agg[0], 2.5, epsilon = 1e-12);
    assert_abs_diff_eq!(agg[1], 4.0, epsilon = 1e-12);
}

#[test]
fn fedavg_rejects_mismatched_dims_or_empty() {
    assert!(fedavg(&[]).is_none());

    let u1 = ModelUpdate::new("a", vec![1.0], 1);
    let u2 = ModelUpdate::new("b", vec![1.0, 2.0], 1);
    assert!(fedavg(&[u1, u2]).is_none());
}

#[test]
fn coordinator_collects_and_aggregates() {
    let mut c = FederatedCoordinator::new();
    assert_eq!(c.num_updates(1), 0);

    c.submit_update(1, ModelUpdate::new("a", vec![0.0, 1.0], 1));
    c.submit_update(1, ModelUpdate::new("b", vec![2.0, 3.0], 1));

    assert_eq!(c.num_updates(1), 2);

    let agg = c.aggregate_round(1).expect("agg");
    assert_abs_diff_eq!(agg[0], 1.0, epsilon = 1e-12);
    assert_abs_diff_eq!(agg[1], 2.0, epsilon = 1e-12);

    assert!(c.aggregate_round_min_clients(1, 3).is_none());
    assert!(c.aggregate_round_min_clients(1, 2).is_some());
}
