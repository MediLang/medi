use medi_ai::bias::compute_group_bias;

#[test]
fn compute_group_bias_is_deterministic_and_aggregates() {
    let preds = vec![
        (0.2, "group_a".to_string()),
        (0.4, "group_a".to_string()),
        (0.8, "group_b".to_string()),
    ];

    let mut metrics = compute_group_bias(&preds);
    metrics.sort_by(|a, b| a.group.cmp(&b.group));

    assert_eq!(metrics.len(), 2);
    assert_eq!(metrics[0].group, "group_a");
    assert_eq!(metrics[0].count, 2);
    assert!((metrics[0].mean_score - 0.3).abs() < 1e-12);

    assert_eq!(metrics[1].group, "group_b");
    assert_eq!(metrics[1].count, 1);
    assert!((metrics[1].mean_score - 0.8).abs() < 1e-12);
}
