use std::collections::HashMap;

use tolvex_privacy::{k_anonymity, l_diversity, t_closeness, Record};

fn make_record(pairs: &[(&str, &str)]) -> Record {
    let mut r = HashMap::new();
    for (k, v) in pairs {
        r.insert((*k).to_string(), (*v).to_string());
    }
    r
}

#[test]
fn k_anonymity_basic_and_failure() {
    let records = vec![
        make_record(&[("zip", "12345"), ("age", "30"), ("disease", "flu")]),
        make_record(&[("zip", "12345"), ("age", "30"), ("disease", "cold")]),
        make_record(&[("zip", "99999"), ("age", "40"), ("disease", "flu")]),
        make_record(&[("zip", "99999"), ("age", "40"), ("disease", "allergy")]),
    ];

    assert!(k_anonymity(&records, &["zip", "age"], 2));
    assert!(!k_anonymity(&records, &["zip", "age"], 3));
}

#[test]
fn l_diversity_basic_and_failure() {
    let records = vec![
        make_record(&[("zip", "12345"), ("age", "30"), ("disease", "flu")]),
        make_record(&[("zip", "12345"), ("age", "30"), ("disease", "cold")]),
        make_record(&[("zip", "12345"), ("age", "30"), ("disease", "allergy")]),
        make_record(&[("zip", "99999"), ("age", "40"), ("disease", "flu")]),
        make_record(&[("zip", "99999"), ("age", "40"), ("disease", "flu")]),
    ];

    // First class has 3 distinct diseases, second has 1
    assert!(l_diversity(&records, &["zip", "age"], "disease", 1));
    assert!(!l_diversity(&records, &["zip", "age"], "disease", 2));
}

#[test]
fn t_closeness_basic_and_failure() {
    let records = vec![
        make_record(&[("zip", "12345"), ("age", "30"), ("disease", "flu")]),
        make_record(&[("zip", "12345"), ("age", "30"), ("disease", "cold")]),
        make_record(&[("zip", "12345"), ("age", "30"), ("disease", "allergy")]),
        make_record(&[("zip", "99999"), ("age", "40"), ("disease", "flu")]),
        make_record(&[("zip", "99999"), ("age", "40"), ("disease", "flu")]),
    ];

    // With these distributions, TV distance per class is modest; expect pass for a loose t
    assert!(t_closeness(&records, &["zip", "age"], "disease", 0.6));

    // Very tight threshold should fail
    assert!(!t_closeness(&records, &["zip", "age"], "disease", 0.05));
}
