use std::collections::HashMap;

pub type Record = HashMap<String, String>;

fn qi_key(record: &Record, quasi_identifiers: &[&str]) -> Vec<String> {
    quasi_identifiers
        .iter()
        .map(|k| record.get(*k).cloned().unwrap_or_default())
        .collect()
}

pub fn equivalence_classes<'a>(
    records: &'a [Record],
    quasi_identifiers: &[&str],
) -> HashMap<Vec<String>, Vec<&'a Record>> {
    let mut classes: HashMap<Vec<String>, Vec<&'a Record>> = HashMap::new();
    for r in records {
        let key = qi_key(r, quasi_identifiers);
        classes.entry(key).or_default().push(r);
    }
    classes
}

pub fn k_anonymity(records: &[Record], quasi_identifiers: &[&str], k: usize) -> bool {
    if k == 0 {
        return true;
    }
    let classes = equivalence_classes(records, quasi_identifiers);
    classes.values().all(|c| c.len() >= k)
}

pub fn l_diversity(
    records: &[Record],
    quasi_identifiers: &[&str],
    sensitive_attr: &str,
    l: usize,
) -> bool {
    if l == 0 {
        return true;
    }
    let classes = equivalence_classes(records, quasi_identifiers);
    classes.values().all(|class| {
        let mut distinct: HashMap<String, ()> = HashMap::new();
        for r in class {
            if let Some(v) = r.get(sensitive_attr) {
                distinct.insert(v.clone(), ());
            }
        }
        distinct.len() >= l
    })
}

fn distribution(records: &[&Record], sensitive_attr: &str) -> HashMap<String, f64> {
    let mut counts: HashMap<String, u64> = HashMap::new();
    for r in records {
        let v = r.get(sensitive_attr).cloned().unwrap_or_default();
        *counts.entry(v).or_insert(0) += 1;
    }
    let total = records.len() as f64;
    let mut dist: HashMap<String, f64> = HashMap::new();
    if total == 0.0 {
        return dist;
    }
    for (k, c) in counts {
        dist.insert(k, (c as f64) / total);
    }
    dist
}

fn total_variation_distance(p: &HashMap<String, f64>, q: &HashMap<String, f64>) -> f64 {
    let mut keys: HashMap<&str, ()> = HashMap::new();
    for k in p.keys() {
        keys.insert(k.as_str(), ());
    }
    for k in q.keys() {
        keys.insert(k.as_str(), ());
    }

    let mut sum = 0.0;
    for k in keys.keys() {
        let pk = p.get(*k).copied().unwrap_or(0.0);
        let qk = q.get(*k).copied().unwrap_or(0.0);
        sum += (pk - qk).abs();
    }
    0.5 * sum
}

pub fn t_closeness(
    records: &[Record],
    quasi_identifiers: &[&str],
    sensitive_attr: &str,
    t: f64,
) -> bool {
    if t < 0.0 {
        return false;
    }

    let all_refs: Vec<&Record> = records.iter().collect();
    let global = distribution(&all_refs, sensitive_attr);

    let classes = equivalence_classes(records, quasi_identifiers);
    classes.values().all(|class| {
        let class_dist = distribution(class, sensitive_attr);
        total_variation_distance(&class_dist, &global) <= t
    })
}
