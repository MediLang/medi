use criterion::{criterion_group, criterion_main, BatchSize, Criterion};
use medi_compliance::{evaluate_rule_expr, load_builtin_rules, ComplianceStandard};

fn make_large_json(size_bytes: usize) -> serde_json::Value {
    // Build ~size_bytes JSON by repeating a phrase
    let base = "Patient note lorem ipsum SSN 123-45-6789 phone (555)123-4567 email a@b.co.";
    let mut s = String::new();
    while s.len() < size_bytes {
        s.push_str(base);
        s.push('\n');
    }
    serde_json::json!({ "note": s, "tags": ["phi"], "age": 17, "patient": {"id": "p1"} })
}

fn bench_rules(c: &mut Criterion) {
    let hipaa_rules = load_builtin_rules(ComplianceStandard::Hipaa);
    // Duplicate rules to reach ~100 items
    let mut rules = Vec::new();
    for _ in 0..10 {
        rules.extend(hipaa_rules.clone());
    }

    let data = make_large_json(1_200_000); // ~1.2MB

    c.bench_function("evaluate 100 rules on ~1MB", |b| {
        b.iter_batched(
            || data.clone(),
            |d| {
                for r in &rules {
                    if let Some(expr) = &r.expr {
                        let _ = evaluate_rule_expr(&d, expr);
                    }
                }
            },
            BatchSize::SmallInput,
        )
    });
}

criterion_group!(benches, bench_rules);
criterion_main!(benches);
