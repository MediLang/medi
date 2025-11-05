use criterion::{black_box, criterion_group, criterion_main, Criterion};
use medi_data::{
    fhir::{FHIRObservation, FHIRPatient},
    fhir_any::FHIRAny,
    fhir_query,
    query::Predicate,
};

fn mk_any(n: usize) -> Vec<FHIRAny> {
    let mut v = Vec::with_capacity(n);
    for i in 0..n {
        if i % 2 == 0 {
            v.push(FHIRAny::Patient(FHIRPatient {
                id: format!("p{i}"),
                given_name: Some(format!("Name{i}")),
                family_name: Some(if i % 4 == 0 {
                    "Card".into()
                } else {
                    "Other".into()
                }),
                birth_date: Some("1980-01-01".into()),
            }));
        } else {
            v.push(FHIRAny::Observation(FHIRObservation {
                id: format!("o{i}"),
                code: if i % 3 == 0 {
                    "HR".into()
                } else {
                    "TEMP".into()
                },
                value: Some((60 + (i % 40)) as f64),
                unit: Some(if i % 3 == 0 { "bpm".into() } else { "C".into() }),
            }));
        }
    }
    v
}

fn bench_any_or_groups(c: &mut Criterion) {
    let data = mk_any(100_000);
    let q = fhir_query("Any")
        .any_in_group(vec![
            Predicate::ContainsCi {
                key: "family_name".into(),
                value: "card".into(),
            },
            Predicate::EqCi {
                key: "code".into(),
                value: "hr".into(),
            },
        ])
        .build();

    c.bench_function("any_query_or_groups", |b| {
        b.iter(|| {
            let res = q.execute_any(black_box(&data));
            black_box(res.len())
        })
    });
}

criterion_group!(benches, bench_any_or_groups);
criterion_main!(benches);
