use criterion::{black_box, criterion_group, criterion_main, Criterion};
use medi_data::{
    fhir::FHIRObservation,
    fhir_query,
    sanitize::{canonize_observation_code, normalize_observation_unit},
};

fn mk_observations(n: usize) -> Vec<FHIRObservation> {
    (0..n)
        .map(|i| FHIRObservation {
            id: format!("o{i}"),
            code: if i % 2 == 0 {
                " hr ".into()
            } else {
                "TEMP".into()
            },
            value: Some((60 + (i % 40)) as f64),
            unit: if i % 2 == 0 {
                Some(" BPM ".into())
            } else {
                Some("C".into())
            },
        })
        .collect()
}

fn bench_obs_query(c: &mut Criterion) {
    let mut data = mk_observations(50_000);
    let q_hr_bpm = fhir_query("Observation")
        .filter_eq_ci("code", "HR")
        .filter_eq_ci("unit", "bpm")
        .build();

    // Without sanitization (likely few matches)
    c.bench_function("obs_query_no_sanitize", |b| {
        b.iter(|| {
            let res = q_hr_bpm.execute_observations(black_box(&data));
            black_box(res.len())
        })
    });

    // With sanitization
    for o in &mut data {
        canonize_observation_code(o);
        normalize_observation_unit(o);
    }
    c.bench_function("obs_query_with_sanitize", |b| {
        b.iter(|| {
            let res = q_hr_bpm.execute_observations(black_box(&data));
            black_box(res.len())
        })
    });
}

criterion_group!(benches, bench_obs_query);
criterion_main!(benches);
