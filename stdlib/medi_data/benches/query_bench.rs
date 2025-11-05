use criterion::{black_box, criterion_group, criterion_main, Criterion};
use medi_data::{fhir::FHIRPatient, fhir_query};

fn mk_patients(n: usize) -> Vec<FHIRPatient> {
    (0..n)
        .map(|i| FHIRPatient {
            id: format!("p{i}"),
            given_name: Some(format!("Name{i}")),
            family_name: Some(format!("Family{i}")),
            birth_date: Some("1980-01-01".into()),
        })
        .collect()
}

fn bench_patient_query(c: &mut Criterion) {
    let data = mk_patients(50_000);
    let q = fhir_query("Patient")
        .filter_contains_ci("family_name", "9999")
        .build();

    c.bench_function("patient_query_contains_ci", |b| {
        b.iter(|| {
            let res = q.execute_patients(black_box(&data));
            black_box(res.len())
        })
    });
}

criterion_group!(benches, bench_patient_query);
criterion_main!(benches);
