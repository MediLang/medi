use criterion::{criterion_group, criterion_main, BatchSize, Criterion};
use tolvex_data::{hl7_fhir::hl7_to_fhir_patient_minimal, testdata::bundle_factory};

fn bench_hl7_to_fhir(c: &mut Criterion) {
    // Minimal HL7 with PID fields
    let hl7 = "MSH|^~\\&|SRC|FAC|DST|HOSP|202501010101||ADT^A01|123|P|2.5\r\
               PID|1|ALTID|P12345||Doe^Jane||19851224|F\r";
    c.bench_function("hl7_to_fhir_minimal", |b| {
        b.iter(|| {
            let p = hl7_to_fhir_patient_minimal(hl7).expect("ok");
            criterion::black_box(p);
        })
    });
}

fn bench_bundle_validate(c: &mut Criterion) {
    c.bench_function("bundle_validate_100", |b| {
        b.iter_batched(
            || bundle_factory(100),
            |bndl| {
                bndl.validate().expect("valid");
                criterion::black_box(bndl);
            },
            BatchSize::SmallInput,
        )
    });

    c.bench_function("bundle_validate_1000", |b| {
        b.iter_batched(
            || bundle_factory(1000),
            |bndl| {
                bndl.validate().expect("valid");
                criterion::black_box(bndl);
            },
            BatchSize::SmallInput,
        )
    });
}

criterion_group!(benches, bench_hl7_to_fhir, bench_bundle_validate);
criterion_main!(benches);
