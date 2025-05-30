use criterion::{criterion_group, criterion_main, Criterion, black_box};

fn simple_addition() -> i32 {
    let x = 2 + 2;
    x
}

fn simple_benchmark(c: &mut Criterion) {
    println!("Starting simple benchmark...");
    
    // First, run the function directly to ensure it works
    let result = simple_addition();
    println!("Simple addition result: {}", result);
    
    // Then benchmark it
    c.bench_function("simple_addition", |b| {
        b.iter(|| {
            let x = simple_addition();
            black_box(x);
        })
    });
    
    println!("Benchmark setup complete");
}

criterion_group! {
    name = benches;
    config = Criterion::default()
        .sample_size(10)
        .measurement_time(std::time::Duration::from_secs(1));
    targets = simple_benchmark
}

criterion_main!(benches);
