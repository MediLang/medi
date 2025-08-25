use std::fs::{self, File};
use std::io::Write;
use std::path::{Path, PathBuf};

fn ensure_dir<P: AsRef<Path>>(p: P) {
    if let Err(e) = fs::create_dir_all(&p) {
        eprintln!("Failed to create dir {}: {}", p.as_ref().display(), e);
    }
}

fn repeat_to_size(mut f: &File, chunk: &str, target_bytes: usize) -> std::io::Result<usize> {
    let mut written = 0usize;
    let bytes = chunk.as_bytes();
    while written < target_bytes {
        f.write_all(bytes)?;
        written += bytes.len();
    }
    Ok(written)
}

fn main() -> std::io::Result<()> {
    let bench_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = bench_dir.join("benchdata");
    ensure_dir(&out_dir);

    let sizes = [
        (256 * 1024, "256k"),
        (512 * 1024, "512k"),
        (2 * 1024 * 1024, "2m"),
        (4 * 1024 * 1024, "4m"),
        (8 * 1024 * 1024, "8m"),
    ];

    // Function-heavy template: simple, valid function
    let func_tpl = r#"
// Function to calculate BMI
fn calculate_bmi(weight_kg: float, height_m: float) {
    let bmi = weight_kg / (height_m * height_m);
    return bmi;
}

"#;

    // Expression-heavy template: one function with many complex expressions
    let expr_tpl = r#"
fn expr_heavy() {
    let a = 1 + 2 * 3 - 4 / 5 + 6 % 7 - 8 * 9 + 10 - 11 + 12 * 13 - 14;
    let b = (1 + 2) * (3 + 4) - (5 - 6) * (7 + 8) / 9;
    let c = 100 - 3 * 4 + 5 % 6 - 7 + 8 * 9 - 10 / 2 + 11 * 12;
    let d = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10;
    let e = (1 + 2 * 3 - 4) * (5 + 6 - 7 * 8) + 9;
    return a + b + c + d + e;
}

"#;

    for (bytes, label) in sizes {
        // Function-heavy files
        let func_path = out_dir.join(format!("large_func_{}.medi", label));
        let mut f = File::create(&func_path)?;
        let wrote = repeat_to_size(&f, func_tpl, bytes)?;
        println!("Generated {} ({} bytes)", func_path.display(), wrote);

        // Expression-heavy files
        let expr_path = out_dir.join(format!("large_expr_{}.medi", label));
        let mut f2 = File::create(&expr_path)?;
        let wrote2 = repeat_to_size(&f2, expr_tpl, bytes)?;
        println!("Generated {} ({} bytes)", expr_path.display(), wrote2);
    }

    Ok(())
}
