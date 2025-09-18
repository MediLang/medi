use std::fs::File;
use std::io::Write;

fn main() {
    let output_path = "benches/large_test_file.medi";
    let target_size = 1024 * 1024; // 1MB
    let target_tokens = 50_000;

    // Estimate tokens per line (average 20 tokens per line)
    let lines = target_tokens / 20;
    let _tokens_per_line = target_tokens / lines;

    let mut file = File::create(output_path).expect("Failed to create test file");

    // Write a simple function that we'll repeat
    let function_template = r#"
// Function to calculate BMI
fn calculate_bmi(weight_kg: float, height_m: float) -> float {
    weight_kg / (height_m * height_m)
}

// Function to check blood pressure
fn check_blood_pressure(systolic: int, diastolic: int) -> string {
    match (systolic, diastolic) {
        (s, d) if s < 120 && d < 80 => "Normal",
        (s, d) if s < 130 && d < 80 => "Elevated",
        (s, d) if s < 140 || d < 90 => "Stage 1 Hypertension",
        _ => "Stage 2 Hypertension"
    }
}
}
"#;

    // Write the template multiple times to reach target size
    let mut bytes_written = 0;
    let template_bytes = function_template.as_bytes();
    let template_size = template_bytes.len();

    while bytes_written < target_size {
        file.write_all(template_bytes)
            .expect("Failed to write to file");
        bytes_written += template_size;
    }

    println!("Generated test file: {output_path} ({bytes_written} bytes)");
}
