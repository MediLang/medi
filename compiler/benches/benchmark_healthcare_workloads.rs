use medi_ai::RiskScorer;
use medi_compliance::{run_hipaa_bundle, HipaaKeywordRule, RuleSeverity};
use medi_data::{
    fhir_query,
    hl7_fhir::hl7_to_fhir_patient_minimal,
    sanitize::{normalize_patient_birth_date, trim_patient_names},
    testdata::{bundle_factory, synthetic_lab_results},
    validate::{validate_observation, validate_patient},
};
use medi_stats::t_test_welch;
use memory_stats::memory_stats;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::fs;
use std::process::Command;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

#[derive(Debug, Serialize, Deserialize)]
struct WorkloadResult {
    name: String,
    iterations: usize,
    avg_time_ms: f64,
    min_time_ms: f64,
    max_time_ms: f64,
    rss_mb: Option<f64>,
    meta: BTreeMap<String, String>,
}

fn now_ts() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or(Duration::from_secs(0))
        .as_secs()
}

fn get_cpu_info() -> String {
    if cfg!(target_os = "linux") {
        if let Ok(info) = fs::read_to_string("/proc/cpuinfo") {
            for line in info.lines() {
                if line.starts_with("model name") {
                    return line
                        .split(':')
                        .nth(1)
                        .unwrap_or("Unknown")
                        .trim()
                        .to_string();
                }
            }
        }
    }
    "Unknown".to_string()
}

fn get_mem_info() -> String {
    if cfg!(target_os = "linux") {
        if let Ok(info) = fs::read_to_string("/proc/meminfo") {
            for line in info.lines() {
                if line.starts_with("MemTotal") {
                    if let Some(kb) = line.split_whitespace().nth(1) {
                        if let Ok(kb) = kb.parse::<u64>() {
                            return format!("{:.1} GB", kb as f64 / 1024.0 / 1024.0);
                        }
                    }
                }
            }
        }
    }
    "Unknown".to_string()
}

fn rust_version() -> String {
    Command::new("rustc")
        .arg("--version")
        .output()
        .ok()
        .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
        .unwrap_or_else(|| "rustc (unknown)".to_string())
}

fn bench<F>(name: &str, iterations: usize, mut f: F) -> WorkloadResult
where
    F: FnMut() -> BTreeMap<String, String>,
{
    let mut times: Vec<f64> = Vec::with_capacity(iterations);
    let mut meta: BTreeMap<String, String> = BTreeMap::new();

    // Warm-up once
    let _ = f();

    for _ in 0..iterations {
        let start = Instant::now();
        let this_meta = f();
        let elapsed = start.elapsed().as_secs_f64() * 1000.0;
        times.push(elapsed);
        // Keep the most recent meta snapshot
        meta = this_meta;
    }

    let (min_time_ms, max_time_ms) = times
        .iter()
        .copied()
        .fold((f64::INFINITY, f64::NEG_INFINITY), |(min, max), x| {
            (f64::min(min, x), f64::max(max, x))
        });
    let avg_time_ms = times.iter().sum::<f64>() / times.len().max(1) as f64;

    let rss_mb = memory_stats().map(|s| s.physical_mem as f64 / (1024.0 * 1024.0));

    WorkloadResult {
        name: name.to_string(),
        iterations,
        avg_time_ms,
        min_time_ms,
        max_time_ms,
        rss_mb,
        meta,
    }
}

fn parse_subresults_container(container: WorkloadResult) -> Vec<WorkloadResult> {
    if let Some(s) = container.meta.get("subresults") {
        if let Ok(v) = serde_json::from_str::<Vec<WorkloadResult>>(s) {
            return v;
        }
    }
    vec![container]
}

fn try_run_python(bench_dir: &std::path::Path) -> Option<Vec<WorkloadResult>> {
    let script = bench_dir.join("python").join("workloads.py");
    if !script.exists() {
        return None;
    }

    let out = Command::new("python3").arg(script).output().ok()?;

    if !out.status.success() {
        return Some(vec![WorkloadResult {
            name: "python_workloads".to_string(),
            iterations: 0,
            avg_time_ms: 0.0,
            min_time_ms: 0.0,
            max_time_ms: 0.0,
            rss_mb: None,
            meta: BTreeMap::from([
                ("status".to_string(), format!("failed: {}", out.status)),
                (
                    "stderr".to_string(),
                    String::from_utf8_lossy(&out.stderr).to_string(),
                ),
            ]),
        }]);
    }

    let container = serde_json::from_slice::<WorkloadResult>(&out.stdout).ok()?;
    Some(parse_subresults_container(container))
}

fn try_run_r(bench_dir: &std::path::Path) -> Option<Vec<WorkloadResult>> {
    let script = bench_dir.join("r").join("workloads.R");
    if !script.exists() {
        return None;
    }

    let out = Command::new("Rscript").arg(script).output().ok()?;

    if !out.status.success() {
        return Some(vec![WorkloadResult {
            name: "r_workloads".to_string(),
            iterations: 0,
            avg_time_ms: 0.0,
            min_time_ms: 0.0,
            max_time_ms: 0.0,
            rss_mb: None,
            meta: BTreeMap::from([
                ("status".to_string(), format!("failed: {}", out.status)),
                (
                    "stderr".to_string(),
                    String::from_utf8_lossy(&out.stderr).to_string(),
                ),
            ]),
        }]);
    }

    let container = serde_json::from_slice::<WorkloadResult>(&out.stdout).ok()?;
    Some(parse_subresults_container(container))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let bench_dir = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = bench_dir.join("benchdata");
    fs::create_dir_all(&out_dir)?;

    let mut results: Vec<WorkloadResult> = Vec::new();

    // Workload 1: HL7 -> FHIR patient + sanitize + validate
    results.push(bench("medi_data_hl7_patient_pipeline", 20, || {
        let hl7 = "MSH|^~\\&|SRC|FAC|DST|HOSP|202501010101||ADT^A01|123|P|2.5\r\
PID|1|ALTID|P12345||Doe^Jane||19851224|F\r";
        let mut p = hl7_to_fhir_patient_minimal(hl7).expect("ok");
        trim_patient_names(&mut p);
        normalize_patient_birth_date(&mut p);
        validate_patient(&p).expect("valid");
        let mut meta = BTreeMap::new();
        meta.insert("patient_id".to_string(), p.id);
        meta
    }));

    // Workload 2: Observations validate + query
    results.push(bench("medi_data_observation_validate_query", 20, || {
        let obs = synthetic_lab_results(20_000);
        for o in &obs {
            validate_observation(o).expect("valid");
        }
        let q = fhir_query("Observation")
            .filter_eq_ci("code", "HGB")
            .build();
        let matched = q.execute_observations(&obs);
        let mut meta = BTreeMap::new();
        meta.insert("observations".to_string(), obs.len().to_string());
        meta.insert("matched".to_string(), matched.len().to_string());
        meta
    }));

    // Workload 3: Bundle validate (patients)
    results.push(bench("medi_data_bundle_validate", 10, || {
        let b = bundle_factory(5_000);
        b.validate().expect("valid");
        let mut meta = BTreeMap::new();
        meta.insert("entries".to_string(), b.entries.len().to_string());
        meta
    }));

    // Workload 4: Stats t-test (Welch)
    results.push(bench("medi_stats_t_test_welch", 1_000, || {
        let a = [1.0, 2.0, 3.0, 4.0];
        let b = [2.0, 3.0, 4.0, 5.0];
        let res = t_test_welch(&a, &b);
        let mut meta = BTreeMap::new();
        meta.insert("t_stat".to_string(), format!("{:.3}", res.t_stat));
        meta.insert("df".to_string(), format!("{:.1}", res.df));
        meta
    }));

    // Workload 5: HIPAA keyword scan
    results.push(bench("medi_compliance_hipaa_keywords", 1_000, || {
        let rules = vec![HipaaKeywordRule {
            id: "k1".into(),
            description: "Detect SSN".into(),
            keywords: vec!["ssn".into(), "social security".into()],
            severity: RuleSeverity::Error,
        }];
        let text = "Patient SSN: 123-45-6789. social security number present.";
        let report = run_hipaa_bundle(text, &rules, None, None, None, Some(400), &[]);
        let mut meta = BTreeMap::new();
        meta.insert("failed".to_string(), report.overall.failed.to_string());
        meta
    }));

    // Workload 6: AI risk scorer
    results.push(bench("medi_ai_risk_score", 1000, || {
        let model = RiskScorer {
            weights: vec![0.2, 0.5, 0.3],
            bias: 0.1,
            model_name: "demo".into(),
        };
        let features = [0.4, 0.2, 0.9];
        let score = model.predict(&features);
        let mut meta = BTreeMap::new();
        meta.insert("score".to_string(), format!("{score:.6}"));
        meta
    }));

    // Optional comparators
    if let Some(py_rows) = try_run_python(&bench_dir) {
        results.extend(py_rows);
    }
    if let Some(r_rows) = try_run_r(&bench_dir) {
        results.extend(r_rows);
    }

    // Write outputs
    let ts = now_ts();
    let json_path = out_dir.join(format!("healthcare_workloads_{ts}.json"));
    let md_path = out_dir.join(format!("healthcare_workloads_{ts}.md"));
    let json_latest_path = out_dir.join("healthcare_workloads_latest.json");
    let md_latest_path = out_dir.join("healthcare_workloads_latest.md");

    let json_payload = serde_json::to_string_pretty(&results)?;
    fs::write(&json_path, &json_payload)?;
    fs::write(&json_latest_path, &json_payload)?;

    // Markdown report
    let mut md = String::new();
    md.push_str("# Healthcare Workload Benchmarks\n\n");
    md.push_str("This report is generated by `cargo run -p medic_benchmarks --bin benchmark_healthcare_workloads --release`.\n\n");
    md.push_str("## Environment\n\n");
    md.push_str(&format!("- CPU: {}\n", get_cpu_info()));
    md.push_str(&format!("- RAM: {}\n", get_mem_info()));
    md.push_str(&format!("- Rust: {}\n", rust_version()));
    md.push_str("- Build: `--release`\n\n");

    md.push_str("## Results\n\n");
    md.push_str("| Workload | Iterations | Avg (ms) | Min (ms) | Max (ms) | RSS (MB) |\n");
    md.push_str("|---|---:|---:|---:|---:|---:|\n");
    for r in &results {
        md.push_str(&format!(
            "| {} | {} | {:.3} | {:.3} | {:.3} | {} |\n",
            r.name,
            r.iterations,
            r.avg_time_ms,
            r.min_time_ms,
            r.max_time_ms,
            r.rss_mb
                .map(|m| format!("{m:.1}"))
                .unwrap_or_else(|| "".to_string())
        ));
    }
    md.push_str("\n## Notes\n\n");
    md.push_str("- RSS is process-level and best-effort; use a profiler for per-workload allocation detail.\n");
    md.push_str(
        "- Python/R comparators only run if scripts exist and interpreters are available.\n",
    );

    fs::write(&md_path, &md)?;
    fs::write(&md_latest_path, &md)?;

    // Also place a copy under docs so MkDocs can include it without reaching outside docs_dir.
    let docs_out = bench_dir
        .join("..")
        .join("..")
        .join("docs")
        .join("content")
        .join("technical")
        .join("healthcare_workloads_latest.md");
    let _ = fs::write(&docs_out, &md);

    eprintln!(
        "Wrote {}, {}, {} and {}",
        json_path.display(),
        md_path.display(),
        json_latest_path.display(),
        md_latest_path.display()
    );
    Ok(())
}
