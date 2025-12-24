use medi_ai::RiskScorer;
use medi_compliance::{run_hipaa_bundle, HipaaKeywordRule, RuleSeverity};
use medi_data::{
    fhir_query,
    hl7_fhir::hl7_to_fhir_patient_minimal,
    ndjson::{from_ndjson, to_ndjson},
    sanitize::{normalize_patient_birth_date, trim_patient_names},
    testdata::{bundle_factory, synthetic_lab_results},
    validate::{validate_observation, validate_patient},
    FHIRObservation,
};
use medi_stats::t_test_welch;
use memory_stats::memory_stats;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::env;
use std::fs;
use std::process::Command;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

#[derive(Debug, Serialize, Deserialize)]
struct WorkloadResult {
    name: String,
    iterations: usize,
    #[serde(default)]
    repeats: usize,
    avg_time_ms: f64,
    min_time_ms: f64,
    max_time_ms: f64,
    #[serde(default)]
    stddev_time_ms: f64,
    #[serde(default)]
    cv_pct: f64,
    rss_mb: Option<f64>,
    #[serde(default)]
    rss_before_mb: Option<f64>,
    #[serde(default)]
    rss_after_mb: Option<f64>,
    #[serde(default)]
    rss_delta_mb: Option<f64>,
    meta: BTreeMap<String, String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Tier {
    Small,
    Medium,
    Large,
    All,
}

#[derive(Debug, Clone)]
struct Config {
    tier: Tier,
    obs_n: usize,
    bundle_n: usize,
    iterations_fast: usize,
    iterations_medium: usize,
    iterations_slow: usize,
    repeats: usize,
    run_python: bool,
    run_r: bool,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            tier: Tier::Medium,
            obs_n: 20_000,
            bundle_n: 5_000,
            iterations_fast: 1_000,
            iterations_medium: 20,
            iterations_slow: 10,
            repeats: 1,
            run_python: true,
            run_r: true,
        }
    }
}

fn parse_usize(value: &str) -> Option<usize> {
    value.parse::<usize>().ok()
}

fn parse_tier(value: &str) -> Option<Tier> {
    match value {
        "small" => Some(Tier::Small),
        "medium" => Some(Tier::Medium),
        "large" => Some(Tier::Large),
        "all" => Some(Tier::All),
        _ => None,
    }
}

fn apply_tier_defaults(cfg: &mut Config) {
    match cfg.tier {
        Tier::Small => {
            cfg.obs_n = 5_000;
            cfg.bundle_n = 1_000;
            cfg.iterations_fast = 2_000;
            cfg.iterations_medium = 50;
            cfg.iterations_slow = 20;
        }
        Tier::Medium => {
            cfg.obs_n = 20_000;
            cfg.bundle_n = 5_000;
            cfg.iterations_fast = 1_000;
            cfg.iterations_medium = 20;
            cfg.iterations_slow = 10;
        }
        Tier::Large => {
            cfg.obs_n = 100_000;
            cfg.bundle_n = 25_000;
            cfg.iterations_fast = 500;
            cfg.iterations_medium = 10;
            cfg.iterations_slow = 3;
        }
        Tier::All => {}
    }
}

fn tier_name(tier: Tier) -> &'static str {
    match tier {
        Tier::Small => "small",
        Tier::Medium => "medium",
        Tier::Large => "large",
        Tier::All => "all",
    }
}

fn parse_config() -> Config {
    let mut cfg = Config::default();
    let mut args = env::args().skip(1);
    while let Some(a) = args.next() {
        match a.as_str() {
            "--tier" => {
                if let Some(v) = args.next().as_deref().and_then(parse_tier) {
                    cfg.tier = v;
                }
            }
            "--obs-n" => {
                if let Some(v) = args.next().as_deref().and_then(parse_usize) {
                    cfg.obs_n = v;
                }
            }
            "--bundle-n" => {
                if let Some(v) = args.next().as_deref().and_then(parse_usize) {
                    cfg.bundle_n = v;
                }
            }
            "--iterations-fast" => {
                if let Some(v) = args.next().as_deref().and_then(parse_usize) {
                    cfg.iterations_fast = v;
                }
            }
            "--iterations-medium" => {
                if let Some(v) = args.next().as_deref().and_then(parse_usize) {
                    cfg.iterations_medium = v;
                }
            }
            "--iterations-slow" => {
                if let Some(v) = args.next().as_deref().and_then(parse_usize) {
                    cfg.iterations_slow = v;
                }
            }
            "--repeats" => {
                if let Some(v) = args.next().as_deref().and_then(parse_usize) {
                    cfg.repeats = v.max(1);
                }
            }
            "--no-python" => cfg.run_python = false,
            "--no-r" => cfg.run_r = false,
            "--help" | "-h" => {
                eprintln!(
                    "benchmark_healthcare_workloads options:\n\
  --tier <small|medium|large|all>  Preset dataset/iteration defaults (default: medium)\n\
  --obs-n <N>               Number of synthetic observations (default: 20000)\n\
  --bundle-n <N>            Number of bundle entries (default: 5000)\n\
  --iterations-fast <N>     Iterations for fast workloads (default: 1000)\n\
  --iterations-medium <N>   Iterations for medium workloads (default: 20)\n\
  --iterations-slow <N>     Iterations for slow workloads (default: 10)\n\
  --repeats <N>             Repeat each workload N times and report stddev/CV (default: 1)\n\
  --no-python               Disable Python comparator\n\
  --no-r                    Disable R comparator\n"
                );
                return cfg;
            }
            _ => {}
        }
    }

    apply_tier_defaults(&mut cfg);
    cfg
}

fn now_ts() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or(Duration::from_secs(0))
        .as_secs()
}

fn mean(values: &[f64]) -> f64 {
    if values.is_empty() {
        return 0.0;
    }
    values.iter().sum::<f64>() / values.len() as f64
}

fn stddev_sample(values: &[f64]) -> f64 {
    let n = values.len();
    if n < 2 {
        return 0.0;
    }
    let mu = mean(values);
    let var = values
        .iter()
        .map(|x| {
            let d = x - mu;
            d * d
        })
        .sum::<f64>()
        / (n as f64 - 1.0);
    var.sqrt()
}

fn rss_mb_now() -> Option<f64> {
    memory_stats().map(|s| s.physical_mem as f64 / (1024.0 * 1024.0))
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

fn bench<F>(name: &str, iterations: usize, repeats: usize, mut f: F) -> WorkloadResult
where
    F: FnMut() -> BTreeMap<String, String>,
{
    let mut times: Vec<f64> = Vec::with_capacity(iterations * repeats.max(1));
    let mut meta: BTreeMap<String, String> = BTreeMap::new();

    let rss_before_mb = rss_mb_now();

    let repeats = repeats.max(1);
    for _ in 0..repeats {
        // Warm-up once per repeat
        let _ = f();
        for _ in 0..iterations {
            let start = Instant::now();
            let this_meta = f();
            let elapsed = start.elapsed().as_secs_f64() * 1000.0;
            times.push(elapsed);
            // Keep the most recent meta snapshot
            meta = this_meta;
        }
    }

    let (min_time_ms, max_time_ms) = times
        .iter()
        .copied()
        .fold((f64::INFINITY, f64::NEG_INFINITY), |(min, max), x| {
            (f64::min(min, x), f64::max(max, x))
        });
    let avg_time_ms = mean(&times);
    let stddev_time_ms = stddev_sample(&times);
    let cv_pct = if avg_time_ms.abs() > f64::EPSILON {
        (stddev_time_ms / avg_time_ms) * 100.0
    } else {
        0.0
    };

    let rss_after_mb = rss_mb_now();
    let rss_delta_mb = match (rss_before_mb, rss_after_mb) {
        (Some(b), Some(a)) => Some(a - b),
        _ => None,
    };
    let rss_mb = rss_after_mb;

    WorkloadResult {
        name: name.to_string(),
        iterations,
        repeats,
        avg_time_ms,
        min_time_ms,
        max_time_ms,
        stddev_time_ms,
        cv_pct,
        rss_mb,
        rss_before_mb,
        rss_after_mb,
        rss_delta_mb,
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
            repeats: 0,
            avg_time_ms: 0.0,
            min_time_ms: 0.0,
            max_time_ms: 0.0,
            stddev_time_ms: 0.0,
            cv_pct: 0.0,
            rss_mb: None,
            rss_before_mb: None,
            rss_after_mb: None,
            rss_delta_mb: None,
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
            repeats: 0,
            avg_time_ms: 0.0,
            min_time_ms: 0.0,
            max_time_ms: 0.0,
            stddev_time_ms: 0.0,
            cv_pct: 0.0,
            rss_mb: None,
            rss_before_mb: None,
            rss_after_mb: None,
            rss_delta_mb: None,
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

fn run_workloads(cfg: &Config, tier_label: &str) -> Vec<WorkloadResult> {
    let mut results: Vec<WorkloadResult> = Vec::new();

    let add_tier_meta = |mut r: WorkloadResult| {
        r.meta.insert("tier".to_string(), tier_label.to_string());
        r.meta.insert("obs_n".to_string(), cfg.obs_n.to_string());
        r.meta
            .insert("bundle_n".to_string(), cfg.bundle_n.to_string());
        r
    };

    // Workload 1: HL7 -> FHIR patient + sanitize + validate
    results.push(add_tier_meta(bench(
        "medi_data_hl7_patient_pipeline",
        cfg.iterations_medium,
        cfg.repeats,
        || {
            let hl7 = "MSH|^~\\&|SRC|FAC|DST|HOSP|202501010101||ADT^A01|123|P|2.5\r\
PID|1|ALTID|P12345||Doe^Jane||19851224|F\r";
            let mut p = hl7_to_fhir_patient_minimal(hl7).expect("ok");
            trim_patient_names(&mut p);
            normalize_patient_birth_date(&mut p);
            validate_patient(&p).expect("valid");
            let mut meta = BTreeMap::new();
            meta.insert("patient_id".to_string(), p.id);
            meta
        },
    )));

    // Workload 2: Observations validate + query
    results.push(add_tier_meta(bench(
        "medi_data_observation_validate_query",
        cfg.iterations_medium,
        cfg.repeats,
        || {
            let obs = synthetic_lab_results(cfg.obs_n);
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
        },
    )));

    // Workload 3: Bundle validate (patients)
    results.push(add_tier_meta(bench(
        "medi_data_bundle_validate",
        cfg.iterations_slow,
        cfg.repeats,
        || {
            let b = bundle_factory(cfg.bundle_n);
            b.validate().expect("valid");
            let mut meta = BTreeMap::new();
            meta.insert("entries".to_string(), b.entries.len().to_string());
            meta
        },
    )));

    // Workload 4: NDJSON roundtrip I/O (serialize/parse)
    results.push(add_tier_meta(bench(
        "medi_data_observation_ndjson_roundtrip",
        cfg.iterations_slow,
        cfg.repeats,
        || {
            let obs: Vec<FHIRObservation> = synthetic_lab_results(cfg.obs_n);
            let nd = to_ndjson(&obs).expect("ndjson");
            let back = from_ndjson::<FHIRObservation>(&nd).expect("parse");
            let mut meta = BTreeMap::new();
            meta.insert("bytes".to_string(), nd.len().to_string());
            meta.insert("items".to_string(), back.len().to_string());
            meta
        },
    )));

    // Workload 5: Stats t-test (Welch)
    results.push(add_tier_meta(bench(
        "medi_stats_t_test_welch",
        cfg.iterations_fast,
        cfg.repeats,
        || {
            let a = [1.0, 2.0, 3.0, 4.0];
            let b = [2.0, 3.0, 4.0, 5.0];
            let res = t_test_welch(&a, &b);
            let mut meta = BTreeMap::new();
            meta.insert("t_stat".to_string(), format!("{:.3}", res.t_stat));
            meta.insert("df".to_string(), format!("{:.1}", res.df));
            meta
        },
    )));

    // Workload 6: HIPAA keyword scan
    results.push(add_tier_meta(bench(
        "medi_compliance_hipaa_keywords",
        cfg.iterations_fast,
        cfg.repeats,
        || {
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
        },
    )));

    // Workload 7: AI risk scorer
    results.push(add_tier_meta(bench(
        "medi_ai_risk_score",
        cfg.iterations_fast,
        cfg.repeats,
        || {
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
        },
    )));

    results
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cfg = parse_config();
    let bench_dir = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let out_dir = bench_dir.join("benchdata");
    fs::create_dir_all(&out_dir)?;

    let mut results: Vec<WorkloadResult> = Vec::new();
    if cfg.tier == Tier::All {
        for (tier, label) in [
            (Tier::Small, "small"),
            (Tier::Medium, "medium"),
            (Tier::Large, "large"),
        ] {
            let mut tier_cfg = cfg.clone();
            tier_cfg.tier = tier;
            apply_tier_defaults(&mut tier_cfg);
            results.extend(run_workloads(&tier_cfg, label));
        }
    } else {
        results.extend(run_workloads(&cfg, tier_name(cfg.tier)));
    }

    if cfg.run_python {
        if let Some(py_rows) = try_run_python(&bench_dir) {
            results.extend(py_rows);
        }
    }
    if cfg.run_r {
        if let Some(r_rows) = try_run_r(&bench_dir) {
            results.extend(r_rows);
        }
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
    md.push_str(
        "This report is generated by `cargo run -p medic_benchmarks --bin benchmark_healthcare_workloads --release`.\n\n",
    );
    md.push_str("## Environment\n\n");
    md.push_str(&format!("- CPU: {}\n", get_cpu_info()));
    md.push_str(&format!("- RAM: {}\n", get_mem_info()));
    md.push_str(&format!("- Rust: {}\n", rust_version()));
    md.push_str("- Build: `--release`\n\n");

    md.push_str("## Results\n\n");
    md.push_str("| Workload | Tier | Iterations | Repeats | Avg (ms) | Stddev (ms) | CV (%) | Min (ms) | Max (ms) | RSS Δ (MB) | RSS (MB) |\n");
    md.push_str("|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|\n");
    for r in &results {
        md.push_str(&format!(
            "| {} | {} | {} | {} | {:.3} | {:.3} | {:.2} | {:.3} | {:.3} | {} | {} |\n",
            r.name,
            r.meta.get("tier").map(|s| s.as_str()).unwrap_or(""),
            r.iterations,
            r.repeats,
            r.avg_time_ms,
            r.stddev_time_ms,
            r.cv_pct,
            r.min_time_ms,
            r.max_time_ms,
            r.rss_delta_mb
                .map(|m| format!("{m:.1}"))
                .unwrap_or_else(|| "".to_string()),
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
