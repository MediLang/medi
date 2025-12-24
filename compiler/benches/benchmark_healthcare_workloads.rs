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
use std::sync::Arc;
use std::thread;
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
    threads: usize,
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
            threads: 4,
            run_python: true,
            run_r: true,
        }
    }
}

#[derive(Debug, Deserialize)]
struct ConfigFile {
    tier: Option<String>,
    obs_n: Option<usize>,
    bundle_n: Option<usize>,
    iterations_fast: Option<usize>,
    iterations_medium: Option<usize>,
    iterations_slow: Option<usize>,
    repeats: Option<usize>,
    threads: Option<usize>,
    run_python: Option<bool>,
    run_r: Option<bool>,
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

fn parse_bool(value: &str) -> Option<bool> {
    match value {
        "true" => Some(true),
        "false" => Some(false),
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

fn git_sha() -> String {
    Command::new("git")
        .args(["rev-parse", "HEAD"])
        .output()
        .ok()
        .and_then(|o| {
            if o.status.success() {
                Some(String::from_utf8_lossy(&o.stdout).trim().to_string())
            } else {
                None
            }
        })
        .unwrap_or_else(|| "unknown".to_string())
}

fn apply_config_file(cfg: &mut Config, file_cfg: ConfigFile) {
    if let Some(tier) = file_cfg.tier.as_deref().and_then(parse_tier) {
        cfg.tier = tier;
    }
    if let Some(v) = file_cfg.obs_n {
        cfg.obs_n = v;
    }
    if let Some(v) = file_cfg.bundle_n {
        cfg.bundle_n = v;
    }
    if let Some(v) = file_cfg.iterations_fast {
        cfg.iterations_fast = v;
    }
    if let Some(v) = file_cfg.iterations_medium {
        cfg.iterations_medium = v;
    }
    if let Some(v) = file_cfg.iterations_slow {
        cfg.iterations_slow = v;
    }
    if let Some(v) = file_cfg.repeats {
        cfg.repeats = v.max(1);
    }
    if let Some(v) = file_cfg.threads {
        cfg.threads = v.max(1);
    }
    if let Some(v) = file_cfg.run_python {
        cfg.run_python = v;
    }
    if let Some(v) = file_cfg.run_r {
        cfg.run_r = v;
    }
}

fn parse_config() -> Config {
    let args: Vec<String> = env::args().collect();
    let mut cfg = Config::default();
    let mut config_path: Option<String> = None;

    // Pass 1: discover tier/config so defaults can be applied before overrides.
    let mut it = args.iter().skip(1);
    while let Some(a) = it.next() {
        match a.as_str() {
            "--config" => {
                if let Some(p) = it.next() {
                    config_path = Some(p.clone());
                }
            }
            "--tier" => {
                if let Some(v) = it.next().and_then(|s| parse_tier(s)) {
                    cfg.tier = v;
                }
            }
            _ => {}
        }
    }

    apply_tier_defaults(&mut cfg);

    if let Some(p) = &config_path {
        if let Ok(s) = fs::read_to_string(p) {
            if let Ok(file_cfg) = serde_json::from_str::<ConfigFile>(&s) {
                apply_config_file(&mut cfg, file_cfg);
                if cfg.tier != Tier::All {
                    apply_tier_defaults(&mut cfg);
                }
            }
        }
    }

    // Pass 2: apply CLI overrides (highest precedence)
    let mut it = args.iter().skip(1);
    while let Some(a) = it.next() {
        match a.as_str() {
            "--tier" => {
                if let Some(v) = it.next().and_then(|s| parse_tier(s)) {
                    cfg.tier = v;
                }
            }
            "--obs-n" => {
                if let Some(v) = it.next().and_then(|s| parse_usize(s)) {
                    cfg.obs_n = v;
                }
            }
            "--bundle-n" => {
                if let Some(v) = it.next().and_then(|s| parse_usize(s)) {
                    cfg.bundle_n = v;
                }
            }
            "--iterations-fast" => {
                if let Some(v) = it.next().and_then(|s| parse_usize(s)) {
                    cfg.iterations_fast = v;
                }
            }
            "--iterations-medium" => {
                if let Some(v) = it.next().and_then(|s| parse_usize(s)) {
                    cfg.iterations_medium = v;
                }
            }
            "--iterations-slow" => {
                if let Some(v) = it.next().and_then(|s| parse_usize(s)) {
                    cfg.iterations_slow = v;
                }
            }
            "--repeats" => {
                if let Some(v) = it.next().and_then(|s| parse_usize(s)) {
                    cfg.repeats = v.max(1);
                }
            }
            "--threads" => {
                if let Some(v) = it.next().and_then(|s| parse_usize(s)) {
                    cfg.threads = v.max(1);
                }
            }
            "--no-python" => cfg.run_python = false,
            "--no-r" => cfg.run_r = false,
            "--run-python" => {
                if let Some(v) = it.next().and_then(|s| parse_bool(s)) {
                    cfg.run_python = v;
                }
            }
            "--run-r" => {
                if let Some(v) = it.next().and_then(|s| parse_bool(s)) {
                    cfg.run_r = v;
                }
            }
            "--help" | "-h" => {
                eprintln!(
                    "benchmark_healthcare_workloads options:\n\
  --config <path>           Load JSON config (tier defaults applied first; CLI overrides win)\n\
  --tier <small|medium|large|all>  Preset dataset/iteration defaults (default: medium)\n\
  --obs-n <N>               Number of synthetic observations (default: 20000)\n\
  --bundle-n <N>            Number of bundle entries (default: 5000)\n\
  --iterations-fast <N>     Iterations for fast workloads (default: 1000)\n\
  --iterations-medium <N>   Iterations for medium workloads (default: 20)\n\
  --iterations-slow <N>     Iterations for slow workloads (default: 10)\n\
  --repeats <N>             Repeat each workload N times and report stddev/CV (default: 1)\n\
  --threads <N>             Worker threads for concurrency workloads (default: 4)\n\
  --no-python               Disable Python comparator\n\
  --no-r                    Disable R comparator\n\
  --run-python <true|false> Explicitly enable/disable Python comparator\n\
  --run-r <true|false>      Explicitly enable/disable R comparator\n"
                );
            }
            _ => {}
        }
    }

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

fn workload_group(name: &str) -> Option<&'static str> {
    match name {
        "medi_compliance_hipaa_keywords" | "python_keyword_scan" | "r_keyword_scan" => {
            Some("keyword_scan")
        }
        "medi_ai_risk_score" | "python_dot_product" | "r_dot_product" => Some("dot_product"),
        "python_mean" | "r_mean" => Some("mean"),
        "medi_data_observation_validate_query"
        | "python_observation_validate_query"
        | "r_observation_validate_query" => Some("observation_validate_query"),
        "medi_data_observation_ndjson_roundtrip"
        | "python_observation_ndjson_roundtrip"
        | "r_observation_ndjson_roundtrip" => Some("observation_ndjson_roundtrip"),
        "medi_data_observation_validate_query_parallel"
        | "python_observation_validate_query_parallel"
        | "r_observation_validate_query_parallel" => Some("observation_validate_query_parallel"),
        _ => None,
    }
}

fn try_run_python(bench_dir: &std::path::Path) -> Option<Vec<WorkloadResult>> {
    let script = bench_dir.join("python").join("workloads.py");
    if !script.exists() {
        return None;
    }

    let args = env::args().collect::<Vec<_>>();
    let out = match Command::new("python3")
        .arg(script)
        .args(args.into_iter().skip(1))
        .output()
    {
        Ok(out) => out,
        Err(e) => {
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
                    ("lang".to_string(), "python".to_string()),
                    ("status".to_string(), "failed_to_spawn".to_string()),
                    ("error".to_string(), e.to_string()),
                ]),
            }]);
        }
    };

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
    let mut rows = parse_subresults_container(container);
    for r in &mut rows {
        r.meta.insert("lang".to_string(), "python".to_string());
    }
    Some(rows)
}

fn try_run_r(bench_dir: &std::path::Path) -> Option<Vec<WorkloadResult>> {
    let script = bench_dir.join("r").join("workloads.R");
    if !script.exists() {
        return None;
    }

    let args = env::args().collect::<Vec<_>>();
    let out = match Command::new("Rscript")
        .arg(script)
        .args(args.into_iter().skip(1))
        .output()
    {
        Ok(out) => out,
        Err(e) => {
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
                    ("lang".to_string(), "r".to_string()),
                    ("status".to_string(), "failed_to_spawn".to_string()),
                    ("error".to_string(), e.to_string()),
                ]),
            }]);
        }
    };

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

    let container = match serde_json::from_slice::<WorkloadResult>(&out.stdout) {
        Ok(v) => v,
        Err(e) => {
            let stdout = String::from_utf8_lossy(&out.stdout);
            let stderr = String::from_utf8_lossy(&out.stderr);
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
                    ("lang".to_string(), "r".to_string()),
                    ("status".to_string(), "parse_failed".to_string()),
                    ("error".to_string(), e.to_string()),
                    (
                        "stdout".to_string(),
                        stdout.chars().take(8000).collect::<String>(),
                    ),
                    (
                        "stderr".to_string(),
                        stderr.chars().take(8000).collect::<String>(),
                    ),
                ]),
            }]);
        }
    };

    let mut rows = parse_subresults_container(container);
    for r in &mut rows {
        r.meta.insert("lang".to_string(), "r".to_string());
    }
    Some(rows)
}

fn run_workloads(
    cfg: &Config,
    tier_label: &str,
    run_meta: &BTreeMap<String, String>,
) -> Vec<WorkloadResult> {
    let mut results: Vec<WorkloadResult> = Vec::new();

    let add_tier_meta = |mut r: WorkloadResult| {
        r.meta.insert("lang".to_string(), "medi".to_string());
        r.meta.insert("tier".to_string(), tier_label.to_string());
        r.meta.insert("obs_n".to_string(), cfg.obs_n.to_string());
        r.meta
            .insert("bundle_n".to_string(), cfg.bundle_n.to_string());
        r.meta
            .insert("threads".to_string(), cfg.threads.to_string());
        for (k, v) in run_meta {
            r.meta.insert(k.clone(), v.clone());
        }
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

    // Workload 2b: Observations validate in parallel + query
    results.push(add_tier_meta(bench(
        "medi_data_observation_validate_query_parallel",
        cfg.iterations_slow,
        cfg.repeats,
        || {
            let obs: Vec<FHIRObservation> = synthetic_lab_results(cfg.obs_n);
            let obs = Arc::new(obs);
            let workers = cfg.threads.max(1).min(obs.len().max(1));
            let chunk = obs.len().div_ceil(workers);

            thread::scope(|s| {
                for w in 0..workers {
                    let obs = Arc::clone(&obs);
                    let start = w * chunk;
                    let end = ((w + 1) * chunk).min(obs.len());
                    s.spawn(move || {
                        for i in start..end {
                            validate_observation(&obs[i]).expect("valid");
                        }
                    });
                }
            });

            let q = fhir_query("Observation")
                .filter_eq_ci("code", "HGB")
                .build();
            let matched = q.execute_observations(&obs);

            let mut meta = BTreeMap::new();
            meta.insert("observations".to_string(), obs.len().to_string());
            meta.insert("matched".to_string(), matched.len().to_string());
            meta.insert("workers".to_string(), workers.to_string());
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

    let argv = env::args().collect::<Vec<_>>().join(" ");
    let mut run_meta: BTreeMap<String, String> = BTreeMap::new();
    run_meta.insert("cpu".to_string(), get_cpu_info());
    run_meta.insert("ram".to_string(), get_mem_info());
    run_meta.insert("rust".to_string(), rust_version());
    run_meta.insert("git_sha".to_string(), git_sha());
    run_meta.insert("argv".to_string(), argv);

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
            results.extend(run_workloads(&tier_cfg, label, &run_meta));
        }
    } else {
        results.extend(run_workloads(&cfg, tier_name(cfg.tier), &run_meta));
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

    let mut groups: BTreeMap<String, BTreeMap<String, f64>> = BTreeMap::new();
    for r in &results {
        let Some(g) = workload_group(&r.name) else {
            continue;
        };
        let lang = r.meta.get("lang").map(|s| s.as_str()).unwrap_or("unknown");
        groups
            .entry(g.to_string())
            .or_default()
            .insert(lang.to_string(), r.avg_time_ms);
    }

    if !groups.is_empty() {
        md.push_str("\n## Comparison (Medi vs Python/R)\n\n");
        md.push_str(
            "| Group | Medi Avg (ms) | Python Avg (ms) | R Avg (ms) | Python/Medi | R/Medi |\n",
        );
        md.push_str("|---|---:|---:|---:|---:|---:|\n");
        for (g, langs) in groups {
            let medi = langs.get("medi").copied();
            let py = langs.get("python").copied();
            let rlang = langs.get("r").copied();
            if medi.is_none() && py.is_none() && rlang.is_none() {
                continue;
            }
            let py_ratio = match (py, medi) {
                (Some(p), Some(m)) if m.abs() > f64::EPSILON => Some(p / m),
                _ => None,
            };
            let r_ratio = match (rlang, medi) {
                (Some(p), Some(m)) if m.abs() > f64::EPSILON => Some(p / m),
                _ => None,
            };
            md.push_str(&format!(
                "| {} | {} | {} | {} | {} | {} |\n",
                g,
                medi.map(|v| format!("{v:.3}"))
                    .unwrap_or_else(|| "".to_string()),
                py.map(|v| format!("{v:.3}"))
                    .unwrap_or_else(|| "".to_string()),
                rlang
                    .map(|v| format!("{v:.3}"))
                    .unwrap_or_else(|| "".to_string()),
                py_ratio
                    .map(|v| format!("{v:.2}"))
                    .unwrap_or_else(|| "".to_string()),
                r_ratio
                    .map(|v| format!("{v:.2}"))
                    .unwrap_or_else(|| "".to_string())
            ));
        }
    }

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
