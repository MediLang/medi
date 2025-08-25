use std::fs;
use std::path::{Path, PathBuf};
use std::time::Instant;

use medic_lexer::lexer::Lexer;
use medic_lexer::token::Token;
use medic_parser::parser::{parse_program, TokenSlice};

// Silence stdout/stderr during parsing to avoid debug print overhead
use std::fs::OpenOptions;
use std::os::unix::io::AsRawFd;

struct SilenceStdio {
    orig_out: libc::c_int,
    orig_err: libc::c_int,
}

impl SilenceStdio {
    fn new() -> Self {
        unsafe {
            let orig_out = libc::dup(libc::STDOUT_FILENO);
            let orig_err = libc::dup(libc::STDERR_FILENO);
            // Open /dev/null once
            let devnull = OpenOptions::new().write(true).open("/dev/null").unwrap();
            let fd = devnull.as_raw_fd();
            libc::dup2(fd, libc::STDOUT_FILENO);
            libc::dup2(fd, libc::STDERR_FILENO);
            // devnull will be closed here, but the duplicated fds remain in effect
            SilenceStdio { orig_out, orig_err }
        }
    }
}

impl Drop for SilenceStdio {
    fn drop(&mut self) {
        unsafe {
            libc::dup2(self.orig_out, libc::STDOUT_FILENO);
            libc::dup2(self.orig_err, libc::STDERR_FILENO);
            libc::close(self.orig_out);
            libc::close(self.orig_err);
        }
    }
}

fn bench_file(path: &Path) -> std::io::Result<(bool, usize, u128)> {
    let content = fs::read_to_string(path)?;
    let tokens: Vec<Token> = Lexer::new(&content).collect();
    let leaked: &'static [Token] = Box::leak(tokens.into_boxed_slice());
    let start = Instant::now();
    // Silence stdio while parsing to avoid debug print overhead
    let _silence = SilenceStdio::new();
    let res = parse_program(TokenSlice(leaked));
    let elapsed = start.elapsed().as_micros();
    let (ok, stmts) = match res {
        Ok((_rest, program)) => (true, program.statements.len()),
        Err(_) => (false, 0),
    };
    Ok((ok, stmts, elapsed))
}

#[derive(serde::Serialize)]
struct Row<'a> {
    kind: &'a str,
    size: &'a str,
    bytes: u64,
    ok: bool,
    stmts: usize,
    time_ms: f64,
}

fn main() -> std::io::Result<()> {
    let base = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("benchdata");
    let sizes = ["256k", "512k", "2m", "4m", "8m"];
    let kinds = ["func", "expr"];

    println!("Parser scaling benchmark (release)\nBase dir: {}\n", base.display());

    let mut rows: Vec<Row> = Vec::new();

    println!("kind,size,bytes,ok,stmts,time_ms");
    for kind in kinds.iter() {
        for size in sizes.iter() {
            let fname = format!("large_{}_{size}.medi", kind);
            let path = base.join(&fname);
            if !path.exists() {
                eprintln!("Missing file: {} (generate with generate_sized_files)", path.display());
                continue;
            }
            let bytes = fs::metadata(&path)?.len();
            // 3 iterations per file for stability
            let mut times = Vec::new();
            let mut last_ok = false;
            let mut last_stmts = 0usize;
            for _ in 0..3 {
                let (ok, stmts, t) = bench_file(&path)?;
                last_ok = ok; last_stmts = stmts; times.push(t);
            }
            let avg_ms = times.iter().copied().sum::<u128>() as f64 / times.len() as f64 / 1000.0;
            println!("{},{},{},{} ,{}, {:.2}", kind, size, bytes, last_ok, last_stmts, avg_ms);
            rows.push(Row { kind, size, bytes, ok: last_ok, stmts: last_stmts, time_ms: avg_ms });
        }
    }

    // Write CSV and JSON outputs
    fs::create_dir_all(&base)?;
    let ts = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_secs();
    let csv_path = base.join(format!("scaling_{}.csv", ts));
    let json_path = base.join(format!("scaling_{}.json", ts));

    // CSV
    {
        let mut s = String::from("kind,size,bytes,ok,stmts,time_ms\n");
        for r in &rows {
            s.push_str(&format!("{},{},{},{},{},{}\n", r.kind, r.size, r.bytes, r.ok, r.stmts, r.time_ms));
        }
        fs::write(&csv_path, s)?;
    }
    // JSON
    {
        let j = serde_json::to_string_pretty(&rows).unwrap();
        fs::write(&json_path, j)?;
    }

    eprintln!("Wrote {} and {}", csv_path.display(), json_path.display());

    Ok(())
}
