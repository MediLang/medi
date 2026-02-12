use criterion::{black_box, criterion_group, criterion_main, Criterion};
use tlvxc_lexer::lexer::Lexer;
use tlvxc_lexer::token::Token;
use tlvxc_parser::parser::{parse_program, TokenSlice};
use std::fs;
use std::path::PathBuf;

fn load_tokens(path: &str) -> Vec<Token> {
    let p = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(path);
    let content = fs::read_to_string(&p).expect("read file");
    Lexer::new(&content).collect()
}

fn bench_parse(c: &mut Criterion) {
    // Change this target to profile different sizes/variants
    let target = "benchdata/large_func_2m.tlvx";
    let tokens = load_tokens(target);
    // Leak tokens so their lifetime outlives the benchmark iteration
    let leaked: &'static [Token] = Box::leak(tokens.into_boxed_slice());

    c.bench_function(&format!("parse {target}"), |b| {
        b.iter(|| {
            let input = TokenSlice(black_box(leaked));
            let _silence = SilenceStdout::new();
            let _ = parse_program(input);
        })
    });
}

// Helper to silence stdout temporarily
struct SilenceStdout {
    old_fd: i32,
}
impl SilenceStdout {
    fn new() -> Self {
        unsafe {
            let old_fd = libc::dup(libc::STDOUT_FILENO);
            let devnull = std::ffi::CString::new("/dev/null").unwrap();
            let fd = libc::open(devnull.as_ptr(), libc::O_WRONLY);
            if fd >= 0 {
                libc::dup2(fd, libc::STDOUT_FILENO);
                libc::close(fd);
            }
            SilenceStdout { old_fd }
        }
    }
}
impl Drop for SilenceStdout {
    fn drop(&mut self) {
        unsafe {
            if self.old_fd >= 0 {
                libc::dup2(self.old_fd, libc::STDOUT_FILENO);
                libc::close(self.old_fd);
            }
        }
    }
}

criterion_group!(benches, bench_parse);
criterion_main!(benches);
