use std::fs;
use std::process::Command;

#[test]
fn e2e_init_then_build_runs() {
    let dir = tempfile::tempdir().unwrap();

    let status = Command::new(env!("CARGO_BIN_EXE_medipack"))
        .arg("init")
        .current_dir(dir.path())
        .status()
        .expect("spawn medipack init");
    assert!(status.success(), "medipack init failed: {status:?}");

    // Verify files were created
    assert!(dir.path().join("medi.toml").exists());
    assert!(dir.path().join("src/main.medi").exists());

    // Build will attempt to invoke `medic`. If it's not installed on PATH in CI/dev,
    // we skip rather than fail Phase 1 scaffolding tests.
    let build = Command::new(env!("CARGO_BIN_EXE_medipack"))
        .arg("build")
        .current_dir(dir.path())
        .output()
        .expect("spawn medipack build");

    if !build.status.success() {
        let stderr = String::from_utf8_lossy(&build.stderr);
        if stderr.contains("failed to spawn 'medic'") {
            eprintln!("skipping build: medic not found on PATH");
            return;
        }
        panic!(
            "medipack build failed. status: {:?}, stderr: {}",
            build.status, stderr
        );
    }
}

#[test]
fn e2e_init_with_name_and_description() {
    let dir = tempfile::tempdir().unwrap();

    let status = Command::new(env!("CARGO_BIN_EXE_medipack"))
        .args([
            "init",
            "--name",
            "my_healthcare_app",
            "--description",
            "A HIPAA-compliant app",
        ])
        .current_dir(dir.path())
        .status()
        .expect("spawn medipack init");
    assert!(status.success(), "medipack init --name failed: {status:?}");

    let manifest = fs::read_to_string(dir.path().join("medi.toml")).unwrap();
    assert!(manifest.contains("name = \"my_healthcare_app\""));
    assert!(manifest.contains("description = \"A HIPAA-compliant app\""));
}

#[test]
fn e2e_init_lib_project() {
    let dir = tempfile::tempdir().unwrap();

    let status = Command::new(env!("CARGO_BIN_EXE_medipack"))
        .args(["init", "--lib", "--name", "my_lib"])
        .current_dir(dir.path())
        .status()
        .expect("spawn medipack init --lib");
    assert!(status.success(), "medipack init --lib failed: {status:?}");

    assert!(dir.path().join("src/lib.medi").exists());
    assert!(!dir.path().join("src/main.medi").exists());
}

#[test]
fn e2e_check_validates_manifest() {
    let dir = tempfile::tempdir().unwrap();

    // First init
    Command::new(env!("CARGO_BIN_EXE_medipack"))
        .arg("init")
        .current_dir(dir.path())
        .status()
        .expect("init");

    // Then check
    let status = Command::new(env!("CARGO_BIN_EXE_medipack"))
        .arg("check")
        .current_dir(dir.path())
        .status()
        .expect("spawn medipack check");
    assert!(status.success(), "medipack check failed: {status:?}");
}

#[test]
fn e2e_check_fails_without_manifest() {
    let dir = tempfile::tempdir().unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_medipack"))
        .arg("check")
        .current_dir(dir.path())
        .output()
        .expect("spawn medipack check");
    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("not found"));
}

#[test]
fn e2e_clean_removes_target() {
    let dir = tempfile::tempdir().unwrap();

    // Create target directory manually
    fs::create_dir_all(dir.path().join("target")).unwrap();
    fs::write(dir.path().join("target/artifact.o"), b"fake object").unwrap();

    let status = Command::new(env!("CARGO_BIN_EXE_medipack"))
        .arg("clean")
        .current_dir(dir.path())
        .status()
        .expect("spawn medipack clean");
    assert!(status.success(), "medipack clean failed: {status:?}");
    assert!(!dir.path().join("target").exists());
}

#[test]
fn e2e_verbose_flag_works() {
    let dir = tempfile::tempdir().unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_medipack"))
        .args(["-v", "init", "--name", "verbose_test"])
        .current_dir(dir.path())
        .output()
        .expect("spawn medipack -v init");
    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("Created"));
}

#[test]
fn e2e_quiet_flag_suppresses_output() {
    let dir = tempfile::tempdir().unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_medipack"))
        .args(["-q", "init", "--name", "quiet_test"])
        .current_dir(dir.path())
        .output()
        .expect("spawn medipack -q init");
    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.is_empty() || stdout.trim().is_empty());
}
