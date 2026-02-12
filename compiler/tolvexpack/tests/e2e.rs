use std::fs;
use std::process::Command;

#[test]
fn e2e_init_then_build_runs() {
    let dir = tempfile::tempdir().unwrap();

    let status = Command::new(env!("CARGO_BIN_EXE_tolvexpack"))
        .arg("init")
        .current_dir(dir.path())
        .status()
        .expect("spawn tolvexpack init");
    assert!(status.success(), "tolvexpack init failed: {status:?}");

    // Verify files were created
    assert!(dir.path().join("tolvex.toml").exists());
    assert!(dir.path().join("src/main.tlvx").exists());

    // Build will attempt to invoke `tlvxc`. If it's not installed on PATH in CI/dev,
    // we skip rather than fail Phase 1 scaffolding tests.
    let build = Command::new(env!("CARGO_BIN_EXE_tolvexpack"))
        .arg("build")
        .current_dir(dir.path())
        .output()
        .expect("spawn tolvexpack build");

    if !build.status.success() {
        let stderr = String::from_utf8_lossy(&build.stderr);
        if stderr.contains("failed to spawn 'tlvxc'") {
            eprintln!("skipping build: tlvxc not found on PATH");
            return;
        }
        panic!(
            "tolvexpack build failed. status: {:?}, stderr: {}",
            build.status, stderr
        );
    }
}

#[test]
fn e2e_init_with_name_and_description() {
    let dir = tempfile::tempdir().unwrap();

    let status = Command::new(env!("CARGO_BIN_EXE_tolvexpack"))
        .args([
            "init",
            "--name",
            "my_healthcare_app",
            "--description",
            "A HIPAA-compliant app",
        ])
        .current_dir(dir.path())
        .status()
        .expect("spawn tolvexpack init");
    assert!(status.success(), "tolvexpack init --name failed: {status:?}");

    let manifest = fs::read_to_string(dir.path().join("tolvex.toml")).unwrap();
    assert!(manifest.contains("name = \"my_healthcare_app\""));
    assert!(manifest.contains("description = \"A HIPAA-compliant app\""));
}

#[test]
fn e2e_init_lib_project() {
    let dir = tempfile::tempdir().unwrap();

    let status = Command::new(env!("CARGO_BIN_EXE_tolvexpack"))
        .args(["init", "--lib", "--name", "my_lib"])
        .current_dir(dir.path())
        .status()
        .expect("spawn tolvexpack init --lib");
    assert!(status.success(), "tolvexpack init --lib failed: {status:?}");

    assert!(dir.path().join("src/lib.tlvx").exists());
    assert!(!dir.path().join("src/main.tlvx").exists());
}

#[test]
fn e2e_check_validates_manifest() {
    let dir = tempfile::tempdir().unwrap();

    // First init
    Command::new(env!("CARGO_BIN_EXE_tolvexpack"))
        .arg("init")
        .current_dir(dir.path())
        .status()
        .expect("init");

    // Then check
    let status = Command::new(env!("CARGO_BIN_EXE_tolvexpack"))
        .arg("check")
        .current_dir(dir.path())
        .status()
        .expect("spawn tolvexpack check");
    assert!(status.success(), "tolvexpack check failed: {status:?}");
}

#[test]
fn e2e_check_fails_without_manifest() {
    let dir = tempfile::tempdir().unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_tolvexpack"))
        .arg("check")
        .current_dir(dir.path())
        .output()
        .expect("spawn tolvexpack check");
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

    let status = Command::new(env!("CARGO_BIN_EXE_tolvexpack"))
        .arg("clean")
        .current_dir(dir.path())
        .status()
        .expect("spawn tolvexpack clean");
    assert!(status.success(), "tolvexpack clean failed: {status:?}");
    assert!(!dir.path().join("target").exists());
}

#[test]
fn e2e_verbose_flag_works() {
    let dir = tempfile::tempdir().unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_tolvexpack"))
        .args(["-v", "init", "--name", "verbose_test"])
        .current_dir(dir.path())
        .output()
        .expect("spawn tolvexpack -v init");
    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("Created"));
}

#[test]
fn e2e_quiet_flag_suppresses_output() {
    let dir = tempfile::tempdir().unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_tolvexpack"))
        .args(["-q", "init", "--name", "quiet_test"])
        .current_dir(dir.path())
        .output()
        .expect("spawn tolvexpack -q init");
    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.is_empty() || stdout.trim().is_empty());
}
