use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command as ProcessCommand;

use clap::{Args, Parser, Subcommand};

pub mod manifest;

const MANIFEST_FILE: &str = "medi.toml";
const SRC_DIR: &str = "src";
const MAIN_FILE: &str = "main.medi";
const BUILD_DIR: &str = "target";

#[derive(Debug, Parser)]
#[command(
    name = "medipack",
    version,
    author = "MediLang Team",
    about = "Medi package manager for healthcare-safe applications",
    long_about = "medipack is the package manager for Medi.\n\n\
        Commands:\n  \
        init     Create a new Medi project\n  \
        build    Compile the current project\n  \
        check    Validate medi.toml without building\n  \
        clean    Remove build artifacts\n\n\
        Phase 1 provides project scaffolding and basic build integration.",
    after_help = "For more information, visit: https://github.com/MediLang/medi"
)]
struct Cli {
    /// Enable verbose output
    #[arg(short, long, global = true)]
    verbose: bool,

    /// Suppress non-error output
    #[arg(short, long, global = true)]
    quiet: bool,

    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    /// Initialize a new Medi project
    Init(InitArgs),
    /// Build a Medi project using medi.toml
    Build(BuildArgs),
    /// Validate medi.toml manifest without building
    Check,
    /// Remove build artifacts (target directory)
    Clean,
}

#[derive(Debug, Args)]
struct InitArgs {
    /// Set the project name (defaults to directory name)
    #[arg(long)]
    name: Option<String>,

    /// Set a short project description
    #[arg(long)]
    description: Option<String>,

    /// Create a library project instead of a binary
    #[arg(long)]
    lib: bool,
}

#[derive(Debug, Args)]
struct BuildArgs {
    /// Build with optimizations (release mode)
    #[arg(long)]
    release: bool,

    /// Pass additional flags to medic
    #[arg(last = true)]
    medic_args: Vec<String>,
}

fn main() {
    let cli = Cli::parse();
    let ctx = Context {
        verbose: cli.verbose,
        quiet: cli.quiet,
    };
    let rc = match cli.command {
        Command::Init(args) => run_init(&ctx, &args),
        Command::Build(args) => run_build(&ctx, &args),
        Command::Check => run_check(&ctx),
        Command::Clean => run_clean(&ctx),
    };
    std::process::exit(rc);
}

struct Context {
    verbose: bool,
    quiet: bool,
}

impl Context {
    fn info(&self, msg: &str) {
        if !self.quiet {
            println!("{msg}");
        }
    }

    fn verbose(&self, msg: &str) {
        if self.verbose && !self.quiet {
            println!("  {msg}");
        }
    }

    fn success(&self, msg: &str) {
        if !self.quiet {
            println!("✓ {msg}");
        }
    }
}

fn run_init(ctx: &Context, args: &InitArgs) -> i32 {
    let manifest_path = PathBuf::from(MANIFEST_FILE);
    if manifest_path.exists() {
        eprintln!("error: '{}' already exists", manifest_path.display());
        return 2;
    }

    // Determine project name
    let project_name = args.name.clone().unwrap_or_else(|| {
        std::env::current_dir()
            .ok()
            .and_then(|p| p.file_name().map(|s| s.to_string_lossy().to_string()))
            .unwrap_or_else(|| "my_medi_project".to_string())
    });

    ctx.info(&format!("Creating Medi project '{project_name}'..."));

    let src_dir = PathBuf::from(SRC_DIR);
    if let Err(e) = fs::create_dir_all(&src_dir) {
        eprintln!("error: failed to create '{}': {e}", src_dir.display());
        return 2;
    }
    ctx.verbose(&format!("Created {SRC_DIR}/"));

    let source_file = if args.lib { "lib.medi" } else { MAIN_FILE };
    let source_path = src_dir.join(source_file);
    if source_path.exists() {
        eprintln!("error: '{}' already exists", source_path.display());
        return 2;
    }

    // Generate manifest with optional description
    let manifest_contents = manifest::generate_manifest(&project_name, args.description.as_deref());
    if let Err(e) = fs::write(&manifest_path, &manifest_contents) {
        eprintln!("error: failed to write '{}': {e}", manifest_path.display());
        return 2;
    }
    ctx.verbose(&format!("Created {MANIFEST_FILE}"));

    // Generate source file
    let source_contents = if args.lib {
        format!(
            "//! {project_name} library\n\n\
             /// Example function demonstrating healthcare-safe patterns.\n\
             pub fn greet(name: &str) -> str {{\n  \
               return \"Hello, \" + name\n\
             }}\n"
        )
    } else {
        format!(
            "//! {project_name} - A Medi healthcare application\n\n\
             fn main() {{\n  \
               // Example: patient identifier (PHI - handle with care)\n  \
               let patient_id = \"P-12345\";\n  \
               let _ = patient_id;\n\
             }}\n"
        )
    };
    if let Err(e) = fs::write(&source_path, source_contents) {
        eprintln!("error: failed to write '{}': {e}", source_path.display());
        return 2;
    }
    ctx.verbose(&format!("Created {SRC_DIR}/{source_file}"));

    // Generate README
    let readme_path = PathBuf::from("README.md");
    if !readme_path.exists() {
        let desc = args
            .description
            .as_deref()
            .unwrap_or("A Medi healthcare application");
        let readme = format!(
            "# {project_name}\n\n\
             {desc}\n\n\
             ## Prerequisites\n\n\
             - [Medi compiler](https://github.com/MediLang/medi) (`medic`)\n\
             - medipack (this project's package manager)\n\n\
             ## Build\n\n\
             ```bash\n\
             medipack build\n\
             ```\n\n\
             ## Check manifest\n\n\
             ```bash\n\
             medipack check\n\
             ```\n"
        );
        if let Err(e) = fs::write(&readme_path, readme) {
            eprintln!("warning: failed to write '{}': {e}", readme_path.display());
        } else {
            ctx.verbose("Created README.md");
        }
    }

    // Generate .gitignore
    let gitignore_path = PathBuf::from(".gitignore");
    if !gitignore_path.exists() {
        let gitignore = "/target/\n*.log\n.env\n";
        if let Err(e) = fs::write(&gitignore_path, gitignore) {
            eprintln!(
                "warning: failed to write '{}': {e}",
                gitignore_path.display()
            );
        } else {
            ctx.verbose("Created .gitignore");
        }
    }

    ctx.success(&format!("Created project '{project_name}'"));
    ctx.info("");
    ctx.info("Next steps:");
    ctx.info("  medipack build    # Compile the project");
    ctx.info("  medipack check    # Validate medi.toml");

    0
}

fn load_manifest(ctx: &Context) -> Result<manifest::Manifest, i32> {
    let manifest_path = PathBuf::from(MANIFEST_FILE);
    if !manifest_path.exists() {
        eprintln!(
            "error: '{}' not found (run 'medipack init')",
            manifest_path.display()
        );
        return Err(2);
    }

    let text = match fs::read_to_string(&manifest_path) {
        Ok(t) => t,
        Err(e) => {
            eprintln!("error: failed to read '{}': {e}", manifest_path.display());
            return Err(2);
        }
    };

    match manifest::parse_manifest(&text) {
        Ok(m) => {
            ctx.verbose(&format!("Loaded manifest: {m}"));
            Ok(m)
        }
        Err(e) => {
            eprintln!("error: invalid '{}': {e}", manifest_path.display());
            Err(2)
        }
    }
}

fn find_entrypoint() -> Option<PathBuf> {
    let main = Path::new(SRC_DIR).join(MAIN_FILE);
    if main.exists() {
        return Some(main);
    }
    let lib = Path::new(SRC_DIR).join("lib.medi");
    if lib.exists() {
        return Some(lib);
    }
    None
}

fn run_build(ctx: &Context, args: &BuildArgs) -> i32 {
    let manifest = match load_manifest(ctx) {
        Ok(m) => m,
        Err(rc) => return rc,
    };

    ctx.info(&format!(
        "Building {} v{}...",
        manifest.name, manifest.version
    ));

    let entry = match find_entrypoint() {
        Some(p) => p,
        None => {
            eprintln!(
                "error: no entrypoint found (expected '{SRC_DIR}/{MAIN_FILE}' or '{SRC_DIR}/lib.medi')"
            );
            return 2;
        }
    };
    ctx.verbose(&format!("Entrypoint: {}", entry.display()));

    // Create target directory for future build artifacts
    let target_dir = PathBuf::from(BUILD_DIR);
    if let Err(e) = fs::create_dir_all(&target_dir) {
        eprintln!("warning: failed to create '{}': {e}", target_dir.display());
    }

    let mut cmd = ProcessCommand::new("medic");
    cmd.arg("check").arg(&entry);

    // Pass through additional medic args
    for arg in &args.medic_args {
        cmd.arg(arg);
    }

    if args.release {
        ctx.verbose("Release mode enabled (optimization flags will apply when supported)");
    }

    let status = match cmd.status() {
        Ok(s) => s,
        Err(e) => {
            eprintln!("error: failed to spawn 'medic': {e}");
            eprintln!("hint: ensure 'medic' is installed and available on PATH");
            return 2;
        }
    };

    if status.success() {
        ctx.success(&format!("Built {} successfully", manifest.name));
        0
    } else {
        eprintln!("error: build failed");
        status.code().unwrap_or(1)
    }
}

fn run_check(ctx: &Context) -> i32 {
    let manifest = match load_manifest(ctx) {
        Ok(m) => m,
        Err(rc) => return rc,
    };

    ctx.success(&format!("Manifest valid: {manifest}"));

    // Additional validation
    if manifest.name.is_empty() {
        eprintln!("warning: package name is empty");
    }
    if manifest.version.is_empty() {
        eprintln!("warning: package version is empty");
    }

    // Check for entrypoint
    if find_entrypoint().is_none() {
        eprintln!("warning: no source entrypoint found in '{SRC_DIR}/'");
    }

    // Report dependencies (stub)
    if !manifest.dependencies.is_empty() {
        ctx.info(&format!(
            "Dependencies: {} (resolution not yet implemented)",
            manifest.dependencies.len()
        ));
    }

    // Report FHIR config if present
    if let Some(fhir) = &manifest.fhir {
        let ver = fhir.version.as_deref().unwrap_or("unspecified");
        ctx.info(&format!("FHIR: version={ver}, strict={}", fhir.strict));
    }

    0
}

fn run_clean(ctx: &Context) -> i32 {
    let target_dir = PathBuf::from(BUILD_DIR);
    if !target_dir.exists() {
        ctx.info("Nothing to clean (no target directory)");
        return 0;
    }

    match fs::remove_dir_all(&target_dir) {
        Ok(()) => {
            ctx.success("Removed target directory");
            0
        }
        Err(e) => {
            eprintln!("error: failed to remove '{}': {e}", target_dir.display());
            2
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_manifest_requires_name_and_version() {
        let ok = "name = \"a\"\nversion = \"0.1.0\"\n";
        assert!(manifest::parse_manifest(ok).is_ok());

        let missing = "name = \"a\"\n";
        assert!(manifest::parse_manifest(missing).is_err());
    }

    #[test]
    fn parse_manifest_with_all_fields() {
        let full = r#"
name = "my_app"
version = "1.2.3"
description = "A healthcare app"
authors = ["Alice <alice@example.com>"]
license = "MIT"
edition = "2025"

[dependencies]
medi_data = "0.1"

[fhir]
version = "R4"
strict = true
"#;
        let m = manifest::parse_manifest(full).unwrap();
        assert_eq!(m.name, "my_app");
        assert_eq!(m.version, "1.2.3");
        assert_eq!(m.description, Some("A healthcare app".to_string()));
        assert_eq!(m.authors, vec!["Alice <alice@example.com>"]);
        assert_eq!(m.license, Some("MIT".to_string()));
        assert_eq!(m.edition, Some("2025".to_string()));
        assert_eq!(m.dependencies.len(), 1);
        assert!(m.fhir.is_some());
        let fhir = m.fhir.unwrap();
        assert_eq!(fhir.version, Some("R4".to_string()));
        assert!(fhir.strict);
    }

    #[test]
    fn parse_manifest_detailed_dependency() {
        let toml = r#"
name = "app"
version = "0.1.0"

[dependencies]
local_lib = { path = "../lib" }
git_lib = { git = "https://github.com/example/lib", branch = "main" }
"#;
        let m = manifest::parse_manifest(toml).unwrap();
        assert_eq!(m.dependencies.len(), 2);
    }

    #[test]
    fn manifest_display() {
        let m = manifest::Manifest::new("my_pkg");
        assert_eq!(format!("{m}"), "my_pkg v0.1.0");
    }

    #[test]
    fn generate_manifest_includes_comments() {
        let text = manifest::generate_manifest("foo", Some("A foo project"));
        assert!(text.contains("name = \"foo\""));
        assert!(text.contains("description = \"A foo project\""));
        assert!(text.contains("[dependencies]"));
        assert!(text.contains("# [fhir]"));
    }
}
