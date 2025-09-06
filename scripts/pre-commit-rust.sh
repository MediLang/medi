#!/bin/bash
set -e

# Install rustfmt if not already installed
rustup component add rustfmt 2>/dev/null || true

# Run rustfmt
cargo fmt -- --check --color always

# Install clippy if not already installed
rustup component add clippy 2>/dev/null || true

# Run clippy (default features only; feature-heavy builds should be tested in CI)
cargo clippy --workspace --all-targets -- -D warnings

# Run cargo check (avoid --all-features to keep optional backends optional locally)
cargo check --workspace --all-targets

# Run tests
cargo test --workspace --all-targets

echo "All checks passed!"
