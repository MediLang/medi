#!/bin/bash
set -e

# Install rustfmt if not already installed
rustup component add rustfmt 2>/dev/null || true

# Run rustfmt
cargo fmt -- --check --color always

# Install clippy if not already installed
rustup component add clippy 2>/dev/null || true

# Run clippy
cargo clippy -- -D warnings

# Run cargo check
cargo check --all-targets --all-features

# Run tests
cargo test --all-targets --all-features

echo "All checks passed!"
