# Troubleshooting

- Validation fails (medi_data): sanitize codes/units before validate.
- Low coverage in CI: run tests locally and check ignored tests/benches.
- LLVM toolchain issues in CI: ensure LLVM 15 setup matches OS; see `.github/workflows/ci.yml`.
- Codecov checks missing: trigger a PR run and enable `codecov/project` & `codecov/patch` in Branch Protection.
