# 🔄 Project Migration Notice

## Medi → Tolvex

**This repository has been rebranded and migrated to a new location.**

---

## 🆕 New Repository

**Tolvex** is now located at:

### **[https://github.com/Tolvex/tolvex](https://github.com/Tolvex/tolvex)**

---

## 📋 What Changed?

The **Medi** programming language has been officially rebranded to **Tolvex**. All development, releases, and documentation have moved to the new repository.

### Key Changes

| Component | Old (Medi) | New (Tolvex) |
|-----------|------------|--------------|
| **Language Name** | Medi | Tolvex |
| **File Extension** | `.medi` | `.tlvx` |
| **Compiler Binary** | `medic` | `tlvxc` |
| **Package Manager** | `medipack` | `tvx` |
| **Package Registry** | `medipacks.io` | `formulary.tolvex.dev` |
| **Manifest File** | `medi.toml` | `formula.toml` |
| **Lock File** | `medi.lock` | `formula.lock` |
| **Packages Called** | packages/crates | formulas |
| **Python Bindings** | `pymedi` | `pytolvex` |
| **Compiler Crates** | `medic_*` | `tlvxc_*` |
| **Stdlib Crates** | `medi_*` | `tolvex_*` |
| **GitHub Org** | MediLang | Tolvex |
| **Repository** | medi | tolvex |
| **Website** | medi-lang.org | tolvex.dev |
| **Social Media** | @MediLangHQ | @TolvexLang |

---

## 🚀 Getting Started with Tolvex

### Clone the New Repository

```bash
git clone https://github.com/Tolvex/tolvex.git
cd tolvex
```

### Build the Compiler

```bash
cargo build --workspace --release
```

### Install the Tools

```bash
# Compiler
cargo install --path compiler/tlvxc

# Package Manager
cargo install --path compiler/tvx
```

---

## 📚 Documentation

- **Official Website**: [https://tolvex.dev](https://tolvex.dev)
- **Documentation**: [https://tolvex.dev/docs](https://tolvex.dev/docs)
- **GitHub**: [https://github.com/Tolvex/tolvex](https://github.com/Tolvex/tolvex)
- **Changelog**: [CHANGELOG.md](https://github.com/Tolvex/tolvex/blob/main/CHANGELOG.md)

---

## 🏷️ Release History

All historical releases (v0.0.1 through v0.1.15) have been preserved and migrated to the new repository. The first official **Tolvex** release is:

### **[v0.1.6 - Tolvex Rebranding Release](https://github.com/Tolvex/tolvex/releases/tag/v0.1.6)**

This release marks the complete transition from Medi to Tolvex with all naming conventions, tooling, and documentation updated.

---

## ❓ Why the Rebrand?

The rebrand from **Medi** to **Tolvex** reflects the evolution of the project's vision and scope. The new name better represents the language's mission to empower healthcare analytics with secure, fast, and clinician-friendly tools.

For detailed information about the rebranding process, see [REBRAND.md](https://github.com/Tolvex/tolvex/blob/main/REBRAND.md) in the new repository.

---

## 🔗 Quick Links

- **New Repository**: [github.com/Tolvex/tolvex](https://github.com/Tolvex/tolvex)
- **Issues & Discussions**: [github.com/Tolvex/tolvex/issues](https://github.com/Tolvex/tolvex/issues)
- **Contributing Guide**: [CONTRIBUTING.md](https://github.com/Tolvex/tolvex/blob/main/CONTRIBUTING.md)
- **Community**: [tolvex.dev/community](https://tolvex.dev/community)
- **X/Twitter**: [@TolvexLang](https://twitter.com/TolvexLang)

---

## 📦 Migration Guide for Existing Users

If you have existing Medi code, here's how to migrate:

### 1. Update File Extensions
```bash
# Rename all .medi files to .tlvx
find . -name "*.medi" -exec rename 's/\.medi$/.tlvx/' {} \;
```

### 2. Update Manifest Files
Rename `medi.toml` → `formula.toml` and update package references:

```toml
# Old (medi.toml)
[package]
name = "my-app"
version = "0.1.0"

# New (formula.toml)
[package]
name = "my-app"
version = "0.1.0"
```

### 3. Update Imports
Replace namespace references in your code:
- `medi::*` → `tolvex::*`
- `use medi_stats::*` → `use tolvex_stats::*`

### 4. Update Tooling
```bash
# Old commands
medic build
medipack add fhir-utils

# New commands
tlvxc build
tvx add fhir-utils
```

---

## 💬 Questions or Issues?

If you have questions about the migration or encounter any issues:

- Open an [issue](https://github.com/Tolvex/tolvex/issues) in the new repository

---

## 🙏 Thank You

Thank you for your support of the Medi project. We're excited to continue this journey as **Tolvex** and look forward to building the future of healthcare programming together!

**— The Tolvex Team**

---

**Last Updated**: February 18, 2026  
**Migration Effective**: v0.1.6
