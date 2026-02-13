# Tolvex Rebrand Execution Plan

## Overview

**From:** Medi (MediLang) → **To:** Tolvex (TolvexLang)

| Property | Old | New |
|----------|-----|-----|
| Language name | Medi | Tolvex |
| File extension | `.medi` | `.tlvx` |
| Compiler binary | `medic` | `tlvxc` |
| GitHub org | MediLang | TolvexLang |
| GitHub repo | medi | tolvex |
| Domain | medi-lang.org | tolvex.dev |
| Crate prefix | `medic_*` | `tlvxc_*` |
| Stdlib prefix | `medi.*` | `tolvex.*` |
| Twitter/X | @MediLangHQ | @TolvexLang |
| Discord | existing server | rename or new |
| npm | N/A | @tolvex/* |
| crates.io | N/A | tolvex / tlvxc_* |
| PyPI | N/A | tolvex |

---

## Execution Order (Preserving All Commits)

### Phase 1: GitHub Organization & Repository Rename

**Why first:** All other changes reference the repo URL. GitHub auto-redirects old URLs.

1. **Rename GitHub org:** Settings → Organization name → `TolvexLang`
   - Old URL `github.com/MediLang/*` auto-redirects to `github.com/TolvexLang/*`
   - ⚠️ Redirect breaks if someone creates a new `MediLang` org

2. **Rename repo:** Settings → Repository name → `tolvex`
   - Old URL `github.com/TolvexLang/medi` auto-redirects to `github.com/Tolvex/tolvex`
   - All 316 commits, stars, issues preserved

3. **Update local clone remote:**
   ```bash
   git remote set-url origin https://github.com/Tolvex/tolvex.git
   ```

4. **Update GitHub topics:** Remove `medi`, add `tolvex`, `tolvex-lang`, `tlvx`

5. **Update repo description:** "Domain-specific Programming Language Empowering Healthcare with Secure, Fast, and Clinician-Friendly Analytics."

6. **Update repo website:** `https://tolvex.dev`

---

### Phase 2: Crate & Package Renames (Cargo.toml)

All Rust crate names change. This is the deepest structural change.

#### 2.1 Workspace Cargo.toml (root)

```
# Old                          # New
[workspace]                     [workspace]
members = [                     members = [
  "compiler/medic",               "compiler/tlvxc",
  "compiler/medic_lexer",         "compiler/tlvxc_lexer",
  "compiler/medic_parser",        "compiler/tlvxc_parser",
  "compiler/medic_ast",           "compiler/tlvxc_ast",
  "compiler/medic_type",          "compiler/tlvxc_type",
  "compiler/medic_typeck",        "compiler/tlvxc_typeck",
  "compiler/medic_codegen_llvm",  "compiler/tlvxc_codegen_llvm",
  "compiler/medic_runtime",       "compiler/tlvxc_runtime",
]
```

#### 2.2 Rename crate directories

```bash
cd compiler/
mv medic tlvxc
mv medic_lexer tlvxc_lexer
mv medic_parser tlvxc_parser
mv medic_ast tlvxc_ast
mv medic_type tlvxc_type
mv medic_typeck tlvxc_typeck
mv medic_codegen_llvm tlvxc_codegen_llvm
mv medic_runtime tlvxc_runtime
# Also rename any other medic_* or medi_* crates
```

#### 2.3 Update every crate's Cargo.toml

Each crate's `[package]` name and `[dependencies]` references must change:

```toml
# Example: compiler/tlvxc_parser/Cargo.toml
[package]
name = "tlvxc_parser"  # was medic_parser

[dependencies]
tlvxc_lexer = { path = "../tlvxc_lexer" }  # was medic_lexer
tlvxc_ast = { path = "../tlvxc_ast" }      # was medic_ast
```

#### 2.4 Update all `use` / `extern crate` statements in Rust source

Global find-and-replace across all `.rs` files:

```
medic_lexer    → tlvxc_lexer
medic_parser   → tlvxc_parser
medic_ast      → tlvxc_ast
medic_type     → tlvxc_type
medic_typeck   → tlvxc_typeck
medic_codegen_llvm → tlvxc_codegen_llvm
medic_runtime  → tlvxc_runtime
medi_compliance → tolvex_compliance
```

#### 2.5 Binary target name

In `compiler/tlvxc/Cargo.toml`:
```toml
[[bin]]
name = "tlvxc"  # was medic
```

---

### Phase 3: File Extension Change (.medi → .tlvx)

#### 3.1 Rename all source files

```bash
find . -name "*.medi" -exec sh -c 'mv "$1" "${1%.medi}.tlvx"' _ {} \;
```

#### 3.2 Update all references to `.medi` in code

Files to check:
- Lexer (file extension detection)
- Parser (file loading)
- CLI (argument parsing, help text)
- Tests (test file paths)
- Examples (all example files)
- Documentation
- VS Code extension config (if any)
- `.gitattributes` / `.editorconfig`
- GitHub linguist config

Search pattern: `grep -rn "\.medi" --include="*.rs" --include="*.md" --include="*.json" --include="*.toml" --include="*.yaml" --include="*.yml"`

---

### Phase 4: Source Code String Replacements

These are string literals, comments, and documentation within the Rust codebase.

#### 4.1 Language name references

| Find | Replace |
|------|---------|
| `"Medi"` (as language name) | `"Tolvex"` |
| `"medi"` (as language identifier) | `"tolvex"` |
| `"MEDI"` | `"TOLVEX"` |
| `medi-lang` | `tolvex-lang` |
| `MediLang` | `TolvexLang` |

#### 4.2 Compiler name references

| Find | Replace |
|------|---------|
| `"medic"` (as compiler name) | `"tlvxc"` |
| `MEDIC` | `TLVXC` |

#### 4.3 Standard library module prefixes

| Find | Replace |
|------|---------|
| `medi.data` | `tolvex.data` |
| `medi.compliance` | `tolvex.compliance` |
| `medi.stats` | `tolvex.stats` |
| `medi.ai` | `tolvex.ai` |
| `medi.iot` | `tolvex.iot` |
| `medi.viz` | `tolvex.viz` |
| `medi.privacy` | `tolvex.privacy` |
| `medi.ops` | `tolvex.ops` |
| `medi.genomics` | `tolvex.genomics` |

#### 4.4 Environment variables

| Find | Replace |
|------|---------|
| `MEDI_GC_LOG` | `TOLVEX_GC_LOG` |
| `MEDI_GC_STEP_SCALE` | `TOLVEX_GC_STEP_SCALE` |
| `MEDI_GC_NURSERY_BYTES` | `TOLVEX_GC_NURSERY_BYTES` |

---

### Phase 5: Documentation Updates

#### 5.1 README.md — Full rewrite

- Replace all "Medi" → "Tolvex"
- Replace all "medic" → "tlvxc"
- Replace `.medi` → `.tlvx`
- Update logo path (if logo changes)
- Update all GitHub URLs to `github.com/Tolvex/tolvex`
- Update badge URLs
- Update clone URL: `git clone https://github.com/Tolvex/tolvex.git`
- Update website: `tolvex.dev`
- Update social: `@TolvexLang`
- Update Discord invite link
- Architecture diagram: all crate names

#### 5.2 Other markdown files

| File | Action |
|------|--------|
| `CHANGELOG.md` | Add v0.1.6 entry documenting rebrand; preserve history |
| `CONTRIBUTING.md` | Update all references |
| `CODE_OF_CONDUCT.md` | Update project name/contact |
| `LANG_SPEC.md` | Full rename pass |
| `ECOSYSTEM_ROADMAP.md` | Full rename pass |
| `SELF_HOSTING.md` | Full rename pass |
| `CLAUDE.md` | Update for new names |
| `AGENTS.md` | Update for new names |
| `README-MediCMM.md` | Rename to `README-TolvexCMM.md` or equivalent |

#### 5.3 MkDocs / docs/ directory

- Update `mkdocs.yml` site name and URL
- Update all docs content
- Update any code examples in docs

---

### Phase 6: CI/CD & Configuration Files

#### 6.1 GitHub Actions (.github/workflows/)

- Update any references to `medic`, `medi`, crate names
- Update cargo commands if crate names changed
- Update artifact names
- Update badge URLs in workflow files

#### 6.2 Configuration files

| File | Changes |
|------|---------|
| `Cargo.lock` | Regenerated automatically after Cargo.toml changes |
| `package.json` | Update name, repository URL, description |
| `package-lock.json` | Regenerated after package.json changes |
| `codecov.yml` | Update if references project name |
| `.pre-commit-config.yaml` | Check for name references |
| `.gitignore` | Check for `.medi` references |
| `.env.example` | Update env var names |

#### 6.3 IDE/Editor configs

| File/Dir | Changes |
|----------|---------|
| `.vscode/` | Update settings, tasks, launch configs |
| `.cursor/` | Update rules |
| `.clinerules/` | Update references |
| `.roo/` | Update references |
| `.trae/rules/` | Update references |
| `.windsurfrules` | Update references |
| `.roomodes` | Update references |

---

### Phase 7: Standard Library (stdlib/)

- Rename any `medi_*` modules to `tolvex_*`
- Update internal references
- Update import paths in examples

---

### Phase 8: Examples & Tests

#### 8.1 Example files

```bash
# Rename all example files
find examples/ -name "*.medi" -exec sh -c 'mv "$1" "${1%.medi}.tlvx"' _ {} \;

# Update content references inside examples
grep -rl "medi\." examples/ | xargs sed -i 's/medi\./tolvex./g'
```

#### 8.2 Test files

```bash
# Rename test fixture files
find tests/ -name "*.medi" -exec sh -c 'mv "$1" "${1%.medi}.tlvx"' _ {} \;

# Update test source code references
grep -rl "\.medi" tests/ | xargs sed -i 's/\.medi/.tlvx/g'
grep -rl "medic" tests/ | xargs sed -i 's/medic/tlvxc/g'
```

#### 8.3 Debug files (root)

- `debug_test.medi` → `debug_test.tlvx`
- `debug_minimal.rs` / `debug_simple.rs` → update references

---

### Phase 9: Scripts

- Update `scripts/` for any references to `medi`, `medic`, `.medi`

---

### Phase 10: Taskmaster (.taskmaster/)

- Update task descriptions referencing `medic`, `medi`, etc.

---

### Phase 11: Domain & Web Properties

| Property | Action |
|----------|--------|
| `tolvex.dev` | Already reserved — point DNS to hosting |
| `medi-lang.org` | Keep for 1 year with redirect to `tolvex.dev` |
| GitHub Pages | Update CNAME if using custom domain |
| npm `@tolvex` | Reserve scope (already done) |
| crates.io `tolvex` | Reserve name (already done) |
| PyPI `tolvex` | Reserve name (already done) |

---

### Phase 12: Social & Community

| Platform | Action |
|----------|--------|
| Twitter/X | Create `@TolvexLang` or rename `@MediLangHQ` |
| Discord | Rename server to "Tolvex" |
| Discord invite | Update link in README if changed |

---

### Phase 13: Release v0.1.6

#### 13.1 Version bump

Update version in all `Cargo.toml` files from `0.0.8` to `0.1.6`.

#### 13.2 CHANGELOG.md entry

```markdown
## [0.1.6] - 2026-02-XX

### 🎉 Rebrand: Medi → Tolvex

The Medi Programming Language is now **Tolvex** — a distinctive, globally
trademarkable name for the next phase of the project.

#### What Changed
- **Language name:** Medi → Tolvex
- **File extension:** `.medi` → `.tlvx`
- **Compiler binary:** `medic` → `tlvxc`
- **Crate prefix:** `medic_*` → `tlvxc_*`
- **Stdlib prefix:** `medi.*` → `tolvex.*`
- **Environment variables:** `MEDI_*` → `TOLVEX_*`
- **GitHub:** github.com/Tolvex/tolvex
- **Website:** tolvex.dev
- **Trademark:** DJKI application filed (Classes 9 & 42)

#### Why the Rebrand
- "Medi" conflicts with existing trademarks globally
- "Tolvex" is clean across all jurisdictions (USPTO, EUIPO, DJKI)
- All platforms reserved: npm, crates.io, PyPI, tolvex.dev

#### Migration
- Rename `.medi` files to `.tlvx`
- Replace `medic` CLI with `tlvxc`
- Update `use medic_*` imports to `use tlvxc_*`
- GitHub redirects from old URLs still work
```

#### 13.3 Git tag and GitHub release

```bash
git add -A
git commit -m "feat: rebrand Medi → Tolvex (v0.1.6)

BREAKING CHANGE: Language renamed from Medi to Tolvex
- File extension: .medi → .tlvx
- Compiler: medic → tlvxc
- Crates: medic_* → tlvxc_*
- Stdlib: medi.* → tolvex.*
- Env vars: MEDI_* → TOLVEX_*
- GitHub: Tolvex/tolvex
- Website: tolvex.dev"

git tag -a v0.1.6 -m "v0.1.6: Rebrand Medi → Tolvex"
git push origin main --tags
```

---

## Commit Strategy

To preserve all 316 commits while keeping clean history:

**Option A: Single rebrand commit (recommended)**
- Make all changes in one commit
- Clear, atomic change
- Easy to `git bisect` around

**Option B: Phased commits**
1. `chore: rename crates medic_* → tlvxc_*`
2. `chore: rename file extension .medi → .tlvx`
3. `docs: update all documentation for Tolvex rebrand`
4. `chore: update CI/CD and configs`
5. `feat: bump version to 0.1.6`

Option B gives better granularity but more noise. Either way, all 316 prior commits remain intact.

---

## Blog / Announcement Outline

### Title: "Introducing Tolvex: The Next Chapter for Healthcare's Programming Language"

1. **The Journey** — Brief history of the project (Medi → Tolvex)
2. **Why Rename?** — Trademark conflicts, global ambitions, professional identity
3. **What is Tolvex?** — Elevator pitch for new readers
4. **What Changed** — Quick migration table
5. **What Didn't Change** — Same mission, same codebase, same community
6. **Trademark Status** — DJKI filed, USPTO next
7. **What's Next** — Roadmap highlights post-rebrand
8. **Call to Action** — Star the repo, join Discord, contribute

---

## Verification Checklist

After completing all changes:

- [ ] `cargo build --workspace` succeeds
- [ ] `cargo test --workspace` passes
- [ ] `tlvxc --help` shows correct name and version
- [ ] No references to "medic" or ".medi" in codebase (except CHANGELOG history)
- [ ] All example `.tlvx` files parse correctly
- [ ] GitHub Actions pass
- [ ] README renders correctly on GitHub
- [ ] Website `tolvex.dev` resolves
- [ ] Old GitHub URLs redirect properly
- [ ] Git log shows all 316+ commits preserved