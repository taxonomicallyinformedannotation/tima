# POLISH_LOG — full-codebase polish pass

Branch: `polish/full-pass` (from `main` @ 7550d4e0)
Started: 2026-10-19

## Phase 0: Reconnaissance

### What this repo is
`tima` — **T**axonomically **I**nformed **M**etabolite **A**nnotation. An R package
(Bioconductor-adjacent) with:
- `R/` — 144 source files, ~54.5k lines (Collate order fixed in DESCRIPTION)
- `src/` — small C layer (`gnps.c`, `init.c`, headers; compiled `tima.so`)
- `tests/testthat/` — 155 test files + setup/helper
- `man/` — 505 roxygen-generated `.Rd` files (roxygen2 8.1.1, markdown ON)
- `inst/` — Shiny app, params/paths YAML, targets pipeline, Python helpers, Dockerfile, WORDLIST
- `vignettes/`, `docs/`, `_quarto/`, `pkgdown/`, `altdoc/` — docs site
- CI: `.github/workflows/` — R-CMD-check (5 OS/R matrix), format-check, test-coverage,
  mega-linter, altdoc, docker, external-use, etc.

### Build / lint / test commands (discovered)
| Purpose | Command | Notes |
|---|---|---|
| Format check | `air format . --check` | CI: format-check.yaml; air installed @ ~/.local/bin |
| Build | `R CMD build . --no-build-vignettes` | works, exit 0 |
| Tests | `Rscript -e 'devtools::test()'` | must load pkg; slow (network-y tests) |
| R CMD check | `rcmdcheck::rcmdcheck()` / CI | not run locally yet (slow) |
| Markdown fmt | `panache format .` (pre-commit) | panache installed |
| TOML fmt | `tombi format` / `tombi lint` | installed |
| Pre-commit | `prek` (hooks incl. air-format, parsable-R, deps-in-desc…) | installed |
| jarl (R lint) | `jarl check .` | **PANICS** (`lengths.rs:77` unwrap on None) — jarl bug, not our repo |

### Baseline (recorded before any change)
- `air format . --check`: **PASS** (exit 0)
- `R CMD build`: **PASS**
- `jarl check .`: **CRASH** (tool bug) — treat as unavailable
- `devtools::test()`: running in background → `/tmp/tima_test_baseline.log`
- Working tree was clean on `main`.

### Review queue (by priority)
1. Core logic: `annotate_masses*.R`, `annotate_spectra.R`, `clean_chemo*.R`,
   `weight_*.R`, `mass_evidence*.R`, `adduct_universe*.R`, `parse_adduct.R`
2. Public surface: exports in NAMESPACE, `install.R`, `run_tima.R`, `run_app.R`,
   `prepare_params.R`, `parse_cli_params.R`, `validate_inputs.R`
3. Utilities: `*_utils.R`, `errors_utils.R`, `assert_utils.R`, `logs_utils.R`,
   `safe_fread.R`, `retry_utils.R`, `go_to_cache.R`, `string_utils.R`
4. mztab cluster: `read_mztab.R`, `write_mztab*.R`, `mztab_*.R`
5. C code: `src/*.c`, `src/*.h`
6. Tests: `tests/testthat/*`
7. Docs/config: README.qmd/README.md, DESCRIPTION, NEWS.md, CI workflows, prek.toml

## Progress

(files marked ✅ reviewed & fixed; ⏭️ reviewed, no change; ⚠️ flagged, not fixed)

### Phase 1 — file-by-file
(in progress)

## Issues found but not fixed

## Open questions

## Proposals requiring human decision
