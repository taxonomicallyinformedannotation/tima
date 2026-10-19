# AGENTS.md --- Tima Development Guide

TIMA (Taxonomically Informed Metabolite Annotation) is an R package with C
extensions, a targets-based pipeline, a Shiny app, and CLI scripts. Uses
Bioconductor.

## Quick Commands

  | Task | Command |
  | ------ | --------- |
  | Run all tests | `Rscript tests/test-all.R` |
  | Run single test file | `Rscript tests/test-single.R test-FILENAME.R` |
  | Format R code | `air format .` |
  | Check formatting | `air format . --check` |
  | Lint R code | `Rscript -e 'library(lintr); lintr::lint_dir("R")'` |
  | Roxygen docs | `Rscript -e 'roxygen2::roxygenise(".")'` |
  | R CMD check | `R CMD check . --no-manual` |
  | Regenerate code map | `graphify . . --no-commit` (requires graphify) |

## Project Structure

- `R/` --- R package source (144 files, ~54k LOC). Key entry points:
  `R/run_tima.R`, `R/run_app.R`
- `src/` --- C source. `gnps.c` implements spectral similarity; `init.c`
  registers symbols
- `tests/testthat/` --- 157 test files (testthat, 3rd edition). `setup.R` loads
  via pkgload
- `tests/test-single.R` --- fast single-file test runner
- `tests/test-all.R` --- full test runner (uses pkgload::load_all)
- `inst/scripts/` --- CLI scripts (docopt-based). Usage in
  `inst/scripts/docopt/*.txt`
- `inst/targets/` --- targets pipeline sections (sourced by `inst/_targets.R`)
- `inst/app/` --- Shiny app (`app_server.R`, `app_ui.R`)
- `inst/params/` --- YAML parameter templates
- `inst/contracts/` --- mzTab-m schema definitions
- `inst/python/` --- RDKit/ctypes Python integration for SMILES and GNPS
- `man/` --- roxygen-generated `.Rd` docs (507 files)
- `vignettes/` --- Quarto vignettes (I-gathering → IV-benchmarking)

## Gotcha

- Tests MUST use `pkgload::load_all(".")` to access internal (non-exported)
  functions. `testthat::test_package("tima")` against an installed namespace
  will fail with "could not find function" errors for internal helpers.
- Bioconductor deps require `BiocManager::install()` --- not
  `install.packages()`
- C code is compiled via `src/init.c`; modify `.Call()` wrappers only in
  `R/c_wrappers.R`
- `inst/scripts/docopt/*.txt` define CLI usage --- update when adding CLI
  options
- Do NOT run `R CMD check` without Bioconductor deps installed (~10+ min
  otherwise)

## Definition of Done

1. `air format . --check` passes (exit 0)
2. `Rscript tests/test-single.R test-<affected>.R` passes
3. `Rscript -e 'roxygen2::roxygenise(".")'` passes with no errors
4. If new code affects R → run `R CMD check . --no-manual` locally before
   pushing

## Code Style

- Follow tidyverse style guide (<https://style.tidyverse.org>)
- R formatting via Air: `air format .`
- roxygen2 with Markdown syntax for documentation
- 80-char line limit (enforced by air)
