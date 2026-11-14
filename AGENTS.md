# TIMA Agent Guidelines

TIMA (`tima`) is an R package with a C spectral-scoring extension (`src/`), a
`targets` pipeline (`inst/targets/`), a Shiny app (`inst/app*.R`), 36 CLI
scripts (`inst/scripts/`), and Python helpers (`inst/python/`). R >= 4.4.0.

## Verified Commands

Always run `Rscript --vanilla -e '...'`. Never append extra trailing CLI
arguments after `-e`, because `get_params()` inspects
`commandArgs(trailingOnly = TRUE)` via `docopt`.

```sh
# Full offline test suite (use load_all so internal helpers are visible)
TIMA_TESTS_OFFLINE=true Rscript --vanilla -e 'pkgload::load_all(".", export_all = TRUE, helpers = FALSE, quiet = TRUE); testthat::test_local(".", reporter = "summary", stop_on_failure = TRUE)'

# Single test file
TIMA_TESTS_OFFLINE=true Rscript --vanilla -e 'pkgload::load_all(".", export_all = TRUE, quiet = TRUE); testthat::test_file("tests/testthat/test-NAME.R", stop_on_failure = TRUE)'

# Formatting and R static checks
air format .
air format . --check
jarl check .
Rscript --vanilla -e 'stopifnot(length(lintr::lint_dir("R")) == 0L)'
Rscript --vanilla -e 'pkgload::load_all(".", quiet = TRUE); env <- new.env(parent = emptyenv()); env$out <- character(); codetools::checkUsagePackage("tima", report = function(s) env$out <- c(env$out, s), skipWith = TRUE, suppressLocal = FALSE, suppressUndefined = utils::globalVariables(package = "tima")); stopifnot(length(env$out) == 0L)'

# Docs and spellcheck
Rscript --vanilla -e 'roxygen2::roxygenise(".")'
Rscript --vanilla -e 'stopifnot(nrow(spelling::spell_check_package(".")) == 0L)'

# Python, TOML, Markdown, and all pre-commit hooks
ruff check inst/python
tombi lint prek.toml
panache lint .
prek run --all-files

# Clean build artifacts (zsh null-glob syntax)
rm -f -- src/*.o(N) src/*.so(N)

# Package build and offline check (--no-build-vignettes emits 2 expected inst/doc warnings)
R CMD build . --no-build-vignettes
TIMA_TESTS_OFFLINE=true _R_CHECK_FORCE_SUGGESTS_=false _R_CHECK_CRAN_INCOMING_=false R CMD check tima_*.tar.gz --no-manual --no-build-vignettes --no-vignettes
```

## Repo Map

- `R/` (144 files): package logic; entry points `R/run_tima.R`, `R/run_app.R`; C
  `.Call()` wrappers in `R/c_wrappers.R`; matrix sanitization in
  `R/sanitize_spectrum_matrix.R`; rank column maps in `R/columns_utils.R`.
- `src/`: C GNPS similarity (`gnps.c`, `gnps_portable.h`, `tima.h`, `init.c`).
- `inst/`: Shiny app (`app.R`, `app_ui.R`, `app_server.R`), targets workflow
  (`_targets.R`, `targets/*.R`), CLI (`scripts/*.R`, `scripts/docopt/*.txt`),
  config (`params/`, `paths.yaml`), mzTab schemas (`contracts/`), Python helpers
  (`python/`).
- `tests/testthat/`: 158 `test-*.R` files, `setup.R`, `helper-*.R`, and repo
  guardrails in `test-repo_integrity.R`.
- `man/` (524 `.Rd` files), `vignettes/` (Quarto), `prek.toml`, `.lintr`,
  `.github/workflows/`.

## Non-Negotiable Invariants

- Human docs (`README.qmd`, `README.md`, `TESTING.md`, `vignettes/`, `.github/`,
  `R/` roxygen, `man/`) must never mention or link to `AGENTS.md`, `.agents/`,
  `.claudeignore`, `.agentignore`, `.mcp.json`, or `.agent-work/`.
- Agent docs may reference human docs (`README.md`, `TESTING.md`). Keep all
  edits free of decorative emojis and filler claims.
- Modify `.Call()` wrappers only in `R/c_wrappers.R` and register C entry points
  in `src/init.c` and `src/tima.h`.
- Spectra passed to `C_gnps` / `C_gnps_chain_dp*` must be finite, strictly
  positive, sorted by m/z, and deduplicated within tolerance.
- Keep `inst/scripts/docopt/*.txt`, `inst/scripts/*.R`, and
  `R/parse_cli_params.R` synchronized when changing CLI flags or steps.
- Keep `.Rbuildignore` sorted with `LC_ALL=C sort -u` and zero blank lines.
- Check `README.md` mtime >= `README.qmd` mtime after doc edits.
- Remove `src/*.o` and `src/*.so` after `roxygen2::roxygenise(".")` or
  `pkgload::load_all()`.

## Deeper References

- [architecture.md](.agents/architecture.md): targets loading, C extension,
  15-rank taxonomy schema, and packaging boundaries.
- [pitfalls.md](.agents/pitfalls.md): known test-runner, linter, Python, and
  hook traps found during audit.

## Definition of Done

1. `air format . --check`, `jarl check .`, `ruff check inst/python`, and
   `lintr::lint_dir("R")` pass with zero errors.
2. `roxygen2::roxygenise(".")` and `spelling::spell_check_package(".")` succeed
   with zero unexpected diffs or spelling errors.
3. Offline `testthat::test_local()` and `test-repo_integrity.R` pass with zero
   failures, clean working tree (`src/*.o`, `src/*.so` removed), and
   `prek run --all-files` passes.
