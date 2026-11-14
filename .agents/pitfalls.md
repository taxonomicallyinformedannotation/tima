# TIMA Audit Pitfalls and Guardrails

## Test Suite Traps

- Installed namespace shadowing: `tests/testthat/setup.R` calls
  `pkgload::load_all()` only when `requireNamespace("tima")` returns `FALSE`. If
  `tima` is already installed in your R library, plain `test_local()` or
  `test_check("tima")` tests the installed namespace instead of edited internal
  functions. Always run
  `pkgload::load_all(".", export_all = TRUE, quiet = TRUE)` in test commands.
- `R CMD check` runner trap: `tests/testthat.R` must call
  `testthat::test_check("tima")`, never `testthat::test_package("tima")`.
  Because `inst/tests/testthat/` is installed into the package library for
  `shinytest2`, `test_package("tima")` resolves
  `system.file("tests", "testthat", package = "tima")` to `inst/tests/testthat/`
  and skips all 158 files in `tests/testthat/`.
- Trailing CLI argument collision: `Rscript --vanilla -e '...' file.R` passes
  `file.R` into `commandArgs(trailingOnly = TRUE)`, which `get_params()` reads
  via `docopt`. Keep filenames inside the `-e` expression.
- Real home directory pollution: tests touching `~/.tima` or user config paths
  must call `local_test_home()` (`tests/testthat/helper-home.R`) instead of
  writing to `$HOME` or leaving files in `tests/testthat/data/`.
- Network determinism: call `skip_if_no_network()`
  (`tests/testthat/helper-network.R`) before live HTTP/API calls so
  `TIMA_TESTS_OFFLINE=true` produces deterministic offline skips rather than
  flaky failures.

## Linter and Static Check Constraints

- `jarl`: forbids superassignment `<<-` (use `new.env(parent = emptyenv())` for
  mutable test or helper state) and flags outer negations such as `any(!x)` in
  favor of `!all(x)` or `all(...)`.
- `.lintr`: delegates formatting, object naming, commented-code, and
  `object_usage` rules (handled separately by `air`, `jarl`, and
  `codetools::checkUsagePackage()`) while enforcing semantic lints on `R/` with
  zero findings.

## Pre-Commit Hooks and Docs Parity

- `prek.toml` runs `readme-synced`, which fails if `README.md` is more than one
  second older than `README.qmd`. Always verify timestamp parity after
  formatting or editing `README.qmd`.
- `prek.toml` runs `file-contents-sorter` on `.Rbuildignore` (`LC_ALL=C` byte
  order).
- `forbid-debug-calls` fails if `browser()`, `debug()`, or `debugonce()` appear
  in `R/` or `inst/`.

## Runtime and Environment Caveats

- `R/install.R`: `.normalize_install_type()` validates `type` (including `NA`
  and non-character inputs) and collapses platform binary strings such as
  `mac.binary.*` and `win.binary` to `"binary"` before delegating to
  `.install_packages()`.
- `inst/python/process_smiles.py`: imports `chembl_structure_pipeline`, which is
  not guaranteed to be installed in system Python outside the configured
  `reticulate`/Conda environment. Static lint passes via
  `ruff check inst/python`, while live execution requires a Python environment
  containing `chembl_structure_pipeline`.
