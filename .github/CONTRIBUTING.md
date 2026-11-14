# Contributing to tima

Thank you for contributing to `tima`.

## Reporting Bugs and Suggesting Changes

Before starting larger changes, open an issue at
<https://github.com/taxonomicallyinformedannotation/tima/issues> describing the
problem or proposed change. For bugs, include a minimal reproducible example
along with your `tima` version and R `sessionInfo()`.

For typo or documentation fixes in R help pages, edit the roxygen2 comments in
the source `R/*.R` files rather than editing generated `man/*.Rd` files
directly.

## Local Development Setup

Clone the repository:

```bash
git clone https://github.com/taxonomicallyinformedannotation/tima.git
```

Install Bioconductor and CRAN/r-universe dependencies before running local
checks (Bioconductor packages require `BiocManager::install()`):

```r
install.packages(c("BiocManager", "devtools", "roxygen2", "testthat", "pkgload", "lintr"))
BiocManager::install(c("MetaboCoreUtils", "MsBackendMgf", "MsBackendMsp", "MsCoreUtils", "Spectra"))
```

## Pull Request Workflow

1. Create a focused branch from `main`.
2. Edit code and add or update unit tests under `tests/testthat/`.
3. Run the verification checks below before pushing:
   - Format R code with [Air](https://posit-dev.github.io/air/):
     ```bash
     air format .
     air format . --check
     ```

   - Static checks and linting:
     ```bash
     jarl check .
     Rscript --vanilla -e 'lintr::lint_dir("R")'
     ```

   - Run the offline test suite via `pkgload::load_all(".")` (required for
     internal helper access):
     ```bash
     TIMA_TESTS_OFFLINE=true Rscript --vanilla -e 'pkgload::load_all(".", export_all = TRUE, helpers = FALSE, quiet = TRUE); testthat::test_local(".")'
     ```

   - Regenerate roxygen documentation if any `@export` or roxygen tag changed:
     ```bash
     Rscript --vanilla -e 'roxygen2::roxygenise(".")'
     ```

   - Check spelling and full package build:
     ```bash
     Rscript --vanilla -e 'spelling::spell_check_package(".")'
     R CMD check . --no-manual --no-build-vignettes --no-vignettes
     ```
4. Add a concise bullet under `NEWS.md` for user-facing changes.

## Code Conventions

- Follow the tidyverse style guide (<https://style.tidyverse.org/>), enforced
  via `air format .` (80-character line width).
- Document functions with `roxygen2` using Markdown syntax.
- Ensure tests are deterministic, offline-safe under `TIMA_TESTS_OFFLINE=true`,
  and clean up temporary files with `withr::local_tempdir()` /
  `withr::local_tempfile()`.

## Code of Conduct

Please note that this project adheres to the [Code of
Conduct](CODE_OF_CONDUCT.md). By participating, you agree to abide by its terms.
