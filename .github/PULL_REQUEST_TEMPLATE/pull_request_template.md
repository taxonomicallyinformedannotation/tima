# Fixes #(issue)

## Summary

Brief description of what changed and why.

## Type of Change

- [ ] Bug fix (non-breaking fix for an incorrect behavior or crash)
- [ ] New feature or enhancement
- [ ] Documentation or vignette update
- [ ] CI, build, or test infrastructure change
- [ ] Breaking change (explain migration path below)

## Verification

- [ ] `air format . --check` passes
- [ ] `jarl check .` and `Rscript --vanilla -e 'lintr::lint_dir("R")'` pass
- [ ] Offline test suite passes (`TIMA_TESTS_OFFLINE=true`)
- [ ] `Rscript --vanilla -e 'roxygen2::roxygenise(".")'` is up to date (if R
  docs or `@export` changed)
- [ ] `R CMD check . --no-manual --no-build-vignettes --no-vignettes` passes (if
  R or C code changed)
- [ ] Updated documentation / `NEWS.md` where user-facing behavior changed

## Checklist

- [ ] No unintended changes to unrelated files or local data artifacts
- [ ] No leftover `browser()`, `print()`, `debug()`, or temporary files
- [ ] New or modified behavior is covered by tests in `tests/testthat/`
