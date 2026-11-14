# Getting Help with tima

## Documentation

Start with the package vignettes and reference documentation:

- Package site and articles:
  <https://taxonomicallyinformedannotation.github.io/tima/>
- Validating inputs before running the pipeline: `tima::validate_inputs()`

## Asking Questions and Reporting Issues

Before opening a new issue, search [existing
issues](https://github.com/taxonomicallyinformedannotation/tima/issues) to check
whether the problem has already been reported or fixed.

When filing a bug report or usage question on [GitHub
Issues](https://github.com/taxonomicallyinformedannotation/tima/issues/new/choose),
include:

- A minimal reproducible example with small sample data or a parameter snippet
- Which entry point you used (`run_tima()`, `run_app()`, CLI scripts in
  `inst/scripts/`, or `targets::tar_make()`)
- Your `tima` version and `sessionInfo()` output
