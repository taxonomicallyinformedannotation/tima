# Testing Guide for TIMA

This document describes the testing conventions, coverage workflow, and
mutation-testing protocol for the `tima` R package.

## Core Principles

1. **Behavior and mutation resistance over line coverage**: Every test must
   assert on concrete outcomes (`expect_identical`, `expect_equal` with explicit
   `tolerance`, `expect_error(..., class = ...)`) and must fail if an operator
   (`>` vs `>=`, `&&` vs `||`), constant, negation, or branch is mutated.
   Coverage-only tests that execute a function without verifying its return
   value or side effects are prohibited.
2. **Independent expected values**: Compute expected outputs by hand or via a
   trivially verifiable reference calculation---never by calling the function
   under test.
3. **Complete equivalence classes and boundaries**: Cover `0`, `1`, `n - 1`,
   `n`, `n + 1`, empty inputs (`character(0)`, zero-row data frames/tidytables),
   `NA` / `NaN` / `Inf` / `NULL`, single-row vs multi-row tables, duplicates,
   unsorted inputs, and non-ASCII/UTF-8 strings.
4. **Both sides of every conditional**: Exercise both the `TRUE` and `FALSE`
   branches with inputs that produce distinct outputs.
5. **Deterministic and isolated**:
   - Use `withr::local_tempdir()`, `withr::local_tempfile()`,
     `withr::local_options()`, and `withr::local_envvar()` so tests leave no
     filesystem or session state behind.
   - Use `testthat::local_mocked_bindings()` only to isolate external network,
     filesystem, or system boundaries---never to mock the logic under test.
   - When using randomized inputs for property checks (idempotence,
     commutativity, round-trip, order invariance), fix the seed via
     `withr::local_seed()`.

## Running Tests Locally

TIMA tests access internal (non-exported) package helpers and **must** load the
package via `pkgload::load_all(".")` (handled automatically by
`tests/testthat/setup.R` when running `test_file()` or `devtools::test()`).

### Run a single test file (fast iteration)

```bash
Rscript -e '
  pkgload::load_all(".", export_all = TRUE, helpers = FALSE, quiet = TRUE)
  testthat::test_file("tests/testthat/test-columns_utils.R")
'
```

### Run the full test suite

```bash
Rscript -e '
  pkgload::load_all(".", export_all = TRUE, helpers = FALSE, quiet = TRUE)
  testthat::test_local(".")
'
```

To skip live network calls deterministically (for example, GNPS or Open Tree of
Life API checks gated by `skip_if_no_network()`):

```bash
TIMA_TESTS_OFFLINE=true Rscript -e '
  pkgload::load_all(".", export_all = TRUE, helpers = FALSE, quiet = TRUE)
  testthat::test_local(".")
'
```

## Measuring Test Coverage (`covr`)

### File-level coverage (recommended during development)

Note that `covr` sets `NOT_CRAN=true` internally; export
`TIMA_TESTS_OFFLINE=true` to keep runs fast and offline:

```bash
Rscript -e '
  Sys.setenv(TIMA_TESTS_OFFLINE = "true")
  pkgload::load_all(".", export_all = TRUE, helpers = FALSE, quiet = TRUE)
  cov <- covr::file_coverage(
    source_files = "R/columns_utils.R",
    test_files = "tests/testthat/test-columns_utils.R"
  )
  print(covr::percent_coverage(cov))
  print(covr::zero_coverage(cov))
'
```

### Full package coverage

```bash
Rscript -e '
  Sys.setenv(TIMA_TESTS_OFFLINE = "true")
  cov <- covr::package_coverage(type = "tests")
  print(cov)
'
```

Aim for **>= 90% line coverage** on every touched file as a floor, with any
remaining uncovered lines justified or tested.

## Mutation Testing

Line coverage only proves that a line executed; mutation testing proves that the
test suite *detects* faults on that line.

Because `pkgload::load_all(".", export_all = TRUE)` binds internal functions in
both the package namespace (`asNamespace("tima")`) and the attached package
environment (`as.environment("package:tima")`), in-memory mutation testing can
update both bindings directly in ~0.2 seconds per mutant without recompiling C
code or reloading the package.

### Fast in-memory mutation testing harness

```r
Sys.setenv(TIMA_TESTS_OFFLINE = "true")
pkgload::load_all(".", export_all = TRUE, helpers = FALSE, quiet = TRUE)

ns <- asNamespace("tima")
pkg_env <- as.environment("package:tima")

set_tima_fn <- function(fn_name, fn_obj) {
  environment(fn_obj) <- ns
  rlang::env_binding_unlock(ns, fn_name)
  assign(fn_name, fn_obj, envir = ns)
  rlang::env_binding_lock(ns, fn_name)
  if (exists(fn_name, envir = pkg_env, inherits = FALSE)) {
    rlang::env_binding_unlock(pkg_env, fn_name)
    assign(fn_name, fn_obj, envir = pkg_env)
    rlang::env_binding_lock(pkg_env, fn_name)
  }
}

run_mutant <- function(id, file, line, old_code, new_code, test_files) {
  orig_lines <- readLines(file)
  on.exit(
    {
      writeLines(orig_lines, file)
      env <- new.env(parent = ns)
      sys.source(file, envir = env)
      for (nm in ls(env, all.names = TRUE)) {
        if (is.function(env[[nm]]) && exists(nm, envir = ns, inherits = FALSE)) {
          set_tima_fn(nm, env[[nm]])
        }
      }
    },
    add = TRUE
  )

  mut_lines <- orig_lines
  mut_lines[line] <- sub(old_code, new_code, mut_lines[line], fixed = TRUE)
  stopifnot(!identical(mut_lines, orig_lines))
  writeLines(mut_lines, file)

  env <- new.env(parent = ns)
  sys.source(file, envir = env)
  for (nm in ls(env, all.names = TRUE)) {
    if (is.function(env[[nm]]) && exists(nm, envir = ns, inherits = FALSE)) {
      set_tima_fn(nm, env[[nm]])
    }
  }

  killed <- FALSE
  for (tf in test_files) {
    res <- tryCatch(
      {
        df <- as.data.frame(testthat::test_file(tf, reporter = "silent"))
        any(df$failed > 0L | df$error)
      },
      error = function(e) TRUE
    )
    if (isTRUE(res)) {
      killed <- TRUE
      break
    }
  }
  message(sprintf("[%s] %s:%d -> %s", id, file, line, if (killed) "KILLED" else "SURVIVED"))
  killed
}
```

### Standard mutation operators to check on touched functions

- **Relational boundary shifts**: `>` <-> `>=`, `<` <-> `<=`, `==` <-> `!=`
- **Logical operator swaps**: `&&` <-> `||`, `&` <-> `|`, removing `!`
- **Constant perturbations**: `0L` -> `1L`, `1L` -> `2L`, `-1L` -> `0L`,
  tolerance/threshold scaling (`* 1e-6` -> `* 1e-5`)
- **Branch / guard removal**: replacing an early-return or validation condition
  with `if (FALSE)` or `if (TRUE)`
- **State / attribute cleanup removal**: removing
  `attr(x, ".match.hash") <- NULL` or directory creation calls

Every surviving mutant must either be killed by adding a targeted test or
documented as an **equivalent mutant** (e.g., an optimization fast-path whose
fallback computes the exact same mathematical output).
