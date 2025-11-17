# ============================================================================
# Test Suite: prepare_params & get_params (fixture-based)
# ============================================================================

library(testthat)

# Ensure param fixtures are available (loaded via setup sourcing fixtures/*.R)

# Helper to build small & advanced param fixtures with an ID
fixture_small <- function(id = "TID") create_fixture_params_small(id)
fixture_adv <- function(id = "TID") create_fixture_params_advanced(id)
fixture_yamls <- function(id = "TID") create_fixture_yamls_params(id)

# ----------------------------------------------------------------------------
# Test: prepare_params combines small & advanced and performs id replacement
# ----------------------------------------------------------------------------

test_that("prepare_params merges fixtures and replaces {id}", {
  options(tima_id = "ABC001")
  testthat::local_mocked_bindings(
    load_yaml_files = function() fixture_yamls("ABC001"),
    get_params = function(step) {
      if (step == "prepare_params") {
        return(fixture_small("ABC001"))
      }
      if (step == "prepare_params_advanced") {
        return(fixture_adv("ABC001"))
      }
      stop("unexpected step")
    },
    export_params = function(...) character(),
    create_dir = function(export) invisible(NULL)
  )
  paths <- prepare_params()
  expect_type(paths, "character")
  # Check replacement for one propagated structural path template
  tmpl <- fixture_yamls(
    "ABC001"
  )$yamls_params$annotate_masses$files$features$prepared$tsv
  replaced <- sub("{id}", getOption("tima_id"), tmpl, fixed = TRUE)
  expect_match(replaced, "ABC001")
})

# ----------------------------------------------------------------------------
# Test: prepare_params handles empty taxon gracefully
# ----------------------------------------------------------------------------

test_that("prepare_params handles empty taxon (removes raw metadata) without error", {
  small <- fixture_small("AXX")
  small$organisms$taxon <- "" # triggers org_tax NULL logic
  testthat::local_mocked_bindings(
    load_yaml_files = function() fixture_yamls("AXX"),
    get_params = function(step) {
      if (step == "prepare_params") {
        return(small)
      }
      if (step == "prepare_params_advanced") {
        return(fixture_adv("AXX"))
      }
    },
    export_params = function(...) character(),
    create_dir = function(export) invisible(NULL)
  )
  expect_silent(prepare_params())
})

# ----------------------------------------------------------------------------
# Test: input validation errors
# ----------------------------------------------------------------------------

test_that("prepare_params validates list inputs", {
  expect_error(prepare_params(params_small = "bad"), "list")
  expect_error(prepare_params(params_advanced = "bad"), "list")
})

# ----------------------------------------------------------------------------
# get_params: invalid step value handling
# ----------------------------------------------------------------------------

test_that("get_params rejects missing or invalid step values", {
  expect_error(get_params(NULL), "must be provided")
  expect_error(get_params(""), "must be provided")
  expect_error(get_params(NA_character_), "does not exist")
  # Length >1 should error; capture error containing length diagnostic
  expect_error(get_params(c("a", "b")), "length = 2")
})

# ----------------------------------------------------------------------------
# get_params: unknown step
# ----------------------------------------------------------------------------

test_that("get_params errors for unknown step", {
  # Create a temp script directory mimicking docopt scripts
  tmp_scripts <- withr::local_tempdir()
  writeLines(
    "Usage: prepare_params",
    file.path(tmp_scripts, "prepare_params.txt")
  )
  writeLines(
    "Usage: annotate_masses",
    file.path(tmp_scripts, "annotate_masses.txt")
  )

  testthat::local_mocked_bindings(
    .env = baseenv(),
    get_default_paths = function() {
      list(
        params = list(
          default = list(path = "params/default"),
          prepare_params = "params/prepare_params.yaml"
        )
      )
    },
    system.file = function(subdir = NULL, package = NULL) tmp_scripts,
    parse_cli_params = function(...) list(),
    parse_yaml_params = function(...) list(parameters = list())
  )

  expect_error(get_params("unknown"), "does not exist")
})

# ----------------------------------------------------------------------------
# prepare_params manual param injection
# ----------------------------------------------------------------------------

test_that("prepare_params accepts explicit fixture lists", {
  testthat::local_mocked_bindings(
    load_yaml_files = function() fixture_yamls("MANUAL"),
    get_params = function(step) {
      # Mock get_params to handle internal call from replace_id
      if (step == "prepare_params") {
        return(fixture_small("MANUAL"))
      }
      if (step == "prepare_params_advanced") {
        return(fixture_adv("MANUAL"))
      }
      stop("Unexpected step in test: ", step)
    },
    export_params = function(...) character(),
    create_dir = function(export) invisible(NULL)
  )
  expect_silent(prepare_params(
    params_small = fixture_small("MANUAL"),
    params_advanced = fixture_adv("MANUAL")
  ))
})
