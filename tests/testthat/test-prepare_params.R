# Test Suite: prepare_params & get_params ----

library(testthat)

stage_installed_params <- function() {
  params_root <- system.file("params", package = "tima")
  skip_if(!nzchar(params_root), "Installed params directory not available")

  src_files <- list.files(params_root, recursive = TRUE, full.names = TRUE)
  rel_files <- list.files(params_root, recursive = TRUE, full.names = FALSE)
  dst_files <- file.path("params", rel_files)

  for (dst in unique(dirname(dst_files))) {
    dir.create(dst, recursive = TRUE, showWarnings = FALSE)
  }
  ok <- file.copy(from = src_files, to = dst_files, overwrite = TRUE)
  stopifnot(all(ok))
}

test_that("prepare_params returns YAML export paths in a temp project", {
  withr::local_dir(new = temp_test_dir("prepare_params_integration"))
  stage_installed_params()

  paths <- prepare_params()
  expect_type(paths, "character")
  expect_true(length(paths) > 0L)
  expect_true(all(file.exists(paths)))
})

test_that("prepare_params validates list inputs", {
  withr::local_dir(new = temp_test_dir("prepare_params_validate"))
  stage_installed_params()

  expect_error(
    prepare_params(params_small = "bad"),
    "In index: 1\\.|invalid for atomic vectors"
  )
  expect_error(
    prepare_params(params_advanced = "bad"),
    "In index: 1\\.|invalid for atomic vectors"
  )
})

test_that("get_params rejects missing or invalid step values", {
  expect_error(
    get_params(NULL),
    "must be provided",
    class = "tima_validation_error"
  )
  expect_error(
    get_params(""),
    "must be provided",
    class = "tima_validation_error"
  )
  expect_error(
    get_params(NA_character_),
    "must be provided",
    class = "tima_validation_error"
  )
  expect_error(
    get_params(c("a", "b")),
    "single character",
    class = "tima_validation_error"
  )
})

test_that("prepare_params accepts explicit fixture lists", {
  withr::local_dir(new = temp_test_dir("prepare_params_manual"))
  stage_installed_params()

  expect_silent(prepare_params(
    params_small = get_params(step = "prepare_params"),
    params_advanced = get_params(step = "prepare_params_advanced")
  ))
})
