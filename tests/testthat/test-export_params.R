# Test Suite: export_params ----

library(testthat)

test_that("export_params writes YAML file to specified directory", {
  td <- withr::local_tempdir()
  out <- export_params(
    parameters = list(alpha = 1L, beta = "ok"),
    step = "unit_test",
    directory = td
  )
  expect_null(out)
  files <- list.files(td, pattern = "_unit_test[.]yaml$", full.names = TRUE)
  expect_length(files, 1L)
  parsed <- yaml::read_yaml(files[[1L]])
  expect_identical(parsed$alpha, 1L)
  expect_identical(parsed$beta, "ok")
})

test_that("export_params raises classed runtime error when write fails", {
  local_mocked_bindings(
    write_yaml = function(x, file, ...) {
      force(x)
      force(file)
      unused_dots <- list(...)
      invisible(unused_dots)
      stop("mock write failure")
    },
    .package = "yaml"
  )

  expect_error(
    export_params(
      parameters = list(alpha = 1),
      step = "unit_test",
      directory = tempdir()
    ),
    "failed to export parameters",
    class = "tima_runtime_error"
  )
})
