# Test Suite: export_params ----

library(testthat)

test_that("export_params function exists", {
  expect_true(exists("export_params"))
  expect_type(export_params, "closure")
})

test_that("export_params raises classed runtime error when write fails", {
  local_mocked_bindings(
    write_yaml = function(...) {
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

