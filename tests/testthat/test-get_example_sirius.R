# Test Suite: get_example_sirius ----

library(testthat)

test_that("get_example_sirius validates url structure", {
  expect_error(
    get_example_sirius(
      url = list(v5 = "https://example.org/v5.zip"),
      export = list(v5 = "v5.zip", v6 = "v6_6.zip")
    ),
    "url",
    class = "tima_validation_error"
  )
})

test_that("get_example_sirius validates export structure", {
  expect_error(
    get_example_sirius(
      url = list(
        v5 = "https://example.org/v5.zip",
        v6 = "https://example.org/v6.zip"
      ),
      export = list(v5 = "v5.zip")
    ),
    "export",
    class = "tima_validation_error"
  )
})

test_that("adjust_sirius_v6_export_path removes _6 suffix", {
  expect_identical(
    adjust_sirius_v6_export_path("path/to/example_sirius_6.zip"),
    "path/to/example_sirius.zip"
  )
  expect_identical(
    adjust_sirius_v6_export_path("path/to/example_sirius.zip"),
    "path/to/example_sirius.zip"
  )
})
