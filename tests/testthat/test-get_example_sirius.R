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

test_that("get_example_sirius downloads v5 and adjusted v6 paths", {
  calls <- list()

  with_mocked_bindings(
    get_file = function(url, export) {
      calls[[length(calls) + 1L]] <<- list(url = url, export = export)
      invisible(NULL)
    },
    log_debug = function(...) invisible(NULL),
    log_info = function(...) invisible(NULL),
    {
      result <- get_example_sirius(
        url = list(
          v5 = "https://example.org/v5.zip",
          v6 = "https://example.org/v6.zip"
        ),
        export = list(v5 = "example_sirius_5.zip", v6 = "example_sirius_6.zip")
      )

      expect_null(result)
      expect_equal(length(calls), 2)
      expect_identical(calls[[1L]]$url, "https://example.org/v5.zip")
      expect_identical(calls[[1L]]$export, "example_sirius_5.zip")
      expect_identical(calls[[2L]]$url, "https://example.org/v6.zip")
      expect_identical(calls[[2L]]$export, "example_sirius.zip")
    }
  )
})
