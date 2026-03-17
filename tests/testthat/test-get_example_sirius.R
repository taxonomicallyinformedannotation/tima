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

test_that("get_example_sirius downloads v5 and adjusted v6 paths", {
  seen <- list(url = character(0), export = character(0))

  local_mocked_bindings(
    get_file = function(url, export) {
      seen$url <<- c(seen$url, url)
      seen$export <<- c(seen$export, export)
      invisible(NULL)
    }
  )

  expect_invisible(
    get_example_sirius(
      url = list(
        v5 = "https://example.org/v5.zip",
        v6 = "https://example.org/v6.zip"
      ),
      export = list(
        v5 = "path/to/example_sirius_5.zip",
        v6 = "path/to/example_sirius_6.zip"
      )
    )
  )

  expect_equal(
    seen$url,
    c("https://example.org/v5.zip", "https://example.org/v6.zip")
  )
  expect_equal(
    seen$export,
    c("path/to/example_sirius_5.zip", "path/to/example_sirius.zip")
  )
})
