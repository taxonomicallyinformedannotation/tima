# ==============================================================================
# Test Suite: tima_full (smoke)
# ==============================================================================
library(testthat)
library(tima)

test_that("tima_full runs workflow entry point (skipped heavy)", {
  skip_on_cran()
  skip("Full workflow skipped in unit tests")
  expect_invisible(tima_full())
})

