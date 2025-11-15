# ==============================================================================
# Test Suite: complement_metadata_structures
# ==============================================================================
library(testthat)
library(tima)

test_that("complement_metadata_structures handles empty input", {
  result <- complement_metadata_structures(tidytable::tidytable())
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
})

