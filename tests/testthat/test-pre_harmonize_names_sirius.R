# Test Suite: pre_harmonize_names_sirius ----

library(testthat)

test_that("pre_harmonize_names_sirius removes suffix after slash", {
  expect_equal(
    pre_harmonize_names_sirius("column_name/suffix"),
    "column_name"
  )

  expect_equal(
    pre_harmonize_names_sirius("some_data/extra_info"),
    "some_data"
  )
})

test_that("pre_harmonize_names_sirius handles names without slash", {
  expect_equal(
    pre_harmonize_names_sirius("simple_name"),
    "simple_name"
  )

  expect_equal(
    pre_harmonize_names_sirius("another_column"),
    "another_column"
  )
})

test_that("pre_harmonize_names_sirius handles multiple slashes", {
  # Should remove from first slash onwards
  expect_equal(
    pre_harmonize_names_sirius("column/suffix1/suffix2"),
    "column"
  )
})

test_that("pre_harmonize_names_sirius handles empty string", {
  expect_equal(pre_harmonize_names_sirius(""), "")
})

test_that("pre_harmonize_names_sirius handles slash at start", {
  expect_equal(pre_harmonize_names_sirius("/column"), "")
})

test_that("pre_harmonize_names_sirius works with vector input", {
  input <- c("col1/suffix", "col2", "col3/data/extra")
  expected <- c("col1", "col2", "col3")

  expect_equal(pre_harmonize_names_sirius(input), expected)
})
