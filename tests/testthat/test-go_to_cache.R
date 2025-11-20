# Test Suite: go_to_cache ----

library(testthat)

test_that("go_to_cache validates directory name", {
  expect_error(go_to_cache(dir = NULL), "non-empty")
  expect_error(go_to_cache(dir = ""), "non-empty")
  expect_error(go_to_cache(dir = c("a", "b")), "non-empty")
})

test_that("go_to_cache creates and switches directory", {
  # Use temp dir name inside home path simulation: create subdir then change
  # We cannot alter HOME easily; just call function with custom dirname
  expect_no_error(go_to_cache(dir = ".tima_test_cache"))
  # Confirm working dir ends with provided dir name
  wd <- getwd()
  expect_true(grepl(".tima_test_cache$", wd))
})
