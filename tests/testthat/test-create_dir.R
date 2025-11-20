# Test Suite: create_dir ----

library(testthat)

test_that("create_dir creates directory for file path", {
  target <- file.path("sub/dir/out.tsv")
  create_dir(export = target)
  expect_true(dir.exists(file.path("sub/dir")))
})

test_that("create_dir validates input", {
  expect_error(create_dir(export = NULL), "non-empty")
  expect_error(create_dir(export = ""), "non-empty")
  # Vector input should error about single character
  expect_error(create_dir(export = c("a", "b")), "'length = 2")
})
