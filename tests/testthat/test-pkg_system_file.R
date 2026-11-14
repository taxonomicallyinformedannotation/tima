# Test Suite: pkg_system_file ----

library(testthat)

test_that("pkg_system_file resolves existing top-level and nested package files", {
  paths_yaml <- pkg_system_file("paths.yaml")
  expect_type(paths_yaml, "character")
  expect_length(paths_yaml, 1L)
  expect_true(nzchar(paths_yaml))
  expect_true(file.exists(paths_yaml))

  docopt_txt <- pkg_system_file("scripts", "docopt", "weight_annotations.txt")
  expect_true(nzchar(docopt_txt))
  expect_true(file.exists(docopt_txt))
})

test_that("pkg_system_file returns empty string for missing files", {
  missing <- pkg_system_file("nonexistent_dir", "missing_file_12345.yaml")
  expect_identical(missing, "")
})
