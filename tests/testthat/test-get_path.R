# Test Suite: get_path ----

library(testthat)

test_that("get_path validates input", {
  expect_error(
    get_path(""),
    class = "tima_error"
  )
})

test_that("get_path returns existing base path as-is", {
  p <- temp_test_path("get_path_as_is.tsv")
  writeLines("ok", p)

  expect_identical(get_path(p), p)
})

test_that("get_path resolves by removing inst prefix", {
  root <- temp_test_dir("get_path_without_inst")
  base_path <- file.path(root, "inst", "params", "default.yaml")
  resolved <- gsub("inst", "", base_path, fixed = TRUE)
  dir.create(dirname(resolved), recursive = TRUE, showWarnings = FALSE)
  writeLines("a: 1", resolved)

  expect_identical(get_path(base_path), resolved)
})

test_that("get_path resolves against installed package directory", {
  pkg_dir <- system.file(package = "tima")
  skip_if(
    pkg_dir == "",
    "Installed package path unavailable in this test context"
  )

  withr::local_dir(temp_test_dir("get_path_pkg_dir"))
  expected <- file.path(pkg_dir, "paths.yaml")
  skip_if_not(
    file.exists(expected),
    "paths.yaml not available in installed package path"
  )

  expect_identical(get_path("inst/paths.yaml"), expected)
})
