# Test Suite: run_app ----

library(testthat)

test_that("run_app validates parameters", {
  expect_error(run_app(host = ""), "host")
  expect_error(run_app(port = 0), "port")
  expect_error(run_app(port = 70000), "port")
  expect_error(run_app(browser = "yes"), "browser")
})

test_that("app_path_exists reports file existence", {
  tmp <- tempfile(fileext = ".R")
  writeLines("# app stub", tmp)

  expect_true(app_path_exists(tmp))
  expect_false(app_path_exists(paste0(tmp, ".missing")))
})

test_that("is_docker_env returns a scalar logical", {
  out <- is_docker_env()
  expect_type(out, "logical")
  expect_length(out, 1)
})

test_that("get_app_path returns a character scalar", {
  out <- get_app_path()
  expect_type(out, "character")
  expect_length(out, 1)
})
