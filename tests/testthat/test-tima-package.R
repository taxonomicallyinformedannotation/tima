library(testthat)


test_that("package lifecycle hooks run without error", {
  ns <- asNamespace("tima")
  onLoad <- get(".onLoad", envir = ns) # nolint: object_name_linter.
  onAttach <- get(".onAttach", envir = ns) # nolint: object_name_linter.

  expect_no_error(onLoad(libname = "", pkgname = "tima"))
  expect_message(onAttach(libname = "", pkgname = "tima"), "Welcome to tima")
})
