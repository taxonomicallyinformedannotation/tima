library(testthat)


test_that("package lifecycle hooks run without error", {
  expect_no_error(.onLoad(libname = "", pkgname = "tima"))
  expect_message(.onAttach(libname = "", pkgname = "tima"), "Welcome to tima")
})
