## Test the app
testthat::test_that("Test app", {
  setwd("../../")
  shinytest2::test_app()
  succeed()
})
