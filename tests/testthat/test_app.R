## Test the functions
setwd("../../")
testthat::test_that("Test app", {
  shinytest2::test_app()
  succeed()
})
