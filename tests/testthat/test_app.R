## Test the app
testthat::test_uthat("Test app", {
  setwd("../../")
  shinytest2::test_app()
  succeed()
})
