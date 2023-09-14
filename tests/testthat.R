library(shinytest2)
library(testthat)
library(timaR)

## Test the package
testthat::test_check(package = "timaR")
## Test the functions
shinytest2::test_app()
