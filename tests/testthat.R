library(shinytest2)
library(testthat)
library(timaR)

## Test the package
testthat::test_check(package = "timaR")

## Test the app
shinytest2::test_app(filter = "shinytest2")
