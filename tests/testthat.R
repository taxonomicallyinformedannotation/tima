library(shinytest)
library(testthat)
library(timaR)

## Test the package
testthat::test_check(package = "timaR")

## Test the app
shinytest::testApp(appDir = dirname(list.files(
  pattern = "^app.R$", recursive = TRUE
)))
