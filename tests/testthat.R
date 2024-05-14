library(shinytest2)
library(testthat)
library(timaR)

## Test the package
test_check(package = "timaR")

## Test the app
test_app(app_dir = "..", filter = "shinytest2")
