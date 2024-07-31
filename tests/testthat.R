library(shinytest2)
library(testthat)
library(tima)

## Test the package
test_check(package = "tima")

## Test the app
test_app(app_dir = "..", filter = "shinytest2")
