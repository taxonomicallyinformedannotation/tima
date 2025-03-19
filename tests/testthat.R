library(shinytest2)
library(testthat)

## Test the app
shinytest2::test_app(app_dir = "..", filter = "shinytest2")

## Test the package
testthat::test_package(package = "tima")
