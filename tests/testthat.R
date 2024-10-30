library(shinytest2)
library(testthat)

## Test the app
test_app(app_dir = "..", filter = "shinytest2")

## Test the package
test_package(package = "tima")
