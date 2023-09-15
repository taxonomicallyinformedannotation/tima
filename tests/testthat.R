library(shinytest)
library(testthat)
library(timaR)

## Test the package
testthat::test_check(package = "timaR")

## Test the app
shinytest::testApp(appDir = list.dirs()[grepl(pattern = "app$", list.dirs())])
