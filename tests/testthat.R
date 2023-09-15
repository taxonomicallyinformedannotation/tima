library(shinytest)
library(testthat)
library(timaR)

## Test the package
# testthat::test_check(package = "timaR")

## Test the app
setwd("../../")
system(
  command =
    "
    mkdir inst/app/tests/shinytest/inst/
    cp -R inst/ inst/app/tests/shinytest/inst/
    "
)
shinytest::testApp(appDir = dirname(list.files(
  pattern = "^app.R$", recursive = TRUE
)))
