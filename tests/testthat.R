library(shinytest2)
library(spelling)
library(testthat)
library(tima)

## Test spelling
spell_check_test(
  vignettes = TRUE,
  error = FALSE,
  skip_on_cran = TRUE
)

## Test the package
test_package(package = "tima")

## Test the app
test_app(app_dir = "..", filter = "shinytest2")
