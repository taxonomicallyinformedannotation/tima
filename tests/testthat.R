Sys.setenv("R_TESTS" = "")

library(testthat)
library(timaR)

testthat::test_check(package = "timaR")
