library(testthat)
## Sourcing all functions
list.files("R", full.names = TRUE) |>
  lapply(FUN = source)

testthat::test_check(package = "timaR")
