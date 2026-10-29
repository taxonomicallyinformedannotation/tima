# Wall-clock assertions ----

#' Expect an elapsed time to stay within a budget
#'
#' @description Budgets are written for a typical laptop. They are scaled by
#'     `TIMA_TEST_TIME_FACTOR` (default 5) so that slower or loaded machines,
#'     such as shared CI runners, do not fail by chance. The point of these
#'     tests is to catch order-of-magnitude regressions, not machine speed.
#'
#' @param elapsed Elapsed time in seconds (a number or a `system.time()`
#'     "elapsed" element).
#' @param budget Budget in seconds on a typical laptop.
expect_fast <- function(elapsed, budget) {
  factor <- suppressWarnings(
    as.numeric(Sys.getenv("TIMA_TEST_TIME_FACTOR", "5"))
  )
  if (!is.finite(factor) || factor <= 0) {
    factor <- 5
  }
  testthat::expect_lt(
    unname(as.numeric(elapsed)),
    budget * factor,
    label = "elapsed seconds"
  )
}
