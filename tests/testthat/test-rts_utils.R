# Test Suite: normalize_rt_to_minutes ----

library(testthat)

test_that("normalize_rt_to_minutes leaves minute-scale values unchanged (auto)", {
  x <- c(0.5, 1.2, 10, 30, NA, 45)
  y <- normalize_rt_to_minutes(x, unit = "auto", quiet = TRUE)
  expect_equal(y[!is.na(y)], x[!is.na(x)])
})

test_that("normalize_rt_to_minutes converts seconds > heuristic thresholds", {
  secs <- c(350, 420, 500) # clearly seconds ( > 120 and > 180 95th percentile )
  mins <- normalize_rt_to_minutes(secs, unit = "auto", quiet = TRUE)
  expect_equal(mins, secs / 60)
})

test_that("normalize_rt_to_minutes respects explicit 'seconds' unit", {
  secs <- c(180, 240, 300)
  mins <- normalize_rt_to_minutes(secs, unit = "seconds", quiet = TRUE)
  expect_equal(mins, secs / 60)
})

test_that("normalize_rt_to_minutes respects explicit 'minutes' unit", {
  mins_in <- c(1, 5, 45)
  mins_out <- normalize_rt_to_minutes(mins_in, unit = "minutes", quiet = TRUE)
  expect_equal(mins_out, mins_in)
})
