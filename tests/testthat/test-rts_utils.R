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

test_that("normalize_rt_to_minutes returns NULL unchanged", {
  expect_null(normalize_rt_to_minutes(NULL, quiet = TRUE))
})

test_that("normalize_rt_to_minutes returns empty numeric unchanged", {
  out <- normalize_rt_to_minutes(numeric(0), quiet = TRUE)
  expect_equal(length(out), 0L)
})

test_that("normalize_rt_to_minutes returns all-NA unchanged", {
  out <- normalize_rt_to_minutes(c(NA_real_, NA_real_), quiet = TRUE)
  expect_true(all(is.na(out)))
})

test_that("normalize_rt_to_minutes coerces character to numeric", {
  out <- normalize_rt_to_minutes(
    c("120", "240"),
    unit = "seconds",
    quiet = TRUE
  )
  expect_equal(out, c(2, 4))
})

test_that("normalize_rt_to_minutes errors on non-numeric non-character", {
  expect_error(normalize_rt_to_minutes(list(1, 2), quiet = TRUE), "numeric")
})

test_that("normalize_rt_to_minutes: min > 2 triggers seconds detection", {
  # All values > 2 → should be treated as seconds
  x <- c(3, 5, 10, 15, 20)
  out <- normalize_rt_to_minutes(x, unit = "auto", quiet = TRUE)
  expect_equal(out, x / 60)
})

test_that("normalize_rt_to_minutes: max > 180 triggers seconds detection", {
  # max = 200 > 180 → should be treated as seconds
  x <- c(50, 100, 200)
  out <- normalize_rt_to_minutes(x, unit = "auto", quiet = TRUE)
  expect_equal(out, x / 60)
})
