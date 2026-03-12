# Test Suite: round_reals ----

library(testthat)

test_that("round_reals rounds named SOP columns to 5 dp by default", {
  df <- data.frame(
    structure_exact_mass = 123.456789,
    structure_xlogp = 2.3456789,
    unrelated = 9.9999999
  )
  out <- round_reals(df)
  expect_equal(out$structure_exact_mass, 123.45679)
  expect_equal(out$structure_xlogp, 2.34568)
  expect_equal(out$unrelated, 9.9999999)
})

test_that("round_reals respects dig argument", {
  df <- data.frame(structure_exact_mass = 123.456789)
  expect_equal(round_reals(df, dig = 2)$structure_exact_mass, 123.46)
  expect_equal(round_reals(df, dig = 0)$structure_exact_mass, 123)
})

test_that("round_reals leaves NA and Inf intact", {
  df <- data.frame(structure_exact_mass = c(1.23456, NA_real_, Inf, -Inf))
  out <- round_reals(df)
  expect_equal(out$structure_exact_mass[1], 1.23456)
  expect_true(is.na(out$structure_exact_mass[2]))
  expect_equal(out$structure_exact_mass[3], Inf)
  expect_equal(out$structure_exact_mass[4], -Inf)
})

test_that("round_reals uses custom column pattern", {
  df <- data.frame(my_mass = 9.876543, other = 9.876543)
  out <- round_reals(df, cols = "my_mass")
  expect_equal(out$my_mass, 9.87654)
  expect_equal(out$other, 9.876543)
})

test_that("round_reals returns df unchanged when no columns match", {
  df <- data.frame(foo = 1.23456789)
  expect_equal(round_reals(df), df)
})

test_that("round_reals rejects invalid dig", {
  df <- data.frame(structure_exact_mass = 1.0)
  expect_error(round_reals(df, dig = -1), "non-negative")
  expect_error(round_reals(df, dig = "5"), "non-negative integer")
  expect_error(round_reals(df, dig = 2.5), "non-negative integer")
})

test_that("round_reals rejects non-data-frame input", {
  expect_error(round_reals("x"), "data frame")
})
