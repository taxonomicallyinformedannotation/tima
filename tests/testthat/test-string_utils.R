# Test Suite: string_utils ----

library(testthat)

test_that("stri_detect_token matches whole values, not substrings", {
  expect_false(stri_detect_token(str = "Arabidopsis", pattern = "Ara"))
  expect_true(stri_detect_token(str = "Arabidopsis", pattern = "Arabidopsis"))
})

test_that("stri_detect_token matches against collapsed values", {
  expect_equal(
    stri_detect_token(
      str = c("Gentiana $ Arabidopsis", "Gentiana $ Arabidopsis"),
      pattern = c("Arabidopsis", "Ara")
    ),
    c(TRUE, FALSE)
  )
})

test_that("stri_detect_token takes regex metacharacters literally", {
  expect_true(stri_detect_token(str = "a (+) b", pattern = "a (+) b"))
  expect_false(stri_detect_token(str = "ab", pattern = "a."))
  expect_false(stri_detect_token(str = NA_character_, pattern = "a"))
})
