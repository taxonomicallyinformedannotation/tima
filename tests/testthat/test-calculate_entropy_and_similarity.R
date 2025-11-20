# Test Suite: calculate_entropy_and_similarity ----

library(testthat)

test_that("calculate_entropy_and_similarity works with small spectra", {
  # Small test spectra
  x_small <- cbind(
    mz = c(10, 36, 63, 91, 93),
    intensity = c(14, 15, 999, 650, 1)
  ) |>
    list()

  y_small <- cbind(
    mz = c(10, 12, 50, 63, 105),
    intensity = c(35, 5, 16, 999, 450)
  ) |>
    list()

  pmz_x <- 91.0
  pmz_y <- 105.0

  # Cosine similarity with approximation
  result1 <- calculate_entropy_and_similarity(
    lib_ids = 1L,
    lib_precursors = pmz_y,
    lib_spectra = y_small,
    query_ids = 1L,
    query_precursors = pmz_x,
    query_spectra = x_small,
    dalton = 0.01,
    ppm = 1.0,
    threshold = 0.0,
    method = "cosine",
    approx = TRUE
  )
  expect_type(result1, "list")

  # GNPS method without approximation
  result2 <- calculate_entropy_and_similarity(
    lib_ids = 1L,
    lib_precursors = pmz_y,
    lib_spectra = y_small,
    query_ids = 1L,
    query_precursors = pmz_x,
    query_spectra = x_small,
    dalton = 0.01,
    ppm = 1.0,
    threshold = 0.0,
    method = "gnps",
    approx = FALSE
  )
  expect_type(result2, "list")
})

test_that("calculate_entropy_and_similarity works with large spectra", {
  set.seed(123)
  n_query_large <- 1000
  n_lib_large <- 1000

  x_large <- cbind(
    mz = sort(runif(n_query_large, 100, 1000)),
    intensity = runif(n_query_large, 1, 1000)
  ) |>
    list()

  y_large <- cbind(
    mz = sort(runif(n_lib_large, 100, 1000)),
    intensity = runif(n_lib_large, 1, 1000)
  ) |>
    list()

  pmz_x <- 91.0
  pmz_y <- 105.0

  # Entropy method with approximation
  result <- calculate_entropy_and_similarity(
    lib_ids = 1L,
    lib_precursors = pmz_y,
    lib_spectra = y_large,
    query_ids = 1L,
    query_precursors = pmz_x,
    query_spectra = x_large,
    dalton = 0.01,
    ppm = 1.0,
    threshold = 0.0,
    method = "entropy",
    approx = TRUE
  )
  expect_type(result, "list")
})

test_that("calculate_entropy_and_similarity fails with invalid method", {
  x_small <- cbind(mz = c(10, 36, 63), intensity = c(14, 15, 999)) |> list()
  y_small <- cbind(mz = c(10, 12, 50), intensity = c(35, 5, 16)) |> list()

  expect_error(
    calculate_entropy_and_similarity(
      lib_ids = 1L,
      lib_precursors = 105.0,
      lib_spectra = y_small,
      query_ids = 1L,
      query_precursors = 91.0,
      query_spectra = x_small,
      dalton = 0.01,
      ppm = 1.0,
      threshold = 0.0,
      method = "foo",
      approx = TRUE
    )
  )
})
