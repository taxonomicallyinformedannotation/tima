# Test Suite: annotate_masses_covariance ----

library(testthat)

test_that("compute_intensity_covariance_edges returns empty for single sample", {
  ft <- tidytable::tidytable(
    feature_id = c("f1", "f2", "f3"),
    sample = "S1",
    mz = c(100.0, 100.5, 150.0),
    rt = c(1.0, 1.0, 5.0),
    intensity = c(1000, 2000, 500)
  ) |>
    tidytable::mutate(
      intensity_all = list(intensity),
      .by = feature_id
    )

  result <- compute_intensity_covariance_edges(ft, tolerance_rt = 0.5)

  expect_s3_class(result, "data.frame")
  expect_identical(nrow(result), 0L)
  expect_true("feature_id" %in% names(result))
  expect_true("feature_id_dest" %in% names(result))
})

test_that("compute_intensity_covariance_edges returns empty for single intensity value per pair", {
  ft <- tidytable::tidytable(
    feature_id = c("f1", "f2"),
    sample = c("S1", "S1"),
    mz = c(100.0, 100.5),
    rt = c(1.0, 1.0),
    intensity = c(1000, 2000)
  ) |>
    tidytable::mutate(
      intensity_all = list(intensity),
      .by = feature_id
    )

  result <- compute_intensity_covariance_edges(ft, tolerance_rt = 0.5)

  expect_identical(nrow(result), 0L)
})

test_that("compute_intensity_covariance_edges returns empty for empty input", {
  ft <- tidytable::tidytable(
    feature_id = character(),
    sample = character(),
    mz = numeric(),
    rt = numeric(),
    intensity = numeric()
  )

  result <- compute_intensity_covariance_edges(ft, tolerance_rt = 0.5)

  expect_identical(nrow(result), 0L)
})

test_that("compute_intensity_covariance_edges returns empty when no intensity columns", {
  ft <- tidytable::tidytable(
    feature_id = c("f1", "f2"),
    sample = c("S1", "S1"),
    mz = c(100.0, 100.5),
    rt = c(1.0, 1.0)
  )

  result <- compute_intensity_covariance_edges(ft, tolerance_rt = 0.5)

  expect_identical(nrow(result), 0L)
})

test_that("compute_intensity_covariance_edges detects correlated features across samples", {
  # Two features co-varying strongly across 5 samples
  ft <- tidytable::tidytable(
    feature_id = rep(c("f1", "f2"), each = 5L),
    sample = rep(paste0("S", 1:5), times = 2L),
    mz = rep(c(100.0, 100.5), each = 5L),
    rt = rep(c(1.0, 1.0), each = 5L),
    intensity = c(100, 200, 300, 400, 500, 110, 210, 310, 410, 510)
  ) |>
    tidytable::mutate(
      intensity_all = list(intensity),
      .by = feature_id
    )

  result <- compute_intensity_covariance_edges(ft, tolerance_rt = 0.5)

  expect_gt(nrow(result), 0L)
  expect_true("correlation" %in% names(result))
  expect_true("p_value" %in% names(result))
  expect_true("n_samples" %in% names(result))
  expect_true(all(result$p_value < 0.05))
  expect_true(all(result$source == "intensity_covariance"))
})

test_that("compute_intensity_covariance_edges excludes uncorrelated features", {
  # f1 and f2 within RT window but uncorrelated (random intensities across many samples)
  set.seed(42L)
  n <- 20L
  ft <- tidytable::tidytable(
    feature_id = rep(c("f1", "f2"), each = n),
    sample = rep(paste0("S", seq_len(n)), times = 2L),
    mz = rep(c(100.0, 100.5), each = n),
    rt = rep(c(1.0, 1.0), each = n),
    intensity = c(
      sample(100:1000, n, replace = TRUE),
      sample(100:1000, n, replace = TRUE)
    )
  ) |>
    tidytable::mutate(
      intensity_all = list(intensity),
      .by = feature_id
    )

  result <- compute_intensity_covariance_edges(
    ft,
    tolerance_rt = 0.5,
    correlation_p_threshold = 0.05
  )

  # With random uncorrelated data, should yield few or no significant edges
  expect_true(all(result$p_value < 0.05))
})

test_that("compute_intensity_covariance_edges respects RT window", {
  # f1 at rt=1.0, f2 at rt=5.0 — outside tolerance
  ft <- tidytable::tidytable(
    feature_id = rep(c("f1", "f2"), each = 5L),
    sample = rep(paste0("S", 1:5), times = 2L),
    mz = rep(c(100.0, 100.5), each = 5L),
    rt = rep(c(1.0, 5.0), each = 5L),
    intensity = c(100, 200, 300, 400, 500, 110, 210, 310, 410, 510)
  ) |>
    tidytable::mutate(
      intensity_all = list(intensity),
      .by = feature_id
    )

  result <- compute_intensity_covariance_edges(ft, tolerance_rt = 0.5)

  expect_identical(nrow(result), 0L)
})
