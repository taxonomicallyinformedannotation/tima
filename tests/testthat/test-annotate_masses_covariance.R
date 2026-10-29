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

test_that("compute_intensity_covariance_edges excludes negative correlation", {
  ft <- tidytable::tidytable(
    feature_id = rep(c("f1", "f2"), each = 5L),
    sample = rep(paste0("S", 1:5), times = 2L),
    mz = rep(c(100.0, 100.5), each = 5L),
    rt = rep(c(1.0, 1.0), each = 5L),
    intensity = c(10, 20, 30, 40, 50, 50, 40, 30, 20, 10)
  ) |>
    tidytable::mutate(
      intensity_all = list(intensity),
      .by = feature_id
    )

  result <- compute_intensity_covariance_edges(ft, tolerance_rt = 0.5)

  expect_identical(nrow(result), 0L)
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

.covariance_table <- function(intensities, rt = 1.0) {
  # intensities: named list feature_id -> numeric vector across samples
  ids <- names(intensities)
  tidytable::tidytable(
    feature_id = ids,
    sample = "S1",
    mz = seq_along(ids) + 100,
    rt = rt,
    intensity_all = unname(intensities)
  )
}

.reference_covariance <- function(intensities, threshold) {
  # Independent oracle: all pairs, pairwise-complete positive values, BH
  ids <- names(intensities)
  pairs <- utils::combn(ids, 2L, simplify = FALSE)
  rows <- lapply(pairs, function(p) {
    x <- intensities[[p[[1L]]]]
    y <- intensities[[p[[2L]]]]
    ok <- !is.na(x) & !is.na(y) & x > 0 & y > 0
    ct <- stats::cor.test(x[ok], y[ok])
    data.frame(
      key = paste(sort(p), collapse = "-"),
      r = unname(ct$estimate),
      p_raw = ct$p.value
    )
  })
  ref <- do.call(rbind, rows)
  ref$p_adj <- stats::p.adjust(ref$p_raw, method = "BH")
  ref[ref$r > 0 & ref$p_adj < threshold, ]
}

test_that("covariance edges control the false discovery rate with BH", {
  set.seed(1L)
  n <- 8L
  base <- seq(100, 800, length.out = n)
  intensities <- c(
    # Strong pair: survives any reasonable correction
    list(a = base, b = base * 1.1 + 5),
    # Weak pair: raw p just under 0.05 for n = 8 (r ~ 0.72)
    list(c = c(1, 2, 3, 4, 5, 6, 7, 8) * 100),
    list(d = c(3, 1, 4, 2, 7, 5, 8, 6) * 100),
    # Null features
    stats::setNames(
      lapply(1:8, function(i) stats::runif(n, 100, 1000)),
      paste0("n", 1:8)
    )
  )
  raw_cd <- stats::cor.test(intensities$c, intensities$d)$p.value
  expect_lt(raw_cd, 0.05)

  result <- compute_intensity_covariance_edges(
    .covariance_table(intensities),
    tolerance_rt = 0.5,
    correlation_p_threshold = 0.05
  )
  ref <- .reference_covariance(intensities, threshold = 0.05)
  keys <- vapply(
    seq_len(nrow(result)),
    function(i) {
      paste(
        sort(c(result$feature_id[[i]], result$feature_id_dest[[i]])),
        collapse = "-"
      )
    },
    character(1L)
  )

  expect_setequal(keys, ref$key)
  expect_true("a-b" %in% keys)
  # Raw p < 0.05 is no longer enough once 45 pairs are tested
  expect_false("c-d" %in% keys)
  expect_equal(
    result$p_value[match(ref$key, keys)],
    ref$p_adj,
    tolerance = 1e-12
  )
  expect_true(all(result$p_value >= result$p_value_raw))
})

test_that("covariance BH adjustment is a no-op for a single tested pair", {
  intensities <- list(a = c(1, 2, 3, 4, 5), b = c(1.1, 2.3, 2.9, 4.2, 5))
  result <- compute_intensity_covariance_edges(
    .covariance_table(intensities),
    tolerance_rt = 0.5
  )
  expect_identical(nrow(result), 1L)
  expect_equal(result$p_value, result$p_value_raw)
})

test_that("covariance keeps samples aligned when intensities are missing", {
  # Missing values must drop the sample from both vectors, not shift one
  x <- c(10, NA, 30, 40, 50, 60)
  y <- c(12, 999, 31, 43, 49, 62)
  result <- compute_intensity_covariance_edges(
    .covariance_table(list(a = x, b = y)),
    tolerance_rt = 0.5
  )
  ok <- !is.na(x)
  expect_identical(nrow(result), 1L)
  expect_identical(result$n_samples, 5L)
  expect_equal(result$correlation, stats::cor(x[ok], y[ok]))

  # Pipe-separated strings (as read back from CSV) behave the same
  as_text <- .covariance_table(list(
    a = paste(x, collapse = "|"),
    b = paste(y, collapse = "|")
  ))
  as_text$intensity_all <- unlist(as_text$intensity_all)
  from_text <- compute_intensity_covariance_edges(as_text, tolerance_rt = 0.5)
  expect_equal(from_text$correlation, result$correlation)
})

test_that("covariance skips pairs measured on different sample sets", {
  result <- compute_intensity_covariance_edges(
    .covariance_table(list(a = c(1, 2, 3, 4), b = c(1, 2, 3))),
    tolerance_rt = 0.5
  )
  expect_identical(nrow(result), 0L)
  expect_named(
    result,
    c(
      "feature_id",
      "feature_id_dest",
      "correlation",
      "p_value",
      "p_value_raw",
      "n_samples",
      "source"
    )
  )
})
