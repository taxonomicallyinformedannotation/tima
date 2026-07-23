test_that("validate_adduct_edges_by_intensity_covariance returns empty results for empty input", {
  edges <- tidytable::tidytable(
    feature_id = character(),
    adduct = character(),
    adduct_dest = character(),
    feature_id_dest = character()
  )
  features <- tidytable::tidytable(
    feature_id = character(),
    sample = character()
  )

  result <- validate_adduct_edges_by_intensity_covariance(
    adduct_edges = edges,
    features_table = features,
    min_correlation = 0.7
  )

  expect_equal(nrow(result$adduct_edges), 0L)
  expect_equal(nrow(result$rejected_edges), 0L)
})

test_that("validate_adduct_edges_by_intensity_covariance keeps highly correlated edges", {
  edges <- tidytable::tidytable(
    feature_id = "F1",
    adduct = "[M+H]+",
    adduct_dest = "[M+Na]+",
    feature_id_dest = "F2"
  )

  # Create features with identical intensities (perfect correlation = 1.0)
  features <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F1", "F2", "F2", "F2"),
    sample = c("S1", "S2", "S3", "S1", "S2", "S3"),
    intensity = c(100, 200, 300, 100, 200, 300)
  )

  result <- validate_adduct_edges_by_intensity_covariance(
    adduct_edges = edges,
    features_table = features,
    min_correlation = 0.7
  )

  expect_equal(nrow(result$adduct_edges), 1L)
  expect_equal(nrow(result$rejected_edges), 0L)
})

test_that("validate_adduct_edges_by_intensity_covariance rejects low-correlation edges", {
  edges <- tidytable::tidytable(
    feature_id = "F1",
    adduct = "[M+H]+",
    adduct_dest = "[M+Na]+",
    feature_id_dest = "F2"
  )

  # Create features with no correlation
  features <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F1", "F2", "F2", "F2"),
    sample = c("S1", "S2", "S3", "S1", "S2", "S3"),
    intensity = c(100, 200, 300, 300, 200, 100)
  )

  result <- validate_adduct_edges_by_intensity_covariance(
    adduct_edges = edges,
    features_table = features,
    min_correlation = 0.7
  )

  expect_equal(nrow(result$adduct_edges), 0L)
  expect_equal(nrow(result$rejected_edges), 1L)
  expect_true(stringi::stri_detect_fixed(
    result$rejected_edges$rejection_reason[1],
    "Low intensity co-variance"
  ))
})

test_that("validate_adduct_edges_by_intensity_covariance handles multiple edges", {
  edges <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F3"),
    adduct = c("[M+H]+", "[M+H]+", "[M+H]+"),
    adduct_dest = c("[M+Na]+", "[M+K]+", "[M+Na]+"),
    feature_id_dest = c("F2", "F2", "F4")
  )

  # F1-F2: perfect correlation (keep)
  # F1-F2: identical to F1-F2 (keep)
  # F3-F4: negative correlation (reject)
  features <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F1", "F2", "F2", "F2", "F3", "F3", "F3", "F4", "F4", "F4"),
    sample = c("S1", "S2", "S3", "S1", "S2", "S3", "S1", "S2", "S3", "S1", "S2", "S3"),
    intensity = c(100, 200, 300, 100, 200, 300, 100, 200, 300, 300, 200, 100)
  )

  result <- validate_adduct_edges_by_intensity_covariance(
    adduct_edges = edges,
    features_table = features,
    min_correlation = 0.7
  )

  expect_equal(nrow(result$adduct_edges), 2L)
  expect_equal(nrow(result$rejected_edges), 1L)
})

test_that("validate_adduct_edges_by_intensity_covariance handles missing values correctly", {
  edges <- tidytable::tidytable(
    feature_id = "F1",
    adduct = "[M+H]+",
    adduct_dest = "[M+Na]+",
    feature_id_dest = "F2"
  )

  # Create features with NAs in intensities
  features <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F1", "F2", "F2", "F2"),
    sample = c("S1", "S2", "S3", "S1", "S2", "S3"),
    intensity = c(100, NA, 300, 100, 200, 300)
  )

  result <- validate_adduct_edges_by_intensity_covariance(
    adduct_edges = edges,
    features_table = features,
    min_correlation = 0.7
  )

  # Should have sufficient data for correlation (2 valid pairs: (100, 100) and (300, 300))
  expect_equal(nrow(result$adduct_edges), 1L)
  expect_equal(nrow(result$rejected_edges), 0L)
})

test_that("validate_adduct_edges_by_intensity_covariance handles no intensity columns", {
  edges <- tidytable::tidytable(
    feature_id = "F1",
    adduct = "[M+H]+",
    adduct_dest = "[M+Na]+",
    feature_id_dest = "F2"
  )

  # Create features with no intensity columns
  features <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    sample = c("S1", "S1"),
    mz = c(100.5, 122.5),
    rt = c(5.0, 5.1)
  )

  result <- validate_adduct_edges_by_intensity_covariance(
    adduct_edges = edges,
    features_table = features,
    min_correlation = 0.7
  )

  # Should keep edges when no intensity data available
  expect_equal(nrow(result$adduct_edges), 1L)
  expect_equal(nrow(result$rejected_edges), 0L)
})

test_that("validate_adduct_edges_by_intensity_covariance uses custom threshold", {
  edges <- tidytable::tidytable(
    feature_id = "F1",
    adduct = "[M+H]+",
    adduct_dest = "[M+Na]+",
    feature_id_dest = "F2"
  )

  # Create features with moderate correlation (~0.7)
  features <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F1", "F1", "F2", "F2", "F2", "F2"),
    sample = c("S1", "S2", "S3", "S4", "S1", "S2", "S3", "S4"),
    intensity = c(100, 300, 200, 400, 200, 400, 100, 350)
  )

  # With threshold 0.8, should reject (cor ~ 0.703)
  result_strict <- validate_adduct_edges_by_intensity_covariance(
    adduct_edges = edges,
    features_table = features,
    min_correlation = 0.8
  )
  expect_equal(nrow(result_strict$adduct_edges), 0L)

  # With threshold 0.6, should keep
  result_lenient <- validate_adduct_edges_by_intensity_covariance(
    adduct_edges = edges,
    features_table = features,
    min_correlation = 0.6
  )
  expect_equal(nrow(result_lenient$adduct_edges), 1L)
})
