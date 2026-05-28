# Test Suite: annotate_masses_features ----
# Tests for feature table preparation helpers in annotate_masses_features.R

library(testthat)

# ── prepare_features_table ────────────────────────────────────────────────────

test_that("prepare_features_table returns a tidytable with required columns", {
  features <- tidytable::tidytable(
    feature_id = c("f1", "f2"),
    mz = c(100.0, 200.0),
    rt = c(1.0, 2.0),
    sample = c("s1", "s1")
  )
  result <- prepare_features_table(features, tolerance_rt = 0.05)
  expect_s3_class(result, "data.frame")
  expect_true("rt_min" %in% colnames(result))
  expect_true("rt_max" %in% colnames(result))
  expect_equal(result$rt_min, c(0.95, 1.95))
  expect_equal(result$rt_max, c(1.05, 2.05))
})

test_that("prepare_features_table adds missing 'adduct' column as NA", {
  features <- tidytable::tidytable(
    feature_id = c("f1"),
    mz = c(100.0),
    rt = c(1.0)
  )
  result <- prepare_features_table(features, tolerance_rt = 0.1)
  expect_true("adduct" %in% colnames(result))
  expect_true(is.na(result$adduct[[1L]]))
})

test_that("prepare_features_table adds 'sample' column when missing", {
  features <- tidytable::tidytable(
    feature_id = c("f1"),
    mz = c(100.0),
    rt = c(2.0)
  )
  result <- prepare_features_table(features, tolerance_rt = 0.05)
  expect_true("sample" %in% colnames(result))
  expect_equal(result$sample[[1L]], "all")
})

test_that("prepare_features_table deduplicates by feature_id and sample", {
  features <- tidytable::tidytable(
    feature_id = c("f1", "f1", "f2"),
    mz = c(100.0, 100.1, 200.0),
    rt = c(1.0, 1.0, 2.0),
    sample = c("s1", "s1", "s1")
  )
  result <- prepare_features_table(features, tolerance_rt = 0.05)
  expect_equal(nrow(result), 2L)
})

test_that("prepare_features_table uses candidate_adduct when adduct is NA", {
  features <- tidytable::tidytable(
    feature_id = c("f1"),
    mz = c(100.0),
    rt = c(1.0),
    adduct = NA_character_,
    candidate_adduct = "[M+H]+"
  )
  result <- prepare_features_table(features, tolerance_rt = 0.05)
  # After harmonize_adducts, any valid translation should be applied
  expect_false(is.na(result$adduct[[1L]]))
})

test_that("prepare_features_table adds sequential rt proxy when rt is missing", {
  features <- tidytable::tidytable(
    feature_id = c("f1", "f2"),
    mz = c(100.0, 200.0)
  )
  result <- suppressWarnings(prepare_features_table(
    features,
    tolerance_rt = 0.05
  ))
  expect_true("rt" %in% colnames(result))
  expect_true(all(is.finite(result$rt)))
})

# ── extract_preassigned_adducts ───────────────────────────────────────────────

test_that("extract_preassigned_adducts returns tidytable with feature_id and adduct", {
  features <- tidytable::tidytable(
    feature_id = c("f1", "f2"),
    adduct = c("[M+H]+", "[M-H]-")
  )
  result <- extract_preassigned_adducts(features)
  expect_s3_class(result, "data.frame")
  expect_true("feature_id" %in% colnames(result))
  expect_true("adduct" %in% colnames(result))
  expect_equal(nrow(result), 2L)
})

test_that("extract_preassigned_adducts returns empty table when no adduct column", {
  features <- tidytable::tidytable(
    feature_id = c("f1", "f2"),
    mz = c(100.0, 200.0)
  )
  result <- extract_preassigned_adducts(features)
  expect_equal(nrow(result), 0L)
  expect_true("feature_id" %in% colnames(result))
  expect_true("adduct" %in% colnames(result))
})

test_that("extract_preassigned_adducts filters out NA adducts", {
  features <- tidytable::tidytable(
    feature_id = c("f1", "f2"),
    adduct = c("[M+H]+", NA_character_)
  )
  result <- extract_preassigned_adducts(features)
  expect_equal(nrow(result), 1L)
  expect_equal(result$feature_id, "f1")
})

test_that("extract_preassigned_adducts expands pipe-separated adducts", {
  features <- tidytable::tidytable(
    feature_id = c("f1"),
    adduct = c("[M+H]+|[M+Na]+")
  )
  result <- extract_preassigned_adducts(features)
  expect_gte(nrow(result), 2L)
  expect_true("f1" %in% result$feature_id)
})

test_that("extract_preassigned_adducts uses candidate_adduct as fallback", {
  features <- tidytable::tidytable(
    feature_id = c("f1"),
    adduct = NA_character_,
    candidate_adduct = "[M+H]+"
  )
  result <- extract_preassigned_adducts(features)
  expect_equal(nrow(result), 1L)
  expect_equal(result$feature_id, "f1")
})

# ── propagate_preassigned_over_adduct_edges ───────────────────────────────────

test_that("propagate_preassigned_over_adduct_edges returns empty when edges empty", {
  adduct_edges <- tidytable::tidytable(
    feature_id = character(),
    adduct = character(),
    feature_id_dest = character(),
    adduct_dest = character()
  )
  preassigned <- tidytable::tidytable(
    feature_id = c("f1"),
    adduct = c("[M+H]+")
  )
  result <- propagate_preassigned_over_adduct_edges(adduct_edges, preassigned)
  expect_equal(nrow(result), 0L)
})

test_that("propagate_preassigned_over_adduct_edges returns empty when preassigned empty", {
  adduct_edges <- tidytable::tidytable(
    feature_id = c("f1"),
    adduct = c("[M+H]+"),
    feature_id_dest = c("f2"),
    adduct_dest = c("[M+Na]+")
  )
  preassigned <- tidytable::tidytable(
    feature_id = character(),
    adduct = character()
  )
  result <- propagate_preassigned_over_adduct_edges(adduct_edges, preassigned)
  expect_equal(nrow(result), 0L)
})

test_that("propagate_preassigned_over_adduct_edges propagates forward and backward", {
  adduct_edges <- tidytable::tidytable(
    feature_id = c("f1"),
    adduct = c("[M+H]+"),
    feature_id_dest = c("f2"),
    adduct_dest = c("[M+Na]+")
  )
  preassigned <- tidytable::tidytable(
    feature_id = c("f1"),
    adduct = c("[M+H]+")
  )
  result <- propagate_preassigned_over_adduct_edges(adduct_edges, preassigned)
  expect_true(nrow(result) >= 1L)
  # The dest feature f2 should receive the propagated adduct_dest
  expect_true("f2" %in% result$feature_id)
})
