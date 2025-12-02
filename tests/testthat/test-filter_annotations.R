# Test Suite: filter_annotations (mother-function only) ----

library(testthat)

# Minimal fixtures for mother-function tests only

.make_ann <- function(path) {
  write.table(
    data.frame(
      feature_id = c("F1", "F2"),
      candidate_library = c("ms1", "spectral"),
      candidate_structure_inchikey_connectivity_layer = c("A", "B"),
      rt = c("1.0", "2.0"),
      candidate_score_similarity = c(NA, 0.8)
    ),
    path,
    sep = "\t",
    row.names = FALSE
  )
  path
}

.make_feat <- function(path) {
  write.table(
    data.frame(
      feature_id = c("F1", "F2"),
      rt = c(1.0, 2.0),
      mz = c(100.05, 150.08)
    ),
    path,
    sep = "\t",
    row.names = FALSE
  )
  path
}

.make_rt <- function(path) {
  write.table(
    data.frame(
      candidate_structure_inchikey_connectivity_layer = c("A", "B"),
      rt_target = c("1.02", "2.05")
    ),
    path,
    sep = "\t",
    row.names = FALSE
  )
  path
}

# Validation through mother function

test_that("filter_annotations() errors on missing annotations file", {
  feat <- tempfile(fileext = ".tsv")
  out <- tempfile(fileext = ".tsv")
  .make_feat(feat)
  expect_error(
    filter_annotations(
      annotations = "nope.tsv",
      features = feat,
      rts = character(0),
      output = out
    ),
    "Annotation file"
  )
})

test_that("filter_annotations() errors on missing features file", {
  ann <- tempfile(fileext = ".tsv")
  .make_ann(ann)
  out <- tempfile(fileext = ".tsv")
  expect_error(
    filter_annotations(
      annotations = ann,
      features = "nope.tsv",
      rts = character(0),
      output = out
    ),
    "Features file"
  )
})

test_that("filter_annotations() runs end-to-end without RT library", {
  ann <- tempfile(fileext = ".tsv")
  .make_ann(ann)
  feat <- tempfile(fileext = ".tsv")
  .make_feat(feat)
  out <- tempfile(fileext = ".tsv")
  res <- filter_annotations(
    annotations = ann,
    features = feat,
    rts = character(0),
    output = out,
    tolerance_rt = 0.5
  )
  expect_true(file.exists(res))
})

# test_that("filter_annotations() runs with RT library and multiple annotation files", {
#   ann1 <- tempfile(fileext = ".tsv")
#   .make_ann(ann1)
#   ann2 <- tempfile(fileext = ".tsv")
#   .make_ann(ann2)
#   feat <- tempfile(fileext = ".tsv")
#   .make_feat(feat)
#   rts <- tempfile(fileext = ".tsv")
#   .make_rt(rts)
#   out <- tempfile(fileext = ".tsv")
#   res <- filter_annotations(
#     annotations = c(ann1, ann2),
#     features = feat,
#     rts = rts,
#     output = out,
#     tolerance_rt = 0.1
#   )
#   expect_true(file.exists(res))
# })

test_that("filter_annotations() errors on invalid tolerance_rt", {
  ann <- tempfile(fileext = ".tsv")
  .make_ann(ann)
  feat <- tempfile(fileext = ".tsv")
  .make_feat(feat)
  out <- tempfile(fileext = ".tsv")
  expect_error(
    filter_annotations(
      annotations = ann,
      features = feat,
      rts = character(0),
      output = out,
      tolerance_rt = -1
    ),
    "tolerance_rt must be"
  )
})

## Internal Helper Tests ----

test_that("validate_filter_annotations_inputs validates tolerance_rt", {
  ann <- tempfile(fileext = ".tsv")
  .make_ann(ann)
  feat <- tempfile(fileext = ".tsv")
  .make_feat(feat)

  expect_error(
    validate_filter_annotations_inputs(
      annotations = ann,
      features = feat,
      rts = character(0),
      output = tempfile(),
      tolerance_rt = 0
    ),
    "tolerance_rt must be a positive number"
  )

  expect_error(
    validate_filter_annotations_inputs(
      annotations = ann,
      features = feat,
      rts = character(0),
      output = tempfile(),
      tolerance_rt = -5
    ),
    "tolerance_rt must be a positive number"
  )

  expect_error(
    validate_filter_annotations_inputs(
      annotations = ann,
      features = feat,
      rts = character(0),
      output = tempfile(),
      tolerance_rt = "invalid"
    ),
    "tolerance_rt must be a positive number"
  )
})

test_that("validate_filter_annotations_inputs validates output path", {
  ann <- tempfile(fileext = ".tsv")
  .make_ann(ann)
  feat <- tempfile(fileext = ".tsv")
  .make_feat(feat)

  expect_error(
    validate_filter_annotations_inputs(
      annotations = ann,
      features = feat,
      rts = character(0),
      output = c("path1", "path2"),
      tolerance_rt = 0.5
    ),
    "output must be a single character string"
  )

  expect_error(
    validate_filter_annotations_inputs(
      annotations = ann,
      features = feat,
      rts = character(0),
      output = 123,
      tolerance_rt = 0.5
    ),
    "output must be a single character string"
  )
})

test_that("validate_filter_annotations_inputs validates features", {
  ann <- tempfile(fileext = ".tsv")
  .make_ann(ann)

  expect_error(
    validate_filter_annotations_inputs(
      annotations = ann,
      features = "nonexistent.tsv",
      rts = character(0),
      output = tempfile(),
      tolerance_rt = 0.5
    ),
    "Features file not found"
  )

  expect_error(
    validate_filter_annotations_inputs(
      annotations = ann,
      features = c("feat1", "feat2"),
      rts = character(0),
      output = tempfile(),
      tolerance_rt = 0.5
    ),
    "features must be a single character string"
  )
})

test_that("validate_filter_annotations_inputs validates annotations list", {
  feat <- tempfile(fileext = ".tsv")
  .make_feat(feat)

  expect_error(
    validate_filter_annotations_inputs(
      annotations = list(),
      features = feat,
      rts = character(0),
      output = tempfile(),
      tolerance_rt = 0.5
    ),
    "annotations must be a non-empty"
  )
})

test_that("validate_filter_annotations_inputs accepts valid character vector annotations", {
  ann <- tempfile(fileext = ".tsv")
  .make_ann(ann)
  feat <- tempfile(fileext = ".tsv")
  .make_feat(feat)

  expect_silent(
    validate_filter_annotations_inputs(
      annotations = ann,
      features = feat,
      rts = character(0),
      output = tempfile(),
      tolerance_rt = 0.5
    )
  )
})

test_that("filter_ms1_redundancy filters correctly", {
  skip_if_not_installed("tidytable")

  ms1_data <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B", "C"),
    candidate_library = c("ms1", "ms1", "ms1"),
    score = c(0.5, 0.6, 0.7)
  )

  spectral_data <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B"),
    candidate_library = c("spectral", "spectral"),
    score = c(0.8, 0.9)
  )

  ann_list <- list(
    ms1 = ms1_data,
    spectral = spectral_data
  )

  result <- filter_ms1_redundancy(ann_list)

  # F1 and F2 should be removed from MS1 (superseded by spectral)
  # Only F3 from MS1 should remain, plus F1 and F2 from spectral
  expect_equal(nrow(result), 3)

  # Check that spectral annotations are present
  spectral_results <- result |>
    tidytable::filter(candidate_library == "spectral")
  expect_equal(nrow(spectral_results), 2)

  # Check that only non-redundant MS1 annotation remains
  ms1_results <- result |>
    tidytable::filter(candidate_library == "ms1")
  expect_equal(nrow(ms1_results), 1)
  expect_true("F3" %in% ms1_results$feature_id)
})

test_that("filter_ms1_redundancy handles no MS1 annotations", {
  skip_if_not_installed("tidytable")

  spectral_data <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B"),
    candidate_library = c("spectral", "spectral")
  )

  ann_list <- list(spectral = spectral_data)

  result <- filter_ms1_redundancy(ann_list)
  expect_equal(nrow(result), 2)
  expect_true(all(result$candidate_library == "spectral"))
})

# test_that("filter_ms1_redundancy handles only MS1 annotations", {
#   skip_if_not_installed("tidytable")
#
#   ms1_data <- tidytable::tidytable(
#     feature_id = c("F1", "F2"),
#     candidate_structure_inchikey_connectivity_layer = c("A", "B"),
#     candidate_library = c("ms1", "ms1")
#   )
#
#   ann_list <- list(ms1 = ms1_data)
#
#   result <- filter_ms1_redundancy(ann_list)
#   expect_equal(nrow(result), 2)
#   expect_true(all(result$candidate_library == "ms1"))
# })

test_that("apply_rt_filter filters annotations by RT tolerance", {
  skip_if_not_installed("tidytable")

  features_ann <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B", "C"),
    rt = c("1.0", "2.0", "3.0")
  )

  rt_lib <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = c("A", "B", "C"),
    rt_target = c("1.02", "2.5", "3.01")
  )

  result <- apply_rt_filter(features_ann, rt_lib, tolerance_rt = 0.1)

  # F1 (diff=0.02) and F3 (diff=0.01) should pass, F2 (diff=0.5) should fail
  expect_equal(nrow(result), 2)
  expect_true("F1" %in% result$feature_id)
  expect_true("F3" %in% result$feature_id)
  expect_false("F2" %in% result$feature_id)
})

test_that("apply_rt_filter keeps annotations without RT target", {
  skip_if_not_installed("tidytable")

  features_ann <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B"),
    rt = c("1.0", "2.0")
  )

  rt_lib <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = c("A"),
    rt_target = c("1.02")
  )

  result <- apply_rt_filter(features_ann, rt_lib, tolerance_rt = 0.1)

  # F1 should pass (within tolerance), F2 should also pass (no RT target)
  expect_equal(nrow(result), 2)
})

test_that("filter_annotations handles multiple annotation files", {
  skip_if_not_installed("tidytable")

  ann1 <- tempfile(fileext = ".tsv")
  ann2 <- tempfile(fileext = ".tsv")
  .make_ann(ann1)
  .make_ann(ann2)
  feat <- tempfile(fileext = ".tsv")
  .make_feat(feat)
  out <- tempfile(fileext = ".tsv")

  result <- filter_annotations(
    annotations = c(ann1, ann2),
    features = feat,
    rts = character(0),
    output = out,
    tolerance_rt = 0.5
  )

  expect_true(file.exists(result))
})

# test_that("filter_annotations handles RT library filtering", {
#   skip_if_not_installed("tidytable")
#
#   ann <- tempfile(fileext = ".tsv")
#   .make_ann(ann)
#   feat <- tempfile(fileext = ".tsv")
#   .make_feat(feat)
#   rts <- tempfile(fileext = ".tsv")
#   .make_rt(rts)
#   out <- tempfile(fileext = ".tsv")
#
#   result <- filter_annotations(
#     annotations = ann,
#     features = feat,
#     rts = rts,
#     output = out,
#     tolerance_rt = 0.1
#   )
#
#   expect_true(file.exists(result))
#
#   # Read result and check RT filtering was applied
#   result_data <- tidytable::fread(result)
#   expect_true(nrow(result_data) >= 0)
# })
