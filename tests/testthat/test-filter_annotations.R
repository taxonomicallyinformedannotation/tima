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
    "annotation file"
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
    "features file"
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

test_that("filter_annotations() runs when features table has no rt column", {
  ann <- tempfile(fileext = ".tsv")
  .make_ann(ann)
  feat <- tempfile(fileext = ".tsv")
  write.table(
    data.frame(
      feature_id = c("F1", "F2"),
      mz = c(100.05, 150.08)
    ),
    feat,
    sep = "\t",
    row.names = FALSE
  )
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

test_that("filter_annotations() skips RT filtering when features lack rt column", {
  ann <- tempfile(fileext = ".tsv")
  .make_ann(ann)
  feat <- tempfile(fileext = ".tsv")
  write.table(
    data.frame(
      feature_id = c("F1", "F2"),
      mz = c(100.05, 150.08)
    ),
    feat,
    sep = "\t",
    row.names = FALSE
  )
  rts <- tempfile(fileext = ".tsv")
  .make_rt(rts)
  out <- tempfile(fileext = ".tsv")
  # Should succeed (log_warn is issued but doesn't raise R warning)
  res <- filter_annotations(
    annotations = ann,
    features = feat,
    rts = rts,
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

test_that("filter_annotations() errors on invalid tolerance_rt with RT library", {
  ann <- tempfile(fileext = ".tsv")
  .make_ann(ann)
  feat <- tempfile(fileext = ".tsv")
  .make_feat(feat)
  rts <- tempfile(fileext = ".tsv")
  .make_rt(rts)
  out <- tempfile(fileext = ".tsv")
  expect_error(
    filter_annotations(
      annotations = ann,
      features = feat,
      rts = rts,
      output = out,
      tolerance_rt = -1
    ),
    "tolerance_rt must be"
  )
})

## Internal Helper Tests ----

test_that("validate_filter_annotations_inputs validates tolerance_rt with RT library", {
  ann <- tempfile(fileext = ".tsv")
  .make_ann(ann)
  feat <- tempfile(fileext = ".tsv")
  .make_feat(feat)
  rts <- tempfile(fileext = ".tsv")
  .make_rt(rts)

  expect_error(
    validate_filter_annotations_inputs(
      annotations = ann,
      features = feat,
      rts = rts,
      output = tempfile(),
      tolerance_rt = 0
    ),
    "tolerance_rt must be a positive number"
  )

  expect_error(
    validate_filter_annotations_inputs(
      annotations = ann,
      features = feat,
      rts = rts,
      output = tempfile(),
      tolerance_rt = -5
    ),
    "tolerance_rt must be a positive number"
  )

  expect_error(
    validate_filter_annotations_inputs(
      annotations = ann,
      features = feat,
      rts = rts,
      output = tempfile(),
      tolerance_rt = "invalid"
    ),
    "tolerance_rt must be a positive number"
  )
})

test_that("validate_filter_annotations_inputs allows invalid tolerance_rt without RT library", {
  ann <- tempfile(fileext = ".tsv")
  .make_ann(ann)
  feat <- tempfile(fileext = ".tsv")
  .make_feat(feat)

  # Should NOT error when rts is empty, even with bad tolerance_rt
  expect_silent(
    validate_filter_annotations_inputs(
      annotations = ann,
      features = feat,
      rts = character(0),
      output = tempfile(),
      tolerance_rt = -5
    )
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
    "features file not found"
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

test_that("filter_ms1_redundancy filters correctly with quality gate", {
  skip_if_not_installed("tidytable")

  ms1_data <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3", "F4"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B", "C", "D"),
    candidate_library = c("ms1", "ms1", "ms1", "ms1"),
    score = c(0.5, 0.6, 0.7, 0.8)
  )

  spectral_data <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F4"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B", "D"),
    candidate_library = c("spectral", "spectral", "spectral"),
    score = c(0.8, 0.9, 0.3),
    candidate_score_similarity = c("0.8", "0.9", "0.1"),
    candidate_count_similarity_peaks_matched = c("3", "5", "0")
  )

  ann_list <- list(
    ms1 = ms1_data,
    spectral = spectral_data
  )

  result <- filter_ms1_redundancy(ann_list)

  # F1, F2: high-quality spectral → MS1 suppressed
  # F3: no spectral match → MS1 kept
  # F4: low-quality spectral (sim=0.1, peaks=0) → MS1 NOT suppressed
  ms1_results <- result |>
    tidytable::filter(candidate_library == "ms1")
  expect_equal(nrow(ms1_results), 2)
  expect_true(all(c("F3", "F4") %in% ms1_results$feature_id))

  # All spectral annotations preserved regardless of quality
  spectral_results <- result |>
    tidytable::filter(candidate_library == "spectral")
  expect_equal(nrow(spectral_results), 3)
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

test_that("apply_rt_filter computes RT errors without hard cutoff", {
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

  # All rows should be kept (no hard cutoff); RT errors are computed

  expect_equal(nrow(result), 3)
  expect_true("candidate_structure_error_rt" %in% names(result))
  # F1: error = -0.02, F2: error = -0.5, F3: error = -0.01
  expect_true(!anyNA(result$candidate_structure_error_rt))
  # F2 has the largest error
  expect_true(
    abs(result$candidate_structure_error_rt[result$feature_id == "F2"]) > 0.4
  )
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

test_that("validate_filter_annotations_inputs rejects non-character list elements", {
  feat <- tempfile(fileext = ".tsv")
  .make_feat(feat)

  expect_error(
    validate_filter_annotations_inputs(
      annotations = list(123),
      features = feat,
      rts = character(0),
      output = tempfile(),
      tolerance_rt = 0.5
    ),
    "all annotation elements must be character strings"
  )
})

test_that("validate_filter_annotations_inputs rejects missing RT library files", {
  ann <- tempfile(fileext = ".tsv")
  .make_ann(ann)
  feat <- tempfile(fileext = ".tsv")
  .make_feat(feat)

  expect_error(
    validate_filter_annotations_inputs(
      annotations = ann,
      features = feat,
      rts = "does_not_exist_rt.tsv",
      output = tempfile(),
      tolerance_rt = 0.5
    ),
    "retention time file\\(s\\) not found"
  )
})

test_that("apply_rt_filter deduplicates multiple RT matches to best one", {
  skip_if_not_installed("tidytable")

  features_ann <- tidytable::tidytable(
    feature_id = "F1",
    candidate_structure_inchikey_connectivity_layer = "A",
    rt = "1.0"
  )

  # Two RT entries for same structure -> duplicate join rows
  rt_lib <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = c("A", "A"),
    rt_target = c("1.00", "1.10")
  )

  result <- apply_rt_filter(features_ann, rt_lib, tolerance_rt = Inf)

  # Should deduplicate to best match (error = 0.0)
  expect_equal(nrow(result), 1L)
  expect_true("candidate_structure_error_rt" %in% names(result))
  expect_equal(result$candidate_structure_error_rt[[1L]], 0.0)
})

test_that("apply_rt_filter deduplicates to best RT match when tolerance is finite", {
  skip_if_not_installed("tidytable")

  features_ann <- tidytable::tidytable(
    feature_id = "F1",
    candidate_structure_inchikey_connectivity_layer = "A",
    rt = "1.0"
  )

  rt_lib <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = c("A", "A"),
    rt_target = c("1.20", "1.01")
  )

  result <- apply_rt_filter(features_ann, rt_lib, tolerance_rt = 1.0)

  expect_equal(nrow(result), 1L)
  expect_true(abs(result$candidate_structure_error_rt[[1L]]) < 0.05)
})

test_that("filter_annotations errors when RT table has neither rt nor rt_target", {
  ann <- tempfile(fileext = ".tsv")
  .make_ann(ann)
  feat <- tempfile(fileext = ".tsv")
  .make_feat(feat)
  bad_rt <- tempfile(fileext = ".tsv")
  write.table(
    data.frame(
      candidate_structure_inchikey_connectivity_layer = c("A", "B"),
      wrong_col = c("1", "2")
    ),
    bad_rt,
    sep = "\t",
    row.names = FALSE
  )

  expect_error(
    filter_annotations(
      annotations = ann,
      features = feat,
      rts = bad_rt,
      output = tempfile(fileext = ".tsv"),
      tolerance_rt = 0.5
    ),
    "retention time library must contain column 'rt' or 'rt_target'",
    class = "tima_validation_error"
  )
})

test_that("filter_annotations accepts RT libraries already using rt_target", {
  ann <- tempfile(fileext = ".tsv")
  # Keep annotation table without rt column to avoid rt/rt suffix ambiguity.
  write.table(
    data.frame(
      feature_id = c("F1", "F2"),
      candidate_library = c("ms1", "spectral"),
      candidate_structure_inchikey_connectivity_layer = c("A", "B"),
      candidate_score_similarity = c(NA, 0.8)
    ),
    ann,
    sep = "\t",
    row.names = FALSE
  )
  feat <- tempfile(fileext = ".tsv")
  .make_feat(feat)
  rt_target_file <- tempfile(fileext = ".tsv")
  write.table(
    data.frame(
      candidate_structure_inchikey_connectivity_layer = c("A", "B"),
      rt_target = c("1.02", "2.05")
    ),
    rt_target_file,
    sep = "\t",
    row.names = FALSE
  )

  out <- tempfile(fileext = ".tsv")
  res <- filter_annotations(
    annotations = ann,
    features = feat,
    rts = rt_target_file,
    output = out,
    tolerance_rt = 0.2
  )

  expect_true(file.exists(res))
})
