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

test_that("enforce_ms1_adduct_semantics drops incompatible non-MS1 adducts and keeps exact matches", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F1"),
    mz = c(200, 200, 200),
    candidate_library = c("TIMA MS1", "spectral", "spectral"),
    candidate_adduct = c("[M+NH4]+", "[M+H]+", "[M+NH4]+"),
    candidate_annotation_level = c("primary", NA, NA),
    candidate_evidence_tier = c("supported_strong", NA, NA)
  )

  out <- enforce_ms1_adduct_semantics(df)

  # Non-MS1 [M+H]+ is incompatible with strong [M+NH4]+ at same m/z and must drop.
  expect_false(any(
    out$candidate_library == "spectral" & out$candidate_adduct == "[M+H]+"
  ))
  expect_true(any(
    out$candidate_library == "spectral" & out$candidate_adduct == "[M+NH4]+"
  ))
  expect_true(any(out$candidate_adduct_match_mode == "exact_adduct"))
})

test_that("enforce_ms1_adduct_semantics tags neutral-mass rescue as m_delta_rescued", {
  m_ref <- 300
  mz_ms1 <- calculate_mz_from_mass(m_ref, "[M+NH4]+")
  mz_spec <- calculate_mz_from_mass(m_ref, "[M+H]+")

  df <- tidytable::tidytable(
    feature_id = c("F2", "F2"),
    mz = c(mz_ms1, mz_spec),
    candidate_library = c("TIMA MS1", "spectral"),
    candidate_adduct = c("[M+NH4]+", "[M+H]+"),
    candidate_annotation_level = c("primary", NA),
    candidate_evidence_tier = c("supported_strong", NA)
  )

  out <- enforce_ms1_adduct_semantics(df)
  rescued <- out |>
    tidytable::filter(candidate_library == "spectral")

  expect_equal(nrow(rescued), 1L)
  expect_identical(rescued$candidate_adduct_match_mode[[1L]], "m_delta_rescued")
})

test_that("enforce_ms1_adduct_semantics populates default rescue annotation_note and preserves existing note", {
  m_ref <- 300
  mz_ms1 <- calculate_mz_from_mass(m_ref, "[M+NH4]+")
  mz_spec <- calculate_mz_from_mass(m_ref, "[M+H]+")

  df <- tidytable::tidytable(
    feature_id = c("F2", "F2", "F2"),
    mz = c(mz_ms1, mz_spec, mz_spec),
    candidate_library = c("TIMA MS1", "spectral", "spectral"),
    candidate_adduct = c("[M+NH4]+", "[M+H]+", "[M+H]+"),
    candidate_annotation_level = c("primary", NA, NA),
    candidate_evidence_tier = c("supported_strong", NA, NA),
    annotation_note = c(NA_character_, "prior_note", NA_character_)
  )

  out <- enforce_ms1_adduct_semantics(df)
  rescued <- out |>
    tidytable::filter(candidate_library == "spectral")

  expect_equal(nrow(rescued), 2L)
  expect_identical(rescued$annotation_note[[1L]], "prior_note")
  expect_identical(
    rescued$annotation_note[[2L]],
    "Adduct differs from strong MS1 state; retained by neutral-mass delta consistency"
  )
})

test_that("filter_ms1_redundancy handles NA connectivity layer consistently with anti_join", {
  ms1_data <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3"),
    candidate_structure_inchikey_connectivity_layer = c(
      NA_character_,
      "B",
      NA_character_
    ),
    candidate_library = c("ms1", "ms1", "ms1")
  )
  spectral_data <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    candidate_structure_inchikey_connectivity_layer = c(NA_character_, "B"),
    candidate_library = c("spectral", "spectral"),
    candidate_score_similarity = c("0.9", "0.9"),
    candidate_count_similarity_peaks_matched = c("5", "5")
  )
  res <- filter_ms1_redundancy(list(ms1 = ms1_data, spectral = spectral_data))
  ms1_kept <- res |> tidytable::filter(candidate_library == "ms1")
  expect_equal(ms1_kept$feature_id, "F3")
})

test_that("apply_rt_filter handles mixed single/multi RT entries and pre-existing duplicates", {
  features_ann <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F2", "F3"),
    candidate_structure_inchikey_connectivity_layer = c("A", "A", "B", "C"),
    candidate_library = c("ms1", "ms1", "spectral", "sirius"),
    rt = c("5.0", "5.0", "8.0", NA_character_)
  )
  rt_lib <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = c(
      "A",
      "A",
      "B",
      "C",
      "C"
    ),
    rt_target = c("5.5", "4.9", "8.2", "3.0", "4.0"),
    type = c(
      "experimental",
      "predicted",
      "experimental",
      "experimental",
      "predicted"
    )
  )
  res <- apply_rt_filter(features_ann, rt_lib, tolerance_rt = 0.5)
  expect_equal(nrow(res), 3L)
  expect_equal(
    res$candidate_structure_error_rt[res$feature_id == "F1"],
    0.1,
    tolerance = 1e-6
  )
  expect_equal(
    res$candidate_structure_error_rt[res$feature_id == "F2"],
    -0.2,
    tolerance = 1e-6
  )
  expect_true(is.na(res$candidate_structure_error_rt[res$feature_id == "F3"]))
})

test_that("validate_filter_annotations_inputs, filter_ms1_redundancy, apply_rt_filter, and enforce_ms1_adduct_semantics cover remaining branches", {
  tmp_dir <- withr::local_tempdir()
  valid_file <- file.path(tmp_dir, "valid.tsv")
  writeLines("feature_id\nF1", valid_file)

  # 1. validate_filter_annotations_inputs error paths
  expect_error(
    validate_filter_annotations_inputs(
      annotations = list(file.path(tmp_dir, "missing_list.tsv")),
      features = valid_file,
      rts = NULL,
      output = "out.tsv",
      tolerance_rt = 0.5
    ),
    "annotation file\\(s\\) not found",
    class = "tima_validation_error"
  )
  expect_error(
    validate_filter_annotations_inputs(
      annotations = character(0),
      features = valid_file,
      rts = NULL,
      output = "out.tsv",
      tolerance_rt = 0.5
    ),
    "annotations must be a non-empty character vector or list",
    class = "tima_validation_error"
  )

  expect_error(
    validate_filter_annotations_inputs(
      annotations = valid_file,
      features = valid_file,
      rts = valid_file,
      output = "out.tsv",
      tolerance_rt = 0
    ),
    "tolerance_rt must be a positive number",
    class = "tima_validation_error"
  )

  # 2. filter_ms1_redundancy: exact boundary (sim == 0.3, peaks == 2), inchikey_no_stereo key, NA feature_id in spectral, and 1-col anti_join fallback
  res_nostereo <- filter_ms1_redundancy(list(
    ms1 = tidytable::tidytable(
      feature_id = c("F1", "F2", NA_character_),
      candidate_structure_inchikey_no_stereo = c("IK1", "IK2", "IK3"),
      candidate_structure_inchikey_connectivity_layer = c("IK1", "IK2", "IK3")
    ),
    spectral = tidytable::tidytable(
      feature_id = c("F1", NA_character_),
      candidate_structure_inchikey_no_stereo = c("IK1", NA_character_),
      candidate_structure_inchikey_connectivity_layer = c("IK1", NA_character_),
      candidate_score_similarity = c("0.3", "0.9"),
      candidate_count_similarity_peaks_matched = c("2", "5")
    )
  ))
  expect_identical(nrow(res_nostereo), 4L)

  res_1col_join <- filter_ms1_redundancy(list(
    ms1 = tidytable::tidytable(feature_id = c("F1", "F2")),
    spectral = tidytable::tidytable(
      feature_id = "F1",
      candidate_score_similarity = "0.9",
      candidate_count_similarity_peaks_matched = "5"
    )
  ))
  expect_identical(res_1col_join$feature_id, c("F2", "F1"))

  # 3. apply_rt_filter: empty rt_table, single-RT drop_rt_cols, multi-RT drop_rt_cols, and left_join fallback (extra column in rt_table)
  f_ann_type <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B"),
    candidate_library = c("ms1", "spectral"),
    rt = c("5.0", "8.0"),
    type = c("old1", "old2")
  )
  res_empty_rt <- apply_rt_filter(
    f_ann_type,
    tidytable::tidytable(),
    tolerance_rt = 0.5
  )
  expect_true(all(is.na(res_empty_rt$candidate_structure_error_rt)))
  expect_false("type" %in% names(res_empty_rt))

  res_single_drop <- apply_rt_filter(
    f_ann_type,
    tidytable::tidytable(
      candidate_structure_inchikey_connectivity_layer = "A",
      rt_target = "4.5"
    ),
    tolerance_rt = 0.5
  )
  expect_false("type" %in% names(res_single_drop))
  expect_equal(res_single_drop$candidate_structure_error_rt, c(0.5, NA_real_))

  res_multi_drop <- apply_rt_filter(
    f_ann_type,
    tidytable::tidytable(
      candidate_structure_inchikey_connectivity_layer = c("A", "A"),
      rt_target = c(NA_character_, NA_character_)
    ),
    tolerance_rt = 0.5
  )
  expect_false("type" %in% names(res_multi_drop))

  res_lj_fallback <- apply_rt_filter(
    tidytable::tidytable(
      feature_id = c("F1", "F2"),
      candidate_structure_inchikey_connectivity_layer = c("A", "B"),
      candidate_library = c("ms1", "spectral"),
      rt = c("5.0", "8.0")
    ),
    tidytable::tidytable(
      candidate_structure_inchikey_connectivity_layer = c("A", "A", "B"),
      rt_target = c("4.8", "6.0", "8.5"),
      extra_rt_col = c("e1", "e2", "e3")
    ),
    tolerance_rt = 0.5
  )
  expect_identical(nrow(res_lj_fallback), 2L)
  expect_equal(
    res_lj_fallback$candidate_structure_error_rt[
      res_lj_fallback$feature_id == "F1"
    ],
    0.2,
    tolerance = 1e-6
  )

  # 4. enforce_ms1_adduct_semantics: 0-row, missing library col, missing mz col,
  # multiple primary strong MS1 adducts per feature, and pre-populated match_mode
  expect_identical(
    nrow(enforce_ms1_adduct_semantics(tidytable::tidytable())),
    0L
  )

  res_nomz <- enforce_ms1_adduct_semantics(
    tidytable::tidytable(
      feature_id = c("F1", "F2"),
      candidate_adduct = c("[M+H]+", "[M+Na]+")
    )
  )
  expect_identical(
    res_nomz$candidate_adduct_match_mode,
    c("unconstrained", "unconstrained")
  )

  res_multi_strong <- enforce_ms1_adduct_semantics(
    tidytable::tidytable(
      feature_id = c("F1", "F1", "F1", "F2"),
      mz = c(101.0073, 101.0073, 101.0073, 200.0),
      candidate_adduct = c("[M+H]+", "[M+Na]+", "[M+H]+", "[M+H]+"),
      candidate_library = c("ms1", "ms1", "spectral", "ms1"),
      candidate_annotation_level = c("primary", "primary", NA, "primary"),
      candidate_evidence_tier = c(
        "supported_strong",
        "supported_strong",
        NA,
        "supported_strong"
      ),
      candidate_structure_error_mz = c("0.0001", "0.0050", "0.0002", "0.0001"),
      candidate_adduct_match_mode = c("preset", NA, NA, NA)
    )
  )
  expect_true("preset" %in% res_multi_strong$candidate_adduct_match_mode)
  expect_true("exact_adduct" %in% res_multi_strong$candidate_adduct_match_mode)

  # 5. filter_ms1_redundancy transfers 0.00 spectral similarity onto MS1 row
  # in-place without duplicating rows and drops unassociated 0.00 spectral rows
  res_zero_merge <- filter_ms1_redundancy(list(
    ms1 = tidytable::tidytable(
      feature_id = c("F1", "F2"),
      candidate_structure_inchikey_no_stereo = c(
        "IK_MEASURED_ZERO",
        "IK_MS1_ONLY"
      ),
      candidate_structure_inchikey_connectivity_layer = c(
        "IK_MEASURED_ZERO",
        "IK_MS1_ONLY"
      ),
      candidate_library = c("TIMA MS1", "TIMA MS1"),
      candidate_annotation_level = c("primary", "primary"),
      candidate_evidence_tier = c("supported_strong", "supported_strong"),
      candidate_score_similarity = c(NA_character_, NA_character_),
      candidate_count_similarity_peaks_matched = c(NA_character_, NA_character_)
    ),
    spectral = tidytable::tidytable(
      feature_id = c("F1", "F1"),
      candidate_structure_inchikey_no_stereo = c(
        "IK_MEASURED_ZERO",
        "IK_NON_MS1_ZERO"
      ),
      candidate_structure_inchikey_connectivity_layer = c(
        "IK_MEASURED_ZERO",
        "IK_NON_MS1_ZERO"
      ),
      candidate_library = c("ISDB", "ISDB"),
      candidate_annotation_level = c(NA_character_, NA_character_),
      candidate_evidence_tier = c(NA_character_, NA_character_),
      candidate_score_similarity = c("0", "0"),
      candidate_count_similarity_peaks_matched = c("0", "0")
    )
  ))
  expect_identical(nrow(res_zero_merge), 2L)
  expect_identical(res_zero_merge$candidate_library, c("TIMA MS1", "TIMA MS1"))
  expect_identical(
    res_zero_merge$candidate_annotation_level,
    c("primary", "primary")
  )
  expect_identical(
    res_zero_merge$candidate_score_similarity,
    c("0", NA_character_)
  )
  expect_identical(
    res_zero_merge$candidate_count_similarity_peaks_matched,
    c("0", NA_character_)
  )
})
