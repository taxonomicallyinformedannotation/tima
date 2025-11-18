# ==============================================================================
# Test Suite: filter_annotations
# ==============================================================================

test_that("validate_filter_annotations_inputs accepts valid inputs", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  writeLines("", "features.tsv")
  writeLines("", "ann.tsv")

  expect_silent(
    tima:::validate_filter_annotations_inputs(
      annotations = "ann.tsv",
      features = "features.tsv",
      rts = character(0),
      output = "output.tsv",
      tolerance_rt = 0.05
    )
  )
})

test_that("validate_filter_annotations_inputs rejects invalid tolerance_rt", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  writeLines("", "features.tsv")
  writeLines("", "ann.tsv")

  expect_error(
    tima:::validate_filter_annotations_inputs(
      annotations = "ann.tsv",
      features = "features.tsv",
      rts = character(0),
      output = "output.tsv",
      tolerance_rt = -1
    ),
    "tolerance_rt must be a positive number"
  )
})

test_that("validate_filter_annotations_inputs rejects missing files", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  writeLines("", "features.tsv")

  expect_error(
    tima:::validate_filter_annotations_inputs(
      annotations = "nonexistent.tsv",
      features = "features.tsv",
      rts = character(0),
      output = "output.tsv",
      tolerance_rt = 0.05
    ),
    "Annotation file.*not found"
  )
})

test_that("filter_ms1_redundancy removes MS1 when spectral exists", {
  ms1 <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B")
  )

  spectral <- tidytable::tidytable(
    feature_id = c("F1"),
    candidate_structure_inchikey_connectivity_layer = c("A")
  )

  annotation_list <- list(ms1 = ms1, spectral = spectral)

  result <- tima:::filter_ms1_redundancy(annotation_list)

  # Should keep: F2 from MS1 (no spectral match) + F1 from spectral
  expect_equal(nrow(result), 2)
  expect_true("F2" %in% result$feature_id)
  expect_true("F1" %in% result$feature_id)
})

test_that("filter_ms1_redundancy handles no MS1 annotations", {
  spectral <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B")
  )

  annotation_list <- list(spectral = spectral)

  result <- tima:::filter_ms1_redundancy(annotation_list)

  expect_equal(nrow(result), 2)
})

test_that("apply_rt_filter filters by tolerance", {
  features_ann <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3"),
    rt = c("1.5", "2.0", "3.0"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B", "C")
  )

  rt_table <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = c("A", "B", "C"),
    rt_target = c("1.48", "2.2", "3.0")
  )

  result <- tima:::apply_rt_filter(features_ann, rt_table, tolerance_rt = 0.1)

  # F1: |1.5 - 1.48| = 0.02 ≤ 0.1 ✓
  # F2: |2.0 - 2.2| = 0.2 > 0.1 ✗
  # F3: |3.0 - 3.0| = 0.0 ≤ 0.1 ✓
  expect_equal(nrow(result), 2)
  expect_true(all(c("F1", "F3") %in% result$feature_id))
})

test_that("apply_rt_filter keeps annotations without RT standards", {
  features_ann <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    rt = c("1.5", "2.0"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B")
  )

  rt_table <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = c("A"),
    rt_target = c("1.48")
  )

  result <- tima:::apply_rt_filter(features_ann, rt_table, tolerance_rt = 0.1)

  # F1: within tolerance
  # F2: no RT standard, should be kept (NA handling)
  expect_equal(nrow(result), 2)
})

# ==============================================================================
# Additional Test Suite: validate_filter_annotations_inputs - Edge Cases
# ==============================================================================

test_that("validate_filter_annotations_inputs accepts list of annotations", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  writeLines("", "features.tsv")
  writeLines("", "ann1.tsv")
  writeLines("", "ann2.tsv")

  expect_silent(
    tima:::validate_filter_annotations_inputs(
      annotations = list("ann1.tsv", "ann2.tsv"),
      features = "features.tsv",
      rts = character(0),
      output = "output.tsv",
      tolerance_rt = 0.05
    )
  )
})

test_that("validate_filter_annotations_inputs rejects empty annotations", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  writeLines("", "features.tsv")

  expect_error(
    tima:::validate_filter_annotations_inputs(
      annotations = character(0),
      features = "features.tsv",
      rts = character(0),
      output = "output.tsv",
      tolerance_rt = 0.05
    ),
    "must be a non-empty"
  )
})

test_that("validate_filter_annotations_inputs rejects non-character output", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  writeLines("", "features.tsv")
  writeLines("", "ann.tsv")

  expect_error(
    tima:::validate_filter_annotations_inputs(
      annotations = "ann.tsv",
      features = "features.tsv",
      rts = character(0),
      output = c("out1.tsv", "out2.tsv"),
      tolerance_rt = 0.05
    ),
    "output must be a single character string"
  )
})

test_that("validate_filter_annotations_inputs rejects zero tolerance", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  writeLines("", "features.tsv")
  writeLines("", "ann.tsv")

  expect_error(
    tima:::validate_filter_annotations_inputs(
      annotations = "ann.tsv",
      features = "features.tsv",
      rts = character(0),
      output = "output.tsv",
      tolerance_rt = 0
    ),
    "tolerance_rt must be a positive number"
  )
})

test_that("validate_filter_annotations_inputs rejects missing RT files", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  writeLines("", "features.tsv")
  writeLines("", "ann.tsv")
  writeLines("", "rt1.tsv")

  expect_error(
    tima:::validate_filter_annotations_inputs(
      annotations = "ann.tsv",
      features = "features.tsv",
      rts = c("rt1.tsv", "rt2.tsv"),
      output = "output.tsv",
      tolerance_rt = 0.05
    ),
    "Retention time file.*not found"
  )
})

# ==============================================================================
# Additional Test Suite: filter_ms1_redundancy - Comprehensive
# ==============================================================================

test_that("filter_ms1_redundancy handles only spectral annotations", {
  spectral1 <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B")
  )

  spectral2 <- tidytable::tidytable(
    feature_id = c("F3"),
    candidate_structure_inchikey_connectivity_layer = c("C")
  )

  annotation_list <- list(spectral1 = spectral1, spectral2 = spectral2)

  result <- tima:::filter_ms1_redundancy(annotation_list)

  expect_equal(nrow(result), 3)
})

test_that("filter_ms1_redundancy removes all MS1 when all have spectral matches", {
  ms1 <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B")
  )

  spectral <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B")
  )

  annotation_list <- list(ms1 = ms1, spectral = spectral)

  result <- tima:::filter_ms1_redundancy(annotation_list)

  # All MS1 removed, only spectral kept
  expect_equal(nrow(result), 2)
})

test_that("filter_ms1_redundancy keeps MS1 with different inchikeys", {
  ms1 <- tidytable::tidytable(
    feature_id = c("F1", "F1"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B")
  )

  spectral <- tidytable::tidytable(
    feature_id = c("F1"),
    candidate_structure_inchikey_connectivity_layer = c("A")
  )

  annotation_list <- list(ms1 = ms1, spectral = spectral)

  result <- tima:::filter_ms1_redundancy(annotation_list)

  # F1-A removed from MS1, F1-B kept, plus F1-A from spectral = 2 rows
  expect_equal(nrow(result), 2)
  expect_true("B" %in% result$candidate_structure_inchikey_connectivity_layer)
})

test_that("filter_ms1_redundancy handles empty MS1 table", {
  ms1 <- tidytable::tidytable(
    feature_id = character(),
    candidate_structure_inchikey_connectivity_layer = character()
  )

  spectral <- tidytable::tidytable(
    feature_id = c("F1"),
    candidate_structure_inchikey_connectivity_layer = c("A")
  )

  annotation_list <- list(ms1 = ms1, spectral = spectral)

  result <- tima:::filter_ms1_redundancy(annotation_list)

  expect_equal(nrow(result), 1)
})

test_that("filter_ms1_redundancy handles multiple spectral sources", {
  ms1 <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B", "C")
  )

  spectral1 <- tidytable::tidytable(
    feature_id = c("F1"),
    candidate_structure_inchikey_connectivity_layer = c("A")
  )

  spectral2 <- tidytable::tidytable(
    feature_id = c("F2"),
    candidate_structure_inchikey_connectivity_layer = c("B")
  )

  annotation_list <- list(
    ms1 = ms1,
    spectral1 = spectral1,
    spectral2 = spectral2
  )

  result <- tima:::filter_ms1_redundancy(annotation_list)

  # F1 and F2 removed from MS1, F3 kept from MS1, F1 and F2 from spectral
  expect_equal(nrow(result), 3)
  expect_true("C" %in% result$candidate_structure_inchikey_connectivity_layer)
})

# ==============================================================================
# Additional Test Suite: apply_rt_filter - Comprehensive
# ==============================================================================

test_that("apply_rt_filter handles exact RT match", {
  features_ann <- tidytable::tidytable(
    feature_id = c("F1"),
    rt = c("1.5"),
    candidate_structure_inchikey_connectivity_layer = c("A")
  )

  rt_table <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = c("A"),
    rt_target = c("1.5")
  )

  result <- tima:::apply_rt_filter(features_ann, rt_table, tolerance_rt = 0.01)

  expect_equal(nrow(result), 1)
})

# test_that("apply_rt_filter handles boundary tolerance", {
#   features_ann <- tidytable::tidytable(
#     feature_id = c("F1", "F2"),
#     rt = c("1.5", "1.6"),
#     candidate_structure_inchikey_connectivity_layer = c("A", "A")
#   )
#
#   rt_table <- tidytable::tidytable(
#     candidate_structure_inchikey_connectivity_layer = c("A"),
#     rt_target = c("1.5")
#   )
#
#   result <- apply_rt_filter(features_annotated_table = features_ann, rt_table = rt_table, tolerance_rt = 0.1)
#
#   # F1: 0.0 ≤ 0.1 ✓
#   # F2: 0.1 ≤ 0.1 ✓ (boundary)
#   expect_equal(nrow(result), 2)
# })

test_that("apply_rt_filter removes duplicates keeping closest RT", {
  features_ann <- tidytable::tidytable(
    feature_id = c("F1", "F1"),
    rt = c("1.5", "1.6"),
    candidate_structure_inchikey_connectivity_layer = c("A", "A")
  )

  rt_table <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = c("A"),
    rt_target = c("1.5")
  )

  result <- tima:::apply_rt_filter(features_ann, rt_table, tolerance_rt = 0.05)

  # Should keep only one F1-A, the one with closest RT (1.5)
  expect_equal(nrow(result), 1)
  expect_equal(result$rt, "1.5")
})

test_that("apply_rt_filter handles all annotations outside tolerance", {
  features_ann <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    rt = c("1.5", "2.0"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B")
  )

  rt_table <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = c("A", "B"),
    rt_target = c("2.0", "3.5")
  )

  result <- tima:::apply_rt_filter(features_ann, rt_table, tolerance_rt = 0.1)

  # F1: |1.5 - 2.0| = 0.5 > 0.1 ✗
  # F2: |2.0 - 3.5| = 1.5 > 0.1 ✗
  expect_equal(nrow(result), 0)
})

test_that("apply_rt_filter handles empty RT table", {
  features_ann <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    rt = c("1.5", "2.0"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B")
  )

  rt_table <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = character(),
    rt_target = character()
  )

  result <- tima:::apply_rt_filter(features_ann, rt_table, tolerance_rt = 0.1)

  # All should be kept (no RT standards to compare against)
  expect_equal(nrow(result), 2)
})

test_that("apply_rt_filter calculates error correctly", {
  features_ann <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    rt = c("2.0", "1.5"),
    candidate_structure_inchikey_connectivity_layer = c("A", "A")
  )

  rt_table <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = c("A"),
    rt_target = c("1.8")
  )

  result <- tima:::apply_rt_filter(features_ann, rt_table, tolerance_rt = 0.3)

  # Both within tolerance, but distinct keeps closest:
  # F2: |1.5 - 1.8| = 0.3 (passes, but farther)
  # F1: |2.0 - 1.8| = 0.2 (passes, closer)
  # Distinct keeps F1 (smallest abs error after arrange)
  expect_equal(nrow(result), 1)
  expect_equal(result$feature_id, "F1")
})

test_that("apply_rt_filter removes optional columns", {
  features_ann <- tidytable::tidytable(
    feature_id = c("F1"),
    rt = c("1.5"),
    candidate_structure_inchikey_connectivity_layer = c("A")
  )

  rt_table <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = c("A"),
    rt_target = c("1.5"),
    type = c("standard")
  )

  result <- tima:::apply_rt_filter(features_ann, rt_table, tolerance_rt = 0.1)

  # Should not have rt_target or type columns in result
  expect_false("rt_target" %in% names(result))
  expect_false("type" %in% names(result))
})
