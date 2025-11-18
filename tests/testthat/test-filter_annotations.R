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
