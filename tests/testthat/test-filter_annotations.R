# Test Suite: filter_annotations ----

library(testthat)

.test_path <- function(name) file.path(tempdir(), name)

test_that("validate_filter_annotations_inputs accepts valid inputs", {
  features <- .test_path("features.tsv")
  ann <- .test_path("ann.tsv")
  out <- .test_path("output.tsv")

  writeLines("", features)
  writeLines("", ann)

  expect_silent(
    validate_filter_annotations_inputs(
      annotations = ann,
      features = features,
      rts = character(0),
      output = out,
      tolerance_rt = 0.05
    )
  )
})

test_that("validate_filter_annotations_inputs rejects invalid tolerance_rt", {
  features <- .test_path("features.tsv")
  ann <- .test_path("ann.tsv")
  out <- .test_path("output.tsv")

  writeLines("", features)
  writeLines("", ann)

  expect_error(
    validate_filter_annotations_inputs(
      annotations = ann,
      features = features,
      rts = character(0),
      output = out,
      tolerance_rt = -1
    ),
    "tolerance_rt must be a positive number"
  )
})

test_that("validate_filter_annotations_inputs rejects missing files", {
  features <- .test_path("features.tsv")
  out <- .test_path("output.tsv")

  writeLines("", features)

  expect_error(
    validate_filter_annotations_inputs(
      annotations = .test_path("nonexistent.tsv"),
      features = features,
      rts = character(0),
      output = out,
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

  result <- filter_ms1_redundancy(annotation_list)

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

  result <- filter_ms1_redundancy(annotation_list)

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

  result <- apply_rt_filter(features_ann, rt_table, tolerance_rt = 0.1)

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

  result <- apply_rt_filter(features_ann, rt_table, tolerance_rt = 0.1)

  # F1: within tolerance
  # F2: no RT standard, should be kept (NA handling)
  expect_equal(nrow(result), 2)
})

## validate_filter_annotations_inputs - Edge Cases ----

test_that("validate_filter_annotations_inputs accepts list of annotations", {
  features <- .test_path("features.tsv")
  ann1 <- .test_path("ann1.tsv")
  ann2 <- .test_path("ann2.tsv")
  out <- .test_path("output.tsv")

  writeLines("", features)
  writeLines("", ann1)
  writeLines("", ann2)

  expect_silent(
    validate_filter_annotations_inputs(
      annotations = list(ann1, ann2),
      features = features,
      rts = character(0),
      output = out,
      tolerance_rt = 0.05
    )
  )
})

test_that("validate_filter_annotations_inputs rejects empty annotations", {
  features <- .test_path("features.tsv")
  out <- .test_path("output.tsv")

  writeLines("", features)

  expect_error(
    validate_filter_annotations_inputs(
      annotations = character(0),
      features = features,
      rts = character(0),
      output = out,
      tolerance_rt = 0.05
    ),
    "must be a non-empty"
  )
})

test_that("validate_filter_annotations_inputs rejects non-character output", {
  features <- .test_path("features.tsv")
  ann <- .test_path("ann.tsv")
  out <- .test_path("output.tsv")

  writeLines("", features)
  writeLines("", ann)

  expect_error(
    validate_filter_annotations_inputs(
      annotations = ann,
      features = features,
      rts = character(0),
      output = c("out1.tsv", "out2.tsv"),
      tolerance_rt = 0.05
    ),
    "output must be a single character string"
  )
})

test_that("validate_filter_annotations_inputs rejects zero tolerance", {
  features <- .test_path("features.tsv")
  ann <- .test_path("ann.tsv")
  out <- .test_path("output.tsv")

  writeLines("", features)
  writeLines("", ann)

  expect_error(
    validate_filter_annotations_inputs(
      annotations = ann,
      features = features,
      rts = character(0),
      output = out,
      tolerance_rt = 0
    ),
    "tolerance_rt must be a positive number"
  )

  expect_error(
    validate_filter_annotations_inputs(
      annotations = ann,
      features = features,
      rts = character(0),
      output = out,
      tolerance_rt = "0.05"
    ),
    "tolerance_rt must be a positive number"
  )
})

test_that("validate_filter_annotations_inputs validates RT files", {
  features <- .test_path("features.tsv")
  ann <- .test_path("ann.tsv")
  out <- .test_path("output.tsv")

  writeLines("", features)
  writeLines("", ann)

  expect_error(
    validate_filter_annotations_inputs(
      annotations = ann,
      features = features,
      rts = c(.test_path("nonexistent_rt.tsv")),
      output = out,
      tolerance_rt = 0.05
    ),
    "Retention time file.*not found"
  )
})

## filter_ms1_redundancy - Comprehensive Tests ----

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

  annotation_list <- list(ms1 = ms1, gnps = spectral1, massbank = spectral2)

  result <- filter_ms1_redundancy(annotation_list)

  # Should keep: F3 from MS1 + F1 from gnps + F2 from massbank
  expect_equal(nrow(result), 3)
  expect_setequal(result$feature_id, c("F1", "F2", "F3"))
})

test_that("filter_ms1_redundancy handles all MS1 superseded", {
  ms1 <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B")
  )

  spectral <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B")
  )

  annotation_list <- list(ms1 = ms1, spectral = spectral)

  result <- filter_ms1_redundancy(annotation_list)

  # All MS1 removed, only spectral kept
  expect_equal(nrow(result), 2)
})

test_that("filter_ms1_redundancy handles no MS1 superseded", {
  ms1 <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B")
  )

  spectral <- tidytable::tidytable(
    feature_id = c("F3", "F4"),
    candidate_structure_inchikey_connectivity_layer = c("C", "D")
  )

  annotation_list <- list(ms1 = ms1, spectral = spectral)

  result <- filter_ms1_redundancy(annotation_list)

  # All annotations kept
  expect_equal(nrow(result), 4)
})

test_that("filter_ms1_redundancy handles empty MS1", {
  ms1 <- tidytable::tidytable(
    feature_id = character(0),
    candidate_structure_inchikey_connectivity_layer = character(0)
  )

  spectral <- tidytable::tidytable(
    feature_id = c("F1"),
    candidate_structure_inchikey_connectivity_layer = c("A")
  )

  annotation_list <- list(ms1 = ms1, spectral = spectral)

  result <- filter_ms1_redundancy(annotation_list)

  expect_equal(nrow(result), 1)
})

## apply_rt_filter - Comprehensive Tests ----

test_that("apply_rt_filter handles edge of tolerance", {
  features_ann <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3"),
    rt = c("1.00", "2.00", "3.00"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B", "C")
  )

  rt_table <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = c("A", "B", "C"),
    rt_target = c("1.05", "2.10", "3.11")
  )

  result <- apply_rt_filter(features_ann, rt_table, tolerance_rt = 0.1)

  # F1: |1.00 - 1.05| = 0.05 ≤ 0.1 ✓
  # F2: |2.00 - 2.10| = 0.10 ≤ 0.1 ✓ (exactly at tolerance)
  # F3: |3.00 - 3.11| = 0.11 > 0.1 ✗
  expect_equal(nrow(result), 2)
  expect_setequal(result$feature_id, c("F1", "F2"))
})

test_that("apply_rt_filter handles numeric RT values", {
  features_ann <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    rt = c(1.5, 2.0),
    candidate_structure_inchikey_connectivity_layer = c("A", "B")
  )

  rt_table <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = c("A", "B"),
    rt_target = c(1.48, 2.2)
  )

  result <- apply_rt_filter(features_ann, rt_table, tolerance_rt = 0.1)

  expect_equal(nrow(result), 1)
  expect_equal(result$feature_id, "F1")
})

test_that("apply_rt_filter removes duplicate feature-structure pairs", {
  # Same feature-structure pair with different RT errors should keep best
  features_ann <- tidytable::tidytable(
    feature_id = c("F1", "F1"),
    rt = c("1.50", "1.50"),
    candidate_structure_inchikey_connectivity_layer = c("A", "A")
  )

  rt_table <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = c("A"),
    rt_target = c("1.48")
  )

  result <- apply_rt_filter(features_ann, rt_table, tolerance_rt = 0.1)

  # Should have only one row after distinct
  expect_equal(nrow(result), 1)
})

test_that("apply_rt_filter adds RT error column", {
  features_ann <- tidytable::tidytable(
    feature_id = c("F1"),
    rt = c("1.50"),
    candidate_structure_inchikey_connectivity_layer = c("A")
  )

  rt_table <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = c("A"),
    rt_target = c("1.48")
  )

  result <- apply_rt_filter(features_ann, rt_table, tolerance_rt = 0.1)

  expect_true("candidate_structure_error_rt" %in% names(result))
  expect_equal(
    as.numeric(result$candidate_structure_error_rt),
    0.02,
    tolerance = 0.001
  )
})

## Performance Tests ----

test_that("filter_ms1_redundancy is efficient with large datasets", {
  # Create large test data
  n <- 10000

  ms1 <- tidytable::tidytable(
    feature_id = paste0("F", 1:n),
    candidate_structure_inchikey_connectivity_layer = paste0("IK", 1:n)
  )

  spectral <- tidytable::tidytable(
    feature_id = paste0("F", 1:(n / 2)),
    candidate_structure_inchikey_connectivity_layer = paste0("IK", 1:(n / 2))
  )

  annotation_list <- list(ms1 = ms1, spectral = spectral)

  elapsed <- system.time({
    result <- filter_ms1_redundancy(annotation_list)
  })

  # Should complete in reasonable time (< 2 seconds for 10k rows)
  expect_true(elapsed["elapsed"] < 2.0)

  # Verify correctness
  expect_equal(nrow(result), n / 2 + n / 2) # 5k from spectral + 5k from MS1
})

test_that("apply_rt_filter is efficient with large datasets", {
  n <- 10000

  features_ann <- tidytable::tidytable(
    feature_id = paste0("F", 1:n),
    rt = as.character(runif(n, 0, 10)),
    candidate_structure_inchikey_connectivity_layer = paste0("IK", 1:n)
  )

  rt_table <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = paste0("IK", 1:n),
    rt_target = as.character(runif(n, 0, 10))
  )

  elapsed <- system.time({
    result <- apply_rt_filter(features_ann, rt_table, tolerance_rt = 0.1)
  })

  # Should complete in reasonable time (< 3 seconds for 10k rows)
  expect_true(elapsed["elapsed"] < 3.0)
})

## Integration Tests ----

test_that("validate_filter_annotations_inputs integrates with all helpers", {
  # This test ensures validation happens before processing
  features <- .test_path("features.tsv")
  ann <- .test_path("ann.tsv")
  out <- .test_path("output.tsv")

  writeLines("feature_id\nF1", features)
  writeLines("feature_id\nF1", ann)

  # Should pass all validations
  expect_silent(
    validate_filter_annotations_inputs(
      annotations = list(ann),
      features = features,
      rts = character(0),
      output = out,
      tolerance_rt = 0.05
    )
  )
})

## Regression Tests ----

test_that("helper functions maintain backward compatibility", {
  # Ensure function signatures haven't changed
  expect_equal(
    length(formals(validate_filter_annotations_inputs)),
    5
  )

  expect_equal(
    length(formals(filter_ms1_redundancy)),
    1
  )

  expect_equal(
    length(formals(apply_rt_filter)),
    3
  )
})

## Edge Cases - More ----

test_that("filter_ms1_redundancy preserves column names", {
  ms1 <- tidytable::tidytable(
    feature_id = c("F1"),
    candidate_structure_inchikey_connectivity_layer = c("A"),
    extra_column = c("test")
  )

  spectral <- tidytable::tidytable(
    feature_id = c("F2"),
    candidate_structure_inchikey_connectivity_layer = c("B"),
    different_column = c("value")
  )

  annotation_list <- list(ms1 = ms1, spectral = spectral)

  result <- filter_ms1_redundancy(annotation_list)

  # Should have all columns from both tables
  expect_true("extra_column" %in% names(result))
  expect_true("different_column" %in% names(result))
})

test_that("apply_rt_filter handles negative RT values", {
  # Edge case: negative RT (shouldn't happen but test robustness)
  features_ann <- tidytable::tidytable(
    feature_id = c("F1"),
    rt = c("-1.0"),
    candidate_structure_inchikey_connectivity_layer = c("A")
  )

  rt_table <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = c("A"),
    rt_target = c("-0.95")
  )

  result <- apply_rt_filter(features_ann, rt_table, tolerance_rt = 0.1)

  # Should still calculate error correctly
  expect_equal(nrow(result), 1)
  expect_equal(
    abs(as.numeric(result$candidate_structure_error_rt)),
    0.05,
    tolerance = 0.001
  )
})

test_that("validate_filter_annotations_inputs rejects missing RT files", {
  features <- .test_path("features.tsv")
  ann <- .test_path("ann.tsv")
  out <- .test_path("output.tsv")
  rt1 <- .test_path("rt1.tsv")

  writeLines("", features)
  writeLines("", ann)
  writeLines("", rt1)

  expect_error(
    validate_filter_annotations_inputs(
      annotations = ann,
      features = features,
      rts = c(rt1, "rt2.tsv"),
      output = out,
      tolerance_rt = 0.05
    ),
    "Retention time file.*not found"
  )
})

## filter_ms1_redundancy -----

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

  result <- filter_ms1_redundancy(annotation_list)

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

  result <- filter_ms1_redundancy(annotation_list)

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

  result <- filter_ms1_redundancy(annotation_list)

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

  result <- filter_ms1_redundancy(annotation_list)

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

  result <- filter_ms1_redundancy(annotation_list)

  # F1 and F2 removed from MS1, F3 kept from MS1, F1 and F2 from spectral
  expect_equal(nrow(result), 3)
  expect_true("C" %in% result$candidate_structure_inchikey_connectivity_layer)
})

## apply_rt_filter ----

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

  result <- apply_rt_filter(features_ann, rt_table, tolerance_rt = 0.01)

  expect_equal(nrow(result), 1)
})

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

  result <- apply_rt_filter(features_ann, rt_table, tolerance_rt = 0.05)

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

  result <- apply_rt_filter(features_ann, rt_table, tolerance_rt = 0.1)

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

  result <- apply_rt_filter(features_ann, rt_table, tolerance_rt = 0.1)

  # All should be kept (no RT standards to compare against)
  expect_equal(nrow(result), 2)
})

# test_that("apply_rt_filter calculates error correctly", {
#   features_ann <- tidytable::tidytable(
#     feature_id = c("F1", "F2"),
#     rt = c("2.0", "1.5"),
#     candidate_structure_inchikey_connectivity_layer = c("A", "A")
#   )
#
#   rt_table <- tidytable::tidytable(
#     candidate_structure_inchikey_connectivity_layer = c("A"),
#     rt_target = c("1.8")
#   )
#
#   result <- apply_rt_filter(features_ann, rt_table, tolerance_rt = 0.3)
#
#   # Both within tolerance, but distinct keeps closest:
#   # F2: |1.5 - 1.8| = 0.3 (passes, but farther)
#   # F1: |2.0 - 1.8| = 0.2 (passes, closer)
#   # Distinct keeps F1 (smallest abs error after arrange)
#   expect_equal(nrow(result), 1)
#   expect_equal(result$feature_id, "F1")
# })

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

  result <- apply_rt_filter(features_ann, rt_table, tolerance_rt = 0.1)

  # Should not have rt_target or type columns in result
  expect_false("rt_target" %in% names(result))
  expect_false("type" %in% names(result))
})

## filter_ms1_redundancy - Additional edge cases ----

test_that("test-filter_ms1_redundancy preserves all MS1 when no spectral overlap", {
  ms1 <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B")
  )

  spectral <- tidytable::tidytable(
    feature_id = c("F3", "F4"),
    candidate_structure_inchikey_connectivity_layer = c("C", "D")
  )

  annotation_list <- list(ms1 = ms1, spectral = spectral)
  result <- filter_ms1_redundancy(annotation_list)

  # All 4 annotations should be kept (no overlap)
  expect_equal(nrow(result), 4)
})

test_that("test-filter_ms1_redundancy handles empty MS1 table", {
  ms1 <- tidytable::tidytable(
    feature_id = character(),
    candidate_structure_inchikey_connectivity_layer = character()
  )

  spectral <- tidytable::tidytable(
    feature_id = c("F1"),
    candidate_structure_inchikey_connectivity_layer = c("A")
  )

  annotation_list <- list(ms1 = ms1, spectral = spectral)
  result <- filter_ms1_redundancy(annotation_list)

  expect_equal(nrow(result), 1)
  expect_equal(result$feature_id[1], "F1")
})

## apply_rt_filter - Additional edge cases ----

test_that("test-apply_rt_filter handles features with no RT value", {
  features_ann <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    rt = c(NA_character_, "2.0"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B")
  )

  rt_table <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = c("A", "B"),
    rt_target = c("1.5", "2.0")
  )

  result <- apply_rt_filter(features_ann, rt_table, tolerance_rt = 0.1)

  # F2 should pass (exact match)
  # F1 has NA rt, behavior depends on implementation
  expect_true("F2" %in% result$feature_id)
})

test_that("test-apply_rt_filter handles empty RT table", {
  features_ann <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    rt = c("1.5", "2.0"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B")
  )

  rt_table <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = character(),
    rt_target = character()
  )

  result <- apply_rt_filter(features_ann, rt_table, tolerance_rt = 0.1)

  # All features should pass (no RT constraints)
  expect_equal(nrow(result), 2)
})

test_that("test-apply_rt_filter calculates RT error correctly", {
  features_ann <- tidytable::tidytable(
    feature_id = c("F1"),
    rt = c("5.0"),
    candidate_structure_inchikey_connectivity_layer = c("A")
  )

  rt_table <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = c("A"),
    rt_target = c("4.5")
  )

  result <- apply_rt_filter(features_ann, rt_table, tolerance_rt = 1.0)

  # Error = 5.0 - 4.5 = 0.5, within 1.0 tolerance
  expect_equal(nrow(result), 1)
  expect_true("candidate_structure_error_rt" %in% names(result))
  expect_equal(result$candidate_structure_error_rt[1], 0.5)
})

# test_that("test-filter_annotations combines annotations and removes MS1 redundancy", {
#   tmp <- withr::local_tempdir(.local_envir = parent.frame())
#   withr::local_dir(tmp, .local_envir = parent.frame())
#   ms1 <- tidytable::tidytable(
#     feature_id = c("F1"),
#     candidate_structure_inchikey_connectivity_layer = c("AAA")
#   )
#   spectral <- tidytable::tidytable(
#     feature_id = c("F1"),
#     candidate_structure_inchikey_connectivity_layer = c("AAA"),
#     candidate_score_similarity = c("0.9")
#   )
#   features <- tidytable::tidytable(
#     feature_id = c("F1"),
#     mz = c(100),
#     rt = c("1.0")
#   )
#   ann_ms1 <- file.path(tmp, "ms1.tsv")
#   ann_spec <- file.path(tmp, "spec.tsv")
#   feat <- file.path(tmp, "features.tsv")
#   tidytable::fwrite(ms1, ann_ms1, sep = "\t")
#   tidytable::fwrite(spectral, ann_spec, sep = "\t")
#   tidytable::fwrite(features, feat, sep = "\t")
#   out <- file.path(tmp, "filtered.tsv")
#   res <- filter_annotations(
#     annotations = c(ann_ms1, ann_spec),
#     features = feat,
#     rts = character(0),
#     output = out,
#     tolerance_rt = 0.1
#   )
#   expect_true(file.exists(res))
#   df <- tidytable::fread(res)
#   expect_equal(nrow(df), 1) # spectral replaces MS1
# })

# test_that("test-filter_annotations applies RT filtering when library provided", {
#   tmp <- withr::local_tempdir(.local_envir = parent.frame())
#   withr::local_dir(tmp, .local_envir = parent.frame())
#   spectral <- tidytable::tidytable(
#     feature_id = c("F1"),
#     candidate_structure_inchikey_connectivity_layer = c("AAA"),
#     candidate_score_similarity = c("0.9"),
#     rt = c("1.05")
#   )
#   features <- tidytable::tidytable(
#     feature_id = c("F1"),
#     rt = c("1.05"),
#     mz = c(100)
#   )
#   rt_lib <- tidytable::tidytable(
#     candidate_structure_inchikey_layer = c("AAA"),
#     rt_target = c("1.00")
#   )
#   ann_spec <- file.path(tmp, "spec.tsv")
#   feat <- file.path(tmp, "features.tsv")
#   rtl <- file.path(tmp, "rt.tsv")
#   tidytable::fwrite(spectral, ann_spec, sep = "\t")
#   tidytable::fwrite(features, feat, sep = "\t")
#   tidytable::fwrite(rt_lib, rtl, sep = "\t")
#   out <- file.path(tmp, "filtered.tsv")
#   res <- filter_annotations(
#     annotations = ann_spec,
#     features = feat,
#     rts = rtl,
#     output = out,
#     tolerance_rt = 0.1
#   )
#   df <- tidytable::fread(res)
#   expect_true(file.exists(res))
#   expect_equal(nrow(df), 1)
# })

# test_that("test-filter_annotations retains annotation outside RT when tolerance larger", {
#   tmp <- withr::local_tempdir(.local_envir = parent.frame())
#   withr::local_dir(tmp, .local_envir = parent.frame())
#   spectral <- tidytable::tidytable(
#     feature_id = c("F1"),
#     candidate_structure_inchikey_connectivity_layer = c("AAA"),
#     candidate_score_similarity = c("0.9"),
#     rt = c("1.5")
#   )
#   features <- tidytable::tidytable(
#     feature_id = c("F1"),
#     rt = c("1.5"),
#     mz = c(100)
#   )
#   rt_lib <- tidytable::tidytable(
#     candidate_structure_inchikey_layer = c("AAA"),
#     rt_target = c("1.0")
#   )
#   ann_spec <- file.path(tmp, "spec.tsv")
#   feat <- file.path(tmp, "features.tsv")
#   rtl <- file.path(tmp, "rt.tsv")
#   tidytable::fwrite(spectral, ann_spec, sep = "\t")
#   tidytable::fwrite(features, feat, sep = "\t")
#   tidytable::fwrite(rt_lib, rtl, sep = "\t")
#   out <- file.path(tmp, "filtered.tsv")
#   res <- filter_annotations(
#     annotations = ann_spec,
#     features = feat,
#     rts = rtl,
#     output = out,
#     tolerance_rt = 1.0
#   )
#   df <- tidytable::fread(res)
#   expect_true(file.exists(res))
#   expect_equal(nrow(df), 1)
# })
