# ==============================================================================
# Test Suite: minimize_results
# ==============================================================================

# ==============================================================================
# Test: Input Validation
# ==============================================================================

test_that("minimize_results validates data frame input", {
  expect_error(
    minimize_results(
      df = "not_a_dataframe",
      features_table = tidytable::tidytable()
    ),
    "must be a data frame"
  )
})

# test_that("minimize_results handles empty data frame", {
#   result <- minimize_results(
#     df = tidytable::tidytable(),
#     features_table = tidytable::tidytable(),
#     best_percentile = 0.9
#   )
#   expect_s3_class(result, "data.frame")
#   expect_equal(nrow(result), 0L)
# })

# ==============================================================================
# Test: Percentile Filtering
# ==============================================================================

# test_that("minimize_results filters by percentile threshold", {
#   skip_on_cran()
#
#   df <- tidytable::tidytable(
#     feature_id = rep("F1", 5),
#     candidate_structure_inchikey_connectivity_layer = paste0("STRUCT", 1:5),
#     score_weighted_chemo = c(1.0, 0.9, 0.8, 0.5, 0.3),
#     feature_pred_tax_npc_01pat_val = rep("Alkaloids", 5),
#     feature_pred_tax_npc_02sup_val = rep("Terpenoids", 5),
#     feature_pred_tax_npc_03cla_val = rep("Class1", 5),
#     feature_pred_tax_cla_01kin_val = rep("Kingdom1", 5),
#     feature_pred_tax_cla_02sup_val = rep("Superclass1", 5),
#     feature_pred_tax_cla_03cla_val = rep("Class3", 5),
#     feature_pred_tax_cla_04dirpar_val = rep("Parent1", 5)
#   )
#
#   features <- tidytable::tidytable(feature_id = "F1")
#
#   result <- minimize_results(
#     df = df,
#     features_table = features,
#     best_percentile = 0.9
#   )
#
#   # Should keep candidates >= 90% of max score (1.0 * 0.9 = 0.9)
#   expect_s3_class(result, "data.frame")
#   expect_true(all(result$score_weighted_chemo >= 0.9))
#   expect_lte(nrow(result), nrow(df))
# })

# test_that("minimize_results with 100% percentile keeps all candidates", {
#   skip_on_cran()
#
#   df <- tidytable::tidytable(
#     feature_id = rep("F1", 3),
#     score_weighted_chemo = c(1.0, 0.5, 0.1),
#     feature_pred_tax_npc_01pat_val = rep("Alkaloids", 3),
#     feature_pred_tax_npc_02sup_val = rep("Terpenoids", 3),
#     feature_pred_tax_npc_03cla_val = rep("Class1", 3),
#     feature_pred_tax_cla_01kin_val = rep("Kingdom1", 3),
#     feature_pred_tax_cla_02sup_val = rep("Superclass1", 3),
#     feature_pred_tax_cla_03cla_val = rep("Class3", 3),
#     feature_pred_tax_cla_04dirpar_val = rep("Parent1", 3)
#   )
#
#   features <- tidytable::tidytable(feature_id = "F1")
#
#   result <- minimize_results(
#     df = df,
#     features_table = features,
#     best_percentile = 1.0
#   )
#
#   expect_equal(nrow(result), 3)
# })

# ==============================================================================
# Test: Parameter Validation
# ==============================================================================

test_that("minimize_results validates best_percentile parameter", {
  skip_on_cran()

  df <- tidytable::tidytable(
    feature_id = "F1",
    score_weighted_chemo = 1.0
  )

  features <- tidytable::tidytable(feature_id = "F1")

  expect_error(
    minimize_results(df, features, best_percentile = -0.1),
    "between 0 and 1"
  )

  expect_error(
    minimize_results(df, features, best_percentile = 1.5),
    "between 0 and 1"
  )
})

# ==============================================================================
# Test: Multiple Features
# ==============================================================================

# test_that("minimize_results handles multiple features independently", {
#   skip_on_cran()
#
#   df <- tidytable::tidytable(
#     feature_id = c(rep("F1", 3), rep("F2", 3)),
#     score_weighted_chemo = c(1.0, 0.8, 0.5, 0.9, 0.85, 0.4),
#     feature_pred_tax_npc_01pat_val = rep("Alkaloids", 6),
#     feature_pred_tax_npc_02sup_val = rep("Terpenoids", 6),
#     feature_pred_tax_npc_03cla_val = rep("Class1", 6),
#     feature_pred_tax_cla_01kin_val = rep("Kingdom1", 6),
#     feature_pred_tax_cla_02sup_val = rep("Superclass1", 6),
#     feature_pred_tax_cla_03cla_val = rep("Class3", 6),
#     feature_pred_tax_cla_04dirpar_val = rep("Parent1", 6)
#   )
#
#   features <- tidytable::tidytable(feature_id = c("F1", "F2"))
#
#   result <- minimize_results(
#     df = df,
#     features_table = features,
#     best_percentile = 0.9
#   )
#
#   # F1: max=1.0, threshold=0.9, keep 1.0 and 0.8? No, only >=0.9
#   # F2: max=0.9, threshold=0.81, keep 0.9 and 0.85
#   expect_s3_class(result, "data.frame")
#   expect_true(all(c("F1", "F2") %in% result$feature_id))
# })

# ==============================================================================
# Test: Candidate Counting
# ==============================================================================

# test_that("minimize_results adds candidate count columns", {
#   skip_on_cran()
#
#   df <- tidytable::tidytable(
#     feature_id = rep("F1", 3),
#     score_weighted_chemo = c(1.0, 0.9, 0.8),
#     feature_pred_tax_npc_01pat_val = rep("Alkaloids", 3),
#     feature_pred_tax_npc_02sup_val = rep("Terpenoids", 3),
#     feature_pred_tax_npc_03cla_val = rep("Class1", 3),
#     feature_pred_tax_cla_01kin_val = rep("Kingdom1", 3),
#     feature_pred_tax_cla_02sup_val = rep("Superclass1", 3),
#     feature_pred_tax_cla_03cla_val = rep("Class3", 3),
#     feature_pred_tax_cla_04dirpar_val = rep("Parent1", 3)
#   )
#
#   features <- tidytable::tidytable(feature_id = "F1")
#
#   result <- minimize_results(
#     df = df,
#     features_table = features,
#     best_percentile = 0.9
#   )
#
#   expect_true(
#     "candidates_evaluated" %in%
#       names(result) ||
#       "candidates_best" %in% names(result)
#   )
# })

# ==============================================================================
# Test: Edge Cases
# ==============================================================================

# test_that("minimize_results handles all equal scores", {
#   skip_on_cran()
#
#   df <- tidytable::tidytable(
#     feature_id = rep("F1", 3),
#     score_weighted_chemo = c(0.8, 0.8, 0.8),
#     feature_pred_tax_npc_01pat_val = rep("Alkaloids", 3),
#     feature_pred_tax_npc_02sup_val = rep("Terpenoids", 3),
#     feature_pred_tax_npc_03cla_val = rep("Class1", 3),
#     feature_pred_tax_cla_01kin_val = rep("Kingdom1", 3),
#     feature_pred_tax_cla_02sup_val = rep("Superclass1", 3),
#     feature_pred_tax_cla_03cla_val = rep("Class3", 3),
#     feature_pred_tax_cla_04dirpar_val = rep("Parent1", 3)
#   )
#
#   features <- tidytable::tidytable(feature_id = "F1")
#
#   result <- minimize_results(
#     df = df,
#     features_table = features,
#     best_percentile = 0.9
#   )
#
#   # All scores are equal, so all should pass the percentile filter
#   expect_equal(nrow(result), 3)
# })

# test_that("minimize_results handles NA scores", {
#   skip_on_cran()
#
#   df <- tidytable::tidytable(
#     feature_id = rep("F1", 3),
#     score_weighted_chemo = c(1.0, NA, 0.8),
#     feature_pred_tax_npc_01pat_val = rep("Alkaloids", 3),
#     feature_pred_tax_npc_02sup_val = rep("Terpenoids", 3),
#     feature_pred_tax_npc_03cla_val = rep("Class1", 3),
#     feature_pred_tax_cla_01kin_val = rep("Kingdom1", 3),
#     feature_pred_tax_cla_02sup_val = rep("Superclass1", 3),
#     feature_pred_tax_cla_03cla_val = rep("Class3", 3),
#     feature_pred_tax_cla_04dirpar_val = rep("Parent1", 3)
#   )
#
#   features <- tidytable::tidytable(feature_id = "F1")
#
#   result <- minimize_results(
#     df = df,
#     features_table = features,
#     best_percentile = 0.9
#   )
#
#   expect_s3_class(result, "data.frame")
# })
