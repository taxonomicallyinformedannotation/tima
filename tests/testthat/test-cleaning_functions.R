# Test: Cleaning Functions (Bio & Chemo)
library(testthat)

# =============================================================================
# Tests for clean_bio()
# =============================================================================

# test_that("clean_bio returns early with empty annotation table", {
#   # Use fixture utilities for clean, reusable empty structures
#   empty_annotations <- create_empty_table("annotation")
#   empty_edges <- create_empty_table("edges")
#
#   result <- clean_bio(
#     annot_table_wei_bio = empty_annotations,
#     edges_table = empty_edges,
#     minimal_consistency = 0.5
#   )
#
#   expect_s3_class(result, "data.frame")
#   expect_equal(nrow(result), 0)
# })

# test_that("clean_bio returns early with empty edges table", {
#   local_test_project(copy = TRUE)
#
#   annotations <- tidytable::tidytable(
#     feature_id = c("FT001", "FT002"),
#     candidate_structure_inchikey_connectivity_layer = c("AAAAA", "BBBBB"),
#     candidate_structure_tax_cla_01kin = c("Organic compounds", "Organic compounds"),
#     candidate_structure_tax_npc_01pat = c("Alkaloids", "Terpenoids"),
#     candidate_structure_tax_cla_02sup = c("Alkaloids", "Terpenoids"),
#     candidate_structure_tax_npc_02sup = c("Alkaloids", "Terpenoids"),
#     candidate_structure_tax_cla_03cla = c("Alkaloids", "Terpenoids"),
#     candidate_structure_tax_npc_03cla = c("Alkaloids", "Terpenoids"),
#     candidate_structure_tax_cla_04dirpar = c("Alkaloids", "Terpenoids"),
#     score_weighted_bio = c(0.8, 0.9)
#   )
#
#   empty_edges <- tidytable::tidytable(
#     feature_source = character(0),
#     feature_target = character(0)
#   )
#
#   result <- clean_bio(
#     annot_table_wei_bio = annotations,
#     edges_table = empty_edges,
#     minimal_consistency = 0.5
#   )
#
#   # Should return input with default columns added
#   expect_s3_class(result, "data.frame")
#   expect_true("feature_pred_tax_cla_01kin_val" %in% colnames(result))
#   expect_true(all(result$feature_pred_tax_cla_01kin_val == "empty"))
#
#
# })

# test_that("clean_bio adds required columns when no valid edges", {
#   local_test_project(copy = TRUE)
#
#   annotations <- tidytable::tidytable(
#     feature_id = c("FT001"),
#     candidate_structure_inchikey_connectivity_layer = c("AAAAA"),
#     candidate_structure_tax_cla_01kin = c("Organic compounds"),
#     candidate_structure_tax_npc_01pat = c("Alkaloids"),
#     candidate_structure_tax_cla_02sup = c("Alkaloids"),
#     candidate_structure_tax_npc_02sup = c("Alkaloids"),
#     candidate_structure_tax_cla_03cla = c("Alkaloids"),
#     candidate_structure_tax_npc_03cla = c("Alkaloids"),
#     candidate_structure_tax_cla_04dirpar = c("Alkaloids"),
#     score_weighted_bio = c(0.8)
#   )
#
#   # Edges with only 1 neighbor (won't pass >=2 filter)
#   edges <- tidytable::tidytable(
#     feature_source = c("FT001"),
#     feature_target = c("FT001"),
#     feature_spectrum_entropy = c(1.5)
#   )
#
#   result <- clean_bio(
#     annot_table_wei_bio = annotations,
#     edges_table = edges,
#     minimal_consistency = 0.5
#   )
#
#   # Should add all required columns with defaults
#   expect_true("feature_pred_tax_cla_01kin_val" %in% colnames(result))
#   expect_true("consistency_structure_cla_kin" %in% colnames(result))
#   expect_true("feature_pred_tax_cla_01kin_score" %in% colnames(result))
#
#
# })

# =============================================================================
# Tests for clean_chemo()
# =============================================================================

# test_that("clean_chemo returns early with empty annotation table", {
#   # Use fixture utilities for empty structure
#   empty_annotations <- create_empty_table("annotation")
#
#   result <- clean_chemo(
#     annot_table_wei_chemo = empty_annotations,
#     candidates_final = 1L,
#     score_chemical_cla_kingdom = 0.5,
#     score_chemical_cla_superclass = 0.5,
#     score_chemical_cla_class = 0.5,
#     score_chemical_cla_parent = 0.5,
#     score_chemical_npc_pathway = 0.5,
#     score_chemical_npc_superclass = 0.5,
#     score_chemical_npc_class = 0.5
#   )
#
#   expect_s3_class(result, "data.frame")
#   expect_equal(nrow(result), 0)
# })

# test_that("clean_chemo validates candidates_final parameter", {
#   annotations <- create_minimal_data("annotation", n_rows = 3)
#
#   # Add required columns for clean_chemo
#   annotations$score_chemical <- c(0.9, 0.7, 0.5)
#   annotations$feature_pred_tax_cla_01kin_val <- rep("Organic", 3)
#   annotations$feature_pred_tax_cla_02sup_val <- rep("Alkaloids", 3)
#   annotations$feature_pred_tax_cla_03cla_val <- c("Class1", "Class2", "Class3")
#   annotations$feature_pred_tax_cla_04dirpar_val <- c("Parent1", "Parent2", "Parent3")
#   annotations$feature_pred_tax_npc_01pat_val <- c("Path1", "Path2", "Path3")
#   annotations$feature_pred_tax_npc_02sup_val <- c("Super1", "Super2", "Super3")
#   annotations$feature_pred_tax_npc_03cla_val <- c("NPC1", "NPC2", "NPC3")
#
#   # Should error with candidates_final = 0
#   expect_error(
#     clean_chemo(
#       annot_table_wei_chemo = annotations,
#       candidates_final = 0L,
#       score_chemical_cla_kingdom = 0.1,
#       score_chemical_cla_superclass = 0.1,
#       score_chemical_cla_class = 0.1,
#       score_chemical_cla_parent = 0.1,
#       score_chemical_npc_pathway = 0.1,
#       score_chemical_npc_superclass = 0.1,
#       score_chemical_npc_class = 0.1
#     ),
#     "positive"
#   )
# })

# test_that("clean_chemo filters candidates correctly", {
#   skip_on_cran()
#   local_test_project(copy = TRUE)
#
#   annotations <- tidytable::tidytable(
#     feature_id = c("FT001", "FT001", "FT001"),
#     candidate_structure_inchikey_connectivity_layer = c("AAAAA", "BBBBB", "CCCCC"),
#     score_chemical = c(0.9, 0.7, 0.5),
#     feature_pred_tax_cla_01kin_val = c("Organic", "Organic", "Organic"),
#     feature_pred_tax_cla_02sup_val = c("Alkaloids", "Alkaloids", "Alkaloids"),
#     feature_pred_tax_cla_03cla_val = c("Class1", "Class2", "Class3"),
#     feature_pred_tax_cla_04dirpar_val = c("Parent1", "Parent2", "Parent3"),
#     feature_pred_tax_npc_01pat_val = c("Path1", "Path2", "Path3"),
#     feature_pred_tax_npc_02sup_val = c("Super1", "Super2", "Super3"),
#     feature_pred_tax_npc_03cla_val = c("NPC1", "NPC2", "NPC3")
#   )
#
#   result <- clean_chemo(
#     annot_table_wei_chemo = annotations,
#     candidates_final = 2L, # Keep top 2
#     score_chemical_cla_kingdom = 0.1,
#     score_chemical_cla_superclass = 0.1,
#     score_chemical_cla_class = 0.1,
#     score_chemical_cla_parent = 0.1,
#     score_chemical_npc_pathway = 0.1,
#     score_chemical_npc_superclass = 0.1,
#     score_chemical_npc_class = 0.1
#   )
#
#   expect_s3_class(result, "data.frame")
#   expect_true(nrow(result) <= 2) # Should keep top 2 candidates
#
#
# })

# =============================================================================
# Tests for validation helper
# =============================================================================

# test_that("clean functions handle invalid input gracefully", {
#   expect_error(
#     clean_bio(
#       annot_table_wei_bio = "not a dataframe",
#       edges_table = tidytable::tidytable(),
#       minimal_consistency = 0.5
#     ),
#     "must be a data frame"
#   )
#
#   expect_error(
#     clean_chemo(
#       annot_table_wei_chemo = "not a dataframe",
#       candidates_final = 1L,
#       score_chemical_cla_kingdom = 0.5,
#       score_chemical_cla_superclass = 0.5,
#       score_chemical_cla_class = 0.5,
#       score_chemical_cla_parent = 0.5,
#       score_chemical_npc_pathway = 0.5,
#       score_chemical_npc_superclass = 0.5,
#       score_chemical_npc_class = 0.5
#     ),
#     "must be a data frame"
#   )
# })
