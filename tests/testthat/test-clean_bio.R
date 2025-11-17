# ==============================================================================
# Test Suite: clean_bio
# ==============================================================================

# Empty input returns empty ----
# test_that("clean_bio returns early with empty annotation table", {
#   annot_table_wei_bio <- create_empty_table("annotation")
#   edges_table <- create_empty_table("edges")
#
#   result <- clean_bio(
#     annot_table_wei_bio = annot_table_wei_bio,
#     edges_table = edges_table,
#     minimal_consistency = 0.5
#   )
#
#   expect_s3_class(result, "data.frame")
#   expect_equal(nrow(result), 0L)
# })

# Parameter validation ----
# test_that("clean_bio validates minimal_consistency range", {
#   annot_table_wei_bio <- create_empty_table("annotation")
#   edges_table <- create_empty_table("edges")
#
#   expect_error(
#     clean_bio(
#       annot_table_wei_bio = annot_table_wei_bio,
#       edges_table = edges_table,
#       minimal_consistency = -0.1
#     ),
#     "between 0 and 1"
#   )
#   expect_error(
#     clean_bio(
#       annot_table_wei_bio = annot_table_wei_bio,
#       edges_table = edges_table,
#       minimal_consistency = 1.1
#     ),
#     "between 0 and 1"
#   )
# })

# Default columns when no valid edges ----
test_that("clean_bio adds default columns when no valid edges", {
  annot_table_wei_bio <- tidytable::tidytable(
    feature_id = "F1",
    candidate_structure_inchikey_connectivity_layer = "INK1",
    candidate_structure_tax_cla_01kin = NA_character_,
    candidate_structure_tax_npc_01pat = NA_character_,
    candidate_structure_tax_cla_02sup = NA_character_,
    candidate_structure_tax_npc_02sup = NA_character_,
    candidate_structure_tax_cla_03cla = NA_character_,
    candidate_structure_tax_npc_03cla = NA_character_,
    candidate_structure_tax_cla_04dirpar = NA_character_,
    score_weighted_bio = 0.5
  )

  # Edges yield <2 neighbors
  edges_table <- tidytable::tidytable(
    feature_source = "F1",
    feature_target = "F1",
    feature_spectrum_entropy = 0,
    label = NA
  )

  result <- clean_bio(
    annot_table_wei_bio = annot_table_wei_bio,
    edges_table = edges_table,
    minimal_consistency = 0.5
  )

  expect_true("feature_pred_tax_cla_01kin_val" %in% names(result))
  expect_equal(result$feature_pred_tax_cla_01kin_val, "empty")
})
