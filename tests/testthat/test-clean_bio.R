# ==============================================================================
# Test Suite: clean_bio
# ==============================================================================

edges_table <- tidytable::tidytable(feature_id = character())

# test_that("clean_bio handles empty data frame", {
#   result <- clean_bio(tidytable::tidytable())
#   expect_s3_class(result, "data.frame")
#   expect_equal(nrow(result), 0L)
# })

edges_table <- tidytable::tidytable(feature_id = c("Gentiana lutea"))

# test_that("clean_bio removes duplicate taxonomy rows", {
#   df <- tidytable::tidytable(
#     organism_name = c("Gentiana lutea", "Gentiana lutea"),
#     organism_taxonomy_01domain = c("Eukaryota", "Eukaryota")
#   )
#   result <- clean_bio(df)
#   expect_lte(nrow(result), 2L)
# })

test_that("clean_bio validates minimal_consistency range", {
  edges_table <- tidytable::tidytable(
    feature_source = character(),
    feature_target = character()
  )
  annot_table_wei_bio <- tidytable::tidytable()
  expect_error(
    clean_bio(
      annot_table_wei_bio = annot_table_wei_bio,
      edges_table = edges_table,
      minimal_consistency = -0.1
    ),
    "between 0 and 1"
  )
  expect_error(
    clean_bio(
      annot_table_wei_bio = annot_table_wei_bio,
      edges_table = edges_table,
      minimal_consistency = 1.1
    ),
    "between 0 and 1"
  )
})

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
