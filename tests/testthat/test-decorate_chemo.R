# Test Suite: decorate_chemo ----

library(testthat)

test_that("decorate_chemo returns input unchanged", {
  test_df <- tidytable::tidytable(
    feature_id = c("FT001", "FT002"),
    score_chemical = c(0.8, 0.9),
    candidate_structure_inchikey_connectivity_layer = c("AAAAA", "BBBBB"),
    feature_pred_tax_cla_01kin_val = c("Organic", "Organic"),
    feature_pred_tax_cla_02sup_val = c("Alkaloids", "Terpenoids"),
    feature_pred_tax_cla_03cla_val = c("Class1", "Class2"),
    feature_pred_tax_cla_04dirpar_val = c("Parent1", "Parent2"),
    feature_pred_tax_npc_01pat_val = c("Path1", "Path2"),
    feature_pred_tax_npc_02sup_val = c("Super1", "Super2"),
    feature_pred_tax_npc_03cla_val = c("NPC1", "NPC2")
  )

  result <- decorate_chemo(
    annot_table_wei_chemo = test_df,
    score_chemical_cla_kingdom = 0.1,
    score_chemical_cla_superclass = 0.2,
    score_chemical_cla_class = 0.3,
    score_chemical_cla_parent = 0.4,
    score_chemical_npc_pathway = 0.5,
    score_chemical_npc_superclass = 0.6,
    score_chemical_npc_class = 0.7
  )

  # Should return input unchanged (decorator pattern)
  expect_identical(result, test_df)
})

test_that("decorate_chemo warns on missing columns", {
  incomplete_df <- tidytable::tidytable(
    feature_id = c("FT001")
  )

  expect_warning(
    decorate_chemo(
      annot_table_wei_chemo = incomplete_df,
      score_chemical_cla_kingdom = 0.1,
      score_chemical_cla_superclass = 0.2,
      score_chemical_cla_class = 0.3,
      score_chemical_cla_parent = 0.4,
      score_chemical_npc_pathway = 0.5,
      score_chemical_npc_superclass = 0.6,
      score_chemical_npc_class = 0.7
    ),
    NA
  )
})

# test_that(
#   skip("Not implemented")
# )
# test_that("decorate_chemo handles empty input", {
#   # Use fixture utilities
#   empty_df <- create_empty_table("annotation")
#   empty_df$score_chemical <- numeric(0)
#
#   result <- decorate_chemo(
#     annot_table_wei_chemo = empty_df,
#     score_chemical_cla_kingdom = 0.1,
#     score_chemical_cla_superclass = 0.2,
#     score_chemical_cla_class = 0.3,
#     score_chemical_cla_parent = 0.4,
#     score_chemical_npc_pathway = 0.5,
#     score_chemical_npc_superclass = 0.6,
#     score_chemical_npc_class = 0.7
#   )
#
#   expect_s3_class(result, "data.frame")
#   expect_equal(nrow(result), 0)
# })
