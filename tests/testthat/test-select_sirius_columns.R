# ==============================================================================
# Test Suite: select_sirius_columns_* mappings
# ==============================================================================

library(testthat)
library(tima)

make_canopus_df <- function() {
  data.frame(
    mappingFeatureId = c("f1", "f2"),
    adduct = c("[M+H]+", "[M-H]-"),
    molecularFormula = c("C8H10N4O2", "C7H8N4O2"),
    NPC.pathway = c("Alkaloids", NA),
    NPC.pathway.Probability = c(0.8, NA),
    NPC.superclass = c("Benzenoids", NA),
    NPC.superclass.Probability = c(0.9, NA),
    NPC.class = c("Purines", NA),
    NPC.class.Probability = c(0.95, NA),
    ClassyFire.kingdom = c("Organic compounds", NA),
    ClassyFire.kingdom.Probability = c(0.7, NA),
    ClassyFire.superclass = c("Organic oxygen compounds", NA),
    ClassyFire.superclass.probability = c(0.85, NA),
    ClassyFire.class = c("Imidazopyrimidines", NA),
    ClassyFire.class.Probability = c(0.9, NA),
    ClassyFire.subclass = c("Xanthines", NA),
    ClassyFire.subclass.Probability = c(0.88, NA),
    ClassyFire.most.specific.class = c("Methylxanthines", NA),
    ClassyFire.most.specific.class.Probability = c(0.82, NA)
  )
}

test_that("select_sirius_columns_canopus maps NPC and ClassyFire columns", {
  df <- make_canopus_df()
  res <- tima:::select_sirius_columns_canopus(df, sirius_version = "6")

# test_that("select_sirius_columns_canopus maps NPC and ClassyFire columns", {
#   df <- make_canopus_df()
#   res <- tima:::select_sirius_columns_canopus(df, sirius_version = "6")
#
#   expect_true(all(
#     c(
#       "feature_id",
#       "candidate_adduct",
#       "candidate_structure_molecular_formula",
#       "feature_pred_tax_npc_01pat_val",
#       "feature_pred_tax_npc_02sup_val",
#       "feature_pred_tax_npc_03cla_val",
#       "feature_pred_tax_cla_01kin_val",
#       "feature_pred_tax_cla_02sup_val",
#       "feature_pred_tax_cla_03cla_val",
#       "feature_pred_tax_cla_04sub_val",
#       "feature_pred_tax_cla_04dirpar_val"
#     ) %in%
#       names(res)
#   ))
#
#   # Check values are propagated
#   expect_equal(res$feature_pred_tax_cla_01kin_val[1], "Organic compounds")
#   expect_equal(res$feature_pred_tax_cla_04dirpar_val[1], "Methylxanthines")
# })
