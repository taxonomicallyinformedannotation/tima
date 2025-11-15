# ==============================================================================
# Test Suite: select_sirius_columns_structures
# ==============================================================================

library(testthat)
library(tima)

test_that("select_sirius_columns_structures selects expected structure columns", {
  df <- tidytable::tidytable(
    adduct = c("[M+H]+"),
    name = c("Caffeine"),
    smiles = c("Cn1cnc2c1c(=O)n(C)c(=O)n2C"),
    InChIkey2D = c("RYYVLZVUVIJVGH"),
    molecularFormula = c("C8H10N4O2"),
    xlogp = c("-0.1"),
    ConfidenceScore = c("0.95"),
    `CSI.FingerIDScore` = c("0.80"),
    ModelScore = c("0.75")
  )
  res <- tima:::select_sirius_columns_structures(df, sirius_version = "5")
  expect_true("candidate_structure_name" %in% names(res))
  expect_true("candidate_score_sirius_confidence" %in% names(res))
  expect_true("candidate_score_sirius_csi" %in% names(res))
  expect_true("candidate_score_sirius_msnovelist" %in% names(res))
})
