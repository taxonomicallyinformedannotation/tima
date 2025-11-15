# ==============================================================================
# Test Suite: clean_chemo
# ==============================================================================
library(testthat)
library(tima)

candidates_final <- tidytable::tidytable(candidate_structure_inchikey_connectivity_layer = character())

test_that("clean_chemo handles empty data frame", {
  result <- clean_chemo(tidytable::tidytable())
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
})

test_that("clean_chemo processes minimal chemical class columns", {
  df <- tidytable::tidytable(
    structure_inchikey_connectivity_layer = c("A","B"),
    structure_classyfire_class = c("Alkaloids","Flavonoids")
  )
  candidates_final <- tidytable::tidytable(candidate_structure_inchikey_connectivity_layer = c("A","B"))
  result <- clean_chemo(df)
  expect_true("structure_inchikey_connectivity_layer" %in% names(result))
})
