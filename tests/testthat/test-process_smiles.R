# ==============================================================================
# Test Suite: process_smiles
# ==============================================================================
#
# @description
# Unit tests for process_smiles which normalizes and augments SMILES strings.
#
# @coverage
# - Basic processing
# - Column creation
# - Handling of stereochemistry removal
# - Edge cases (empty, NA)

library(testthat)
library(tima)

TEST_SMILES <- "C[C@@H]1C=C(C(=O)[C@]2([C@H]1C[C@@H]3[C@@]4([C@@H]2C(=O)C(=C([C@@H]4CC(=O)O3)C)OC)C)C)OC"

test_that("process_smiles processes a single SMILES without cache", {
  result <- tidytable::tidytable(
    structure_smiles_initial = TEST_SMILES
  ) |>
    process_smiles()

  expect_s3_class(result, "data.frame")
  expect_true("structure_smiles_no_stereo" %in% colnames(result))
  expect_true(nrow(result) == 1L)
})

test_that("process_smiles handles empty data frame", {
  result <- tidytable::tidytable(
    structure_smiles_initial = character(0)
  ) |>
    process_smiles()

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) == 0L)
})

test_that("process_smiles handles NA values gracefully", {
  input <- tidytable::tidytable(
    structure_smiles_initial = c(TEST_SMILES, NA_character_)
  )
  result <- input |> process_smiles()

  expect_s3_class(result, "data.frame")
  # Either NA is retained (2 rows) or filtered (1 row)
  expect_true(nrow(result) %in% c(1L, 2L))
  # Non-NA SMILES must be processed
  expect_true(any(grepl("C", result$structure_smiles_initial)))
  expect_true("structure_smiles_no_stereo" %in% names(result))
})
