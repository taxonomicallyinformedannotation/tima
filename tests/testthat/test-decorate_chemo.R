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

test_that("decorate_chemo logs both candidate and unique structure counts", {
  test_df <- tidytable::tidytable(
    feature_id = c("FT001", "FT002"),
    score_chemical = c(0.8, 0.8),
    candidate_structure_inchikey_connectivity_layer = c("AAAAA", "AAAAA"),
    feature_pred_tax_cla_01kin_val = c("Organic", "Organic"),
    feature_pred_tax_cla_02sup_val = c("Alkaloids", "Alkaloids"),
    feature_pred_tax_cla_03cla_val = c("Class1", "Class1"),
    feature_pred_tax_cla_04dirpar_val = c("Parent1", "Parent1"),
    feature_pred_tax_npc_01pat_val = c("Path1", "Path1"),
    feature_pred_tax_npc_02sup_val = c("Super1", "Super1"),
    feature_pred_tax_npc_03cla_val = c("NPC1", "NPC1")
  )

  log_file <- withr::local_tempfile(fileext = ".log")
  setup_logger(filename = log_file, threshold = 400)

  decorate_chemo(
    annot_table_wei_chemo = test_df,
    score_chemical_cla_kingdom = 0.7,
    score_chemical_cla_superclass = 0.7,
    score_chemical_cla_class = 0.7,
    score_chemical_cla_parent = 0.7,
    score_chemical_npc_pathway = 0.7,
    score_chemical_npc_superclass = 0.7,
    score_chemical_npc_class = 0.7
  )

  logged <- paste(readLines(log_file, warn = FALSE), collapse = "\n")

  expect_true(grepl("candidates", logged, fixed = TRUE))
  expect_true(grepl("unique", logged, fixed = TRUE))
  expect_true(
    grepl("Kingdom level:\\s+2 candidates \\(1 unique\\)", logged)
  )
})

test_that("decorate_chemo cascades ClassyFire and NPClassifier filters independently and excludes notAnnotated/notConsistent", {
  test_df <- tidytable::tidytable(
    feature_id = c("FT1", "FT2", "FT3", "FT4"),
    score_chemical = c(0.9, 0.5, 0.9, 0.9),
    candidate_structure_inchikey_connectivity_layer = c(
      "IK1",
      "IK2",
      "IK3",
      "IK1"
    ),
    feature_pred_tax_cla_01kin_val = c(
      "Organic",
      "Organic",
      "notAnnotated",
      "Organic"
    ),
    feature_pred_tax_cla_02sup_val = c(
      "Alkaloids",
      "Alkaloids",
      "Alkaloids",
      "notConsistent"
    ),
    feature_pred_tax_cla_03cla_val = c("Class1", "Class1", "Class1", "Class1"),
    feature_pred_tax_cla_04dirpar_val = c("Par1", "Par1", "Par1", "Par1"),
    feature_pred_tax_npc_01pat_val = c("Path1", "Path1", "Path1", "Path1"),
    feature_pred_tax_npc_02sup_val = c(
      "Super1",
      "Super1",
      "notConsistent",
      "Super1"
    ),
    feature_pred_tax_npc_03cla_val = c("NPC1", "NPC1", "NPC1", NA_character_)
  )

  log_file <- withr::local_tempfile(fileext = ".log")
  setup_logger(filename = log_file, threshold = 400)

  res <- decorate_chemo(
    annot_table_wei_chemo = test_df,
    score_chemical_cla_kingdom = 0.5,
    score_chemical_cla_superclass = 0.8,
    score_chemical_cla_class = 0.8,
    score_chemical_cla_parent = 0.8,
    score_chemical_npc_pathway = 0.5,
    score_chemical_npc_superclass = 0.5,
    score_chemical_npc_class = 0.5
  )

  expect_identical(res, test_df)
  logged <- paste(readLines(log_file, warn = FALSE), collapse = "\n")
  # ClassyFire:
  # Kingdom (>=0.5, valid): FT1 (IK1), FT2 (IK2), FT4 (IK1) -> 3 candidates (2 unique)
  # Superclass (>=0.8, valid, from Kingdom): FT1 (IK1) -> 1 candidates (1 unique)
  expect_true(grepl("Kingdom level:\\s+3 candidates \\(2 unique\\)", logged))
  expect_true(grepl("Superclass level:\\s+1 candidates \\(1 unique\\)", logged))
  # NPClassifier:
  # Pathway (>=0.5): FT1, FT2, FT3, FT4 -> 4 candidates (3 unique)
  # Superclass (>=0.5, valid): FT1, FT2, FT4 -> 3 candidates (2 unique)
  # Class (>=0.5, valid): FT1, FT2 -> 2 candidates (2 unique)
  expect_true(grepl("Pathway level:\\s+4 candidates \\(3 unique\\)", logged))
  expect_true(grepl("Class level:\\s+2 candidates \\(2 unique\\)", logged))
})
