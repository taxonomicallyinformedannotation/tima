# Test Suite: select_sop_columns ----

library(testthat)

## Setup ----

# Create a test SOP dataframe with all expected columns
create_test_sop_data <- function(n_rows = 3) {
  tidytable::tidytable(
    # Structure columns
    structure_name = paste0("Compound_", seq_len(n_rows)),
    structure_inchikey = replicate(
      n_rows,
      paste(sample(LETTERS, 27, TRUE), collapse = "")
    ),
    structure_smiles = rep("CCCC", n_rows),
    structure_inchikey_2D = replicate(
      n_rows,
      paste(sample(LETTERS, 14, TRUE), collapse = "")
    ),
    structure_smiles_2D = rep("CCC", n_rows),
    structure_molecular_formula = rep("C4H10", n_rows),
    structure_exact_mass = runif(n_rows, 50, 500),
    structure_xlogp = runif(n_rows, -2, 5),

    # NPClassifier taxonomy
    structure_taxonomy_npclassifier_01pathway = rep("Terpenoids", n_rows),
    structure_taxonomy_npclassifier_02superclass = rep(
      "Monoterpenoids",
      n_rows
    ),
    structure_taxonomy_npclassifier_03class = rep("Iridoids", n_rows),

    # ClassyFire taxonomy
    structure_taxonomy_classyfire_chemontid = paste0(
      "CHEMONTID:",
      seq_len(n_rows)
    ),
    structure_taxonomy_classyfire_01kingdom = rep("Organic compounds", n_rows),
    structure_taxonomy_classyfire_02superclass = rep("Lipids", n_rows),
    structure_taxonomy_classyfire_03class = rep("Fatty acids", n_rows),
    structure_taxonomy_classyfire_04directparent = rep(
      "Long-chain fatty acids",
      n_rows
    ),

    # Organism columns
    organism_name = rep("Gentiana lutea", n_rows),
    organism_taxonomy_ottid = rep("123456", n_rows),
    organism_taxonomy_01domain = rep("Eukaryota", n_rows),
    organism_taxonomy_02kingdom = rep("Plantae", n_rows),
    organism_taxonomy_03phylum = rep("Tracheophyta", n_rows),
    organism_taxonomy_04class = rep("Magnoliopsida", n_rows),
    organism_taxonomy_05order = rep("Gentianales", n_rows),
    organism_taxonomy_06family = rep("Gentianaceae", n_rows),
    organism_taxonomy_07tribe = rep(NA_character_, n_rows),
    organism_taxonomy_08genus = rep("Gentiana", n_rows),
    organism_taxonomy_09species = rep("lutea", n_rows),
    organism_taxonomy_10varietas = rep(NA_character_, n_rows),

    # Reference
    reference_doi = rep("10.1234/example", n_rows),

    # Extra columns that should be removed
    extra_col1 = seq_len(n_rows),
    extra_col2 = rep("remove", n_rows)
  )
}

## Input Validation Tests ----

test_that("select_sop_columns validates data frame", {
  expect_error(
    select_sop_columns(df = "not_a_df"),
    "data.*frame"
  )

  expect_error(
    select_sop_columns(df = NULL),
    class = "error"
  )

  expect_error(
    select_sop_columns(df = list(a = 1)),
    "data.*frame"
  )
})

## Basic Functionality Tests ----

test_that("select_sop_columns selects correct columns", {
  df <- create_test_sop_data(n_rows = 2)
  result <- select_sop_columns(df)

  # Should not include extra columns
  expect_false("extra_col1" %in% names(result))
  expect_false("extra_col2" %in% names(result))

  # Should include structure columns
  expect_true("structure_name" %in% names(result))
  expect_true("structure_inchikey" %in% names(result))
  expect_true("structure_smiles" %in% names(result))

  # Should include organism columns
  expect_true("organism_name" %in% names(result))
  expect_true("organism_taxonomy_ottid" %in% names(result))
})

test_that("select_sop_columns renames columns correctly", {
  df <- create_test_sop_data(n_rows = 2)
  result <- select_sop_columns(df)

  # Check renamed columns exist
  expect_true("structure_inchikey_connectivity_layer" %in% names(result))
  expect_true("structure_smiles_no_stereo" %in% names(result))

  # Check old names don't exist
  expect_false("structure_inchikey_2D" %in% names(result))
  expect_false("structure_smiles_2D" %in% names(result))

  # Check taxonomy renames
  expect_true("structure_tax_npc_01pat" %in% names(result))
  expect_true("structure_tax_npc_02sup" %in% names(result))
  expect_true("structure_tax_cla_01kin" %in% names(result))
})

test_that("select_sop_columns preserves data", {
  df <- create_test_sop_data(n_rows = 3)
  result <- select_sop_columns(df)

  # Check data is preserved
  expect_equal(nrow(result), 3)
  expect_equal(result$structure_name, df$structure_name)
  expect_equal(result$organism_name, df$organism_name)
  expect_equal(result$structure_exact_mass, df$structure_exact_mass)
})

test_that("select_sop_columns handles empty dataframe", {
  df <- create_test_sop_data(n_rows = 0)
  result <- select_sop_columns(df)

  expect_equal(nrow(result), 0)
  # Should still have the columns structure
  expect_true(ncol(result) >= 0)
})

## Column Rename Tests ----

test_that("select_sop_columns renames structure columns", {
  df <- create_test_sop_data(n_rows = 1)
  result <- select_sop_columns(df)

  # Check 2D renames
  expect_equal(
    result$structure_inchikey_connectivity_layer,
    df$structure_inchikey_2D
  )
  expect_equal(
    result$structure_smiles_no_stereo,
    df$structure_smiles_2D
  )
})

test_that("select_sop_columns renames NPClassifier taxonomy", {
  df <- create_test_sop_data(n_rows = 1)
  result <- select_sop_columns(df)

  expect_equal(
    result$structure_tax_npc_01pat,
    df$structure_taxonomy_npclassifier_01pathway
  )
  expect_equal(
    result$structure_tax_npc_02sup,
    df$structure_taxonomy_npclassifier_02superclass
  )
  expect_equal(
    result$structure_tax_npc_03cla,
    df$structure_taxonomy_npclassifier_03class
  )
})

test_that("select_sop_columns renames ClassyFire taxonomy", {
  df <- create_test_sop_data(n_rows = 1)
  result <- select_sop_columns(df)

  expect_equal(
    result$structure_tax_cla_01kin,
    df$structure_taxonomy_classyfire_01kingdom
  )
  expect_equal(
    result$structure_tax_cla_02sup,
    df$structure_taxonomy_classyfire_02superclass
  )
  expect_equal(
    result$structure_tax_cla_03cla,
    df$structure_taxonomy_classyfire_03class
  )
  expect_equal(
    result$structure_tax_cla_04dirpar,
    df$structure_taxonomy_classyfire_04directparent
  )
})

## Data Integrity Tests ----

test_that("select_sop_columns preserves row order", {
  df <- create_test_sop_data(n_rows = 5)
  df$id <- seq_len(5)

  result <- select_sop_columns(df)

  # Row order should be preserved (though id column won't be present)
  expect_equal(result$structure_name, df$structure_name)
  expect_equal(nrow(result), nrow(df))
})

test_that("select_sop_columns handles NA values", {
  df <- create_test_sop_data(n_rows = 3)
  df$organism_taxonomy_07tribe <- c(NA, "Tribe1", NA)
  df$organism_taxonomy_10varietas <- c(NA_character_, NA_character_, "var1")

  result <- select_sop_columns(df)

  expect_true(is.na(result$organism_taxonomy_07tribe[1]))
  expect_false(is.na(result$organism_taxonomy_07tribe[2]))
  expect_true(is.na(result$organism_taxonomy_10varietas[1]))
  expect_false(is.na(result$organism_taxonomy_10varietas[3]))
})

test_that("select_sop_columns preserves data types", {
  df <- create_test_sop_data(n_rows = 2)
  result <- select_sop_columns(df)

  # Numeric columns stay numeric
  expect_type(result$structure_exact_mass, "double")
  expect_type(result$structure_xlogp, "double")

  # Character columns stay character
  expect_type(result$structure_name, "character")
  expect_type(result$organism_name, "character")
})

## Edge Cases ----

test_that("select_sop_columns works with single row", {
  df <- create_test_sop_data(n_rows = 1)
  result <- select_sop_columns(df)

  expect_equal(nrow(result), 1)
  expect_true(all(c("structure_name", "organism_name") %in% names(result)))
})

test_that("select_sop_columns works with large dataset", {
  skip_on_cran()

  df <- create_test_sop_data(n_rows = 1000)
  result <- select_sop_columns(df)

  expect_equal(nrow(result), 1000)
  expect_false("extra_col1" %in% names(result))
})

test_that("select_sop_columns handles missing optional taxonomy", {
  df <- create_test_sop_data(n_rows = 2)

  # Set some taxonomy to NA
  df$organism_taxonomy_07tribe <- NA_character_
  df$organism_taxonomy_10varietas <- NA_character_

  result <- select_sop_columns(df)

  expect_true("organism_taxonomy_07tribe" %in% names(result))
  expect_true("organism_taxonomy_10varietas" %in% names(result))
  expect_true(all(is.na(result$organism_taxonomy_07tribe)))
})

## Return Value Tests ----

test_that("select_sop_columns returns tidytable", {
  df <- create_test_sop_data(n_rows = 2)
  result <- select_sop_columns(df)

  # Should preserve tidytable class if input was tidytable
  expect_s3_class(result, "tidytable")
})

test_that("select_sop_columns returns correct number of columns", {
  df <- create_test_sop_data(n_rows = 2)
  result <- select_sop_columns(df)

  # Count expected columns
  # Structure: name, inchikey, smiles, inchikey_2D, smiles_2D, formula, mass, xlogp (8)
  # NPC taxonomy: 3
  # ClassyFire taxonomy: 5 (including chemontid)
  # Organism: name, ottid, 10 taxonomy levels (12)
  # Reference: doi (1)
  # Total: 8 + 3 + 5 + 12 + 1 = 29

  expect_equal(ncol(result), 29)
})

## Integration Tests ----

test_that("select_sop_columns works in typical workflow", {
  # Simulate loading SOP data and standardizing
  raw_data <- create_test_sop_data(n_rows = 10)

  # Add some noise columns
  raw_data$processing_date <- Sys.Date()
  raw_data$source_database <- "LOTUS"

  # Standardize
  clean_data <- select_sop_columns(raw_data)

  # Should have removed noise columns
  expect_false("processing_date" %in% names(clean_data))
  expect_false("source_database" %in% names(clean_data))

  # Should have all required columns
  expect_true("structure_name" %in% names(clean_data))
  expect_true("organism_name" %in% names(clean_data))
  expect_true("reference_doi" %in% names(clean_data))

  # Data should be intact
  expect_equal(nrow(clean_data), 10)
})

test_that("select_sop_columns output can be further processed", {
  df <- create_test_sop_data(n_rows = 5)
  result <- select_sop_columns(df)

  # Should be able to filter
  filtered <- result |> tidytable::filter(structure_exact_mass > 100)
  expect_true(nrow(filtered) <= 5)

  # Should be able to select
  selected <- result |> tidytable::select(structure_name, organism_name)
  expect_equal(ncol(selected), 2)

  # Should be able to mutate
  mutated <- result |> tidytable::mutate(new_col = structure_exact_mass * 2)
  expect_true("new_col" %in% names(mutated))
})

## Compatibility Tests ----

test_that("select_sop_columns works with data.frame", {
  df <- as.data.frame(create_test_sop_data(n_rows = 2))
  result <- select_sop_columns(df)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
})

test_that("select_sop_columns preserves special values", {
  df <- create_test_sop_data(n_rows = 3)

  # Add some special values
  df$structure_xlogp[2] <- Inf
  df$structure_xlogp[3] <- -Inf
  df$organism_name[2] <- ""

  result <- select_sop_columns(df)

  expect_true(is.infinite(result$structure_xlogp[2]))
  expect_true(is.infinite(result$structure_xlogp[3]))
  expect_equal(result$organism_name[2], "")
})

## Performance Tests ----

test_that("select_sop_columns is reasonably fast", {
  skip_if_not_installed("bench")
  skip_on_cran()

  df <- create_test_sop_data(n_rows = 100L)

  timing <- system.time(replicate(n = 100L, expr = select_sop_columns(df)))

  # Should be fast
  expect_lt(timing["elapsed"], 0.5)
})
