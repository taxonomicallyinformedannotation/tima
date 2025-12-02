# Test Suite: split_tables_sop ----

library(testthat)

## Input validation ----

test_that("split_tables_sop validates data frame input", {
  # Non-data frame should error
  expect_error(
    split_tables_sop(table = "not a dataframe", cache = NULL),
    "data frame or tibble"
  )

  expect_error(
    split_tables_sop(table = list(a = 1), cache = NULL),
    "data frame or tibble"
  )
})

test_that("split_tables_sop handles empty table gracefully", {
  empty_table <- tidytable::tidytable()

  result <- split_tables_sop(table = empty_table, cache = NULL)

  # Should return list with empty tables
  expect_type(result, "list")
  expect_named(
    result,
    c(
      "table_keys",
      "table_structures_stereo",
      "table_organisms",
      "table_structural"
    )
  )

  # All tables should be empty
  expect_equal(nrow(result$table_keys), 0)
  expect_equal(nrow(result$table_structures_stereo), 0)
  expect_equal(nrow(result$table_organisms), 0)
  expect_equal(nrow(result$table_structural), 0)
})

## Functional Tests ----

test_that("split_tables_sop splits table into components", {
  # Load fixture instead of creating table inline
  sop_table <- load_fixture("sop_table_minimal")

  # Mock process_smiles to return the same structure data
  with_mocked_bindings(
    process_smiles = function(table, cache) {
      table |>
        tidytable::select(
          structure_smiles_initial,
          tidyselect::any_of(
            x = c(
              "structure_inchikey",
              "structure_inchikey_connectivity_layer",
              "structure_smiles",
              "structure_smiles_no_stereo",
              "structure_molecular_formula",
              "structure_exact_mass",
              "structure_xlogp"
            )
          )
        ) |>
        tidytable::distinct()
    },
    {
      result <- split_tables_sop(table = sop_table, cache = NULL)

      # Should return all expected tables
      expect_type(result, "list")
      expect_true(all(
        c(
          "key",
          "org_tax_ott",
          "str_stereo",
          "str_met",
          "str_nam",
          "str_tax_cla",
          "str_tax_npc"
        ) %in%
          names(result)
      ))

      # Keys table should have structure-organism pairs
      expect_s3_class(result$key, "data.frame")
      expect_true("structure_inchikey" %in% colnames(result$key))
      expect_true("organism_name" %in% colnames(result$key))
      expect_equal(nrow(result$key), 2)

      # Structures stereo should have stereochemistry
      expect_s3_class(result$str_stereo, "data.frame")
      expect_true("structure_inchikey" %in% colnames(result$str_stereo))
      expect_true("structure_smiles_no_stereo" %in% colnames(result$str_stereo))

      # Organisms table should have taxonomy
      expect_s3_class(result$org_tax_ott, "data.frame")
      expect_true("organism_name" %in% colnames(result$org_tax_ott))
    }
  )
})

test_that("split_tables_sop handles missing SMILES gracefully", {
  # Load fixture with missing SMILES instead of creating inline
  sop_table <- load_fixture("sop_table_missing_smiles")

  with_mocked_bindings(
    process_smiles = function(table, cache) {
      table |>
        tidytable::select(
          structure_smiles_initial,
          tidyselect::any_of(
            x = c(
              "structure_inchikey",
              "structure_inchikey_connectivity_layer",
              "structure_smiles",
              "structure_smiles_no_stereo",
              "structure_molecular_formula",
              "structure_exact_mass",
              "structure_xlogp"
            )
          )
        )
    },
    {
      result <- split_tables_sop(table = sop_table, cache = NULL)

      expect_type(result, "list")
      expect_true(nrow(result$key) >= 0)
      expect_true("structure_inchikey" %in% colnames(result$str_stereo))
    }
  )
})

test_that("split_tables_sop preserves unique organisms", {
  # Load fixture with duplicate organisms instead of creating inline
  sop_table <- load_fixture("sop_table_duplicate_organisms")

  with_mocked_bindings(
    process_smiles = function(table, cache) {
      table |>
        tidytable::select(
          structure_smiles_initial,
          tidyselect::any_of(
            x = c(
              "structure_inchikey",
              "structure_inchikey_connectivity_layer",
              "structure_smiles",
              "structure_smiles_no_stereo",
              "structure_molecular_formula",
              "structure_exact_mass",
              "structure_xlogp"
            )
          )
        )
    },
    {
      result <- split_tables_sop(table = sop_table, cache = NULL)

      # Should deduplicate organisms
      unique_orgs <- result$org_tax_ott |>
        tidytable::distinct(organism_name) |>
        nrow()
      expect_equal(unique_orgs, 1)

      # But should have 2 structure-organism pairs
      expect_equal(nrow(result$key), 2)
    }
  )
})

test_that("split_tables_sop handles cache parameter", {
  # Load fixture instead of creating inline
  sop_table <- load_fixture("sop_table_cache_test")

  # With NULL cache
  result_no_cache <- split_tables_sop(table = sop_table, cache = NULL)
  expect_type(result_no_cache, "list")

  # With cache path (file doesn't need to exist)
  cache_file <- file.path(tempdir(), "test_cache.tsv")
  result_with_cache <- split_tables_sop(table = sop_table, cache = cache_file)
  expect_type(result_with_cache, "list")
})

## Edge Cases ----

test_that("split_tables_sop handles special characters in organism names", {
  # Load fixture with special characters instead of creating inline
  sop_table <- load_fixture("sop_table_special_chars")

  result <- split_tables_sop(table = sop_table, cache = NULL)

  expect_type(result, "list")
  expect_true(nrow(result$org_tax_ott) > 0)
})

test_that("split_tables_sop handles missing taxonomy fields", {
  # Load fixture with missing taxonomy instead of creating inline
  sop_table <- load_fixture("sop_table_missing_taxonomy")

  result <- split_tables_sop(table = sop_table, cache = NULL)

  expect_type(result, "list")
  expect_true(nrow(result$org_tax_ott) >= 0)
})
