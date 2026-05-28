# Test Suite: split_tables_sop ----

library(testthat)

.mock_process_smiles_fast <- function(table, cache) {
  force(cache)
  table |>
    tidytable::mutate(
      structure_inchikey = tidytable::coalesce(
        structure_inchikey,
        "AAAAAAAAAAAAAA-BBBBBBBBBB-N"
      ),
      structure_inchikey_connectivity_layer = tidytable::coalesce(
        structure_inchikey_connectivity_layer,
        "AAAAAAAAAAAAAA"
      ),
      structure_smiles = tidytable::coalesce(
        structure_smiles,
        structure_smiles_no_stereo,
        structure_smiles_initial
      ),
      structure_smiles_no_stereo = tidytable::coalesce(
        structure_smiles_no_stereo,
        structure_smiles,
        structure_smiles_initial
      ),
      structure_molecular_formula = tidytable::coalesce(
        structure_molecular_formula,
        "C2H6O"
      ),
      structure_exact_mass = tidytable::coalesce(
        structure_exact_mass,
        46.0419
      )
    ) |>
    tidytable::select(
      structure_smiles_initial,
      structure_inchikey,
      structure_inchikey_connectivity_layer,
      structure_smiles,
      structure_smiles_no_stereo,
      tidyselect::any_of(c(
        "structure_molecular_formula",
        "structure_exact_mass",
        "structure_xlogp",
        "structure_tag"
      ))
    ) |>
    tidytable::distinct()
}

## Input validation ----

test_that("split_tables_sop validates data frame input", {
  # Non-data frame should error
  expect_error(
    split_tables_sop(table = "not a dataframe", cache = NULL),
    "Fix: Ensure input is a valid data frame",
    fixed = TRUE
  )

  expect_error(
    split_tables_sop(table = list(a = 1), cache = NULL),
    "Fix: Ensure input is a valid data frame",
    fixed = TRUE
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
      "key",
      "org_tax_ott",
      "str_can",
      "str_stereo",
      "str_met",
      "str_tax_cla",
      "str_tax_npc"
    )
  )

  # All tables should be empty
  expect_equal(nrow(result$key), 0)
  expect_equal(nrow(result$str_stereo), 0)
  expect_equal(nrow(result$org_tax_ott), 0)
  expect_equal(nrow(result$str_met), 0)
  expect_equal(nrow(result$str_can), 0)
  expect_equal(nrow(result$str_tax_cla), 0)
  expect_equal(nrow(result$str_tax_npc), 0)
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
              "structure_xlogp",
              "structure_tag"
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
              "structure_xlogp",
              "structure_tag"
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
              "structure_xlogp",
              "structure_tag"
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

  with_mocked_bindings(
    process_smiles = .mock_process_smiles_fast,
    {
      # With NULL cache
      result_no_cache <- split_tables_sop(table = sop_table, cache = NULL)
      expect_type(result_no_cache, "list")

      # With cache path (file doesn't need to exist)
      cache_file <- file.path(tempdir(), "test_cache.tsv")
      result_with_cache <- split_tables_sop(
        table = sop_table,
        cache = cache_file
      )
      expect_type(result_with_cache, "list")
    }
  )
})

## Edge Cases ----

test_that("split_tables_sop handles special characters in organism names", {
  # Load fixture with special characters instead of creating inline
  sop_table <- load_fixture("sop_table_special_chars")

  result <- with_mocked_bindings(
    process_smiles = .mock_process_smiles_fast,
    split_tables_sop(table = sop_table, cache = NULL)
  )

  expect_type(result, "list")
  expect_true(nrow(result$org_tax_ott) > 0)
})

test_that("split_tables_sop handles missing taxonomy fields", {
  # Load fixture with missing taxonomy instead of creating inline
  sop_table <- load_fixture("sop_table_missing_taxonomy")

  result <- with_mocked_bindings(
    process_smiles = .mock_process_smiles_fast,
    split_tables_sop(table = sop_table, cache = NULL)
  )

  expect_type(result, "list")
  expect_true(nrow(result$org_tax_ott) >= 0)
})

test_that("split_tables_sop collapses multi-source structure names and taxonomy", {
  sop_table <- tidytable::tidytable(
    structure_smiles = c("C[C@H](O)C", "C[C@H](O)C"),
    structure_smiles_no_stereo = c("CC(O)C", "CC(O)C"),
    structure_name = c("Alpha", " alpha "),
    structure_tag = c("tag1", "tag2"),
    structure_tax_npc_01pat = c("PathA", "PathB"),
    structure_tax_npc_02sup = c("SupA", "SupA"),
    structure_tax_npc_03cla = c("ClaA", "ClaB"),
    structure_tax_cla_chemontid = c("CHEMONTID:0001", "CHEMONTID:0002"),
    structure_tax_cla_01kin = c("KinA", "KinB"),
    structure_tax_cla_02sup = c("Sup1", "Sup2"),
    structure_tax_cla_03cla = c("Class1", "Class2"),
    structure_tax_cla_04dirpar = c("Dir1", "Dir2"),
    organism_name = c("Org A", "Org A"),
    organism_taxonomy_ottid = c("100", "100"),
    reference_doi = c("10.1000/a", "10.1000/b")
  )

  with_mocked_bindings(
    process_smiles = function(table, cache) {
      table |>
        tidytable::mutate(
          structure_inchikey = "AAAAAAAAAAAAAA-BBBBBBBBBB-N",
          structure_inchikey_connectivity_layer = "AAAAAAAAAAAAAA",
          structure_molecular_formula = "C3H8O",
          structure_exact_mass = 60.0575,
          structure_xlogp = c("1.0", NA_character_)
        ) |>
        tidytable::select(
          structure_smiles_initial,
          structure_inchikey,
          structure_inchikey_connectivity_layer,
          structure_smiles,
          structure_smiles_no_stereo,
          structure_molecular_formula,
          structure_exact_mass,
          structure_xlogp
        )
    },
    {
      result <- split_tables_sop(table = sop_table, cache = NULL)
    }
  )

  expect_equal(nrow(result$str_stereo), 1)
  expect_match(result$str_stereo$structure_name[[1]], "Alpha")
  expect_true(grepl("tag1", result$str_stereo$structure_tag[[1]], fixed = TRUE))
  expect_true(grepl("tag2", result$str_stereo$structure_tag[[1]], fixed = TRUE))
  expect_true(grepl("\\$", result$str_tax_npc$structure_tax_npc_01pat[[1]]))
  expect_true(grepl("\\$", result$str_tax_cla$structure_tax_cla_01kin[[1]]))
})

test_that("split_tables_sop handles missing optional structure columns", {
  sop_table <- tidytable::tidytable(
    structure_smiles = "CCO",
    structure_smiles_no_stereo = "CCO",
    structure_tax_npc_01pat = NA_character_,
    structure_tax_npc_02sup = NA_character_,
    structure_tax_npc_03cla = NA_character_,
    structure_tax_cla_chemontid = NA_character_,
    structure_tax_cla_01kin = NA_character_,
    structure_tax_cla_02sup = NA_character_,
    structure_tax_cla_03cla = NA_character_,
    structure_tax_cla_04dirpar = NA_character_,
    organism_name = "Org B",
    organism_taxonomy_ottid = "200",
    reference_doi = "10.1000/c"
  )

  with_mocked_bindings(
    process_smiles = function(table, cache) {
      table |>
        tidytable::mutate(
          structure_inchikey = "CCCCCCCCCCCCCC-DDDDDDDDDD-N",
          structure_inchikey_connectivity_layer = "CCCCCCCCCCCCCC",
          structure_molecular_formula = "C2H6O",
          structure_exact_mass = 46.0419
        ) |>
        tidytable::select(
          structure_smiles_initial,
          structure_inchikey,
          structure_inchikey_connectivity_layer,
          structure_smiles,
          structure_smiles_no_stereo,
          structure_molecular_formula,
          structure_exact_mass
        )
    },
    {
      result <- split_tables_sop(table = sop_table, cache = NULL)
    }
  )

  expect_true("structure_xlogp" %in% names(result$str_stereo))
  expect_true("structure_name" %in% names(result$str_stereo))
  expect_true("structure_tag" %in% names(result$str_stereo))
  expect_true(all(is.na(result$str_stereo$structure_xlogp)))
})
