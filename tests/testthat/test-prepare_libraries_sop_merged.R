# Test Suite: prepare_libraries_sop_merged ----

library(testthat)

## Validation ----

test_that("validate_sop_merged_inputs accepts valid inputs", {
  expect_silent(
    validate_sop_merged_inputs(
      files = c("file1.tsv"),
      filter = FALSE,
      level = "family",
      value = "Test",
      output_key = temp_test_path("key.tsv"),
      output_org_tax_ott = temp_test_path("org.tsv"),
      output_str_can = temp_test_path("can.tsv"),
      output_str_stereo = temp_test_path("stereo.tsv"),
      output_str_met = temp_test_path("met.tsv"),
      output_str_tax_cla = temp_test_path("cla.tsv"),
      output_str_tax_npc = temp_test_path("npc.tsv")
    )
  )
})

test_that("validate_sop_merged_inputs rejects invalid filter", {
  expect_error(
    validate_sop_merged_inputs(
      files = c("file1.tsv"),
      filter = "yes",
      level = "family",
      value = "Test",
      output_key = temp_test_path("key.tsv"),
      output_org_tax_ott = temp_test_path("org.tsv"),
      output_str_can = temp_test_path("can.tsv"),
      output_str_stereo = temp_test_path("stereo.tsv"),
      output_str_met = temp_test_path("met.tsv"),
      output_str_tax_cla = temp_test_path("cla.tsv"),
      output_str_tax_npc = temp_test_path("npc.tsv")
    ),
    "filter must be a single logical value",
    class = "tima_validation_error"
  )
})

test_that("validate_sop_merged_inputs rejects invalid level", {
  expect_error(
    validate_sop_merged_inputs(
      files = c("file1.tsv"),
      filter = TRUE,
      level = "invalid_level",
      value = "Test",
      output_key = temp_test_path("key.tsv"),
      output_org_tax_ott = temp_test_path("org.tsv"),
      output_str_can = temp_test_path("can.tsv"),
      output_str_stereo = temp_test_path("stereo.tsv"),
      output_str_met = temp_test_path("met.tsv"),
      output_str_tax_cla = temp_test_path("cla.tsv"),
      output_str_tax_npc = temp_test_path("npc.tsv")
    ),
    "supported taxonomic ranks",
    class = "tima_validation_error"
  )
})

test_that("complete_organism_taxonomy handles empty missing data", {
  table_keys <- tidytable::tidytable(
    organism_name = c("Org1", "Org2")
  )

  table_org_tax_ott <- tidytable::tidytable(
    organism_name = c("Org1", "Org2"),
    kingdom = c("Plantae", "Plantae")
  )

  result <- complete_organism_taxonomy(table_keys, table_org_tax_ott)

  expect_equal(nrow(result), 2)
})

test_that("apply_taxonomic_filter filters correctly", {
  # Load fixtures instead of creating inline
  table_keys <- load_fixture("structure_keys_filter_test")
  table_org_tax_ott <- load_fixture("organism_taxonomy_family")

  result <- apply_taxonomic_filter(
    table_keys,
    table_org_tax_ott,
    level = "family",
    value = "Fam1"
  )

  expect_equal(nrow(result), 2)
  expect_true(all(c("Plant1", "Plant3") %in% result$organism_name))
})

test_that("apply_taxonomic_filter errors on no matches", {
  # Load fixtures instead of creating inline
  table_keys <- load_fixture("structure_keys_single")
  table_org_tax_ott <- load_fixture("organism_taxonomy_single_family")

  expect_error(
    apply_taxonomic_filter(
      table_keys,
      table_org_tax_ott,
      level = "family",
      value = "NonExistent"
    ),
    "filter led to no entries",
    class = "tima_validation_error"
  )
})

## validate_sop_merged_inputs - Additional edge cases ----

test_that("test-validate_sop_merged_inputs accepts all valid taxonomic levels", {
  valid_levels <- c(
    "domain",
    "kingdom",
    "phylum",
    "class",
    "order",
    "family",
    "tribe",
    "genus",
    "species",
    "varietas"
  )

  invisible(vapply(
    X = valid_levels,
    FUN = function(lvl) {
      expect_silent(
        validate_sop_merged_inputs(
          files = c("file1.tsv"),
          filter = TRUE,
          level = lvl,
          value = "TestValue",
          output_key = temp_test_path("key.tsv"),
          output_org_tax_ott = temp_test_path("org.tsv"),
          output_str_can = temp_test_path("can.tsv"),
          output_str_stereo = temp_test_path("stereo.tsv"),
          output_str_met = temp_test_path("met.tsv"),
          output_str_tax_cla = temp_test_path("cla.tsv"),
          output_str_tax_npc = temp_test_path("npc.tsv")
        )
      )
      TRUE
    },
    FUN.VALUE = logical(1L)
  ))
})

test_that("test-validate_sop_merged_inputs rejects non-character output paths", {
  expect_error(
    validate_sop_merged_inputs(
      files = c("file1.tsv"),
      filter = FALSE,
      level = "family",
      value = "Test",
      output_key = 123, # Invalid
      output_org_tax_ott = temp_test_path("org.tsv"),
      output_str_can = temp_test_path("can.tsv"),
      output_str_stereo = temp_test_path("stereo.tsv"),
      output_str_met = temp_test_path("met.tsv"),
      output_str_tax_cla = temp_test_path("cla.tsv"),
      output_str_tax_npc = temp_test_path("npc.tsv")
    ),
    "Output parameter\\(s\\) must be single character strings.*output_key",
    class = "tima_validation_error"
  )
})

test_that("test-validate_sop_merged_inputs rejects vector output paths", {
  expect_error(
    validate_sop_merged_inputs(
      files = c("file1.tsv"),
      filter = FALSE,
      level = "family",
      value = "Test",
      output_key = temp_test_path("key.tsv"),
      output_org_tax_ott = c("org1.tsv", "org2.tsv"), # Invalid - vector
      output_str_can = temp_test_path("can.tsv"),
      output_str_stereo = temp_test_path("stereo.tsv"),
      output_str_met = temp_test_path("met.tsv"),
      output_str_tax_cla = temp_test_path("cla.tsv"),
      output_str_tax_npc = temp_test_path("npc.tsv")
    ),
    "Output parameter\\(s\\) must be single character strings.*output_org_tax_ott",
    class = "tima_validation_error"
  )
})

test_that("test-validate_sop_merged_inputs requires value when filtering", {
  expect_error(
    validate_sop_merged_inputs(
      files = c("file1.tsv"),
      filter = TRUE,
      level = "family",
      value = 123, # Invalid - not character
      output_key = temp_test_path("key.tsv"),
      output_org_tax_ott = temp_test_path("org.tsv"),
      output_str_can = temp_test_path("can.tsv"),
      output_str_stereo = temp_test_path("stereo.tsv"),
      output_str_met = temp_test_path("met.tsv"),
      output_str_tax_cla = temp_test_path("cla.tsv"),
      output_str_tax_npc = temp_test_path("npc.tsv")
    ),
    "value must be a single character string",
    class = "tima_validation_error"
  )
})

test_that("test-validate_sop_merged_inputs accepts filter=FALSE without checking level/value", {
  # When filter=FALSE, level and value are not validated
  expect_silent(
    validate_sop_merged_inputs(
      files = c("file1.tsv"),
      filter = FALSE,
      level = "invalid_level", # This should be ignored when filter=FALSE
      value = "Test",
      output_key = temp_test_path("key.tsv"),
      output_org_tax_ott = temp_test_path("org.tsv"),
      output_str_can = temp_test_path("can.tsv"),
      output_str_stereo = temp_test_path("stereo.tsv"),
      output_str_met = temp_test_path("met.tsv"),
      output_str_tax_cla = temp_test_path("cla.tsv"),
      output_str_tax_npc = temp_test_path("npc.tsv")
    )
  )
})

test_that("test-validate_sop_merged_inputs rejects NULL filter", {
  expect_error(
    validate_sop_merged_inputs(
      files = c("file1.tsv"),
      filter = NULL,
      level = "family",
      value = "Test",
      output_key = temp_test_path("key.tsv"),
      output_org_tax_ott = temp_test_path("org.tsv"),
      output_str_can = temp_test_path("can.tsv"),
      output_str_stereo = temp_test_path("stereo.tsv"),
      output_str_met = temp_test_path("met.tsv"),
      output_str_tax_cla = temp_test_path("cla.tsv"),
      output_str_tax_npc = temp_test_path("npc.tsv")
    ),
    "filter must be a single logical value",
    class = "tima_validation_error"
  )
})

test_that("test-validate_sop_merged_inputs validates all output parameters", {
  # Test each output parameter individually
  output_params <- c(
    "output_key",
    "output_org_tax_ott",
    "output_str_can",
    "output_str_stereo",
    "output_str_met",
    "output_str_tax_cla",
    "output_str_tax_npc"
  )

  invisible(vapply(
    X = output_params,
    FUN = function(param) {
      args <- list(
        files = c("file1.tsv"),
        filter = FALSE,
        level = "family",
        value = "Test",
        output_key = temp_test_path("key.tsv"),
        output_org_tax_ott = temp_test_path("org.tsv"),
        output_str_can = temp_test_path("can.tsv"),
        output_str_stereo = temp_test_path("stereo.tsv"),
        output_str_met = temp_test_path("met.tsv"),
        output_str_tax_cla = temp_test_path("cla.tsv"),
        output_str_tax_npc = temp_test_path("npc.tsv")
      )
      args[[param]] <- c("path1.tsv", "path2.tsv")
      expect_error(
        do.call(validate_sop_merged_inputs, args),
        paste0(
          "Output parameter\\(s\\) must be single character strings.*",
          param
        ),
        class = "tima_validation_error"
      )
      TRUE
    },
    FUN.VALUE = logical(1L)
  ))
})

## complete_organism_taxonomy ----

test_that("test-complete_organism_taxonomy returns original when all organisms present", {
  # Load fixtures instead of creating inline
  table_keys <- load_fixture("organism_keys_basic")
  table_org_tax_ott <- load_fixture("organism_taxonomy_basic")

  result <- complete_organism_taxonomy(table_keys, table_org_tax_ott)

  expect_equal(nrow(result), 2)
  expect_equal(sort(result$organism_name), sort(c("Org1", "Org2")))
})

test_that("test-complete_organism_taxonomy handles all missing organisms", {
  skip_if_no_network("https://api.opentreeoflife.org")
  # Load fixtures instead of creating inline
  table_keys <- load_fixture("organism_keys_new")
  table_org_tax_ott <- load_fixture("organism_taxonomy_empty")

  # This will try to fetch taxonomy - may fail in test environment
  # We just check it doesn't error
  expect_warning(
    complete_organism_taxonomy(table_keys, table_org_tax_ott),
    "NewOrg2, NewOrg1 are not matched",
    fixed = TRUE
  )
})

test_that("test-complete_organism_taxonomy handles mixed missing/present", {
  skip_if_no_network("https://api.opentreeoflife.org")
  # Load fixtures instead of creating inline
  table_keys <- load_fixture("organism_keys_mixed")
  table_org_tax_ott <- load_fixture("organism_taxonomy_partial")

  # Should attempt to fetch taxonomy for NewOrg
  expect_warning(
    complete_organism_taxonomy(table_keys, table_org_tax_ott),
    "NewOrg are not matched",
    fixed = TRUE
  )
})

test_that("test-complete_organism_taxonomy preserves existing data", {
  # Load fixtures instead of creating inline
  table_keys <- load_fixture("organism_keys_basic")
  table_org_tax_ott <- load_fixture("organism_taxonomy_multi_level")

  result <- complete_organism_taxonomy(table_keys, table_org_tax_ott)

  expect_equal(result$organism_taxonomy_ottol_kingdom, c("Plantae", "Animalia"))
  expect_equal(result$organism_taxonomy_ottol_family, c("Fam1", "Fam2"))
})

## apply_taxonomic_filter ----

test_that("test-apply_taxonomic_filter filters by kingdom", {
  # Load fixtures instead of creating inline
  table_keys <- load_fixture("structure_keys_kingdom")
  table_org_tax_ott <- load_fixture("organism_taxonomy_kingdom")

  result <- apply_taxonomic_filter(
    table_keys,
    table_org_tax_ott,
    level = "kingdom",
    value = "Plantae"
  )

  expect_equal(nrow(result), 2)
  expect_true(all(c("Plant1", "Plant2") %in% result$organism_name))
})

test_that("test-apply_taxonomic_filter filters by genus", {
  # Load fixtures instead of creating inline
  table_keys <- load_fixture("structure_keys_genus")
  table_org_tax_ott <- load_fixture("organism_taxonomy_genus")

  result <- apply_taxonomic_filter(
    table_keys,
    table_org_tax_ott,
    level = "genus",
    value = "Genus1"
  )

  expect_equal(nrow(result), 2)
  expect_true(all(c("Species1", "Species3") %in% result$organism_name))
})

test_that("test-apply_taxonomic_filter filters by species", {
  # Load fixtures instead of creating inline
  table_keys <- load_fixture("structure_keys_species")
  table_org_tax_ott <- load_fixture("organism_taxonomy_species")

  result <- apply_taxonomic_filter(
    table_keys,
    table_org_tax_ott,
    level = "species",
    value = "Homo sapiens"
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$organism_name, "Homo sapiens")
})

test_that("test-apply_taxonomic_filter handles case sensitivity", {
  # Load fixtures instead of creating inline
  table_keys <- load_fixture("structure_keys_two_orgs")
  table_org_tax_ott <- load_fixture("organism_taxonomy_case_test")

  # Test exact case match
  result <- apply_taxonomic_filter(
    table_keys,
    table_org_tax_ott,
    level = "family",
    value = "Fabaceae"
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$organism_name, "Org1")
})

test_that("test-apply_taxonomic_filter handles NA taxonomy values", {
  # Load fixtures instead of creating inline
  table_keys <- load_fixture("structure_keys_three_orgs")
  table_org_tax_ott <- load_fixture("organism_taxonomy_order_na")

  result <- apply_taxonomic_filter(
    table_keys,
    table_org_tax_ott,
    level = "order",
    value = "Order1"
  )

  # Should only match Org1 and Org3
  expect_equal(nrow(result), 2)
  expect_false("Org2" %in% result$organism_name)
})

test_that("test-apply_taxonomic_filter returns unique structures", {
  # Load fixtures instead of creating inline
  table_keys <- load_fixture("structure_keys_duplicate_structures")
  table_org_tax_ott <- load_fixture("organism_taxonomy_class")

  result <- apply_taxonomic_filter(
    table_keys,
    table_org_tax_ott,
    level = "class",
    value = "Class1"
  )

  # Should return both rows for structure A (different organisms)
  expect_equal(nrow(result), 2)
  expect_equal(sum(result$structure_inchikey == "A"), 2)
})

test_that("test-apply_taxonomic_filter handles empty result", {
  # Load fixtures instead of creating inline
  table_keys <- load_fixture("structure_keys_two_orgs")
  table_org_tax_ott <- load_fixture("organism_taxonomy_phylum")

  # Filter by non-existent value
  expect_error(
    apply_taxonomic_filter(
      table_keys,
      table_org_tax_ott,
      level = "phylum",
      value = "NonExistentPhylum"
    ),
    "filter led to no entries",
    class = "tima_validation_error"
  )
})

test_that("test-apply_taxonomic_filter handles missing taxonomy column", {
  # Load fixtures instead of creating inline
  table_keys <- load_fixture("structure_keys_single")
  table_org_tax_ott <- load_fixture("organism_taxonomy_missing_column")

  expect_error(
    apply_taxonomic_filter(
      table_keys,
      table_org_tax_ott,
      level = "tribe",
      value = "TestTribe"
    ),
    "no column found matching requested taxonomic level",
    class = "tima_validation_error"
  )
})

## Integration ----

test_that("test-prepare_libraries_sop_merged validates inputs before processing", {
  # Test that validation happens first
  expect_error(
    prepare_libraries_sop_merged(
      files = c("file.tsv"),
      filter = "invalid", # Invalid type
      level = "family",
      value = "Test",
      output_key = "key.tsv",
      output_org_tax_ott = "org.tsv",
      output_str_can = temp_test_path("can.tsv"),
      output_str_stereo = temp_test_path("stereo.tsv"),
      output_str_met = temp_test_path("met.tsv"),
      output_str_tax_cla = temp_test_path("cla.tsv"),
      output_str_tax_npc = temp_test_path("npc.tsv")
    ),
    "filter must be a single logical value",
    class = "tima_validation_error"
  )
})

## Edge cases and boundary conditions ----

test_that("test-validate_sop_merged_inputs accepts empty value when filter=FALSE", {
  expect_silent(
    validate_sop_merged_inputs(
      files = c("file1.tsv"),
      filter = FALSE,
      level = "family",
      value = "", # Empty string OK when not filtering
      output_key = temp_test_path("key.tsv"),
      output_org_tax_ott = temp_test_path("org.tsv"),
      output_str_can = temp_test_path("can.tsv"),
      output_str_stereo = temp_test_path("stereo.tsv"),
      output_str_met = temp_test_path("met.tsv"),
      output_str_tax_cla = temp_test_path("cla.tsv"),
      output_str_tax_npc = temp_test_path("npc.tsv")
    )
  )
})

test_that("test-complete_organism_taxonomy handles duplicate organism names", {
  table_keys <- tidytable::tidytable(
    organism_name = c("Org1", "Org1", "Org2")
  )

  table_org_tax_ott <- tidytable::tidytable(
    organism_name = c("Org1", "Org2"),
    organism_taxonomy_ottol_kingdom = c("Plantae", "Animalia")
  )

  result <- complete_organism_taxonomy(table_keys, table_org_tax_ott)

  # Should still return 2 unique organisms
  expect_true(nrow(result) >= 2)
})

test_that("test-apply_taxonomic_filter preserves core columns", {
  table_keys <- tidytable::tidytable(
    structure_inchikey = c("A", "B"),
    structure_smiles_no_stereo = c("S1", "S2"),
    organism_name = c("Org1", "Org2"),
    reference_doi = c("doi1", "doi2")
  )

  table_org_tax_ott <- tidytable::tidytable(
    organism_name = c("Org1", "Org2"),
    organism_taxonomy_ottol_domain = c("Eukaryota", "Eukaryota")
  )

  result <- apply_taxonomic_filter(
    table_keys,
    table_org_tax_ott,
    level = "domain",
    value = "Eukaryota"
  )

  # Should preserve core columns
  expect_true("structure_inchikey" %in% names(result))
  expect_true("organism_name" %in% names(result))
  expect_true("structure_smiles_no_stereo" %in% names(result))
})

# ==== SUCCESS PATH TESTS FOR EXPORTED FUNCTION ====

test_that("prepare_libraries_sop_merged() runs without filtering", {
  # Create minimal library data
  tmpdir <- tempfile()
  dir.create(tmpdir, recursive = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE))

  # Minimal structure-organism-pair data
  sop_data <- tidytable::tidytable(
    structure_inchikey = c("AAAAA", "BBBBB", "CCCCC"),
    structure_smiles = c("CCC", "CCCC", "CCCCC"),
    structure_smiles_no_stereo = c("CCC", "CCCC", "CCCCC"),
    structure_molecular_formula = c("C3H8", "C4H10", "C5H12"),
    structure_name = c("nameA", "nameB", "nameC"),
    structure_exact_mass = c(44.06, 58.08, 72.09),
    structure_xlogp = c(1.5, 2.0, 2.5),
    structure_tag = c("foo", "bar", "bim"),
    structure_tax_npc_01pat = c("A", "A", "A"),
    structure_tax_npc_02sup = c(NA, NA, NA),
    structure_tax_npc_03cla = c(NA, NA, NA),
    structure_tax_cla_chemontid = c(NA, NA, NA),
    structure_tax_cla_01kin = c("O", "O", "O"),
    structure_tax_cla_02sup = c(NA, NA, NA),
    structure_tax_cla_03cla = c(NA, NA, NA),
    structure_tax_cla_04dirpar = c(NA, NA, NA),
    organism_name = c("Plant1", "Plant2", "Plant3"),
    organism_taxonomy_ottid = c("123", "456", "789"),
    organism_taxonomy_01domain = c("Eukaryota", "Eukaryota", "Eukaryota"),
    organism_taxonomy_02kingdom = c("Plantae", "Plantae", "Plantae"),
    organism_taxonomy_03phylum = c(
      "Tracheophyta",
      "Tracheophyta",
      "Tracheophyta"
    ),
    reference_doi = c("10.1234/test1", "10.1234/test2", "10.1234/test3")
  )

  input_file <- file.path(tmpdir, "library.tsv")
  tidytable::fwrite(x = sop_data, file = input_file, sep = "\t")

  # Output files
  output_key <- file.path(tmpdir, "keys.tsv")
  output_org <- file.path(tmpdir, "org_tax.tsv")
  output_can <- file.path(tmpdir, "canonical.tsv")
  output_stereo <- file.path(tmpdir, "stereo.tsv")
  output_met <- file.path(tmpdir, "metadata.tsv")
  output_cla <- file.path(tmpdir, "classyfire.tsv")
  output_npc <- file.path(tmpdir, "npc.tsv")

  # Mock get_params and export_params
  local_mocked_bindings(
    get_params = function(step) list(step = "prepare_libraries_sop_merged"),
    export_params = function(...) invisible(NULL),
    .package = "tima"
  )

  # Run function
  result <- prepare_libraries_sop_merged(
    files = input_file,
    filter = FALSE,
    output_key = output_key,
    output_org_tax_ott = output_org,
    output_str_can = output_can,
    output_str_stereo = output_stereo,
    output_str_met = output_met,
    output_str_tax_cla = output_cla,
    output_str_tax_npc = output_npc
  )

  # Verify outputs created
  expect_true(file.exists(output_key))
  expect_true(file.exists(output_org))

  # Verify data
  keys <- tidytable::fread(output_key)
  expect_equal(nrow(keys), 3)
  expect_true(all(c("structure_inchikey", "organism_name") %in% names(keys)))
})

test_that("prepare_libraries_sop_merged() filters by taxonomy", {
  tmpdir <- tempfile()
  dir.create(tmpdir, recursive = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE))

  # Data with different families
  sop_data <- tidytable::tidytable(
    structure_inchikey = c("AAAAA", "BBBBB", "CCCCC", "DDDDD"),
    structure_smiles = c("C", "CC", "CCC", "CCCC"),
    structure_smiles_no_stereo = c("C", "CC", "CCC", "CCCC"),
    structure_molecular_formula = c("CH4", "C2H6", "C3H8", "C4H10"),
    structure_name = c("nameA", "nameB", "nameC", "nameD"),
    structure_exact_mass = c(16, 30, 44, 58),
    structure_xlogp = c(1, 1.5, 2, 2.5),
    structure_tag = c("foo", "bar", "bim", "boum"),
    structure_tax_npc_01pat = c("A", "A", "A", "A"),
    structure_tax_npc_02sup = c(NA, NA, NA, NA),
    structure_tax_npc_03cla = c(NA, NA, NA, NA),
    structure_tax_cla_chemontid = c(NA, NA, NA, NA),
    structure_tax_cla_01kin = c("O", "O", "O", "O"),
    structure_tax_cla_02sup = c(NA, NA, NA, NA),
    structure_tax_cla_03cla = c(NA, NA, NA, NA),
    structure_tax_cla_04dirpar = c(NA, NA, NA, NA),
    organism_name = c("Plant1", "Plant2", "Plant3", "Plant4"),
    organism_taxonomy_ottid = c("1", "2", "3", "4"),
    organism_taxonomy_01domain = c("E", "E", "E", "E"),
    organism_taxonomy_02kingdom = c("P", "P", "P", "P"),
    organism_taxonomy_03phylum = c("T", "T", "T", "T"),
    organism_taxonomy_06family = c("Fam1", "Fam1", "Fam2", "Fam2"),
    reference_doi = c("d1", "d2", "d3", "d4")
  )

  input_file <- file.path(tmpdir, "library.tsv")
  tidytable::fwrite(x = sop_data, file = input_file, sep = "\t")

  output_key <- file.path(tmpdir, "keys.tsv")
  output_org <- file.path(tmpdir, "org_tax.tsv")
  output_can <- file.path(tmpdir, "canonical.tsv")
  output_stereo <- file.path(tmpdir, "stereo.tsv")
  output_met <- file.path(tmpdir, "metadata.tsv")
  output_cla <- file.path(tmpdir, "classyfire.tsv")
  output_npc <- file.path(tmpdir, "npc.tsv")

  local_mocked_bindings(
    get_params = function(step) list(step = "prepare_libraries_sop_merged"),
    export_params = function(...) invisible(NULL),
    .package = "tima"
  )

  # Filter by family
  result <- prepare_libraries_sop_merged(
    files = input_file,
    filter = TRUE,
    level = "family",
    value = "Fam1",
    output_key = output_key,
    output_org_tax_ott = output_org,
    output_str_can = output_can,
    output_str_stereo = output_stereo,
    output_str_met = output_met,
    output_str_tax_cla = output_cla,
    output_str_tax_npc = output_npc
  )

  # Verify filtered output
  keys <- tidytable::fread(output_key)
  expect_equal(nrow(keys), 2) # Only Fam1 entries
  expect_true(all(c("Plant1", "Plant2") %in% keys$organism_name))
  expect_false("Plant3" %in% keys$organism_name)
})

test_that("prepare_libraries_sop_merged() merges multiple input files", {
  tmpdir <- tempfile()
  dir.create(tmpdir, recursive = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE))

  # Create two separate library files
  minimal_cols <- c(
    "structure_inchikey",
    "structure_smiles_no_stereo",
    "structure_molecular_formula",
    "structure_exact_mass",
    "structure_xlogp",
    "structure_tag",
    "structure_taxonomy_npclassifier_01pathway",
    "structure_taxonomy_npclassifier_02superclass",
    "structure_taxonomy_npclassifier_03class",
    "structure_taxonomy_classyfire_chemontid",
    "structure_taxonomy_classyfire_01kingdom",
    "structure_taxonomy_classyfire_02superclass",
    "structure_taxonomy_classyfire_03class",
    "structure_taxonomy_classyfire_04directparent",
    "organism_name",
    "organism_taxonomy_ottid",
    "organism_taxonomy_01domain",
    "organism_taxonomy_02kingdom",
    "organism_taxonomy_03phylum",
    "reference_doi"
  )

  sop_data1 <- tidytable::tidytable(
    structure_inchikey = "AAAAA",
    structure_smiles = "C",
    structure_smiles_no_stereo = "C",
    structure_molecular_formula = "CH4",
    structure_exact_mass = 16,
    structure_xlogp = 1,
    structure_tag = "foo",
    structure_tax_npc_01pat = "A",
    structure_tax_npc_02sup = NA,
    structure_tax_npc_03cla = NA,
    structure_tax_cla_chemontid = NA,
    structure_tax_cla_01kin = "O",
    structure_tax_cla_02sup = NA,
    structure_tax_cla_03cla = NA,
    structure_tax_cla_04dirpar = NA,
    organism_name = "Plant1",
    organism_taxonomy_ottid = "1",
    organism_taxonomy_01domain = "E",
    organism_taxonomy_02kingdom = "P",
    organism_taxonomy_03phylum = "T",
    reference_doi = "d1"
  )

  sop_data2 <- tidytable::tidytable(
    structure_inchikey = "BBBBB",
    structure_smiles_no_stereo = "CC",
    structure_molecular_formula = "C2H6",
    structure_name = "nameA",
    structure_exact_mass = 30,
    structure_xlogp = 1.5,
    structure_tag = "A",
    structure_tax_npc_01pat = "A",
    structure_tax_npc_02sup = NA,
    structure_tax_npc_03cla = NA,
    structure_tax_cla_chemontid = NA,
    structure_tax_cla_01kin = "O",
    structure_tax_cla_02sup = NA,
    structure_tax_cla_03cla = NA,
    structure_tax_cla_04dirpar = NA,
    organism_name = "Plant2",
    organism_taxonomy_ottid = "2",
    organism_taxonomy_01domain = "E",
    organism_taxonomy_02kingdom = "P",
    organism_taxonomy_03phylum = "T",
    reference_doi = "d2"
  )

  file1 <- file.path(tmpdir, "lib1.tsv")
  file2 <- file.path(tmpdir, "lib2.tsv")
  tidytable::fwrite(x = sop_data1, file = file1, sep = "\t")
  tidytable::fwrite(x = sop_data2, file = file2, sep = "\t")

  output_key <- file.path(tmpdir, "keys.tsv")
  output_org <- file.path(tmpdir, "org_tax.tsv")
  output_can <- file.path(tmpdir, "canonical.tsv")
  output_stereo <- file.path(tmpdir, "stereo.tsv")
  output_met <- file.path(tmpdir, "metadata.tsv")
  output_cla <- file.path(tmpdir, "classyfire.tsv")
  output_npc <- file.path(tmpdir, "npc.tsv")

  local_mocked_bindings(
    get_params = function(step) list(step = "prepare_libraries_sop_merged"),
    export_params = function(...) invisible(NULL),
    .package = "tima"
  )

  # Merge both files
  result <- prepare_libraries_sop_merged(
    files = c(file1, file2),
    filter = FALSE,
    output_key = output_key,
    output_org_tax_ott = output_org,
    output_str_can = output_can,
    output_str_stereo = output_stereo,
    output_str_met = output_met,
    output_str_tax_cla = output_cla,
    output_str_tax_npc = output_npc
  )

  # Verify merged output
  keys <- tidytable::fread(output_key)
  expect_equal(nrow(keys), 2)
  # expect_true(all(c("AAAAA", "BBBBB") %in% keys$structure_inchikey))
})

## enrich_taxonomy_from_cache ----

test_that("enrich_taxonomy_from_cache returns taxonomy_table when cache_path is NULL", {
  taxonomy <- tidytable::tidytable(
    structure_smiles = "C",
    structure_tax_npc_01pat = "PathA",
    structure_tax_npc_02sup = "SuperA",
    structure_tax_npc_03cla = "ClassA"
  )
  result <- enrich_taxonomy_from_cache(
    taxonomy_table = taxonomy,
    cache_path = NULL,
    key_col = "structure_smiles",
    all_keys = c("C", "CC"),
    taxonomy_name = "NPClassifier"
  )
  expect_equal(nrow(result), 1)
})

test_that("enrich_taxonomy_from_cache returns taxonomy_table when cache file does not exist", {
  taxonomy <- tidytable::tidytable(
    structure_smiles = "C",
    structure_tax_npc_01pat = "PathA",
    structure_tax_npc_02sup = "SuperA",
    structure_tax_npc_03cla = "ClassA"
  )
  result <- enrich_taxonomy_from_cache(
    taxonomy_table = taxonomy,
    cache_path = "/nonexistent/file.tsv",
    key_col = "structure_smiles",
    all_keys = c("C", "CC"),
    taxonomy_name = "NPClassifier"
  )
  expect_equal(nrow(result), 1)
})

test_that("enrich_taxonomy_from_cache enriches NPC taxonomy from cache with internal column names", {
  tmpdir <- tempfile()
  dir.create(tmpdir, recursive = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE))

  taxonomy <- tidytable::tidytable(
    structure_smiles = "C",
    structure_tax_npc_01pat = "PathA",
    structure_tax_npc_02sup = "SuperA",
    structure_tax_npc_03cla = "ClassA"
  )

  # Cache with same internal column names
  cache <- tidytable::tidytable(
    structure_smiles = c("CC", "CCC"),
    structure_tax_npc_01pat = c("PathB", "PathC"),
    structure_tax_npc_02sup = c("SuperB", "SuperC"),
    structure_tax_npc_03cla = c("ClassB", "ClassC")
  )
  cache_path <- file.path(tmpdir, "npc_cache.tsv")
  tidytable::fwrite(cache, cache_path, sep = "\t")

  result <- enrich_taxonomy_from_cache(
    taxonomy_table = taxonomy,
    cache_path = cache_path,
    key_col = "structure_smiles",
    all_keys = c("C", "CC", "CCC"),
    taxonomy_name = "NPClassifier"
  )
  expect_equal(nrow(result), 3)
  expect_true("CC" %in% result$structure_smiles)
  expect_true("CCC" %in% result$structure_smiles)
})

test_that("enrich_taxonomy_from_cache applies column mapping for external format", {
  tmpdir <- tempfile()
  dir.create(tmpdir, recursive = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE))

  taxonomy <- tidytable::tidytable(
    structure_smiles = "C",
    structure_tax_npc_01pat = "PathA",
    structure_tax_npc_02sup = "SuperA",
    structure_tax_npc_03cla = "ClassA"
  )

  # Cache with external column names (from NPClassifier API)
  cache <- tidytable::tidytable(
    structure_smiles = c("CC"),
    pathway = c("PathB"),
    superclass = c("SuperB"),
    class = c("ClassB")
  )
  cache_path <- file.path(tmpdir, "npc_cache_ext.tsv")
  tidytable::fwrite(cache, cache_path, sep = "\t")

  result <- enrich_taxonomy_from_cache(
    taxonomy_table = taxonomy,
    cache_path = cache_path,
    key_col = "structure_smiles",
    all_keys = c("C", "CC"),
    col_mapping = list(
      "structure_smiles" = "structure_smiles",
      "structure_tax_npc_01pat" = "pathway",
      "structure_tax_npc_02sup" = "superclass",
      "structure_tax_npc_03cla" = "class"
    ),
    taxonomy_name = "NPClassifier"
  )
  expect_equal(nrow(result), 2)
  added <- result[result$structure_smiles == "CC", ]
  expect_equal(added$structure_tax_npc_01pat, "PathB")
})

test_that("enrich_taxonomy_from_cache does not duplicate existing entries", {
  tmpdir <- tempfile()
  dir.create(tmpdir, recursive = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE))

  taxonomy <- tidytable::tidytable(
    structure_smiles = c("C", "CC"),
    structure_tax_npc_01pat = c("PathA", "PathB"),
    structure_tax_npc_02sup = c("SuperA", "SuperB"),
    structure_tax_npc_03cla = c("ClassA", "ClassB")
  )

  # Cache also has "CC" but all keys are already covered

  cache <- tidytable::tidytable(
    structure_smiles = c("CC"),
    structure_tax_npc_01pat = c("PathB_alt"),
    structure_tax_npc_02sup = c("SuperB_alt"),
    structure_tax_npc_03cla = c("ClassB_alt")
  )
  cache_path <- file.path(tmpdir, "npc_cache.tsv")
  tidytable::fwrite(cache, cache_path, sep = "\t")

  result <- enrich_taxonomy_from_cache(
    taxonomy_table = taxonomy,
    cache_path = cache_path,
    key_col = "structure_smiles",
    all_keys = c("C", "CC"),
    taxonomy_name = "NPClassifier"
  )
  # Should not add anything since all keys already have taxonomy
  expect_equal(nrow(result), 2)
})

test_that("enrich_taxonomy_from_cache enriches ClassyFire taxonomy", {
  tmpdir <- tempfile()
  dir.create(tmpdir, recursive = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE))

  taxonomy <- tidytable::tidytable(
    structure_inchikey = "AAAAA",
    structure_tax_cla_chemontid = "0001",
    structure_tax_cla_01kin = "Organic",
    structure_tax_cla_02sup = "Lipids",
    structure_tax_cla_03cla = "Fatty acyls",
    structure_tax_cla_04dirpar = "Fatty acids"
  )

  cache <- tidytable::tidytable(
    structure_inchikey = c("BBBBB"),
    structure_tax_cla_chemontid = c("0002"),
    structure_tax_cla_01kin = c("Organic"),
    structure_tax_cla_02sup = c("Alkaloids"),
    structure_tax_cla_03cla = c("Amines"),
    structure_tax_cla_04dirpar = c("Primary amines")
  )
  cache_path <- file.path(tmpdir, "cla_cache.tsv")
  tidytable::fwrite(cache, cache_path, sep = "\t")

  result <- enrich_taxonomy_from_cache(
    taxonomy_table = taxonomy,
    cache_path = cache_path,
    key_col = "structure_inchikey",
    all_keys = c("AAAAA", "BBBBB"),
    taxonomy_name = "ClassyFire"
  )
  expect_equal(nrow(result), 2)
  expect_true("BBBBB" %in% result$structure_inchikey)
})

test_that("prepare_libraries_sop_merged() enriches taxonomy from additional caches", {
  tmpdir <- tempfile()
  dir.create(tmpdir, recursive = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE))

  # Library data with some structures missing taxonomy
  sop_data <- tidytable::tidytable(
    structure_inchikey = c("AAAAA", "BBBBB"),
    structure_smiles = c("C", "CC"),
    structure_smiles_no_stereo = c("C", "CC"),
    structure_molecular_formula = c("CH4", "C2H6"),
    structure_name = c("nameA", "nameB"),
    structure_exact_mass = c(16, 30),
    structure_xlogp = c(1, 1.5),
    structure_tag = c("foo", "bar"),
    # Only first structure has NPC taxonomy
    structure_tax_npc_01pat = c("PathA", NA),
    structure_tax_npc_02sup = c(NA, NA),
    structure_tax_npc_03cla = c(NA, NA),
    structure_tax_cla_chemontid = c(NA, NA),
    # Only first structure has CLA taxonomy
    structure_tax_cla_01kin = c("Organic", NA),
    structure_tax_cla_02sup = c(NA, NA),
    structure_tax_cla_03cla = c(NA, NA),
    structure_tax_cla_04dirpar = c(NA, NA),
    organism_name = c("Plant1", "Plant2"),
    organism_taxonomy_ottid = c("1", "2"),
    organism_taxonomy_01domain = c("E", "E"),
    organism_taxonomy_02kingdom = c("P", "P"),
    organism_taxonomy_03phylum = c("T", "T"),
    reference_doi = c("d1", "d2")
  )

  input_file <- file.path(tmpdir, "library.tsv")
  tidytable::fwrite(x = sop_data, file = input_file, sep = "\t")

  # Create additional NPC cache that has taxonomy for "CC"
  npc_cache <- tidytable::tidytable(
    structure_smiles = c("CC"),
    structure_tax_npc_01pat = c("PathB"),
    structure_tax_npc_02sup = c("SuperB"),
    structure_tax_npc_03cla = c("ClassB")
  )
  npc_cache_path <- file.path(tmpdir, "n.tsv")
  tidytable::fwrite(npc_cache, npc_cache_path, sep = "\t")

  # Create additional CLA cache that has taxonomy for "CC"'s real InChIKey
  # process_smiles computes OTMSDBZUPAUEDD-UHFFFAOYSA-N for SMILES "CC"
  cla_cache <- tidytable::tidytable(
    structure_inchikey = c("OTMSDBZUPAUEDD-UHFFFAOYSA-N"),
    structure_tax_cla_chemontid = c("0002"),
    structure_tax_cla_01kin = c("Organic"),
    structure_tax_cla_02sup = c("Alkaloids"),
    structure_tax_cla_03cla = c("Amines"),
    structure_tax_cla_04dirpar = c("Primary amines")
  )
  cla_cache_path <- file.path(tmpdir, "c.tsv")
  tidytable::fwrite(cla_cache, cla_cache_path, sep = "\t")

  output_key <- file.path(tmpdir, "keys.tsv")
  output_org <- file.path(tmpdir, "org_tax.tsv")
  output_can <- file.path(tmpdir, "canonical.tsv")
  output_stereo <- file.path(tmpdir, "stereo.tsv")
  output_met <- file.path(tmpdir, "metadata.tsv")
  output_cla <- file.path(tmpdir, "classyfire.tsv")
  output_npc <- file.path(tmpdir, "npc.tsv")

  local_mocked_bindings(
    get_params = function(step) list(step = "prepare_libraries_sop_merged"),
    export_params = function(...) invisible(NULL),
    .package = "tima"
  )

  result <- prepare_libraries_sop_merged(
    files = input_file,
    filter = FALSE,
    npc_cache = npc_cache_path,
    cla_cache = cla_cache_path,
    output_key = output_key,
    output_org_tax_ott = output_org,
    output_str_can = output_can,
    output_str_stereo = output_stereo,
    output_str_met = output_met,
    output_str_tax_cla = output_cla,
    output_str_tax_npc = output_npc
  )

  # Check that NPC taxonomy was enriched
  npc_result <- tidytable::fread(output_npc)
  expect_true("CC" %in% npc_result$structure_smiles)

  # Check that CLA taxonomy was enriched
  cla_result <- tidytable::fread(output_cla)
  expect_true("OTMSDBZUPAUEDD-UHFFFAOYSA-N" %in% cla_result$structure_inchikey)
})

test_that("enrich_taxonomy_from_cache writes back library entries to cache for persistence", {
  tmpdir <- tempfile()
  dir.create(tmpdir, recursive = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE))

  # Library already has taxonomy for "C"
  taxonomy <- tidytable::tidytable(
    structure_smiles = "C",
    structure_tax_npc_01pat = "PathA",
    structure_tax_npc_02sup = "SuperA",
    structure_tax_npc_03cla = "ClassA"
  )

  # Cache has taxonomy for "CC" only
  cache <- tidytable::tidytable(
    structure_smiles = c("CC"),
    structure_tax_npc_01pat = c("PathB"),
    structure_tax_npc_02sup = c("SuperB"),
    structure_tax_npc_03cla = c("ClassB")
  )
  cache_path <- file.path(tmpdir, "npc_cache.tsv")
  tidytable::fwrite(cache, cache_path, sep = "\t")

  result <- enrich_taxonomy_from_cache(
    taxonomy_table = taxonomy,
    cache_path = cache_path,
    key_col = "structure_smiles",
    all_keys = c("C", "CC"),
    taxonomy_name = "NPClassifier"
  )

  # Result should have both

  expect_equal(nrow(result), 2)

  # Cache file should now also contain "C" (written back from library)
  updated_cache <- tidytable::fread(cache_path)
  expect_true("C" %in% updated_cache$structure_smiles)
  expect_true("CC" %in% updated_cache$structure_smiles)
  expect_equal(nrow(updated_cache), 2)
})

test_that("enrich_taxonomy_from_cache handles classy_magic long-name NPC format", {
  tmpdir <- tempfile()
  dir.create(tmpdir, recursive = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE))

  taxonomy <- tidytable::tidytable(
    structure_smiles = "C",
    structure_tax_npc_01pat = "PathA",
    structure_tax_npc_02sup = "SuperA",
    structure_tax_npc_03cla = "ClassA"
  )

  # Cache with classy_magic long-name format
  cache <- tidytable::tidytable(
    structure_smiles = c("CC"),
    structure_taxonomy_npclassifier_01pathway = c("PathB"),
    structure_taxonomy_npclassifier_02superclass = c("SuperB"),
    structure_taxonomy_npclassifier_03class = c("ClassB")
  )
  cache_path <- file.path(tmpdir, "npc_long.tsv")
  tidytable::fwrite(cache, cache_path, sep = "\t")

  result <- enrich_taxonomy_from_cache(
    taxonomy_table = taxonomy,
    cache_path = cache_path,
    key_col = "structure_smiles",
    all_keys = c("C", "CC"),
    col_mapping = list(
      "structure_smiles" = "structure_smiles",
      "structure_tax_npc_01pat" = c(
        "pathway",
        "structure_taxonomy_npclassifier_01pathway"
      ),
      "structure_tax_npc_02sup" = c(
        "superclass",
        "structure_taxonomy_npclassifier_02superclass"
      ),
      "structure_tax_npc_03cla" = c(
        "class",
        "structure_taxonomy_npclassifier_03class"
      )
    ),
    taxonomy_name = "NPClassifier"
  )
  expect_equal(nrow(result), 2)
  added <- result[result$structure_smiles == "CC", ]
  expect_equal(added$structure_tax_npc_01pat, "PathB")
})

test_that("enrich_taxonomy_from_cache handles marimo CSV format (smiles key column)", {
  tmpdir <- tempfile()
  dir.create(tmpdir, recursive = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE))

  taxonomy <- tidytable::tidytable(
    structure_smiles = "C",
    structure_tax_npc_01pat = "PathA",
    structure_tax_npc_02sup = "SuperA",
    structure_tax_npc_03cla = "ClassA"
  )

  # Cache with marimo format: 'smiles' key, 'pathway', 'superclass', 'class'
  cache <- tidytable::tidytable(
    smiles = c("CC"),
    pathway = c("PathB"),
    superclass = c("SuperB"),
    class = c("ClassB"),
    isglycoside = c("False"),
    error = c("")
  )
  cache_path <- file.path(tmpdir, "npc_marimo.csv")
  tidytable::fwrite(cache, cache_path, sep = ",")

  result <- enrich_taxonomy_from_cache(
    taxonomy_table = taxonomy,
    cache_path = cache_path,
    key_col = "structure_smiles",
    all_keys = c("C", "CC"),
    col_mapping = list(
      "structure_smiles" = "smiles",
      "structure_tax_npc_01pat" = c(
        "pathway",
        "structure_taxonomy_npclassifier_01pathway"
      ),
      "structure_tax_npc_02sup" = c(
        "superclass",
        "structure_taxonomy_npclassifier_02superclass"
      ),
      "structure_tax_npc_03cla" = c(
        "class",
        "structure_taxonomy_npclassifier_03class"
      )
    ),
    taxonomy_name = "NPClassifier"
  )
  expect_equal(nrow(result), 2)
  added <- result[result$structure_smiles == "CC", ]
  expect_equal(added$structure_tax_npc_01pat, "PathB")
})

test_that("enrich_taxonomy_from_cache parses headerless TSV ClassyFire cache (.csv extension)", {
  tmpdir <- tempfile()
  dir.create(tmpdir, recursive = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE))

  taxonomy <- tidytable::tidytable(
    structure_inchikey = "EXISTINGIK00001-UHFFFAOYSA-N",
    structure_tax_cla_chemontid = "CHEMONTID:0000001",
    structure_tax_cla_01kin = "Organic compounds",
    structure_tax_cla_02sup = "Lipids",
    structure_tax_cla_03cla = "Fatty Acyls",
    structure_tax_cla_04dirpar = "Fatty acids"
  )

  # Exact format from production classyfire_cache.csv (headerless TSV with spaces and commas in values)
  cache_path <- file.path(tmpdir, "classyfire_cache.csv")
  writeLines(
    c(
      paste(
        "AAABBEXKLPHOMC-UHFFFAOYSA-N",
        "CHEMONTID:0003469",
        "Organic compounds",
        "Benzenoids",
        "Benzene and substituted derivatives",
        "N-benzylbenzamides",
        sep = "\t"
      ),
      paste(
        "BBBBBBBBBBBBBB-UHFFFAOYSA-N",
        "CHEMONTID:0001234",
        "Organic compounds",
        "Phenylpropanoids and polyketides",
        "Flavonoids, 3-substituted",
        "Flavones",
        sep = "\t"
      )
    ),
    con = cache_path
  )

  result <- enrich_taxonomy_from_cache(
    taxonomy_table = taxonomy,
    cache_path = cache_path,
    key_col = "structure_inchikey",
    all_keys = c(
      "EXISTINGIK00001-UHFFFAOYSA-N",
      "AAABBEXKLPHOMC-UHFFFAOYSA-N"
    ),
    col_mapping = list(
      "structure_inchikey" = "inchikey",
      "structure_tax_cla_chemontid" = c(
        "chemontid",
        "structure_taxonomy_classyfire_chemontid"
      ),
      "structure_tax_cla_01kin" = c(
        "kingdom",
        "structure_taxonomy_classyfire_01kingdom"
      ),
      "structure_tax_cla_02sup" = c(
        "superclass",
        "structure_taxonomy_classyfire_02superclass"
      ),
      "structure_tax_cla_03cla" = c(
        "class",
        "structure_taxonomy_classyfire_03class"
      ),
      "structure_tax_cla_04dirpar" = c(
        "directparent",
        "structure_taxonomy_classyfire_04directparent"
      )
    ),
    taxonomy_name = "ClassyFire"
  )

  expect_equal(nrow(result), 2L)
  added <- result[
    result$structure_inchikey == "AAABBEXKLPHOMC-UHFFFAOYSA-N",
  ]
  expect_equal(added$structure_tax_cla_chemontid, "CHEMONTID:0003469")
  expect_equal(added$structure_tax_cla_01kin, "Organic compounds")
  expect_equal(added$structure_tax_cla_02sup, "Benzenoids")
  expect_equal(
    added$structure_tax_cla_03cla,
    "Benzene and substituted derivatives"
  )
  expect_equal(added$structure_tax_cla_04dirpar, "N-benzylbenzamides")
})

test_that("apply_taxonomic_filter matches organisms without expanding non-matching keys", {
  keys <- tidytable::tidytable(
    structure_inchikey = c(
      "IK1-UHFFFAOYSA-N",
      "IK2-UHFFFAOYSA-N",
      "IK1-UHFFFAOYSA-N"
    ),
    structure_smiles_no_stereo = c("C", "CC", "C"),
    organism_name = c("Rosa canina", "Pinus sylvestris", "Rosa canina"),
    reference_doi = c("10.1/a", "10.1/b", "10.1/a")
  )
  tax <- tidytable::tidytable(
    organism_name = c("Rosa canina", "Pinus sylvestris"),
    organism_taxonomy_06family = c("Rosaceae", "Pinaceae")
  )
  out <- apply_taxonomic_filter(keys, tax, level = "family", value = "Rosaceae")
  expect_equal(nrow(out), 1L)
  expect_equal(out$organism_name, "Rosa canina")
})

test_that("enrich_taxonomy_from_cache skips disk rewrite when cache is already normalized and complete", {
  tmpdir <- tempfile()
  dir.create(tmpdir, recursive = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE))

  taxonomy <- tidytable::tidytable(
    structure_smiles = "C",
    structure_tax_npc_01pat = "PathA",
    structure_tax_npc_02sup = "SuperA",
    structure_tax_npc_03cla = "ClassA"
  )
  cache <- tidytable::tidytable(
    structure_smiles = c("C", "CC"),
    structure_tax_npc_01pat = c("PathA", "PathB"),
    structure_tax_npc_02sup = c("SuperA", "SuperB"),
    structure_tax_npc_03cla = c("ClassA", "ClassB")
  )
  cache_path <- file.path(tmpdir, "npc_cache.tsv")
  tidytable::fwrite(cache, cache_path, sep = "\t")
  old_time <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC")
  Sys.setFileTime(cache_path, old_time)

  res1 <- enrich_taxonomy_from_cache(
    taxonomy_table = taxonomy,
    cache_path = cache_path,
    key_col = "structure_smiles",
    all_keys = c("C", "CC"),
    taxonomy_name = "NPClassifier"
  )
  expect_equal(nrow(res1), 2L)
  expect_equal(as.numeric(file.mtime(cache_path)), as.numeric(old_time))
})

test_that("11_targets_libraries seed cache targets do not track mutable files with format = 'file'", {
  src <- readLines(
    system.file("targets/11_targets_libraries.R", package = "tima"),
    warn = FALSE
  )
  txt <- paste(src, collapse = "\n")
  for (tname in c(
    "lib_sop_mer_str_pro",
    "lib_sop_mer_npc_cache",
    "lib_sop_mer_cla_cache"
  )) {
    pat <- paste0(
      "name\\s*=\\s*",
      tname,
      "[\\s\\S]*?\\n\\s*\\)"
    )
    m <- regmatches(txt, regexpr(pat, txt, perl = TRUE))
    expect_length(m, 1L)
    expect_false(grepl('format\\s*=\\s*"file"', m))
  }
})

test_that(".normalize_and_write_cache normalizes columns, replaces notClassified, writes tsv/gz, and handles write errors", {
  tmp_dir <- withr::local_tempdir()
  raw_cache <- tidytable::tidytable(
    structure_smiles = c("C", "CC", "C"),
    structure_tax_npc_01pat = c("Alkaloids", "notClassified", "Alkaloids"),
    num_col = factor(c("notClassified", "Terpenoids", "notClassified")),
    extra_col = c("drop1", "drop2", "drop3")
  )
  expected_cols <- c("structure_smiles", "structure_tax_npc_01pat", "num_col")

  # 1. Plain TSV output
  tsv_path <- file.path(tmp_dir, "norm_cache.tsv")
  ok_tsv <- .normalize_and_write_cache(
    cache_data = raw_cache,
    cache_path = tsv_path,
    expected_cols = expected_cols,
    taxonomy_name = "NPClassifier"
  )
  expect_true(ok_tsv)
  read_back <- tidytable::fread(tsv_path, na.strings = "")
  expect_identical(nrow(read_back), 2L)
  expect_identical(colnames(read_back), expected_cols)
  expect_identical(read_back$structure_smiles, c("C", "CC"))
  expect_identical(
    read_back$structure_tax_npc_01pat,
    c("Alkaloids", NA_character_)
  )
  expect_identical(read_back$num_col, c(NA_character_, "Terpenoids"))

  # 2. Gzipped TSV.gz output
  gz_path <- file.path(tmp_dir, "norm_cache.tsv.gz")
  ok_gz <- .normalize_and_write_cache(
    cache_data = raw_cache,
    cache_path = gz_path,
    expected_cols = expected_cols,
    taxonomy_name = "NPClassifier"
  )
  expect_true(ok_gz)
  read_gz <- tidytable::fread(gz_path, na.strings = "")
  expect_identical(nrow(read_gz), 2L)

  # 3. Error fallback returns FALSE without throwing
  bad_path <- file.path(tmp_dir, "nonexistent_subdir", "fail.tsv")
  ok_err <- .normalize_and_write_cache(
    cache_data = raw_cache,
    cache_path = bad_path,
    expected_cols = expected_cols,
    taxonomy_name = "NPClassifier"
  )
  expect_false(ok_err)

  # 4. enrich_taxonomy_from_cache triggers .normalize_and_write_cache when supplement is NULL but needs_disk_norm is TRUE
  csv_cache_path <- file.path(tmp_dir, "dirty_cache.tsv")
  writeLines(
    c(
      "structure_smiles,structure_tax_npc_01pat",
      "C,notClassified"
    ),
    csv_cache_path
  )
  tax_tbl <- tidytable::tidytable(
    structure_smiles = "C",
    structure_tax_npc_01pat = "Alkaloids"
  )
  res_enr <- enrich_taxonomy_from_cache(
    taxonomy_table = tax_tbl,
    cache_path = csv_cache_path,
    key_col = "structure_smiles",
    all_keys = "C",
    taxonomy_name = "NPClassifier"
  )
  expect_identical(res_enr$structure_tax_npc_01pat, "Alkaloids")
  norm_lines <- readLines(csv_cache_path)
  expect_true(grepl("\t", norm_lines[[1]], fixed = TRUE))
  expect_false(any(grepl("notClassified", norm_lines, fixed = TRUE)))
})

test_that("complete_organism_taxonomy handles all-NA keys and enriches missing organisms with type coercion", {
  tax_init <- tidytable::tidytable(
    organism_name = "Gentiana lutea",
    organism_taxonomy_ottid = "100",
    flag_col = "TRUE",
    list_col = "a"
  )

  # 1. All-NA organism_name returns table_org_tax_ott unchanged
  keys_na <- tidytable::tidytable(
    organism_name = c(NA_character_, NA_character_)
  )
  expect_identical(complete_organism_taxonomy(keys_na, tax_init), tax_init)

  # 2. Missing organism triggers get_organism_taxonomy_ott and coerces numeric/list/logical cols to character
  keys_missing <- tidytable::tidytable(
    organism_name = c("Gentiana lutea", "Arabidopsis thaliana", NA_character_)
  )
  local_mocked_bindings(
    get_organism_taxonomy_ott = function(df, retry = FALSE) {
      expect_identical(df$organism, "Arabidopsis thaliana")
      expect_false(retry)
      data.frame(
        organism_name = "Arabidopsis thaliana",
        organism_taxonomy_ottid = 200L,
        flag_col = FALSE,
        list_col = I(list("b")),
        stringsAsFactors = FALSE
      )
    }
  )

  res <- complete_organism_taxonomy(keys_missing, tax_init)
  expect_identical(nrow(res), 2L)
  expect_identical(
    res$organism_name,
    c("Gentiana lutea", "Arabidopsis thaliana")
  )
  expect_identical(res$organism_taxonomy_ottid, c("100", "200"))
  expect_identical(res$flag_col, c("TRUE", "FALSE"))
  expect_identical(res$list_col, c("a", "b"))
})

test_that("load_and_merge_libraries, export_library_tables, and apply_taxonomic_filter edge branches work", {
  tmp_dir <- withr::local_tempdir()

  # 1. load_and_merge_libraries reads and binds multiple library files then splits
  f1 <- file.path(tmp_dir, "l1.tsv")
  f2 <- file.path(tmp_dir, "l2.tsv")
  lib_row <- tidytable::tidytable(
    structure_inchikey = "VNWKTOKETHGBQD-UHFFFAOYSA-N",
    structure_smiles = "C",
    structure_smiles_no_stereo = "C",
    structure_molecular_formula = "CH4",
    structure_name = "methane",
    structure_exact_mass = "16.0313",
    structure_xlogp = "0.6",
    structure_tag = "alkane",
    structure_tax_npc_01pat = "PathA",
    structure_tax_npc_02sup = "SupA",
    structure_tax_npc_03cla = "ClaA",
    structure_tax_cla_chemontid = "0000001",
    structure_tax_cla_01kin = "Organic",
    structure_tax_cla_02sup = "Hydrocarbons",
    structure_tax_cla_03cla = "Alkanes",
    structure_tax_cla_04dirpar = "Methane",
    organism_name = "Gentiana lutea",
    organism_taxonomy_ottid = "100",
    organism_taxonomy_01domain = "Eukaryota",
    organism_taxonomy_02kingdom = "Plantae",
    organism_taxonomy_03phylum = "Tracheophyta",
    reference_doi = "10.1000/1"
  )
  tidytable::fwrite(lib_row, f1, sep = "\t")
  tidytable::fwrite(lib_row, f2, sep = "\t")
  cache_smi <- file.path(tmp_dir, "smi_cache.tsv")
  tidytable::fwrite(
    tidytable::tidytable(
      structure_smiles_initial = "C",
      structure_smiles = "C",
      structure_inchikey = "VNWKTOKETHGBQD-UHFFFAOYSA-N",
      structure_molecular_formula = "CH4",
      structure_exact_mass = 16.0313,
      structure_smiles_no_stereo = "C",
      structure_xlogp = 0.6
    ),
    cache_smi,
    sep = "\t"
  )

  merged_split <- load_and_merge_libraries(c(f1, f2), cache = cache_smi)
  expect_type(merged_split, "list")
  expect_true("key" %in% names(merged_split))
  expect_identical(nrow(merged_split$key), 1L)

  # 2. export_library_tables writes all 7 tables and returns named path vector
  paths <- file.path(
    tmp_dir,
    c("k.tsv", "o.tsv", "c.tsv", "s.tsv", "m.tsv", "cla.tsv", "npc.tsv")
  )
  dummy_tbl <- tidytable::tidytable(col = "val")
  ret_paths <- export_library_tables(
    table_keys = dummy_tbl,
    table_org_tax_ott = dummy_tbl,
    table_structures_canonical = dummy_tbl,
    table_structures_stereo = dummy_tbl,
    table_structures_metadata = dummy_tbl,
    table_structures_taxonomy_cla = dummy_tbl,
    table_structures_taxonomy_npc = dummy_tbl,
    output_key = paths[[1]],
    output_org_tax_ott = paths[[2]],
    output_str_can = paths[[3]],
    output_str_stereo = paths[[4]],
    output_str_met = paths[[5]],
    output_str_tax_cla = paths[[6]],
    output_str_tax_npc = paths[[7]]
  )
  expect_identical(
    names(ret_paths),
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
  expect_true(all(file.exists(ret_paths)))

  # 3. apply_taxonomic_filter aborts when matching_orgs is non-empty in taxonomy table but absent from table_keys
  expect_error(
    validate_sop_merged_inputs(
      files = character(0),
      filter = FALSE,
      level = "family",
      value = "Test",
      output_key = "k.tsv",
      output_org_tax_ott = "o.tsv",
      output_str_can = "c.tsv",
      output_str_stereo = "s.tsv",
      output_str_met = "m.tsv",
      output_str_tax_cla = "cla.tsv",
      output_str_tax_npc = "npc.tsv"
    ),
    "files must be a non-empty character vector",
    class = "tima_validation_error"
  )

  keys_unmatched <- tidytable::tidytable(
    structure_inchikey = "IK1",
    structure_smiles_no_stereo = "C",
    organism_name = "Unrelated_Org",
    reference_doi = "10.1/x"
  )
  tax_matched <- tidytable::tidytable(
    organism_name = "Matched_Org",
    organism_taxonomy_06family = "Gentianaceae"
  )
  expect_error(
    apply_taxonomic_filter(
      table_keys = keys_unmatched,
      table_org_tax_ott = tax_matched,
      level = "family",
      value = "Gentianaceae"
    ),
    "filter led to no entries",
    class = "tima_validation_error"
  )
})

test_that("taxonomy cache internal helpers cover all compression, headerless, normalization, and write-back branches", {
  tmp_dir <- withr::local_tempdir()

  # 1. .get_existing_keys on NULL/empty table
  expect_identical(
    .get_existing_keys(NULL, "structure_smiles"),
    character(0)
  )
  expect_identical(
    .get_existing_keys(tidytable::tidytable(), "structure_smiles"),
    character(0)
  )

  # 2. .prepare_cache_supplement with empty existing_keys, no matching keys, and missing expected_cols
  cache_data <- tidytable::tidytable(
    structure_smiles = "C",
    structure_tax_npc_01pat = "Alkaloids"
  )
  expect_null(
    .prepare_cache_supplement(
      cache_data = cache_data,
      key_col = "structure_smiles",
      all_keys = "UNMATCHED_SMI",
      existing_keys = character(0),
      expected_cols = c("structure_smiles", "structure_tax_npc_01pat"),
      taxonomy_name = "NPClassifier"
    )
  )
  supp_filled <- .prepare_cache_supplement(
    cache_data = cache_data,
    key_col = "structure_smiles",
    all_keys = "C",
    existing_keys = character(0),
    expected_cols = c(
      "structure_smiles",
      "structure_tax_npc_01pat",
      "missing_col"
    ),
    taxonomy_name = "NPClassifier"
  )
  expect_identical(nrow(supp_filled), 1L)
  expect_identical(supp_filled$missing_col, NA_character_)

  # 3. .read_taxonomy_cache_file: empty file, single-col, .gz, .bz2, and headerless with col_mapping / empty expected_cols
  empty_file <- file.path(tmp_dir, "empty.tsv")
  writeLines("", empty_file)
  expect_identical(
    nrow(
      .read_taxonomy_cache_file(
        cache_path = empty_file,
        taxonomy_name = "NPC",
        key_col = "structure_smiles",
        all_keys = "C",
        expected_cols = character(0),
        col_mapping = NULL
      )
    ),
    0L
  )

  single_col_file <- file.path(tmp_dir, "single.tsv")
  writeLines(c("structure_smiles", "C"), single_col_file)
  res_single <- .read_taxonomy_cache_file(
    cache_path = single_col_file,
    taxonomy_name = "NPC",
    key_col = "structure_smiles",
    all_keys = "C",
    expected_cols = "structure_smiles",
    col_mapping = NULL
  )
  expect_identical(res_single$structure_smiles, "C")

  # .gz and .bz2 compressed headerless files with col_mapping vs NULL
  gz_file <- file.path(tmp_dir, "cache.tsv.gz")
  gz_con <- gzfile(gz_file, "wt")
  writeLines("VNWKTOKETHGBQD-UHFFFAOYSA-N\tCHEMONTID:0000001", gz_con)
  close(gz_con)
  res_gz <- .read_taxonomy_cache_file(
    cache_path = gz_file,
    taxonomy_name = "CLA",
    key_col = "structure_inchikey",
    all_keys = "VNWKTOKETHGBQD-UHFFFAOYSA-N",
    expected_cols = character(0),
    col_mapping = list(
      structure_inchikey = "inchikey",
      structure_tax_cla_chemontid = "chemontid"
    )
  )
  expect_identical(
    names(res_gz),
    c("structure_inchikey", "structure_tax_cla_chemontid")
  )

  bz_file <- file.path(tmp_dir, "cache.tsv.bz2")
  bz_con <- bzfile(bz_file, "wt")
  writeLines("VNWKTOKETHGBQD-UHFFFAOYSA-N\tCHEMONTID:0000001", bz_con)
  close(bz_con)
  res_bz <- .read_taxonomy_cache_file(
    cache_path = bz_file,
    taxonomy_name = "CLA",
    key_col = "structure_inchikey",
    all_keys = "VNWKTOKETHGBQD-UHFFFAOYSA-N",
    expected_cols = character(0),
    col_mapping = NULL
  )
  expect_identical(nrow(res_bz), 1L)

  # 4. .cache_needs_disk_normalization branches
  clean_df <- tidytable::tidytable(
    structure_smiles = "C",
    structure_tax_npc_01pat = "Alkaloids"
  )
  expect_true(
    .cache_needs_disk_normalization(
      cache_data = clean_df,
      expected_cols = c("structure_smiles", "other_col")
    )
  )
  nc_df <- tidytable::tidytable(
    structure_smiles = "C",
    structure_tax_npc_01pat = "notClassified"
  )
  expect_true(
    .cache_needs_disk_normalization(
      cache_data = nc_df,
      expected_cols = c("structure_smiles", "structure_tax_npc_01pat")
    )
  )

  # 5. .write_enriched_taxonomy_to_cache branches
  # 5a. Early return FALSE when taxonomy_table is NULL and needs_disk_norm is FALSE
  expect_false(
    .write_enriched_taxonomy_to_cache(
      enriched = clean_df,
      cache_path = file.path(tmp_dir, "noop.tsv"),
      expected_cols = names(clean_df),
      col_mapping = NULL,
      taxonomy_name = "NPC",
      existing_cache = clean_df,
      taxonomy_table = NULL,
      key_col = "structure_smiles",
      needs_disk_norm = FALSE
    )
  )

  # 5b. existing_cache = NULL reads from disk, applies col_mapping, fills missing cols, handles non-char/notClassified, key_col = NULL, writes .gz
  wb_gz <- file.path(tmp_dir, "wb_cache.tsv.gz")
  tidytable::fwrite(
    tidytable::tidytable(smiles = "C", pat = "notClassified"),
    wb_gz,
    sep = "\t",
    compress = "gzip"
  )
  src_tbl <- tidytable::tidytable(
    structure_smiles = "CC",
    num_col = factor("notClassified")
  )
  ok_wb <- .write_enriched_taxonomy_to_cache(
    enriched = src_tbl,
    cache_path = wb_gz,
    expected_cols = c(
      "structure_smiles",
      "structure_tax_npc_01pat",
      "num_col"
    ),
    col_mapping = list(
      structure_smiles = "smiles",
      structure_tax_npc_01pat = "pat"
    ),
    taxonomy_name = "NPC",
    existing_cache = NULL,
    taxonomy_table = src_tbl,
    key_col = NULL,
    needs_disk_norm = TRUE
  )
  expect_true(ok_wb)
  wb_read <- tidytable::fread(wb_gz, na.strings = "")
  expect_identical(nrow(wb_read), 2L)
  expect_true(all(is.na(wb_read$structure_tax_npc_01pat)))

  # 5c. existing_cache = NULL with nonexistent file & empty source_for_new, and error fallback when path is a directory
  wb_empty <- file.path(tmp_dir, "wb_empty.tsv")
  expect_true(
    suppressWarnings(
      .write_enriched_taxonomy_to_cache(
        enriched = tidytable::tidytable(),
        cache_path = wb_empty,
        expected_cols = "structure_smiles",
        col_mapping = NULL,
        taxonomy_name = "NPC",
        existing_cache = NULL,
        taxonomy_table = NULL,
        key_col = NULL,
        needs_disk_norm = TRUE
      )
    )
  )
  dir_path <- file.path(tmp_dir, "as_dir")
  dir.create(dir_path)
  expect_false(
    .write_enriched_taxonomy_to_cache(
      enriched = clean_df,
      cache_path = dir_path,
      expected_cols = names(clean_df),
      col_mapping = NULL,
      taxonomy_name = "NPC",
      existing_cache = clean_df,
      taxonomy_table = clean_df,
      key_col = "structure_smiles",
      needs_disk_norm = TRUE
    )
  )

  # 6. enrich_taxonomy_from_cache edge branches: NULL taxonomy_table, empty cache file, missing key_col, read error
  valid_cache_path <- file.path(tmp_dir, "valid_npc.tsv")
  tidytable::fwrite(clean_df, valid_cache_path, sep = "\t")
  res_null_tax <- enrich_taxonomy_from_cache(
    taxonomy_table = NULL,
    cache_path = valid_cache_path,
    key_col = "structure_smiles",
    all_keys = "C",
    col_mapping = list(structure_smiles = "structure_smiles"),
    taxonomy_name = "NPC"
  )
  expect_identical(nrow(res_null_tax), 1L)

  res_null_tax_nomap <- enrich_taxonomy_from_cache(
    taxonomy_table = NULL,
    cache_path = empty_file,
    key_col = "structure_smiles",
    all_keys = "C",
    col_mapping = NULL,
    taxonomy_name = "NPC"
  )
  expect_null(res_null_tax_nomap)

  wrong_key_path <- file.path(tmp_dir, "wrong_key.tsv")
  tidytable::fwrite(
    tidytable::tidytable(
      other_col = "C",
      structure_tax_npc_01pat = "Terpenoids"
    ),
    wrong_key_path,
    sep = "\t"
  )
  orig_wrong_lines <- readLines(wrong_key_path)
  expect_no_warning(
    expect_identical(
      enrich_taxonomy_from_cache(
        taxonomy_table = clean_df,
        cache_path = wrong_key_path,
        key_col = "structure_smiles",
        all_keys = c("C", "CC"),
        taxonomy_name = "NPC"
      ),
      clean_df
    )
  )
  expect_identical(readLines(wrong_key_path), orig_wrong_lines)

  # Directory as cache_path triggers read error in .read_taxonomy_cache_file and returns taxonomy_table
  expect_identical(
    suppressWarnings(
      enrich_taxonomy_from_cache(
        taxonomy_table = clean_df,
        cache_path = dir_path,
        key_col = "structure_smiles",
        all_keys = c("C", "CC"),
        taxonomy_name = "NPC"
      )
    ),
    clean_df
  )

  # fake_pubchemlite validation and placeholder generation
  expect_error(fake_pubchemlite(123), class = "tima_validation_error")
  fake_pcl <- withr::local_tempfile(fileext = ".csv")
  expect_identical(fake_pubchemlite(fake_pcl), fake_pcl)
  expect_true(file.exists(fake_pcl))
})
