# Test Suite: prepare_libraries_sop_bigg ----

library(testthat)

## Simplified test models for unit tests
test_bigg_models <- list(
  "Escherichia coli" = c(
    "model_id" = "e_coli_core",
    "doi" = "10.1234/5678"
  )
)

## Validation ----

test_that("prepare_libraries_sop_bigg validates output path parameter", {
  # Non-character output
  expect_error(
    prepare_libraries_sop_bigg(
      bigg_models = test_bigg_models,
      output = 123
    ),
    "output must be a single character string",
    class = "tima_validation_error"
  )

  # Vector output
  expect_error(
    prepare_libraries_sop_bigg(
      bigg_models = test_bigg_models,
      output = c("out1.tsv", "out2.tsv")
    ),
    "output must be a single character string",
    class = "tima_validation_error"
  )

  # NULL output
  expect_error(
    prepare_libraries_sop_bigg(
      bigg_models = test_bigg_models,
      output = NULL
    ),
    "output must be a single character string",
    class = "tima_validation_error"
  )
})

test_that("prepare_libraries_sop_bigg validates output is single value", {
  expect_error(
    prepare_libraries_sop_bigg(
      bigg_models = test_bigg_models,
      output = character(0)
    ),
    "output must be a single character string",
    class = "tima_validation_error"
  )
})

## Behavior ----

test_that("prepare_libraries_sop_bigg uses existing valid output file", {
  output_file <- temp_test_path("bigg_output_existing.tsv")

  # Create a valid existing output file (> 100000 bytes)
  mock_data <- tidytable::tidytable(
    structure_inchikey = rep("AAAABBBBCCCCDD-EEEEFFFFF-G", 5000),
    structure_inchikey_2D = rep("AAAABBBBCCCCDD", 5000),
    structure_smiles_2D = rep("C1=CC=CC=C1", 5000),
    structure_name = rep("benzene", 5000),
    organism_name = rep("Escherichia coli", 5000),
    organism_taxonomy_ottid = rep(474506L, 5000),
    reference_doi = rep("10.1234/5678", 5000)
  )
  tidytable::fwrite(x = mock_data, file = output_file, sep = "\t")

  initial_size <- file.size(output_file)
  initial_mtime <- file.mtime(output_file)

  # Run function
  Sys.sleep(0.1) # Small delay to ensure mtime difference if file is rewritten
  result <- prepare_libraries_sop_bigg(
    bigg_models = test_bigg_models,
    output = output_file
  )

  # Should return existing file without regenerating
  expect_equal(result, output_file)
  expect_equal(file.size(output_file), initial_size)
  expect_equal(file.mtime(output_file), initial_mtime)
})

test_that("prepare_libraries_sop_bigg just runs", {
  skip_if_no_network("http://bigg.ucsd.edu")
  output_file <- temp_test_path("bigg_output.tsv")

  # Create a very small invalid file
  writeLines("invalid\ndata", output_file)
  small_size <- file.size(output_file)

  expect_true(small_size < 100000)

  result <- prepare_libraries_sop_bigg(
    bigg_models = test_bigg_models,
    output = output_file
  )

  df <- tidytable::fread(result)

  # Check for expected columns
  expect_true("structure_inchikey" %in% names(df))
  expect_true("structure_name" %in% names(df))
  expect_true("organism_name" %in% names(df))

  # Check for organism taxonomy columns
  expect_true("organism_taxonomy_ottid" %in% names(df))
  expect_true("organism_taxonomy_01domain" %in% names(df))
  expect_true("organism_taxonomy_09species" %in% names(df))
  expect_true("reference_doi" %in% names(df))

  raw_content <- readLines(result, n = 2)
  expect_true(any(grepl("\t", raw_content, fixed = TRUE)))

  expect_true(file.exists(result))
  expect_equal(result, output_file)
  expect_type(result, "character")

  expect_length(result, 1L)
  expect_false(is.na(result))
  expect_false(result == "")
})

## Offline fixtures ----
# Two tiny BiGG models served from a local directory, PubChem answered by a
# mocked httr2, and RDKit replaced by a lookup of standardized structures.

.bigg_metabolite <- function(id, name, inchikey = NULL) {
  annotation <- list(bigg.metabolite = list(id))
  if (!is.null(inchikey)) {
    annotation$inchi_key <- list(inchikey)
  }
  list(id = id, name = name, compartment = "c", annotation = annotation)
}

.ik <- list(
  pyruvate = "LCTONWCANYUNTR-UHFFFAOYSA-M",
  pyruvic_acid = "LCTONWCANYUNTR-UHFFFAOYSA-N",
  alanine = "QNAYBMKLOCPYGJ-REOHCLBHSA-N",
  water = "XLYOFNOQVPJJNP-UHFFFAOYSA-N",
  ethanol = "LFQSCWFLJHTTHZ-UHFFFAOYSA-N",
  unknown = "ZZZZZZZZZZZZZZ-UHFFFAOYSA-N"
)

.bigg_fixture_models <- list(
  e_coli_test = list(
    id = "e_coli_test",
    metabolites = list(
      .bigg_metabolite("pyr_c", "Pyruvate", .ik$pyruvate),
      # Same compound in another compartment
      .bigg_metabolite("pyr_p", "Pyruvate", .ik$pyruvate),
      .bigg_metabolite("ala__L_c", "L-Alanine", .ik$alanine),
      .bigg_metabolite("h2o_c", "H2O H2O", .ik$water),
      .bigg_metabolite("h_c", "H+"),
      .bigg_metabolite("xyz_c", "Unknown compound", .ik$unknown)
    )
  ),
  yeast_test = list(
    id = "yeast_test",
    metabolites = list(
      .bigg_metabolite("pyr_c", "Pyruvate", .ik$pyruvate),
      .bigg_metabolite("h2o_c", "H2O", .ik$water),
      .bigg_metabolite("etoh_c", "Ethanol", .ik$ethanol)
    )
  )
)

# PubChem property records by InChIKey (the standardized pyruvic acid has two
# CIDs, the shorter title must win)
.pubchem_records <- list(
  list(
    CID = 107735L,
    MolecularFormula = "C3H3O3-",
    SMILES = "CC(=O)C(=O)[O-]",
    ConnectivitySMILES = "CC(=O)C(=O)[O-]",
    InChIKey = .ik$pyruvate,
    Title = "Pyruvate"
  ),
  list(
    CID = 2L,
    MolecularFormula = "C3H4O3",
    SMILES = "CC(=O)C(=O)O",
    ConnectivitySMILES = "CC(=O)C(=O)O",
    InChIKey = .ik$pyruvic_acid,
    Title = "2-Oxopropanoic acid"
  ),
  list(
    CID = 1060L,
    MolecularFormula = "C3H4O3",
    SMILES = "CC(=O)C(=O)O",
    ConnectivitySMILES = "CC(=O)C(=O)O",
    InChIKey = .ik$pyruvic_acid,
    Title = "Pyruvic acid"
  ),
  list(
    CID = 5950L,
    MolecularFormula = "C3H7NO2",
    SMILES = "C[C@@H](C(=O)O)N",
    ConnectivitySMILES = "CC(C(=O)O)N",
    InChIKey = .ik$alanine,
    Title = "L-alanine"
  ),
  list(
    CID = 962L,
    MolecularFormula = "H2O",
    SMILES = "O",
    ConnectivitySMILES = "O",
    InChIKey = .ik$water,
    Title = "Water"
  ),
  list(
    CID = 702L,
    MolecularFormula = "C2H6O",
    SMILES = "CCO",
    ConnectivitySMILES = "CCO",
    InChIKey = .ik$ethanol,
    Title = "Ethanol"
  )
)

# What RDKit standardization yields per input SMILES (charges neutralized)
.standardized <- tidytable::tidytable(
  smiles_in = c(
    "CC(=O)C(=O)[O-]",
    "CC(=O)C(=O)O",
    "C[C@@H](C(=O)O)N",
    "O",
    "CCO"
  ),
  structure_smiles = c(
    "CC(=O)C(=O)O",
    "CC(=O)C(=O)O",
    "C[C@H](N)C(=O)O",
    "O",
    "CCO"
  ),
  structure_inchikey = c(
    .ik$pyruvic_acid,
    .ik$pyruvic_acid,
    .ik$alanine,
    .ik$water,
    .ik$ethanol
  ),
  structure_molecular_formula = c(
    "C3H4O3",
    "C3H4O3",
    "C3H7NO2",
    "H2O",
    "C2H6O"
  ),
  structure_exact_mass = c(
    88.016044,
    88.016044,
    89.047678,
    18.010565,
    46.041865
  ),
  structure_smiles_no_stereo = c(
    "CC(=O)C(=O)O",
    "CC(=O)C(=O)O",
    "CC(N)C(=O)O",
    "O",
    "CCO"
  ),
  structure_xlogp = c(-0.3, -0.3, -0.6, -0.5, -0.1)
)

.fake_process_smiles <- function(
  df,
  smiles_colname = "structure_smiles_initial",
  cache = NULL
) {
  df |>
    tidytable::filter(!is.na(.data[[smiles_colname]])) |>
    tidytable::distinct(tidyselect::all_of(smiles_colname)) |>
    tidytable::inner_join(
      .standardized |>
        tidytable::rename(!!smiles_colname := "smiles_in"),
      by = smiles_colname
    ) |>
    tidytable::mutate(
      structure_inchikey_connectivity_layer = substr(structure_inchikey, 1, 14),
      structure_inchikey_no_stereo = paste0(
        substr(structure_inchikey, 1, 14),
        "-",
        substr(structure_inchikey, 27, 27)
      )
    )
}

.fake_pubchem_body <- function(url) {
  iks <- strsplit(
    sub(".*/inchikey/([^/]+)/property/.*", "\\1", url),
    ",",
    fixed = TRUE
  )[[1L]]
  hits <- Filter(function(r) r$InChIKey %in% iks, .pubchem_records)
  if (length(hits) == 0L) {
    return('{"Fault": {"Code": "PUGREST.NotFound"}}')
  }
  jsonlite::toJSON(
    list(PropertyTable = list(Properties = hits)),
    auto_unbox = TRUE
  )
}

# Writes the models as <dir>/<model_id>.json, mocks PubChem and RDKit, and
# returns the URL base and a record of the PubChem queries
local_fake_bigg <- function(
  models = .bigg_fixture_models,
  .env = parent.frame()
) {
  dir <- withr::local_tempdir(pattern = "bigg_", .local_envir = .env)
  for (m in models) {
    jsonlite::write_json(
      m,
      file.path(dir, paste0(m$id, ".json")),
      auto_unbox = TRUE
    )
  }
  calls <- new.env(parent = emptyenv())
  calls$pubchem <- character()
  local_mocked_bindings(
    req_perform = function(req, ...) req,
    resp_body_string = function(resp, ...) {
      calls$pubchem <- c(calls$pubchem, resp$url)
      .fake_pubchem_body(resp$url)
    },
    .package = "httr2",
    .env = .env
  )
  local_mocked_bindings(process_smiles = .fake_process_smiles, .env = .env)
  list(url = paste0(dir, "/"), calls = calls)
}

.two_bigg_models <- list(
  "Escherichia coli" = c(model_id = "e_coli_test", doi = "10.1038/nbt.3956"),
  "Saccharomyces cerevisiae" = c(
    model_id = "yeast_test",
    doi = "10.1186/1752-0509-3-37"
  )
)

.read_bigg <- function(path) {
  tidytable::fread(path, na.strings = "", colClasses = "character")
}

## Processing (offline) ----

test_that("prepare_libraries_sop_bigg pairs each model's structures with it", {
  fake <- local_fake_bigg()
  output <- file.path(withr::local_tempdir(), "bigg.tsv")

  prepare_libraries_sop_bigg(
    bigg_doi = "10.1093/nar/gkv1049",
    bigg_models = .two_bigg_models,
    bigg_url = fake$url,
    output = output
  )
  lib <- .read_bigg(output)

  ecoli <- lib[lib$organism_name == "Escherichia coli", ]
  yeast <- lib[lib$organism_name == "Saccharomyces cerevisiae", ]
  expect_setequal(
    ecoli$structure_name,
    c("L-alanine", "Pyruvic acid", "Water")
  )
  expect_setequal(yeast$structure_name, c("Ethanol", "Pyruvic acid", "Water"))
  # Compartment duplicates collapse into one pair
  expect_equal(sum(ecoli$structure_name == "Pyruvic acid"), 1L)
})

test_that("prepare_libraries_sop_bigg standardizes structures via PubChem", {
  fake <- local_fake_bigg()
  output <- file.path(withr::local_tempdir(), "bigg.tsv")

  prepare_libraries_sop_bigg(
    bigg_models = .two_bigg_models,
    bigg_url = fake$url,
    output = output
  )
  lib <- .read_bigg(output)
  pyr <- lib[
    lib$organism_name == "Escherichia coli" &
      lib$structure_name == "Pyruvic acid",
  ]

  # BiGG's pyruvate anion is replaced by the neutral standardized acid,
  # named after its shortest PubChem title
  expect_equal(pyr$structure_inchikey, .ik$pyruvic_acid)
  expect_equal(pyr$structure_smiles, "CC(=O)C(=O)O")
  expect_equal(pyr$structure_smiles_no_stereo, "CC(=O)C(=O)O")
  expect_equal(pyr$structure_molecular_formula, "C3H4O3")
  expect_equal(as.numeric(pyr$structure_exact_mass), 88.01604)
  expect_false(.ik$pyruvate %in% lib$structure_inchikey)

  ala <- lib[lib$structure_name == "L-alanine", ]
  expect_equal(ala$structure_inchikey, .ik$alanine)
  expect_equal(ala$structure_smiles, "C[C@H](N)C(=O)O")
  expect_equal(ala$structure_smiles_no_stereo, "CC(N)C(=O)O")

  # First pass with BiGG keys, second with the standardized ones
  expect_length(fake$calls$pubchem, 2L)
  expect_match(fake$calls$pubchem[[1L]], .ik$pyruvate, fixed = TRUE)
  expect_match(fake$calls$pubchem[[2L]], .ik$pyruvic_acid, fixed = TRUE)
  expect_no_match(fake$calls$pubchem[[2L]], .ik$pyruvate, fixed = TRUE)
})

test_that("prepare_libraries_sop_bigg skips metabolites without a structure", {
  fake <- local_fake_bigg()
  output <- file.path(withr::local_tempdir(), "bigg.tsv")

  prepare_libraries_sop_bigg(
    bigg_models = .two_bigg_models,
    bigg_url = fake$url,
    output = output
  )
  lib <- .read_bigg(output)

  # The proton has no InChIKey, the unknown key is not in PubChem
  expect_false(any(c("H+", "Unknown compound") %in% lib$structure_name))
  expect_false(.ik$unknown %in% lib$structure_inchikey)
  expect_no_match(fake$calls$pubchem[[1L]], "NULL", fixed = TRUE)
})

test_that("prepare_libraries_sop_bigg attaches organism taxonomy and DOI", {
  fake <- local_fake_bigg()
  output <- file.path(withr::local_tempdir(), "bigg.tsv")

  prepare_libraries_sop_bigg(
    bigg_models = .two_bigg_models,
    bigg_url = fake$url,
    output = output
  )
  lib <- .read_bigg(output)
  ecoli <- unique(lib[
    lib$organism_name == "Escherichia coli",
    c(
      "organism_taxonomy_ottid",
      "organism_taxonomy_01domain",
      "organism_taxonomy_02kingdom",
      "organism_taxonomy_06family",
      "organism_taxonomy_09species",
      "reference_doi"
    )
  ])
  yeast <- unique(lib[
    lib$organism_name == "Saccharomyces cerevisiae",
    c(
      "organism_taxonomy_ottid",
      "organism_taxonomy_02kingdom",
      "organism_taxonomy_08genus",
      "reference_doi"
    )
  ])

  expect_equal(nrow(ecoli), 1L)
  expect_equal(ecoli$organism_taxonomy_ottid, "474506")
  expect_equal(ecoli$organism_taxonomy_01domain, "Bacteria")
  expect_true(is.na(ecoli$organism_taxonomy_02kingdom))
  expect_equal(ecoli$organism_taxonomy_06family, "Enterobacteriaceae")
  expect_equal(ecoli$organism_taxonomy_09species, "Escherichia coli")
  expect_equal(ecoli$reference_doi, "10.1038/nbt.3956")

  expect_equal(nrow(yeast), 1L)
  expect_equal(yeast$organism_taxonomy_ottid, "356221")
  expect_equal(yeast$organism_taxonomy_02kingdom, "Fungi")
  expect_equal(yeast$organism_taxonomy_08genus, "Saccharomyces")
  expect_equal(yeast$reference_doi, "10.1186/1752-0509-3-37")
})

test_that("prepare_libraries_sop_bigg adds Biota rows for shared structures", {
  fake <- local_fake_bigg()
  output <- file.path(withr::local_tempdir(), "bigg.tsv")

  prepare_libraries_sop_bigg(
    bigg_doi = "10.1093/nar/gkv1049",
    bigg_models = .two_bigg_models,
    bigg_url = fake$url,
    output = output
  )
  lib <- .read_bigg(output)
  biota <- lib[lib$organism_name == "Biota", ]

  # Only structures present in every model
  expect_equal(sort(biota$structure_name), c("Pyruvic acid", "Water"))
  expect_equal(unique(biota$organism_taxonomy_ottid), "0")
  expect_equal(unique(biota$organism_taxonomy_01domain), "Biota")
  expect_true(all(is.na(biota$organism_taxonomy_09species)))
  expect_equal(unique(biota$reference_doi), "10.1093/nar/gkv1049")
  # Library sorted by structure name
  expect_equal(lib$structure_name, sort(lib$structure_name))
  expect_equal(nrow(lib), 8L)
})

test_that("prepare_libraries_sop_bigg omits organisms without a taxonomy", {
  fake <- local_fake_bigg()
  output <- file.path(withr::local_tempdir(), "bigg.tsv")
  models <- list(
    "Escherichia coli" = c(model_id = "e_coli_test", doi = "10.1/a"),
    "Unknown organism" = c(model_id = "yeast_test", doi = "10.1/b")
  )

  prepare_libraries_sop_bigg(
    bigg_models = models,
    bigg_url = fake$url,
    output = output
  )
  lib <- .read_bigg(output)

  expect_false("Unknown organism" %in% lib$organism_name)
  expect_setequal(
    lib$structure_name[lib$organism_name == "Escherichia coli"],
    c("L-alanine", "Pyruvic acid", "Water")
  )
})

test_that("prepare_libraries_sop_bigg writes a placeholder if a model fails", {
  fake <- local_fake_bigg()
  output <- file.path(withr::local_tempdir(), "bigg.tsv")
  models <- list(
    "Escherichia coli" = c(model_id = "missing_model", doi = "10.1/a")
  )

  expect_equal(
    prepare_libraries_sop_bigg(
      bigg_models = models,
      bigg_url = fake$url,
      output = output
    ),
    output
  )
  lib <- .read_bigg(output)

  expect_equal(names(lib), names(fake_sop_columns()))
  expect_equal(nrow(lib), 1L)
  expect_true(all(is.na(unlist(lib))))
  expect_length(fake$calls$pubchem, 0L)
})
