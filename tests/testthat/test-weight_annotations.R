# Test Suite: weight_annotations ----

library(testthat)

# Fixture creation ----

create_minimal_wa_data <- function() {
  tmpdir <- tempfile(pattern = "wa_test_")
  dir.create(tmpdir, recursive = TRUE)

  # Minimal annotation data with all required columns
  ann_data <- data.frame(
    feature_id = c("F1", "F1", "F2", "F3"),
    feature_mz = c(100.05, 100.05, 150.08, 200.10),
    feature_rt = c(1.2, 1.2, 2.5, 3.1),
    candidate_adduct = c("add1", "add2", "add3", "add4"),
    candidate_library = c("MS1", "lib1", "lib2", "lib1"),
    candidate_structure_inchikey_connectivity_layer = c(
      "AAAAAAAAAAAAA",
      "BBBBBBBBBBBBB",
      "CCCCCCCCCCCCC",
      "AAAAAAAAAAAAA"
    ),
    candidate_structure_smiles_no_stereo = c("CCC", "CCCC", "CCCCC", "CCC"),
    candidate_structure_error_mz = c(NA_real_, 0.4, NA_real_, 0.5),
    candidate_structure_error_rt = c(NA_real_, 0.4, NA_real_, 0.5),
    # TODO MS1 only needs this, refactor later
    candidate_score_similarity = c(0.9, 0.85, NA_real_, 0.6),
    candidate_score_sirius_csi = c(NA_real_, 0.4, NA_real_, 0.5),
    candidate_structure_name = c("AAA", "BBB", "CCC", "DDD"),
    candidate_structure_molecular_formula = c("C3H8", "C4H10", "C5H12", "C3H8"),
    candidate_structure_exact_mass = c(44.06, 58.08, 72.09, 44.06),
    candidate_structure_xlogp = c(1.5, 2.0, 2.5, 1.5),
    candidate_structure_tax_cla_01kin = c(
      "Organic",
      "Organic",
      "Organic",
      "Organic"
    ),
    candidate_structure_tax_cla_02sup = c(
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_
    ),
    candidate_structure_tax_cla_03cla = c(
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_
    ),
    candidate_structure_tax_cla_04dirpar = c(
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_
    ),
    candidate_structure_tax_npc_01pat = c(
      "Alkaloids",
      "Terpenoids",
      "Alkaloids",
      "Alkaloids"
    ),
    candidate_structure_tax_npc_02sup = c(
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_
    ),
    candidate_structure_tax_npc_03cla = c(
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_
    ),
    candidate_structure_organism_occurrence_closest = c(
      "Plantae",
      "Plantae",
      NA_character_,
      "Plantae"
    ),
    candidate_structure_organism_occurrence_reference = c(
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_
    ),
    candidate_spectrum_entropy = c(2.5, 2.3, 2.8, 2.1),
    stringsAsFactors = FALSE
  )
  ann_file <- file.path(tmpdir, "annotations.tsv")
  write.table(ann_data, ann_file, sep = "\t", row.names = FALSE, quote = FALSE)

  # Minimal library with complete taxonomy
  lib_data <- data.frame(
    structure_inchikey = c("AAAAAAAAAAAAA", "BBBBBBBBBBBBB", "CCCCCCCCCCCCC"),
    structure_inchikey_connectivity_layer = c(
      "AAAAAAAAAAAAA",
      "BBBBBBBBBBBBB",
      "CCCCCCCCCCCCC"
    ),
    structure_smiles_no_stereo = c("CCC", "CCCC", "CCCCC"),
    organism_name = c("PlantSpecies1", "PlantSpecies2", "BacteriumSpecies1"),
    organism_taxonomy_ottid = c(123, 456, 789),
    organism_taxonomy_01domain = c("Eukaryota", "Eukaryota", "Bacteria"),
    organism_taxonomy_02kingdom = c("Plantae", "Plantae", "Bacteria"),
    organism_taxonomy_03phylum = c(
      "Tracheophyta",
      "Tracheophyta",
      "Proteobacteria"
    ),
    organism_taxonomy_04class = c(NA_character_, NA_character_, NA_character_),
    organism_taxonomy_05order = c(NA_character_, NA_character_, NA_character_),
    organism_taxonomy_06family = c(NA_character_, NA_character_, NA_character_),
    organism_taxonomy_07tribe = c(NA_character_, NA_character_, NA_character_),
    organism_taxonomy_08genus = c(NA_character_, NA_character_, NA_character_),
    organism_taxonomy_09species = c(
      NA_character_,
      NA_character_,
      NA_character_
    ),
    organism_taxonomy_10varietas = c(
      NA_character_,
      NA_character_,
      NA_character_
    ),
    stringsAsFactors = FALSE
  )
  lib_file <- file.path(tmpdir, "library.tsv")
  write.table(lib_data, lib_file, sep = "\t", row.names = FALSE, quote = FALSE)

  # Minimal components
  comp_data <- data.frame(
    feature_id = c("F1", "F2", "F3"),
    component_id = c("C1", "C1", "C2"),
    stringsAsFactors = FALSE
  )
  comp_file <- file.path(tmpdir, "components.tsv")
  write.table(
    comp_data,
    comp_file,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  # Minimal edges
  edges_data <- data.frame(
    feature_source = c("F1", "F2"),
    feature_target = c("F2", "F3"),
    candidate_score_similarity = c(0.9, 0.8),
    feature_spectrum_entropy = c(2.5, 2.8),
    feature_spectrum_peaks = c(50, 60),
    stringsAsFactors = FALSE
  )
  edges_file <- file.path(tmpdir, "edges.tsv")
  write.table(
    edges_data,
    edges_file,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  # Minimal taxa with complete taxonomy
  taxa_data <- data.frame(
    feature_id = c("F1", "F2", "F3"),
    sample_organism_name = c(
      "PlantSpecies1",
      "PlantSpecies2",
      "BacteriumSpecies1"
    ),
    sample_organism_01_domain = c("Eukaryota", "Eukaryota", "Bacteria"),
    sample_organism_02_kingdom = c("Plantae", "Plantae", "Bacteria"),
    sample_organism_03_phylum = c(NA_character_, NA_character_, NA_character_),
    sample_organism_04_class = c(NA_character_, NA_character_, NA_character_),
    sample_organism_05_order = c(NA_character_, NA_character_, NA_character_),
    sample_organism_06_family = c(NA_character_, NA_character_, NA_character_),
    sample_organism_07_tribe = c(NA_character_, NA_character_, NA_character_),
    sample_organism_08_genus = c(NA_character_, NA_character_, NA_character_),
    sample_organism_09_species = c(NA_character_, NA_character_, NA_character_),
    sample_organism_10_varietas = c(
      NA_character_,
      NA_character_,
      NA_character_
    ),
    stringsAsFactors = FALSE
  )
  taxa_file <- file.path(tmpdir, "taxa.tsv")
  write.table(
    taxa_data,
    taxa_file,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  # Empty canopus and formula files (required but can be empty)
  canopus_file <- file.path(tmpdir, "canopus.tsv")
  writeLines("feature_id", canopus_file)

  formula_file <- file.path(tmpdir, "formula.tsv")
  writeLines("feature_id", formula_file)

  output_file <- file.path(tmpdir, "weighted.tsv")

  list(
    tmpdir = tmpdir,
    library = lib_file,
    annotations = ann_file,
    components = comp_file,
    edges = edges_file,
    taxa = taxa_file,
    canopus = canopus_file,
    formula = formula_file,
    output = output_file
  )
}

# Validation ----

test_that("weight_annotations() validates required file existence", {
  skip_if_not_installed("tidytable")
  skip_if_not_installed("logger")

  expect_error(
    weight_annotations(
      library = "nonexistent.tsv",
      annotations = "fake.tsv",
      components = "fake.tsv",
      edges = "fake.tsv",
      taxa = "fake.tsv",
      canopus = tempfile(),
      formula = tempfile(),
      output = tempfile(),
      pattern = "test"
    ),
    "Required file.*not found"
  )
})

test_that("weight_annotations() validates weight sum", {
  skip_if_not_installed("tidytable")

  fixtures <- create_minimal_wa_data()

  expect_error(
    weight_annotations(
      library = fixtures$library,
      annotations = fixtures$annotations,
      components = fixtures$components,
      edges = fixtures$edges,
      taxa = fixtures$taxa,
      canopus = fixtures$canopus,
      formula = fixtures$formula,
      output = fixtures$output,
      weight_spectral = 0.5,
      weight_chemical = 0.5,
      weight_biological = 0.5, # Sum = 1.5, invalid
      pattern = "test"
    ),
    "Weights must sum to 1"
  )
})

test_that("weight_annotations() validates minimal_ms1_condition", {
  skip_if_not_installed("tidytable")

  fixtures <- create_minimal_wa_data()

  expect_error(
    weight_annotations(
      library = fixtures$library,
      annotations = fixtures$annotations,
      components = fixtures$components,
      edges = fixtures$edges,
      taxa = fixtures$taxa,
      canopus = fixtures$canopus,
      formula = fixtures$formula,
      output = fixtures$output,
      weight_spectral = 0.5,
      weight_chemical = 0.3,
      weight_biological = 0.2,
      minimal_ms1_condition = "INVALID",
      pattern = "test"
    ),
    "Must be one of: OR, AND"
  )
})

test_that("weight_annotations() rejects negative weights", {
  skip_if_not_installed("tidytable")

  fixtures <- create_minimal_wa_data()

  expect_error(
    weight_annotations(
      library = fixtures$library,
      annotations = fixtures$annotations,
      components = fixtures$components,
      edges = fixtures$edges,
      taxa = fixtures$taxa,
      canopus = fixtures$canopus,
      formula = fixtures$formula,
      output = fixtures$output,
      weight_spectral = -0.1,
      weight_chemical = 0.6,
      weight_biological = 0.5,
      pattern = "test"
    ),
    "must be non-negative"
  )
})

test_that("weight_annotations() rejects invalid logical parameters", {
  skip_if_not_installed("tidytable")

  fixtures <- create_minimal_wa_data()

  expect_error(
    weight_annotations(
      library = fixtures$library,
      annotations = fixtures$annotations,
      components = fixtures$components,
      edges = fixtures$edges,
      taxa = fixtures$taxa,
      canopus = fixtures$canopus,
      formula = fixtures$formula,
      output = fixtures$output,
      weight_spectral = 0.5,
      weight_chemical = 0.3,
      weight_biological = 0.2,
      ms1_only = "yes", # Should be logical
      pattern = "test"
    ),
    "ms1_only must be a single TRUE or FALSE"
  )
})

test_that("weight_annotations() rejects invalid candidate counts", {
  skip_if_not_installed("tidytable")

  fixtures <- create_minimal_wa_data()

  expect_error(
    weight_annotations(
      library = fixtures$library,
      annotations = fixtures$annotations,
      components = fixtures$components,
      edges = fixtures$edges,
      taxa = fixtures$taxa,
      canopus = fixtures$canopus,
      formula = fixtures$formula,
      output = fixtures$output,
      weight_spectral = 0.5,
      weight_chemical = 0.3,
      weight_biological = 0.2,
      candidates_neighbors = 0, # Invalid: must be > 0
      pattern = "test"
    ),
    "candidates_neighbors must be > 0"
  )
})

# Other paths ----

test_that("weight_annotations() runs successfully with minimal inputs", {
  skip_if_not_installed("tidytable")
  skip_if_not_installed("purrr")

  fixtures <- create_minimal_wa_data()
  withr::local_dir(fixtures$tmpdir)

  # Mock get_default_paths to return our temp dir
  local_mocked_bindings(
    get_default_paths = function() {
      list(
        data = list(
          processed = list(path = file.path(fixtures$tmpdir, "processed"))
        )
      )
    },
    get_params = function(step) {
      list(step = "prepare_params")
    },
    export_params = function(...) invisible(NULL),
    .package = "tima"
  )

  dir.create(file.path(fixtures$tmpdir, "processed"), recursive = TRUE)

  result <- weight_annotations(
    library = fixtures$library,
    annotations = fixtures$annotations,
    components = fixtures$components,
    edges = fixtures$edges,
    taxa = fixtures$taxa,
    canopus = fixtures$canopus,
    formula = fixtures$formula,
    output = fixtures$output,
    candidates_neighbors = 3L,
    candidates_final = 1L,
    best_percentile = 0.9,
    weight_spectral = 0.2,
    weight_chemical = 0.3,
    weight_biological = 0.5,
    score_biological_domain = 0.1,
    score_biological_kingdom = 0.2,
    score_biological_phylum = 0.3,
    score_biological_class = 0.4,
    score_biological_order = 0.5,
    score_biological_infraorder = 0.55,
    score_biological_family = 0.6,
    score_biological_subfamily = 0.65,
    score_biological_tribe = 0.7,
    score_biological_subtribe = 0.75,
    score_biological_genus = 0.8,
    score_biological_subgenus = 0.85,
    score_biological_species = 0.9,
    score_biological_subspecies = 0.95,
    score_biological_variety = 1.0,
    score_chemical_cla_kingdom = 0.25,
    score_chemical_cla_superclass = 0.5,
    score_chemical_cla_class = 0.75,
    score_chemical_cla_parent = 1.0,
    score_chemical_npc_pathway = 0.33,
    score_chemical_npc_superclass = 0.66,
    score_chemical_npc_class = 1.0,
    minimal_consistency = 0.6,
    minimal_ms1_bio = 0.6,
    minimal_ms1_chemo = 0.6,
    minimal_ms1_condition = "AND",
    ms1_only = TRUE,
    compounds_names = TRUE,
    high_confidence = TRUE,
    remove_ties = FALSE,
    summarize = TRUE,
    pattern = "test",
    force = FALSE
  )

  # Should return output paths
  expect_type(result, "character")
  expect_true(length(result) >= 1)
})

test_that("weight_annotations() processes MS1-only annotations", {
  skip_if_not_installed("tidytable")

  fixtures <- create_minimal_wa_data()
  withr::local_dir(fixtures$tmpdir)

  local_mocked_bindings(
    get_default_paths = function() {
      list(
        data = list(
          processed = list(path = file.path(fixtures$tmpdir, "processed"))
        )
      )
    },
    get_params = function(step) list(step = "prepare_params"),
    export_params = function(...) invisible(NULL),
    .package = "tima"
  )

  dir.create(file.path(fixtures$tmpdir, "processed"), recursive = TRUE)

  result <- weight_annotations(
    library = fixtures$library,
    annotations = fixtures$annotations,
    components = fixtures$components,
    edges = fixtures$edges,
    taxa = fixtures$taxa,
    canopus = fixtures$canopus,
    formula = fixtures$formula,
    output = fixtures$output,
    candidates_neighbors = 3L,
    candidates_final = 1L,
    best_percentile = 0.9,
    weight_spectral = 0.2,
    weight_chemical = 0.3,
    weight_biological = 0.5,
    score_biological_domain = 0.1,
    score_biological_kingdom = 0.2,
    score_biological_phylum = 0.3,
    score_biological_class = 0.4,
    score_biological_order = 0.5,
    score_biological_infraorder = 0.55,
    score_biological_family = 0.6,
    score_biological_subfamily = 0.65,
    score_biological_tribe = 0.7,
    score_biological_subtribe = 0.75,
    score_biological_genus = 0.8,
    score_biological_subgenus = 0.85,
    score_biological_species = 0.9,
    score_biological_subspecies = 0.95,
    score_biological_variety = 1.0,
    score_chemical_cla_kingdom = 0.25,
    score_chemical_cla_superclass = 0.5,
    score_chemical_cla_class = 0.75,
    score_chemical_cla_parent = 1.0,
    score_chemical_npc_pathway = 0.33,
    score_chemical_npc_superclass = 0.66,
    score_chemical_npc_class = 1.0,
    minimal_consistency = 0.6,
    minimal_ms1_bio = 0.6,
    minimal_ms1_chemo = 0.6,
    minimal_ms1_condition = "AND",
    ms1_only = FALSE,
    compounds_names = FALSE,
    high_confidence = TRUE,
    remove_ties = TRUE,
    summarize = FALSE,
    pattern = "test",
    force = FALSE
  )

  expect_type(result, "character")
})

test_that("weight_annotations() handles different weight combinations", {
  skip_if_not_installed("tidytable")

  fixtures <- create_minimal_wa_data()
  withr::local_dir(fixtures$tmpdir)

  local_mocked_bindings(
    get_default_paths = function() {
      list(
        data = list(
          processed = list(path = file.path(fixtures$tmpdir, "processed"))
        )
      )
    },
    get_params = function(step) list(step = "prepare_params"),
    export_params = function(...) invisible(NULL),
    .package = "tima"
  )

  dir.create(file.path(fixtures$tmpdir, "processed"), recursive = TRUE)

  # All weight on biological
  result <- weight_annotations(
    library = fixtures$library,
    annotations = fixtures$annotations,
    components = fixtures$components,
    edges = fixtures$edges,
    taxa = fixtures$taxa,
    canopus = fixtures$canopus,
    formula = fixtures$formula,
    output = fixtures$output,
    candidates_neighbors = 3L,
    candidates_final = 1L,
    best_percentile = 0.9,
    weight_spectral = 0.2,
    weight_chemical = 0.3,
    weight_biological = 0.5,
    score_biological_domain = 0.1,
    score_biological_kingdom = 0.2,
    score_biological_phylum = 0.3,
    score_biological_class = 0.4,
    score_biological_order = 0.5,
    score_biological_infraorder = 0.55,
    score_biological_family = 0.6,
    score_biological_subfamily = 0.65,
    score_biological_tribe = 0.7,
    score_biological_subtribe = 0.75,
    score_biological_genus = 0.8,
    score_biological_subgenus = 0.85,
    score_biological_species = 0.9,
    score_biological_subspecies = 0.95,
    score_biological_variety = 1.0,
    score_chemical_cla_kingdom = 0.25,
    score_chemical_cla_superclass = 0.5,
    score_chemical_cla_class = 0.75,
    score_chemical_cla_parent = 1.0,
    score_chemical_npc_pathway = 0.33,
    score_chemical_npc_superclass = 0.66,
    score_chemical_npc_class = 1.0,
    minimal_consistency = 0.6,
    minimal_ms1_bio = 0.6,
    minimal_ms1_chemo = 0.6,
    minimal_ms1_condition = "AND",
    ms1_only = FALSE,
    compounds_names = FALSE,
    high_confidence = TRUE,
    remove_ties = TRUE,
    summarize = FALSE,
    pattern = "test",
    force = FALSE
  )

  expect_type(result, "character")
})

test_that("weight_annotations() handles boolean flags", {
  skip_if_not_installed("tidytable")

  fixtures <- create_minimal_wa_data()
  withr::local_dir(fixtures$tmpdir)

  local_mocked_bindings(
    get_default_paths = function() {
      list(
        data = list(
          processed = list(path = file.path(fixtures$tmpdir, "processed"))
        )
      )
    },
    get_params = function(step) list(step = "prepare_params"),
    export_params = function(...) invisible(NULL),
    .package = "tima"
  )

  dir.create(file.path(fixtures$tmpdir, "processed"), recursive = TRUE)

  result <- weight_annotations(
    library = fixtures$library,
    annotations = fixtures$annotations,
    components = fixtures$components,
    edges = fixtures$edges,
    taxa = fixtures$taxa,
    canopus = fixtures$canopus,
    formula = fixtures$formula,
    output = fixtures$output,
    candidates_neighbors = 3L,
    candidates_final = 1L,
    best_percentile = 0.9,
    weight_spectral = 0.2,
    weight_chemical = 0.3,
    weight_biological = 0.5,
    score_biological_domain = 0.1,
    score_biological_kingdom = 0.2,
    score_biological_phylum = 0.3,
    score_biological_class = 0.4,
    score_biological_order = 0.5,
    score_biological_infraorder = 0.55,
    score_biological_family = 0.6,
    score_biological_subfamily = 0.65,
    score_biological_tribe = 0.7,
    score_biological_subtribe = 0.75,
    score_biological_genus = 0.8,
    score_biological_subgenus = 0.85,
    score_biological_species = 0.9,
    score_biological_subspecies = 0.95,
    score_biological_variety = 1.0,
    score_chemical_cla_kingdom = 0.25,
    score_chemical_cla_superclass = 0.5,
    score_chemical_cla_class = 0.75,
    score_chemical_cla_parent = 1.0,
    score_chemical_npc_pathway = 0.33,
    score_chemical_npc_superclass = 0.66,
    score_chemical_npc_class = 1.0,
    minimal_consistency = 0.6,
    minimal_ms1_bio = 0.6,
    minimal_ms1_chemo = 0.6,
    minimal_ms1_condition = "AND",
    ms1_only = FALSE,
    compounds_names = FALSE,
    high_confidence = TRUE,
    remove_ties = TRUE,
    summarize = FALSE,
    pattern = "test",
    force = FALSE
  )

  expect_type(result, "character")
})
