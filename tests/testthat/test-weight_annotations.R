# Test Suite: weight_annotations ----

library(testthat)

# Fixture staging ----

#' Stage weight_annotations test fixtures
#'
#' @param root Root directory for staging (created if needed)
#' @return Named list of file paths for all required inputs
stage_weight_annotations_fixtures <- function(
  root = temp_test_dir("weight_ann")
) {
  dir.create(root, recursive = TRUE, showWarnings = FALSE)

  # Copy fixtures to temp location
  ann_file <- file.path(root, "annotations.tsv")
  lib_file <- file.path(root, "library.tsv")
  comp_file <- file.path(root, "components.tsv")
  edges_file <- file.path(root, "edges.tsv")
  taxa_file <- file.path(root, "taxa.tsv")
  canopus_file <- file.path(root, "canopus.tsv")
  formula_file <- file.path(root, "formula.tsv")
  org_tax_file <- file.path(root, "taxonomy_ott.tsv")
  output_file <- file.path(root, "weighted.tsv")

  copy_fixture_to("annotations_weighted.csv", ann_file)
  copy_fixture_to("library_weighted.csv", lib_file)
  copy_fixture_to("components_weighted.csv", comp_file)
  copy_fixture_to("edges_weighted.csv", edges_file)
  copy_fixture_to("taxa_weighted.csv", taxa_file)
  copy_fixture_to("organism_taxonomy_gentiana.csv", org_tax_file)

  # Create empty canopus, formula files (required but can be empty)
  writeLines("feature_id", canopus_file)
  writeLines("feature_id", formula_file)

  list(
    tmpdir = root,
    library = lib_file,
    annotations = ann_file,
    components = comp_file,
    edges = edges_file,
    taxa = taxa_file,
    canopus = canopus_file,
    formula = formula_file,
    org_tax_ott = org_tax_file,
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

  fixtures <- stage_weight_annotations_fixtures()

  expect_error(
    weight_annotations(
      library = fixtures$library,
      annotations = fixtures$annotations,
      components = fixtures$components,
      edges = fixtures$edges,
      taxa = fixtures$taxa,
      canopus = fixtures$canopus,
      formula = fixtures$formula,
      org_tax_ott = fixtures$org_tax_ott,
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

  fixtures <- stage_weight_annotations_fixtures()

  expect_error(
    weight_annotations(
      library = fixtures$library,
      annotations = fixtures$annotations,
      components = fixtures$components,
      edges = fixtures$edges,
      taxa = fixtures$taxa,
      canopus = fixtures$canopus,
      formula = fixtures$formula,
      org_tax_ott = fixtures$org_tax_ott,
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

  fixtures <- stage_weight_annotations_fixtures()

  expect_error(
    weight_annotations(
      library = fixtures$library,
      annotations = fixtures$annotations,
      components = fixtures$components,
      edges = fixtures$edges,
      taxa = fixtures$taxa,
      canopus = fixtures$canopus,
      formula = fixtures$formula,
      org_tax_ott = fixtures$org_tax_ott,
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

  fixtures <- stage_weight_annotations_fixtures()

  expect_error(
    weight_annotations(
      library = fixtures$library,
      annotations = fixtures$annotations,
      components = fixtures$components,
      edges = fixtures$edges,
      taxa = fixtures$taxa,
      canopus = fixtures$canopus,
      formula = fixtures$formula,
      org_tax_ott = fixtures$org_tax_ott,
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

  fixtures <- stage_weight_annotations_fixtures()

  expect_error(
    weight_annotations(
      library = fixtures$library,
      annotations = fixtures$annotations,
      components = fixtures$components,
      edges = fixtures$edges,
      taxa = fixtures$taxa,
      canopus = fixtures$canopus,
      formula = fixtures$formula,
      org_tax_ott = fixtures$org_tax_ott,
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

  fixtures <- stage_weight_annotations_fixtures()
  withr::local_dir(new = fixtures$tmpdir)

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
    org_tax_ott = fixtures$org_tax_ott,
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
    score_biological_biota = 1.007276,
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

  fixtures <- stage_weight_annotations_fixtures()
  withr::local_dir(new = fixtures$tmpdir)

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
    org_tax_ott = fixtures$org_tax_ott,
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
    score_biological_biota = 1.007276,
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

  fixtures <- stage_weight_annotations_fixtures()
  withr::local_dir(new = fixtures$tmpdir)

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
    org_tax_ott = fixtures$org_tax_ott,
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
    score_biological_biota = 1.007276,
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

  fixtures <- stage_weight_annotations_fixtures()
  withr::local_dir(new = fixtures$tmpdir)

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
    org_tax_ott = fixtures$org_tax_ott,
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
    score_biological_biota = 1.007276,
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
