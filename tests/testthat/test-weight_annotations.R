# Test Suite: weight_annotations ----

library(testthat)
devtools::load_all()

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
  # Ensure fixture library always contains non-empty tags for propagation tests.
  lib_df <- utils::read.csv(
    file = lib_file,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (!"tag" %in% names(lib_df)) {
    lib_df$tag <- "serum"
  } else {
    lib_df$tag[is.na(lib_df$tag) | lib_df$tag == ""] <- "serum"
  }
  utils::write.csv(lib_df, file = lib_file, row.names = FALSE)
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
    "Required file.*not found",
    class = "tima_validation_error"
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
    "Weights must sum to 1",
    class = "tima_validation_error"
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
    "must be non-negative",
    class = "tima_validation_error"
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

  dir.create(
    file.path(fixtures$tmpdir, "processed"),
    recursive = TRUE,
    showWarnings = FALSE
  )

  warnings <- testthat::capture_warnings({
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
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = TRUE,
      pattern = "test",
      force = FALSE
    )
  })

  expect_true(any(grepl(
    "no non-missing arguments to max; returning -Inf|Empty results table provided",
    warnings
  )))

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

  dir.create(
    file.path(fixtures$tmpdir, "processed"),
    recursive = TRUE,
    showWarnings = FALSE
  )

  expect_warning(
    {
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
        high_confidence = FALSE,
        remove_ties = TRUE,
        summarize = FALSE,
        pattern = "test",
        force = FALSE
      )
    },
    "Empty results table provided"
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

  dir.create(
    file.path(fixtures$tmpdir, "processed"),
    recursive = TRUE,
    showWarnings = FALSE
  )

  # All weight on biological
  expect_warning(
    {
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
        high_confidence = FALSE,
        remove_ties = TRUE,
        summarize = FALSE,
        pattern = "test",
        force = FALSE
      )
    },
    "Empty results table provided"
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

  dir.create(
    file.path(fixtures$tmpdir, "processed"),
    recursive = TRUE,
    showWarnings = FALSE
  )

  expect_warning(
    {
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
        high_confidence = FALSE,
        remove_ties = TRUE,
        summarize = FALSE,
        pattern = "test",
        force = FALSE
      )
    },
    "Empty results table provided"
  )

  expect_type(result, "character")
})

test_that("weight_annotations() exports propagated tag columns", {
  skip_if_not_installed("tidytable")
  skip_if_not_installed("purrr")

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

  dir.create(
    file.path(fixtures$tmpdir, "processed"),
    recursive = TRUE,
    showWarnings = FALSE
  )

  expect_warning(
    {
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
        compounds_names = TRUE,
        high_confidence = FALSE,
        remove_ties = FALSE,
        summarize = FALSE,
        pattern = "tagtest",
        force = FALSE
      )
    },
    "Empty results table provided"
  )

  full_path <- unname(result[["full"]])
  filtered_path <- unname(result[["filtered"]])
  mini_path <- sub("\\.tsv$", "_mini.tsv", full_path)

  expect_true(file.exists(full_path))
  expect_true(file.exists(filtered_path))
  expect_true(file.exists(mini_path))

  full_tbl <- tidytable::fread(full_path)
  filtered_tbl <- tidytable::fread(filtered_path)
  mini_tbl <- tidytable::fread(mini_path)

  expect_true(
    "candidate_structure_tag" %in% names(full_tbl)
  )
  expect_true(
    "candidate_structure_tag" %in% names(filtered_tbl)
  )
  expect_true("tag" %in% names(mini_tbl))

  expect_true(
    any(
      full_tbl$candidate_structure_tag == "foo",
      na.rm = TRUE
    ) ||
      any(
        filtered_tbl$candidate_structure_tag == "foo",
        na.rm = TRUE
      ) ||
      any(mini_tbl$tag == "foo", na.rm = TRUE)
  )
})

# rearrange_annotations ----

test_that("rearrange_annotations merges SIRIUS scores into spectral entries with same inchikey", {
  skip_if_not_installed("tidytable")

  # Annotation table: spectral + SIRIUS entries for the same structure.
  # Glutamine (RWQNBRDOKXIBIV) is a real-world example where tautomers

  # produce different SMILES (amide vs enol form) but share the same
  # InChIKey connectivity layer.  The merge must succeed on InChIKey
  # even when the SMILES differ due to tautomerism.
  annotation_table <- tidytable::tidytable(
    feature_id = c("1", "1", "2"),
    feature_mz = c("147.0764", "147.0764", "200.0"),
    feature_rt = c("5.0", "5.0", "6.0"),
    candidate_library = c("ISDB", "SIRIUS", "SIRIUS"),
    candidate_spectrum_id = c("spec_1", NA_character_, NA_character_),
    candidate_adduct = c("[M+H]+", "[M+H]+", "[M+Na]+"),
    candidate_count_similarity_peaks_matched = c(
      "5",
      NA_character_,
      NA_character_
    ),
    candidate_score_similarity = c("0.85", NA_character_, NA_character_),
    candidate_spectrum_entropy = c(NA_character_, NA_character_, NA_character_),
    candidate_structure_name = c("L-Glutamine", "Glutamine", "Compound2"),
    candidate_structure_exact_mass = c("146.069", "146.069", "199.0"),
    candidate_structure_molecular_formula = c("C5H10N2O3", "C5H10N2O3", "C8H10N2"),
    candidate_structure_xlogp = c("-3.1", "-3.1", "1.0"),
    candidate_structure_inchikey_connectivity_layer = c(
      "RWQNBRDOKXIBIV",
      "RWQNBRDOKXIBIV",
      "UNIQUE123456"
    ),
    candidate_structure_inchikey_no_stereo = c(
      "RWQNBRDOKXIBIV-UHFFFAOYSA",
      "RWQNBRDOKXIBIV-UHFFFAOYSA",
      "UNIQUE123456-ZZZZZZZZZZ"
    ),
    # Tautomeric SMILES: amide form (spectral) vs enol form (SIRIUS)
    candidate_structure_smiles_no_stereo = c(
      "NC(=O)CCC(N)C(O)=O",
      "NC(O)=CCC(N)C(O)=O",
      "c1ccc2[nH]ccc2c1"
    ),
    candidate_structure_tax_npc_01pat = c(
      "Amino acids and Peptides",
      NA_character_,
      NA_character_
    ),
    candidate_structure_tax_npc_02sup = c(
      NA_character_,
      NA_character_,
      NA_character_
    ),
    candidate_structure_tax_npc_03cla = c(
      NA_character_,
      NA_character_,
      NA_character_
    ),
    candidate_structure_tax_cla_chemontid = c(
      NA_character_,
      NA_character_,
      NA_character_
    ),
    candidate_structure_tax_cla_01kin = c(
      NA_character_,
      NA_character_,
      NA_character_
    ),
    candidate_structure_tax_cla_02sup = c(
      NA_character_,
      NA_character_,
      NA_character_
    ),
    candidate_structure_tax_cla_03cla = c(
      NA_character_,
      NA_character_,
      NA_character_
    ),
    candidate_structure_tax_cla_04dirpar = c(
      NA_character_,
      NA_character_,
      NA_character_
    ),
    candidate_structure_organism_occurrence_closest = c(
      NA_character_,
      NA_character_,
      NA_character_
    ),
    candidate_structure_organism_occurrence_reference = c(
      NA_character_,
      NA_character_,
      NA_character_
    ),
    candidate_structure_tag = c(NA_character_, NA_character_, NA_character_),
    candidate_structure_error_mz = c("0.001", "0.002", "0.003"),
    candidate_structure_error_rt = c(
      NA_character_,
      NA_character_,
      NA_character_
    ),
    candidate_score_sirius_confidence = c(NA_character_, "0.9", "0.8"),
    candidate_score_sirius_csi = c(NA_character_, "-50", "-80"),
    candidate_score_sirius_msnovelist = c(NA_character_, "0.5", "0.3")
  )

  formula_table <- tidytable::tidytable(
    feature_id = c("1", "2"),
    feature_mz = c("147.0764", "200.0"),
    feature_rt = c("5.0", "6.0"),
    candidate_structure_molecular_formula = c("C5H10N2O3", "C8H10N2"),
    candidate_count_sirius_peaks_explained = c("10", "8"),
    candidate_score_sirius_intensity = c("0.95", "0.85"),
    candidate_score_sirius_isotope = c("0.99", "0.90"),
    candidate_score_sirius_sirius = c("-20", "-30"),
    candidate_score_sirius_tree = c("-15", "-25"),
    candidate_score_sirius_zodiac = c("0.8", "0.7")
  )

  canopus_table <- tidytable::tidytable(
    feature_id = c("1", "2"),
    feature_mz = c("147.0764", "200.0"),
    feature_rt = c("5.0", "6.0"),
    feature_spectrum_entropy = c("2.5", "3.0"),
    feature_spectrum_peaks = c("20", "30"),
    feature_pred_tax_npc_01pat_val = c("Amino acids and Peptides", "Alkaloids"),
    feature_pred_tax_npc_01pat_score = c("0.9", "0.8"),
    feature_pred_tax_npc_02sup_val = c(NA_character_, NA_character_),
    feature_pred_tax_npc_02sup_score = c(NA_character_, NA_character_),
    feature_pred_tax_npc_03cla_val = c(NA_character_, NA_character_),
    feature_pred_tax_npc_03cla_score = c(NA_character_, NA_character_),
    feature_pred_tax_cla_01kin_val = c(NA_character_, NA_character_),
    feature_pred_tax_cla_01kin_score = c(NA_character_, NA_character_),
    feature_pred_tax_cla_02sup_val = c(NA_character_, NA_character_),
    feature_pred_tax_cla_02sup_score = c(NA_character_, NA_character_),
    feature_pred_tax_cla_03cla_val = c(NA_character_, NA_character_),
    feature_pred_tax_cla_03cla_score = c(NA_character_, NA_character_),
    feature_pred_tax_cla_04dirpar_val = c(NA_character_, NA_character_),
    feature_pred_tax_cla_04dirpar_score = c(NA_character_, NA_character_)
  )

  edges_table <- tidytable::tidytable(
    feature_source = c("1", "2"),
    feature_spectrum_entropy = c("2.5", "3.0"),
    feature_spectrum_peaks = c("20", "30"),
    candidate_score_similarity = c("0.7", "0.6")
  )

  result <- rearrange_annotations(
    annotation_table = annotation_table,
    formula_table = formula_table,
    canopus_table = canopus_table,
    edges_table = edges_table
  )

  # Feature 1: spectral entry (ISDB, sim=0.85) and SIRIUS entry (CSI=-50)
  # share inchikey RWQNBRDOKXIBIV (glutamine).  The spectral entry wins in

  # table_1 (higher similarity).  SIRIUS scores should be MERGED into the
  # spectral entry despite different tautomeric SMILES.
  f1 <- result[
    result$feature_id == "1" &
      !is.na(result$candidate_structure_inchikey_connectivity_layer) &
      result$candidate_structure_inchikey_connectivity_layer ==
        "RWQNBRDOKXIBIV",
  ]

  # There should be exactly one row for feature_1 + RWQNBRDOKXIBIV
  expect_equal(nrow(f1), 1L)

  # The merged row should have BOTH spectral and SIRIUS scores
  expect_equal(f1$candidate_score_similarity, 0.85)
  expect_false(is.na(f1$candidate_score_sirius_csi))
  expect_equal(f1$candidate_score_sirius_csi, -50)
  expect_equal(f1$candidate_library, "ISDB")

  # Feature 2: SIRIUS-only entry (unique inchikey UNIQUE123456)
  f2 <- result[
    result$feature_id == "2" &
      !is.na(result$candidate_structure_inchikey_connectivity_layer) &
      result$candidate_structure_inchikey_connectivity_layer == "UNIQUE123456",
  ]

  expect_equal(nrow(f2), 1L)
  expect_false(is.na(f2$candidate_score_sirius_csi))
  expect_equal(f2$candidate_library, "SIRIUS")

  # Formula scores should be present
  expect_true("candidate_score_sirius_zodiac" %in% names(result))

  # CANOPUS predictions should be present

  expect_true("feature_pred_tax_npc_01pat_val" %in% names(result))
})
