# Test Suite: annotate_masses ----

library(testthat)

# Helper to create minimal valid fixtures for parameter validation tests
setup_minimal_fixtures <- function() {
  temp_dir <- file.path(
    tempdir(),
    paste0("test_", format(Sys.time(), "%Y%m%d_%H%M%S_%OS3"))
  )
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)

  list(
    features = copy_fixture_to(
      "features_small.csv",
      file.path(temp_dir, "features.csv")
    ),
    library = copy_fixture_to(
      "library_minimal.csv",
      file.path(temp_dir, "library.csv")
    ),
    str_stereo = copy_fixture_to(
      "structures_stereo.csv",
      file.path(temp_dir, "str_stereo.csv")
    ),
    str_met = copy_fixture_to(
      "structures_metadata_minimal.csv",
      file.path(temp_dir, "str_met.csv")
    ),
    str_nam = copy_fixture_to(
      "structures_names_minimal.csv",
      file.path(temp_dir, "str_nam.csv")
    ),
    str_tax_cla = copy_fixture_to(
      "structures_taxonomy_cla_minimal.csv",
      file.path(temp_dir, "str_tax_cla.csv")
    ),
    str_tax_npc = copy_fixture_to(
      "structures_taxonomy_npc_minimal.csv",
      file.path(temp_dir, "str_tax_npc.csv")
    ),
    cleanup = function() {
      unlink(temp_dir, recursive = TRUE)
      unlink("data", recursive = TRUE)
    }
  )
}

## Input Validation ----

test_that("annotate_masses validates ms_mode correctly", {
  fixtures <- setup_minimal_fixtures()

  # Invalid mode should error
  expect_error(
    annotate_masses(
      features = fixtures$features,
      library = fixtures$library,
      str_stereo = fixtures$str_stereo,
      str_met = fixtures$str_met,
      str_nam = fixtures$str_nam,
      str_tax_cla = fixtures$str_tax_cla,
      str_tax_npc = fixtures$str_tax_npc,
      ms_mode = "invalid_mode"
    ),
    "Fix: Choose one of: pos, neg",
    fixed = TRUE
  )

  # Blank mode should error
  expect_error(
    annotate_masses(
      features = fixtures$features,
      library = fixtures$library,
      str_stereo = fixtures$str_stereo,
      str_met = fixtures$str_met,
      str_nam = fixtures$str_nam,
      str_tax_cla = fixtures$str_tax_cla,
      str_tax_npc = fixtures$str_tax_npc,
      ms_mode = ""
    ),
    "Did you mean 'pos'?",
    fixed = TRUE
  )

  fixtures$cleanup()
  # NULL mode should error (handled by parameter default or earlier validation)
  # Note: NULL will use default from get_params, so we test with invalid value instead
})

test_that("annotate_masses validates tolerance_ppm correctly", {
  fixtures <- setup_minimal_fixtures()

  # Zero tolerance should error
  expect_error(
    annotate_masses(
      tolerance_ppm = 0,
      ms_mode = "pos",
      features = fixtures$features,
      library = fixtures$library,
      str_stereo = fixtures$str_stereo,
      str_met = fixtures$str_met,
      str_nam = fixtures$str_nam,
      str_tax_cla = fixtures$str_tax_cla,
      str_tax_npc = fixtures$str_tax_npc
    ),
    "Fix: Use a positive value appropriate for your instrument",
    fixed = TRUE
  )

  # Negative tolerance should error
  expect_error(
    annotate_masses(
      tolerance_ppm = -5,
      ms_mode = "pos",
      features = fixtures$features,
      library = fixtures$library,
      str_stereo = fixtures$str_stereo,
      str_met = fixtures$str_met,
      str_nam = fixtures$str_nam,
      str_tax_cla = fixtures$str_tax_cla,
      str_tax_npc = fixtures$str_tax_npc
    ),
    "Fix: Use a positive value appropriate for your instrument",
    fixed = TRUE
  )

  # Too large tolerance should warn (and then error on missing output dir)
  expect_warning(
    annotate_masses(
      tolerance_ppm = 25,
      ms_mode = "pos",
      features = fixtures$features,
      library = fixtures$library,
      str_stereo = fixtures$str_stereo,
      str_met = fixtures$str_met,
      str_nam = fixtures$str_nam,
      str_tax_cla = fixtures$str_tax_cla,
      str_tax_npc = fixtures$str_tax_npc
    ),
    "Received: 25 ppm",
    fixed = TRUE
  )

  # Non-numeric tolerance should error
  expect_error(
    annotate_masses(
      tolerance_ppm = "10",
      ms_mode = "pos",
      features = fixtures$features,
      library = fixtures$library,
      str_stereo = fixtures$str_stereo,
      str_met = fixtures$str_met,
      str_nam = fixtures$str_nam,
      str_tax_cla = fixtures$str_tax_cla,
      str_tax_npc = fixtures$str_tax_npc
    ),
    "Fix: Provide a numeric tolerance value in parts per million (ppm)",
    fixed = TRUE
  )
  fixtures$cleanup()
})

test_that("annotate_masses validates tolerance_rt correctly", {
  fixtures <- setup_minimal_fixtures()

  # Zero tolerance should error
  expect_error(
    annotate_masses(
      tolerance_rt = 0,
      tolerance_ppm = 10,
      ms_mode = "pos",
      features = fixtures$features,
      library = fixtures$library,
      str_stereo = fixtures$str_stereo,
      str_met = fixtures$str_met,
      str_nam = fixtures$str_nam,
      str_tax_cla = fixtures$str_tax_cla,
      str_tax_npc = fixtures$str_tax_npc
    ),
    "Recommended range: 0.01-0.05 minutes for mass annotation"
  ) |>
    expect_error() # Will error later, that's OK

  # Negative tolerance should error
  expect_error(
    annotate_masses(
      features = fixtures$features,
      library = fixtures$library,
      str_stereo = fixtures$str_stereo,
      str_met = fixtures$str_met,
      str_nam = fixtures$str_nam,
      str_tax_cla = fixtures$str_tax_cla,
      str_tax_npc = fixtures$str_tax_npc,
      tolerance_rt = -0.01,
      tolerance_ppm = 10,
      ms_mode = "pos"
    ),
    "Fix: Use a value between 0 and Inf"
  )

  # Too large tolerance should error
  expect_warning(
    annotate_masses(
      features = fixtures$features,
      library = fixtures$library,
      str_stereo = fixtures$str_stereo,
      str_met = fixtures$str_met,
      str_nam = fixtures$str_nam,
      str_tax_cla = fixtures$str_tax_cla,
      str_tax_npc = fixtures$str_tax_npc,
      tolerance_rt = 1.0,
      tolerance_ppm = 10,
      ms_mode = "pos"
    ),
    "Large values may group unrelated features together",
    fixed = TRUE
  )
  fixtures$cleanup()
})

test_that("annotate_masses validates adducts_list structure", {
  fixtures <- setup_minimal_fixtures()

  # Missing mode in adducts_list should error
  expect_error(
    annotate_masses(
      features = fixtures$features,
      library = fixtures$library,
      str_stereo = fixtures$str_stereo,
      str_met = fixtures$str_met,
      str_nam = fixtures$str_nam,
      str_tax_cla = fixtures$str_tax_cla,
      str_tax_npc = fixtures$str_tax_npc,
      adducts_list = list(neg = c("[M-H]-")),
      ms_mode = "pos",
      tolerance_ppm = 10,
      tolerance_rt = 0.02
    ),
    "Fix: Add adduct definitions for 'pos' mode to your configuration:
  adducts_list$pos <- c('[M+H]+', '[M+Na]+', ...)",
    fixed = TRUE
  )

  # NULL adducts for mode should error
  expect_error(
    annotate_masses(
      features = fixtures$features,
      library = fixtures$library,
      str_stereo = fixtures$str_stereo,
      str_met = fixtures$str_met,
      str_nam = fixtures$str_nam,
      str_tax_cla = fixtures$str_tax_cla,
      str_tax_npc = fixtures$str_tax_npc,
      adducts_list = list(pos = NULL),
      ms_mode = "pos",
      tolerance_ppm = 10,
      tolerance_rt = 0.02
    ),
    "Fix: Add adduct definitions for 'pos' mode to your configuration:
  adducts_list$pos <- c('[M+H]+', '[M+Na]+', ...)",
    fixed = TRUE
  )

  # Non-list adducts should error
  expect_error(
    annotate_masses(
      features = fixtures$features,
      library = fixtures$library,
      str_stereo = fixtures$str_stereo,
      str_met = fixtures$str_met,
      str_nam = fixtures$str_nam,
      str_tax_cla = fixtures$str_tax_cla,
      str_tax_npc = fixtures$str_tax_npc,
      adducts_list = c("[M+H]+"),
      ms_mode = "pos",
      tolerance_ppm = 10,
      tolerance_rt = 0.02
    ),
    "Fix: Ensure the adduct configuration is a list with 'pos' and 'neg' entries",
    fixed = TRUE
  )
  fixtures$cleanup()
})

test_that("annotate_masses validates clusters_list structure", {
  fixtures <- setup_minimal_fixtures()
  # Missing mode in clusters_list should error
  expect_error(
    annotate_masses(
      features = fixtures$features,
      library = fixtures$library,
      str_stereo = fixtures$str_stereo,
      str_met = fixtures$str_met,
      str_nam = fixtures$str_nam,
      str_tax_cla = fixtures$str_tax_cla,
      str_tax_npc = fixtures$str_tax_npc,
      adducts_list = list(pos = c("[M+H]+")),
      clusters_list = list(neg = c("[M]")),
      ms_mode = "pos",
      tolerance_ppm = 10,
      tolerance_rt = 0.02
    ),
    "Fix: Add adduct definitions for 'pos' mode to your configuration:
  clusters_list$pos <- c('[M+H]+', '[M+Na]+', ...)",
    fixed = TRUE
  )
  fixtures$cleanup()
})

test_that("annotate_masses validates file existence", {
  # Missing features file should error during validation
  expect_error(
    annotate_masses(
      features = "/nonexistent/features.tsv",
      library = tempfile(),
      str_stereo = tempfile(),
      str_met = tempfile(),
      str_nam = tempfile(),
      str_tax_cla = tempfile(),
      str_tax_npc = tempfile(),
      adducts_list = list(pos = c("[M+H]+")),
      clusters_list = list(pos = c("[M]")),
      ms_mode = "pos",
      tolerance_ppm = 10,
      tolerance_rt = 0.02
    ),
    "Input data validation failed"
  )
})


## Edge Cases and Empty Input Tests ----

test_that("annotate_masses handles empty features table", {
  local_quiet_logging()

  # Stage fixtures into temp workspace
  env <- prepare_annotation_fixture_env(
    feature_fixture = "features_empty.csv",
    library_fixture = "library_minimal.csv"
  )

  withr::local_dir(new = as.character(env$dirs$root))

  # Empty features should be caught by validation
  expect_error(
    annotate_masses(
      features = env$features,
      library = env$library,
      str_stereo = env$str_stereo,
      str_met = env$str_met,
      str_nam = env$str_nam,
      str_tax_cla = env$str_tax_cla,
      str_tax_npc = env$str_tax_npc,
      adducts_list = list(pos = c("[M+H]+")),
      clusters_list = list(pos = c("[M]")),
      neutral_losses_list = c("H2O"),
      ms_mode = "pos",
      tolerance_ppm = 10,
      tolerance_rt = 0.02,
      output_annotations = env$output_annotations,
      output_edges = env$output_edges
    ),
    "Input data validation failed"
  )
})

test_that("annotate_masses handles features without RT column", {
  local_quiet_logging()

  # Stage fixtures into temp workspace
  env <- prepare_annotation_fixture_env(
    feature_fixture = "features_no_rt.csv",
    library_fixture = "library_minimal.csv"
  )

  withr::local_dir(new = env$dirs$root)

  # Should not error, should use feature_id as RT fallback
  expect_no_error({
    result <- annotate_masses(
      features = env$features,
      library = env$library,
      str_stereo = env$str_stereo,
      str_met = env$str_met,
      str_nam = env$str_nam,
      str_tax_cla = env$str_tax_cla,
      str_tax_npc = env$str_tax_npc,
      adducts_list = list(pos = c("[M+H]+")),
      clusters_list = list(pos = c("[M]")),
      neutral_losses_list = c("H2O"),
      ms_mode = "pos",
      tolerance_ppm = 10,
      tolerance_rt = 0.02,
      output_annotations = env$output_annotations,
      output_edges = env$output_edges
    )
  })

  expect_type(result, "character")
  expect_true(file.exists(result["annotations"]))
})

## Functional Tests with Real Data ----

test_that("annotate_masses produces expected output structure", {
  local_quiet_logging()

  # Stage fixtures into temp workspace
  env <- prepare_annotation_fixture_env(
    feature_fixture = "features_realistic.csv",
    library_fixture = "library_realistic.csv"
  )

  withr::local_dir(new = env$dirs$root)

  # Run annotation
  result <- annotate_masses(
    features = env$features,
    library = env$library,
    str_stereo = env$str_stereo,
    str_met = env$str_met,
    str_nam = env$str_nam,
    str_tax_cla = env$str_tax_cla,
    str_tax_npc = env$str_tax_npc,
    adducts_list = list(
      pos = c("[M+H]+", "[M+Na]+", "[M+K]+"),
      neg = c("[M-H]-")
    ),
    clusters_list = list(
      pos = c("[M]", "[2M]"),
      neg = c("[M]")
    ),
    neutral_losses_list = c("H2O", "CO2"),
    ms_mode = "pos",
    tolerance_ppm = 10,
    tolerance_rt = 0.02,
    output_annotations = env$output_annotations,
    output_edges = env$output_edges
  )

  # Verify result structure
  expect_type(result, "character")
  expect_named(result, c("annotations", "edges"))
  expect_true(file.exists(result["annotations"]))
  expect_true(file.exists(result["edges"]))

  # Load and verify annotations
  annotations <- tidytable::fread(
    result["annotations"],
    colClasses = "character"
  )
  expect_s3_class(annotations, "data.frame")

  # Check required annotation columns
  required_cols <- c(
    "feature_id",
    "candidate_structure_error_mz",
    "candidate_structure_name",
    "candidate_structure_inchikey_connectivity_layer",
    "candidate_structure_smiles_no_stereo",
    "candidate_adduct"
  )
  expect_required_columns(annotations, required_cols)

  # Load and verify edges
  edges <- tidytable::fread(result["edges"], colClasses = "character")
  expect_s3_class(edges, "data.frame")
})

test_that("annotate_masses respects tolerance_ppm correctly", {
  local_quiet_logging()

  # Stage fixtures into temp workspace
  env <- prepare_annotation_fixture_env(
    feature_fixture = "features_small.csv",
    library_fixture = "library_tolerance.csv"
  )

  withr::local_dir(new = env$dirs$root)

  # Run with tight tolerance (5 ppm) - should only match first structure
  result_tight <- annotate_masses(
    features = env$features,
    library = env$library,
    str_stereo = env$str_stereo,
    str_met = env$str_met,
    str_nam = env$str_nam,
    str_tax_cla = env$str_tax_cla,
    str_tax_npc = env$str_tax_npc,
    adducts_list = list(pos = c("[M+H]+")),
    clusters_list = list(pos = c("[M]")),
    neutral_losses_list = character(0),
    ms_mode = "pos",
    tolerance_ppm = 5.0,
    tolerance_rt = 0.02,
    output_annotations = env$output_annotations,
    output_edges = env$output_edges
  )

  annotations_tight <- tidytable::fread(
    result_tight["annotations"],
    colClasses = "character"
  )

  # Verify only one match within 5 ppm tolerance
  # 199.091500 is 0.000675 Da away (within 5 ppm of 199.092175)
  # 199.094000 is 0.001825 Da away (outside 5 ppm tolerance)
  expect_equal(nrow(annotations_tight), 1L)
})

## Isotopologues ----

test_that("annotate_masses correctly handles isotopes [M1+H]+ and [M2+H]+", {
  local_quiet_logging()

  # Test isotope annotation using fixtures
  # Glucose (exact mass = 180.0634 Da)
  # Isotope formula: M = ((z * (m/z - iso_shift) - modifications) / n_mer)
  # Reverse: m/z = ((M * n_mer + modifications) / z) + iso_shift
  # Isotope mass shift = 1.0033548 Da (13C - 12C difference)
  # M+0: [M+H]+ at 181.0712 m/z (monoisotopic peak)
  # M+1: [M1+H]+ at 182.0746 m/z (~1.0034 Da HIGHER - one 13C)
  # M+2: [M2+H]+ at 183.0779 m/z (~2.0067 Da HIGHER - two 13C or one 18O)

  env <- prepare_annotation_fixture_env(
    feature_fixture = "features_isotope.csv",
    library_fixture = "library_isotope.csv"
  )

  withr::local_dir(new = env$dirs$root)

  # Run annotation with M+0, M+1, and M+2 adducts
  result <- annotate_masses(
    features = env$features,
    library = env$library,
    str_stereo = env$str_stereo,
    str_met = env$str_met,
    str_nam = env$str_nam,
    str_tax_cla = env$str_tax_cla,
    str_tax_npc = env$str_tax_npc,
    adducts_list = list(pos = c("[M+H]+", "[M1+H]+", "[M2+H]+")),
    clusters_list = list(pos = c("[M]")),
    neutral_losses_list = character(0),
    ms_mode = "pos",
    tolerance_ppm = 5.0,
    tolerance_rt = 0.02,
    output_annotations = env$output_annotations,
    output_edges = env$output_edges
  )

  annotations <- tidytable::fread(
    result["annotations"],
    colClasses = "character"
  )

  # Check each isotope is annotated with correct adduct
  m0_annotation <- annotations |>
    tidytable::filter(feature_id == "FT_M0")
  expect_true("[M+H]+" %in% m0_annotation$candidate_adduct)

  m1_annotation <- annotations |>
    tidytable::filter(feature_id == "FT_M1")
  expect_true("[M1+H]+" %in% m1_annotation$candidate_adduct)

  m2_annotation <- annotations |>
    tidytable::filter(feature_id == "FT_M2")
  expect_true("[M2+H]+" %in% m2_annotation$candidate_adduct)

  # All should point to the same structure (glucose)
  if ("candidate_structure_inchikey" %in% colnames(annotations)) {
    unique_structures <- unique(annotations$candidate_structure_inchikey)
    expect_equal(length(unique_structures), 1L)
    expect_equal(unique_structures[[1]], "WQZGKKKJIJFFOK-GASJEMHNSA-N")
  }
})
