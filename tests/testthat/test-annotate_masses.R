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
      str_tax_cla = fixtures$str_tax_cla,
      str_tax_npc = fixtures$str_tax_npc,
      ms_mode = "invalid_mode"
    ),
    "Fix: Choose one of: pos, neg",
    class = "tima_validation_error",
    fixed = TRUE
  )

  # Blank mode should error
  expect_error(
    annotate_masses(
      features = fixtures$features,
      library = fixtures$library,
      str_stereo = fixtures$str_stereo,
      str_met = fixtures$str_met,
      str_tax_cla = fixtures$str_tax_cla,
      str_tax_npc = fixtures$str_tax_npc,
      ms_mode = ""
    ),
    "Did you mean 'pos'?",
    class = "tima_validation_error",
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
      str_tax_cla = fixtures$str_tax_cla,
      str_tax_npc = fixtures$str_tax_npc
    ),
    "Fix: Use a positive value appropriate for your instrument",
    class = "tima_validation_error",
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
      str_tax_cla = fixtures$str_tax_cla,
      str_tax_npc = fixtures$str_tax_npc
    ),
    "Fix: Use a positive value appropriate for your instrument",
    class = "tima_validation_error",
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
      str_tax_cla = fixtures$str_tax_cla,
      str_tax_npc = fixtures$str_tax_npc
    ),
    "Fix: Provide a numeric tolerance value in parts per million (ppm)",
    class = "tima_validation_error",
    fixed = TRUE
  )
  fixtures$cleanup()
})

test_that("annotate_masses validates tolerance_rt correctly", {
  fixtures <- setup_minimal_fixtures()

  # Negative tolerance should error
  expect_error(
    annotate_masses(
      features = fixtures$features,
      library = fixtures$library,
      str_stereo = fixtures$str_stereo,
      str_met = fixtures$str_met,
      str_tax_cla = fixtures$str_tax_cla,
      str_tax_npc = fixtures$str_tax_npc,
      tolerance_rt = -0.01,
      tolerance_ppm = 10,
      ms_mode = "pos"
    ),
    "Use a value between 0 and Inf",
    class = "tima_validation_error"
  )

  # Too large tolerance should error
  expect_warning(
    annotate_masses(
      features = fixtures$features,
      library = fixtures$library,
      str_stereo = fixtures$str_stereo,
      str_met = fixtures$str_met,
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
      str_tax_cla = fixtures$str_tax_cla,
      str_tax_npc = fixtures$str_tax_npc,
      adducts_list = list(neg = c("[M-H]-")),
      ms_mode = "pos",
      tolerance_ppm = 10,
      tolerance_rt = 0.02
    ),
    "Fix: Add adduct definitions for 'pos' mode to your configuration",
    class = "tima_validation_error",
    fixed = TRUE
  )

  # NULL adducts for mode should error
  expect_error(
    annotate_masses(
      features = fixtures$features,
      library = fixtures$library,
      str_stereo = fixtures$str_stereo,
      str_met = fixtures$str_met,
      str_tax_cla = fixtures$str_tax_cla,
      str_tax_npc = fixtures$str_tax_npc,
      adducts_list = list(pos = NULL),
      ms_mode = "pos",
      tolerance_ppm = 10,
      tolerance_rt = 0.02
    ),
    "Fix: Add adduct definitions for 'pos' mode to your configuration",
    class = "tima_validation_error",
    fixed = TRUE
  )

  # Non-list adducts should error
  expect_error(
    annotate_masses(
      features = fixtures$features,
      library = fixtures$library,
      str_stereo = fixtures$str_stereo,
      str_met = fixtures$str_met,
      str_tax_cla = fixtures$str_tax_cla,
      str_tax_npc = fixtures$str_tax_npc,
      adducts_list = c("[M+H]+"),
      ms_mode = "pos",
      tolerance_ppm = 10,
      tolerance_rt = 0.02
    ),
    "Fix: Ensure the adduct configuration is a list with 'pos' and 'neg' entries",
    class = "tima_validation_error",
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
      str_tax_cla = fixtures$str_tax_cla,
      str_tax_npc = fixtures$str_tax_npc,
      adducts_list = list(pos = c("[M+H]+")),
      clusters_list = list(neg = c("[M]")),
      ms_mode = "pos",
      tolerance_ppm = 10,
      tolerance_rt = 0.02
    ),
    "Fix: Add adduct definitions for 'pos' mode to your configuration",
    class = "tima_validation_error",
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
      str_tax_cla = tempfile(),
      str_tax_npc = tempfile(),
      adducts_list = list(pos = c("[M+H]+")),
      clusters_list = list(pos = c("[M]")),
      ms_mode = "pos",
      tolerance_ppm = 10,
      tolerance_rt = 0.02
    ),
    "Input data validation failed",
    class = "tima_validation_error"
  )
})


## Edge Cases and Empty Input Tests ----

test_that("annotate_masses returns empty outputs for an empty features table", {
  seen <- new.env(parent = emptyenv())
  seen$exports <- list()

  local_mocked_bindings(
    log_operation = function(...) list(id = "annotate_masses_ctx"),
    sanitize_all_inputs = function(features_file = NULL, ...) invisible(NULL),
    validate_ms_mode = function(ms_mode) invisible(NULL),
    validate_tolerances = function(...) invisible(NULL),
    validate_adduct_list = function(...) invisible(NULL),
    validate_file_existence = function(...) invisible(NULL),
    safe_fread = function(file, file_type, required_cols = NULL, ...) {
      data.frame(feature_id = character())
    },
    export_output = function(x, file) {
      seen$exports[[length(seen$exports) + 1L]] <- list(x = x, file = file)
      invisible(file)
    }
  )

  result <- annotate_masses(
    features = "features.tsv",
    library = "library.tsv",
    str_stereo = "stereo.tsv",
    str_met = "metadata.tsv",
    str_tax_cla = "cla.tsv",
    str_tax_npc = "npc.tsv",
    output_annotations = "annotations.tsv",
    output_edges = "edges.tsv",
    adducts_list = list(pos = c("[M+H]+")),
    clusters_list = list(pos = c("[M]")),
    neutral_losses_list = c("H2O"),
    ms_mode = "pos",
    tolerance_ppm = 10,
    tolerance_rt = 0.02
  )

  expect_identical(
    unname(result),
    c("annotations.tsv", "edges.tsv")
  )
  expect_length(seen$exports, 3L)
  expect_identical(seen$exports[[1L]]$file, "annotations.tsv")
  expect_identical(seen$exports[[2L]]$file, "edges.tsv")
  expect_identical(seen$exports[[3L]]$file, "annotations_coverage.tsv")
  expect_equal(nrow(seen$exports[[1L]]$x), 0L)
  expect_true(all(
    c("CLUSTERID1", "CLUSTERID2", "label") %in% names(seen$exports[[2L]]$x)
  ))
  expect_true(all(
    c("coverage_class", "N_features", "N_annotations") %in%
      names(seen$exports[[3L]]$x)
  ))
  expect_true(all(
    c("coverage_scope", "coverage_tier") %in%
      names(seen$exports[[3L]]$x)
  ))
})

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
    "Input data validation failed",
    class = "tima_validation_error"
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

  coverage_file <- sub("\\.tsv(\\.gz)?$", "_coverage.tsv\\1", result["annotations"])
  expect_true(file.exists(coverage_file))
  coverage <- tidytable::fread(coverage_file, colClasses = "character")
  expect_required_columns(
    coverage,
    c(
      "coverage_scope",
      "coverage_class",
      "coverage_tier",
      "N_features",
      "N_annotations",
      "Pct_features",
      "Pct_annotations"
    )
  )
})

test_that("coverage report distinguishes feature-level tiers and scopes", {
  annotations <- tidytable::tidytable(
    feature_id = c("F_STR", "F_PAIR", "F_MULTI", "F_BASE", "F_MOD"),
    adduct = c("[M+H]+", "[M+Na]+", "[M+2H]2+", "[M+H]+", "[M+H+H2O]+"),
    source = c("pair", "pair", "multi", "baseline", "loss"),
    candidate_structure_error_mz = c(0.01, NA, NA, NA, NA),
    adduct_support = c(3L, 2L, 2L, 0L, 1L)
  )

  report <- build_annotate_masses_coverage_report(
    annotations = annotations,
    baseline_adduct = "[M+H]+"
  )

  expect_true(all(c("coverage_scope", "coverage_class", "coverage_tier") %in% names(report)))
  expect_true(any(report$coverage_scope == "best" & report$coverage_class == "structure_matched"))
  expect_true(any(report$coverage_scope == "best" & report$coverage_class == "evidence_multicharge_supported"))
  expect_true(any(report$coverage_scope == "best" & report$coverage_class == "baseline_fallback"))
  expect_true(any(report$coverage_scope == "any" & report$coverage_class == "modifier_pairwise_supported"))
  expect_true(any(report$coverage_scope == "best" & report$coverage_class == "all"))
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

  # New behaviour: adduct hypotheses are kept even when no library structure
  # matches. So we filter on actual library hits (= rows with a structure).
  library_hits <- annotations_tight |>
    tidytable::filter(
      !is.na(candidate_structure_inchikey_connectivity_layer) &
        nzchar(candidate_structure_inchikey_connectivity_layer)
    )

  # Verify only one structural match within 5 ppm tolerance
  # 199.092500 is 0.000224 Da away (within 5 ppm of implied M)
  # 199.095000 is 0.002276 Da away (outside 5 ppm tolerance)
  expect_equal(nrow(library_hits), 1L)
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
    expect_equal(unique_structures[[1L]], "WQZGKKKJIJFFOK-GASJEMHNSA-N")
  }
})

test_that("join_couples_with_neutral_losses preserves neutral-loss mass values", {
  df_couples_diff <- tidytable::tidytable(
    feature_id = "F1",
    adduct = "[M+H]+",
    feature_id_dest = "F2",
    adduct_dest = "[M+Na]+",
    delta_min = 17.9,
    delta_max = 18.1
  )
  neutral_losses <- tidytable::tidytable(
    loss = c("H2O loss", "NH3 loss"),
    mass = c(18.010565, 17.026549)
  )

  out <- join_couples_with_neutral_losses(df_couples_diff, neutral_losses)

  expect_equal(nrow(out), 1L)
  expect_equal(out$loss[[1L]], "H2O loss")
  expect_equal(out$mass[[1L]], 18.010565, tolerance = 1e-8)
})

test_that("annotate_masses keeps neutral-loss edges even when adduct-pair edge exists", {
  local_quiet_logging()

  env <- prepare_annotation_fixture_env(
    feature_fixture = "features_overlap_adduct_nh3.csv",
    library_fixture = "library_overlap_adduct_nh3.csv"
  )

  withr::local_dir(new = env$dirs$root)

  result <- annotate_masses(
    features = env$features,
    library = env$library,
    str_stereo = env$str_stereo,
    str_met = env$str_met,
    str_tax_cla = env$str_tax_cla,
    str_tax_npc = env$str_tax_npc,
    adducts_list = list(pos = c("[M+H]+", "[M+NH4]+"), neg = c("[M-H]-")),
    clusters_list = list(pos = c("[M]"), neg = c("[M]")),
    neutral_losses_list = c("NH3"),
    ms_mode = "pos",
    tolerance_ppm = 10,
    tolerance_rt = 0.02,
    output_annotations = env$output_annotations,
    output_edges = env$output_edges
  )

  edges <- tidytable::fread(result[["edges"]], colClasses = "character")

  expect_true(any(edges$label == "[M+H]+ _ [M+H4N]+"))
  expect_true(any(edges$label == "NH3 loss"))
})

test_that("join_multi_with_addlossed preserves observed matched mass", {
  df_multi <- tidytable::tidytable(
    feature_id = "F1",
    adduct = "[2M+Na]+",
    rt = 5,
    mz = 500,
    rt_min = 4.9,
    rt_max = 5.1,
    mass_min = 100,
    mass_max = 110
  )
  df_addlossed_rdy <- tidytable::tidytable(
    feature_id = "X",
    rt = 5.0,
    mz = 250,
    adduct = "[M+H]+",
    mass = 105.4321
  )

  out <- join_multi_with_addlossed(df_multi, df_addlossed_rdy)

  expect_equal(nrow(out), 1L)
  expect_equal(out$feature_id[[1L]], "F1")
  expect_equal(out$mass[[1L]], 105.4321, tolerance = 1e-8)
})

test_that("build_adduct_pair_differences preserves multiple adduct mappings for same delta", {
  add_clu_table <- tidytable::tidytable(
    adduct = c("A", "B", "C"),
    adduct_mass = c(0, 10, 20)
  )

  out <- build_adduct_pair_differences(
    add_clu_table = add_clu_table,
    tolerance_ppm = 1,
    max_mz = 1000
  )

  delta_10 <- out |>
    tidytable::filter(abs(Distance - 10) < 1e-10)

  expect_equal(nrow(delta_10), 2L)
  expect_true(any(delta_10$Group1 == "A" & delta_10$Group2 == "B"))
  expect_true(any(delta_10$Group1 == "B" & delta_10$Group2 == "C"))
})

test_that("canonicalize_adduct_notation keeps carrier terms last", {
  out <- canonicalize_adduct_notation("[M+H+H2O]+")
  expect_equal(out, "[M+H2O+H]+")
})

test_that("propagate_preassigned_over_adduct_edges propagates in both directions", {
  adduct_edges <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    adduct = c("[M+H]+", "[M+Na]+"),
    adduct_dest = c("[M+Na]+", "[M+K]+"),
    feature_id_dest = c("F2", "F3")
  )
  preassigned <- tidytable::tidytable(
    feature_id = c("F1", "F3"),
    adduct = c("[M+H]+", "[M+K]+")
  )

  out <- propagate_preassigned_over_adduct_edges(adduct_edges, preassigned)

  expect_true(any(out$feature_id == "F2" & out$adduct == "[M+Na]+"))
  expect_equal(
    nrow(out |> tidytable::filter(feature_id == "F2", adduct == "[M+Na]+")),
    1L
  )
})

test_that("extract_preassigned_adducts accepts upstream candidate_adduct values", {
  features_table <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    candidate_adduct = c("[M+H]+", NA_character_)
  )

  out <- extract_preassigned_adducts(features_table)

  expect_true(any(out$feature_id == "F1" & out$adduct == "[M+H]+"))
})

test_that("extract_preassigned_adducts expands multi-valued adduct cells", {
  features_table <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    candidate_adduct = c("[M+H]+|[M+Na]+", "[M+K]+/[M+H]+"),
    adduct = c(NA_character_, NA_character_)
  )

  out <- extract_preassigned_adducts(features_table)

  f1 <- out |> tidytable::filter(feature_id == "F1")
  f2 <- out |> tidytable::filter(feature_id == "F2")
  expect_true("[M+H]+" %in% f1$adduct)
  expect_true("[M+Na]+" %in% f1$adduct)
  expect_true("[M+K]+" %in% f2$adduct)
  expect_true("[M+H]+" %in% f2$adduct)
})

test_that("dedupe_node_hypotheses keeps preassigned provenance even if pair source wins", {
  hyps <- tidytable::tidytable(
    feature_id = c("F1", "F1"),
    adduct = c("[M+H]+", "[M+H]+"),
    source = c("pair", "preassigned"),
    is_preassigned = c(FALSE, TRUE),
    adduct_support = c(2L, 0L)
  )

  out <- dedupe_node_hypotheses(hyps)

  expect_equal(nrow(out), 1L)
  expect_true(isTRUE(out$is_preassigned[[1L]]))
  expect_equal(out$source[[1L]], "pair")
})

test_that("prune_candidates_by_network_consensus keeps rows flagged as preassigned", {
  matched <- tidytable::tidytable(
    feature_id = c("F1", "F1"),
    rt = c(1, 1),
    mz = c(100, 100),
    adduct = c("[M+H]+", "[M+Na]+"),
    mass = c(99, 99),
    source = c("pair", "pair"),
    is_preassigned = c(TRUE, FALSE),
    adduct_support = c(0L, 0L),
    structure_exact_mass = c(99, 99),
    error_mz = c(0, 0)
  )
  adduct_edges <- tidytable::tidytable(
    feature_id = "F1",
    adduct = "[M+Na]+",
    adduct_dest = "[M+K]+",
    feature_id_dest = "F2"
  )

  out <- prune_candidates_by_network_consensus(
    matched = matched,
    adduct_edges = adduct_edges,
    baseline_adduct = "[M+H]+"
  )

  expect_true(any(out$adduct == "[M+H]+"))
  expect_true(any(out$adduct == "[M+Na]+"))
})

test_that("prune_candidates_by_network_consensus matches support by adduct state key", {
  matched <- tidytable::tidytable(
    feature_id = c("F1", "F1"),
    rt = c(1, 1),
    mz = c(100, 100),
    adduct = c("[M+H+H2O]+", "[M+K]+"),
    mass = c(99, 99),
    source = c("pair", "pair"),
    is_preassigned = c(FALSE, FALSE),
    adduct_support = c(0L, 0L),
    structure_exact_mass = c(99, 99),
    error_mz = c(0, 0)
  )
  adduct_edges <- tidytable::tidytable(
    feature_id = "F1",
    adduct = "[M+H2O+H]+",
    adduct_dest = "[M+Na]+",
    feature_id_dest = "F2"
  )

  out <- prune_candidates_by_network_consensus(
    matched = matched,
    adduct_edges = adduct_edges,
    baseline_adduct = "[M+H]+"
  )

  expect_true(any(out$adduct == "[M+H+H2O]+"))
  expect_false(any(out$adduct == "[M+K]+"))
})

test_that("enforce_annotation_edge_adduct_agreement keeps enforced-origin rows", {
  annotations <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    adduct = c("[M+H]+", "[2M+Fe]2+"),
    candidate_adduct_origin = c("enforced", "supported"),
    source = c("baseline", "multi")
  )
  adduct_edges <- tidytable::tidytable(
    feature_id = character(),
    adduct = character(),
    adduct_dest = character(),
    feature_id_dest = character()
  )

  out <- enforce_annotation_edge_adduct_agreement(
    annotations = annotations,
    adduct_edges = adduct_edges,
    baseline_adduct = "[M+H]+"
  )

  expect_true(any(out$feature_id == "F1" & out$adduct == "[M+H]+"))
  expect_false(any(out$feature_id == "F2" & out$adduct == "[2M+Fe]2+"))
})

test_that("enforce_annotation_edge_adduct_agreement keeps preassigned-origin rows", {
  annotations <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    adduct = c("[M+H]+", "[2M+Fe]2+"),
    candidate_adduct_origin = c("preassigned", "supported"),
    source = c("preassigned", "multi")
  )
  adduct_edges <- tidytable::tidytable(
    feature_id = character(),
    adduct = character(),
    adduct_dest = character(),
    feature_id_dest = character()
  )

  out <- enforce_annotation_edge_adduct_agreement(
    annotations = annotations,
    adduct_edges = adduct_edges,
    baseline_adduct = "[M+H]+"
  )

  expect_true(any(out$feature_id == "F1" & out$adduct == "[M+H]+"))
  expect_false(any(out$feature_id == "F2" & out$adduct == "[2M+Fe]2+"))
})

test_that("enforce_annotation_edge_adduct_agreement keeps edge-supported and safe adducts", {
  annotations <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3", "F4"),
    adduct = c("[M+H]+", "[M+Na]+", "[2M+Fe]2+", "[M+K]+"),
    source = c("pair", "baseline", "multi", "preassigned")
  )
  adduct_edges <- tidytable::tidytable(
    feature_id = "F1",
    adduct = "[M+H]+",
    adduct_dest = "[M+Na]+",
    feature_id_dest = "F5"
  )

  out <- enforce_annotation_edge_adduct_agreement(
    annotations = annotations,
    adduct_edges = adduct_edges,
    baseline_adduct = "[M+H]+"
  )

  expect_true(any(out$feature_id == "F1" & out$adduct == "[M+H]+"))
  expect_true(any(out$feature_id == "F2" & out$adduct == "[M+Na]+"))
  expect_true(any(out$feature_id == "F4" & out$adduct == "[M+K]+"))
  expect_false(any(out$feature_id == "F3" & out$adduct == "[2M+Fe]2+"))
})

test_that("enforce_annotation_edge_adduct_agreement matches equivalent adduct orderings", {
  annotations <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    adduct = c("[M+H+H2O]+", "[2M+Fe]2+"),
    source = c("pair", "multi")
  )
  adduct_edges <- tidytable::tidytable(
    feature_id = "F1",
    adduct = "[M+H2O+H]+",
    adduct_dest = "[M+Na]+",
    feature_id_dest = "F9"
  )

  out <- enforce_annotation_edge_adduct_agreement(
    annotations = annotations,
    adduct_edges = adduct_edges,
    baseline_adduct = "[M+H]+"
  )

  expect_true(any(out$feature_id == "F1" & out$adduct == "[M+H+H2O]+"))
  expect_false(any(out$feature_id == "F2" & out$adduct == "[2M+Fe]2+"))
})

test_that("enforce_annotation_edge_adduct_agreement keeps neutral-loss-derived adducts", {
  annotations <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    adduct = c("[M-C6H10O5+H]+", "[2M+Fe]2+"),
    source = c("loss", "multi")
  )
  adduct_edges <- tidytable::tidytable(
    feature_id = character(),
    adduct = character(),
    adduct_dest = character(),
    feature_id_dest = character()
  )

  out <- enforce_annotation_edge_adduct_agreement(
    annotations = annotations,
    adduct_edges = adduct_edges,
    baseline_adduct = "[M+H]+"
  )

  expect_true(any(out$feature_id == "F1" & out$adduct == "[M-C6H10O5+H]+"))
  expect_false(any(out$feature_id == "F2" & out$adduct == "[2M+Fe]2+"))
})

test_that("enforce_annotation_edge_adduct_agreement keeps candidates when feature has no edge support", {
  annotations <- tidytable::tidytable(
    feature_id = c("F1", "F1"),
    adduct = c("[M+Na]+", "[M+K]+"),
    source = c("pair", "pair")
  )
  adduct_edges <- tidytable::tidytable(
    feature_id = character(),
    adduct = character(),
    adduct_dest = character(),
    feature_id_dest = character()
  )

  out <- enforce_annotation_edge_adduct_agreement(
    annotations = annotations,
    adduct_edges = adduct_edges,
    baseline_adduct = "[M+H]+"
  )

  expect_equal(nrow(out), 2L)
  expect_true("[M+Na]+" %in% out$adduct)
  expect_true("[M+K]+" %in% out$adduct)
})

test_that("enforce_loss_formula_compatibility demotes incompatible loss-based structural matches", {
  annotations <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    adduct = c("[M-C6H10O5+H]+", "[M-H2O+H]+"),
    candidate_structure_molecular_formula = c("C5H12O6", "C10H20O5"),
    candidate_structure_error_mz = c("0.001", "0.002"),
    candidate_structure_name = c("hexose", "hydrated"),
    candidate_library = c("TIMA MS1", "TIMA MS1"),
    source = c("loss", "loss"),
    loss_term = c("C6H10O5", "H2O")
  )

  out <- enforce_loss_formula_compatibility(annotations)

  row_f1 <- out |> tidytable::filter(feature_id == "F1")
  row_f2 <- out |> tidytable::filter(feature_id == "F2")
  expect_true(is.na(row_f1$candidate_structure_error_mz[[1L]]))
  expect_true(is.na(row_f1$candidate_structure_name[[1L]]))
  expect_true(is.na(row_f1$candidate_library[[1L]]))
  expect_false(is.na(row_f2$candidate_structure_error_mz[[1L]]))
})

test_that("enforce_loss_formula_compatibility keeps rows when formula is unavailable", {
  annotations <- tidytable::tidytable(
    feature_id = "F1",
    adduct = "[M-H2O+H]+",
    candidate_structure_molecular_formula = NA_character_,
    candidate_structure_error_mz = "0.001",
    candidate_structure_name = "unknown_formula",
    candidate_library = "TIMA MS1",
    source = "loss",
    loss_term = "H2O"
  )

  out <- enforce_loss_formula_compatibility(annotations)

  expect_false(is.na(out$candidate_structure_error_mz[[1L]]))
  expect_false(is.na(out$candidate_structure_name[[1L]]))
  expect_false(is.na(out$candidate_library[[1L]]))
})

test_that("loss_term_atom_requirements extracts multiplicities correctly", {
  req <- loss_term_atom_requirements("2H2O")
  expect_equal(unname(req["H"]), 4L)
  expect_equal(unname(req["O"]), 2L)
})

test_that("adduct_loss_atom_requirements remains a backwards-compatible alias", {
  req <- adduct_loss_atom_requirements("[M-C6H10O5+H]+")
  expect_equal(unname(req["C"]), 6L)
  expect_equal(unname(req["H"]), 10L)
  expect_equal(unname(req["O"]), 5L)
})

test_that("apply_adduct_consistency_filter keeps sparse single-hypothesis edges in conditional mode", {
  df_add <- tidytable::tidytable(
    feature_id = c("F1"),
    adduct = c("[M+H]+"),
    adduct_dest = c("[M+Na]+"),
    feature_id_dest = c("F2")
  )

  out <- apply_adduct_consistency_filter(
    df_add = df_add,
    adduct_consistency = "conditional",
    adduct_min_support = 2L,
    adduct_consistency_min_degree = 3L
  )

  expect_equal(nrow(out), 1L)
})

test_that("apply_adduct_consistency_filter prunes ambiguous unsupported edges", {
  df_add <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F1", "F2", "F3", "F2"),
    adduct = c("A", "X", "A", "A", "A", "A"),
    adduct_dest = c("B", "Y", "B", "B", "B", "B"),
    feature_id_dest = c("F2", "F2", "F3", "F4", "F4", "F4")
  )

  out <- apply_adduct_consistency_filter(
    df_add = df_add,
    adduct_consistency = "conditional",
    adduct_min_support = 2L,
    adduct_consistency_min_degree = 3L
  )

  expect_true(any(out$feature_id == "F1" & out$adduct == "A"))
  expect_false(any(out$feature_id == "F1" & out$adduct == "X"))
})

test_that("enforce_graph_adduct_consistency removes impossible combinations", {
  df_add <- tidytable::tidytable(
    feature_id = c("1879", "1884", "1911", "1879"),
    adduct = c("[M]+", "[M+H]+", "[M+H2O+Na]+", "[M-H2+Fe+H2O]+"),
    adduct_dest = c(
      "[M+Na+NaCl]+",
      "[M+H2O+Na]+",
      "[M+H2O+K]+",
      "[M+K+NaCl]+"
    ),
    feature_id_dest = c("1889", "1911", "1889", "1884")
  )

  out <- enforce_graph_adduct_consistency(df_add)

  expect_lte(nrow(out), 2L)

  node_states <- tidytable::bind_rows(
    out |>
      tidytable::transmute(feature_id, adduct),
    out |>
      tidytable::transmute(feature_id = feature_id_dest, adduct = adduct_dest)
  ) |>
    tidytable::distinct() |>
    tidytable::count(feature_id, name = "n_states")

  expect_true(all(node_states$n_states <= 1L))
})

test_that("enforce_graph_adduct_consistency keeps exotic edge when it is consistent", {
  df_add <- tidytable::tidytable(
    feature_id = c("A"),
    adduct = c("[M-H2+Fe+H2O]+"),
    adduct_dest = c("[M+K+NaCl]+"),
    feature_id_dest = c("B")
  )

  out <- enforce_graph_adduct_consistency(df_add)

  expect_equal(nrow(out), 1L)
  expect_equal(out$adduct[[1L]], "[M-H2+Fe+H2O]+")
  expect_equal(out$adduct_dest[[1L]], "[M+K+NaCl]+")
})

test_that("enforce_graph_adduct_consistency removes impossible combinations in larger graphs", {
  # 5-node cycle where each node also gets a conflicting alternative state.
  df_add <- tidytable::tidytable(
    feature_id = c(
      "A",
      "B",
      "C",
      "D",
      "E", # coherent cycle
      "A",
      "B",
      "C",
      "D",
      "E" # conflicting alternatives
    ),
    adduct = c(
      "[M]+",
      "[M+H]+",
      "[M+Na]+",
      "[M+K]+",
      "[M+H4N]+",
      "[M+Cu]+",
      "[M+H+H2O]+",
      "[M+Na+NaCl]+",
      "[M-H2+Fe+H2O]+",
      "[M+H2O+K]+"
    ),
    adduct_dest = c(
      "[M+H]+",
      "[M+Na]+",
      "[M+K]+",
      "[M+H4N]+",
      "[M]+",
      "[M+Na]+",
      "[M+K]+",
      "[M+H4N]+",
      "[M]+",
      "[M+H]+"
    ),
    feature_id_dest = c(
      "B",
      "C",
      "D",
      "E",
      "A",
      "C",
      "D",
      "E",
      "A",
      "B"
    )
  )

  out <- enforce_graph_adduct_consistency(df_add)

  node_states <- tidytable::bind_rows(
    out |>
      tidytable::transmute(feature_id, adduct),
    out |>
      tidytable::transmute(feature_id = feature_id_dest, adduct = adduct_dest)
  ) |>
    tidytable::distinct() |>
    tidytable::count(feature_id, name = "n_states")

  # Strong invariant: no feature can keep more than one adduct state.
  expect_true(all(node_states$n_states <= 1L))
})

test_that("enforce_graph_adduct_consistency exposes audit metadata", {
  df_add <- tidytable::tidytable(
    feature_id = c("X", "X"),
    adduct = c("[M]+", "[M+Cu]+"),
    adduct_dest = c("[M+H]+", "[M+H]+"),
    feature_id_dest = c("Y", "Y")
  )

  out <- enforce_graph_adduct_consistency(df_add)
  audit <- attr(out, "consistency_audit")

  expect_true(is.list(audit))
  expect_true(all(c("n_input", "n_kept", "n_dropped") %in% names(audit)))
  expect_equal(audit$n_input, 2L)
  expect_equal(audit$n_kept + audit$n_dropped, audit$n_input)
})

test_that("enforce_graph_adduct_consistency does not split equivalent H2O text forms", {
  df_add <- tidytable::tidytable(
    feature_id = c("A", "B"),
    adduct = c("[M+H2O+H]+", "[M+H+H2O]+"),
    adduct_dest = c("[M+H+H2O]+", "[M+H2O+Na]+"),
    feature_id_dest = c("B", "C")
  )

  out <- enforce_graph_adduct_consistency(df_add)

  expect_equal(nrow(out), 2L)
})

test_that("build_adduct_state_key_map canonicalizes summed loss/cluster text order", {
  mapped <- build_adduct_state_key_map(c(
    "[M-C3H6O3+H2O+H4N]+",
    "[M+H4N+H2O-C3H6O3]+"
  ))

  expect_equal(length(unique(mapped$state_key)), 1L)
})

test_that("discover_evidence_adduct_signal recovers multicharged ion species", {
  M <- 400
  features <- tidytable::tidytable(
    feature_id = c("F_SINGLE", "F_DOUBLE"),
    sample = c("S1", "S1"),
    rt = c(5, 5),
    mz = c(M + 1.007276, (M + (2 * 1.007276)) / 2)
  )

  out <- discover_evidence_adduct_signal(
    features_table = features,
    adducts = c("[M+H]+", "[M+2H]2+"),
    clusters = character(),
    neutral_losses = character(),
    ms_mode = "pos",
    tolerance_ppm = 10,
    tolerance_rt = 0.02,
    exact_masses = M
  )

  expect_true(any(
    out$hypotheses$feature_id == "F_DOUBLE" &
      out$hypotheses$adduct == "[M+2H]2+"
  ))
  expect_true(any(
    out$edges$feature_id == "F_DOUBLE" |
      out$edges$feature_id_dest == "F_DOUBLE"
  ))
})

test_that("pairwise-support filter removes modifier-bearing evidence-only adducts", {
  features <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    mz = c(100, 122),
    rt = c(1, 1),
    sample = c("S", "S")
  )
  universe <- build_adduct_universe(
    adducts_list = list(pos = c("[M+H]+", "[M+C2H3N+Na]+"), neg = c("[M-H]-")),
    clusters_list = list(pos = c("C2H3N"), neg = c("CH2O2")),
    neutral_losses_list = character(),
    polarity = "pos"
  )
  evidence_signal <- list(
    hypotheses = tidytable::tidytable(
      feature_id = c("F1", "F2", "F2"),
      adduct = c("[M+H]+", "[M+C2H3N+Na]+", "[M+2H]2+"),
      source = c("evidence", "evidence", "evidence"),
      is_preassigned = c(FALSE, FALSE, FALSE),
      adduct_support = c(2L, 2L, 2L)
    ),
    edges = tidytable::tidytable(
      feature_id = c("F1", "F1"),
      adduct = c("[M+H]+", "[M+H]+"),
      feature_id_dest = c("F2", "F2"),
      adduct_dest = c("[M+C2H3N+Na]+", "[M+2H]2+")
    )
  )

  out <- filter_modifier_evidence_by_pairwise_support(
    evidence_signal = evidence_signal,
    universe = universe,
    adduct_edges = tidytable::tidytable(
      feature_id = character(),
      adduct = character(),
      adduct_dest = character(),
      feature_id_dest = character()
    ),
    cluster_edges = tidytable::tidytable(
      feature_id = character(),
      cluster = character(),
      mass = numeric(),
      feature_id_dest = character()
    ),
    loss_edges = tidytable::tidytable(
      feature_id = character(),
      loss = character(),
      mass = numeric(),
      feature_id_dest = character()
    ),
    baseline_adduct = "[M+H]+",
    features_table = features
  )

  expect_false("[M+C2H3N+Na]+" %in% out$hypotheses$adduct)
  expect_true("[M+2H]2+" %in% out$hypotheses$adduct)
  expect_false(any(out$edges$adduct_dest == "[M+C2H3N+Na]+"))
  expect_true(any(out$edges$adduct_dest == "[M+2H]2+"))
})

test_that("conflict-resolution filter removes edge-conflicting annotation states", {
  annotations <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F2", "F2"),
    adduct = c("[M+H]+", "[M+Na]+", "[M+Na]+", "[M+K]+"),
    source = c("pair", "pair", "pair", "pair")
  )
  adduct_edges <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F2"),
    adduct = c("[M+H]+", "[M+Na]+", "[M+Na]+"),
    adduct_dest = c("[M+Na]+", "[M+K]+", "[M+H]+"),
    feature_id_dest = c("F2", "F2", "F3")
  )

  out <- enforce_non_conflicting_annotation_states(
    annotations = annotations,
    adduct_edges = adduct_edges
  )

  expect_true(any(out$feature_id == "F1" & out$adduct == "[M+H]+"))
  expect_false(any(out$feature_id == "F1" & out$adduct == "[M+Na]+"))
  expect_true(any(out$feature_id == "F2" & out$adduct == "[M+Na]+"))
  expect_false(any(out$feature_id == "F2" & out$adduct == "[M+K]+"))
})

test_that("conflict-resolution filter keeps broad coverage for unconstrained features", {
  annotations <- tidytable::tidytable(
    feature_id = c("F1", "F1", "ISO"),
    adduct = c("[M+H]+", "[M+Na]+", "[M+K]+"),
    source = c("pair", "pair", "evidence")
  )
  adduct_edges <- tidytable::tidytable(
    feature_id = "F1",
    adduct = "[M+H]+",
    adduct_dest = "[M+Na]+",
    feature_id_dest = "F2"
  )

  out <- enforce_non_conflicting_annotation_states(
    annotations = annotations,
    adduct_edges = adduct_edges
  )

  expect_true(any(out$feature_id == "ISO" & out$adduct == "[M+K]+"))
})

test_that("annotate_masses recovers doubly charged species through evidence discovery", {
  local_quiet_logging()

  M <- 180.0634
  env <- prepare_annotation_fixture_env(
    feature_fixture = "features_small.csv",
    library_fixture = "library_isotope.csv"
  )

  withr::local_dir(new = env$dirs$root)

  features_multicharge <- tidytable::tidytable(
    feature_id = c("FT_SINGLE", "FT_DOUBLE"),
    mz = c(M + 1.007276, (M + (2 * 1.007276)) / 2),
    rt = c(2.5, 2.5),
    sample = c("Sample001", "Sample001"),
    adduct = c(NA_character_, NA_character_)
  )
  utils::write.table(
    x = features_multicharge,
    file = env$features,
    sep = ",",
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE
  )

  result <- annotate_masses(
    features = env$features,
    library = env$library,
    str_stereo = env$str_stereo,
    str_met = env$str_met,
    str_tax_cla = env$str_tax_cla,
    str_tax_npc = env$str_tax_npc,
    adducts_list = list(
      pos = c("[M+H]+", "[M+2H]2+"),
      neg = c("[M-H]-")
    ),
    clusters_list = list(pos = c("H2O"), neg = c("CH2O2")),
    neutral_losses_list = character(),
    ms_mode = "pos",
    tolerance_ppm = 10,
    tolerance_rt = 0.02,
    output_annotations = env$output_annotations,
    output_edges = env$output_edges
  )

  annotations <- tidytable::fread(
    result[["annotations"]],
    colClasses = "character"
  )
  edges <- tidytable::fread(result[["edges"]], colClasses = "character")

  ft_double <- annotations |>
    tidytable::filter(feature_id == "FT_DOUBLE")
  expect_true("[M+2H]2+" %in% ft_double$candidate_adduct)
  expect_true(any(
    (edges$CLUSTERID1 == "FT_DOUBLE" | edges$CLUSTERID2 == "FT_DOUBLE") &
      grepl("\\[M\\+2H\\]2\\+|\\[M\\+H\\]\\+", edges$label)
  ))
})
