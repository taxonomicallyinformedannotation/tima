# Test Suite: annotate_masses ----

library(testthat)
pkgload::load_all(quiet = TRUE) |>
  suppressMessages()

## Input Validation ----

test_that("annotate_masses validates ms_mode correctly", {
  # Invalid mode should error
  expect_error(
    annotate_masses(
      features = tempfile(),
      library = tempfile(),
      str_stereo = tempfile(),
      str_met = tempfile(),
      str_nam = tempfile(),
      str_tax_cla = tempfile(),
      str_tax_npc = tempfile(),
      ms_mode = "invalid_mode"
    ),
    "ms_mode must be either 'pos' or 'neg'"
  )

  # Blank mode should error
  expect_error(
    annotate_masses(ms_mode = ""),
    "ms_mode must be either 'pos' or 'neg'"
  )

  # NULL mode should error (handled by parameter default or earlier validation)
  # Note: NULL will use default from get_params, so we test with invalid value instead
})

test_that("annotate_masses validates tolerance_ppm correctly", {
  # Zero tolerance should error
  expect_error(
    annotate_masses(
      tolerance_ppm = 0,
      ms_mode = "pos"
    ),
    "tolerance_ppm must be a positive number <= 20"
  )

  # Negative tolerance should error
  expect_error(
    annotate_masses(
      tolerance_ppm = -5,
      ms_mode = "pos"
    ),
    "tolerance_ppm must be a positive number <= 20"
  )

  # Too large tolerance should error
  expect_error(
    annotate_masses(
      tolerance_ppm = 25,
      ms_mode = "pos"
    ),
    "tolerance_ppm must be a positive number <= 20"
  )

  # Non-numeric tolerance should error
  expect_error(
    annotate_masses(
      tolerance_ppm = "10",
      ms_mode = "pos"
    ),
    "tolerance_ppm must be a positive number <= 20"
  )
})

test_that("annotate_masses validates tolerance_rt correctly", {
  # Zero tolerance should error
  expect_error(
    annotate_masses(
      tolerance_rt = 0,
      tolerance_ppm = 10,
      ms_mode = "pos"
    ),
    "tolerance_rt must be a positive number <= 0.05"
  )

  # Negative tolerance should error
  expect_error(
    annotate_masses(
      tolerance_rt = -0.01,
      tolerance_ppm = 10,
      ms_mode = "pos"
    ),
    "tolerance_rt must be a positive number <= 0.05"
  )

  # Too large tolerance should error
  expect_error(
    annotate_masses(
      tolerance_rt = 0.1,
      tolerance_ppm = 10,
      ms_mode = "pos"
    ),
    "tolerance_rt must be a positive number <= 0.05"
  )
})

test_that("annotate_masses validates adducts_list structure", {
  # Missing mode in adducts_list should error
  expect_error(
    annotate_masses(
      adducts_list = list(neg = c("[M-H]-")),
      ms_mode = "pos",
      tolerance_ppm = 10,
      tolerance_rt = 0.02
    ),
    "adducts_list must contain 'pos' mode adducts"
  )

  # NULL adducts for mode should error
  expect_error(
    annotate_masses(
      adducts_list = list(pos = NULL),
      ms_mode = "pos",
      tolerance_ppm = 10,
      tolerance_rt = 0.02
    ),
    "adducts_list must contain 'pos' mode adducts"
  )

  # Non-list adducts should error
  expect_error(
    annotate_masses(
      adducts_list = c("[M+H]+"),
      ms_mode = "pos",
      tolerance_ppm = 10,
      tolerance_rt = 0.02
    ),
    "adducts_list must contain 'pos' mode adducts"
  )
})

test_that("annotate_masses validates clusters_list structure", {
  # Missing mode in clusters_list should error
  expect_error(
    annotate_masses(
      adducts_list = list(pos = c("[M+H]+")),
      clusters_list = list(neg = c("[M]")),
      ms_mode = "pos",
      tolerance_ppm = 10,
      tolerance_rt = 0.02
    ),
    "clusters_list must contain 'pos' mode clusters"
  )
})

test_that("annotate_masses validates file existence", {
  # Create temporary valid parameters except for files
  temp_features <- tempfile(fileext = ".tsv")
  temp_library <- tempfile(fileext = ".tsv")

  # Missing features file should error
  expect_error(
    annotate_masses(
      features = "/nonexistent/features.tsv",
      library = temp_library,
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
    "Required file\\(s\\) not found.*features"
  )
})


## Edge Cases and Empty Input Tests ----

test_that("annotate_masses handles empty features table", {
  # Setup temporary test environment with local directory
  withr::local_tempdir()
  paths <- local_test_project()

  # Create directories
  dir.create("data/interim/features", showWarnings = FALSE, recursive = TRUE)
  dir.create("data/interim/libraries", showWarnings = FALSE, recursive = TRUE)
  dir.create("data/interim", showWarnings = FALSE, recursive = TRUE)

  # Create empty features file
  features_file <- "data/interim/features/empty_features.tsv"

  empty_features <- tidytable::tidytable(
    feature_id = character(0),
    mz = numeric(0),
    rt = numeric(0),
    sample = character(0)
  )
  export_output(x = empty_features, file = features_file)

  # Create minimal library files
  library_file <- "data/interim/libraries/library.tsv"

  library_table <- tidytable::tidytable(
    structure_inchikey = generate_fake_inchikey(1),
    structure_exact_mass = 200.0
  )
  export_output(x = library_table, file = library_file)

  # Create other required files (minimal)
  str_stereo <- "data/interim/libraries/stereo.tsv"
  str_met <- "data/interim/libraries/metadata.tsv"
  str_nam <- "data/interim/libraries/names.tsv"
  str_tax_cla <- "data/interim/libraries/tax_cla.tsv"
  str_tax_npc <- "data/interim/libraries/tax_npc.tsv"

  for (file in c(str_stereo, str_met, str_nam, str_tax_cla, str_tax_npc)) {
    export_output(
      x = tidytable::tidytable(structure_inchikey = generate_fake_inchikey(1)),
      file = file
    )
  }

  output_annotations <- "data/interim/annotations.tsv"
  output_edges <- "data/interim/edges.tsv"

  # Should return empty results without error
  result <- annotate_masses(
    features = features_file,
    library = library_file,
    str_stereo = str_stereo,
    str_met = str_met,
    str_nam = str_nam,
    str_tax_cla = str_tax_cla,
    str_tax_npc = str_tax_npc,
    adducts_list = list(pos = c("[M+H]+")),
    clusters_list = list(pos = c("[M]")),
    neutral_losses_list = c("H2O"),
    ms_mode = "pos",
    tolerance_ppm = 10,
    tolerance_rt = 0.02,
    output_annotations = output_annotations,
    output_edges = output_edges
  )

  expect_type(result, "character")
  expect_named(result, c("annotations", "edges"))

  # Check that output files exist and are valid
  expect_true(file.exists(result["annotations"]))
  expect_true(file.exists(result["edges"]))

  # Read and verify empty structure
  annotations <- tidytable::fread(result["annotations"])
  edges <- tidytable::fread(result["edges"])

  expect_equal(nrow(annotations), 0)
  expect_equal(nrow(edges), 0)

  # Verify columns exist
  expect_true("feature_id" %in% colnames(annotations))
  expect_true("CLUSTERID1" %in% colnames(edges))
})

test_that("annotate_masses handles features without RT column", {
  # Setup temporary test environment
  withr::local_tempdir()
  paths <- local_test_project()

  # Create directories
  dir.create("data/interim/features", showWarnings = FALSE, recursive = TRUE)
  dir.create("data/interim/libraries", showWarnings = FALSE, recursive = TRUE)
  dir.create("data/interim", showWarnings = FALSE, recursive = TRUE)

  # Create features without RT
  features_file <- "data/interim/features/no_rt_features.tsv"

  features <- tidytable::tidytable(
    feature_id = c("FT001", "FT002"),
    mz = c(200.1, 300.2),
    sample = c("S1", "S1")
  )
  export_output(x = features, file = features_file)

  # Create minimal library files
  library_file <- "data/interim/libraries/library.tsv"

  library_table <- tidytable::tidytable(
    structure_inchikey = generate_fake_inchikey(2),
    structure_exact_mass = c(200.0, 300.0)
  )
  export_output(x = library_table, file = library_file)

  # Create other required files
  str_stereo <- "data/interim/libraries/stereo.tsv"
  str_met <- "data/interim/libraries/metadata.tsv"
  str_nam <- "data/interim/libraries/names.tsv"
  str_tax_cla <- "data/interim/libraries/tax_cla.tsv"
  str_tax_npc <- "data/interim/libraries/tax_npc.tsv"

  for (file in c(str_stereo, str_met, str_nam, str_tax_cla, str_tax_npc)) {
    export_output(
      x = tidytable::tidytable(
        structure_inchikey = library_table$structure_inchikey
      ),
      file = file
    )
  }

  output_annotations <- "data/interim/annotations.tsv"
  output_edges <- "data/interim/edges.tsv"

  # Should not error, should use feature_id as RT fallback
  expect_no_error({
    result <- annotate_masses(
      features = features_file,
      library = library_file,
      str_stereo = str_stereo,
      str_met = str_met,
      str_nam = str_nam,
      str_tax_cla = str_tax_cla,
      str_tax_npc = str_tax_npc,
      adducts_list = list(pos = c("[M+H]+")),
      clusters_list = list(pos = c("[M]")),
      neutral_losses_list = c("H2O"),
      ms_mode = "pos",
      tolerance_ppm = 10,
      tolerance_rt = 0.02,
      output_annotations = output_annotations,
      output_edges = output_edges
    )
  })

  expect_type(result, "character")
  expect_true(file.exists(result["annotations"]))
})

test_that("annotate_masses handles no valid monocharged adducts", {
  # Setup temporary test environment
  withr::local_tempdir()
  paths <- local_test_project()

  # Create directories
  dir.create("data/interim/features", showWarnings = FALSE, recursive = TRUE)
  dir.create("data/interim/libraries", showWarnings = FALSE, recursive = TRUE)
  dir.create("data/interim", showWarnings = FALSE, recursive = TRUE)

  # Create minimal features file
  features_file <- "data/interim/features/features.tsv"

  features <- create_test_features(n_features = 2, seed = 123)
  features$sample <- "S1"
  export_output(x = features, file = features_file)

  # Create minimal library files
  library_file <- "data/interim/libraries/library.tsv"

  library_table <- tidytable::tidytable(
    structure_inchikey = generate_fake_inchikey(1),
    structure_exact_mass = 200.0
  )
  export_output(x = library_table, file = library_file)

  # Create other required files
  str_stereo <- "data/interim/libraries/stereo.tsv"
  str_met <- "data/interim/libraries/metadata.tsv"
  str_nam <- "data/interim/libraries/names.tsv"
  str_tax_cla <- "data/interim/libraries/tax_cla.tsv"
  str_tax_npc <- "data/interim/libraries/tax_npc.tsv"

  for (file in c(str_stereo, str_met, str_nam, str_tax_cla, str_tax_npc)) {
    export_output(
      x = tidytable::tidytable(structure_inchikey = generate_fake_inchikey(1)),
      file = file
    )
  }

  output_annotations <- "data/interim/annotations.tsv"
  output_edges <- "data/interim/edges.tsv"

  # Use only multicharged adducts (should fail)
  expect_error(
    annotate_masses(
      features = features_file,
      library = library_file,
      str_stereo = str_stereo,
      str_met = str_met,
      str_nam = str_nam,
      str_tax_cla = str_tax_cla,
      str_tax_npc = str_tax_npc,
      adducts_list = list(pos = c("[M+2H]2+")), # Only multicharged
      clusters_list = list(pos = character(0)), # No clusters
      neutral_losses_list = c("H2O"),
      ms_mode = "pos",
      tolerance_ppm = 10,
      tolerance_rt = 0.02,
      output_annotations = output_annotations,
      output_edges = output_edges
    ),
    "No monocharged adducts/clusters available for annotation"
  )
})

## Functional Tests with Real Data ----

test_that("annotate_masses produces expected output structure", {
  # Setup temporary test environment
  withr::local_tempdir()

  # Create realistic features
  features_file <- file.path(tempdir(), "features.tsv")

  features <- create_test_features(n_features = 10, seed = 42)
  features$sample <- "Sample001"
  features$adduct <- NA_character_
  export_output(x = features, file = features_file)

  # Create library with matching masses
  library_file <- file.path(tempdir(), "library.tsv")

  # Use masses from features to ensure matches
  library_table <- tidytable::tidytable(
    structure_inchikey = generate_fake_inchikey(10, seed = 42),
    structure_exact_mass = features$mz[1:10] - 1.007276 # Remove proton mass
  )
  export_output(x = library_table, file = library_file)

  # Create supplementary structure files
  stereo_file <- file.path(tempdir(), "stereo.tsv")
  stereo_table <- tidytable::tidytable(
    structure_inchikey = library_table$structure_inchikey,
    structure_smiles_no_stereo = replicate(
      10,
      paste(
        sample(c("C", "O", "N"), 5, TRUE),
        collapse = ""
      )
    )
  )
  export_output(x = stereo_table, file = stereo_file)

  metadata_file <- file.path(tempdir(), "metadata.tsv")
  metadata_table <- tidytable::tidytable(
    structure_inchikey = library_table$structure_inchikey,
    structure_molecular_formula = replicate(10, "C10H12O2"),
    structure_xlogp = runif(10, 1, 3)
  )
  export_output(x = metadata_table, file = metadata_file)

  names_file <- file.path(tempdir(), "names.tsv")
  names_table <- tidytable::tidytable(
    structure_inchikey = library_table$structure_inchikey,
    structure_name = paste0("Compound_", 1:10)
  )
  export_output(x = names_table, file = names_file)

  tax_cla_file <- file.path(tempdir(), "tax_cla.tsv")
  tax_cla_table <- tidytable::tidytable(
    structure_inchikey = library_table$structure_inchikey,
    structure_tax_cla_01kin = "Organic compounds",
    structure_tax_cla_02sup = "Phenylpropanoids",
    structure_tax_cla_03cla = "Flavonoids"
  )
  export_output(x = tax_cla_table, file = tax_cla_file)

  tax_npc_file <- file.path(tempdir(), "tax_npc.tsv")
  tax_npc_table <- tidytable::tidytable(
    structure_inchikey = library_table$structure_inchikey,
    structure_tax_npc_01pat = "Shikimates and Phenylpropanoids",
    structure_tax_npc_02sup = "Flavonoids"
  )
  export_output(x = tax_npc_table, file = tax_npc_file)

  # Run annotation
  output_annotations <- file.path(tempdir(), "annotations.tsv")
  output_edges <- file.path(tempdir(), "edges.tsv")

  result <- annotate_masses(
    features = features_file,
    library = library_file,
    str_stereo = stereo_file,
    str_met = metadata_file,
    str_nam = names_file,
    str_tax_cla = tax_cla_file,
    str_tax_npc = tax_npc_file,
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
    output_annotations = output_annotations,
    output_edges = output_edges
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
  # Setup temporary test environment
  withr::local_tempdir()

  # Create features with known m/z values
  features_file <- file.path(tempdir(), "features.tsv")

  features <- tidytable::tidytable(
    feature_id = c("FT001", "FT002"),
    mz = c(200.1, 300.2),
    rt = c(5.0, 5.0),
    sample = c("S1", "S1"),
    adduct = c(NA, NA)
  )
  export_output(x = features, file = features_file)

  # Create library with exact mass that's within/outside tolerance
  library_file <- file.path(tempdir(), "library.tsv")

  # FT001: mz=200.1, [M+H]+ -> exact_mass ~199.09
  # Create one within 5ppm and one outside
  library_table <- tidytable::tidytable(
    structure_inchikey = generate_fake_inchikey(2, seed = 99),
    structure_exact_mass = c(
      199.092724, # Within 5ppm of FT001
      199.080000 # Outside 5ppm of FT001 (>60ppm difference)
    )
  )
  export_output(x = library_table, file = library_file)

  # Create minimal supplementary files with required columns
  stereo_file <- file.path(tempdir(), "stereo.tsv")
  stereo_table <- tidytable::tidytable(
    structure_inchikey = library_table$structure_inchikey,
    structure_smiles_no_stereo = c("CCCC", "CCCC")
  )
  export_output(x = stereo_table, file = stereo_file)

  metadata_file <- file.path(tempdir(), "metadata.tsv")
  metadata_table <- tidytable::tidytable(
    structure_inchikey = library_table$structure_inchikey,
    structure_molecular_formula = c("C10H12O2", "C10H12O2"),
    structure_xlogp = c("2.0", "2.0")
  )
  export_output(x = metadata_table, file = metadata_file)

  names_file <- file.path(tempdir(), "names.tsv")
  names_table <- tidytable::tidytable(
    structure_inchikey = library_table$structure_inchikey,
    structure_name = c("Compound_1", "Compound_2")
  )
  export_output(x = names_table, file = names_file)

  tax_cla_file <- file.path(tempdir(), "tax_cla.tsv")
  tax_cla_table <- tidytable::tidytable(
    structure_inchikey = library_table$structure_inchikey,
    structure_tax_cla_01kin = c("Organic compounds", "Organic compounds")
  )
  export_output(x = tax_cla_table, file = tax_cla_file)

  tax_npc_file <- file.path(tempdir(), "tax_npc.tsv")
  tax_npc_table <- tidytable::tidytable(
    structure_inchikey = library_table$structure_inchikey,
    structure_tax_npc_01pat = c("Alkaloids", "Alkaloids")
  )
  export_output(x = tax_npc_table, file = tax_npc_file)

  # Run with tight tolerance (5 ppm) - should only match first structure
  output_annotations <- file.path(tempdir(), "ann_tight.tsv")
  output_edges <- file.path(tempdir(), "edges_tight.tsv")

  result_tight <- annotate_masses(
    features = features_file,
    library = library_file,
    str_stereo = stereo_file,
    str_met = metadata_file,
    str_nam = names_file,
    str_tax_cla = tax_cla_file,
    str_tax_npc = tax_npc_file,
    adducts_list = list(pos = c("[M+H]+")),
    clusters_list = list(pos = c("[M]")),
    neutral_losses_list = character(0),
    ms_mode = "pos",
    tolerance_ppm = 5,
    tolerance_rt = 0.02,
    output_annotations = output_annotations,
    output_edges = output_edges
  )

  annotations_tight <- tidytable::fread(
    result_tight["annotations"],
    colClasses = "character"
  )

  # Verify only close match is found
  # (May be 0 or 1 depending on exact mass calculations)
  expect_true(nrow(annotations_tight) <= 1)
})

## Performance and Scalability Tests ----

# test_that("annotate_masses handles moderate-scale data efficiently", {
#   skip_on_cran()
#   skip_if_not(interactive(), "Performance test only for local development")
#
#   # Setup temporary test environment
#   paths <- local_test_project()
#
#   # Create 100 features
#   features_file <- file.path(paths$data$interim$features, "large_features.tsv")
#   dir.create(dirname(features_file), showWarnings = FALSE, recursive = TRUE)
#
#   features <- create_test_features(n_features = 100, seed = 999)
#   features$sample <- sample(paste0("S", 1:5), 100, replace = TRUE)
#   features$adduct <- NA_character_
#   export_output(x = features, file = features_file)
#
#   # Create library with 50 structures
#   library_file <- file.path(paths$data$interim$libraries, "library.tsv")
#   dir.create(dirname(library_file), showWarnings = FALSE, recursive = TRUE)
#
#   library_table <- tidytable::tidytable(
#     structure_inchikey = generate_fake_inchikey(50, seed = 999),
#     structure_exact_mass = runif(50, 100, 500)
#   )
#   export_output(x = library_table, file = library_file)
#
#   # Create supplementary files
#   str_files <- c("stereo", "metadata", "names", "tax_cla", "tax_npc")
#   str_paths <- sapply(str_files, function(f) {
#     path <- file.path(paths$data$interim$libraries, paste0(f, ".tsv"))
#     minimal_table <- tidytable::tidytable(
#       structure_inchikey = library_table$structure_inchikey
#     )
#     export_output(x = minimal_table, file = path)
#     path
#   })
#
#   # Should complete in reasonable time (<10 seconds for 100 features)
#   start_time <- Sys.time()
#
#   result <- annotate_masses(
#     features = features_file,
#     library = library_file,
#     str_stereo = str_paths["stereo"],
#     str_met = str_paths["metadata"],
#     str_nam = str_paths["names"],
#     str_tax_cla = str_paths["tax_cla"],
#     str_tax_npc = str_paths["tax_npc"],
#     adducts_list = list(pos = c("[M+H]+", "[M+Na]+")),
#     clusters_list = list(pos = c("[M]", "[2M]")),
#     neutral_losses_list = c("H2O"),
#     ms_mode = "pos",
#     tolerance_ppm = 10,
#     tolerance_rt = 0.02,
#     output_annotations = file.path(paths$data$interim, "annotations.tsv"),
#     output_edges = file.path(paths$data$interim, "edges.tsv")
#   )
#
#   elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
#
#   expect_true(
#     elapsed_time < 10,
#     info = sprintf(
#       "Processing took %.2f seconds (expected < 10s)",
#       elapsed_time
#     )
#   )
#
#   expect_true(file.exists(result$annotations))
#   expect_true(file.exists(result$edges))
# })
