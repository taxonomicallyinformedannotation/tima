# Test: SMILES Processing and Weighting
library(testthat)

test_that("process_smiles works without cache", {
  result <- tidytable::tidytable(
    "structure_smiles_initial" = "C[C@@H]1C=C(C(=O)[C@]2([C@H]1C[C@@H]3[C@@]4([C@@H]2C(=O)C(=C([C@@H]4CC(=O)O3)C)OC)C)C)OC"
  ) |>
    process_smiles()

  expect_s3_class(result, "data.frame")
  expect_true("structure_smiles_2D" %in% colnames(result))
})

test_that("filter_annotations works without RT library", {
  skip_on_cran()
  copy_backbone(cache_dir = ".")
  paths <- get_default_paths()

  # Setup basic data
  get_file(
    url = paths$urls$examples$features,
    export = paths$data$source$features
  )
  prepare_features_tables()

  fake_lotus(export = paths$data$source$libraries$sop$lotus)
  prepare_libraries_sop_lotus()
  prepare_libraries_sop_merged()

  annotate_masses(
    tolerance_ppm = 1.0,
    tolerance_rt = 0.01,
    ms_mode = "pos"
  )

  expect_no_error(filter_annotations(rts = list()))

  unlink("data", recursive = TRUE)
})

test_that("filter_annotations works with default parameters", {
  skip_on_cran()
  copy_backbone(cache_dir = ".")
  paths <- get_default_paths()

  # Full setup
  get_file(
    url = paths$urls$examples$features,
    export = paths$data$source$features
  )
  get_file(
    url = paths$urls$examples$lib_mini$rt,
    export = paths$data$source$libraries$rt$example_mini
  )
  prepare_features_tables()
  prepare_libraries_rt(temp_exp = paths$data$source$libraries$rt$example_mini)

  fake_lotus(export = paths$data$source$libraries$sop$lotus)
  prepare_libraries_sop_lotus()
  prepare_libraries_sop_merged()

  annotate_masses(
    tolerance_ppm = 1.0,
    tolerance_rt = 0.01,
    ms_mode = "pos"
  )

  expect_no_error(filter_annotations())

  unlink("data", recursive = TRUE)
})

test_that("weight_annotations works with default parameters", {
  skip_on_cran()
  copy_backbone(cache_dir = ".")
  paths <- get_default_paths()

  # Full pipeline setup
  get_file(
    url = paths$urls$examples$features,
    export = paths$data$source$features
  )
  get_file(
    url = paths$urls$examples$metadata,
    export = paths$data$source$metadata
  )
  get_file(
    url = paths$urls$examples$spectra_mini,
    export = paths$data$source$spectra
  )

  prepare_features_tables()
  prepare_taxa()

  fake_lotus(export = paths$data$source$libraries$sop$lotus)
  prepare_libraries_sop_lotus()
  prepare_libraries_sop_merged()

  annotate_masses(
    tolerance_ppm = 1.0,
    tolerance_rt = 0.01,
    ms_mode = "pos"
  )
  filter_annotations(rts = list())

  create_edges_spectra(
    ppm = 1.0,
    dalton = 0.001,
    method = "entropy"
  )
  prepare_features_edges()
  create_components()
  prepare_features_components()

  expect_no_error(
    weight_annotations(candidates_final = 1, minimal_ms1_bio = 0.8)
  )

  unlink("data", recursive = TRUE)
})

test_that("weight_annotations works with MS1 only mode", {
  skip_on_cran()
  copy_backbone(cache_dir = ".")
  paths <- get_default_paths()

  # Setup
  get_file(
    url = paths$urls$examples$features,
    export = paths$data$source$features
  )
  get_file(
    url = paths$urls$examples$metadata,
    export = paths$data$source$metadata
  )
  get_file(
    url = paths$urls$examples$spectra_mini,
    export = paths$data$source$spectra
  )

  prepare_features_tables()
  prepare_taxa()

  fake_lotus(export = paths$data$source$libraries$sop$lotus)
  prepare_libraries_sop_lotus()
  prepare_libraries_sop_merged()

  annotate_masses(
    tolerance_ppm = 1.0,
    tolerance_rt = 0.01,
    ms_mode = "pos"
  )
  filter_annotations(rts = list())

  create_edges_spectra(
    ppm = 1.0,
    dalton = 0.001,
    method = "entropy"
  )
  prepare_features_edges()
  create_components()
  prepare_features_components()

  expect_no_error(
    weight_annotations(
      ms1_only = TRUE,
      remove_ties = TRUE,
      summarize = TRUE,
      candidates_final = 1L,
      minimal_ms1_bio = 0.8,
      minimal_ms1_condition = "AND",
      compounds_names = TRUE,
      high_confidence = FALSE
    )
  )

  unlink("data", recursive = TRUE)
})
