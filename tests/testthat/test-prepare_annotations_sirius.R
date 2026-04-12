# Test Suite: prepare_annotations_sirius ----

library(testthat)

## Internal Utility Helpers ----

#' Stage structure fixtures for SIRIUS annotation tests
#'
#' @param root Root directory for staging
#' @return Named list of structure file paths
stage_structure_fixtures <- function(
  root = temp_test_dir("prep_sirius_structures")
) {
  # Create directory structure
  dir.create(
    file.path(root, "structures", "taxonomies"),
    recursive = TRUE,
    showWarnings = FALSE
  )

  # Define target paths
  stereo <- file.path(root, "structures", "stereo.tsv")
  met <- file.path(root, "structures", "metadata.tsv")
  nam <- file.path(root, "structures", "names.tsv")
  cla <- file.path(root, "structures", "taxonomies", "classyfire.tsv")
  npc <- file.path(root, "structures", "taxonomies", "npc.tsv")

  # Copy fixtures using the helper that handles path resolution
  copy_fixture_to("structures_stereo.csv", stereo)
  copy_fixture_to("structures_metadata.csv", met)
  copy_fixture_to("structures_names.csv", nam)
  copy_fixture_to("structures_taxonomy_cla.csv", cla)
  copy_fixture_to("structures_taxonomy_npc.csv", npc)

  list(
    root = root,
    stereo = stereo,
    met = met,
    nam = nam,
    cla = cla,
    npc = npc
  )
}

## Validation ----

test_that("test-prepare_annotations_sirius validates sirius_version", {
  s <- stage_structure_fixtures()
  tmp <- tempdir()
  out_ann <- file.path(tmp, "ann.tsv")
  out_can <- file.path(tmp, "can.tsv")
  out_for <- file.path(tmp, "for.tsv")
  expect_error(
    prepare_annotations_sirius(
      input_directory = temp_test_path("missing.zip"),
      output_ann = out_ann,
      output_can = out_can,
      output_for = out_for,
      sirius_version = "7",
      str_stereo = s$stereo,
      str_met = s$met,
      str_tax_cla = s$cla,
      str_tax_npc = s$npc
    ),
    "sirius_version must be '5' or '6'",
    class = "tima_validation_error"
  )
})

test_that("test-prepare_annotations_sirius validates output parameters and structure files", {
  s <- stage_structure_fixtures()
  expect_error(
    prepare_annotations_sirius(
      input_directory = temp_test_path("missing.zip"),
      output_ann = c("a", "b"),
      output_can = "can.tsv",
      output_for = "for.tsv",
      sirius_version = "5",
      str_stereo = s$stereo,
      str_met = s$met,
      str_tax_cla = s$cla,
      str_tax_npc = s$npc
    ),
    "Output path\\(s\\) must be single character strings.*output_ann",
    class = "tima_validation_error"
  )
  expect_error(
    prepare_annotations_sirius(
      input_directory = temp_test_path("missing.zip"),
      output_ann = "ann.tsv",
      output_can = "can.tsv",
      output_for = "for.tsv",
      sirius_version = "5",
      str_stereo = temp_test_path("missing.tsv"),
      str_met = s$met,
      str_tax_cla = s$cla,
      str_tax_npc = s$npc
    ),
    "Required file(s) not found:",
    fixed = TRUE
  )
})

## Behavior ----

test_that("test-prepare_annotations_sirius handles missing input by producing empty outputs", {
  s <- stage_structure_fixtures()
  tmp <- tempdir()
  out_ann <- file.path(tmp, "ann.tsv")
  out_can <- file.path(tmp, "can.tsv")
  out_for <- file.path(tmp, "for.tsv")
  res <- prepare_annotations_sirius(
    input_directory = temp_test_path("not_there.zip"),
    output_ann = out_ann,
    output_can = out_can,
    output_for = out_for,
    sirius_version = "5",
    str_stereo = s$stereo,
    str_met = s$met,
    str_tax_cla = s$cla,
    str_tax_npc = s$npc
  )
  expect_true(file.exists(out_can))
  expect_true(file.exists(out_for))
  expect_true(file.exists(out_ann))
  expect_true(is.character(res[1]))
})

test_that("test-prepare_annotations_sirius handles version 6", {
  s <- stage_structure_fixtures()
  tmp <- tempdir()
  out_ann <- file.path(tmp, "ann_v6.tsv")
  out_can <- file.path(tmp, "can_v6.tsv")
  out_for <- file.path(tmp, "for_v6.tsv")
  res <- prepare_annotations_sirius(
    input_directory = temp_test_path("not_there.zip"),
    output_ann = out_ann,
    output_can = out_can,
    output_for = out_for,
    sirius_version = "6",
    str_stereo = s$stereo,
    str_met = s$met,
    str_tax_cla = s$cla,
    str_tax_npc = s$npc
  )
  expect_true(file.exists(out_can))
  expect_true(file.exists(out_for))
  expect_true(file.exists(out_ann))
})

test_that("test-prepare_annotations_sirius validates numeric version", {
  s <- stage_structure_fixtures()
  tmp <- tempdir()
  out_ann <- file.path(tmp, "ann_numeric.tsv")
  out_can <- file.path(tmp, "can_numeric.tsv")
  out_for <- file.path(tmp, "for_numeric.tsv")

  # Numeric versions should be converted and accepted
  expect_no_error(
    prepare_annotations_sirius(
      input_directory = temp_test_path("not_there.zip"),
      output_ann = out_ann,
      output_can = out_can,
      output_for = out_for,
      sirius_version = 5,
      str_stereo = s$stereo,
      str_met = s$met,
      str_tax_cla = s$cla,
      str_tax_npc = s$npc
    )
  )
})

## Internal Helper Tests ----

test_that("validate_sirius_inputs validates version correctly", {
  s <- stage_structure_fixtures()
  tmp <- tempdir()

  expect_error(
    validate_sirius_inputs(
      sirius_version = "4",
      output_ann = file.path(tmp, "ann.tsv"),
      output_can = file.path(tmp, "can.tsv"),
      output_for = file.path(tmp, "for.tsv"),
      str_stereo = s$stereo,
      str_met = s$met,
      str_tax_cla = s$cla,
      str_tax_npc = s$npc,
      max_analog_abs_mz_error = 0.01
    ),
    "sirius_version must be '5' or '6'",
    class = "tima_validation_error"
  )

  # Test invalid type
  expect_error(
    validate_sirius_inputs(
      sirius_version = "invalid",
      output_ann = file.path(tmp, "ann.tsv"),
      output_can = file.path(tmp, "can.tsv"),
      output_for = file.path(tmp, "for.tsv"),
      str_stereo = s$stereo,
      str_met = s$met,
      str_tax_cla = s$cla,
      str_tax_npc = s$npc,
      max_analog_abs_mz_error = 0.01
    ),
    "sirius_version must be '5' or '6'",
    class = "tima_validation_error"
  )
})

test_that("validate_sirius_inputs validates output paths", {
  s <- stage_structure_fixtures()

  expect_error(
    validate_sirius_inputs(
      sirius_version = "5",
      output_ann = c("path1", "path2"),
      output_can = "can.tsv",
      output_for = "for.tsv",
      str_stereo = s$stereo,
      str_met = s$met,
      str_tax_cla = s$cla,
      str_tax_npc = s$npc,
      max_analog_abs_mz_error = 0.01
    ),
    "Output path\\(s\\) must be single character strings.*output_ann",
    class = "tima_validation_error"
  )

  expect_error(
    validate_sirius_inputs(
      sirius_version = "5",
      output_ann = "ann.tsv",
      output_can = 123,
      output_for = "for.tsv",
      str_stereo = s$stereo,
      str_met = s$met,
      str_tax_cla = s$cla,
      str_tax_npc = s$npc,
      max_analog_abs_mz_error = 0.01
    ),
    "Output path\\(s\\) must be single character strings.*output_can",
    class = "tima_validation_error"
  )

  expect_error(
    validate_sirius_inputs(
      sirius_version = "5",
      output_ann = "ann.tsv",
      output_can = "can.tsv",
      output_for = NULL,
      str_stereo = s$stereo,
      str_met = s$met,
      str_tax_cla = s$cla,
      str_tax_npc = s$npc,
      max_analog_abs_mz_error = 0.01
    ),
    "Output path\\(s\\) must be single character strings.*output_for",
    class = "tima_validation_error"
  )
})

test_that("validate_sirius_inputs checks structure file existence", {
  s <- stage_structure_fixtures()
  tmp <- tempdir()

  expect_error(
    validate_sirius_inputs(
      sirius_version = "5",
      output_ann = file.path(tmp, "ann.tsv"),
      output_can = file.path(tmp, "can.tsv"),
      output_for = file.path(tmp, "for.tsv"),
      str_stereo = "nonexistent_stereo.tsv",
      str_met = s$met,
      str_tax_cla = s$cla,
      str_tax_npc = s$npc,
      max_analog_abs_mz_error = 0.01
    ),
    "file.*not found"
  )

  expect_error(
    validate_sirius_inputs(
      sirius_version = "5",
      output_ann = file.path(tmp, "ann.tsv"),
      output_can = file.path(tmp, "can.tsv"),
      output_for = file.path(tmp, "for.tsv"),
      str_stereo = s$stereo,
      str_met = "nonexistent_metadata.tsv",
      str_tax_cla = s$cla,
      str_tax_npc = s$npc,
      max_analog_abs_mz_error = 0.01
    ),
    "file.*not found"
  )
})

test_that("validate_sirius_inputs validates max_analog_abs_mz_error", {
  s <- stage_structure_fixtures()
  tmp <- tempdir()

  expect_error(
    validate_sirius_inputs(
      sirius_version = "5",
      output_ann = file.path(tmp, "ann.tsv"),
      output_can = file.path(tmp, "can.tsv"),
      output_for = file.path(tmp, "for.tsv"),
      str_stereo = s$stereo,
      str_met = s$met,
      str_tax_cla = s$cla,
      str_tax_npc = s$npc,
      max_analog_abs_mz_error = -0.01
    ),
    "max_analog_abs_mz_error out of valid range",
    fixed = TRUE
  )
})

test_that("get_sirius_filenames returns correct names for version 6", {
  result <- get_sirius_filenames("6")

  expect_true(is.list(result))
  # v6 patterns are regex, not exact filenames
  expect_true(grepl("canopus_formula_summary", result$canopus))
  expect_true(grepl("formula_identifications", result$formulas))
  expect_true(grepl("structure_identifications", result$structures))
  expect_true(grepl("denovo_structure_identifications", result$denovo))
  expect_true(grepl("spectral_matches", result$spectral))
})

test_that("get_sirius_filenames v6 patterns match expected filenames", {
  result <- get_sirius_filenames("6")

  # Standard _all filenames
  expect_true(grepl(result$canopus, "canopus_formula_summary_all.tsv"))
  expect_true(grepl(result$formulas, "proj/formula_identifications_all.tsv"))
  expect_true(grepl(
    result$structures,
    "proj/structure_identifications_all.tsv"
  ))
  expect_true(grepl(result$denovo, "denovo_structure_identifications_all.tsv"))
  expect_true(grepl(result$spectral, "spectral_matches_all.tsv"))

  # Variant suffixes (e.g. top-N filters from SIRIUS)
  expect_true(grepl(result$canopus, "canopus_formula_summary-15.tsv"))
  expect_true(grepl(result$formulas, "proj/formula_identifications-15.tsv"))
  expect_true(grepl(result$structures, "proj/structure_identifications-15.tsv"))
  expect_true(grepl(result$denovo, "denovo_structure_identifications-15.tsv"))
  expect_true(grepl(result$spectral, "spectral_matches_analog_top-15.tsv"))

  # structures must NOT match denovo_structure_identifications
  expect_false(grepl(
    result$structures,
    "denovo_structure_identifications_all.tsv"
  ))
})

test_that("get_sirius_filenames returns correct patterns for version 6", {
  result <- get_sirius_filenames("6")

  expect_true(is.list(result))

  # Patterns should match the standard _all.tsv filenames
  expect_true(grepl(
    result$canopus,
    "canopus_formula_summary_all.tsv",
    perl = TRUE
  ))
  expect_true(grepl(
    result$formulas,
    "formula_identifications_all.tsv",
    perl = TRUE
  ))
  expect_true(grepl(
    result$structures,
    "/structure_identifications_all.tsv",
    perl = TRUE
  ))
  expect_true(grepl(
    result$denovo,
    "denovo_structure_identifications_all.tsv",
    perl = TRUE
  ))
  expect_true(grepl(result$spectral, "spectral_matches_all.tsv", perl = TRUE))

  # Patterns should also match variant suffixes (e.g. -15, _analog_top-15)
  expect_true(grepl(
    result$canopus,
    "canopus_formula_summary-15.tsv",
    perl = TRUE
  ))
  expect_true(grepl(
    result$formulas,
    "formula_identifications-15.tsv",
    perl = TRUE
  ))
  expect_true(grepl(
    result$spectral,
    "spectral_matches_analog_top-15.tsv",
    perl = TRUE
  ))

  # structures pattern must NOT match denovo_structure_identifications
  expect_false(grepl(
    result$structures,
    "denovo_structure_identifications_all.tsv",
    perl = TRUE
  ))
})

test_that("create_empty_sirius_annotations returns proper template", {
  skip_if_not_installed("tidytable")

  result <- create_empty_sirius_annotations()

  expect_true(tidytable::is_tidytable(result))
  expect_true(nrow(result) >= 0)

  # Check for SIRIUS-specific columns
  expected_cols <- c(
    "feature_pred_tax_cla_02sup_val",
    "feature_pred_tax_cla_03cla_val",
    "feature_pred_tax_npc_01pat_val",
    "candidate_score_sirius_sirius",
    "candidate_score_sirius_zodiac"
  )

  expect_true(all(expected_cols %in% names(result)))
})

test_that("split_sirius_results splits table correctly", {
  # Create mock combined table
  mock_table <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3"),
    feature_pred_tax_cla_01kin_val = c("Kingdom1", NA, "Kingdom3"),
    feature_pred_tax_cla_02sup_val = c(
      "Superclass1",
      "Superclass2",
      "Superclass2"
    ),
    candidate_adduct = c("[M+H]+", "[M+Na]+", "[M+H]+"),
    candidate_score_sirius_sirius = c(0.9, 0.8, 0.7),
    candidate_structure_inchikey = c("KEY1", "KEY2", "KEY3"),
    candidate_count_sirius_peaks_explained = c(1, 2, 3)
  )

  result <- split_sirius_results(mock_table)

  expect_true(is.list(result))
  expect_true("canopus" %in% names(result))
  expect_true("formula" %in% names(result))
  expect_true("structures" %in% names(result))

  expect_true(tidytable::is_tidytable(result$canopus))
  expect_true(tidytable::is_tidytable(result$formula))
  expect_true(tidytable::is_tidytable(result$structures))
})

test_that("merge_sirius_structures_with_spectral merges matching candidates", {
  structures_prepared <- tidytable::tidytable(
    feature_id = "F1",
    candidate_library = "SIRIUS",
    candidate_adduct = "[M+H]+",
    candidate_structure_inchikey_connectivity_layer = "ABCDEFGHIJKLMN",
    candidate_structure_smiles_no_stereo = "CCO",
    candidate_structure_name = NA_character_,
    candidate_spectrum_id = NA_character_,
    candidate_score_sirius_csi = "-25"
  )

  spectral_prepared <- tidytable::tidytable(
    feature_id = "F1",
    candidate_library = "SIRIUS spectral",
    candidate_adduct = "[M+H]+",
    candidate_structure_inchikey_connectivity_layer = "ABCDEFGHIJKLMN",
    candidate_structure_smiles_no_stereo = "CCO",
    candidate_structure_name = "ReferenceHit",
    candidate_spectrum_id = "splash10-ref",
    candidate_score_similarity = "0.86",
    candidate_count_similarity_peaks_matched = "14"
  )

  out <- merge_sirius_structures_with_spectral(
    structures_prepared,
    spectral_prepared
  )

  expect_equal(nrow(out), 1L)
  expect_equal(out$candidate_library[[1L]], "SIRIUS")
  expect_equal(as.numeric(out$candidate_score_similarity[[1L]]), 0.86)
  expect_equal(out$candidate_structure_name[[1L]], "ReferenceHit")
  expect_equal(out$candidate_spectrum_id[[1L]], "splash10-ref")
  expect_equal(
    as.integer(out$candidate_count_similarity_peaks_matched[[1L]]),
    14L
  )
  expect_equal(out$candidate_score_sirius_csi[[1L]], "-25")
})

test_that("merge_sirius_structures_with_spectral keeps all unmatched direct rows", {
  structures_prepared <- tidytable::tidytable(
    feature_id = "F1",
    candidate_library = "SIRIUS",
    candidate_adduct = "[M+H]+",
    candidate_structure_inchikey_connectivity_layer = "AAAAAAAAAAAAAA",
    candidate_structure_smiles_no_stereo = "CCO"
  )

  spectral_prepared <- tidytable::tidytable(
    feature_id = c("F2", "F2"),
    candidate_library = c("SIRIUS spectral", "SIRIUS spectral"),
    candidate_adduct = c("[M+H]+", "[M+H]+"),
    candidate_structure_inchikey_connectivity_layer = c(
      "BBBBBBBBBBBBBB",
      "BBBBBBBBBBBBBB"
    ),
    candidate_structure_smiles_no_stereo = c("CCCC", "CCCC"),
    candidate_spectrum_id = c("splash10-a", "splash10-b"),
    candidate_score_similarity = c("0.40", "0.42"),
    candidate_count_similarity_peaks_matched = c("9", "11")
  )

  out <- merge_sirius_structures_with_spectral(
    structures_prepared,
    spectral_prepared
  )

  spectral_out <- out[out$candidate_library == "SIRIUS spectral", ]
  expect_equal(nrow(spectral_out), 2L)
  expect_true(all(
    c("splash10-a", "splash10-b") %in% spectral_out$candidate_spectrum_id
  ))
})

test_that("merge_sirius_structures_with_spectral filters high-error analog rows by default", {
  structures_prepared <- tidytable::tidytable(
    feature_id = "F1",
    candidate_library = "SIRIUS",
    candidate_adduct = "[M+H]+",
    candidate_structure_inchikey_connectivity_layer = "AAAAAAAAAAAAAA",
    candidate_structure_smiles_no_stereo = "CCO"
  )

  spectral_prepared <- tidytable::tidytable(
    feature_id = "F1",
    candidate_library = "SIRIUS spectral (analog)",
    candidate_adduct = "[M+H]+",
    candidate_structure_inchikey_connectivity_layer = "BBBBBBBBBBBBBB",
    candidate_structure_smiles_no_stereo = "CCCC",
    candidate_score_similarity = "0.42",
    candidate_structure_error_mz = "17.03"
  )

  out <- merge_sirius_structures_with_spectral(
    structures_prepared,
    spectral_prepared
  )

  expect_equal(nrow(out), 1L)
  expect_true(any(out$candidate_library == "SIRIUS"))
  expect_false(any(out$candidate_library == "SIRIUS spectral (analog)"))
})

test_that("merge_sirius_structures_with_spectral merges analog hits like direct spectral", {
  # Analog hits are now treated identically to direct spectral matches:
  # if feature_id + InChIKey match, they merge into the structure row.
  structures_prepared <- tidytable::tidytable(
    feature_id = "F1",
    candidate_library = "SIRIUS",
    candidate_adduct = "[M+H]+",
    candidate_structure_inchikey_connectivity_layer = "SAMEKEY0000000",
    candidate_structure_smiles_no_stereo = "CCO",
    candidate_score_sirius_csi = "-30"
  )

  spectral_prepared <- tidytable::tidytable(
    feature_id = "F1",
    candidate_library = "SIRIUS spectral (analog)",
    candidate_adduct = "[M+H]+",
    candidate_structure_inchikey_connectivity_layer = "SAMEKEY0000000",
    candidate_structure_smiles_no_stereo = "CCO",
    candidate_score_similarity = "0.55",
    candidate_structure_error_mz = "14.02"
  )

  out <- merge_sirius_structures_with_spectral(
    structures_prepared,
    spectral_prepared,
    max_analog_abs_mz_error = 20
  )

  # Should merge into one row because keys overlap
  expect_equal(nrow(out), 1L)
  expect_equal(out$candidate_library[[1L]], "SIRIUS")
  expect_equal(as.numeric(out$candidate_score_similarity[[1L]]), 0.55)
  expect_equal(out$candidate_score_sirius_csi[[1L]], "-30")
})

test_that("merge_sirius_structures_with_spectral filters analog by threshold", {
  structures_prepared <- tidytable::tidytable(
    feature_id = "F1",
    candidate_library = "SIRIUS",
    candidate_adduct = "[M+H]+",
    candidate_structure_inchikey_connectivity_layer = "AAAAAAAAAAAAAA",
    candidate_structure_smiles_no_stereo = "CCO"
  )

  spectral_prepared <- tidytable::tidytable(
    feature_id = "F1",
    candidate_library = "SIRIUS spectral (analog)",
    candidate_adduct = "[M+H]+",
    candidate_structure_inchikey_connectivity_layer = "BBBBBBBBBBBBBB",
    candidate_structure_smiles_no_stereo = "CCCC",
    candidate_score_similarity = "0.42",
    candidate_structure_error_mz = "17.03"
  )

  out <- merge_sirius_structures_with_spectral(
    structures_prepared,
    spectral_prepared,
    max_analog_abs_mz_error = 0.02
  )

  # Analog has error 17.03 > threshold 0.02, so it is filtered out
  expect_equal(nrow(out), 1L)
  expect_true(any(out$candidate_library == "SIRIUS"))
  expect_false(any(out$candidate_library == "SIRIUS spectral (analog)"))
})

test_that("merge_sirius_structures_with_spectral keeps analog standalone when no key match", {
  structures_prepared <- tidytable::tidytable(
    feature_id = "F1",
    candidate_library = "SIRIUS",
    candidate_adduct = "[M+H]+",
    candidate_structure_inchikey_connectivity_layer = "STRUCTKEY000001",
    candidate_structure_smiles_no_stereo = "CCO",
    candidate_score_sirius_csi = "-30"
  )

  spectral_prepared <- tidytable::tidytable(
    feature_id = "F1",
    candidate_library = "SIRIUS spectral (analog)",
    candidate_adduct = "[M+H]+",
    candidate_structure_inchikey_connectivity_layer = "DIFFERENTKEY000",
    candidate_structure_smiles_no_stereo = "CCCC",
    candidate_score_similarity = "0.55",
    candidate_structure_error_mz = "14.02"
  )

  out <- merge_sirius_structures_with_spectral(
    structures_prepared,
    spectral_prepared,
    max_analog_abs_mz_error = 20
  )

  # Different InChIKeys => no overlap => both stay as separate candidates
  expect_equal(nrow(out), 2L)
  expect_true(any(out$candidate_library == "SIRIUS"))
  expect_true(any(out$candidate_library == "SIRIUS spectral (analog)"))
})

test_that("merge_sirius_structures_with_spectral can keep high-error analogs when disabled", {
  structures_prepared <- tidytable::tidytable(
    feature_id = "F1",
    candidate_library = "SIRIUS",
    candidate_adduct = "[M+H]+",
    candidate_structure_inchikey_connectivity_layer = "AAAAAAAAAAAAAA",
    candidate_structure_smiles_no_stereo = "CCO"
  )

  spectral_prepared <- tidytable::tidytable(
    feature_id = "F1",
    candidate_library = "SIRIUS spectral (analog)",
    candidate_adduct = "[M+H]+",
    candidate_structure_inchikey_connectivity_layer = "BBBBBBBBBBBBBB",
    candidate_structure_smiles_no_stereo = "CCCC",
    candidate_score_similarity = "0.42",
    candidate_structure_error_mz = "17.03"
  )

  out <- merge_sirius_structures_with_spectral(
    structures_prepared,
    spectral_prepared,
    max_analog_abs_mz_error = Inf
  )

  expect_equal(nrow(out), 2L)
  expect_true(any(out$candidate_library == "SIRIUS spectral (analog)"))
})

test_that("join_sirius_annotation_tables keeps formula mz error on structure rows", {
  structures_prepared <- tidytable::tidytable(
    feature_id = "F1",
    candidate_adduct = "[M+H]+",
    candidate_structure_molecular_formula = "C10H12N2",
    candidate_structure_inchikey_connectivity_layer = "ABCDEFGHIJKLMN",
    candidate_score_sirius_csi = "-25"
  )

  formulas_prepared <- tidytable::tidytable(
    feature_id = "F1",
    candidate_adduct = "[M+H]+",
    candidate_structure_molecular_formula = "C10H12N2",
    candidate_structure_exact_mass = 160.1000,
    candidate_structure_error_mz = 0.0012,
    candidate_score_sirius_sirius = 15
  )

  canopus_prepared <- tidytable::tidytable(
    feature_id = "F1",
    candidate_adduct = "[M+H]+",
    candidate_structure_molecular_formula = "C10H12N2",
    feature_pred_tax_npc_01pat_val = "Alkaloids"
  )

  denovo_prepared <- tidytable::tidytable(
    feature_id = "F1",
    candidate_adduct = "[M+H]+",
    candidate_structure_molecular_formula = "C10H12N2",
    candidate_structure_inchikey_connectivity_layer = "ABCDEFGHIJKLMN",
    candidate_score_sirius_csi = "-20"
  )

  out <- join_sirius_annotation_tables(
    structures_prepared = structures_prepared,
    formulas_prepared = formulas_prepared,
    canopus_prepared = canopus_prepared,
    denovo_prepared = denovo_prepared
  )

  expect_equal(nrow(out), 1L)
  expect_equal(as.numeric(out$candidate_structure_error_mz[[1L]]), 0.0012)
  expect_equal(out$feature_pred_tax_npc_01pat_val[[1L]], "Alkaloids")
})

test_that("join_sirius_annotation_tables retains de novo-only candidates", {
  structures_prepared <- tidytable::tidytable(
    feature_id = "F1",
    candidate_adduct = "[M+H]+",
    candidate_structure_molecular_formula = "C10H12N2",
    candidate_structure_inchikey_connectivity_layer = "STRUCTKEY000001",
    candidate_structure_smiles_no_stereo = "CCO",
    candidate_library = "SIRIUS"
  )

  formulas_prepared <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    candidate_adduct = c("[M+H]+", "[M+H]+"),
    candidate_structure_molecular_formula = c("C10H12N2", "C11H14N2"),
    candidate_structure_error_mz = c(0.0012, 0.0025)
  )

  canopus_prepared <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    candidate_adduct = c("[M+H]+", "[M+H]+"),
    candidate_structure_molecular_formula = c("C10H12N2", "C11H14N2"),
    feature_pred_tax_npc_01pat_val = c("Alkaloids", "Peptides")
  )

  denovo_prepared <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    candidate_adduct = c("[M+H]+", "[M+H]+"),
    candidate_structure_molecular_formula = c("C10H12N2", "C11H14N2"),
    candidate_structure_inchikey_connectivity_layer = c(
      "STRUCTKEY000001",
      "DENOVOKEY000002"
    ),
    candidate_structure_smiles_no_stereo = c("CCO", "CCN"),
    candidate_library = c("SIRIUS", "SIRIUS")
  )

  out <- join_sirius_annotation_tables(
    structures_prepared = structures_prepared,
    formulas_prepared = formulas_prepared,
    canopus_prepared = canopus_prepared,
    denovo_prepared = denovo_prepared
  )

  expect_equal(nrow(out), 2L)
  expect_true(any(out$feature_id == "F2"))
  denovo_row <- out[out$feature_id == "F2", ]
  expect_equal(
    denovo_row$candidate_structure_inchikey_connectivity_layer[[1L]],
    "DENOVOKEY000002"
  )
  expect_equal(
    as.numeric(denovo_row$candidate_structure_error_mz[[1L]]),
    0.0025
  )
  expect_equal(denovo_row$feature_pred_tax_npc_01pat_val[[1L]], "Peptides")
})

test_that("join_sirius_annotation_tables backfills de novo error_mz by feature_id", {
  structures_prepared <- tidytable::tidytable(
    feature_id = "F1",
    candidate_adduct = "[M+H]+",
    candidate_structure_molecular_formula = "C10H12N2",
    candidate_structure_inchikey_connectivity_layer = "STRUCTKEY000001",
    candidate_structure_smiles_no_stereo = "CCO",
    candidate_library = "SIRIUS"
  )

  formulas_prepared <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    candidate_adduct = c("[M+H]+", "[M+H]+"),
    candidate_structure_molecular_formula = c("C10H12N2", "C11H14N2"),
    candidate_structure_error_mz = c(0.0012, 0.0042)
  )

  canopus_prepared <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    feature_pred_tax_npc_01pat_val = c("Alkaloids", "Peptides")
  )

  # F2 is de novo-only and intentionally missing adduct/formula keys
  denovo_prepared <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    candidate_structure_inchikey_connectivity_layer = c(
      "STRUCTKEY000001",
      "DENOVOKEY000002"
    ),
    candidate_structure_smiles_no_stereo = c("CCO", "CCN"),
    candidate_library = c("SIRIUS", "SIRIUS")
  )

  out <- join_sirius_annotation_tables(
    structures_prepared = structures_prepared,
    formulas_prepared = formulas_prepared,
    canopus_prepared = canopus_prepared,
    denovo_prepared = denovo_prepared
  )

  expect_equal(nrow(out), 2L)
  denovo_row <- out[out$feature_id == "F2", ]
  expect_equal(
    as.numeric(denovo_row$candidate_structure_error_mz[[1L]]),
    0.0042
  )
})

test_that("join_sirius_annotation_tables ignores non-overlapping optional join keys", {
  structures_prepared <- tidytable::tidytable(
    feature_id = "F1",
    candidate_adduct = "[M+H]+",
    candidate_structure_molecular_formula = "C10H12N2",
    candidate_structure_inchikey_connectivity_layer = "STRUCTKEY000001",
    candidate_structure_smiles_no_stereo = "CCO",
    candidate_library = "SIRIUS"
  )

  formulas_prepared <- tidytable::tidytable(
    feature_id = "F2",
    candidate_adduct = "[M+H]+",
    candidate_structure_molecular_formula = "C11H14N2",
    candidate_structure_error_mz = 0.0061
  )

  canopus_prepared <- tidytable::tidytable(feature_id = character())

  # De novo-only row has optional key columns present but non-overlapping/NA.
  # Previous logic used all shared columns and over-constrained joins.
  denovo_prepared <- tidytable::tidytable(
    feature_id = "F2",
    candidate_adduct = NA_character_,
    candidate_structure_molecular_formula = NA_character_,
    candidate_structure_inchikey_connectivity_layer = "DENOVOKEY000002",
    candidate_structure_smiles_no_stereo = "CCN",
    candidate_library = "SIRIUS"
  )

  out <- join_sirius_annotation_tables(
    structures_prepared = structures_prepared,
    formulas_prepared = formulas_prepared,
    canopus_prepared = canopus_prepared,
    denovo_prepared = denovo_prepared
  )

  denovo_row <- out[out$feature_id == "F2", ]
  expect_equal(nrow(denovo_row), 1L)
  expect_equal(
    as.numeric(denovo_row$candidate_structure_error_mz[[1L]]),
    0.0061
  )
})

test_that("join_sirius_annotation_tables fills de novo error_mz despite adduct formatting mismatch", {
  structures_prepared <- tidytable::tidytable(
    feature_id = "F1",
    candidate_adduct = "[M-H]-",
    candidate_structure_molecular_formula = "C7H14N2O3",
    candidate_structure_inchikey_connectivity_layer = "STRUCTKEY000001",
    candidate_structure_smiles_no_stereo = "CCO",
    candidate_library = "SIRIUS"
  )

  formulas_prepared <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    candidate_adduct = c("[M-H]-", "[M-H]-"),
    candidate_structure_molecular_formula = c("C7H14N2O3", "C7H14N2O3"),
    candidate_structure_error_mz = c(0.0012, 0.5860)
  )

  canopus_prepared <- tidytable::tidytable(feature_id = character())

  denovo_prepared <- tidytable::tidytable(
    feature_id = "F2",
    candidate_adduct = "[M - H]-",
    candidate_structure_molecular_formula = "C7H14N2O3",
    candidate_structure_inchikey_connectivity_layer = "DENOVOKEY000002",
    candidate_structure_smiles_no_stereo = "CCN",
    candidate_library = "SIRIUS"
  )

  out <- join_sirius_annotation_tables(
    structures_prepared = structures_prepared,
    formulas_prepared = formulas_prepared,
    canopus_prepared = canopus_prepared,
    denovo_prepared = denovo_prepared
  )

  denovo_row <- out[out$feature_id == "F2", ]
  expect_equal(nrow(denovo_row), 1L)
  expect_equal(
    as.numeric(denovo_row$candidate_structure_error_mz[[1L]]),
    0.5860
  )
})

test_that("load_sirius_tables filters formulaRank and loads denovo/spectral when present", {
  local_mocked_bindings(
    read_from_sirius_zip = function(input_directory, file) {
      if (grepl("canopus_formula_summary", file)) {
        return(tidytable::tidytable(formulaRank = c(1L, 2L), x = c("a", "b")))
      }
      if (grepl("formula_identifications", file)) {
        return(tidytable::tidytable(formulaRank = c(1L, 3L), y = c("c", "d")))
      }
      if (grepl("structure_identifications", file) && !grepl("denovo", file)) {
        return(tidytable::tidytable(id = 1:2))
      }
      if (grepl("denovo_structure_identifications", file)) {
        return(tidytable::tidytable(mappingFeatureId = c("F1", "F2")))
      }
      tidytable::tidytable()
    },
    read_sirius_internal_file = function(input_directory, internal_file) {
      force(input_directory)
      tidytable::tidytable(
        mappingFeatureId = "F_spec",
        analogHit = grepl("analog", internal_file),
        similarity = "0.8",
        sharedPeaks = "10"
      )
    },
    .package = "tima"
  )
  local_mocked_bindings(
    unzip = function(zipfile, list = FALSE, ...) {
      if (isTRUE(list)) {
        return(data.frame(
          Name = c(
            "denovo_structure_identifications_all.tsv",
            "spectral_matches_all.tsv",
            "spectral_matches_analog_all.tsv"
          )
        ))
      }
      invisible(NULL)
    },
    .package = "utils"
  )

  out <- load_sirius_tables(input_directory = "dummy.zip", version = "6")

  expect_equal(nrow(out$canopus), 1L)
  expect_equal(nrow(out$formulas), 1L)
  expect_equal(nrow(out$structures), 2L)
  expect_equal(nrow(out$denovo), 2L)
  expect_equal(nrow(out$spectral), 2L)
})

test_that("load_sirius_tables v6 leaves spectral template when no spectral files", {
  local_mocked_bindings(
    read_from_sirius_zip = function(input_directory, file) {
      force(input_directory)
      if (grepl("canopus_formula_summary", file)) {
        return(tidytable::tidytable(formulaRank = 1L, x = "a"))
      }
      if (grepl("formula_identifications", file)) {
        return(tidytable::tidytable(formulaRank = 1L, y = "b"))
      }
      if (grepl("structure_identifications", file) && !grepl("denovo", file)) {
        return(tidytable::tidytable(id = 1L))
      }
      tidytable::tidytable()
    },
    .package = "tima"
  )

  local_mocked_bindings(
    unzip = function(zipfile, list = FALSE, ...) {
      force(zipfile)
      invisible(list(...))
      if (isTRUE(list)) {
        return(data.frame(Name = c("structure_identifications_all.tsv")))
      }
      invisible(NULL)
    },
    .package = "utils"
  )

  out <- load_sirius_tables(input_directory = "dummy.zip", version = "6")

  expect_equal(nrow(out$spectral), 1L)
  expect_true(all(is.na(out$spectral$mappingFeatureId)))
})

test_that("load_sirius_summaries returns empty when no summary files", {
  local_mocked_bindings(
    unzip = function(zipfile, list = FALSE, ...) {
      if (isTRUE(list)) {
        return(data.frame(Name = c("other.tsv")))
      }
      invisible(NULL)
    },
    .package = "utils"
  )

  out <- load_sirius_summaries("dummy.zip")
  expect_equal(nrow(out), 0L)
})

test_that("load_sirius_summaries loads and binds discovered summary files", {
  local_mocked_bindings(
    unzip = function(zipfile, list = FALSE, ...) {
      if (isTRUE(list)) {
        return(data.frame(
          Name = c(
            "dummy/feat1/structure_candidates.tsv",
            "dummy/feat2/structure_candidates.tsv"
          )
        ))
      }
      invisible(NULL)
    },
    .package = "utils"
  )
  local_mocked_bindings(
    read_from_sirius_zip = function(input_directory, file) {
      if (grepl("feat1", file)) {
        return(tidytable::tidytable(score = 0.9))
      }
      tidytable::tidytable(score = 0.8)
    },
    .package = "tima"
  )

  out <- load_sirius_summaries("dummy.zip")
  expect_true("feature_id" %in% names(out))
  expect_equal(nrow(out), 2L)
})

test_that("load_sirius_summaries returns empty when all discovered summaries are empty", {
  local_mocked_bindings(
    unzip = function(zipfile, list = FALSE, ...) {
      force(zipfile)
      invisible(list(...))
      if (isTRUE(list)) {
        return(data.frame(
          Name = c(
            "dummy/feat1/structure_candidates.tsv",
            "dummy/feat2/structure_candidates.tsv"
          )
        ))
      }
      invisible(NULL)
    },
    .package = "utils"
  )

  local_mocked_bindings(
    read_from_sirius_zip = function(input_directory, file) {
      force(input_directory)
      force(file)
      tidytable::tidytable()
    },
    .package = "tima"
  )

  out <- load_sirius_summaries("dummy.zip")
  expect_true(tidytable::is_tidytable(out))
  expect_equal(nrow(out), 0L)
})

test_that("load_sirius_tables v6 keeps only formulaRank 1 when ranks are character", {
  skip_if_not_installed("archive")

  tmp <- temp_test_dir("sirius_formula_rank_char")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)

  can_path <- file.path(tmp, "canopus_formula_summary_all.tsv")
  for_path <- file.path(tmp, "formula_identifications_all.tsv")
  str_path <- file.path(tmp, "structure_identifications_all.tsv")
  zip_path <- file.path(tmp, "sirius_mock.zip")

  can_df <- data.frame(
    mappingFeatureId = c("F1", "F1"),
    formulaRank = c("1", "2"),
    adduct = c("[M+H]+", "[M+H]+"),
    molecularFormula = c("C10H12N2", "C9H10N2"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  for_df <- data.frame(
    mappingFeatureId = c("F1", "F1"),
    formulaRank = c("1", "2"),
    adduct = c("[M+H]+", "[M+H]+"),
    ionMass = c("160.1", "150.1"),
    `massErrorPrecursor.ppm.` = c("1.2", "2.4"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  str_df <- data.frame(
    mappingFeatureId = "F1",
    adduct = "[M+H]+",
    InChIkey2D = "AAAAAAAAAAAAAA",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  utils::write.table(
    can_df,
    can_path,
    sep = "\t",
    quote = FALSE,
    row.names = FALSE
  )
  utils::write.table(
    for_df,
    for_path,
    sep = "\t",
    quote = FALSE,
    row.names = FALSE
  )
  utils::write.table(
    str_df,
    str_path,
    sep = "\t",
    quote = FALSE,
    row.names = FALSE
  )

  old_wd <- getwd()
  setwd(tmp)
  on.exit(setwd(old_wd), add = TRUE)
  utils::zip(
    zipfile = zip_path,
    files = c(
      basename(can_path),
      basename(for_path),
      basename(str_path)
    )
  )

  out <- load_sirius_tables(zip_path, version = "6")

  expect_equal(nrow(out$canopus), 1L)
  expect_equal(nrow(out$formulas), 1L)
  expect_equal(out$canopus$formulaRank[[1L]], "1")
  expect_equal(out$formulas$formulaRank[[1L]], "1")
})
