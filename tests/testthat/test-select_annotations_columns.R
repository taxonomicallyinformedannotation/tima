# Test Suite: select_annotations_columns ----

library(testthat)

make_fake_struct_files <- function(root = tempdir()) {
  fixture_dir <- file.path(
    root,
    paste0("select-annotations-", as.integer(stats::runif(1L) * 1e9))
  )
  dir.create(fixture_dir, recursive = TRUE, showWarnings = FALSE)

  paths <- list(
    stereo = file.path(fixture_dir, "stereo.tsv"),
    met = file.path(fixture_dir, "metadata.tsv"),
    cla = file.path(fixture_dir, "classyfire.tsv"),
    npc = file.path(fixture_dir, "npclassifier.tsv")
  )

  tidytable::fwrite(
    tidytable::tidytable(
      structure_inchikey = character(),
      structure_smiles = character(),
      structure_inchikey_connectivity_layer = character(),
      structure_inchikey_no_stereo = character(),
      structure_smiles_no_stereo = character(),
      structure_xlogp = character(),
      structure_name = character(),
      structure_tag = character()
    ),
    paths$stereo
  )

  tidytable::fwrite(
    tidytable::tidytable(
      structure_inchikey_no_stereo = character(),
      structure_exact_mass = character(),
      structure_molecular_formula = character()
    ),
    paths$met
  )

  tidytable::fwrite(
    tidytable::tidytable(
      structure_inchikey = character(),
      structure_tax_cla_chemontid = character(),
      structure_tax_cla_01kin = character(),
      structure_tax_cla_02sup = character(),
      structure_tax_cla_03cla = character(),
      structure_tax_cla_04dirpar = character()
    ),
    paths$cla
  )

  tidytable::fwrite(
    tidytable::tidytable(
      structure_smiles = character(),
      structure_tax_npc_01pat = character(),
      structure_tax_npc_02sup = character(),
      structure_tax_npc_03cla = character()
    ),
    paths$npc
  )

  paths
}

test_that("select_annotations_columns handles empty input", {
  paths <- make_fake_struct_files()
  df <- tidytable::tidytable()
  result <- select_annotations_columns(
    df = df,
    str_stereo = paths$stereo,
    str_met = paths$met,
    str_tax_cla = paths$cla,
    str_tax_npc = paths$npc
  )
  expect_equal(nrow(result), 0)
})

test_that("select_annotations_columns cleans text sentinels and keeps expected columns", {
  paths <- make_fake_struct_files()

  df <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3"),
    feature_spectrum_entropy = c(0.5, 0.9, 1.1),
    candidate_structure_molecular_formula = c("N/A", "null", " C10H20 "),
    candidate_structure_inchikey_connectivity_layer = c("IK1", "IK2", "IK3"),
    candidate_score_sirius_intensity = c(100, 200, 300)
  )

  result <- select_annotations_columns(
    df = df,
    str_stereo = paths$stereo,
    str_met = paths$met,
    str_tax_cla = paths$cla,
    str_tax_npc = paths$npc,
    recompute_smiles = FALSE
  )

  expect_equal(result$candidate_structure_molecular_formula[[1]], NA_character_)
  expect_equal(result$candidate_structure_molecular_formula[[2]], NA_character_)
  expect_equal(result$candidate_structure_molecular_formula[[3]], "C10H20")
  expect_true(all(
    c(
      "feature_id",
      "candidate_structure_molecular_formula",
      "candidate_score_sirius_intensity"
    ) %in%
      names(result)
  ))
})

test_that("select_annotations_columns converts numeric outputs to character once standardized", {
  paths <- make_fake_struct_files()

  df <- tidytable::tidytable(
    feature_id = "F1",
    feature_spectrum_entropy = 0.5,
    candidate_score_sirius_intensity = 100,
    candidate_structure_exact_mass = 123.456789,
    candidate_structure_inchikey_connectivity_layer = "IK1"
  )

  result <- select_annotations_columns(
    df = df,
    str_stereo = paths$stereo,
    str_met = paths$met,
    str_tax_cla = paths$cla,
    str_tax_npc = paths$npc,
    recompute_smiles = FALSE
  )

  expect_type(result$feature_spectrum_entropy, "character")
  expect_type(result$candidate_score_sirius_intensity, "character")
  expect_type(result$candidate_structure_exact_mass, "character")
  expect_equal(result$candidate_structure_exact_mass, "123.45679")
})

test_that("select_annotations_columns drops duplicate raw structure metadata columns", {
  paths <- make_fake_struct_files()

  df <- tidytable::tidytable(
    feature_id = "F1",
    candidate_structure_molecular_formula = "C2H6O",
    candidate_structure_exact_mass = 46.0419,
    candidate_structure_xlogp = -0.3,
    candidate_structure_tag = "trusted",
    candidate_structure_name = "ethanol",
    candidate_structure_inchikey_connectivity_layer = "ABCDEFGHIJKLMN",
    candidate_structure_smiles_no_stereo = "CCO"
  )

  result <- select_annotations_columns(
    df = df,
    str_stereo = paths$stereo,
    str_met = paths$met,
    str_tax_cla = paths$cla,
    str_tax_npc = paths$npc,
    recompute_smiles = FALSE
  )

  expect_true("candidate_structure_molecular_formula" %in% names(result))
  expect_false(any(grepl("^structure_", names(result))))
})

test_that("select_annotations_columns preserves annotation provenance columns", {
  paths <- make_fake_struct_files()

  df <- tidytable::tidytable(
    feature_id = "F1",
    candidate_adduct = "[M+Na]+",
    candidate_query_adduct = "[M+H]+",
    candidate_adduct_match_mode = "m_delta_rescued",
    candidate_adduct_origin = "supported",
    candidate_annotation_level = "primary",
    candidate_evidence_tier = "supported_strong",
    adduct_support = 3L,
    annotation_note = paste(
      "Spectral match rescued in neutral-mass space:",
      "observed adduct [M+H]+, library adduct [M+Na]+"
    ),
    candidate_library = "spectral_lib",
    candidate_structure_inchikey_connectivity_layer = "IK1",
    candidate_structure_smiles_no_stereo = "C"
  )

  result <- select_annotations_columns(
    df = df,
    str_stereo = paths$stereo,
    str_met = paths$met,
    str_tax_cla = paths$cla,
    str_tax_npc = paths$npc,
    recompute_smiles = FALSE
  )

  expect_true(all(
    c(
      "candidate_query_adduct",
      "candidate_adduct_match_mode",
      "candidate_adduct_origin",
      "candidate_annotation_level",
      "candidate_evidence_tier",
      "adduct_support",
      "annotation_note"
    ) %in%
      names(result)
  ))
  expect_equal(result$candidate_query_adduct[[1L]], "[M+H]+")
  expect_equal(result$candidate_adduct_match_mode[[1L]], "m_delta_rescued")
  expect_equal(result$candidate_adduct_origin[[1L]], "supported")
  expect_equal(result$candidate_annotation_level[[1L]], "primary")
  expect_equal(result$candidate_evidence_tier[[1L]], "supported_strong")
  expect_equal(result$adduct_support[[1L]], "3")
  expect_match(result$annotation_note[[1L]], "neutral-mass space")
})

test_that("recompute_structure_fields_from_smiles updates fields without join artifacts", {
  df <- tidytable::tidytable(
    candidate_structure_smiles_no_stereo = c("CCO", "", NA_character_),
    candidate_structure_inchikey_connectivity_layer = c("old1", "old2", "old3"),
    candidate_structure_inchikey_no_stereo = c("old_ns1", "old_ns2", "old_ns3"),
    candidate_structure_molecular_formula = c("old_f1", "old_f2", "old_f3"),
    candidate_structure_exact_mass = c("old_m1", "old_m2", "old_m3"),
    candidate_structure_xlogp = c("old_x1", "old_x2", "old_x3")
  )

  result <- recompute_structure_fields_from_smiles(df)

  expect_true(
    result$candidate_structure_inchikey_connectivity_layer[[1]] != "old1"
  )
  expect_true(result$candidate_structure_inchikey_no_stereo[[1]] != "old_ns1")
  expect_equal(result$candidate_structure_molecular_formula[[1]], "C2H6O")
  expect_true(is.numeric(result$candidate_structure_exact_mass))
  expect_equal(
    result$candidate_structure_exact_mass[[1]],
    46.041864812,
    tolerance = 1e-7
  )
  expect_true(is.numeric(result$candidate_structure_xlogp))
  expect_true(is.finite(result$candidate_structure_xlogp[[1]]))

  expect_true(is.na(result$candidate_structure_molecular_formula[[2]]))
  expect_true(is.na(result$candidate_structure_molecular_formula[[3]]))
  expect_false(any(grepl("^\\.recomputed_", names(result))))
})

# test_that(
#   skip("Not implemented")
# )
# test_that("select_annotations_columns selects and cleans columns", {
#   paths <- make_fake_struct_files()
#   df <- tidytable::tidytable(
#     feature_id = c("F1", "F2"),
#     feature_spectrum_entropy = c(0.5, 0.9),
#     candidate_structure_molecular_formula = c("C8H10N4O2", "C7H8N4O2"),
#     candidate_score_sirius_intensity = c(100, 200),
#     candidate_score_similarity = c(0.8, 0.9),
#     candidate_structure_inchikey_connectivity_layer = c("AAAA", "BBBB"),
#     candidate_library = c("LIB", "LIB"),
#     candidate_structure_error_rt = c(0.01, 0.02)
#   )
#
#   result <- select_annotations_columns(
#     df = df,
#     str_stereo = paths$stereo,
#     str_met = paths$met,
#     str_tax_cla = paths$cla,
#     str_tax_npc = paths$npc
#   )
#
#   expect_true("feature_id" %in% names(result))
#   expect_true("candidate_structure_molecular_formula" %in% names(result))
#   expect_true("candidate_score_sirius_intensity" %in% names(result))
#   expect_true("candidate_score_similarity" %in% names(result))
#   expect_s3_class(result, "data.frame")
# })

# test_that("select_annotations_columns converts numeric to character", {
#   paths <- make_fake_struct_files()
#   df <- tidytable::tidytable(
#     feature_id = "F1",
#     feature_spectrum_entropy = 0.5,
#     candidate_score_sirius_intensity = 100,
#     candidate_structure_error_rt = 0.01
#   )
#   result <- select_annotations_columns(
#     df = df,
#     str_stereo = paths$stereo,
#     str_met = paths$met,
#     str_tax_cla = paths$cla,
#     str_tax_npc = paths$npc
#   )
#   # feature_spectrum_entropy becomes character after conversion
#   expect_type(result$feature_spectrum_entropy, "character")
# })

# test_that(
#   skip("Not implemented")
# )
# test_that("select_annotations_columns preserves row count after cleaning", {
#   paths <- make_fake_struct_files()
#   df <- tidytable::tidytable(
#     feature_id = c("F1", "F2"),
#     feature_spectrum_entropy = c(0.5, 0.9)
#   )
#   result <- select_annotations_columns(
#     df = df,
#     str_stereo = paths$stereo,
#     str_met = paths$met,
#     str_tax_cla = paths$cla,
#     str_tax_npc = paths$npc
#   )
#   expect_equal(nrow(result), 2)
# })
