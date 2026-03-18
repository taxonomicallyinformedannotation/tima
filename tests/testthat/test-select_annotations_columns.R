# Test Suite: select_annotations_columns ----

library(testthat)

make_fake_struct_files <- function(root = tempdir()) {
  paths <- list(
    stereo = file.path(root, "stereo.tsv"),
    met = file.path(root, "metadata.tsv"),
    nam = file.path(root, "names.tsv"),
    cla = file.path(root, "classyfire.tsv"),
    npc = file.path(root, "npclassifier.tsv")
  )
  tidytable::fwrite(
    tidytable::tidytable(structure_inchikey_connectivity_layer = NA),
    paths$stereo
  )
  tidytable::fwrite(
    tidytable::tidytable(structure_inchikey_connectivity_layer = NA),
    paths$met
  )
  tidytable::fwrite(
    tidytable::tidytable(structure_inchikey_connectivity_layer = NA),
    paths$nam
  )
  tidytable::fwrite(
    tidytable::tidytable(structure_inchikey_connectivity_layer = NA),
    paths$cla
  )
  tidytable::fwrite(
    tidytable::tidytable(structure_inchikey_connectivity_layer = NA),
    paths$npc
  )
  paths
}

test_that("select_annotations_columns handles empty input", {
  df <- tidytable::tidytable()
  result <- select_annotations_columns(df)
  expect_equal(nrow(result), 0)
})

test_that("select_annotations_columns cleans text sentinels and keeps expected columns", {
  paths <- make_fake_struct_files()

  local_mocked_bindings(
    complement_metadata_structures = function(df, ...) {
      invisible(list(...))
      df
    },
    .package = "tima"
  )

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
    str_nam = paths$nam,
    str_tax_cla = paths$cla,
    str_tax_npc = paths$npc
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

  local_mocked_bindings(
    complement_metadata_structures = function(df, ...) {
      invisible(list(...))
      df
    },
    .package = "tima"
  )

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
    str_nam = paths$nam,
    str_tax_cla = paths$cla,
    str_tax_npc = paths$npc
  )

  expect_type(result$feature_spectrum_entropy, "character")
  expect_type(result$candidate_score_sirius_intensity, "character")
  expect_type(result$candidate_structure_exact_mass, "character")
  expect_equal(result$candidate_structure_exact_mass, "123.45679")
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
#     str_nam = paths$nam,
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
#     str_nam = paths$nam,
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
#     str_nam = paths$nam,
#     str_tax_cla = paths$cla,
#     str_tax_npc = paths$npc
#   )
#   expect_equal(nrow(result), 2)
# })
