# Test Suite: prepare_annotations_mzmine ----

library(testthat)

## Internal Utility Helpers ----

#' Stage structure fixtures for mzmine annotation tests
#'
#' @param root Root directory for staging
#' @return Named list of structure file paths
stage_mzmine_structure_fixtures <- function(
  root = temp_test_dir("prep_mzmine_structures")
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

  # Copy fixtures
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

test_that("test-prepare_annotations_mzmine validates output path", {
  s <- stage_mzmine_structure_fixtures()
  withr::local_dir(new = as.character(s$root))

  expect_error(
    prepare_annotations_mzmine(
      input = character(0),
      output = c("a.tsv", "b.tsv"),
      str_stereo = s$stereo,
      str_met = s$met,
      str_nam = s$nam,
      str_tax_cla = s$cla,
      str_tax_npc = s$npc
    ),
    "output must be a single character string"
  )
})

test_that("test-prepare_annotations_mzmine validates structure file paths", {
  out <- temp_test_path("mzmine.tsv")

  expect_error(
    prepare_annotations_mzmine(
      input = character(0),
      output = out,
      str_stereo = temp_test_path("missing.tsv"),
      str_met = temp_test_path("missing.tsv"),
      str_nam = temp_test_path("missing.tsv"),
      str_tax_cla = temp_test_path("missing.tsv"),
      str_tax_npc = temp_test_path("missing.tsv")
    ),
    "Required file(s) not found",
    fixed = TRUE
  )
})

## Behavior ----

test_that("test-prepare_annotations_mzmine handles missing input files by creating empty output", {
  s <- stage_mzmine_structure_fixtures()
  withr::local_dir(new = as.character(s$root))

  out <- temp_test_path("mzmine_empty.tsv")
  res <- prepare_annotations_mzmine(
    input = temp_test_path("does_not_exist.tsv"),
    output = out,
    str_stereo = s$stereo,
    str_met = s$met,
    str_nam = s$nam,
    str_tax_cla = s$cla,
    str_tax_npc = s$npc
  )
  expect_equal(res, out)
  expect_true(file.exists(out))
  df <- tidytable::fread(out)
  expect_true(is.data.frame(df))
})

test_that("test-prepare_annotations_mzmine processes minimal valid mzmine file", {
  s <- stage_mzmine_structure_fixtures()
  withr::local_dir(new = as.character(s$root))
  out <- temp_test_path("mzmine_valid.tsv")

  mzmine_data <- tidytable::tidytable(
    `id` = c("F1"),
    `compound_name` = c("Quercetin"),
    `inchi_key` = c("AAAAAAAAAAAAA-BBBBBBBBBB-C"),
    `smiles` = c("CCCC"),
    `mol_formula` = c("C10H12O2"),
    `adduct` = c("[M+H]+"),
    `score` = c("0.85"),
    `precursor_mz` = c("303.05")
  )
  mzmine_path <- temp_test_path("mzmine_in.tsv")
  tidytable::fwrite(x = mzmine_data, file = mzmine_path, sep = "\t")

  res <- prepare_annotations_mzmine(
    input = mzmine_path,
    output = out,
    str_stereo = s$stereo,
    str_met = s$met,
    str_nam = s$nam,
    str_tax_cla = s$cla,
    str_tax_npc = s$npc
  )
  expect_equal(res, out)
  expect_true(file.exists(out))
  df <- tidytable::fread(out)
  expect_true("feature_id" %in% names(df))
  expect_true("candidate_library" %in% names(df))
  expect_equal(df$candidate_library[1], "mzmine")
})

test_that("test-prepare_annotations_mzmine handles empty input vector", {
  s <- stage_mzmine_structure_fixtures()
  withr::local_dir(new = as.character(s$root))
  out <- temp_test_path("mzmine_empty_vec.tsv")

  res <- prepare_annotations_mzmine(
    input = character(0),
    output = out,
    str_stereo = s$stereo,
    str_met = s$met,
    str_nam = s$nam,
    str_tax_cla = s$cla,
    str_tax_npc = s$npc
  )
  expect_equal(res, out)
  expect_true(file.exists(out))
  df <- tidytable::fread(out)
  expect_true(is.data.frame(df))
})

test_that("test-prepare_annotations_mzmine handles multiple input files", {
  s <- stage_mzmine_structure_fixtures()
  withr::local_dir(new = as.character(s$root))
  out <- temp_test_path("mzmine_multi.tsv")

  mzmine_data1 <- tidytable::tidytable(
    `id` = c("F1"),
    `compound_name` = c("Cmpd1"),
    `inchi_key` = c("AAAAAAAAAAAAA-BBBBBBBBBB-C"),
    `smiles` = c("CCCC"),
    `mol_formula` = c("C10H12O2"),
    `adduct` = c("[M+H]+"),
    `score` = c("0.9"),
    `precursor_mz` = c("300.0")
  )
  mzmine_data2 <- tidytable::tidytable(
    `id` = c("F2"),
    `compound_name` = c("Cmpd2"),
    `inchi_key` = c("AAAAAAAAAAAAA-BBBBBBBBBB-A"),
    `smiles` = c("CCCCOC"),
    `mol_formula` = c("C10H12O2"),
    `adduct` = c("[M-H]-"),
    `score` = c("0.75"),
    `precursor_mz` = c("350.0")
  )
  path1 <- temp_test_path("mzmine_multi_1.tsv")
  path2 <- temp_test_path("mzmine_multi_2.tsv")
  tidytable::fwrite(x = mzmine_data1, file = path1, sep = "\t")
  tidytable::fwrite(x = mzmine_data2, file = path2, sep = "\t")

  res <- prepare_annotations_mzmine(
    input = c(path1, path2),
    output = out,
    str_stereo = s$stereo,
    str_met = s$met,
    str_nam = s$nam,
    str_tax_cla = s$cla,
    str_tax_npc = s$npc
  )
  expect_equal(res, out)
  expect_true(file.exists(out))
  df <- tidytable::fread(out)
  expect_true(nrow(df) >= 2)
})
