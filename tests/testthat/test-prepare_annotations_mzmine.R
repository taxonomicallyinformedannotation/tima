# Test Suite: prepare_annotations_mzmine ----

library(testthat)

## Validation ----

test_that("test-prepare_annotations_mzmine validates output path", {
  s <- stage_structure_fixtures("prep_mzmine_structures")
  withr::local_dir(new = as.character(s$root))

  expect_error(
    prepare_annotations_mzmine(
      input = character(0),
      output = c("a.tsv", "b.tsv"),
      str_stereo = s$stereo,
      str_met = s$met,
      str_tax_cla = s$cla,
      str_tax_npc = s$npc
    ),
    "output must be a single character string",
    class = "tima_validation_error"
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
      str_tax_cla = temp_test_path("missing.tsv"),
      str_tax_npc = temp_test_path("missing.tsv")
    ),
    "Required file(s) not found",
    fixed = TRUE
  )
})

## Behavior ----

test_that("test-prepare_annotations_mzmine handles missing input files by creating empty output", {
  s <- stage_structure_fixtures("prep_mzmine_structures")
  withr::local_dir(new = as.character(s$root))

  out <- temp_test_path("mzmine_empty.tsv")
  res <- prepare_annotations_mzmine(
    input = temp_test_path("does_not_exist.tsv"),
    output = out,
    str_stereo = s$stereo,
    str_met = s$met,
    str_tax_cla = s$cla,
    str_tax_npc = s$npc
  )
  expect_equal(res, out)
  expect_true(file.exists(out))
  df <- tidytable::fread(out)
  expect_true(is.data.frame(df))
})

test_that("test-prepare_annotations_mzmine processes minimal valid mzmine file", {
  s <- stage_structure_fixtures("prep_mzmine_structures")
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
  s <- stage_structure_fixtures("prep_mzmine_structures")
  withr::local_dir(new = as.character(s$root))
  out <- temp_test_path("mzmine_empty_vec.tsv")

  res <- prepare_annotations_mzmine(
    input = character(0),
    output = out,
    str_stereo = s$stereo,
    str_met = s$met,
    str_tax_cla = s$cla,
    str_tax_npc = s$npc
  )
  expect_equal(res, out)
  expect_true(file.exists(out))
  df <- tidytable::fread(out)
  expect_true(is.data.frame(df))
})

test_that("test-prepare_annotations_mzmine handles multiple input files", {
  s <- stage_structure_fixtures("prep_mzmine_structures")
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
    str_tax_cla = s$cla,
    str_tax_npc = s$npc
  )
  expect_equal(res, out)
  expect_true(file.exists(out))
  df <- tidytable::fread(out)
  expect_true(nrow(df) >= 2)
})
