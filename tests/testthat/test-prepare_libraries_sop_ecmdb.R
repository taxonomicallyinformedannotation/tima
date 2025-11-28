# Test Suite: prepare_libraries_sop_ecmdb ----

library(testthat)

## Internal Utility Helpers ----

.create_ecmdb_zip <- function(zip_path, inner_name = "ecmdb") {
  # Create temp file for inner content
  temp_inner <- file.path(dirname(zip_path), inner_name)
  lines <- c(
    '{"name":"MetA","moldb_inchikey":"AAAAAAAAAAAAAA-BBBBBBBBBB-C","moldb_smiles":"C","moldb_formula":"CH4","moldb_mono_mass":16.0313,"moldb_logp":0.1}',
    '{"name":"MetB","moldb_inchikey":"CCCCCCCCCCCCCC-DDDDDDDDDD-E","moldb_smiles":"CC","moldb_formula":"C2H6","moldb_mono_mass":30.07,"moldb_logp":0.2}'
  )
  writeLines(lines, temp_inner)

  # Create zip in the directory of zip_path
  withr::local_dir(new = dirname(zip_path))
  utils::zip(zipfile = basename(zip_path), files = inner_name)

  # Clean up temp file
  unlink(temp_inner)
}

## Validation ----

test_that("test-prepare_libraries_sop_ecmdb validates input and output types", {
  expect_error(
    prepare_libraries_sop_ecmdb(input = c("a", "b"), output = "x.tsv"),
    "single character"
  )
  expect_error(
    prepare_libraries_sop_ecmdb(input = "a.zip", output = c("x", "y")),
    "single character"
  )
})

## Behavior ----

test_that("test-prepare_libraries_sop_ecmdb handles missing input by creating empty library", {
  out <- temp_test_path("ecmdb.tsv")
  res <- prepare_libraries_sop_ecmdb(
    input = temp_test_path("missing.zip"),
    output = out
  )
  expect_equal(res, out)
  expect_true(file.exists(out))
  df <- tidytable::fread(out)
  expect_true(is.data.frame(df))
})

test_that("test-prepare_libraries_sop_ecmdb parses minimal valid zip", {
  inzip <- temp_test_path("ecmdb.zip")
  .create_ecmdb_zip(inzip, inner_name = "ecmdb")
  out <- temp_test_path("ecmdb.tsv")
  res <- prepare_libraries_sop_ecmdb(input = inzip, output = out)
  expect_equal(res, out)
  expect_true(file.exists(out))
  df <- tidytable::fread(out)
  expect_true("organism_name" %in% names(df))
  expect_true(all(df$organism_name == "Escherichia coli"))
})
