# Test Suite: prepare_libraries_sop_ecmdb ----

library(testthat)

## Internal Utility Helpers ----

.create_ecmdb_zip <- function(zip_path, inner_name = "ecmdb") {
  tmpdir <- tempfile(pattern = "ecmdbjson")
  dir.create(tmpdir)
  json_path <- file.path(tmpdir, inner_name)
  lines <- c(
    '{"name":"MetA","moldb_inchikey":"AAAAAAAAAAAAAA-BBBBBBBBBB-C","moldb_smiles":"C","moldb_formula":"CH4","moldb_mono_mass":16.0313,"moldb_logp":0.1}',
    '{"name":"MetB","moldb_inchikey":"CCCCCCCCCCCCCC-DDDDDDDDDD-E","moldb_smiles":"CC","moldb_formula":"C2H6","moldb_mono_mass":30.07,"moldb_logp":0.2}'
  )
  writeLines(lines, json_path)
  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(tmpdir)
  utils::zip(zipfile = zip_path, files = inner_name)
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
  tmp <- withr::local_tempdir(.local_envir = parent.frame())
  withr::local_dir(tmp, .local_envir = parent.frame())
  out <- file.path(tmp, "ecmdb.tsv")
  res <- prepare_libraries_sop_ecmdb(
    input = file.path(tmp, "missing.zip"),
    output = out
  )
  expect_equal(res, out)
  expect_true(file.exists(out))
  df <- tidytable::fread(out)
  expect_true(is.data.frame(df))
})

test_that("test-prepare_libraries_sop_ecmdb parses minimal valid zip", {
  tmp <- withr::local_tempdir(.local_envir = parent.frame())
  withr::local_dir(tmp, .local_envir = parent.frame())
  inzip <- file.path(tmp, "ecmdb.zip")
  .create_ecmdb_zip(inzip, inner_name = "ecmdb")
  out <- file.path(tmp, "ecmdb.tsv")
  res <- prepare_libraries_sop_ecmdb(input = inzip, output = out)
  expect_equal(res, out)
  expect_true(file.exists(out))
  df <- tidytable::fread(out)
  expect_true("organism_name" %in% names(df))
  expect_true(all(df$organism_name == "Escherichia coli"))
})
