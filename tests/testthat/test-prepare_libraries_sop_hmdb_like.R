# Test Suite: prepare_libraries_sop_hmdb_like ----
# Tests for the HMDB-like SOP library preparation helper

library(testthat)

# ── Input validation ──────────────────────────────────────────────────────────

test_that("prepare_libraries_sop_hmdb_like errors on non-character input", {
  expect_error(
    prepare_libraries_sop_hmdb_like(
      input = 1L,
      output = tempfile(fileext = ".tsv"),
      source_name = "test"
    ),
    class = "tima_error"
  )
})

test_that("prepare_libraries_sop_hmdb_like errors on non-character output", {
  expect_error(
    prepare_libraries_sop_hmdb_like(
      input = "/some/input.sdf.zip",
      output = 1L,
      source_name = "test"
    ),
    class = "tima_error"
  )
})

test_that("prepare_libraries_sop_hmdb_like errors on non-character source_name", {
  expect_error(
    prepare_libraries_sop_hmdb_like(
      input = "/some/input.sdf.zip",
      output = tempfile(fileext = ".tsv"),
      source_name = 99
    ),
    class = "tima_error"
  )
})

test_that("prepare_libraries_sop_hmdb_like errors on invalid tag type", {
  expect_error(
    prepare_libraries_sop_hmdb_like(
      input = "/some/input.sdf.zip",
      output = tempfile(fileext = ".tsv"),
      source_name = "test",
      tag = 123L
    ),
    class = "tima_error"
  )
})

test_that("prepare_libraries_sop_hmdb_like accepts NA as tag", {
  # NA tag gets converted to NA_character_ silently - test input validation only
  expect_error(
    prepare_libraries_sop_hmdb_like(
      input = "/nonexistent/input.sdf.zip",
      output = tempfile(fileext = ".tsv"),
      source_name = "test",
      tag = NA
    )
  )
})

test_that("prepare_libraries_sop_hmdb_like errors on multi-element tag", {
  expect_error(
    prepare_libraries_sop_hmdb_like(
      input = "/some/input.sdf.zip",
      output = tempfile(fileext = ".tsv"),
      source_name = "test",
      tag = c("a", "b")
    ),
    class = "tima_error"
  )
})

test_that("parse_hmdb_like_sdf_lines parses repeated headers and keeps first field per record", {
  sdf_lines <- c(
    "Record1",
    "> <DATABASE_ID>",
    "HMDB0000001",
    "> <HMDB_ID>",
    "HMDB_SECONDARY",
    "> <SMILES>",
    "CCO",
    "> <INCHI_KEY>",
    "LFQSCWFLJHTTHZ-UHFFFAOYSA-N",
    "> <FORMULA>",
    "C2H6O",
    "> <EXACT_MASS>",
    "46.04186481",
    "> <JCHEM_LOGP>",
    "-0.31",
    "> <GENERIC_NAME>",
    "Ethanol",
    "> <UNKNOWN_TAG>",
    "ignored",
    "$$$$",
    "Record2",
    "> <DATABASE_ID>",
    "HMDB0000002",
    "> <SMILES>",
    "",
    "$$$$"
  )

  parsed <- parse_hmdb_like_sdf_lines(sdf_lines)
  expect_equal(nrow(parsed), 2L)
  expect_equal(parsed$id, c("HMDB0000001", "HMDB0000002"))
  expect_equal(parsed$smiles, c("CCO", NA_character_))
  expect_equal(parsed$name, c("Ethanol", NA_character_))

  tmp_sdf <- tempfile(fileext = ".sdf")
  tmp_out <- tempfile(fileext = ".tsv")
  on.exit(unlink(c(tmp_sdf, tmp_out)), add = TRUE)
  writeLines(sdf_lines, tmp_sdf)

  res_path <- prepare_libraries_sop_hmdb_like(
    input = tmp_sdf,
    output = tmp_out,
    source_name = "HMDB_test",
    tag = "endogenous"
  )
  expect_equal(res_path, tmp_out)
  out_dt <- tidytable::fread(tmp_out)
  expect_equal(nrow(out_dt), 1L)
  expect_equal(out_dt$structure_smiles, "CCO")
  expect_equal(out_dt$structure_exact_mass, 46.04186, tolerance = 1e-5)
  expect_equal(out_dt$structure_tag, "endogenous")
})
