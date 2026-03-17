# Test Suite: prepare_libraries_sop_hmdb ----
# These tests focus on behavior — caching, parsing, organism taxonomy defaults,
# and error recovery. Input-type validation is already covered by
# test-validations_utils.R and test-parse_hmdb_like_sdf_lines.R.

library(testthat)

# ---- helpers -----------------------------------------------------------------

#' Write a minimal single-compound HMDB-style SDF as a zip file.
write_hmdb_zip <- function(dir, compounds = 1L, include_inchikey = TRUE) {
  blocks <- lapply(seq_len(compounds), function(i) {
    ink <- if (include_inchikey) {
      paste0("AAAAAAA", sprintf("%07d", i), "-UHFFFAOYSA-N")
    } else {
      NULL
    }
    c(
      paste0("HMDB", sprintf("%07d", i)),
      "  Mrv2211",
      "",
      "  0  0  0  0  0  0  0  0  0  0999 V2000",
      "M  END",
      paste0("> <DATABASE_ID>"),
      paste0("HMDB", sprintf("%07d", i)),
      "",
      "> <GENERIC_NAME>",
      paste0("Compound", i),
      "",
      "> <SMILES>",
      "CC",
      "",
      if (!is.null(ink)) c("> <INCHI_KEY>", ink, "") else NULL,
      "> <FORMULA>",
      "C2H6",
      "",
      "> <EXACT_MASS>",
      "30.047",
      "",
      "> <JCHEM_LOGP>",
      "1.5",
      "",
      "$$$$"
    )
  })
  sdf_path <- file.path(dir, "structures.sdf")
  writeLines(unlist(blocks), sdf_path)
  zip_path <- file.path(dir, "structures.zip")
  withr::with_dir(dir, utils::zip(basename(zip_path), basename(sdf_path)))
  zip_path
}

# ---- caching behaviour -------------------------------------------------------

test_that("prepare_libraries_sop_hmdb returns path to existing large output without re-parsing", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))
  out <- file.path(td, "hmdb.tsv")

  # Create a file that is truly above the cache-size threshold used by the function.
  big <- data.frame(
    structure_inchikey = sprintf("IK%06d-UHFFFAOYSA-N", seq_len(12000L)),
    organism_name = rep("Homo sapiens", 12000L),
    stringsAsFactors = FALSE
  )
  tidytable::fwrite(x = big, file = out, sep = "\t")

  size_before <- file.size(out)
  expect_gt(size_before, 100000)

  prepare_libraries_sop_hmdb(
    input = file.path(td, "nonexistent.zip"),
    output = out
  )

  expect_equal(file.size(out), size_before)
})

test_that("prepare_libraries_sop_hmdb creates empty output when input is missing", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))
  out <- file.path(td, "hmdb.tsv")
  prepare_libraries_sop_hmdb(input = file.path(td, "ghost.zip"), output = out)
  expect_true(file.exists(out))
  df <- tidytable::fread(out)
  expect_true("structure_inchikey" %in% names(df))
})

# ---- parsing -----------------------------------------------------------------

test_that("prepare_libraries_sop_hmdb parses a single-compound SDF zip", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))
  zip <- write_hmdb_zip(td, compounds = 1L)
  out <- file.path(td, "hmdb.tsv")
  prepare_libraries_sop_hmdb(input = zip, output = out)
  df <- tidytable::fread(out)
  expect_equal(nrow(df), 1L)
  expect_equal(df$organism_name, "Homo sapiens")
  expect_equal(df$structure_molecular_formula, "C2H6")
})

test_that("prepare_libraries_sop_hmdb parses multiple compounds", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))
  zip <- write_hmdb_zip(td, compounds = 5L)
  out <- file.path(td, "hmdb.tsv")
  prepare_libraries_sop_hmdb(input = zip, output = out)
  df <- tidytable::fread(out)
  expect_equal(nrow(df), 5L)
})

test_that("prepare_libraries_sop_hmdb drops compounds without InChIKey", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))
  zip <- write_hmdb_zip(td, compounds = 3L, include_inchikey = FALSE)
  out <- file.path(td, "hmdb.tsv")
  prepare_libraries_sop_hmdb(input = zip, output = out)
  df <- tidytable::fread(out)
  expect_equal(nrow(df), 0L)
})

# ---- default organism taxonomy -----------------------------------------------

test_that("prepare_libraries_sop_hmdb attaches full human taxonomy by default", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))
  zip <- write_hmdb_zip(td, compounds = 1L)
  out <- file.path(td, "hmdb.tsv")
  prepare_libraries_sop_hmdb(input = zip, output = out)
  df <- tidytable::fread(out)
  if (nrow(df) == 0L) {
    skip("No valid entries parsed")
  }
  expect_equal(df$organism_name[1], "Homo sapiens")
  expect_equal(as.character(df$organism_taxonomy_ottid[1]), "770315")
  expect_equal(df$organism_taxonomy_04class[1], "Mammalia")
  expect_equal(df$organism_taxonomy_08genus[1], "Homo")
})

# ---- input validation --------------------------------------------------------

test_that("prepare_libraries_sop_hmdb rejects non-character input", {
  expect_error(
    prepare_libraries_sop_hmdb(input = 123, output = "x.tsv"),
    "single character",
    class = "tima_validation_error"
  )
})

test_that("prepare_libraries_sop_hmdb rejects vector output", {
  expect_error(
    prepare_libraries_sop_hmdb(input = "x.zip", output = c("a.tsv", "b.tsv")),
    "single character",
    class = "tima_validation_error"
  )
})

test_that("parse_hmdb_like_input rejects unsupported input extensions", {
  expect_error(
    parse_hmdb_like_input("input.txt"),
    "unsupported input format",
    class = "tima_validation_error"
  )
})

test_that("parse_hmdb_like_input rejects zip archives without sdf entries", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

  txt_file <- file.path(td, "readme.txt")
  writeLines("not an sdf", txt_file)
  zip_file <- file.path(td, "archive.zip")
  withr::with_dir(td, utils::zip(basename(zip_file), basename(txt_file)))

  expect_error(
    parse_hmdb_like_input(zip_file),
    "no .sdf file found after unzip",
    class = "tima_validation_error"
  )
})
