library(testthat)

build_meta <- .mztab_build_metadata_table
write_meta <- .mztab_write_metadata
write_section <- .mztab_write_section
mtd <- .mtd
opt_colname <- .mztab_opt_colname

test_that(".mtd builds tab-separated metadata line", {
  expect_equal(mtd("mzTab-version", "2.1.0-M"), "MTD\tmzTab-version\t2.1.0-M")
})

test_that(".mztab_opt_colname normalizes names", {
  expect_equal(opt_colname("Candidate Score!"), "opt_global_candidate_score")
  expect_equal(opt_colname("___"), "opt_global_extra")
})

test_that(".mztab_build_metadata_table includes mandatory keys", {
  sme <- tidytable::tidytable(`id_confidence_measure[1]` = "0.9")
  meta <- build_meta(
    mztab_id = "test_id",
    title = "Test",
    description = "Desc",
    software_version = "1.0.0",
    ms_run_location = "file://spectra.mzML",
    polarity = "positive",
    sample_name = "Sample A",
    publication = NULL,
    contact = NULL,
    xrefs_index = NULL,
    sme_table = sme,
    existing_metadata = NULL
  )
  expect_true(all(c("key", "value") %in% names(meta)))
  expect_true(any(meta$key == "mzTab-version"))
  expect_true(any(meta$key == "mzTab-ID"))
  expect_true(any(meta$key == "software[1]"))
  expect_true(any(meta$key == "ms_run[1]-location"))
})

test_that(".mztab_build_metadata_table respects existing metadata entries", {
  existing <- tidytable::tidytable(
    key = c("mzTab-version", "title"),
    value = c("custom", "Existing Title")
  )
  sme <- tidytable::tidytable(`id_confidence_measure[1]` = "0.9")
  meta <- build_meta(
    mztab_id = "id",
    title = "New Title",
    description = "Desc",
    software_version = "1.0.0",
    ms_run_location = "file://x",
    xrefs_index = NULL,
    sme_table = sme,
    existing_metadata = existing
  )
  expect_equal(meta$value[meta$key == "mzTab-version"][1], "custom")
  expect_equal(meta$value[meta$key == "title"][1], "Existing Title")
})

test_that(".mztab_write_metadata writes null for NA values", {
  meta <- tidytable::tidytable(
    key = c("a", "b"),
    value = c(NA_character_, "x")
  )
  out <- tempfile(fileext = ".mztab")
  on.exit(unlink(out), add = TRUE)
  con <- file(out, open = "wt")
  write_meta(con, meta)
  close(con)

  txt <- readLines(out, warn = FALSE)
  expect_true(any(grepl("MTD\ta\tnull$", txt)))
  expect_true(any(grepl("MTD\tb\tx$", txt)))
})

test_that(".mztab_write_section writes header and rows without feature_id", {
  tbl <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    database_identifier = c("CHEBI:1", NA),
    chemical_name = c("A", "")
  )
  out <- tempfile(fileext = ".mztab")
  on.exit(unlink(out), add = TRUE)
  con <- file(out, open = "wt")
  write_section(con, "SMH", "SML", tbl)
  close(con)

  txt <- readLines(out, warn = FALSE)
  expect_true(any(grepl("^SMH\tdatabase_identifier\tchemical_name$", txt)))
  expect_true(any(grepl("^SML\tCHEBI:1\tA$", txt)))
  expect_true(any(grepl("^SML\tnull\tnull$", txt)))
})

test_that(".mztab_write_section no-op on empty table", {
  tbl <- tidytable::tidytable(a = character(0))
  out <- tempfile(fileext = ".mztab")
  on.exit(unlink(out), add = TRUE)
  con <- file(out, open = "wt")
  expect_invisible(write_section(con, "SMH", "SML", tbl))
})
