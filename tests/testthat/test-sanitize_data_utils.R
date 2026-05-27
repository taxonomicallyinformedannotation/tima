library(testthat)
library(tima)

ns <- asNamespace("tima")
sanitize_mgf <- get("sanitize_mgf", ns)
sanitize_csv <- get("sanitize_csv", ns)
sanitize_metadata <- get("sanitize_metadata", ns)
sanitize_sirius <- get("sanitize_sirius", ns)

# ── sanitize_mgf ──────────────────────────────────────────────────────────────

test_that("sanitize_mgf returns invalid for missing file", {
  result <- sanitize_mgf(tempfile("nonexistent_"))
  expect_false(result$valid)
  expect_match(result$issues, "not found")
})

test_that("sanitize_mgf validates a minimal well-formed MGF", {
  mgf <- tempfile(pattern = "tima-mgf-", fileext = ".mgf")
  writeLines(
    c(
      "BEGIN IONS",
      "PEPMASS=200.123",
      "100.0 0.5",
      "150.0 1.0",
      "END IONS"
    ),
    mgf
  )
  on.exit(unlink(mgf))

  result <- sanitize_mgf(mgf)
  expect_true(result$valid)
  expect_equal(result$n_spectra, 1L)
  expect_length(result$issues, 0L)
})

test_that("sanitize_mgf detects empty MGF (no BEGIN IONS)", {
  mgf <- tempfile(pattern = "tima-mgf-empty-", fileext = ".mgf")
  writeLines(c("# comment only"), mgf)
  on.exit(unlink(mgf))

  result <- sanitize_mgf(mgf)
  expect_false(result$valid)
  expect_true(any(grepl("No spectra", result$issues)))
})

test_that("sanitize_mgf detects mismatched BEGIN/END IONS", {
  mgf <- tempfile(pattern = "tima-mgf-mismatch-", fileext = ".mgf")
  writeLines(
    c(
      "BEGIN IONS",
      "PEPMASS=200.123",
      "100.0 0.5"
      # Missing END IONS
    ),
    mgf
  )
  on.exit(unlink(mgf))

  result <- sanitize_mgf(mgf)
  expect_false(result$valid)
  expect_true(any(grepl(
    "Mismatched|mismatch|END IONS|No spectra",
    result$issues,
    ignore.case = TRUE
  )))
})

test_that("sanitize_mgf counts multiple MS2 spectra", {
  mgf <- tempfile(pattern = "tima-mgf-multi-", fileext = ".mgf")
  writeLines(
    c(
      "BEGIN IONS",
      "PEPMASS=100.0",
      "50.0 1.0",
      "END IONS",
      "BEGIN IONS",
      "PEPMASS=200.0",
      "100.0 1.0",
      "END IONS"
    ),
    mgf
  )
  on.exit(unlink(mgf))

  result <- sanitize_mgf(mgf)
  expect_true(result$valid)
  expect_equal(result$n_spectra, 2L)
})

test_that("sanitize_mgf detects missing PEPMASS in first MS2 spectrum", {
  mgf <- tempfile(pattern = "tima-mgf-nopepmass-", fileext = ".mgf")
  writeLines(
    c(
      "BEGIN IONS",
      "TITLE=NoMass",
      "100.0 0.5",
      "END IONS"
    ),
    mgf
  )
  on.exit(unlink(mgf))

  result <- sanitize_mgf(mgf)
  expect_true(any(grepl("PEPMASS", result$issues)))
})

# ── sanitize_csv ──────────────────────────────────────────────────────────────

test_that("sanitize_csv returns invalid for missing file", {
  result <- sanitize_csv(tempfile("nonexistent_"))
  expect_false(result$valid)
  expect_match(result$issues, "not found")
})

test_that("sanitize_csv validates a simple CSV file", {
  csv <- tempfile(pattern = "tima-csv-", fileext = ".csv")
  write.csv(
    data.frame(feature_id = 1:3, mz = c(100, 200, 300)),
    csv,
    row.names = FALSE
  )
  on.exit(unlink(csv))

  result <- sanitize_csv(csv, required_cols = c("feature_id", "mz"))
  expect_true(result$valid)
  expect_equal(result$n_rows, 3L)
  expect_true("feature_id" %in% result$columns)
})

test_that("sanitize_csv detects missing required columns", {
  csv <- tempfile(pattern = "tima-csv-nocol-", fileext = ".csv")
  write.csv(data.frame(x = 1:3), csv, row.names = FALSE)
  on.exit(unlink(csv))

  result <- sanitize_csv(csv, required_cols = c("feature_id", "mz"))
  expect_false(result$valid)
  expect_true(any(grepl("Missing required columns", result$issues)))
})

test_that("sanitize_csv detects empty CSV", {
  csv <- tempfile(pattern = "tima-csv-empty-", fileext = ".csv")
  write.csv(data.frame(feature_id = character(0)), csv, row.names = FALSE)
  on.exit(unlink(csv))

  result <- sanitize_csv(csv)
  expect_false(result$valid)
  expect_true(any(grepl("empty", result$issues, ignore.case = TRUE)))
})

# ── sanitize_metadata ──────────────────────────────────────────────────────────

test_that("sanitize_metadata returns invalid for missing file", {
  result <- sanitize_metadata(tempfile("nonexistent_"))
  expect_false(result$valid)
  expect_match(result$issues, "not found")
})

test_that("sanitize_metadata validates a minimal metadata file", {
  meta <- tempfile(pattern = "tima-meta-", fileext = ".tsv")
  write.table(
    data.frame(
      filename = c("sample1.mzML", "sample2.mzML"),
      organism = c("Homo sapiens", "Mus musculus")
    ),
    meta,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )
  on.exit(unlink(meta))

  result <- sanitize_metadata(meta)
  expect_true(result$valid)
  expect_equal(result$n_samples, 2L)
  expect_equal(result$n_with_organism, 2L)
})

test_that("sanitize_metadata detects missing organism column gracefully", {
  meta <- tempfile(pattern = "tima-meta-noorg-", fileext = ".tsv")
  write.table(
    data.frame(filename = c("s1.mzML", "s2.mzML")),
    meta,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )
  on.exit(unlink(meta))

  # No organism column => valid=TRUE, n_with_organism = 0, no fatal error
  result <- sanitize_metadata(meta)
  expect_equal(result$n_with_organism, 0L)
})

# ── sanitize_sirius ────────────────────────────────────────────────────────────

test_that("sanitize_sirius returns invalid for missing path", {
  result <- sanitize_sirius(tempfile("nonexistent_"))
  expect_false(result$valid)
  expect_match(result$issues, "not found")
})

test_that("sanitize_sirius returns invalid for non-directory non-ZIP file", {
  f <- tempfile(pattern = "tima-sirius-", fileext = ".txt")
  writeLines("hello", f)
  on.exit(unlink(f))

  result <- sanitize_sirius(f)
  expect_false(result$valid)
  expect_true(any(grepl(
    "not a directory|ZIP",
    result$issues,
    ignore.case = TRUE
  )))
})

test_that("sanitize_sirius detects missing required files in directory", {
  d <- tempfile(pattern = "tima-sirius-dir-")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE))

  result <- sanitize_sirius(d)
  expect_false(result$valid)
  expect_true(any(grepl(
    "formula|canopus|structure",
    result$issues,
    ignore.case = TRUE
  )))
})

test_that("sanitize_sirius validates a complete SIRIUS v5 directory", {
  d <- tempfile(pattern = "tima-sirius-v5-")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE))

  # Write minimal v5 files
  writeLines(
    "id\tformula\n1\tC6H12O6",
    file.path(d, "formula_identifications.tsv")
  )
  writeLines("id\tclass\n1\tFlavonoids", file.path(d, "canopus_summary.tsv"))
  writeLines(
    "id\tInChIKey\n1\tABCDE",
    file.path(d, "compound_identifications.tsv")
  )

  result <- sanitize_sirius(d)
  expect_true(result$valid)
  expect_true(result$has_formula)
  expect_true(result$has_canopus)
  expect_true(result$has_structure)
  expect_equal(result$sirius_version, "5")
  expect_false(result$is_zip)
})

test_that("sanitize_sirius detects SIRIUS v6 naming convention", {
  d <- tempfile(pattern = "tima-sirius-v6-")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE))

  writeLines(
    "id\tformula\n1\tC6H12O6",
    file.path(d, "formula_identifications_all.tsv")
  )
  writeLines(
    "id\tclass\n1\tFlavonoids",
    file.path(d, "canopus_formula_summary_all.tsv")
  )
  writeLines(
    "id\tInChIKey\n1\tABCDE",
    file.path(d, "structure_identifications_all.tsv")
  )

  result <- sanitize_sirius(d)
  expect_true(result$valid)
  expect_equal(result$sirius_version, "6")
})
