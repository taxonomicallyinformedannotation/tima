library(testthat)

sanitize_mgf <- sanitize_mgf
sanitize_csv <- sanitize_csv
sanitize_metadata <- sanitize_metadata
sanitize_sirius <- sanitize_sirius

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

test_that("sanitize_csv remaps feature_id requirement to custom feature column", {
  csv <- tempfile(pattern = "tima-csv-feature-col-", fileext = ".csv")
  write.csv(
    data.frame(my_feature = c("f1", "f2"), mz = c("100", "200")),
    csv,
    row.names = FALSE
  )
  on.exit(unlink(csv))

  result <- sanitize_csv(
    file = csv,
    required_cols = c("feature_id", "mz"),
    feature_col = "my_feature"
  )

  expect_true(result$valid)
  expect_equal(result$n_rows, 2L)
  expect_true("my_feature" %in% result$columns)
})

test_that("sanitize_csv reports read errors from safe_fread", {
  csv <- tempfile(pattern = "tima-csv-read-error-", fileext = ".csv")
  writeLines("x\n1", csv)
  on.exit(unlink(csv))

  testthat::with_mocked_bindings(
    safe_fread = function(...) {
      stop("synthetic read failure")
    },
    {
      result <- sanitize_csv(csv, file_type = "mock csv")
      expect_false(result$valid)
      expect_true(any(grepl("Failed to read file", result$issues)))
    },
    .package = "tima"
  )
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

test_that("sanitize_metadata flags missing filename column", {
  meta <- tempfile(pattern = "tima-meta-nofilename-", fileext = ".tsv")
  write.table(
    data.frame(organism = c("OrgA", "OrgB")),
    meta,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )
  on.exit(unlink(meta))

  result <- sanitize_metadata(meta)
  expect_false(result$valid)
  expect_true(any(grepl("Filename column not found", result$issues)))
})

test_that("sanitize_metadata flags organism column with only empty values", {
  meta <- tempfile(pattern = "tima-meta-empty-org-", fileext = ".tsv")
  write.table(
    data.frame(
      filename = c("s1.mzML", "s2.mzML"),
      organism = c("", NA)
    ),
    meta,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )
  on.exit(unlink(meta))

  result <- sanitize_metadata(meta)
  expect_false(result$valid)
  expect_equal(result$n_with_organism, 0L)
  expect_true(any(grepl("No organism information", result$issues)))
})

test_that("sanitize_metadata reports partial filename matches against features", {
  meta <- tempfile(pattern = "tima-meta-partial-", fileext = ".tsv")
  features <- tempfile(pattern = "tima-features-partial-", fileext = ".tsv")

  write.table(
    data.frame(
      filename = c("sample1.mzML", "sample2.mzML", "sample3.mzML"),
      organism = c("Org1", "Org2", "Org3")
    ),
    meta,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  write.table(
    data.frame(
      feature_id = c("f1", "f2"),
      sample1 = c("10", "20"),
      sample2_peak = c("30", "40")
    ),
    features,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  on.exit({
    unlink(meta)
    unlink(features)
  })

  result <- sanitize_metadata(meta, features_file = features)
  expect_true(result$filenames_match)
  expect_equal(result$n_matched, 2L)
  expect_equal(result$n_unmatched, 1L)
  expect_identical(result$unmatched_files, "sample3")
})

test_that("sanitize_metadata fails when no metadata filenames match features", {
  meta <- tempfile(pattern = "tima-meta-nomatch-", fileext = ".tsv")
  features <- tempfile(pattern = "tima-features-nomatch-", fileext = ".tsv")

  write.table(
    data.frame(
      filename = c("zzzz111.mzML", "yyyy222.mzML"),
      organism = c("OrgA", "OrgB")
    ),
    meta,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  write.table(
    data.frame(feature_id = c("f1", "f2"), other_col = c("x", "y")),
    features,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  on.exit({
    unlink(meta)
    unlink(features)
  })

  result <- sanitize_metadata(meta, features_file = features)
  expect_false(result$valid)
  expect_false(result$filenames_match)
  expect_true(any(grepl("No metadata filenames", result$issues)))
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

test_that("sanitize_sirius reports ZIP extraction failures", {
  fake_zip <- tempfile(pattern = "tima-bad-zip-", fileext = ".zip")
  writeLines("not a real zip", fake_zip)
  on.exit(unlink(fake_zip))

  result <- suppressWarnings(sanitize_sirius(fake_zip))
  expect_false(result$valid)
  expect_true(result$is_zip)
  expect_true(any(grepl(
    "Failed to extract ZIP|Missing formula|Missing CANOPUS|Missing structure",
    result$issues
  )))
})

test_that("sanitize_sirius counts structure annotations when present", {
  d <- tempfile(pattern = "tima-sirius-v6-count-")
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
    c("id\tInChIKey", "1\tAAAAA", "2\tBBBBB"),
    file.path(d, "structure_identifications_all.tsv")
  )

  result <- sanitize_sirius(d)
  expect_true(result$valid)
  expect_equal(result$n_features, 2L)
})

test_that("sanitize_sirius keeps validation status when structure count read fails", {
  d <- tempfile(pattern = "tima-sirius-v6-readfail-")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE))

  structure_file <- file.path(d, "structure_identifications_all.tsv")
  writeLines("id\tInChIKey\n1\tAAAAA", structure_file)
  writeLines(
    "id\tformula\n1\tC6H12O6",
    file.path(d, "formula_identifications_all.tsv")
  )
  writeLines(
    "id\tclass\n1\tFlavonoids",
    file.path(d, "canopus_formula_summary_all.tsv")
  )

  testthat::with_mocked_bindings(
    safe_fread = function(file, ...) {
      if (identical(file, structure_file)) {
        stop("forced structure read failure")
      }
      tidytable::tidytable(id = "1")
    },
    {
      result <- sanitize_sirius(d)
      expect_true(result$valid)
      expect_true(result$has_structure)
      expect_equal(result$n_features, 0L)
    },
    .package = "tima"
  )
})
