# Test Suite: graceful handling of MS1-only / empty MGF inputs ----
#
# Regression guard for the crash where a collaborator-submitted MGF
# containing only MS1 (precursor) spectra — no MS2 fragment peaks —
# caused TIMA to hard-abort during the pre-flight sanitization step.
# These inputs now degrade gracefully: one clear, actionable warning
# and a valid (empty) template output, with no error or stack trace.

library(testthat)

# Helper: write a valid MS2 library MGF used as the matching target.
.write_lib_mgf <- function(path, precursors) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  lines <- unlist(
    lapply(seq_along(precursors), function(i) {
      mz <- precursors[[i]]
      c(
        "BEGIN IONS",
        paste0("TITLE=Lib_", i),
        paste0("PEPMASS=", mz),
        "CHARGE=1+",
        "MSLEVEL=2",
        "SMILES=CCO",
        paste0(round(mz * 0.5), " 50"),
        paste0(round(mz * 0.75), " 200"),
        paste0(round(mz), " 300"),
        "END IONS",
        ""
      )
    }),
    use.names = FALSE
  )
  writeLines(lines, path)
  invisible(path)
}

# Helper: write an MS1-only MGF (MSLEVEL=1, peak lines present but MS1)
.write_ms1_only_mgf <- function(path, n = 2L) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  lines <- unlist(
    lapply(seq_len(n), function(i) {
      mz <- 100 + i * 50
      c(
        "BEGIN IONS",
        paste0("TITLE=id:", i, ", mz:", mz),
        paste0("PEPMASS=", mz),
        "CHARGE=1+",
        "MSLEVEL=1",
        paste0(round(mz * 0.5), " 100"),
        paste0(round(mz), " 500"),
        "END IONS",
        ""
      )
    }),
    use.names = FALSE
  )
  writeLines(lines, path)
  invisible(path)
}

# Helper: write a mixed MGF (one MS1 + one MS2 sharing a precursor with lib)
.write_mixed_mgf <- function(path, ms1_mz, ms2_mz) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  lines <- c(
    "BEGIN IONS",
    paste0("TITLE=id:1, mz:", ms1_mz),
    paste0("PEPMASS=", ms1_mz),
    "CHARGE=1+",
    "MSLEVEL=1",
    paste0(round(ms1_mz * 0.5), " 100"),
    "END IONS",
    "",
    "BEGIN IONS",
    paste0("TITLE=id:2, mz:", ms2_mz),
    paste0("PEPMASS=", ms2_mz),
    "CHARGE=1+",
    "MSLEVEL=2",
    paste0(round(ms2_mz * 0.5), " 50"),
    paste0(round(ms2_mz * 0.75), " 200"),
    paste0(round(ms2_mz), " 300"),
    "END IONS",
    ""
  )
  writeLines(lines, path)
  invisible(path)
}

# Helper: a tiny well-formed MS2 query MGF for regression checks.
# Uses the same peak pattern as the library so identical spectra yield
# a similarity of 1 (after precursor-peak filtering).
.write_ms2_mgf <- function(path, precursors) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  lines <- unlist(
    lapply(seq_along(precursors), function(i) {
      mz <- precursors[[i]]
      c(
        "BEGIN IONS",
        paste0("TITLE=id:", i),
        paste0("PEPMASS=", mz),
        "CHARGE=1+",
        "MSLEVEL=2",
        paste0(round(mz * 0.5), " 50"),
        paste0(round(mz * 0.75), " 200"),
        paste0(round(mz), " 300"),
        "END IONS",
        ""
      )
    }),
    use.names = FALSE
  )
  writeLines(lines, path)
  invisible(path)
}

# ── MS1-only MGF ──────────────────────────────────────────────────────────────

test_that("MS1-only MGF emits a single warning and empty template", {
  withr::local_dir(new = temp_test_dir("ms1_only"))
  local_test_project(copy = TRUE)

  lib_path <- file.path(
    "data",
    "interim",
    "libraries",
    "spectra",
    "lib_pos.mgf"
  )
  .write_lib_mgf(lib_path, precursors = c(200))

  query_path <- file.path(
    "data",
    "interim",
    "spectra",
    "query_ms1_only.mgf"
  )
  dir.create(dirname(query_path), recursive = TRUE, showWarnings = FALSE)
  .write_ms1_only_mgf(query_path, n = 2L)

  out_path <- file.path(temp_test_dir("ms1_only"), "ms1_only_out.tsv")

  warned <- character(0L)
  withr::local_options(testthat.default.reporter = NULL)
  expect_warning(
    {
      out <- annotate_spectra(
        input = query_path,
        libraries = list(pos = lib_path),
        polarity = "pos",
        output = out_path
      )
    },
    class = "tima_spectra_no_ms2_warning"
  )

  # Exactly one warning, matching the actionable message requirements.
  warned <- testthat::capture_warnings(
    annotate_spectra(
      input = query_path,
      libraries = list(pos = lib_path),
      polarity = "pos",
      output = out_path
    )
  )
  expect_length(warned, 1L)
  msg <- warned[[1L]]
  expect_match(msg, "No MS2 spectra were found")
  expect_match(msg, "MS1-only data cannot be annotated")
  expect_match(msg, "annotate_masses|MS2|fragment")

  # Output file exists and has the expected (empty template) schema.
  expect_true(file.exists(out_path))
  df <- tidytable::fread(out_path)
  expect_true(all(names(fake_annotations_columns()) %in% names(df)))
  expect_equal(nrow(df), 1L)
})

# ── Empty MGF ─────────────────────────────────────────────────────────────────

test_that("empty MGF emits a clear warning and empty template", {
  withr::local_dir(new = temp_test_dir("empty_mgf"))
  local_test_project(copy = TRUE)

  lib_path <- file.path(
    "data",
    "interim",
    "libraries",
    "spectra",
    "lib_pos.mgf"
  )
  .write_lib_mgf(lib_path, precursors = c(200))

  query_path <- file.path(
    "data",
    "interim",
    "spectra",
    "query_empty.mgf"
  )
  dir.create(dirname(query_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(c("# empty file", ""), query_path)

  out_path <- file.path(temp_test_dir("empty_mgf"), "empty_out.tsv")

  expect_warning(
    {
      out <- annotate_spectra(
        input = query_path,
        libraries = list(pos = lib_path),
        polarity = "pos",
        output = out_path
      )
    },
    "No MS2 spectra were found|no spectra at all|MS1-only"
  )

  expect_true(file.exists(out_path))
  df <- tidytable::fread(out_path)
  expect_true(all(names(fake_annotations_columns()) %in% names(df)))
  expect_equal(nrow(df), 1L)
})

# ── Mixed MGF (MS1 + MS2) ────────────────────────────────────────────────────

test_that("mixed MS1/MS2 MGF warns once with skip count and annotates MS2", {
  withr::local_dir(new = temp_test_dir("mixed_mgf"))
  local_test_project(copy = TRUE)

  lib_path <- file.path(
    "data",
    "interim",
    "libraries",
    "spectra",
    "lib_pos.mgf"
  )
  # Library precursor matches the MS2 feature in the query
  .write_lib_mgf(lib_path, precursors = c(200))

  query_path <- file.path(
    "data",
    "interim",
    "spectra",
    "query_mixed.mgf"
  )
  dir.create(dirname(query_path), recursive = TRUE, showWarnings = FALSE)
  # One MS1 feature (skipped) + one MS2 feature (annotated)
  .write_mixed_mgf(query_path, ms1_mz = 293.1, ms2_mz = 200)

  out_path <- file.path(temp_test_dir("mixed_mgf"), "mixed_out.tsv")

  warned <- character(0L)
  expect_warning(
    {
      out <- annotate_spectra(
        input = query_path,
        libraries = list(pos = lib_path),
        polarity = "pos",
        output = out_path,
        threshold = 0
      )
    },
    "MS1 .* cannot be annotated"
  )
  warned <- testthat::capture_warnings(
    annotate_spectra(
      input = query_path,
      libraries = list(pos = lib_path),
      polarity = "pos",
      output = out_path,
      threshold = 0
    )
  )
  # Exactly one warning (the MS1-skip warning), with the skipped count.
  expect_length(warned, 1L)
  expect_match(warned[[1L]], "Skipped 1 MS1")

  # The MS2 feature was still annotated.
  expect_true(file.exists(out_path))
  df <- tidytable::fread(out_path)
  actual <- df[!is.na(df$candidate_score_similarity), , drop = FALSE]
  expect_gte(nrow(actual), 1L)
  expect_true("feature_id" %in% names(actual))
})

# ── Regression: valid MS2 input is unchanged ─────────────────────────────────

test_that("valid MS2 input still annotates with no MS1 warnings", {
  withr::local_dir(new = temp_test_dir("ms2_regression"))
  local_test_project(copy = TRUE)

  lib_path <- file.path(
    "data",
    "interim",
    "libraries",
    "spectra",
    "lib_pos.mgf"
  )
  .write_lib_mgf(lib_path, precursors = c(200))

  query_path <- file.path(
    "data",
    "interim",
    "spectra",
    "query_ms2.mgf"
  )
  dir.create(dirname(query_path), recursive = TRUE, showWarnings = FALSE)
  .write_ms2_mgf(query_path, precursors = c(200))

  out_path <- file.path(temp_test_dir("ms2_regression"), "reg_out.tsv")

  warned <- testthat::capture_warnings(
    annotate_spectra(
      input = query_path,
      libraries = list(pos = lib_path),
      polarity = "pos",
      output = out_path,
      threshold = 0
    )
  )
  expect_length(warned, 0L)

  df <- tidytable::fread(out_path)
  actual <- df[!is.na(df$candidate_score_similarity), , drop = FALSE]
  expect_gte(nrow(actual), 1L)
  expect_gt(actual$candidate_score_similarity[[1L]], 0)
})

# ── Malformed MGF still aborts fatally ──────────────────────────────────────

test_that("malformed MGF (mismatched BEGIN/END IONS) still errors", {
  withr::local_dir(new = temp_test_dir("malformed_mgf"))
  local_test_project(copy = TRUE)

  lib_path <- file.path(
    "data",
    "interim",
    "libraries",
    "spectra",
    "lib_pos.mgf"
  )
  .write_lib_mgf(lib_path, precursors = c(200))

  bad_path <- file.path("data", "interim", "spectra", "bad.mgf")
  dir.create(dirname(bad_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(
    c("BEGIN IONS", "PEPMASS=200.0", "50.0 50"), # missing END IONS
    bad_path
  )

  expect_error(
    annotate_spectra(
      input = bad_path,
      libraries = list(pos = lib_path),
      polarity = "pos",
      output = file.path(temp_test_dir("malformed_mgf"), "bad_out.tsv")
    ),
    class = "tima_validation_error"
  )
})

# ── get_spectra_ids on empty Spectra does not crash ──────────────────────────

test_that("get_spectra_ids is safe on empty Spectra", {
  empty_df <- methods::as(object = data.frame(), Class = "DataFrame")
  sp <- Spectra::Spectra(empty_df)
  expect_equal(length(sp), 0L)
  ids <- get_spectra_ids(sp)
  expect_equal(ids, character(0L))
})

# ── import_spectra attribute & scoping ───────────────────────────────────────

test_that("import_spectra records tima_ms1_skipped attribute for mixed input", {
  query_path <- withr::local_tempfile(fileext = ".mgf")
  .write_mixed_mgf(query_path, ms1_mz = 293.1, ms2_mz = 200)
  sp <- import_spectra(file = query_path, sanitize = FALSE, combine = FALSE)
  expect_s4_class(sp, "Spectra")
  expect_equal(attr(sp, "tima_ms1_skipped"), 1L)
  expect_gte(length(sp), 1L)
})

test_that("import_spectra does not warn on MS1 filtering (scoping)", {
  query_path <- withr::local_tempfile(fileext = ".mgf")
  .write_ms1_only_mgf(query_path, n = 3L)
  state <- new.env(parent = emptyenv())
  state$warned <- character(0L)
  sp <- withCallingHandlers(
    import_spectra(file = query_path, sanitize = FALSE, combine = FALSE),
    warning = function(w) {
      state$warned <- c(state$warned, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(state$warned, 0L)
  expect_equal(attr(sp, "tima_ms1_skipped"), 3L)
})

test_that("import_spectra sets tima_ms1_skipped to 0 for pure MS2", {
  query_path <- withr::local_tempfile(fileext = ".mgf")
  .write_ms2_mgf(query_path, precursors = c(200, 300))
  sp <- import_spectra(file = query_path, sanitize = FALSE, combine = FALSE)
  expect_equal(attr(sp, "tima_ms1_skipped"), 0L)
})
