# Test Suite: create_edges_spectra ----

library(testthat)

test_that("create_edges_spectra validates input parameter", {
  expect_error(
    create_edges_spectra(input = NULL),
    "must be"
  )

  expect_error(
    create_edges_spectra(input = 123),
    "must be"
  )
})

test_that("create_edges_spectra validates threshold parameter", {
  mgf_file <- tempfile(fileext = ".mgf")
  mgf <- c("BEGIN IONS", "PEPMASS=100", "CHARGE=1+", "100 10", "END IONS")
  writeLines(mgf, mgf_file)

  expect_error(
    create_edges_spectra(input = mgf_file, threshold = -0.1),
    "threshold"
  )

  expect_error(
    create_edges_spectra(input = mgf_file, threshold = 1.5),
    "threshold"
  )
})

test_that("create_edges_spectra validates matched_peaks parameter", {
  mgf_file <- tempfile(fileext = ".mgf")
  mgf <- c("BEGIN IONS", "PEPMASS=100", "CHARGE=1+", "100 10", "END IONS")
  writeLines(mgf, mgf_file)

  expect_error(
    create_edges_spectra(input = mgf_file, matched_peaks = 0),
    "matched_peaks"
  )

  expect_error(
    create_edges_spectra(input = mgf_file, matched_peaks = -5),
    "matched_peaks"
  )
})

test_that("create_edges_spectra validates ppm parameter", {
  mgf_file <- tempfile(fileext = ".mgf")
  mgf <- c("BEGIN IONS", "PEPMASS=100", "CHARGE=1+", "100 10", "END IONS")
  writeLines(mgf, mgf_file)

  expect_error(
    create_edges_spectra(input = mgf_file, ppm = -10),
    "ppm"
  )
})

test_that("create_edges_spectra validates dalton parameter", {
  mgf_file <- tempfile(fileext = ".mgf")
  mgf <- c("BEGIN IONS", "PEPMASS=100", "CHARGE=1+", "100 10", "END IONS")
  writeLines(mgf, mgf_file)

  expect_error(
    create_edges_spectra(input = mgf_file, dalton = -0.01),
    "dalton"
  )
})

test_that("create_edges_spectra validates method parameter", {
  mgf_file <- tempfile(fileext = ".mgf")
  writeLines(
    c("BEGIN IONS", "PEPMASS=100", "CHARGE=1+", "100 10", "END IONS"),
    mgf_file
  )

  expect_error(
    create_edges_spectra(input = mgf_file, method = "unknown_method"),
    "method"
  )
})

test_that("create_edges_spectra validates cutoff parameter", {
  mgf_file <- tempfile(fileext = ".mgf")
  writeLines(
    c("BEGIN IONS", "PEPMASS=100", "CHARGE=1+", "100 10", "END IONS"),
    mgf_file
  )

  expect_error(
    create_edges_spectra(input = mgf_file, cutoff = -0.5),
    "cutoff"
  )
})

test_that("create_edges_spectra validates output parameter", {
  mgf_file <- tempfile(fileext = ".mgf")
  writeLines(
    c("BEGIN IONS", "PEPMASS=100", "CHARGE=1+", "100 10", "END IONS"),
    mgf_file
  )

  expect_error(
    create_edges_spectra(input = mgf_file, output = 123),
    "output must be a single character string",
    class = "tima_validation_error"
  )
})

test_that("create_edges_spectra validates list input file entries", {
  expect_error(
    create_edges_spectra(input = list(1, 2)),
    "all input elements must be character strings",
    class = "tima_validation_error"
  )

  expect_error(
    create_edges_spectra(input = list("missing_a.mgf", "missing_b.mgf")),
    "input file\\(s\\) not found",
    class = "tima_validation_error"
  )
})

test_that("create_edges_spectra checks input file exists", {
  expect_error(
    create_edges_spectra(input = "nonexistent.mgf"),
    "not found|does not exist"
  )
})

test_that("create_edges_spectra handles single spectrum", {
  withr::local_dir(new = temp_test_dir("create_edges_spectra_single"))
  mgf_file <- tempfile(fileext = ".mgf")
  mgf <- c(
    "BEGIN IONS",
    "TITLE=Spectrum1",
    "PEPMASS=100.5",
    "CHARGE=1+",
    "100 10",
    "101 20",
    "END IONS"
  )
  writeLines(mgf, mgf_file)

  output <- create_edges_spectra(
    input = mgf_file,
    threshold = 0.0,
    matched_peaks = 1,
    ppm = 10,
    dalton = 0.01
  )

  expect_true(file.exists(output))

  df <- tidytable::fread(output)
  # Single spectrum should produce empty edges or single NA row
  expect_true(nrow(df) <= 1)
})

test_that("create_edges_spectra creates output file", {
  withr::local_dir(new = temp_test_dir("create_edges_spectra_output"))
  mgf_file <- tempfile(fileext = ".mgf")
  mgf <- c(
    "BEGIN IONS",
    "TITLE=Spectrum1",
    "FEATURE_ID=1",
    "PEPMASS=100",
    "CHARGE=1+",
    "50 10",
    "75 20",
    "100 30",
    "END IONS",
    "",
    "BEGIN IONS",
    "TITLE=Spectrum2",
    "FEATURE_ID=2",
    "PEPMASS=200",
    "CHARGE=1+",
    "50 15",
    "75 25",
    "200 35",
    "END IONS"
  )
  writeLines(mgf, mgf_file)

  output <- create_edges_spectra(
    input = mgf_file,
    threshold = 0.0,
    matched_peaks = 1
  )

  expect_true(file.exists(output))
  expect_match(output, "\\.tsv$")
})

test_that("create_edges_spectra() creates edges from two spectra with entropy", {
  skip_if_not_installed("Spectra")
  withr::local_dir(new = temp_test_dir("create_edges_spectra_entropy"))
  mgf_file <- tempfile(fileext = ".mgf")
  writeLines(
    c(
      "BEGIN IONS",
      "TITLE=Spectrum1",
      "FEATURE_ID=1",
      "PEPMASS=100",
      "CHARGE=1+",
      "50 10",
      "75 20",
      "100 30",
      "END IONS",
      "",
      "BEGIN IONS",
      "TITLE=Spectrum2",
      "FEATURE_ID=2",
      "PEPMASS=200",
      "CHARGE=1+",
      "50 15",
      "75 25",
      "200 35",
      "END IONS"
    ),
    mgf_file
  )
  out <- create_edges_spectra(
    input = mgf_file,
    method = "entropy",
    threshold = 0.0,
    matched_peaks = 1
  )
  expect_true(file.exists(out))
  df <- tidytable::fread(out)
  expect_true(
    nrow(df) <= 1 || all(c("CLUSTERID1", "CLUSTERID2") %in% names(df))
  )
})

test_that("create_edges_spectra() runs with cosine method", {
  skip_if_not_installed("Spectra")
  withr::local_dir(new = temp_test_dir("create_edges_spectra_cosine"))
  mgf_file <- tempfile(fileext = ".mgf")
  writeLines(
    c(
      "BEGIN IONS",
      "TITLE=Spectrum1",
      "FEATURE_ID=1",
      "PEPMASS=100",
      "CHARGE=1+",
      "50 10",
      "75 20",
      "100 30",
      "END IONS",
      "",
      "BEGIN IONS",
      "TITLE=Spectrum2",
      "FEATURE_ID=2",
      "PEPMASS=200",
      "CHARGE=1+",
      "50 15",
      "75 25",
      "200 35",
      "END IONS"
    ),
    mgf_file
  )
  out <- create_edges_spectra(
    input = mgf_file,
    method = "cosine",
    threshold = 0.0,
    matched_peaks = 1
  )
  expect_true(file.exists(out))
})

test_that("create_edges_spectra handles empty MGF file", {
  withr::local_dir(new = temp_test_dir("create_edges_empty"))
  mgf_file <- tempfile(fileext = ".mgf")
  writeLines("", mgf_file)

  # Should handle gracefully
  expect_error(
    create_edges_spectra(input = mgf_file),
    "Spectra': 'data' must be of a vector type, was 'NULL'"
  )
})

test_that("create_edges_spectra handles invalid MGF format", {
  withr::local_dir(new = temp_test_dir("create_edges_invalid"))
  mgf_file <- tempfile(fileext = ".mgf")
  writeLines("INVALID MGF CONTENT", mgf_file)

  expect_error(
    create_edges_spectra(input = mgf_file),
    "Spectra': 'data' must be of a vector type, was 'NULL'"
  )
})

test_that("create_edges_spectra completes downstream edge-building with mocked spectra", {
  input_file <- tempfile(fileext = ".mgf")
  writeLines("BEGIN IONS\nEND IONS", input_file)
  query_mats <- list(
    matrix(c(50, 10, 75, 20), ncol = 2, byrow = TRUE),
    matrix(c(50, 15, 80, 30), ncol = 2, byrow = TRUE)
  )
  mock_spectra <- Spectra::Spectra(
    object = data.frame(
      msLevel = c(2L, 2L),
      precursorMz = c(100, 110)
    )
  )
  mock_spectra@backend@peaksData <- query_mats
  exported <- new.env(parent = emptyenv())
  exported$data <- NULL
  exported$file <- NULL

  local_mocked_bindings(
    get_params = function(step) list(),
    import_spectra = function(input, cutoff, dalton, ppm) mock_spectra,
    create_edges = function(
      frags,
      nspecs,
      precs,
      method,
      ms2_tolerance,
      ppm_tolerance,
      threshold,
      matched_peaks
    ) {
      tidytable::tidytable(
        feature_id = 1L,
        target_id = 2L,
        score = 0.91,
        matched_peaks = 2L
      )
    },
    get_spectra_ids = function(x) c("F1", "F2"),
    export_params = function(parameters, step) invisible(NULL),
    export_output = function(x, file) {
      exported$data <- x
      exported$file <- file
      invisible(file)
    },
    log_complete = function(ctx, ...) invisible(NULL)
  )

  out <- create_edges_spectra(
    input = input_file,
    output = "mock_edges.tsv",
    name_source = "CLUSTERID1",
    name_target = "CLUSTERID2",
    method = "gnps",
    threshold = 0.2,
    matched_peaks = 1,
    ppm = 10,
    dalton = 0.01,
    cutoff = NULL
  )

  expect_identical(out, "mock_edges.tsv")
  expect_identical(exported$file, "mock_edges.tsv")
  expect_true(all(c("CLUSTERID1", "CLUSTERID2") %in% names(exported$data)))
  expect_true(all(
    c("feature_spectrum_entropy", "feature_spectrum_peaks") %in%
      names(exported$data)
  ))
  expect_true(any(exported$data$CLUSTERID1 == "F1"))
  expect_true(any(exported$data$CLUSTERID2 == "F2"))
})
